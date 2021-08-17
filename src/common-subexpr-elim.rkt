#lang racket

;; TODO: Once the let unroller is implemented we can eliminate more subexpressions.
;; Right now we don't look inside any let or while statements for eliminating, but once
;; the unroller is implemented, we can just expand everything for MAXIMUM elimination.
(require racket/hash)
(require "common.rkt" "fpcore-reader.rkt" "fpcore-visitor.rkt")

(provide core-common-subexpr-elim *expr-cse-able?*)

(module+ test (require rackunit))

(define *names* (make-parameter (mutable-set)))
(define *expr-cse-able?* (make-parameter list?))

;;;;;; old gensym

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (for/list ([_ (cons name prefixed)] [i (in-naturals)])
      (string->symbol (format "~a_~a" name i))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (add-name! name)
  (set-add! (*names*) name))

;;;;;; fuse let expressions

(define (visit-let/fuse visitor let_ vars vals body #:ctx [ctx '()])
  (match (list let_ vars body)
   [(list 'let (list var) `(let ([,var2 ,val2]) ,body2))          ; (let ([a ...]) (let ([b ...]) ...))
    (visit/ctx visitor
              `(let* (,(list var (car vals)) ,(list var2 val2)) ,body2)
               ctx)]
   [(list 'let (list var) `(let* ([,vars2 ,vals2] ...) ,body2))   ; (let ([a ...]) (let* ([b ...] ...) ...))
    (visit/ctx visitor
              `(let* (,@(map list (cons var vars2) (cons (car vals) vals2))) ,body2)
               ctx)]
   [(list 'let* _ `(let ([,var2 ,val2]) ,body2))   ; (let* ([a ...] ...) (let ([b ...]) ...))
    (visit/ctx visitor
              `(let* (,@(map list vars vals) ,(list var2 val2)) ,body2)
               ctx)]
   [(list 'let* _ `(let* ([,vars2 ,vals2] ...) ,body2))   ; (let* ([a ...] ...) (let* ([b ...] ...) ...))
    (visit/ctx visitor
              `(let* (,@(map list (append vars vars2) (append vals vals2))) ,body2)
               ctx)]
   [_
    `(,let_ (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
          ,(visit/ctx visitor body ctx))]))

(define/transform-expr (fuse-let expr)
  [visit-let_ visit-let/fuse])

;;;;;; main cse

(define (syntax? name)
  (set-member? '(if let let* while while* for for* tensor tensor* !) name))

(define (filter-no-edges deps)
  (map car (filter (compose null? cdr) deps)))

; kahn's algorithm
(define (topo-sort deps)
  (let loop ([sorted '()] [deps deps] [no-edges (filter-no-edges deps)])
    (cond
     [(null? no-edges) (append (reverse sorted) (map car deps))]
     [else 
      (define chosen (car no-edges))
      (define deps*
        (for/list ([v (in-list deps)] #:unless (= (car v) chosen))
          (cons (car v) (filter-not (curry = chosen) (cdr v)))))
      (define edges* (filter-no-edges deps*))
      (loop (cons chosen sorted) deps* edges*)])))

;; eliminator
(define (common-subexpr-elim vars prec expr)
  (define exprhash  ; expr -> idx
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons var i))))
  (define exprs ; idx -> munged expr
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons i (list var)))))
  (define exprc (length vars))
  (define common (mutable-set))
  (define varc exprc)

  ; convert to indices to process
  (define (munge expr)
    (let loop ([expr expr] [prec prec])
      (define key (cons expr prec))
      (cond
       [(hash-has-key? exprhash key) ; already seen
        (define idx (hash-ref exprhash key))
        (when ((*expr-cse-able?*) expr)
          (set-add! common idx))
        idx]
       [else    ; new expresion
        (define old-exprc exprc)
        (hash-set! exprhash key exprc)
        (begin0 exprc
          (set! exprc (+ 1 exprc))
          (hash-set! exprs old-exprc
            (match expr
             [(list '! props ... body)
              (define props* (apply dict-set* '() props))
              (cons (cons '! props) (list (loop body (dict-ref props* ':precision prec))))]
             [(list op args ...)  ; don't check let vals
              (cond   ; add variables names if needed
               [(set-member? '(let let*) op)
                (for ([arg (first args)]) (add-name! arg))]
               [(set-member? '(while while* for for*) op)
                (for ([arg (second args)]) (add-name! arg))])
              (cons op (map (curryr loop prec) args))]
             [_  (list expr)])))])))  ; put constants into lists so its not confused with indices

  (define root (munge expr))  ; fill `exprhash` and `exprs` and store top index

  ; let locations, dependent idxs
  ; idx -> (loc, deps ...)
  ; order of let bindings must respect dependencies
  (define common-deps
    (let loop ([idx root])
      (match-define (list op args ...) (hash-ref exprs idx))  ; 'op' can also be a const or var
      (define hs (map loop args))
      (define h       ; add current index if it is common
        (if (set-member? common idx)
            (hash idx (cons idx (remove-duplicates (apply append (map hash-keys hs)))))
            (hash)))
      (define h*      ; combine child tables with this one
        (apply hash-union h hs
               #:combine (λ (x y) (cons idx (remove-duplicates (append (cdr x) (cdr y)))))))
      (cond
       [(syntax? op) h*]    ; do not attempt to propogate up let bindings past block
       [else
        (define prop-up  ; pull up all common idxs bound one level below
          (for/hash ([(k v) (in-hash h*)] #:when (set-member? args (car v)))
            (values k (cons idx (cdr v)))))
        (hash-union h* prop-up #:combine (λ (x y) y))])))

  (define common-locs (mutable-set))
  (for ([vals (hash-values common-deps)])
    (set-add! common-locs (car vals)))

  ; create let bindings: ([var val] ...)
  (define (gen-let-bindings loc)
    (define deps
      (for/list ([(k v) (in-hash common-deps)]
                #:when (= (car v) loc))
        (cons k (cdr v))))
    (set-remove! common-locs loc)
    (for/list ([idx (topo-sort deps)]) ; break ties with topo sort (dependency graph)
      (define name (gensym 't))
      (begin0 (list name (reconstruct idx))
        (hash-set! exprs idx (list name)))))

  ; reconstruct expression
  (define (reconstruct idx)
    (let loop ([idx idx])
      (define expr (hash-ref exprs idx))
      (cond
       [(set-member? common-locs idx) ; location for let bindings
        (define bindings (gen-let-bindings idx))
        (list 'let* bindings (loop idx))]
       [(> (length expr) 1) ; (op args ...)
        (if (list? (car expr))    ; (! props ... body)
            (append (car expr) (map loop (cdr expr)))
            (cons (car expr) (map loop (cdr expr))))]
       [else 
        (if (list? (car expr)) expr (car expr))])))

  (fuse-let (reconstruct root))) ; reconstruct expression top-down

;;;;;;; top-level

(define (core-common-subexpr-elim core)
  (*names* (mutable-set))
  (define-values (name args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define props* (apply dict-set* '() props))
  (define prec (dict-ref props* ':precision 'binary64))
  (for ([arg args]) (add-name! arg))
  (define cse-body (common-subexpr-elim args prec body))
  (if name
    `(FPCore ,name ,args ,@props ,cse-body)
    `(FPCore ,args ,@props ,cse-body)))


(module+ main
  (require racket/cmdline)
  (command-line
   #:program "common-subexpr-elim.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (core-common-subexpr-elim expr)))))

(module+ test

  (define-syntax-rule (check-cse in out)
    (check-equal? (core-common-subexpr-elim in) out))

  (define-syntax-rule (check-fuse-let in out)
    (check-equal? (fuse-let in) out))

  (check-cse '(FPCore (a) a)
             '(FPCore (a) a))

  (check-cse '(FPCore (a) (+ (+ a a) (+ a a)))
             '(FPCore (a) (let* ((t_0 (+ a a))) (+ t_0 t_0))))

  (check-cse '(FPCore (a x) (+ (- (+ a x) a)
                               (- (+ a x) a)))
             '(FPCore (a x) (let* ((t_0 (- (+ a x) a))) (+ t_0 t_0))))
  
  (check-cse '(FPCore (a) (let ((j0 (+ a a))) j0))
             '(FPCore (a) (let ((j0 (+ a a))) j0)))

  (check-cse '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0)))
             '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0))))

  (check-cse '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0)))
             '(FPCore (a) (let* ((i0 (- a a)) (t_0 (+ a a))) (- (+ t_0 t_0) i0))))

  (check-cse '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x)))
             '(FPCore (a) (let* ((x (- a a)) (x (+ a a))) x)))

  (check-cse '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a))))
             '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a)))))

  (check-fuse-let '(let ([a 1]) (let* ([a 2] [b 3]) (+ a b)))
                  '(let* ([a 1] [a 2] [b 3]) (+ a b)))

  (check-fuse-let '(let ([a 1]) (let ([b 2]) (let ([c 3]) (+ (* a b) c))))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let ([c 3]) (+ (* a b) c)))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let* ([c 3] [d 4]) (+ (* a b) (* c d))))
                  '(let* ([a 1] [b 2] [c 3] [d 4]) (+ (* a b) (* c d))))
)
