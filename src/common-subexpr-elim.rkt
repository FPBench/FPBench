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

(define/transform-expr (fuse-let expr)
  [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
    (match (list let_ vars body)
     [(list 'let (list var) `(let ([,var2 ,val2]) ,body2))          ; (let ([a ...]) (let ([b ...]) ...))
      (visit/ctx vtor
                `(let* (,(list var (car vals)) ,(list var2 val2)) ,body2)
                 ctx)]
     [(list 'let (list var) `(let* ([,vars2 ,vals2] ...) ,body2))   ; (let ([a ...]) (let* ([b ...] ...) ...))
      (visit/ctx vtor
                `(let* (,@(map list (cons var vars2) (cons (car vals) vals2))) ,body2)
                 ctx)]
     [(list 'let* _ `(let ([,var2 ,val2]) ,body2))   ; (let* ([a ...] ...) (let ([b ...]) ...))
      (visit/ctx vtor
                `(let* (,@(map list vars vals) ,(list var2 val2)) ,body2)
                 ctx)]
     [(list 'let* _ `(let* ([,vars2 ,vals2] ...) ,body2))   ; (let* ([a ...] ...) (let* ([b ...] ...) ...))
      (visit/ctx vtor
                `(let* (,@(map list (append vars vars2) (append vals vals2))) ,body2)
                 ctx)]
     [_
      `(,let_ (,@(for/list ([var vars] [val vals]) (list var (visit/ctx vtor val ctx))))
            ,(visit/ctx vtor body ctx))])])

;;;;;; main cse

; kahn's algorithm
(define (topo-sort deps)
  (define (filter-no-edges deps)
    (map car (filter (compose null? cdr) deps)))
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

  ; visitor for munge (`rec` is munge)
  (define/visit-expr (munge/visit expr rec)
    [(visit-if vtor cond ift iff #:ctx [rec '()])
      (list 'if (rec cond) (rec ift) (rec iff))]
    [(visit-let_ vtor let_ vars vals body #:ctx [rec '()]) ; special-cased below
      (for ([var vars]) (add-name! var)) ; add variables names
      (list let_ vars (map rec vals) (rec body))]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [rec '()])
      (for ([var vars]) (add-name! var)) ; add variables names
      (list while_ (rec cond) vars (map rec inits) (map rec updates) (rec body))]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [rec '()])
      (for ([var (append vars accums)]) (add-name! var))
      (list for_ vars (map rec vals) accums (map rec inits) (map rec updates) (rec body))]
    [(visit-tensor vtor vars vals body #:ctx [rec '()])
      (for ([var vars]) (add-name! var)) ; add variables names
      (list 'tensor vars (map rec vals) (rec body))]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [rec '()])
      (for ([var (append vars accums)]) (add-name! var)) ; add variables names
      (list 'tensor* vars (map rec vals) accums (map rec inits) (map rec updates) (rec body))]
    [(visit-! vtor props body #:ctx [rec '()])
      (define props* (apply dict-set* '() props))
      (if (dict-has-key? props* ':precision)
          (list '! props (rec body (dict-ref props* ':precision)))
          (list '! props (rec body)))]
    [(visit-op_ vtor op args #:ctx [rec '()])
      (cons op (map rec args))]
    [(visit-call vtor func args #:ctx [rec '()])
      (cons func (map rec args))]
    [(visit-terminal_ vtor x #:ctx [rec '()])
      (list x)]) ; put constants into lists so its not confused with indices

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
                     (munge/visit expr (λ (e [p prec]) (loop e p)))))])))

  ; fill `exprhash` and `exprs` and store top index
  (define root (munge expr))

  ; helper: merges dependency tables
  (define (merge-deps idx hs)
    (define h ; add current index if it is common
      (if (set-member? common idx)
          (hash idx (cons idx (remove-duplicates (append-map hash-keys hs))))
          (hash)))
    (apply hash-union h hs ; combine child tables with this one
           #:combine (λ (x y) (cons idx (remove-duplicates (append (cdr x) (cdr y)))))))

  ; dependent idxs
  ; idx -> (loc, deps ...)
  ; order of let bindings must respect dependencies
  (define common-deps
    (let loop ([idx root])
      (match (hash-ref exprs idx)
       [(list (or 'let 'let* 'tensor) vars vals body) ; don't propogate
        (merge-deps idx (map loop (cons body vals)))]
       [(list (or 'while 'while*) cond vars inits updates body) ; don't propogate
        (merge-deps idx (map loop (append (list cond body) inits updates)))]
       [(list (or 'for 'for* 'tensor*) vars vals accums inits updates body) ; don't propogate
        (merge-deps idx (map loop (append vals inits updates (list body))))]
       [(list '! props body) ; don't propogate
        (merge-deps idx (list (loop body)))]
       [(list op args ..1)
        (define deps (merge-deps idx (map loop args)))
        (define prop-up  ; pull up all common idxs bound one level below
          (for/hash ([(k v) (in-hash deps)] #:when (set-member? args (car v)))
            (values k (cons idx (cdr v)))))
        (hash-union deps prop-up #:combine (λ (x y) y))]
       [(list term) ; ignore constants
        (hash)])))

  ; let locations
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
      (match* ((hash-ref exprs idx) idx)
       [(_ (? (curry set-member? common-locs))) ; location for let bindings
        (list 'let* (gen-let-bindings idx) (loop idx))]
       [((list (and (or 'let 'let*) let_) vars vals body) _)
        `(,let_ ,(for/list ([var vars] [val vals]) (list var (loop val))) ,(loop body))]
       [((list (and (or 'while 'while*) while_) cond vars inits updates body) _)
        `(,while_ ,(loop cond)
                  ,(for/list ([var vars] [init inits] [update updates])
                    (list var (loop init) (loop update)))
                  ,(loop body))]
       [((list (and (or 'for 'for*) for_) vars vals accums inits updates body) _)
        `(,for_ ,(for/list ([var vars] [val vals]) (list var (loop val)))
                ,(for/list ([accum accums] [init inits] [update updates])
                  (list accum (loop init) (loop update)))
                ,(loop body))]
       [((list 'tensor vars vals body) _)
        `(tensor ,(for/list ([var vars] [val vals]) (list var (loop val))) ,(loop body))]
       [((list 'tensor* vars vals accums inits updates body) _)
        `(tensor* ,(for/list ([var vars] [val vals]) (list var (loop val)))
                  ,(for/list ([accum accums] [init inits] [update updates])
                    (list accum (loop init) (loop update)))
                  ,(loop body))]
       [((list '! props body) _)
        `(! ,@props ,(loop body))]
       [((list op args ..1) _)
        (cons op (map loop args))]
       [((list term) _)
        term])))

  ; reconstruct expression top-down
  (fuse-let (reconstruct root)))

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
             '(FPCore (a) (let* ((t_0 (+ a a)) (j0 t_0)) (+ t_0 j0))))

  ; should not combine
  ;;; (check-cse '(FPCore (a) (* (+ a a) (let ((a (* a 2))) (+ a a))))
  ;;;            '(FPCore (a) (* (+ a a) (let ((a (* a 2))) (+ a a)))))

  (check-cse '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0)))
             '(FPCore (a) (let* ((i0 (- a a)) (t_0 (+ a a))) (- (+ t_0 t_0) i0))))

  (check-cse '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x)))
             '(FPCore (a) (let* ((x (- a a)) (x (+ a a))) x)))

  (check-cse '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a))))
             '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a)))))

  (check-cse '(FPCore (a) (let ((x (+ a 1)) (y (- a 1))) (+ (* a x) y)))
             '(FPCore (a) (let ((x (+ a 1)) (y (- a 1))) (+ (* a x) y))))

  (check-cse '(FPCore (a) (while (< i 100) ((i (+ a a) (+ i 1))) (* i (+ a a))))
             '(FPCore (a) (let* ((t_0 (+ a a))) (while (< i 100) ((i t_0 (+ i 1))) (* i t_0)))))

  (check-cse '(FPCore (a) (while* (< i 100) ((i (+ a a) (+ i 1))) (* i (+ a a))))
             '(FPCore (a) (let* ((t_0 (+ a a))) (while* (< i 100) ((i t_0 (+ i 1))) (* i t_0)))))

  (check-cse '(FPCore (a n) (for ((i n)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a n) (let* ((t_0 (+ a a))) (for ((i n)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-cse '(FPCore (a n) (for* ((i n)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a n) (let* ((t_0 (+ a a))) (for* ((i n)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-cse '(FPCore (a n) (tensor ((i n)) (* (+ n a) (+ n a))))
             '(FPCore (a n) (tensor ((i n)) (let* ((t_0 (+ n a))) (* t_0 t_0)))))

  (check-cse '(FPCore (a n) (tensor* ((i n)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a n) (let* ((t_0 (+ a a))) (tensor* ((i n)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-fuse-let '(let ([a 1]) (let* ([a 2] [b 3]) (+ a b)))
                  '(let* ([a 1] [a 2] [b 3]) (+ a b)))

  (check-fuse-let '(let ([a 1]) (let ([b 2]) (let ([c 3]) (+ (* a b) c))))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let ([c 3]) (+ (* a b) c)))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let* ([c 3] [d 4]) (+ (* a b) (* c d))))
                  '(let* ([a 1] [b 2] [c 3] [d 4]) (+ (* a b) (* c d))))
)
