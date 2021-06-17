#lang racket

;; TODO: Once the let unroller is implemented we can eliminate more subexpressions.
;; Right now we don't look inside any let or while statements for eliminating, but once
;; the unroller is implemented, we can just expand everything for MAXIMUM elimination.
(require racket/hash)
(require "common.rkt" "fpcore-reader.rkt" "fpcore-visitor.rkt")

(provide core-common-subexpr-elim)

(module+ test (require rackunit))

(define *names* (make-parameter (mutable-set)))

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
  (match body
   [`(,(or 'let 'let*) ([,vars2 ,vals2] ...) ,body2)
    (define comb-vars (append vars vars2))
    (if (check-duplicates comb-vars) ; don't fuse with duplicates
        `(let (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
              ,(visit/ctx visitor body ctx))
        (visit/ctx visitor
                   `(let* (,@(map list comb-vars (append vals vals2))) ,body2)
                   ctx))]
   [else
    `(,let_ (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
          ,(visit/ctx visitor body ctx))]))

(define/transform-expr (fuse-let expr)
  [visit-let_ visit-let/fuse])

;;;;;; main cse

(define (filter-no-edges deps)
  (map car (filter (compose null? cdr) deps)))

; kahn's algorithm
(define (topo-sort idxs deps)
  (define no-edges (filter-no-edges deps))
  (let loop ([sorted '()] [deps deps] [no-edges no-edges])
    (cond
     [(null? no-edges) (reverse sorted)]
     [else 
      (define chosen (car no-edges))
      (define deps*
        (for/list ([v (in-list deps)] #:unless (= (car v) chosen))
          (cons (car v) (filter-not (curry = chosen) (cdr v)))))
      (define edges* (filter-no-edges deps*))
      (loop (cons chosen sorted) deps* edges*)])))

(define (at-least-two-ops? expr)
  (let loop ([expr expr])
    (match expr
     [(list op args ...) (ormap list? args)]
     [_ #f])))


(define (common-subexpr-elim vars expr)
  (define exprhash
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons var i))))
  (define exprs
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons i (list var)))))
  (define exprc (length vars))

  (define common (mutable-set))
  (define varc exprc)

  ; convert to indices to process
  (define (munge expr)
    (cond
     [(hash-has-key? exprhash expr) ; already seen
      (define idx (hash-ref exprhash expr))
      (when (at-least-two-ops? expr)
        (set-add! common idx))
      idx]
     [else    ; new expresion
      (define old-exprc exprc)
      (hash-set! exprhash expr exprc)
      (begin0 exprc
        (set! exprc (+ 1 exprc))
        (hash-set! exprs old-exprc
          (match expr
           [(list op args ...)
            (cond
             [(set-member? '(let let*) op)
              (for ([arg (first args)]) (add-name! arg))]
             [(set-member? '(while while* for for*) op)
              (for ([arg (second args)]) (add-name! arg))])
            (cons op (map munge args))]
           [_ 
            (list expr)])))]))

  (define root (munge expr))

  ; get let locations, dependent exprs
  ; idx -> (loc, deps ...)
  ; dependent exprs must be bound first
  (define common-deps
    (let loop ([idx root])
      (match-define (list op args ...) (hash-ref exprs idx))  ; 'op' can also be a const or var
      (define hs (map loop args))
      (define h
        (if (set-member? common idx)
            (hash idx (cons idx (remove-duplicates (apply append (map hash-keys hs)))))
            (hash)))
      (apply hash-union h hs
             #:combine (λ (x y) (cons idx (remove-duplicates (append (cdr x) (cdr y))))))))

  (define common-locs (mutable-set))
  (for ([vals (hash-values common-deps)])
    (set-add! common-locs (car vals)))

  ; reconstruct expression
  (define (gen-let-bindings loc)
    (define deps
      (for/list ([(k v) (in-hash common-deps)]
                #:when (= (car v) loc))
        (cons k (cdr v))))
    (define idxs (topo-sort (map car deps) deps))
    (set-remove! common-locs loc)
    (for/list ([idx idxs])
      (define name (gensym 't))
      (begin0 (list name (reconstruct idx))
        (hash-set! exprs idx (list name)))))
  
  (define (reconstruct idx)
    (let loop ([idx idx])
      (define expr (hash-ref exprs idx))
      (cond
       [(set-member? common-locs idx)
        (define bindings (gen-let-bindings idx))
        (list 'let* bindings (loop idx))]
       [(> (length expr) 1)
        (cons (car expr) (map loop (cdr expr)))]
       [else 
        (if (list? (car expr)) expr (car expr))])))

  (fuse-let (reconstruct root)))

;;;;;;; top-level

(define (core-common-subexpr-elim core)
  (*names* (mutable-set))
  (define-values (name args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (for ([arg args]) (add-name! arg))
  (define cse-body (common-subexpr-elim args body))
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
  (define (check-cse in out)
    (check-equal? (core-common-subexpr-elim in) out))

  (check-cse '(FPCore (a) a)
             '(FPCore (a) a))

  (check-cse '(FPCore (a) (+ (+ a a) (+ a a)))
             '(FPCore (a) (let ((t (+ a a))) (+ t t))))

  (check-cse '(FPCore (a x) (+ (- (+ a x) a)
                               (- (+ a x) a)))
             '(FPCore (a x) (let ((t (- (+ x a) x))) (+ t t))))
  
  (check-cse '(FPCore (a) (let ((j0 (+ a a))) j0))
             '(FPCore (a) (let ((j0 (+ a a))) j0)))

  (check-cse '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0)))
             '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0))))

  (check-cse '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0)))
             '(FPCore (a) (let* ((t (+ a a)) (i0 (- a a))) (- (+ t t) i0))))

  (check-cse '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x)))
             '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x))))
)
