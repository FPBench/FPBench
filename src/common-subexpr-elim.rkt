#lang racket

;; TODO: Once the let unroller is implemented we can eliminate more subexpressions.
;; Right now we don't look inside any let or while statements for eliminating, but once
;; the unroller is implemented, we can just expand everything for MAXIMUM elimination.
(require "common.rkt" "fpcore-reader.rkt" "fpcore-visitor.rkt")
(module+ test (require rackunit))

(provide core-common-subexpr-elim)

(define *names* (make-parameter (mutable-set)))

;;;;;; old gensym

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (for/list ([_ (cons name prefixed)] [i (in-naturals)])
      (string->symbol (format "~a~a" name i))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (add-name! name)
  (set-add! (*names*) name))

;;;;;; fuse let expressions

(define (visit-let/fuse visitor vars vals body #:ctx [ctx '()])
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
    `(let (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
          ,(visit/ctx visitor body ctx))]))

(define/transform-expr (fuse-let expr)
  [visit-let visit-let/fuse])

;;;;;; main cse

(define (common-subexpr-elim vars expr)
  (define exprhash
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons var i))))
  (define common (mutable-set))
  (define exprs '())
  (define exprc (length vars))
  (define varc exprc)

  ; convert to indices
  (define (munge expr)
    (cond
     [(hash-has-key? exprhash expr) ; already seen
      (define idx (hash-ref exprhash expr))
      (when (list? expr)
        (set-add! common idx))
      idx]
     [else    ; new expresion
      (define mgd
        (match expr
         [(list op args ...)
          (cons op (map munge args))]
         [_ (list expr)]))
      (begin0 exprc
        (hash-set! exprhash expr exprc)
        (set! exprc (+ 1 exprc))
        (set! exprs (cons mgd exprs)))]))

  (define root (munge expr))
  (define exprvec (list->vector (append (map list vars) (reverse exprs))))

  ; scan dependent indices
  (define (dependent-indices root)
    (filter (curryr >= varc)
      (remove-duplicates
        (let loop ([idx root])
          (match-define (list op args ...) (vector-ref exprvec idx))
          (define idxs (apply append (map loop args)))
          (if (and (not (= idx root)) (set-member? common idx))
              (cons idx idxs)
              idxs)))))

  (define common-depends  ; idx -> list of dependent indices
    (make-hash
      (for/list ([idx (in-set common)])
        (cons idx (dependent-indices idx)))))

  (define bindable (mutable-set)) ; set of idxs that can be put into a let
  (for ([idx (in-set common)])
    (when (null? (hash-ref common-depends idx))
      (set-remove! common idx)
      (hash-remove! common-depends idx)
      (set-add! bindable idx)))

  ; reconstruct expression
  (define (gen-let-bindings)
    (define idxs (set->list bindable)) ; copy
    (define names (build-list (set-count bindable) (λ (_) (gensym 't))))
    (set-clear! bindable)
    (begin0 (for/list ([name names] [idx idxs]) ; descend
              (list name (reconstruct idx)))
      (for ([name names] [idx idxs])            ; then replace exprs
        (vector-set! exprvec idx (list name)))))

  (define (update-common! idx)
    (for ([(k v) (in-hash common-depends)])
      (hash-update! common-depends k (curry remove idx)))
    (for ([idx (in-set common)])
      (when (null? (hash-ref common-depends idx))
        (set-remove! common idx)
        (hash-remove! common-depends idx)
        (set-add! bindable idx))))
  
  (define (reconstruct idx)
    (let loop ([idx idx])
      (define expr (vector-ref exprvec idx))
      (begin0 (cond
               [(not (set-empty? bindable))
                (define bindings (gen-let-bindings))
                (list 'let bindings (loop idx))]
               [(> (length expr) 1)
                (cons (car expr) (map loop (cdr expr)))]
               [else 
                (if (list? (car expr)) expr (car expr))])
        (update-common! idx))))
  (reconstruct root))

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
