#lang racket

;; TODO: Once the let unroller is implemented we can eliminate more subexpressions.
;; Right now we don't look inside any let or while statements for eliminating, but once
;; the unroller is implemented, we can just expand everything for MAXIMUM elimination.
(require "common.rkt" "fpcore-reader.rkt")
(module+ test (require rackunit))

(provide core-common-subexpr-elim)

(define *names* (make-parameter (mutable-set)))

;; old gen-sym
(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name
      (for/list ([_ prefixed] [i (in-naturals)])
        (string->symbol (format "~a~a" name i)))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (add-name! name)
  (set-add! (*names*) name))

(define (common-subexpr-elim vars expr)
  (define exprhash
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons var i))))
  (define common (mutable-set))
  (define exprs (map list vars))
  (define exprc (length vars))
  (define varc exprc)

  ; convert to indices
  (define (munge expr)
    (cond
     [(hash-has-key? exprhash expr)
      (define idx (hash-ref exprhash expr))
      (when (list? expr)
        (set-add! common idx))
      idx]
     [else
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
  (define exprvec (list->vector (reverse exprs)))

  ; scan dependent indices
  (define (dependent-indices idx)
    (filter (curryr >= varc)
      (remove-duplicates
        (let loop ([idx idx])
          (define expr (vector-ref exprvec idx))
          (match expr
           [(list op args ...) (apply append (map loop args))]
           [else (list expr)])))))

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
  
  (displayln exprvec)
  (displayln common-depends)
  (displayln bindable)

  ; reconstruct expression
  (define (gen-let-bindings)
    (define idxs (set->list bindable)) ; copy
    (set-clear! bindable)
    (for/list ([idx idxs])
      (define name (gensym 't))
      (begin0 (list name (reconstruct idx))
        (vector-set! exprvec idx (list name)))))

  (define (update-common idx)
    idx)
  
  (define (reconstruct idx)
    (let loop ([idx idx])
      (define expr (vector-ref exprvec idx))
      (cond
       [(not (set-empty? bindable))
        (define bindings (gen-let-bindings))
        (list 'let bindings (loop idx))]
       [(> (length expr) 1)
        (cons (car expr) (map loop (cdr expr)))]
       [else (car expr)])))
  (reconstruct root))


(define (core-common-subexpr-elim core)
  (*names* (mutable-set))
  (define-values (name args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
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
  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) a))
    '(FPCore (a) a))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (+ (+ a a) (+ a a))))
    '(FPCore (a) (let ((t (+ a a))) (+ t t))))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a x) (+
                                               (- (+ a x) a)
                                               (- (+ a x) a))))
    '(FPCore (a x) (let ((t (- (+ x a) x))) (+ t t))))
  
  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((j0 (+ a a))) j0)))
    '(FPCore (a) (let ((j0 (+ a a))) j0)))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0))))
    '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0))))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0))))
    '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0))))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x))))
    '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x)))))
