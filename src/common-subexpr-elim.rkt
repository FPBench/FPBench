#lang racket

(require "common.rkt" "fpcore.rkt")
(module+ test (require rackunit))

(provide core-common-subexpr-elim)

(define (core-common-subexpr-elim prog)
  (*names* (mutable-set))
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define cs-hash (count-common-subexpr body))
  (define cse-body (common-subexpr-elim cs-hash body))
  `(FPCore ,args ,@props ,cse-body))

(define (combine-common-subexpr-hash h1 h2)
  (for ([(k v) (in-hash h2)])
    (if (hash-has-key? h1 k)
      (hash-set! h1 k #t)
      (hash-set! h1 k v)))
  h1)

(define (count-common-subexpr expr)
  (cond 
    [(list? expr)
     (match-define (list op args ...) expr)
     (let ([cs-hash (make-hash (list (cons expr #f)))])
       (for ([e args])
         (combine-common-subexpr-hash cs-hash (count-common-subexpr e)))
       cs-hash)]
    [else (make-hash)]))

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (common-subexpr-elim cs-hash start-expr)
  (define intermediates '())
  (define name-hash (make-hash))

  (define final-expr (let common-subexpr-body ([expr start-expr])
    (cond
      [(list? expr)
       (match-define (list op args ...) expr)
       (let ([elimed-exprs (for/list ([arg args])
                             (common-subexpr-body arg))])
         (if (hash-ref cs-hash expr #f)
           (if (hash-has-key? name-hash expr)
             (hash-ref name-hash expr)
             (let* ([expr-name (gensym 'i)]
                    [fixed-expr (cons op elimed-exprs)])
               (hash-set! name-hash expr expr-name)
               (set! intermediates (cons (list expr-name fixed-expr)
                                         intermediates))
               expr-name))
           `(,op ,@(for/list ([arg args])
                     (common-subexpr-body arg)))))]
      [else expr])))
  (if (empty? intermediates)
    final-expr
    `(let* (,@(reverse intermediates)) ,final-expr)))

(module+ test
  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) a))
    '(FPCore (a) a))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (+ (+ a a) (+ a a))))
    '(FPCore (a) (let* ((i (+ a a))) (+ i i))))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a x) (+
                                               (- (+ a x) a)
                                               (- (+ a x) a))))
    '(FPCore (a x) (let* ((i (+ a x)) (i1 (- i a))) (+ i1 i1))))
  
  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((j0 (+ a a))) j0)))
    '(FPCore (a) (let* ((j0 (+ a a))) j0)))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0))))
    '(FPCore (a) (let* ((i (+ a a))) (+ i i))))

  (check-equal?
    (core-common-subexpr-elim '(FPCore (a) (let ((i0) (- a a)) (- (+ (+ a a) (+ a a)) i0))))
    '(FPCore (a) (let* ((i (- a a)) (i1 (+ a a))) (- (+ i1 i1) i))))

  
)

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "common-subexpr-elim.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (core-common-subexpr-elim expr)))))
