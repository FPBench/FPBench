#lang racket
 
(require "test-common.rkt" "../src/common.rkt" "../src/core2scala.rkt" "../src/range-analysis.rkt")

(define (compile->scala prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
   (λ (p)
    (fprintf p "~a~a~a\n" (scala-header "main") (core->scala prog "f") (scala-footer))))
  test-file)

(define (run<-scala exec-name ctx type number)
  (define out
    (with-output-to-string
     (λ ()
      (system (format "daisy ~a" exec-name)))))
  (define out*
   (cond
    [(regexp-match #rx"Warning" out)
      (*ignore-by-run* (make-list (length (*ignore-by-run*)) #t))
      (cons "+nan.0" "+nan.0")]
    [(regexp-match #rx"(Cannot continue)|(Fatal)" out)
      (cons "+nan.0" "+nan.0")]
    [(regexp-match #rx"Real range:" out)
      (define bounds_line (regexp-match #rx"Real range: [^\n]*" out))
      (define bounds (regexp-match*
                            #rx"([+-]?[0-9]+[.][0-9]+([eE][-]?[0-9]+)?)|([-]?inf)|([-]?nan)"
                            (car bounds_line)))
      (cons (car bounds) (cadr bounds))]
    [else
      (cons "-inf.0" "+inf.0")]))
  (let ([res
          (cons 
           (match type
            ['binary64 (cons (real->double-flonum (string->number (car out*)))
                              (real->double-flonum (string->number (cdr out*))))]
            ['binary32 (cons (real->single-flonum (string->number (car out*)))
                              (real->single-flonum (string->number (cdr out*))))])
            out*)])
    (*last-run* res)
    res))

(define (scala-equality a bound ulps ignore?)
  (cond
   [ignore? #t]
   [(nan? a) (or (and (nan? (car bound)) (nan? (cdr bound)))
                 (and (infinite? (car bound)) (infinite? (cdr bound))))]
   [else (<= (car bound) a (cdr bound))]))

(define (scala-format-args var val type)
  (format "~a = ~a" var val))

(define (scala-format-output result)
  (format "[~a, ~a]" (car result) (cdr result)))

(define (scala-filter core)
  (match-define (list 'FPCore (list vars ...) props* ... body) core)
  (define-values (_ props) (parse-properties props*))
  (define precond (dict-ref props ':pre '()))
  (define range-table (condition->range-table precond))
  (for/and ([var vars]) 
    (let ([ranges (dict-ref range-table var (list (make-interval -inf.0 +inf.0)))])
      (= (length ranges) 1))))

(define scala-tester (tester "scala" compile->scala run<-scala scala-equality scala-format-args
                             scala-format-output scala-filter scala-supported))

; Command line
(module+ main (parameterize ([*tester* scala-tester] [*scala-suppress* #t])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.scala")])
    (exit state))))