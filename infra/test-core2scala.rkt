#lang racket
 
(require generic-flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/core2scala.rkt" "../src/range-analysis.rkt"
         "../src/supported.rkt")

(define (compile->scala prog ctx type test-file)
  (*scala-prec-file*
    (open-output-file 
      (string-append (string-trim test-file ".scala") ".prec.txt") 
        #:mode 'text #:exists 'truncate))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (fprintf p "~a~a~a\n" (scala-header "main") (core->scala prog "f") (scala-footer))))
  (close-output-port (*scala-prec-file*))
  test-file)

(define (float->output x prec)
  (define-values (es nbits)
    (match prec
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gfl x)))

(define (copy-value x prec)
  (define-values (es nbits)
    (match prec
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gflcopy x)))

(define (run<-scala exec-name ctx type number)
  (define prec-filename (string-append (string-trim exec-name ".scala") ".prec.txt"))
  (define out 
    (run-with-time-limit 
        "daisy" 
        (if (empty? ctx) 
            exec-name
            (format "~a --mixed-precision=~a" exec-name prec-filename))))
  (define timeout? #f)
  (define out*
   (cond
    [(equal? out "timeout")
       (set! timeout? #t)
       'timeout]
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
  (if timeout?
    (cons 'timeout "timeout")
    (cons 
      (cons (float->output (car out*) type) (float->output (cdr out*) type))
      (format "[~a, ~a]" (car out*) (cdr out*)))))

(define (scala-equality a bound ulps type ignore?)
  (cond 
    [ignore? #t]
    [(or (equal? a 'timeout) (equal? bound 'timeout)) #t]
    [(gflnan? a) (or (and (gflnan? (car bound)) (gflnan? (cdr bound)))
                (and (gflinfinite? (car bound)) (gflinfinite? (cdr bound))))]
    [else
     (define a* (copy-value a type))
     (define lb (copy-value (car bound) type))
     (define ub (copy-value (cdr bound) type))
     (gfl<= lb a* ub)]))

(define (scala-format-args var val type)
  (format "~a = ~a" var val))

(define (scala-format-output result)
  (format "[~a, ~a]" (car result) (cdr result)))

(define (scala-filter core)
  (define-values (vars props* body)
   (match core
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define-values (_ props) (parse-properties props*))
  (define precond (dict-ref props ':pre '()))
  (define range-table (condition->range-table precond))
  (define ops (operators-in core))
  (and
    (for/and ([var vars]) 
      (let ([ranges (dict-ref range-table var (list (make-interval -inf.0 +inf.0)))])
        (and (= (length ranges) 1) (nonempty-bounded? ranges))))
    (not (set-member? ops 'if))))

(define scala-tester (tester "scala" compile->scala run<-scala scala-equality scala-format-args
                             scala-format-output scala-filter scala-supported #t))

; Command line
(module+ main (parameterize ([*tester* scala-tester] [*scala-suppress* #t])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.scala")])
    (exit state))))