#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/core2fptaylor.rkt" "../src/range-analysis.rkt")

(define (compile->fptaylor prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port) (fprintf port "~a\n" (core->fptaylor prog "f"))))
  test-file)

(define (float->output x prec)
  (define-values (es nbits)
    (match prec
     ['binary80 (values 15 80)]
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gfl x)))

; From test-core2c
(define (round-result type out)
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (match type
    ['integer (string->number out*)]
    [_ (float->output out type)]))

(define (run<-fptaylor exec-name ctx type number)
  (define out (run-with-time-limit "fptaylor" 
                "--find-bounds=true" "--print-precision=20"
                exec-name))
  (define timeout? #f)
  (define out*
    (cond 
      [(equal? out "timeout")
        (set! timeout? #t)
        'timeout]
      [(regexp-match #rx"ERROR" out)
        ;(printf "~a\n" out) ;; Dump raw output
        (define conservative_bounds_line (regexp-match
                                          #rx"Bounds [(]without rounding[)]: [^\n]*"
                                          out))
        (define conservative_bounds (regexp-match*
                                    #rx"([+-]?[0-9]+(?:[.][0-9]+(?:[eE][+-]?[0-9]+)?)?)|([-]?inf)|([-]?nan)"
                                    (car conservative_bounds_line)))
        (cons (car conservative_bounds) (cadr conservative_bounds))]
      [else
        (define bounds_line (regexp-match
                            #rx"Bounds [(]floating-point[)]: [^\n]*"
                            out))
        (define bounds (regexp-match*
                        #rx"[+-]?[0-9]+(?:[.][0-9]+(?:[eE][+-]?[0-9]+)?)?"
                        (car bounds_line)))
        (cons (car bounds) (cadr bounds))]))
  (if timeout?
    (cons 'timeout "timeout")
    (cons 
      (cons (round-result type (car out*))   
            (round-result type (cdr out*)))
      (format "[~a, ~a]" (car out*) (cdr out*)))))

(define (copy-value x prec)
  (define-values (es nbits)
    (match prec
     ['binary80 (values 15 80)]
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gflcopy x)))

; =*
; Equality is hard for computer number systems; we may need to define a custom
; way to compare numbers to determine if the test has passed.
(define (fptaylor-equality a bound ulps type ignore?)
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

(define (fptaylor-format-args var val type)
  (format "~a = ~a" var val))

(define (fptaylor-format-output result)
  (format "[~a, ~a]" (car result) (cdr result)))

(define (fptaylor-filter core)
  (define-values (vars props* body)
   (match core
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define-values (_ props) (parse-properties props*))
  (define precond (dict-ref props ':pre '()))
  (define range-table (condition->range-table precond))
  (for/and ([var vars]) 
    (let ([ranges (dict-ref range-table var (list (make-interval -inf.0 +inf.0)))])
      (and (= (length ranges) 1) (nonempty-bounded? ranges)))))

(define fptaylor-tester (tester "fptaylor" compile->fptaylor run<-fptaylor fptaylor-equality fptaylor-format-args
                                fptaylor-format-output fptaylor-filter fptaylor-supported #t))

; Command line
(module+ main (parameterize ([*tester* fptaylor-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.fptaylor")])
    (exit state))))
