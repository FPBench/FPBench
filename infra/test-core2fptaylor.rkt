#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/core2fptaylor.rkt" "../src/range-analysis.rkt")

(define (string->double string)
  (cond
    [(regexp-match #rx"[-]inf"    string) -inf.0]
    [(regexp-match #rx"inf"     string) +inf.0]
    [(regexp-match #rx"[-]?nan" string) +nan.0]
    [else (real->double-flonum (string->number string))]))

(define (compile->fptaylor prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port) (fprintf port "~a\n" (core->fptaylor prog "f"))))
  test-file)

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
      (cons (string->double (car out*))   
            (string->double (cdr out*)))
      (format "[~a, ~a]" (car out*) (cdr out*)))))

; =*
; Equality is hard for computer number systems; we may need to define a custom
; way to compare numbers to determine if the test has passed.
(define (fptaylor-equality a bound ulps ignore?)
  (cond 
    [ignore? #t]
    [(or (equal? a 'timeout) (equal? bound 'timeout)) #t]
    [(nan? a) (or (and (nan? (car bound)) (nan? (cdr bound)))
                (and (infinite? (car bound)) (infinite? (cdr bound))))]
    [else (<= (car bound) a (cdr bound))]))

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
