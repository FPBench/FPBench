#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2fptaylor.rkt")

(define (string->double string)
  (cond
    [(regexp-match #rx"[-]inf"    string) -inf.0]
    [(regexp-match #rx"inf"     string) +inf.0]
    [(regexp-match #rx"[-]?nan" string) +nan.0]
    [else (real->double-flonum (string->number string))]))


; compile->X
; This invokes the core2x compiler being tested to produce an executable test
; file. Typically, additional boilerplate is needed to call the function on
; arguments. compile->X takes a file location, usually in /tmp by default, and
; puts the result into that file. The tests are not threadsafe and can’t be run
; in parallel.
(define (compile->fptaylor prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port) (fprintf port "~a\n" (core->fptaylor prog "f"))))
  test-file)

; run<-X
; This executes the test in the specified file, captures its output, and
; translates it back into a value that can be compared to the result from the
; reference interpreter. Arguments to the core can be provided here, or baked in
; during the compilation step.
(define (run<-fptaylor exec-name ctx type)
  (begin
    (define out
      (with-output-to-string
        (lambda ()
          (parameterize ([current-error-port(current-output-port)])
            (system (format "fptaylor ~a" exec-name))))))
    (define out*
      (cond 
        [(regexp-match #rx"ERROR" out)
          (begin
            ;(printf "~a\n" out) ;; Dump raw output
            (define conservative_bounds_line (regexp-match
                                              #rx"Bounds [(]without rounding[)]: [^\n]*"
                                              out))
            (define conservative_bounds (regexp-match*
                                        #rx"([+-]?[0-9]+[.][0-9]+[eE][+-]?[0-9]+)|([-]?inf)|([-]?nan)"
                                        (car conservative_bounds_line)))
            (cons (car conservative_bounds) (cadr conservative_bounds)))]
        [else
          (begin
            (define bounds_line (regexp-match
                                #rx"Bounds [(]floating-point[)]: [^\n]*"
                                out))
            (define bounds (regexp-match*
                            #rx"[+-]?[0-9]+[.][0-9]+[eE][+-]?[0-9]+"
                            (car bounds_line)))
             (cons (car bounds) (cadr bounds)))]))
    (cons (cons (string->double (car out*)) (string->double (cdr out*)))
          out*)))

; =*
; Equality is hard for computer number systems; we may need to define a custom
; way to compare numbers to determine if the test has passed.
(define (fptaylor-equality a bound ulps)
  (if (nan? a)
      (and (nan? (car bound)) (nan? (cdr bound)))
      (<= (car bound) a (cdr bound))))

(define (fptaylor-format-args var val type)
  (format "~a = ~a" var val))

(define (fptaylor-format-output result)
  (format "[~a, ~a]" (car result) (cdr result)))

(define fptaylor-tester (tester "fptaylor" compile->fptaylor run<-fptaylor fptaylor-equality fptaylor-format-args fptaylor-format-output fptaylor-supported))

; Command line
(module+ main (parameterize ([*tester* fptaylor-tester])
  (test-imperative (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.txt")))
