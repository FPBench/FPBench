#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2wls.rkt")

(define (translate->wls prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (fprintf port "~a\n" (core->wls prog "f"))
      (fprintf port
               (format "TimeConstrained[MemoryConstrained[Print[f[~a] // N], 2^32], 5]\n"
                       (string-join (map number->wls (map cdr ctx)) ", ")))))
  test-file)

(define (run<-wls exec-name ctx type)
  (define out 
    (with-output-to-string
      (lambda ()
            (system (format "wolframscript -script ~a" exec-name)))))
  (define fp 
    (match (string-split (string-trim out) "\n")
      ['() ""]
      [s (last s)]))
  (define out* 
    (match fp
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      ["ComplexInfinity" "+nan.0"]
      ["Indeterminate" "+nan.0"]
      [(? string->number x) x]
      [else "+nan.0"]))
  (cons (string->number out*) out*))

(define (wls-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
     (or (= a b)
         ;(<= (abs (flonums-between a b)) (ulps))
         (nan? a)
         (nan? b))]))

(define (wls-format-args var val type)
  (format "~a = ~a" var val))

(define (wls-format-output result)
  (format "~a" result))

(define wls-tester (tester (const "wls") translate->wls run<-wls wls-equality wls-format-args wls-format-output wls-supported))

; Command line
(module+ main (parameterize ([*tester* wls-tester])
  (test-imperative (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.wls")))
