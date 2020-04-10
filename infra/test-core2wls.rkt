#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2wls.rkt")

(define (translate->wls prog ctx type test-file)
  (*prog* (core->wls prog "f"))
  test-file)

(define (run<-wls exec-name ctx type)
  (define wls-prec (prec->wls type))
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (fprintf port "~a\n" (*prog*))
      (fprintf port
        (format "Block[{$MinPrecision=~a,$MaxPrecision=~a,$MaxExtraPrecision=0},TimeConstrained[MemoryConstrained[Print[f[~a]//N],2^32],5]\n"
                wls-prec wls-prec
                (string-join (map (λ (x) (format "N[~a, ~a]" (number->wls x) wls-prec))
                                  (map cdr ctx)) 
                            ", ")))))
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
  (cons
    (match type
      ['binary64 (real->double-flonum (string->number out*))]
      ['binary32 (real->single-flonum (string->number out*))])
    out*))

(define (wls-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
     (or (= a b) (nan? a) (nan? b)
         (and (double-flonum? a) (double-flonum? b) (<= (abs (flonums-between a b)) ulps)))]))

(define (wls-format-args var val type)
  (format "~a = ~a" var val))

(define (wls-format-output result)
  (format "~a" result))

(define wls-tester (tester (const "wls") translate->wls run<-wls wls-equality wls-format-args wls-format-output wls-supported))

; Command line
(module+ main (parameterize ([*tester* wls-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.wls")])
    (exit state))))
