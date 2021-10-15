#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2wls.rkt")

(define (translate->wls prog ctx type test-file)
  (*prog* (core->wls prog "f"))
  test-file)
  
(define (run<-wls exec-name ctx type number)
  (define wls-prec (prec->wls type))
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (fprintf port "~a\n" (*prog*))
      (fprintf port
        (format "Block[{$MinPrecision=~a,$MaxPrecision=~a,$MaxExtraPrecision=0},TimeConstrained[MemoryConstrained[Print[f[~a]//N],2^32],5]]\n"
                wls-prec wls-prec
                (string-join (map (λ (x) (format "N[~a, ~a]" (number->wls x) wls-prec))
                                  (map (compose value->real cdr) ctx)) 
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
    (match (string-replace fp "*^" "e")
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      ["ComplexInfinity" "+nan.0"]
      ["Indeterminate" "+nan.0"]
      [(? string->number x) x]
      [else "+nan.0"]))
  (cons (->value out* type) out*))

(define (wls-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))

(define (wls-format-args var val type)
  (format "~a = ~a" var val))

(define (wls-format-output result)
  (format "~a" result))

(define wls-tester (tester "wls" translate->wls run<-wls wls-equality wls-format-args
                           wls-format-output (const #t) wls-supported #f))

; Command line
(module+ main (parameterize ([*tester* wls-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.wls")])
    (exit state))))
