#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2smtlib2.rkt")

(define (translate->smt prog ctx type test-file)
    (*prog* (core->smtlib2 prog "f"))
    test-file)

(define (float->string x)
  (match x
   [(? gfl?)  (gfl->string x)]
   [(? real?) (~a x)]))

(define (float->output x prec)
  (define-values (es nbits)
    (match prec
     ['binary80 (values 15 80)]
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gfl x)))

(define (copy-value x prec)
  (define-values (es nbits)
    (match prec
     ['binary80 (values 15 80)]
     ['binary64 (values 11 64)]
     ['binary32 (values 8 32)]))
  (parameterize ([gfl-exponent es] [gfl-bits nbits])
    (gflcopy x)))

(define (run<-smt exec-name ctx type number)
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (define-values (w p)
        (match type
          ['binary32 (values 8 24)]
          ['binary64 (values 11 53)]))
      (fprintf port "~a\n\n" (*prog*))
      (for ([arg ctx] [i (in-naturals)])
        (match-define (cons var value) arg)
        (fprintf port "(define-const arg~a (_ FloatingPoint ~a ~a) ~a)\n"
                 i w p (number->smt (float->string value) w p 'nearestEven)))
      (fprintf port "(check-sat)\n")
      (if (= (length ctx) 0)
          (fprintf port "(eval f)\n")
          (fprintf port "(eval (f ~a))\n"
                   (string-join
                    (for/list ([i (in-range (length ctx))])
                      (format "arg~a" i))
                    " ")))))
  (define out 
    (with-output-to-string
      (lambda ()
        (system (format "z3 ~a" exec-name)))))
  (define fp (last (string-split out "\n")))
  (define stx (read-syntax exec-name (open-input-string fp)))

  (define out*
    (match (syntax-e stx)
      [(list (app syntax-e '_) (app syntax-e 'NaN) stxw stxp)    +nan.0]
      [(list (app syntax-e '_) (app syntax-e '+oo) stxw stxp)    +inf.0]
      [(list (app syntax-e '_) (app syntax-e '-oo) stxw stxp)    -inf.0]
      [(list (app syntax-e '_) (app syntax-e '+zero) stxw stxp)  0]
      [(list (app syntax-e '_) (app syntax-e '-zero) stxw stxp)  -0]
      [(list (app syntax-e 'fp) stxS stxE stxC)
       (let
           ([S (syntax->datum stxS)]
            [E (syntax->datum stxE)]
            [C (syntax->datum stxC)])
         (match type
           ['binary32
            (floating-point-bytes->real (integer->integer-bytes
                                         (bitwise-ior (arithmetic-shift S 31) (arithmetic-shift E 23) C)
                                         4 #f))]
           ['binary64
            (floating-point-bytes->real (integer->integer-bytes
                                         (bitwise-ior (arithmetic-shift S 63) (arithmetic-shift E 52) C)
                                         8 #f))]))]))
  (cons (float->output out* type) (format "~a" out*)))

(define (smt-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [(and (gflnan? a) (gflnan? b)) #t]
   [else
    (define a* (copy-value a type))
    (define b* (copy-value b type))
    (gfl= a* b*)]))

(define (smt-format-args var val type)
  (define-values (w p)
        (match type
          ['binary32 (values 8 24)]
          ['binary64 (values 11 53)]))
  (format "~a = ~a\n\t~a = ~a" var val var (number->smt val w p 'nearestEven)))

(define (smt-format-output result)
  (format "~a" result))

(define smt-tester (tester "smt" translate->smt run<-smt smt-equality smt-format-args smt-format-output (const #t) smt-supported #f))

; Command line
(module+ main (parameterize ([*tester* smt-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.smt")])
    (exit state))))