#lang racket

(require "test-common.rkt" "../src/core2smtlib2.rkt")

(define (translate->smt prog ctx type test-file)
    (*prog* (core->smtlib2 prog "f"))
    test-file)

(define (run<-smt exec-name ctx type)
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
                 i w p (number->smt value w p 'nearestEven)))
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
      [(list (app syntax-e '_) (app syntax-e 'NaN) stxw stxp)
       (match type
         ['binary32 +nan.f]
         ['binary64 +nan.0])]
      [(list (app syntax-e '_) (app syntax-e '+oo) stxw stxp)
       (match type
         ['binary32 +inf.f]
         ['binary64 +inf.0])]
      [(list (app syntax-e '_) (app syntax-e '-oo) stxw stxp)
       (match type
         ['binary32 -inf.f]
         ['binary64 -inf.0])]
      [(list (app syntax-e '_) (app syntax-e '+zero) stxw stxp) 0.0]
      [(list (app syntax-e '_) (app syntax-e '-zero) stxw stxp) -0.0]
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
  (cons out* (format "~a" out*)))

(define (smt-equality a b ulps)
  (or (equal? a b) (= a b) (and (nan? a) (nan? b))))

(define (smt-format-args var val type)
  (define-values (w p)
        (match type
          ['binary32 (values 8 24)]
          ['binary64 (values 11 53)]))
  (format "~a = ~a\n\t~a = ~a" var val var (number->smt val w p 'nearestEven)))

(define (smt-format-output result)
  (format "~a" result))

(define smt-tester (tester "smt" translate->smt run<-smt smt-equality smt-format-args smt-format-output smt-supported))

; Command line
(module+ main (parameterize ([*tester* smt-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.smt")])
    (exit state))))