#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2c.rkt" "../src/evaluator.rkt")

(define (compile->c prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "#include <stdlib.h>\n#include <stdio.h>\n~a~a\n\n" (c-header) (core->c prog "f"))
      (fprintf p "int main(int argc, char **argv) { ")
      (define strtox (match type ['binary80 "strtold(argv[~a], NULL)"] ['binary64 "strtod(argv[~a], NULL)"] 
                                 ['binary32 "strtof(argv[~a], NULL)"]  ['integer  "strtoll(argv[~a], NULL, 10)"]))
      (fprintf p "printf(\"%.~a\", f(~a)); return 0; }\n"
               (match type ['binary80 "20Lg"] ['binary64 "17g"] ['binary32 "17g"] ['integer "li"])
               (string-join (map (curry format strtox) (map add1 (range N))) ", "))))
  (define c-file (string-replace test-file ".c" ".bin"))
  (system (format "cc ~a -lm -frounding-math -o ~a" test-file c-file))
  c-file)

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

(define (run<-c exec-name ctx type number)
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name (map float->string (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (float->output out* type) out*))

(define (c-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (copy-value a type))
    (define b* (copy-value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (c-format-args var val type)
  (format "~a = ~a" var val))

(define (c-format-output result)
  (format "~a" result))

(define c-tester (tester "c" compile->c run<-c c-equality c-format-args c-format-output (const #t) c-supported #f))

; Command line
(module+ main (parameterize ([*tester* c-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.c")])
    (exit state))))