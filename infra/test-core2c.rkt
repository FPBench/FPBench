#lang racket

(require math/flonum racket/extflonum math/bigfloat)
(require "test-common.rkt" "../src/core2c.rkt" "../src/sampler.rkt")

(define (compile->c prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (length (second prog)))
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

(define (extfl->real x)
  (cond
    [(equal? x +inf.t)  +inf.0] 
    [(equal? x -inf.t)  -inf.0]
    [(equal? x +nan.t)  +nan.0] 
    [(equal? x -nan.t)  -nan.0]
    [else (extfl->exact x)]))  

(define (run<-c exec-name ctx type)
  (define real->float
    (match type
      ['binary80 (λ (x) (parameterize ([bf-precision 64]) (bigfloat->string (bf (extfl->real x)))))]
      ['binary64 real->double-flonum]
      ['binary32 real->double-flonum]
      ['integer identity]))
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name (map (compose ~a real->float) (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (match type
    ['binary80 (cons (parameterize ([bf-precision 64]) (real->extfl (bigfloat->real (bf out*)))) out*)]
    ['binary64 (cons (real->double-flonum (string->number out*)) out*)]
    ['binary32 (cons (real->single-flonum (string->number out*)) out*)]
    ['integer  (cons (string->number out*) out*)]))

(define (c-equality a b ulps)
  (cond
    [(equal? a 'timeout) true]
    [(extflonum? b)
     (let ([a* (real->extfl a)])
      (or (extfl= a* b)
          (and (equal? a* +nan.t) (equal? b +nan.t))
          (and (parameterize ([bf-precision 64]) 
                  (<= (abs (bigfloats-between (bf (extfl->real a)) (bf (extfl->real b))))
                      ulps)))))]
    [else
      (or (= a b)
          (and (nan? a) (nan? b))
          (and (single-flonum? a)
               (parameterize ([bf-precision 24])
                 (<= (abs (- (float->ordinal a 'binary32) (float->ordinal b 'binary32))) ulps)))
          (and (double-flonum? a) (<= (abs (flonums-between a b)) ulps)))]))
          
(define (c-format-args var val type)
  (format "~a = ~a" var val))

(define (c-format-output result)
  (format "~a" result))

(define c-tester (tester "c" compile->c run<-c c-equality c-format-args c-format-output c-supported))

; Command line
(module+ main (parameterize ([*tester* c-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.c")])
    (exit state))))