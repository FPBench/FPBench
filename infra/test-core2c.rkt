#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2c.rkt")

(define (compile->c prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (length (second prog)))
      (fprintf p "#include <stdlib.h>\n#include <stdio.h>\n~a~a\n\n" (c-header) (core->c prog "f"))
      (fprintf p "int main(int argc, char **argv) { ")
      (define strtox (match type ['binary64 "strtod"] ['binary32 "strtof"]))
      (fprintf p "printf(\"%.20g\", f(~a)); return 0; }\n"
               (string-join (map (curry format "~a(argv[~a], NULL)" strtox) (map add1 (range N))) ", "))))
  (define c-file (string-replace test-file ".c" ".bin"))
  (system (format "cc ~a -lm -o ~a" test-file c-file))
  c-file)

(define (run<-c exec-name ctx type)
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name (map (compose ~a real->double-flonum) (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons
    ((match type
      ['binary64 real->double-flonum]
      ['binary32 real->single-flonum])
    (string->number out*)) out*))

(define (c-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
      (or (= a b)
          (and (nan? a) (nan? b))
          (and (double-flonum? a) (double-flonum? b) (<= (abs (flonums-between a b)) ulps)))]))

(define (c-format-args var val type)
  (format "~a = ~a" var val))

(define (c-format-output result)
  (format "~a" result))

(define c-tester (tester "c" compile->c run<-c c-equality c-format-args c-format-output c-supported))

; Command line
(module+ main (parameterize ([*tester* c-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.c")])
    (exit state))))