#lang racket

(require "../tools/common.rkt" "../tools/core2c.rkt" "../tools/eval.rkt")

(define test-file "/tmp/test.c")

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define (sample-single)
  (real->single-flonum (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f))))

(define (compile->c prog test-file #:type [type 'binary64])
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (length (second prog)))
      (fprintf p "#include <stdlib.h>\n#include <stdio.h>\n#include <math.h>\n\n~a\n\n" (compile-program prog #:name "f"))
      (fprintf p "int main(int argc, char **argv) { ")
      (define strtox (match type ['binary64 "strtod"] ['binary32 "strtof"]))
      (fprintf p "printf(\"%.20g\", f(~a)); return 0; }\n"
               (string-join (map (curry format "~a(argv[~a], NULL)" strtox) (map add1 (range N))) ", "))))
  (define c-file (string-replace test-file ".c" ".bin"))
  (system (format "gcc ~a -lm -o ~a" test-file c-file))
  c-file)

(define (run<-c exec-name ctx #:type [type 'binary64])
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
  ((match type
     ['binary64 real->double-flonum]
     ['binary32 real->single-flonum])
   (string->number out*)))

(define tests-to-run 100)

(define (=* a b)
  (or (= a b) (and (nan? a) (nan? b))))

(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:args files
   (for ([file files])
     (call-with-input-file file
       (λ (p)
         (for ([prog (in-port read p)])
           (match-define (list 'FPCore (list vars ...) props* ... body) prog)
           (define-values (_ props) (parse-properties props*))
           (define type (dict-ref props ':type 'binary64))
           (define exec-file (compile->c prog test-file #:type type))
           (define results
             (for/list ([i (in-range tests-to-run)])
               (define ctx (for/list ([var vars])
                             (cons var (match type
                                         ['binary64 (sample-double)]
                                         ['binary32 (sample-single)]))))
               (define evaltor (match type ['binary64 racket-double-evaluator] ['binary32 racket-single-evaluator]))
               (define out
                 (let ([result ((eval-expr evaltor) body ctx)])
                   ((match type
                      ['binary64 real->double-flonum] ['binary32 real->single-flonum])
                    (if (real? result) result +nan.0))))
               (define out* (run<-c exec-file ctx #:type type))
               (list ctx out out*)))
           (unless (null? results)
             (printf "~a/~a: ~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
                     (dict-ref props ':name body))
             (for ([x (in-list results)] #:unless (=* (second x) (third x)))
               (printf "\t~a ≠ ~a @ ~a\n" (second x) (third x)
                       (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", "))))))))))
