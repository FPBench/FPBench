#lang racket

(require "../tools/common.rkt" "../tools/core2js.rkt" "../tools/fpcore.rkt")

(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.js"))
(define fuel (make-parameter 1000))

(define ((eval-fuel evaltor fuel [default #f]) expr ctx)
  (let/ec k
    (let eval ([expr expr] [ctx ctx] [fuel fuel])
      (if (<= fuel 0)
        (k default)
        ((eval-expr* evaltor (λ (expr ctx) (eval expr ctx (- fuel 1)))) expr ctx)))))

;; TODO: add support for multiple types
(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define (compile->js prog test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
       (define N (length (second prog)))
       (fprintf p "math = require('mathjs')\n\n~a\n\n" (compile-program prog #:name "f"))
       (fprintf p "console.log(f(~a));\n"
                (string-join (map (curry format "~a(process.argv[~a])" 'parseFloat) 
                                  (map ((curry +) 2) (range N))) ", "))))
  test-file)

(define (run<-js file-name ctx)
  (define out
    (with-output-to-string
      (λ ()
         (define file-command (format "node ~a" file-name))
         (system (string-join (cons file-command (map (compose ~a real->double-flonum)
                                                      (dict-values ctx)))
                              " ")))))
  (define out*
    (match out
      ["NaN\n" "+nan.0"]
      ["Infinity\n" "+inf.0"]
      ["-Infinity\n" "-inf.0"]
      [x (substring x 0 (sub1 (string-length x)))]))
  ;; javascript can return imaginary numbers which the reference implementation
  ;; doesn't have (returns NaN)
  (if (number? (string->number out*))
    (real->double-flonum (string->number out*))
    +nan.0))

(define (=* a b)
  (or (equal? a b) (= a b) (and (nan? a) (nan? b))))

;; TODO: Add types
(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
    (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
    (tests-to-run (string->number repeat_))]
   ["-o" name_ "Name for generated js file"
    (test-file name_)]
   #:args files
   (let ([error 0])
     (for ([file files])
       (call-with-input-file file
         (λ (p)
           (port-count-lines! p)
           (for ([prog (in-port (curry read-fpcore file) p)])
             (match-define (list 'FPCore (list vars ...) props* ... body) prog)
             (define-values (_ props) (parse-properties props*))
             (define exec-file (compile->js prog (test-file)))
             (define timeout 0)
             (define results
               (for/list ([i (in-range (tests-to-run))])
                 (define ctx (for/list ([var vars])
                               (cons var (sample-double))))
                 (define evaltor racket-double-evaluator)
                 (define out
                   (match ((eval-fuel evaltor (fuel) 'timeout) body ctx)
                     [(? real? result)
                      (real->double-flonum result)]
                     [(? complex? result)
                      +nan.0]
                     ['timeout 'timeout]
                     [(? boolean? result) result]))
                 (when (equal? out 'timeout)
                   (set! timeout (+ timeout 1)))
                 (define out* (if (equal? out 'timeout) 'timeout (run<-js exec-file ctx)))
                 (list ctx out out*)))
             (unless (null? results)
               (printf "~a/~a: ~a~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
                       (dict-ref props ':name body) (match timeout
                                                      [0 ""]
                                                      [1 " (1 timeout)"]
                                                      [_ (format " (~a timeouts)" timeout)]))
               (set! error (+ error (count (λ (x) (not (=* (second x) (third x)))) results)))
               (for ([x (in-list results)] #:unless (=* (second x) (third x)))
                 (printf "\t~a ≠ ~a @ ~a\n" (second x) (third x)
                         (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", "))))))))
     (exit error))))
