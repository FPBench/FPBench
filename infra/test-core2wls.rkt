#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/core2wls.rkt" "../src/fpcore.rkt")

(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.wls"))
(define fuel (make-parameter 1000))
;(define ulps (make-parameter 0))

(define (translate->wls prog ctx test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (fprintf port "~a\n" (core->wls prog #:name "f"))
      (fprintf port
               (format "TimeConstrained[MemoryConstrained[Print[f[~a] // N], 2^32], 5]\n"
                       (string-join (map number->wls (map cdr ctx)) ", ")))))
  test-file)

(define (run<-wls exec-name)
  (let*
      ([out (with-output-to-string
              (lambda ()
                (system (format "wolframscript -script ~a" exec-name))))]
       [fp (match (string-split (string-trim out) "\n")
             ['() ""]
             [s (last s)])]
       [out* (match fp
               ["Infinity" "+inf.0"]
               ["-Infinity" "-inf.0"]
               ["ComplexInfinity" "+nan.0"]
               ["Indeterminate" "+nan.0"]
               [(? string->number x) x]
               [else "+nan.0"])])
    (string->number out*)))

(define (=* a b)
  (match (list a b)
    ['(timeout timeout) true]
    [else
     (or (= a b)
         ;(<= (abs (flonums-between a b)) (ulps))
         (nan? a)
         (nan? b))]))

(define (run-tests p file)
  (define err 0)
  (port-count-lines! p)
  (for ([prog (in-port (curry read-fpcore file) p)])
    (match-define (list 'FPCore (list vars ...) props* ... body) prog)
    (define-values (_ props) (parse-properties props*))
    (define type (dict-ref props ':precision 'binary64))
    (define timeout 0)
    (define nans 0)
    (define results
      (for/list ([i (in-range (tests-to-run))])
        (define ctx (for/list ([var vars])
                      (cons var (match type
                                  ['binary64 (sample-double)]
                                  ['binary32 (sample-single)]))))
        (define exec-file (translate->wls prog ctx (test-file)))
        (define evaltor (match type ['binary64 racket-double-evaluator] ['binary32 racket-single-evaluator]))
        (define out
          (match ((eval-fuel-expr evaltor (fuel) 'timeout) body ctx)
            [(? real? result)
             ((match type
                ['binary64 real->double-flonum] ['binary32 real->single-flonum])
              result)]
            [(? complex? result)
             (match type
               ['binary64 +nan.0] ['binary32 +nan.f])]
            ['timeout 'timeout]
            [(? boolean? result) result]))
        (when (equal? out 'timeout)
          (set! timeout (+ timeout 1)))
        (define out* (if (equal? out 'timeout) 'timeout (run<-wls exec-file)))
        (when (and (not (equal? out 'timeout)) (or (nan? out) (nan? out*)))
          (set! nans (+ nans 1)))
        (list ctx out out*)))
    (unless (null? results)
      (printf "~a/~a: ~a~a~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
              (dict-ref props ':name body)
              (match timeout
                [0 ""]
                [1 " (1 timeout)"]
                [_ (format " (~a timeouts)" timeout)])
              (match nans
                [0 ""]
                [1 " (1 nan)"]
                [_ (format " (~a nans)" nans)]))
      (set! err (+ err (count (λ (x) (not (=* (second x) (third x)))) results)))
      (for ([x (in-list results)] #:unless (=* (second x) (third x)))
        (printf "\t~a ≠ ~a @ ~a\n" (second x) (third x)
                (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", ")))))
  err)

(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
    (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
    (tests-to-run (string->number repeat_))]
   ["-o" name_ "Name for generated C file"
    (test-file name_)]
   #:args ()

   (let ([error (run-tests (current-input-port) "stdin")])
     (printf "found ~a discrepancies\n" error)
     ;; Warn, but don't actually fail and hold up the build
     (exit 0))))
