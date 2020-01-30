#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/fpcore.rkt" "../src/supported.rkt")
(provide tester *tester* test-imperative)

(define fuel (make-parameter 100))
(define tests-to-run (make-parameter 10))
(define ulps (make-parameter 0))
(define verbose (make-parameter #f))
(define quiet (make-parameter #f))
(define exact-out (make-parameter #f))

; Common test structure
(struct tester (compile run supported equality))
(define *tester* (make-parameter #f))

(define (compile-test prog type test-file)
  ((tester-compile (*tester*)) prog type test-file))

(define (run-test exec-name ctx type)
  ((tester-run (*tester*)) exec-name ctx type))

(define (=* a b)
  ((tester-equality (*tester*)) a b (ulps)))

;;; Tester core
(define (test-imperative argv curr-in-port source test-file)
  (command-line
  #:program "Tester"
  #:once-each
  ["--fuel" fuel_ "Number of computation steps to allow" (fuel (string->number fuel_))]
  ["--repeat" repeat_ "Number of times to test each program" (tests-to-run (string->number repeat_))]
  ["--error" ulps_ "Error, in ULPs, allowed for libc inaccuracies (probably use a value around 3)" (ulps (string->number ulps_))]
  ["--very-verbose" "Very verbose" (verbose #t) (exact-out #t)]
  ["--exact-output" "Exact compiler output" (exact-out #t)]
  [("-o" "--output") name_ "Name for generated C file" (test-file name_)]
  [("-v" "--verbose") "Verbose" (verbose #t)]
  [("-q" "--quiet") "Quiet" (quiet #t)]
  #:args ()
  (if (and (verbose) (quiet)) 
      (error "Verbose and quiet flags cannot be both set") (void))
  (let ([state 0])
    (for ([prog (in-port (curry read-fpcore source) curr-in-port)]
      #:when (valid-core prog (tester-supported (*tester*))))
      (match-define (list 'FPCore (list vars ...) props* ... body) prog)
      (define-values (_ props) (parse-properties props*))
      (define type (dict-ref props ':precision 'binary64))
      (define exec-file (compile-test prog type test-file))
      (define timeout 0)
      (define results  ; run test
        (for/list ([i (in-range (tests-to-run))])
          (define ctx (for/list ([var vars])
            (cons var
              (match type
                ['binary64 (sample-double)]
                ['binary32 (sample-single)]))))
          (define evaltor 
            (match type 
              ['binary64 racket-double-evaluator] 
              ['binary32 racket-single-evaluator]))
          (define out
            (match ((eval-fuel-expr evaltor (fuel) 'timeout) body ctx)
              [(? real? result)
                ((match type
                  ['binary64 real->double-flonum] 
                  ['binary32 real->single-flonum])
                result)]
              [(? complex? result)
                (match type
                  ['binary64 +nan.0]
                  ['binary32 +nan.f])]
                  ['timeout 'timeout]
                  [(? boolean? result)
                result]))
          (when (equal? out 'timeout)
            (set! timeout (+ timeout 1)))
          (define out* (if (equal? out 'timeout) (cons 'timeout "") (run-test exec-file ctx type)))
          (list ctx out out*)))

      (unless (null? results) ; display results
        (define successful (count (λ (x) (=* (second x) (car (third x)))) results))
        (define result-len (length results))
        (unless (and (quiet) (equal? successful result-len))
          (printf "~a/~a: ~a~a\n" successful result-len
            (dict-ref props ':name body) 
            (match timeout
              [0 ""]
              [1 " (1 timeout)"]
              [_ (format " (~a timeouts)" timeout)])))
        (set! state (- result-len successful))
        (for ([i (in-naturals 1)] [x (in-list results)] #:unless (and (not (verbose)) (=* (second x) (car (third x)))))
          (printf "\t~a\t~a\t(Expected) ~a\t(Output) ~a\t(Args) ~a\n" i (if (=* (second x) (car (third x))) "Pass" "Fail") (second x)
              (if (exact-out) (cdr (third x)) (car (third x))) (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", ")))))
    (exit state)))) 
