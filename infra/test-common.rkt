#lang racket

(require math/flonum)
(require "../src/common.rkt" "../src/fpcore.rkt" "../src/supported.rkt")
(provide tester *tester* test-imperative *prog*)

(define fuel (make-parameter 100))
(define tests-to-run (make-parameter 10))
(define ulps (make-parameter 0))
(define verbose (make-parameter #f))
(define quiet (make-parameter #f))
(define exact-out (make-parameter #f))
(define *prog* (make-parameter #f))   ; interpreted languages can store converted core here

; Common test structure
(struct tester (name compile run equality format-args format-output supported))
(define *tester* (make-parameter #f))

(define (compile-test prog ctx type test-file)
  ((tester-compile (*tester*)) prog ctx type test-file))

(define (run-test exec-name ctx type)
  ((tester-run (*tester*)) exec-name ctx type))

(define (format-args var val type)
  ((tester-format-args (*tester*)) var val type))

(define (format-output result)
  ((tester-format-output (*tester*)) result))

(define (=* a b)
  ((tester-equality (*tester*)) a b (ulps)))

;; Helper functions

(define ((eval-fuel-expr evaltor fuel [default #f]) expr ctx)
  (let/ec k
    (let eval ([expr expr] [ctx ctx] [fuel fuel])
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (λ (expr ctx) (eval expr ctx (- fuel 1)))) expr ctx)))))

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
  (when (and (verbose) (quiet)) 
      (error "Verbose and quiet flags cannot be both set"))
  (let ([state 0])
    (for ([prog (in-port (curry read-fpcore source) curr-in-port)]
      #:when (valid-core prog (tester-supported (*tester*))))
      (match-define (list 'FPCore (list vars ...) props* ... body) prog)
      (define-values (_ props) (parse-properties props*))
      (define type (dict-ref props ':precision 'binary64))
      (define exec-name (compile-test prog '() type test-file))
      (define timeout 0)
      (define nans 0) ; wolfram only
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
          (define out* (if (equal? out 'timeout) 
                           (cons 'timeout "") 
                           (run-test exec-name ctx type)))
          (when (equal? (tester-name (*tester*)) "wls")
            (when (and (not (equal? out 'timeout)) (or (nan? out) (nan? (car out*))))
              (set! nans (+ nans 1))))
          (list ctx out out*)))

      (unless (null? results) ; display results
        (define successful (count (λ (x) (=* (second x) (car (third x)))) results))
        (define result-len (length results))
        (unless (and (quiet) (equal? successful result-len))
          (printf "~a/~a: ~a~a~a\n" successful result-len
            (dict-ref props ':name body) 
            (match timeout
              [0 ""]
              [1 " (1 timeout)"]
              [_ (format " (~a timeouts)" timeout)])
            (match nans
              [0 ""]
              [1 " (1 nan)"]
              [_ (format " (~a nans)" nans)])))
        (set! state (- result-len successful))
        (for ([i (in-naturals 1)] [x (in-list results)])   
          (define test-passed (=* (second x) (car (third x))))
          (unless (and (not (verbose)) test-passed)
            (printf "\t~a\t~a\t(Expected) ~a\t(Output) ~a\t(Args) ~a\n" 
                    i                                                   
                    (if test-passed "Pass" "Fail")                         
                    (second x)                                                    
                    (format-output (if (exact-out) (cdr (third x)) (car (third x))))
                    (string-join (map (λ (p) (format-args (car p) (cdr p) type)) (first x)) ", "))))))
    (exit state)))) 
