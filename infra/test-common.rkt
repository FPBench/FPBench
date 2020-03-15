#lang racket

(require math/flonum)
(require "../src/common.rkt" "../src/fpcore.rkt" "../src/range-analysis.rkt" "../src/supported.rkt")
(provide tester *tester* test-core *prog*)

(module+ test
  (require rackunit))

(define fuel (make-parameter 1000))
(define tests-to-run (make-parameter 10))
(define ulps (make-parameter 0))
(define verbose (make-parameter #f))
(define quiet (make-parameter #f))
(define exact-out (make-parameter #f))
(define sample-tries (make-parameter 1000))
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

;;; Preconditions

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (random-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define (random-single) 
  (real->single-flonum (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f))))

;   float            ordinal
;   +nan.0,     <->  n > (2^(bits - 1) - 2^(bits - exp_bits - 1))
;   -nan.0      ->   n > (2^(bits - 1) - 2^(bits - exp_bits - 1))
;   +inf.0      <->  n = (2^(bits - 1) - 2^(bits - exp_bits - 1))
;   0<x<+inf.0  <->  0 < n < (2^(bits - 1) - 2^(bits - exp_bits - 1))
;   0           <->  0
;   -0          ->   0
;   0<x<-inf.0  <->  -(2^(bits - 1) - 2^(bits - exp_bits - 1)) < n < 0
;   -inf.0      <->  n = -(2^(bits - 1) - 2^(bits - exp_bits - 1))
;
(define (float->ordinal x type)
  (define b
    (match type
      ['binary32 4]
      ['binary64 8]))
  (define w (* 8 b))
  (define i (integer-bytes->integer (real->floating-point-bytes x b) #t))
  (define s (bitwise-bit-field i (- w 1) w))
  (define u (bitwise-bit-field i 0 (- w 1)))
  (if (> s 0) (- u) u))

(define (ordinal->float x type)
  (define-values (b e) 
    (match type
      ['binary32 (values 4 8)]
      ['binary64 (values 8 11)]))
  (define w (* 8 b))
  (define inf (- (expt 2 (- w 1)) (expt 2 (- (- w e) 1))))
  (define r
    (cond
      [(> x inf)     +nan.0]
      [(= x inf)     +inf.0]
      [(= x (- inf)) -inf.0]
      [(< x (- inf)) -nan.0]
      [else 
        (let ([s (if (< x 0) 1 0)]
              [u (abs x)])
          (floating-point-bytes->real (integer->integer-bytes
                (bitwise-ior (arithmetic-shift s (- w 1)) u)
                b #f)))]))
  (match type
    ['binary64 r]
    ['binary32 (real->single-flonum r)]))

(define (sample-float intervals type)
  (define inf (float->ordinal +inf.0 type)) ; +inf as an ordinal
  (define interval (first intervals)) ; TODO: multiple intervals 
  ; interval [low, high]
  (define low (+ (float->ordinal (interval-l interval) type) (if (interval-l? interval) 0 1)))
  (define high (- (float->ordinal (interval-u interval) type) (if (interval-u? interval) 0 1)))
  ; random integer on the interval [0, 2 * INT_MAX]
  (define rand
    (exact-round
      (* 2 (* (random)
          (match type
            ['binary64 (- (expt 2 64) 1)]
            ['binary32 (- (expt 2 32) 1)])))))
  ; random integer on the interval [low*, high*] (C equiv: rand() % (high - low + 1) + high)
  (ordinal->float (+ (remainder rand (+ (- high low) 1)) low) type))

;;; Unit tests for float->ordinal and ordinal->float

(module+ test
  ; (ordinal->float (float->ordinal x type)) returns x for all floats but -nan.0 and -0.
  (check-equal? +nan.0 (ordinal->float (float->ordinal +nan.0 'binary64) 'binary64))
  (check-equal? +inf.0 (ordinal->float (float->ordinal +inf.0 'binary64) 'binary64))
  (check-equal? 1.79769e+308 (ordinal->float (float->ordinal 1.79769e+308 'binary64) 'binary64))
  (check-equal? 1e10 (ordinal->float (float->ordinal 1e10 'binary64) 'binary64))
  (check-equal? 4.94066e-324 (ordinal->float (float->ordinal 4.94066e-324 'binary64) 'binary64))
  (check-equal? 0.0 (ordinal->float (float->ordinal 0.0 'binary64) 'binary64))
  (check-equal? -4.94066e-324 (ordinal->float (float->ordinal -4.94066e-324 'binary64) 'binary64))
  (check-equal? -1e10 (ordinal->float (float->ordinal -1e10 'binary64) 'binary64))
  (check-equal? -1.79769e+308 (ordinal->float (float->ordinal -1.79769e+308 'binary64) 'binary64))
  (check-equal? -inf.0 (ordinal->float (float->ordinal -inf.0 'binary64) 'binary64))

  ; If a < x < b and f : float -> ordinal, then f(a) < f(x) < f(b) holds true (special case: f(+/-nan.0) > f(+inf.0))
  (check-true (> (float->ordinal +nan.0 'binary64) (float->ordinal +inf.0 'binary64)))
  (check-true (> (float->ordinal +inf.0 'binary64) (float->ordinal 1.79769e+308 'binary64)))
  (check-true (> (float->ordinal 1.79769e+308 'binary64) (float->ordinal 4.94066e-324 'binary64)))
  (check-true (> (float->ordinal 4.94066e-324 'binary64) (float->ordinal 0 'binary64)))
  (check-true (> (float->ordinal 0 'binary64) (float->ordinal -4.94066e-324 'binary64)))
  (check-true (> (float->ordinal -4.94066e-324 'binary64) (float->ordinal -1e10 'binary64)))
  (check-true (> (float->ordinal -1e10 'binary64) (float->ordinal -1.79769e+308 'binary64)))
  (check-true (> (float->ordinal -1.79769e+308 'binary64) (float->ordinal -inf.0 'binary64)))
)

(define (sample-by-rejection pre var evaltor type)
  (define rand
    (match type
      ['binary64 random-double]
      ['binary32 random-single]))
  (for/fold ([num (rand)])
            ([i (in-range (sample-tries))])
            #:break ((eval-expr evaltor) pre (make-hash (list (cons var num))))
            (rand)))
  
;;; Misc

(define ((eval-fuel-expr evaltor fuel [default #f]) expr ctx)
  (let/ec k
    (let eval ([expr expr] [ctx ctx] [fuel fuel])
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (λ (expr ctx) (eval expr ctx (- fuel 1)))) expr ctx)))))

;;; Tester core

(define (test-core argv curr-in-port source test-file)
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
  (define err 0)
  (for ([prog (in-port (curry read-fpcore source) curr-in-port)]
    #:when (valid-core prog (tester-supported (*tester*))))
    (match-define (list 'FPCore (list vars ...) props* ... body) prog)
    (define-values (_ props) (parse-properties props*))
    (define type (dict-ref props ':precision 'binary64))
    (define precond (dict-ref props ':pre '()))
    (define range-table (condition->range-table precond))
    (define exec-name (compile-test prog '() type test-file))
    (define timeout 0)
    (define nans 0) ; wolfram only
    (define results  ; run test
      (for/list ([i (in-range (tests-to-run))])
        (define evaltor 
          (match type 
            ['binary64 racket-double-evaluator] 
            ['binary32 racket-single-evaluator]))
        (define ctx 
          (for/list ([var vars])
            (cons var 
              (cond
                [(equal? precond '())   ; no precondition
                  (sample-float (list (make-interval -inf.0 +inf.0)) type)] 
                [(equal? range-table #f) ; failed range table
                  (sample-by-rejection precond var evaltor type)]
                [else     ; valid range table
                  (sample-float (dict-ref range-table var (list (make-interval -inf.0 +inf.0))) type)]))))
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
      (set! err (+ err (- result-len successful)))
      (for ([i (in-naturals 1)] [x (in-list results)])   
        (define test-passed (=* (second x) (car (third x))))
        (unless (and (not (verbose)) test-passed)
          (printf "\t~a\t~a\t(Expected) ~a\t(Output) ~a\t(Args) ~a\n" 
                  i                                                   
                  (if test-passed "Pass" "Fail")                         
                  (second x)                                                    
                  (format-output (if (exact-out) (cdr (third x)) (car (third x))))
                  (string-join (map (λ (p) (format-args (car p) (cdr p) type)) (first x)) ", "))))))
  err))
