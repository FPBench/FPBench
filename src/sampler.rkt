#lang racket

(require math/flonum)
(require "evaluator.rkt" "fpcore-interpreter.rkt" "range-analysis.rkt")
(provide sample-float sample-by-rejection sample-random sample-tries
         float->ordinal ordinal->float)

(module+ test
  (require rackunit))

(define sample-tries (make-parameter 100))

; binary32, binary64: f(+-nan) > f(inf)
; binary80:           f(+-nan) < f(-inf)
(define (float->ordinal x type)
  (define-values (w i)
    (match type
     ; ['binary80 (values 80 (let ([b (extfl->floating-point-bytes (real->extfl x))])
     ;                         (+ (integer-bytes->integer (subbytes b 0 8) #f)
     ;                            (* (integer-bytes->integer (subbytes b 8 10) #f) 
     ;                               (expt 2 64)))))]
      ['binary80 (values 64 (integer-bytes->integer (real->floating-point-bytes x 8) #f))]  ; use double instead
      ['binary64 (values 64 (integer-bytes->integer (real->floating-point-bytes x 8) #f))]
      ['binary32 (values 32 (integer-bytes->integer (real->floating-point-bytes x 4) #f))]))
  (define s (bitwise-bit-field i (- w 1) w))
  (define u (bitwise-bit-field i 0 (- w 1)))
  (if (> s 0) (- u) u))

(define (ordinal->float x type)
  (define-values (b e)
    (match type
    ; ['binary80 (values 10 15 real->extfl)]
      ['binary80 (values 8 11)]  ; use double instead
      ['binary64 (values 8 11)]
      ['binary32 (values 4 8)]))
  (define w (* 8 b))
  (define inf 
    (- (expt 2 (- w 1)) 
       (expt 2 (- (- w e) 1))))
  (cond
   [(> x inf)     +nan.0]
   [(= x inf)     +inf.0]
   [(= x (- inf)) -inf.0]
   [(< x (- inf)) -nan.0]
   [else 
    (let ([s (if (< x 0) 1 0)]
          [u (abs x)])
      (floating-point-bytes->real 
        (integer->integer-bytes
          (bitwise-ior (arithmetic-shift s (- w 1)) u)
          b #f)))]))

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

(define (sample-float-on-intervals intervals type)
  (define inf (float->ordinal +inf.0 type)) ; +inf as an ordinal
  (define interval-ranges ; number of floats on the interval for all intervals
    (for/list ([range intervals]) 
      (sub1 (- (+ (float->ordinal (interval-u range) type) (if (interval-u? range) 1 0))
               (- (float->ordinal (interval-l range) type) (if (interval-l? range) 1 0))))))
  (define total-range (for/sum ([range interval-ranges]) range)) ; total number of floats
  (define range-random (exact-round (* (random) (- total-range 1)))) ; random on [0, total-range - 1]
  (define interval ; choose interval
    (for/fold ([low 0] [i 0]
              #:result (list-ref intervals i))
              ([range interval-ranges]
              #:break (and (<= low range-random) (< range-random (+ low range))))
        (values (+ low range) (add1 i))))
  ; interval [low, high]
  (define low (+ (float->ordinal (interval-l interval) type) (if (interval-l? interval) 0 1)))
  (define high (- (float->ordinal (interval-u interval) type) (if (interval-u? interval) 0 1)))
  ; random integer on the interval [0, 2 * UINT_MAX]
  (define rand
    (exact-round
      (* 2 (random)
          (match type
            ['binary80 (- (expt 2 80) 1)]
            ['binary64 (- (expt 2 64) 1)]
            ['binary32 (- (expt 2 32) 1)]))))
  ; random integer on the interval [low*, high*]
  (ordinal->float (+ (remainder rand (+ (- high low) 1)) low) type))

(define (clamp-int x)
  (if (> (abs x) (expt 2 64))
    (inexact->exact (* (expt 2 64) (sgn x)))
    (inexact->exact (truncate x))))

(define (sample-int-on-intervals intervals)
  (define interval-ranges ; number of integers on the interval for all intervals
    (for/list ([range intervals]) 
      (sub1 (- (+ (clamp-int (interval-u range)) (if (interval-u? range) 1 0))
               (- (clamp-int (interval-l range)) (if (interval-l? range) 1 0))))))
  (define total-range (for/sum ([range interval-ranges]) range)) ; total number of integers
  (define range-random (exact-round (* (random) (- total-range 1)))) ; random on [0, total-range - 1]
  (define interval ; choose interval
    (for/fold ([low 0] [i 0]
              #:result (list-ref intervals i))
              ([range interval-ranges]
              #:break (and (<= low range-random) (< range-random (+ low range))))
        (values (+ low range) (add1 i))))
  ; interval [low, high]
  (define low (+ (clamp-int (interval-l interval)) (if (interval-l? interval) 0 1)))
  (define high (- (clamp-int (interval-u interval)) (if (interval-u? interval) 0 1)))
  ; random integer on the interval [0, UINT_MAX]
  (define rand (exact-round (* (random) (expt 2 64))))
  (+ (remainder rand (+ (- high low) 1)) low))

(define (sample-float intervals type)
  (cond
    [(equal? (length intervals) 0) (match type ['binary80 +nan.t] ['binary60 +nan.0] ['binary32 +nan.f])]
    [(equal? type 'integer)  (sample-int-on-intervals intervals)]
    [else (sample-float-on-intervals intervals type)]))

; Returns the float and whether or not it met the precondition
(define (sample-by-rejection pre vars evaltor type)
  (set-evaluator-params! evaltor)
  (for/fold ([nums (for/list ([var vars]) (sample-random type))]
             [attempts 1]
            #:result (values (for/list ([var vars] [num nums]) (cons var num)) 
                             (not (equal? attempts (sample-tries)))))
            ([i (in-range (sample-tries))]
            #:break ((eval-expr evaltor) pre 
                        (make-immutable-hash 
                            (for/list ([var vars] [num nums])
                                      (cons var ((evaluator-real evaltor) num))))))
      (values (for/list ([var vars]) (sample-random type)) (+ i 1))))

;;; Random sampler

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (sample-random type)
  (match type
  ;  ['binary80 (floating-point-bytes->extfl (bytes-append
  ;                (integer->integer-bytes (random-exp 64) 8 #f)
  ;                (integer->integer-bytes (random-exp 16) 2 #f)))]
    ['binary80 (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f))]
    ['binary64 (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f))]
    ['binary32 (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f))]
    ['integer  (- (random-exp 64) (expt 2 63) 1)]))
