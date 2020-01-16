#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/fpcore.rkt" "../src/supported.rkt")
(provide tester *tester* test-imperative)

; Common test structure
(struct tester (compile run unsupported))
(define *tester* (make-parameter #f))

(define (compile-test prog type test-file)
  ((tester-compile (*tester*)) prog type test-file))

(define (run-test exec-name ctx type)
  ((tester-run (*tester*)) exec-name ctx type))

(define (unsupported-test)
  (tester-unsupported (*tester*)))

(define (=* a b)
  ;m(if (equal? (list a b) '(timeout timeout)))
  (match (list a b)
    ['(timeout timeout) true]
    [else
     ;; test ranges (e1, e2) (e2, e1) to include negative inputs
     (or (= a b)
         (and (nan? a) (nan? b))
         ;; can only use flonums-between for doubles...
         (and (double-flonum? a) (double-flonum? b) (<= (abs (flonums-between a b)) (ulps))))]))
  
; Test parameters

(define fuel (make-parameter 100))
(define tests-to-run (make-parameter 10))
(define ulps (make-parameter 0))

;;; Tester core
(define (test-imperative argv curr-in-port source test-file)
  (command-line
  #:program "C/Go/JS tester"
  #:once-each
  ["--fuel" fuel_ "Number of computation steps to allow"
  (fuel (string->number fuel_))]
  ["--repeat" repeat_ "Number of times to test each program"
  (tests-to-run (string->number repeat_))]
  ["--error" ulps_ "Error, in ULPs, allowed for libc inaccuracies (probably use a value around 3)"
  (ulps (string->number ulps_))]
  ["-o" name_ "Name for generated C file"
  (test-file name_)]
  #:args ()
  (let ([state 0] [unsupported (unsupported-test)])
    (for ([prog (in-port (curry read-fpcore source) curr-in-port)]
      #:when (set-empty? (set-intersect (operators-in prog) unsupported)))
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
            (define out* (if (equal? out 'timeout) 'timeout (run-test exec-file ctx type)))
            (list ctx out out*)))

      (unless (null? results) ; display results
      (printf "~a/~a: ~a~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
        (dict-ref props ':name body) 
        (match timeout
          [0 ""]
          [1 " (1 timeout)"]
          [_ (format " (~a timeouts)" timeout)]))
      (set! state (+ state (count (λ (x) (not (=* (second x) (third x)))) results)))
      (for ([x (in-list results)] #:unless (=* (second x) (third x)))
        (printf "\t~a ≠ ~a @ ~a\n" (second x) (third x)
          (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", ")))))
    (exit state))))