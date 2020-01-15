#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/common.rkt" "../src/compilers.rkt" "../src/core2js.rkt" 
         "../src/fpcore.rkt" "../src/supported.rkt") 


(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.js"))
(define fuel (make-parameter 100))
(define ulps (make-parameter 0))

(define (compile->js prog test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
       (define N (length (second prog)))
       (fprintf p "~a\n\n" (core->js prog "f"))
       (fprintf p "console.log(f(~a));\n"
                (string-join (for/list ([i (range N)])
                               (format "parseFloat(process.argv[~a])" (+ i 2)))
                             ", "))))
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
    (match (string-trim out)
      ["NaN" "+nan.0"]
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      [(? string->number x) x]))
  (real->double-flonum (string->number out*)))


(define (=* a b)
  (match (list a b)
    ['(timeout timeout) true]
    [else
     ;; test ranges (e1, e2) (e2, e1) to include negative inputs
     (or (= a b)
         (<= (abs (flonums-between a b)) (ulps))
         (and (nan? a) (nan? b)))]))

;; TODO: Add types
(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
    (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
    (tests-to-run (string->number repeat_))]
   ["--error" ulps_ "Error, in ULPs, allowed for node"
    (ulps (string->number ulps_))]
   ["-o" name_ "Name for generated js file"
    (test-file name_)]
   #:args ()
   (define unsupported    ; get list of unsupported for compiler
     (for/first ([compiler (compilers)]
        #:when (equal? (compiler-export compiler) core->js))
        (compiler-unsupported compiler)))

   (let ([error 0])       ; loop through tests
     (for ([prog (in-port (curry read-fpcore "stdin") (current-input-port))]
        #:when (set-empty? (set-intersect (operators-in prog) unsupported))) ; verify valid operators
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
             (match ((eval-fuel-expr evaltor (fuel) 'timeout) body ctx)
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
                   (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", ")))))
     (exit error))))
