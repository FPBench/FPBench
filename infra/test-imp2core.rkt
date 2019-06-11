#lang racket

(require "test-common.rkt" "../src/common.rkt" "../tools/imp2core.rkt" "../src/fpcore.rkt" "../tools/fpimp.rkt")

(define tests-to-run (make-parameter 10))
(define fuel (make-parameter 100))

(define (=* a b)
  (or (equal? a 'timeout) (ormap (curryr equal? 'timeout) b)
      (andmap (λ (x y) (or (= x y) (and (nan? x) (nan? y)))) a b)))

(module+ main
  (command-line
   #:program "test/imp2core.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
    (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
    (tests-to-run (string->number repeat_))]
   #:args files
   (for ([file files])
     (call-with-input-file file
       (λ (p)
         (define error 0)
         (for ([prog (in-port read p)])
           (match-define (list 'FPImp (list vars ...) props&body ...) prog)
           (define-values (body props) (parse-properties props&body))
           (define type (dict-ref props ':type 'binary64))
           (define timeout 0)
           (define results
             (for/list ([i (in-range (tests-to-run))])
               (define ctx (for/list ([var vars])
                             (cons var (match type
                                         ['binary64 (sample-double)]
                                         ['binary32 (sample-single)]))))
               (define evaltor (match type ['binary64 racket-double-evaluator] ['binary32 racket-single-evaluator]))
               (define out1 ((eval-fuel-stmt evaltor (fuel) 'timeout) body ctx))
               (define out2
                 (for/list ([fpcore (imp->core prog)])
                   (if (equal? out1 'timeout)
                       'timeout
                       ((eval-fuel-expr evaltor (fuel) 'timeout) (last fpcore) ctx))))
               (when (or (equal? out1 'timeout) (equal? out2 'timeout))
                 (set! timeout (+ 1 timeout)))
               (list ctx out1 out2)))
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
         (exit error))))))
