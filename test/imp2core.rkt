#lang racket

(require "../tools/common.rkt" "../tools/imp2core.rkt" "../tools/fpcore.rkt" "../tools/fpimp.rkt")

(define tests-to-run (make-parameter 10))
(define fuel (make-parameter 1000))

(define ((eval-fuel-expr evaltor fuel [default #f]) expr ctx)
  (let/ec k
    (let eval ([expr expr] [ctx ctx] [fuel fuel])
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (λ (expr ctx) (eval expr ctx (- fuel 1)))) expr ctx)))))

(define ((eval-fuel-stmt evaltor fuel [default #f]) stmts ctx)
  (let/ec k
    (define (eval expr ctx fuel)
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (curryr eval (- fuel 1))) expr ctx)))
    
    (let loop ([stmts stmts] [ctx ctx] [fuel fuel])
      ((eval-stmts* (curryr eval fuel) (curryr loop (- fuel 1))) stmts ctx))))

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

(define (=* a b)
  (or (or (equal? a 'timeout) (equal? b 'timeout)) (= (apply + a) b) (and (ormap nan? a) (nan? b))))

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
               (define out2 ((eval-fuel-expr evaltor (fuel) 'timeout) (last (compile-program prog)) ctx))
               (when (or (equal? out1 'timeout) (equal? out2 'timeout))
                 (set! timeout (+ 1 timeout)))
               (list ctx out1 out2)))
           (unless (null? results)
             (printf "~a/~a: ~a~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
                     (dict-ref props ':name body) (match timeout
                                                    [0 ""]
                                                    [1 " (1 timeout)"]
                                                    [_ (format " (~a timeouts)" timeout)]))
             (for ([x (in-list results)] #:unless (=* (second x) (third x)))
               (printf "\t~a ≠ ~a @ ~a\n" (second x) (third x)
                       (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", "))))))))))
