#lang racket

(require "test-common.rkt" "../tools/common.rkt" "../tools/core2smtlib2.rkt" "../tools/fpcore.rkt")

(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.smtlib2"))
(define fuel (make-parameter 100))

(define (translate->smt prog ctx test-file #:type type)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (define-values (w p)
        (match type
          ['binary32 (values 8 24)]
          ['binary64 (values 11 53)]))
      (fprintf port "~a\n\n" (compile-program prog #:name "f"))
      (for ([arg ctx] [i (in-naturals)])
        (match-define (cons var value) arg)
        (fprintf port "(define-const arg~a (_ FloatingPoint ~a ~a) ~a)\n"
                 i w p (number->smt value w p 'nearestEven)))
      (fprintf port "(check-sat)\n")
      (if (= (length ctx) 0)
          (fprintf port "(eval f)\n")
          (fprintf port "(eval (f ~a))\n"
                   (string-join
                    (for/list ([i (in-range (length ctx))])
                      (format "arg~a" i))
                    " ")))))
    test-file)

(define (run<-smt exec-name #:type type)
  (let*
      ([out (with-output-to-string
              (lambda ()
                (system (format "z3 ~a" exec-name))))]
       [fp (last (string-split out "\n"))]
       [stx (read-syntax exec-name (open-input-string fp))])
    (match (syntax-e stx)
      [(list (app syntax-e '_) (app syntax-e 'NaN) stxw stxp)
       (match type
         ['binary32 +nan.f]
         ['binary64 +nan.0])]
      [(list (app syntax-e '_) (app syntax-e '+oo) stxw stxp)
       (match type
         ['binary32 +inf.f]
         ['binary64 +inf.0])]
      [(list (app syntax-e '_) (app syntax-e '-oo) stxw stxp)
       (match type
         ['binary32 -inf.f]
         ['binary64 -inf.0])]
      [(list (app syntax-e '_) (app syntax-e '+zero) stxw stxp) 0.0]
      [(list (app syntax-e '_) (app syntax-e '-zero) stxw stxp) -0.0]
      [(list (app syntax-e 'fp) stxS stxE stxC)
       (let
           ([S (syntax->datum stxS)]
            [E (syntax->datum stxE)]
            [C (syntax->datum stxC)])
         (match type
           ['binary32
            (floating-point-bytes->real (integer->integer-bytes
                                         (bitwise-ior (arithmetic-shift S 31) (arithmetic-shift E 23) C)
                                         4 #f))]
           ['binary64
            (floating-point-bytes->real (integer->integer-bytes
                                         (bitwise-ior (arithmetic-shift S 63) (arithmetic-shift E 52) C)
                                         8 #f))]))])))

(define (=* a b)
  (or (equal? a b) (= a b) (and (nan? a) (nan? b))))

(define (run-tests p file)
  (define err 0)
  (port-count-lines! p)
  (for ([prog (in-port (curry read-fpcore file) p)])
    (match-define (list 'FPCore (list vars ...) props* ... body) prog)
    (define-values (_ props) (parse-properties props*))
    (define type (dict-ref props ':precision 'binary64))
    (define-values (w p)
        (match type
          ['binary32 (values 8 24)]
          ['binary64 (values 11 53)]))
    (define timeout 0)
    (define results
      (for/list ([i (in-range (tests-to-run))])
        (define ctx (for/list ([var vars])
                      (cons var (match type
                                  ['binary64 (sample-double)]
                                  ['binary32 (sample-single)]))))
        (define exec-file (translate->smt prog ctx (test-file) #:type type))
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
        (define out* (if (equal? out 'timeout) 'timeout (run<-smt exec-file #:type type)))
        (list ctx out out*)))
    (unless (null? results)
      (printf "~a/~a: ~a~a\n" (count (λ (x) (=* (second x) (third x))) results) (length results)
              (dict-ref props ':name body) (match timeout
                                             [0 ""]
                                             [1 " (1 timeout)"]
                                             [_ (format " (~a timeouts)" timeout)]))
      (set! err (+ err (count (λ (x) (not (=* (second x) (third x)))) results)))
      (for ([x (in-list results)] #:unless (=* (second x) (third x)))
        (printf "\t~a ≠ ~a @ ~a\n\t~a\n" (second x) (third x)
                (string-join (map (λ (x) (format "~a = ~a" (car x) (cdr x))) (first x)) ", ")
                (string-join (map (λ (x) (format "~a = ~a" (car x) (number->smt (cdr x) w p 'nearestEven))) (first x)) ", ")))))
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
     (exit error))))
