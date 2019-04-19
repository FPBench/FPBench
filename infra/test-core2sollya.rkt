#lang racket

(require "test-common.rkt" "../tools/common.rkt" "../tools/core2sollya.rkt" "../tools/fpcore.rkt")

(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.sollya"))
(define fuel (make-parameter 100))

(define (translate->sollya prog ctx test-file #:type type)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (fprintf port "~a\n\n" (core->sollya prog #:name "f"))
      (fprintf port "prec=4096!;\ndisplay=dyadic!;\n\n")
      (fprintf port "f(~a);\n"
               (string-join
                (for/list ([arg ctx])
                  (match-define (cons var value) arg)
                  (cond
                    [(nan? value) "nan"]
                    [(infinite? value) (if (>= value 0) "infty" "-infty")]
                    [else (format "~a" (inexact->exact value))]))
                ", "))))
  test-file)

(define (dyadic->exact s)
  (let* ([matches (regexp-match #rx"([+-]?[0-9]+)((?i:b)([+-]?[0-9]+))?" s)]
         [m (string->number (second matches))]
         [e (if (fourth matches)
                (string->number (fourth matches))
                0)])
    (if (>= e 0)
        (* m (expt 2 e))
        (/ m (expt 2 (- e))))))

(define (run<-sollya exec-name #:type type)
  (let
      ([out (last
             (string-split
              (string-trim
               (with-output-to-string
                 (lambda ()
                   (system (format "sollya ~a" exec-name)))))
              "\n"))])
    (cond
      [(regexp-match #rx"(?i:error)" out)
       (begin
         (printf "~a\n" out)
         (error 'run<-sollya "Sollya evaluation failed."))]
      [(regexp-match #rx"(?i:nan)" out)
       (match type
         ['binary32 +nan.f]
         ['binary64 +nan.0])]
      [(regexp-match #rx"[-](?i:infty)" out)
       (match type
         ['binary32 -inf.f]
         ['binary64 -inf.0])]
      [(regexp-match #rx"[+]?(?i:infty)" out)
       (match type
         ['binary32 +inf.f]
         ['binary64 +inf.0])]
      [else
       (match type
         ['binary32 (real->single-flonum (dyadic->exact out))]
         ['binary64 (real->double-flonum (dyadic->exact out))])])))

;; In some cases, such as division by zero, sollya does not distinguish
;; between inf and nan. So for infinite results from the reference interpreter,
;; or for zero, as seen in the probabilities in a clustering algorithm benchmark,
;; allow a NaN answer from sollya.
(define (=* a b)
  (or (equal? a b) (= a b) (and (or (= a 0) (infinite? a) (nan? a)) (or (nan? b)))))

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
        (define exec-file (translate->sollya prog ctx (test-file) #:type type))
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
        (define out* (if (equal? out 'timeout) 'timeout (run<-sollya exec-file #:type type)))
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
                (string-join (map (λ (x) (format "~a = ~a" (car x) (inexact->exact (cdr x)))) (first x)) ", ")))))
  err)

(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
    (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
    (tests-to-run (string->number repeat_))]
   ["-o" name_ "Name for generated sollya file"
    (test-file name_)]
   #:args ()

   (let ([error (run-tests (current-input-port) "stdin")])
     (exit error))))
