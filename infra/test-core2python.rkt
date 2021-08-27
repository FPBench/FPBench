#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2python.rkt" "../src/evaluator.rkt")

(define (compile->python prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N
        (match prog
         [(list 'FPCore (list args ...) rest ...) (length args)]
         [(list 'FPCore name (list args ...) rest ...) (length args)]))
      (fprintf p "import sys\n~a~a\n\n" (python-header) (core->python prog "f"))
      (fprintf p "def printf(format, *args):\n\tsys.stdout.write(format % args)\n\n")
      (fprintf p "if __name__ == \"__main__\":\n")
      (fprintf p "\tprintf(\"%.17g\", f(~a))"
                  (string-join
                    (for/list ([i (in-range 1 (+ 1 N))])
                      (format "float(sys.argv[~a])" i))
                    ", "))))
  test-file) 
                   
(define (run<-python exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "nan"]
       ["+inf.0" "inf"]
       ["-inf.0" "-inf"]
       [x x])))
  (define err-p (open-output-string))
  (define-values (out err-out)
    (parameterize ([current-error-port err-p])
      (values
        (with-output-to-string
          (λ () (system (format "python3 ~a ~a" exec-name (string-join in " ")))))
        (get-output-string err-p))))
  (define out*
    (cond
     [(non-empty-string? err-out)
      (if (or (string-contains? err-out "OverflowError")
              (string-contains? err-out "ValueError")
              (string-contains? err-out "ZeroDivisionError"))
          "nan"
          (error 'run<-python "Run failed with: ~a" err-out))]
     [else 
      (match out
       ["nan" "+nan.0"]
       ["-nan" "+nan.0"]
       ["inf" "+inf.0"]
       ["-inf" "-inf.0"]
       [x x])]))
  (cons (->value out* type) out*))

;; Python likes to throw a lot of errors
;; For these tests NaN is the error state.
;; Allow test to pass in these cases
(define (python-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [(and (gflinfinite? a) (gflnan? b)) true]
   [(and (gflzero? a) (gflnan? b)) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (python-format-args var val type)
  (format "~a = ~a" var val))

(define (python-format-output result)
  (format "~a" result))

(define python-tester
  (tester "python"
    compile->python
    run<-python
    python-equality
    python-format-args
    python-format-output
    (const #t)
    python-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* python-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.py")))
  (exit state))
