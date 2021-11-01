#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2matlab.rkt" "../src/evaluator.rkt")

(define (arg-info prog type)
  (define args
    (match prog
     [(list 'FPCore (list args ...) rest ...) args]
     [(list 'FPCore name (list args ...) rest ...) args]))
  (for/list ([arg args])
    (match arg
     [(list '! props ... name) (dict-ref (apply dict-set* '() props) ':precision type)]
     [_ type])))

(define (translate->matlab prog ctx type test-file)
  (define arg-types (arg-info prog type))
  (*prog* (cons arg-types (core->matlab prog "f")))
  test-file)

(define (run<-matlab exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "NaN"]
       ["+inf.0" "Inf"]
       ["-inf.0" "-Inf"]
       [x x])))
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (define arg-types (car (*prog*)))
      (fprintf port "sprintf('~a', f(~a))\n\n"
                    (match type ['binary64 "%.17g"] ['binary32 "%.8g"])
                    (string-join
                      (for/list ([type arg-types] [x in])
                        (match type
                         ['binary64 (~a x)]
                         ['binary32 (format "single(~a)" x)]))
                      ", "))
      (fprintf port "~a\n" (cdr (*prog*)))))
  (define out
    (with-output-to-string
      (λ () (system (format "matlab -nosplash -nodesktop -r \"run('~a'); exit;\"" exec-name)))))
  (define out*
    (match (second (regexp-match #px"'([\\S]+)'" out))
      ["NaN" "+nan.0"]
      ["Inf" "+inf.0"]
      ["-Inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (matlab-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (matlab-format-args var val type)
  (format "~a = ~a" var val))

(define (matlab-format-output result)
  (format "~a" result))

(define matlab-tester
  (tester "matlab03"
    translate->matlab
    run<-matlab
    matlab-equality
    matlab-format-args
    matlab-format-output
    (const #t)
    matlab-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* matlab-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.m")))
  (exit state))