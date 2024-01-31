#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2julia.rkt" "../src/evaluator.rkt")

(define (arg-info prog type)
  (define args
    (match prog
     [(list 'FPCore (list args ...) rest ...) args]
     [(list 'FPCore name (list args ...) rest ...) args]))
  (values (length args)
          (for/list ([arg args])
            (match arg
             [(list '! props ... name) (dict-ref (apply dict-set* '() props) ':precision type)]
             [_ type]))))

(define (compile->julia prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define-values (N arg-types) (arg-info prog type))
      (fprintf p "using Printf\n\n~a\n\n" (core->julia prog "f"))
      (fprintf p "@printf(\"%.17g\", f(~a))\n"
                 (string-join
                   (for/list ([i (in-range 1 (+ N 1))] [type arg-types])
                     (format "parse(~a, ARGS[~a])" (type->julia type) i))
                   ", "))))
  test-file)
                 
(define (run<-julia exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "NaN"]
       ["+inf.0" "Inf"]
       ["-inf.0" "-Inf"]
       [x x])))
  (define err-p (open-output-string))
  (define-values (out err-out)
    (parameterize ([current-error-port err-p])
      (values
        (with-output-to-string
          (λ () (system (format "julia --compile=min -O0 -g0 --history-file=no -- ~a ~a" exec-name (string-join in " ")))))
        (get-output-string err-p))))
  (define out*
    (cond
     [(non-empty-string? err-out)
      (if (string-contains? err-out "DomainError")
          "+nan.0"
          (error 'run<-julia "Run failed with: ~a" err-out))]
     [else
      (match out
       ["NaN" "+nan.0"]
       ["Inf" "+inf.0"]
       ["-Inf" "-inf.0"]
       [x x])]))
  (cons (->value out* type) out*))

(define (julia-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (julia-format-args var val type)
  (format "~a = ~a" var val))

(define (julia-format-output result)
  (format "~a" result))

(define julia-tester
  (tester "julia"
    compile->julia
    run<-julia
    julia-equality
    julia-format-args
    julia-format-output
    (const #t)
    julia-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* julia-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.jl")))
  (exit state))
