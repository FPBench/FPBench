#lang racket

(require "common.rkt" "fpcore.rkt" "fpcore-extra.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define constant->gappa
  (match-lambda
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "1 / sqrt(2)"]
    ['TRUE "0 <= 1"]
    ['FALSE "0 => 1"]
    [c (error 'constant->gappa "Unsupported constant ~a" c)]))

(define/match (operator->gappa op)
  [((or '+ '- '* '/ 'sqrt 'fma)) op]
  [((or '<= '>=)) op]
  ; Gappa does not support strict inequalities
  [('<) '<=]
  [('>) '>=]
  [('not) 'not]
  [('and) '/\\]
  [('or) '\\/]
  [(_) (error 'operator->gappa "Unsupported operation ~a" op)])

(define (application->gappa type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list '== a b)
     (format "(~a = ~a)" a b)]
    [(list '!= a b)
     (format "(~a <> ~a)" a b)]
    [(list (or '+ '- '* '/ '<= '>= '< '> 'and 'or) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list 'fabs arg)
     (format "|~a|" arg)]
    [(list (or 'and 'or) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list (or 'sqrt 'fma 'not) args ...)
     (format "~a(~a)" (operator->gappa operator) (string-join args ", "))]
    [_ (error 'application->gappa "Unsupported operation ~a" operator)]))

(define/match (type->rnd type)
  [('real) ""]
  [('binary32) "float<ieee_32,ne>"]
  [('binary64) "float<ieee_64,ne>"]
  [('binary128) "float<ieee_128,ne>"]
  [('binary80) "float<x86_80,ne>"])

(define (format-rounded type expr)
  (define rnd (type->rnd type))
  (if (equal? rnd "") (format "~a" expr) (format "~a(~a)" rnd expr)))

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->gappa expr #:names [names #hash()] #:type [type 'binary64])
  ;; Takes in an expression. Prints out all local definitions and returns an expression.
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define prefix (if (eq? type 'real) "M" ""))
     (define vars* (map (λ (v) (gensym (string-append prefix (~a v)))) vars))
     (for ([var* vars*] [val vals])
       (printf "~a ~a= ~a;\n" (fix-name var*) (type->rnd type)
               (expr->gappa val #:names names #:type type)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->gappa body #:names names* #:type type)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_gappa
       (map (λ (arg) (expr->gappa arg #:names names #:type type)) args))
     (application->gappa type operator args_gappa)]
    [(? constant?)
     (format "~a(~a)" (type->rnd type) (constant->gappa expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr))]
    [(? number?)
     (define t (inexact->exact expr))
     (if (= (denominator t) 1)
         (format-rounded type t)
         ; Parentheses are required even if type is 'real
         (format "~a(~a)" (type->rnd type) t))]))

(define (compile-program prog
                         #:name name
                         #:precision [precision #f]
                         #:var-precision [var-precision #f]
                         #:rel-error [rel-error #f]
                         #:unroll [unroll #f])
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type
    (if precision precision (dict-ref properties ':precision 'binary64)))
  ; A special property :var-precision
  (define var-type
    (if var-precision var-precision (dict-ref properties ':var-precision 'real)))
  (define name* (dict-ref properties ':name name))
  (define body*
    ((compose canonicalize (if unroll (curryr unroll-loops unroll) identity)) body))

  (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (define real-expr-name (gensym "Mexpr"))
          (define expr-name (gensym "expr"))

          (define real-vars
            (for/hash ([arg args])
              (define name (gensym (string-append "M" (~a arg))))
              (unless (eq? var-type 'real)
                (printf "~a = ~a(~a);\n" (fix-name name) (type->rnd var-type)
                        (fix-name (gensym (string-append "T" (~a arg))))))
              (values arg name)))

          (define vars
            (for/hash ([arg args])
              (define real-name (dict-ref real-vars arg))
              (if (eq? type var-type)
                  (printf "~a = ~a;\n" (fix-name arg) (fix-name real-name))
                  (printf "~a = ~a(~a);\n" (fix-name arg) (type->rnd type) (fix-name real-name)))
              (values arg arg)))

          (printf "\n")
          (define real-expr-body (expr->gappa body* #:names real-vars #:type 'real))
          (printf "~a = ~a;\n\n" real-expr-name real-expr-body)
            
          (define expr-body (expr->gappa body* #:names vars #:type type))
          (printf "~a ~a= ~a;\n\n" expr-name (type->rnd type) expr-body)

          (define cond-body
            (let ([expr (canonicalize (remove-let (dict-ref properties ':pre 'TRUE)))])
              (expr->gappa expr #:names real-vars #:type 'real)))
          (when (equal? cond-body "")
            (set! cond-body "0 in [0,0]"))
          (define goal
            (if rel-error
                (format "~a -/ ~a in ?" expr-name real-expr-name)
                (format "|~a - ~a| in ?" expr-name real-expr-name)))
          (printf "{ ~a\n  -> ~a }\n" cond-body goal)

          ; TODO: subdivision hints
          
          ))))

(module+ test
  (for ([expr (in-port (curry read-fpcore "test")
                       (open-input-file "../benchmarks/fptaylor-tests.fpcore"))])
    (printf "~a\n\n" (compile-program expr #:name "test")))
)

(module+ main
  (require racket/cmdline)
  (define stdout #f)
  (define rel-error #f)
  (define precision #f)
  (define var-precision #f)
  (define unroll #f)
  
  (command-line
   #:program "core2gappa.rkt"
   #:once-each
   ["--stdout" "Print Gappa expressions to the standard output"
               (set! stdout #t)]
   ["--rel-error" "Produce Gappa expressions for relative errors"
                  (set! rel-error #t)]
   ["--precision" prec "The precision of all operations (overrides the :precision property)"
                  (set! precision (string->symbol prec))]
   ["--var-precision" prec "The precision of input variables (overrides the :var-precision property)"
                      (set! var-precision (string->symbol prec))]
   ["--unroll" n "How many iterations to unroll any loops to"
               (set! unroll (string->number n))]
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (with-handlers ([exn:fail? (λ (exn) (eprintf "[ERROR]: ~a\n\n" exn))])
       (define result (compile-program expr
                                       #:name (format "ex~a" n)
                                       #:precision precision
                                       #:var-precision var-precision
                                       #:rel-error rel-error
                                       #:unroll unroll))
       (if stdout
           (printf "~a\n\n" result)
           ; TODO: generate names from the :name properties
           (call-with-output-file (format "ex~a.g" n) #:exists 'replace
             (λ (p) (fprintf p "~a" result)))))))
  )

