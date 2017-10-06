#lang racket

(require "common.rkt" "fpcore.rkt")
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
    [c (error 'constant->gappa "Unsupported constant ~a" c)]))

(define/match (operator->gappa op)
  [((or '+ '- '* '/ 'sqrt 'fma)) op]
  [((or '<= '>= '< '>)) op]
  [('not) 'not]
  [('and) '/\\]
  [('or) '\\/]
  [(_) (error 'operator->gappa "Unsupported operation ~a" op)])

(define (application->gappa type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    ; TODO: == and != are not supported directly
    ; a == b <=> a - b in [0, 0] in Gappa
    [(list (or '== '!=) args ...)
     (error 'application->gappa "Unsupported operation ~a" operator)]
    [(list (or '< '> '<= '>=) head args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "~a ~a ~a" a (operator->gappa operator) b))
              " /\\ "))]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list 'fabs arg)
     (format "|~a|" arg)]
    [(list (or 'and 'or) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list (? operator? f) args ...)
     (format "~a(~a)" (operator->gappa f) (string-join args ", "))]))

(define/match (type->rnd type)
  [('real) ""]
  [('binary32) "float<ieee_32,ne>"]
  [('binary64) "float<ieee_64,ne>"]
  [('binary128) "float<ieee_128,ne>"]
  [('binary80) "float<x86_80,ne>"])

(define/contract (format-number num)
  (-> number? string?)
  (define t (inexact->exact num))
  (if (= (denominator t) 1)
      (format "~a" t)
      (format "(~a)" t)))

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

(define (expr->gappa expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Prints out all local definitions and returns an expression.
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define prefix (if (eq? type 'real) "M" ""))
     (define vars* (map (λ (v) (gensym (string-append prefix (~a v)))) vars))
     (for ([var* vars*] [val vals])
       (printf "~a~a ~a= ~a;\n" indent (fix-name var*) (type->rnd type)
               (expr->gappa val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->gappa body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_gappa
       (map (λ (arg) (expr->gappa arg #:names names #:type type #:indent indent)) args))
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

(define (compile-program prog #:var-type [var-type 'real] #:name name #:indent [indent ""])
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))
  ; A special property :var-precision
  (define var-type* (dict-ref properties ':var-precision var-type))
  (define name* (dict-ref properties ':name name))

  (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (define real-expr-name (gensym "Mexpr"))
          (define expr-name (gensym "expr"))

          (define real-vars
            (for/hash ([arg args])
              (define name (gensym (string-append "M" (~a arg))))
              (unless (eq? var-type 'real)
                (printf "~a~a = ~a(~a);\n" indent (fix-name name) (type->rnd var-type)
                        (fix-name (gensym (string-append "T" (~a arg))))))
              (values arg name)))

          (define vars
            (for/hash ([arg args])
              (define real-name (dict-ref real-vars arg))
              (if (eq? type var-type)
                  (printf "~a~a = ~a;\n" indent (fix-name arg) (fix-name real-name))
                  (printf "~a~a = ~a(~a);\n" indent (fix-name arg) (type->rnd type) (fix-name real-name)))
              (values arg arg)))

          (printf "\n")
          (define real-expr-body (expr->gappa body #:names real-vars #:type 'real #:indent indent))
          (printf "~a~a = ~a;\n\n" indent real-expr-name real-expr-body)
            
          (define expr-body (expr->gappa body #:names vars #:type type #:indent indent))
          (printf "~a~a ~a= ~a;\n\n" indent expr-name (type->rnd type) expr-body)

          ; I'm not sure that local definitions are allowed here.
          ; There should be a function to remove let expressions (in fpcore.rkt).
          ; Another function for unrolling loops.
          (define cond-body
            (expr->gappa (dict-ref properties ':pre '()) #:names real-vars #:type 'real #:indent indent))
          (when (equal? cond-body "")
            (set! cond-body "0 in [0,0]"))
          (printf "~a{ ~a\n~a   -> |~a - ~a| in ? }\n\n" indent cond-body
                  indent expr-name real-expr-name)

          ; TODO: subdivision hints
          
          ))))

(module+ test
  ; My opinion: all decimal numbers in fpcore files are real numbers
  (parameterize ([read-decimal-as-inexact #f])
    (for ([expr (in-port (curry read-fpcore "test")
                         (open-input-file "../benchmarks/test.fpcore"))])
      (printf "~a\n\n" (compile-program expr #:name "test"))))
)

; TODO: save results in separate files?
(module+ main
  (require racket/cmdline)
  (define var-type 'real)
  
  (command-line
   #:program "core2gappa.rkt"
   #:once-each
   ["--var-type" type "The default type of input variables"
                 (set! var-type (string->symbol type))]
   #:args ()
   (port-count-lines! (current-input-port))
   (parameterize ([read-decimal-as-inexact #f])
     (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
       (printf "~a\n\n" (compile-program expr
                                         #:name (format "ex~a" n)
                                         #:var-type var-type
                                         #:indent "  "))))))
