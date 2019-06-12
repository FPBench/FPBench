#lang racket

(require "common.rkt" "fpcore.rkt")
(provide go-header core->go)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define/match (constant->go c)
  [('E) "math.E"]
  [('LOG2E) "math.Log2E"]
  [('LOG10E) "math.Log10E"]
  [('LN2) "math.Ln2"]
  [('LN10) "math.Ln10"]
  [('PI) "math.Pi"]
  [('PI_2) "(math.Pi/2)"]
  [('PI_4) "(math.Pi/4)"]
  [('M_1_PI) "(1/math.Pi)"]
  [('M_2_PI) "(2/math.Pi)"]
  [('M_2_SQRTPI) "(2/math.Sqrt(math.Pi))"]
  [('SQRT2) "math.Sqrt2"]
  [('SQRT1_2) "(1/math.Sqrt2)"]
  [('MAXFLOAT) "math.MaxFloat64"]
  [('TRUE) "true"]
  [('FALSE) "false"]
  [('INFINITY) "math.Inf(1)"]
  [('NAN) "math.NaN()"]
  [(_) (error 'constant->go "Unsupported constant ~a" c)])

(define/match (operator->go op)
  [((or '== '+ '- '* '/  '< '> '<= '>=)) op]
  [('fabs) 'math.Abs]
  [('exp) 'math.Exp]
  [('exp2) 'math.Exp2]
  [('expm1) 'math.Expm1]
  [('log) 'math.Log]
  [('log10) 'math.Log10]
  [('log2) 'math.Log2]
  [('log1p) 'math.Log1p]
  [('pow) 'math.Pow]
  [('sqrt) 'math.Sqrt]
  [('cbrt) 'math.Cbrt]
  [('hypot) 'math.Hypot]
  [('sin) 'math.Sin]
  [('cos) 'math.Cos]
  [('tan) 'math.Tan]
  [('asin) 'math.Asin]
  [('acos) 'math.Acos]
  [('atan) 'math.Atan]
  [('atan2) 'math.Atan2]
  [('sinh) 'math.Sinh]
  [('cosh) 'math.Cosh]
  [('tanh) 'math.Tanh]
  [('asinh) 'math.Asinh]
  [('acosh) 'math.Acosh]
  [('atanh) 'math.Atanh]
  [('erf) 'math.Erf]
  [('erfc) 'math.Erfc]
  [('tgamma) 'math.Tgamma]
  [('lgamma) 'math.Lgamma]
  [('ceil) 'math.Ceil]
  [('floor) 'math.Floor]
  [('remainder) 'math.Remainder]
  [('fmax) 'math.Max]
  [('fmin) 'math.Min]
  [('fdim) 'math.Dim]
  [('copysign) 'math.Copysign]
  [('trunc) 'math.Trunc]
  [('round) 'math.Round]
  [('isinf)  'math.IsInf]
  [('isnan) 'math.IsNaN]
  [(_) (error 'operator->go "Unsupported operation ~a" op)])

(define (application->go type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a operator b)]
    [(list (or '== '!= '< '> '<= '>=))
     "TRUE"]
    [(list (or '== '< '> '<= '>=) head args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "~a ~a ~a" a operator b))
              " && "))]
    [(list '!= args ...)
     (format "(~a)"
             (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                     (for/list ([b (cdr args)])
                       (format "~a != ~a" (car args) b))
                     (loop (cdr args)))))
              " && "))]
    [(list 'and a ...)
     (format "(~a)" (string-join (map ~a a) " && "))]
    [(list 'or a ...)
     (format "(~a)" (string-join (map ~a a) " || "))]
    [(list (? operator? f) args ...)
     (format "~a(~a)" (operator->go f) (string-join args ", "))]))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->go expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~avar ~a = ~a(~a)\n" indent (fix-name var*) (type->go type)
               (expr->go val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->go body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->go cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~avar ~a ~a\n" indent (fix-name outvar) (type->go type))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->go ift #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->go iff #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~avar ~a = ~a(~a)\n" indent (fix-name var*) (type->go type)
               (expr->go val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~a~a := ~a\n" indent (fix-name test-var)
             (expr->go cond #:names names* #:type type #:indent indent))
     (printf "~afor (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\tvar ~a = ~a(~a)\n" indent (fix-name temp-var) (type->go type)
               (expr->go update #:names names* #:type type #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a" indent (fix-name test-var)
             (expr->go cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->go retexpr #:names names* #:type type #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->go arg #:names names #:type type #:indent indent)) args))
     (application->go type operator args_c)]
    [(? constant?)
     (format "((~a) ~a)" (type->go type) (constant->go expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a" (real->double-flonum expr) )]))

(define go-header "package ~a\n\nimport \"math\"\n\nvar _ = math.Pi\n\n")

(define (core->go prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" (fix-name (if (list? var) (car var) var)) (type->go type) )))
  (define c-body
    (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (printf "\treturn ~a\n" (expr->go body #:type type))))))
  (format "func ~a(~a) ~a {\n~a}\n" (fix-name (if (dict-has-key? properties ':name) (dict-ref properties ':name) name)) (string-join arg-strings ", ") (type->go type) c-body))
