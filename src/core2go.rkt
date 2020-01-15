#lang racket

(require "common.rkt" "imperative.rkt" "compilers.rkt")
(provide go-header core->go)

;; Go

(define go-name (const "go"))
(define go-header (curry format "package ~a\n\nimport \"math\"\n\n"))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define (operator->go type operator)
  (match operator
    [(or 'fabs 'fmax 'fmin 'fdim)
     (format "math.~a(~a)" (string-titlecase (substring (~a operator) 1)) "~a")]
    [(or 'isinf 'isnan)
     (format "math.Is~a(~a)" (string-titlecase (substring (~a operator) 2)) "~a")]
    [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'cos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
         'remainder 'copysign 'trunc 'round)
     (format "math.~a(~a)" (string-titlecase (~a operator)) "~a")]))

(define (constant->go type expr)
  (match expr
    ['TRUE "true"]
    ['FALSE "false"]
    [(? symbol?)
     (define name
       (match expr
         ['E "E"] ['LOG2E "Log2E"] ['LOG10E "Log10E"] ['LN2 "Ln2"] ['LN10 "Ln10"]
         ['PI "Pi"] ['PI_2 "Pi/2"] ['PI_4 "Pi/4"] ['SQRT2 "Sqrt2"]
         ['MAXFLOAT "MaxFloat64"] ['INFINITY "Inf(1)"] ['NAN "Nan()"]
         [_ (error 'constant->go "Unsupported constant ~a" expr)]))
     (format "((~a) Math.~a)" type name)]
    [(? number?) (~a (real->double-flonum expr))]))

(define (declaration->go type var [val #f])
  (if val
      (format "var ~a = ~a(~a)" var type val)
      (format "var ~a ~a" var type)))

(define (assignment->go var val)
  (format "~a = ~a" var val))

(define (function->go type name args body return)
  (format "func ~a(~a) ~a {\n~a\treturn ~a;\n}\n"
          name
          (string-join
           (map (λ (arg) (format "~a ~a" arg type)) args)
           ", ")
          type
          body return))

(define go-language (language go-name type->go operator->go constant->go declaration->go assignment->go function->go))

;;; Exports

(define (core->go prog name) (parameterize ([*lang* go-language]) (convert-core prog name)))

(define-compiler '("go") go-header core->go (const "") '())