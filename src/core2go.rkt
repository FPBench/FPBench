#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide go-header core->go)

;; Go

(define go-header (curry format "package ~a\n\nimport \"math\"\n\n"))
(define go-supported (supported-list
   (invert-op-list '(digits))
   fpcore-consts
   '(binary32 binary64)
   '(nearestEven)))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define (operator->go props operator args)
  (let ([arg-list (string-join args ", ")])
    (match operator
      [(or 'fabs 'fmax 'fmin 'fdim)
       (format "math.~a(~a)" (string-titlecase (substring (~a operator) 1)) arg-list)]
      [(or 'isinf 'isnan)
       (format "math.Is~a(~a)" (string-titlecase (substring (~a operator) 2)) arg-list)]
      [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
           'sin 'cos 'tan 'asin 'cos 'atan 'atan2 'sinh 'cosh 'tanh
           'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
           'remainder 'copysign 'trunc 'round)
       (format "math.~a(~a)" (string-titlecase (~a operator)) arg-list)])))

(define (constant->go props expr)
  (define type (type->go (dict-ref props ':precision 'binary64)))
  (match expr
    ['TRUE "true"]
    ['FALSE "false"]
    [(? hex?) (format "~a" expr)]
    [(? number?) (~a (real->double-flonum expr))]
    [(? symbol?)
     (define name
       (match expr
         ['E "E"] ['LOG2E "Log2E"] ['LOG10E "Log10E"] ['LN2 "Ln2"] ['LN10 "Ln10"]
         ['PI "Pi"] ['PI_2 "Pi/2"] ['PI_4 "Pi/4"] ['SQRT2 "Sqrt2"]
         ['MAXFLOAT "MaxFloat64"] ['INFINITY "Inf(1)"] ['NAN "Nan()"]
         [_ (error 'constant->go "Unsupported constant ~a" expr)]))
     (format "((~a) Math.~a)" type name)]))

(define (declaration->go props var [val #f])
  (define type (type->go (dict-ref props ':precision 'binary64)))
  (if val
      (format "var ~a = ~a(~a)" var type val)
      (format "var ~a ~a" var type)))

(define (assignment->go var val)
  (format "~a = ~a" var val))

(define (round->go val props) (format "~a" val)) ; round(val) = val

(define (function->go name args arg-props body return ctx vars)
  (define type (type->go (ctx-lookup-prop ctx ':precision 'binary64)))
  (format "func ~a(~a) ~a {\n~a\treturn ~a;\n}\n"
          name
          (string-join
           (map (λ (arg) (format "~a ~a" arg type)) args)
           ", ")
          type
          body return))

(define go-language (language (const "go") operator->go constant->go declaration->go assignment->go
                              round->go (const "") function->go))

;;; Exports

(define (core->go prog name) (parameterize ([*lang* go-language]) (convert-core prog name)))

(define-compiler '("go") go-header core->go (const "") go-supported)