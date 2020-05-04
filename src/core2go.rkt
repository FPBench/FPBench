#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide go-header core->go go-supported)

; convoluted solution to deal with Go's hatred of unused variables
(define go-header (curry format (string-append "package ~a\n\nimport \"math\"\n\n// Helper function to get rid of annoying unused variable errors\n"
                                               "func Use(vals ...interface{}) {\n\tfor _, val := range vals {\n\t\t_ = val\n\t}\n}\n\n"
                                               "func Lgamma(x float64) float64 {\n\tres, _ := math.Lgamma(x)\n\treturn res\n}\n\n")))
(define go-supported (supported-list
   (invert-op-list '(isnormal isfinite)) 
   (invert-const-list '(M_1_PI M_2_PI M_2_SQRTPI SQRT1_2))
   '(binary64)
   '(nearestEven)))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define (operator->go type operator args)
  (define arg-list (string-join args ", "))
  (match operator
    ['isinf (format "math.IsInf(~a, 0)" arg-list)]
    ['isnan (format "math.IsNaN(~a)" arg-list)]
    ['tgamma (format "math.Gamma(~a)" arg-list)]
    ['lgamma (format "Lgamma(~a)" arg-list)]
    ['log1p (format "math.Log1p(~a)" arg-list)]
    ['fma (format "math.FMA(~a)" arg-list)]
    ['fmod (format "math.Mod(~a)" arg-list)]
    ['nearbyint (format "math.RoundToEven(~a)" arg-list)]
    [(or 'fabs 'fmax 'fmin 'fdim 'fmod)
      (format "math.~a(~a)" (string-titlecase (substring (~a operator) 1)) arg-list)]
    [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'acos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'ceil 'floor
         'remainder 'copysign 'signbit 'trunc 'round)
     (format "math.~a(~a)" (string-titlecase (~a operator)) arg-list)]))

(define (constant->go props expr)
  (define type (type->go (dict-ref props ':precision 'binary64)))
  (match expr
    ['E "math.E"]
    ['LOG2E "math.Log2E"] 
    ['LOG10E "math.Log10E"] 
    ['LN2 "math.Ln2"] 
    ['LN10 "math.Ln10"]
    ['PI "math.Pi"] 
    ['PI_2 "math.Pi/2"] 
    ['PI_4 "math.Pi/4"] 
    ['SQRT2 "math.Sqrt2"]
    ['MAXFLOAT "math.MaxFloat64"] 
    ['INFINITY "math.Inf(1)"] 
    ['NAN "math.NaN()"]
    ['TRUE "true"]
    ['FALSE "false"]
    [(? hex?) (format "~a" expr)]
    [(? number?) (~a (real->double-flonum expr))]
    [(? symbol?) expr]))

(define (declaration->go props var [val #f])
  (define type (type->go (dict-ref props ':precision 'binary64)))
  (if val
      (format "var ~a = ~a(~a)" var type val)
      (format "var ~a ~a" var type)))

(define (assignment->go var val)
  (format "~a = ~a" var val))

(define (round->go val props) (~a val)) ; round(val) = val

(define (function->go name args arg-props body return ctx vars)
  (define type (type->go (ctx-lookup-prop ctx ':precision 'binary64)))
  (format "func ~a(~a) ~a {\n~a\treturn ~a\n}\n"
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
