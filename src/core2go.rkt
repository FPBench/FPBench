#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide go-header core->go go-supported)

;; Go

; convoluted solution to deal with Go's hatred of unused variables
(define go-header (curry format (string-append "package ~a\n\nimport \"math\"\n\n// Helper function to get rid of annoying unused variable errors\n"
                                               "func Use(vals ...interface{}) {\n\tfor _, val := range vals {\n\t\t_ = val\n\t}\n}\n\n")))
(define go-supported (supported-list
   (invert-op-list '(fmod fma isfinite isnormal lgamma log1p remainder)) 
   (invert-const-list '(M_1_PI M_2_PI M_2_SQRTPI SQRT1_2))
   '(binary64)))  ; math operations only allow float64

; Inaccurate ops: fmod, lgamma (returns two values), log1p, remainder
; Unsupported ops: isfinite isnormal
; Unknown: fma (a*b+c might be implicitly converted to a fma instruction)

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
    ['lgamma (format "math.Lgamma(~a)" arg-list)]
    ['nearbyint (format "math.RoundToEven(~a)" arg-list)]
    [(or 'fabs 'fmax 'fmin 'fdim 'fmod)
      (format "math.~a(~a)" (string-titlecase (substring (~a operator) 1)) arg-list)]
    [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'acos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
         'remainder 'copysign 'signbit 'trunc 'round)
     (format "math.~a(~a)" (string-titlecase (~a operator)) arg-list)]))

(define (constant->go type expr)
  (match expr
    ['TRUE "true"]
    ['FALSE "false"]
    [(? symbol?)
     (define name
       (match expr
         ['E "E"]
         ['LOG2E "Log2E"] 
         ['LOG10E "Log10E"] 
         ['LN2 "Ln2"] 
         ['LN10 "Ln10"]
         ['PI "Pi"] 
         ['PI_2 "Pi/2"] 
         ['PI_4 "Pi/4"] 
         ['SQRT2 "Sqrt2"]
         ['MAXFLOAT "MaxFloat64"] 
         ['INFINITY "Inf(1)"] 
         ['NAN "NaN()"]))
     (format "~a(math.~a)" type name)]
    [(? number?) (~a (real->double-flonum expr))]))

(define (declaration->go type var indent [val #f])
  (string-append
    (if val
      (format "var ~a = ~a(~a)" var type val)
      (format "var ~a ~a" var type))
    (format "\n~aUse(~a)" indent var)))

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

(define go-language (language "go" type->go operator->go constant->go declaration->go assignment->go function->go))

;;; Exports

(define (core->go prog name) (parameterize ([*lang* go-language]) (convert-core prog name)))
(define-compiler '("go") go-header core->go (const "") go-supported)