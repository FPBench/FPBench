#lang racket

(require "imperative.rkt")

(provide go-header go-func-header core->go go-supported)

(define go-func-header
  (string-append
    "func Lgamma(x float64) float64 { res, _ := math.Lgamma(x); return res; }\n"
    "func Fmax(x float64, y float64) float64 { if x != x { return y; } else if y != y { return x; } else { return math.Max(x, y); }}\n"
    "func Fmin(x float64, y float64) float64 { if x != x { return y; } else if y != y { return x; } else { return math.Min(x, y); }}\n\n"))

(define go-header 
  (curry format 
    (string-append 
      "package ~a\n\nimport \"math\"\n\n// Helper function to get rid of annoying unused variable errors\n"
      "func Use(vals ...interface{}) { for _, val := range vals { _ = val }}\n"  ; convoluted solution to deal with Go's hatred of unused variables
      go-func-header)))
                   
(define go-supported 
  (supported-list
    (invert-op-proc
      (curry set-member?
            '(fma isnormal isfinite array dim size ref
              array dim size ref for for* tensor tensor*)))
    (invert-const-proc (curry set-member? '(M_1_PI M_2_PI M_2_SQRTPI SQRT1_2)))
    (curry equal? 'binary64)
    (curry equal? 'nearestEven)
    #f))

(define go-reserved   ; Language-specific reserved names (avoid name collisions)
  '(break case chan const continue default defer else
    fallthrough for func go goto if import interface map
    package range return select struct switch type var))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define (operator->go op args ctx)
  (define args* (string-join args ", "))
  (match op
    ['/     (format "(~a / ~a)" (first args) (second args))]
    ['isinf (format "math.IsInf(~a, 0)" args*)]
    ['isnan (format "math.IsNaN(~a)" args*)]
    ['tgamma (format "math.Gamma(~a)" args*)]
    ['lgamma (format "Lgamma(~a)" args*)]
    ['log1p (format "math.Log1p(~a)" args*)]
    ['fma (format "math.FMA(~a)" args*)]
    ['fmax (format "Fmax(~a)" args*)]
    ['fmin (format "Fmin(~a)" args*)]
    ['nearbyint (format "math.RoundToEven(~a)" args*)]
    [(or 'fabs 'fdim 'fmod)
      (format "math.~a(~a)" (string-titlecase (substring (~a op) 1)) args*)]
    [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'acos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'ceil 'floor
         'remainder 'copysign 'signbit 'trunc 'round)
     (format "math.~a(~a)" (string-titlecase (~a op)) args*)]))

(define (constant->go x ctx)
  (match x
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
    [(? hex?) (~a x)]
    [(? number?) (~a (real->double-flonum x))]
    [(? symbol?) (~a x)]))

(define declaration->go
  (case-lambda
   [(var ctx)
    (define type (type->go (ctx-lookup-prop ctx ':precision)))
    (format "var ~a ~a" var type)]
   [(var val ctx)
    (define type (type->go (ctx-lookup-prop ctx ':precision)))
    (format "var ~a = ~a(~a)" var type val)]))

(define (use-vars->go vars ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (format "~aUse(~a)\n" indent (string-join vars ", ")))

(define (params->go args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->go (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" arg type)))
    ", "))

(define (program->go name args arg-ctxs body return ctx vars)
  (define type (type->go (ctx-lookup-prop ctx ':precision)))
  (format "func ~a(~a) ~a {\n~a\treturn ~a\n}\n"
          name (params->go args arg-ctxs) type
          body return))


(define core->go
  (make-imperative-compiler "go"
    #:operator operator->go
    #:constant constant->go
    #:type type->go
    #:declare declaration->go
    #:use-vars use-vars->go
    #:program program->go
    #:flags '(no-parens-around-condition
              for-instead-of-while)
    #:reserved go-reserved))

(define-compiler '("go") go-header core->go (const "") go-supported)
