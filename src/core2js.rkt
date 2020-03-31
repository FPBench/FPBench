#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide core->js js-runtime js-supported)

(define js-runtime (make-parameter "Math"))

;; JS

(define js-header (const "")) ; empty
(define js-supported (supported-list
   (invert-op-list'(!= copysign exp2 erf erfc fdim fma fmod isfinite isnormal lgamma nearbyint remainder signbit tgamma))
   (invert-const-list '())
   '(binary64)
   '(nearestEven)))

(define (type->js type) "var")

(define (operator->js props op args)
  (define arg-list (string-join args ", "))
  (match op
    ['fabs  (format "~a.abs(~a)" (js-runtime) arg-list)]
    ['fmax  (format "~a.max(~a)" (js-runtime) arg-list)]
    ['fmin  (format "~a.min(~a)" (js-runtime) arg-list)]
    ['isinf (format "(~a.abs(~a) === Infinity)" (js-runtime) arg-list)]
    ['isnan (format "isNaN(~a)" arg-list)]
    [_      (format "~a.~a(~a)" (js-runtime) op arg-list)]))

(define (constant->js props expr)
  (match expr
    ['E "Math.E"]
    ['LOG2E "Math.LOG2E"]
    ['LOG10E "Math.LOG10E"]
    ['LN2 "Math.LN2"]
    ['LN10 "Math.LN10"]
    ['PI "Math.PI"]
    ['PI_2 "(Math.PI/2)"]
    ['PI_4 "(Math.PI/4)"]
    ['M_1_PI "(1/Math.PI)"]
    ['M_2_PI "(2/Math.PI)"]
    ['M_2_SQRTPI "(2/Math.sqrt(Math.PI))"]
    ['SQRT2 "Math.SQRT2"]
    ['SQRT1_2 "(Math.SQRT1_2)"]
    ['MAXFLOAT "Number.MAX_VALUE"]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY "Infinity"]
    ['NAN "NaN"]
    [(? symbol?) expr]
    [(? number?) (format "~a" (real->double-flonum expr))]))

(define (decleration->js props var [val #f])
  (if val
    (format "var ~a = ~a;" var val)
    (format "var ~a;" var)))

(define (assignment->js var val)
  (format "~a = ~a;" var val))

(define (round->js val props) (format "~a" val)) ; round(val) = val

(define (function->js name args arg-props body return ctx vars)
  (format "function ~a(~a) {\n~a\treturn ~a;\n}\n"
          name 
          (string-join args ", ") 
          body 
          return))

(define js-language (language (const "js") operator->js constant->js decleration->js assignment->js
                              round->js (const "") function->js))

;;; Exports

(define (core->js prog name) (parameterize ([*lang* js-language]) (convert-core prog name)))
(define-compiler '("js") js-header core->js (const "") js-supported)