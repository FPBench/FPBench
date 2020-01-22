#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide core->js js-runtime js-supported)

(define js-runtime (make-parameter "Math"))

;; JS

(define js-name (const "js"))
(define js-header (const "")) ; empty
(define js-supported (supported-list
   (unsupported-ops->supported '(!= copysign exp2 erf erfc fdim fma fmod isfinite isnormal lgamma nearbyint remainder signbit tgamma))
   (unsupported-const->supported '())
   '(binary64)))

(define (type->js type) "var")

(define (operator->js type op)
  (match op
    ['fabs  (format "~a.abs(~a)" (js-runtime) "~a")]
    ['fmax  (format "~a.max(~a)" (js-runtime) "~a")]
    ['fmin  (format "~a.min(~a)" (js-runtime) "~a")]
    ['isinf (format "(~a.abs(~a) === Infinity)" (js-runtime) "~a")]
    ['isnan "isNaN(~a)"]
    [_      (format "~a.~a(~a)" (js-runtime) op "~a")]))

(define/match (constant->js type expr)
  [(_ 'E) "Math.E"]
  [(_ 'LOG2E) "Math.LOG2E"]
  [(_ 'LOG10E) "Math.LOG10E"]
  [(_ 'LN2) "Math.LN2"]
  [(_ 'LN10) "Math.LN10"]
  [(_ 'PI) "Math.PI"]
  [(_ 'PI_2) "(Math.PI/2)"]
  [(_ 'PI_4) "(Math.PI/4)"]
  [(_ 'M_1_PI) "(1/Math.PI)"]
  [(_ 'M_2_PI) "(2/Math.PI)"]
  [(_ 'M_2_SQRTPI) "(2/Math.sqrt(Math.PI))"]
  [(_ 'SQRT2) "Math.SQRT2"]
  [(_ 'SQRT1_2) "(Math.SQRT1_2)"]
  [(_ 'MAXFLOAT) "Number.MAX_VALUE"]
  [(_ 'TRUE) "true"]
  [(_ 'FALSE) "false"]
  [(_ 'INFINITY) "Infinity"]
  [(_ 'NAN) "NaN"]
  [(_ (? symbol?)) expr]
  [(_ (? number?)) (format "~a" (real->double-flonum expr))])

(define (decleration->js type var [val #f])
  (if val
    (format "~a ~a = ~a;" type var val)
    (format "~a ~a;" type var)))

(define (assignment->js var val)
  (format "~a = ~a;" var val))

(define (function->js type name args body return)
  (format "function ~a(~a) {\n~a\treturn ~a;\n}\n"
          name 
          (string-join args ", ") 
          body 
          return))

(define js-language (language js-name type->js operator->js constant->js decleration->js assignment->js function->js))

;;; Exports

(define (core->js prog name) (parameterize ([*lang* js-language]) (convert-core prog name)))
(define-compiler '("js") js-header core->js (const "") js-supported)