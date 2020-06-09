#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide js-header core->js js-runtime js-supported)

(define js-runtime (make-parameter "Math"))

;; JS

(define js-header
  (λ (x) 
   (apply string-append
    (append
      (map (curryr format x)
          '("function fmax(x , y) { if (x != x) { return y; } else if (y != y) { return x; } else { return ~a.max(x, y); }}\n"
            "function fmin(x , y) { if (x != x) { return y; } else if (y != y) { return x; } else { return ~a.min(x, y); }}\n"))
     '("function fdim(x , y) { if (x != x || y != y) { return NaN; } else if (x > y) { return x - y; } else { return 0; }}\n\n")))))

(define js-supported (supported-list
   (invert-op-list '(!= copysign exp2 erf erfc fma fmod isfinite isnormal lgamma nearbyint remainder signbit tgamma))
   fpcore-consts
   '(binary64)
   '(nearestEven)))

(define js-reserved '())  ; Language-specific reserved names (avoid name collisions)

(define (type->js type) "var")

(define (operator->js props op args)
  (define arg-list (string-join args ", "))
  (match op
    ['fabs  (format "~a.abs(~a)" (js-runtime) arg-list)]
    ['fmax  (format "fmax(~a)" arg-list)]
    ['fmin  (format "fmin(~a)" arg-list)]
    ['fdim  (format "fdim(~a)" arg-list)]
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
    [(? hex?) ((compose ~a real->double-flonum hex->racket) expr)]
    [(? number?) (format "~a" (real->double-flonum expr))]
    [(? symbol?) expr]))

(define (decleration->js props var [val #f])
  (if val
    (format "var ~a = ~a;" var val)
    (format "var ~a;" var)))

(define (assignment->js var val)
  (format "~a = ~a;" var val))

(define (round->js val props) (~a val)) ; round(val) = val

(define (function->js name args arg-props body return ctx vars)
  (format "function ~a(~a) {\n~a\treturn ~a;\n}\n"
          name 
          (string-join args ", ") 
          body 
          return))

(define js-language (language (const "js") operator->js constant->js decleration->js assignment->js
                              round->js (const "") function->js))

;;; Exports

(define (core->js prog name) (parameterize ([*lang* js-language] [*reserved-names* js-reserved]) (convert-core prog name)))
(define-compiler '("js") js-header core->js (const "") js-supported)