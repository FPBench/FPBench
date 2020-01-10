#lang racket

(require "common.rkt" "imperative.rkt" "compilers.rkt")
(provide js-header core->js)

;; JS

(define js-name (const "js"))
(define js-header (const "")) ; empty
(define (type->js type) "var")

(define/match (operator->js type op)
  [(_ 'fabs) "Math.abs(~a)"]
  [(_ 'exp) "Math.exp(~a)"]
  ;[('exp2) "math.pow(2, ~a)"]
  [(_ 'expm1) "Math.expm1(~a)"]
  [(_ 'log) "Math.log(~a)"]
  [(_ 'log10) "Math.log10(~a)"]
  [(_ 'log2) "Math.log2(~a)"]
  [(_ 'log1p) "Math.log1p(~a)"]
  ;[('logb) "math.floor(math.log2(math.abs(~a)))"]
  [(_ 'pow) "Math.pow(~a, ~a)"]
  [(_ 'sqrt) "Math.sqrt(~a)"]
  [(_ 'cbrt) "Math.cbrt(~a)"]
  [(_ 'hypot) "Math.hypot(~a, ~a)"]
  [(_ 'sin) "Math.sin(~a)"]
  [(_ 'cos) "Math.cos(~a)"]
  [(_ 'tan) "Math.tan(~a)"]
  [(_ 'asin) "Math.asin(~a)"]
  [(_ 'acos) "Math.acos(~a)"]
  [(_ 'atan) "Math.atan(~a)"]
  [(_ 'atan2) "Math.atan2(~a, ~a)"]
  [(_ 'sinh) "Math.sinh(~a)"]
  [(_ 'cosh) "Math.cosh(~a)"]
  [(_ 'tanh) "Math.tanh(~a)"]
  [(_ 'asinh) "Math.asinh(~a)"]
  [(_ 'acosh) "Math.acosh(~a)"]
  [(_ 'atanh) "Math.atanh(~a)"]
  ;[('erf) "math.erf(~a)"]
  ;[('erfc) "1 - math.erf(~a)"] ;; TODO: This implementation has large error for large inputs
  ;[('tgamma) "math.gamma(~a)"]
  ;[('lgamma) "math.log(math.gamma(~a))"]
  [(_ 'ceil) "Math.ceil(~a)"]
  [(_ 'floor) "Math.floor(~a)"]
  ;[('remainder) "math.mod(~a, ~a)"]
  [(_ 'fmax) "Math.max(~a, ~a)"]
  [(_ 'fmin) "Math.min(~a, ~a)"]
  ;[('fdim) "math.max(0, ~a - ~a)"]
  ;[('copysign) "math.abs(~a) * math.sign(~a)"]
  [(_ 'trunc) "Math.trunc(~a)"]
  [(_ 'round) "Math.round(~a)"]
  [(_ 'isinf) "(Math.abs(~a) === Infinity)"]
  [(_ 'isnan) "isNaN(~a)"]
  [(_ _) (error 'operator->js "Unsupported operator ~a" op)])

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
  [(_ symbol?) expr]
  [(_ number?) expr]
  [(_ _) (error 'constant->js "Unsupported constant ~a" expr)])

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

(define js-language (language js-name js-header type->js operator->js constant->js decleration->js assignment->js function->js))

;;; Exports

(define (core->js prog name) (parameterize ([*lang* js-language]) (convert-core prog name)))
(define-compiler '("js") js-header core->js (const "") '(!))