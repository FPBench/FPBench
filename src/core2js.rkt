#lang racket

(require "common.rkt" "imperative.rkt" "compilers.rkt")
(provide js-header core->js)

;; JS

(define js-header (const "")) ; empty

(define (type->js type) "var")

(define (operator->js type operator)
  (operator->js-notype operator))

(define/match (operator->js-notype op)
  [((or '== '+ '- '* '/  '< '> '<= '>=))
    (format "(~a ~a ~a)" "~a" op "~a")]
  [('and) "~a && ~a"]
  [('or) "~a || ~a"]
  [('not) "!~a"]
  [('fabs) "Math.abs(~a)"]
  [('exp) "Math.exp(~a)"]
  ;[('exp2) "math.pow(2, ~a)"]
  [('expm1) "Math.expm1(~a)"]
  [('log) "Math.log(~a)"]
  [('log10) "Math.log10(~a)"]
  [('log2) "Math.log2(~a)"]
  [('log1p) "Math.log1p(~a)"]
  ;[('logb) "math.floor(math.log2(math.abs(~a)))"]
  [('pow) "Math.pow(~a, ~a)"]
  [('sqrt) "Math.sqrt(~a)"]
  [('cbrt) "Math.cbrt(~a)"]
  [('hypot) "Math.hypot(~a, ~a)"]
  [('sin) "Math.sin(~a)"]
  [('cos) "Math.cos(~a)"]
  [('tan) "Math.tan(~a)"]
  [('asin) "Math.asin(~a)"]
  [('acos) "Math.acos(~a)"]
  [('atan) "Math.atan(~a)"]
  [('atan2) "Math.atan2(~a, ~a)"]
  [('sinh) "Math.sinh(~a)"]
  [('cosh) "Math.cosh(~a)"]
  [('tanh) "Math.tanh(~a)"]
  [('asinh) "Math.asinh(~a)"]
  [('acosh) "Math.acosh(~a)"]
  [('atanh) "Math.atanh(~a)"]
  ;[('erf) "math.erf(~a)"]
  ;[('erfc) "1 - math.erf(~a)"] ;; TODO: This implementation has large error for large inputs
  ;[('tgamma) "math.gamma(~a)"]
  ;[('lgamma) "math.log(math.gamma(~a))"]
  [('ceil) "Math.ceil(~a)"]
  [('floor) "Math.floor(~a)"]
  ;[('remainder) "math.mod(~a, ~a)"]
  [('fmax) "Math.max(~a, ~a)"]
  [('fmin) "Math.min(~a, ~a)"]
  ;[('fdim) "math.max(0, ~a - ~a)"]
  ;[('copysign) "math.abs(~a) * math.sign(~a)"]
  [('trunc) "Math.trunc(~a)"]
  [('round) "Math.round(~a)"]
  [('isinf)  "(Math.abs(~a) === Infinity)"]
  [('isnan) "isNaN(~a)"]
  [(_) (error 'operator->js-notype "Unsupported operator ~a" op)])

(define/match (constant->js type expr)
  [(? 'E) "Math.E"]
  [(? 'LOG2E) "Math.LOG2E"]
  [(? 'LOG10E) "Math.LOG10E"]
  [(? 'LN2) "Math.LN2"]
  [(? 'LN10) "Math.LN10"]
  [(? 'PI) "Math.PI"]
  [(? 'PI_2) "(Math.PI/2)"]
  [(? 'PI_4) "(Math.PI/4)"]
  [(? 'M_1_PI) "(1/Math.PI)"]
  [(? 'M_2_PI) "(2/Math.PI)"]
  [(? 'M_2_SQRTPI) "(2/Math.sqrt(Math.PI))"]
  [(? 'SQRT2) "Math.SQRT2"]
  [(? 'SQRT1_2) "(Math.SQRT1_2)"]
  [(? 'MAXFLOAT) "Number.MAX_VALUE"]
  [(? 'TRUE) "true"]
  [(? 'FALSE) "false"]
  [(? 'INFINITY) "Infinity"]
  [(? 'NAN) "NaN"]
  [(? symbol?) expr]
  [(? number?) expr]
  [(? _) (error 'constant->js "Unsupported constant ~a" expr)])

(define (decleration->js type var [val #f])
  (if val
    (format "~a ~a = ~a;" type var val)
    (format "~a ~a;" type var)))

(define (assignment->js var val)
  (format "~a = ~a;" var val))

(define (function->js type name args body return)
  (format "function ~a(~a) {\n~a\treturn ~a;\n}\n"
          name
          (string-join
           (map (λ (arg) (format "~a" arg)) args)
           ", ")
          body return))

(define js-language (language js-header type->js operator->js constant->js decleration->js assignment->js function->js))

;;; Exports

(define (core->js prog name) (parameterize ([*lang* js-language]) (convert-core prog name)))

(define-compiler '("js")
  js-header core->js (const "")
  '(!))