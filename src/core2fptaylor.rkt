#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(require "fpcore-extra.rkt" "range-analysis.rkt")
(provide core->fptaylor fptaylor-supported *fptaylor-inexact-scale*)

(define *fptaylor-inexact-scale* (make-parameter 1))

(define fptaylor-supported 
  (supported-list
    (invert-op-proc 
      (curry set-member?
             '(atan2 cbrt ceil copysign erf erfc exp2 expm1 fdim floor fmod hypot if 
              lgamma log10 log1p log2 nearbyint pow remainder round tgamma trunc while while*)))
    fpcore-consts
    (curry set-member? '(binary16 binary32 binary64 binary128 real))
    ; Note: nearestEven and nearestAway behave identically in FPTaylor
    ieee754-rounding-modes))

; Language-specific reserved names (avoid name collisions)
(define fptaylor-reserved 
  '(Variables variables Definitions definitions Expressions expressions Constraints constraints
    IN in int real float16 float32 float64 float128
    rnd no_rnd rnd16_ne rnd16 rnd16_0 rnd16_down rnd16_up
    rnd32_ne rnd32 rnd32_0 rnd32_down rnd32_up
    rnd64_ne rnd64 rnd64_0 rnd64_down rnd64_up
    rnd128_ne rnd128 rnd128_0 rnd128_down rnd128_up
    inv abs fma sqrt min max exp log cos sin tan cosh sinh tanh
    acos asin atan atan2 arccos arcsin arctan acosh asinh atanh
    arsinh arcosh artanh arcsinh arccosh arctanh argsinh argcosh argtanh
    sub2 floor_power2 interval))

(define (inexact-operator? op)
  (set-member? '(exp log sin cos tan asin acos atan
                sinh cosh tanh asinh acosh atanh) op))

(define/match (prec->fptaylor prec)
  [(#f) ""]
  [('real) "real"]
  [('binary16) "float16"]
  [('binary32) "float32"]
  [('binary64) "float64"]
  [('binary128) "float128"]
  [(_) (error 'prec->fptaylor "Unsupported precision ~a" prec)])

(define/match (rm->fptaylor rm)
  [('nearestEven) "ne"]
  ; The same as 'nearestEven
  [('nearestAway) "ne"]
  [('toPositive) "up"]
  [('toNegative) "down"]
  [('toZero) "zero"]
  [(_) (error 'rm->fptaylor "Unsupported rounding mode ~a" rm)])

(define (round->fptaylor expr props #:scale [scale 1])
  (define prec (dict-ref props ':precision 'real))
  (define rm (rm->fptaylor (dict-ref props ':round 'nearestEven)))
  (define bits
    (match prec
      [#f "undefined"]
      ['real ""]
      ['binary16 "16"]
      ['binary32 "32"]
      ['binary64 "64"]
      ['binary128 "128"]
      [_ (error 'round->fptaylor "Unsupported precision ~a" prec)]))
  (cond
    [(equal? bits "undefined") format "rnd(~a)" expr]
    [(equal? bits "") expr]
    [(and (equal? rm "ne") (= scale 1)) (format "rnd~a(~a)" bits expr)]
    [else (format "rnd[~a,~a,~a](~a)" bits rm scale expr)]))

(define (operator->fptaylor props op args)
  (match (cons op args)
    [(list '/ a b) (round->fptaylor (format "(~a / ~a)" a b) props)]
    [(list 'fabs a) (round->fptaylor (format "abs(~a)" a) props)]
    [(list 'fmax a b) (round->fptaylor (format "max(~a, ~a)" a b) props)]
    [(list 'fmin a b) (round->fptaylor (format "min(~a, ~a)" a b) props)]
    [(list 'fma a b c) (round->fptaylor (format "((~a * ~a) + ~a)" a b c) props)]
    [(list (? inexact-operator? f) args ...)
        (round->fptaylor (format "~a(~a)" f (string-join args ", ")) props #:scale (*fptaylor-inexact-scale*))]
    [(list (? operator? f) args ...)
        (round->fptaylor (format "~a(~a)" f (string-join args ", ")) props)]))

(define constant->expr
  (match-lambda
    ['E "exp(1)"]
    ['LN2 "log(2)"]
    ['LN10 "log(10)"]
    ['PI "4 * atan(1)"]
    ['PI_2 "2 * atan(1)"]
    ['PI_4 "atan(1)"]
    ['M_1_PI "1 / (4 * atan(1))"]
    ['M_2_PI "1 / (2 * atan(1))"]
    ['M_2_SQRTPI "1 / sqrt(atan(1))"]
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "1 / sqrt(2)"]
    [(? hex? expr) (format "~a" expr)]
    [(? number? expr) (format "~a" expr)]
    [c (error 'constant->expr "Unsupported constant ~a" c)]))

(define (constant->fptaylor props expr)
  (define cexpr (constant->expr expr))
  (round->fptaylor cexpr props))

(define (declaration->fptaylor props var [val #f])
  (error 'declaration->fptaylor "Unsupported operation"))

(define (assignment->fptaylor var val)
  (format "~a = ~a;" var val))

(define (function->fptaylor name args arg-props body return ctx vars)
  (define arg-rounding
    (filter (compose not void?)
        (for/list ([var args] [prop arg-props] 
                  #:unless (equal? (prec->fptaylor (dict-ref prop ':precision 'real)) "real"))
          (format "\t~a = ~a;" var (round->fptaylor var prop)))))
  (define decl-list
    (set-subtract vars (set-add args name)))

  (define var-string
    (if (> (length decl-list) 0)
        (format "\n\tvar ~a;" (string-join (map (λ (x) (format "~a" x)) decl-list) ", "))
        ""))
  (define rounding-string
    (if (> (length arg-rounding) 0)
        (format "\n~a" (string-join arg-rounding "\n"))
        ""))

  (format "procedure ~a(~a) {~a~a\n~a\t~a;\n};\n"
        name
        (string-join args ", ")
        var-string
        rounding-string
        body
        (trim-infix-parens return)))

(define fptaylor-language 
  (language "fptaylor" 
    operator->fptaylor
    constant->fptaylor 
    declaration->fptaylor 
    assignment->fptaylor
    round->fptaylor
    (const "") 
    function->fptaylor))

;;; Exports

(define (core->fptaylor prog name) 
  (parameterize ([*lang* fptaylor-language] 
                 [*reserved-names* fptaylor-reserved]
                 [*fix-name-format* "_"])
    (convert-core prog name)))

(define-compiler 
  '("fptaylor" "fpt") 
  (const "") 
  core->fptaylor
  (const "") 
  fptaylor-supported)