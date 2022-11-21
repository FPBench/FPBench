#lang racket

(require "imperative.rkt")

(provide core->sollya sollya-supported sollya-header *sollya-warnings*)

(define sollya-supported 
  (supported-list
    (invert-op-proc 
      (curry set-member?
            '(isnormal tgamma lgamma remainder fmod round
              cbrt atan2 signbit array dim size ref
              for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64 binary80 integer))
    ieee754-rounding-modes
    #f))

(define *sollya-warnings* (make-parameter #t))

(define sollya-reserved ; Language-specific reserved names (avoid name collisions)
  '(_x_ boolean constant default do else for from function
    if in integer list nop object of pi proc procedure
    range return switch then time to while var))

(define sollya-header
  (const
    (string-append
      "procedure copysign(x, y) { var res; if (y < 0) then res = -abs(x) else res = abs(x); return res; };\n"
      "procedure fdim(x, y) { var res; if (x != x || y != y) then res = nan else if (x > y) then res = x - y else res = 0; return res; };\n"
      "procedure isfinite(x) { return (x == x && abs(x) != infty); };\n"
      "procedure isinf(x) { return (abs(x) == infty); };\n"
      "procedure isnan(x) { return (x != x); };\n"
      "procedure trunc(x) { var res; if (x < 0) then res = ceil(x) else res = floor(x); return res; };\n"
      "procedure pow(x, y) { var res; if (x == 1 && y != y) then res = 1 else res = (x ^ y); return res; };\n"
      "procedure sin_libm(x) { var res; if (abs(x) == infty) then res = nan else res = sin(x); return res; };\n"
      "procedure cos_libm(x) { var res; if (abs(x) == infty) then res = nan else res = cos(x); return res; };\n"
      "procedure div_warn(x, y) { if (x != 0 && y == 0) then print(\"[WARNING] FPBench: Division by zero. Sollya always returns NaN.\"); return (x / y); };\n\n")))

(define (precision-str prec)
  (match prec
    ['binary16 "halfprecision"]
    ['binary32 "single"]
    ['binary64 "double"]
    ['binary80 "doubleextended"]
    ['binary128 "quad"]
    ;; this will round to the right amount of precision, but not limit the exponent
    [(list 'float es nbits) (~a (- nbits es))]
    ;; real is just a keyword that tells us to omit rounding
    ['real "real"]
    ;; hopefully integers have been selected to behave like reals
    ['integer "real"]
    [_ (error 'constant->sollya "Unsupported constant precision ~a" prec)]))

(define (round-str rm)
  (match rm
    ['nearestEven "RN"]
    ['nearestAway (error 'round->sollya "Nearest away rounding is not supported by Sollya tool.")]
    ['toPositive "RU"]
    ['toNegative "RD"]
    ['toZero "RZ"]
    [_ (error 'round->sollya "Unsupported rounding mode ~a" rm)]))

(define (round->sollya expr ctx)
  (define prec (precision-str (ctx-lookup-prop ctx ':precision)))
  (define rm (round-str (ctx-lookup-prop ctx ':round)))
  (if (equal? prec "real")
      expr
      (format "round(~a, ~a, ~a)" expr prec rm)))

(define (operator->sollya op args ctx)
  (match (cons op args)
    [(list '/ a b)        (round->sollya (format "div_warn(~a, ~a)" a b) ctx)]
    [(list 'fabs a)       (round->sollya (format "abs(~a)" a) ctx)]
    [(list 'fmax a b)     (round->sollya (format "max(~a, ~a)" a b) ctx)]
    [(list 'fmin a b)     (round->sollya (format "min(~a, ~a)" a b) ctx)]
    [(list 'fma a b c)    (round->sollya (format "((~a * ~a) + ~a)" a b c) ctx)]
    [(list 'exp2 a)       (round->sollya (format "(2 ^ ~a)" a) ctx)]
    [(list 'cbrt a)       (round->sollya (format "(~a ^ (1/3))" a) ctx)]
    [(list 'hypot a b)    (round->sollya (format "sqrt((~a ^ 2) + (~a ^ 2))" a b) ctx)]
    [(list 'sin a)        (round->sollya (format "sin_libm(~a)" a) ctx)]
    [(list 'cos a)        (round->sollya (format "cos_libm(~a)" a) ctx)]
    [(list 'atan2 a b)    (round->sollya (format "atan(~a / ~a)" a b) ctx)]
    [(list 'nearbyint a)    
      (match (round-str (ctx-lookup-prop ctx ':round))
       ["RN" (round->sollya (format "nearestint(~a)" a) ctx)]
       ["RU" (round->sollya (format "ceil(~a)" a) ctx)]
       ["RD" (round->sollya (format "floor(~a)" a) ctx)]
       ["RZ" (round->sollya (format "trunc(~a)" a) ctx)])]
    [(list (or 'isnan 'isinf 'isfinite) a)
      (format "~a(~a)" op a)]
    [(list (? operator? f) args ...)
      (round->sollya (format "~a(~a)" f (string-join args ", ")) ctx)]))

(define (constant->sollya expr ctx)
  (match expr
    ['E (round->sollya "exp(1)" ctx)]
    ['LOG2E (round->sollya "log2(exp(1))" ctx)]
    ['LOG10E (round->sollya "log10(exp(1))" ctx)]
    ['LN2 (round->sollya "log(2)" ctx)]
    ['LN10 (round->sollya "log(10)" ctx)]
    ['PI (round->sollya "pi" ctx)]
    ['PI_2 (round->sollya "(pi/2)" ctx)]
    ['PI_4 (round->sollya "(pi/4)" ctx)]
    ['M_1_PI (round->sollya "(1/pi)" ctx)]
    ['M_2_PI (round->sollya "(2/pi)" ctx)]
    ['M_2_SQRTPI (round->sollya "(2/sqrt(pi))" ctx)]
    ['SQRT2 (round->sollya "sqrt(2)" ctx)]
    ['SQRT1_2 (round->sollya "sqrt(1/2)" ctx)]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY "infty"]
    ['NAN "nan"]
    [(? hex?) (round->sollya (~a expr) ctx)]
    [(? symbol?) (~a expr)]
    [(? number?) (round->sollya (~a expr) ctx)]))

(define (program->sollya name args arg-ctxs body ret ctx used-vars)
  (define arg-rounding
    (filter-not void?
      (for/list ([var args] [ctx arg-ctxs]
                #:unless (equal? (precision-str (ctx-lookup-prop ctx ':precision)) "real"))
        (format "\t~a = ~a;" var (round->sollya var ctx)))))
  (define decl-list (set-subtract used-vars (set-add args name)))

  (define var-string
    (if (> (length decl-list) 0)
        (format "\n\tvar ~a;" (string-join (map ~a decl-list) ", "))
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
          (trim-infix-parens ret)))

; Override visitor behavior
(define-expr-visitor imperative-visitor sollya-visitor
  [(visit-! vtor props body #:ctx ctx)
    (visit/ctx vtor body (ctx-update-props ctx props))])

(define (sollya-infix-ops)
  (if (*sollya-warnings*)
      (remove '/ default-infix-ops)
      default-infix-ops))

(define core->sollya
  (make-imperative-compiler "sollya"
    #:infix-ops (sollya-infix-ops)
    #:operator operator->sollya
    #:constant constant->sollya
    #:round round->sollya
    #:program program->sollya
    #:flags '(semicolon-after-enclosing-brace
              if-then
              while-do
              never-declare
              round-after-operation)
    #:reserved sollya-reserved
    #:visitor sollya-visitor))

(define-compiler '("sollya") sollya-header core->sollya (const "") sollya-supported)
