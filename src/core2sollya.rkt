#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide core->sollya sollya-supported sollya-header)

(define sollya-supported (supported-list
  (invert-op-list '(isnormal tgamma lgamma remainder fmod round cbrt atan2 erf))
  fpcore-consts
  '(binary32 binary64)
  ieee754-rounding-modes))

(define sollya-header (const
  (string-append
    "procedure copysign(x, y) { var res; if (y < 0) then res = -abs(x) else res = abs(x); return res; };\n"
    "procedure fdim(x, y) { var res; if (x > y) then res = x - y else res = 0; return res; };\n"
    "procedure isfinite(x) { return (x == x && abs(x) != infty); };\n"
    "procedure isinf(x) { return (abs(x) == infty); };\n"
    "procedure isnan(x) { return (x != x); };\n"
    "procedure signbit(x) { return (x < 0); };\n"
    "procedure trunc(x) { var res; if (x < 0) then res = ceil(x) else res = floor(x); return res; };\n\n")))

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

(define (round->sollya expr props)
  (let ([prec (precision-str (dict-ref props ':precision 'real))]
        [rm (round-str (dict-ref props ':round 'nearestEven))])
    (if (equal? prec "real")
        expr
        (format "round(~a, ~a, ~a)" expr prec rm))))

(define (operator->sollya props op args)
  (match (cons op args)
    [(list 'fabs a)       (round->sollya (format "abs(~a)" a) props)]
    [(list 'fmax a b)     (round->sollya (format "max(~a, ~a)" a b) props)]
    [(list 'fmin a b)     (round->sollya (format "min(~a, ~a)" a b) props)]
    [(list 'fma a b c)    (round->sollya (format "((~a * ~a) + ~a)" a b c) props)]
    [(list 'exp2 a)       (round->sollya (format "(2 ^ ~a)" a) props)]
    [(list 'pow a b)      (round->sollya (format "(~a ^ ~a)" a b) props)]
    [(list 'cbrt a)       (round->sollya (format "(~a ^ (1/3))" a) props)]
    [(list 'hypot a b)    (round->sollya (format "sqrt((~a ^ 2) + (~a ^ 2))" a b) props)]
    [(list 'atan2 a b)    (round->sollya (format "atan(~a / ~a)" a b) props)]
    [(list 'nearbyint a)  
        (let ([rm (round-str (dict-ref props ':round 'nearestEven))])
          (if (equal? rm "RN")
              (round->sollya (format "nearestint(~a)" a) props)
              (error 'application->sollya "Unsupported rounding mode ~a for nearbyint" rm)))]
    [(list (or 'isnan 'isinf 'isfinite 'signbit) a)
        (format "~a(~a)" op a)]
    [(list (? operator? f) args ...)
        (round->sollya (format "~a(~a)" f (string-join args ", ")) props)]))

(define (constant->sollya props expr)
  (match expr
    ['E (round->sollya "exp(1)" props)]
    ['LOG2E (round->sollya "log2(exp(1))" props)]
    ['LOG10E (round->sollya "log10(exp(1))" props)]
    ['LN2 (round->sollya "log(2)" props)]
    ['LN10 (round->sollya "log(10)" props)]
    ['PI (round->sollya "pi" props)]
    ['PI_2 (round->sollya "(pi/2)" props)]
    ['PI_4 (round->sollya "(pi/4)" props)]
    ['M_1_PI (round->sollya "(1/pi)" props)]
    ['M_2_PI (round->sollya "(2/pi)" props)]
    ['M_2_SQRTPI (round->sollya "(2/sqrt(pi))" props)]
    ['SQRT2 (round->sollya "sqrt(2)" props)]
    ['SQRT1_2 (round->sollya "sqrt(1/2)" props)]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY "infty"]
    ['NAN "nan"]
    [(? symbol?) expr]
    [(? number?) (round->sollya (format "~a" expr) props)]))

(define (declaration->sollya props var [val #f])
  (if val
    (format "var ~a = ~a;" var val)
    (format "var ~a;" var)))

(define (assignment->sollya var val)
  (format "~a = ~a;" var val))

(define (function->sollya name args arg-props body return ctx vars)
  (define arg-rounding
    (filter (compose not void?)
        (for/list ([var args] [prop arg-props] 
                  #:unless (equal? (precision-str (dict-ref prop ':precision 'real)) "real"))
          (format "\t~a = ~a;" var (round->sollya var prop)))))
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
        return))

(define sollya-language (language (const "sollya") operator->sollya constant->sollya declaration->sollya assignment->sollya
                                  round->sollya (const "") function->sollya))

;;; Exports

(define (core->sollya  prog name) (parameterize ([*lang* sollya-language]) (convert-core prog name)))
(define-compiler '("sollya") sollya-header core->sollya (const "") sollya-supported)