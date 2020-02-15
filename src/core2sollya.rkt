#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide core->sollya sollya-supported)

(define sollya-name (const "sollya"))
(define sollya-supported (supported-list
  (invert-op-list '(isnormal tgamma lgamma remainder fmod round cbrt atan2 erf))
  (invert-const-list '())
  '(binary32 binary64)))

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
  (let ([prec (precision-str (dict-ref ctx ':precision 'real))]
        [rm (round-str (dict-ref ctx ':round 'nearestEven))])
    (if (equal? prec "real")
        expr
        (format "round(~a, ~a, ~a)" expr prec rm))))

(define (operator->sollya ctx op args)
  (round->sollya
    (match (cons op args)
      [(list 'fabs a)       (format "abs(~a)" a)]
      [(list 'fmax a b)     (format "max(~a, ~a)" a b)]
      [(list 'fmin a b)     (format "min(~a, ~a)" a b)]
      [(list 'fma a b c)    (format "((~a * ~a) + ~a)" a b c)]
      [(list 'exp2 a)       (format "(2 ^ ~a)" a)]
      [(list 'pow a b)      (format "(~a ^ ~a)" a b)]
      [(list 'cbrt a)       (format "(~a ^ (1/3))" a)]
      [(list 'hypot a b)    (format "sqrt((~a ^ 2) + (~a ^ 2))" a b)]
      [(list 'atan2 a b)    (format "atan(~a / ~a)" a b)]
      [(list 'nearbyint a)  
          (let ([rm (round-str (dict-ref ctx ':round 'nearestEven))])
            (if (equal? rm "RN")
                (format "nearestint(~a)" a)
                (error 'application->sollya "Unsupported rounding mode ~a for nearbyint" rm))
            )]
      [(list (? operator? f) args ...)
          (format "~a(~a)" f (string-join args ", "))])
    ctx))

(define (constant->sollya ctx expr)
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
    [(? symbol?) expr]
    [(? number?) (round->sollya (format "~a" expr) ctx)]))

(define (declaration->sollya ctx var [val #f])
  (if val
    (format "var ~a = ~a;" var val)
    (format "var ~a;" var)))

(define (assignment->sollya var val)
  (format "~a = ~a;" var val))

(define (function->sollya name args arg-ctx body return ctx vars)
  (define arg-rounding
    (filter (compose not void?)
        (for/list ([var args] [cx arg-ctx] #:unless (equal? (precision-str (dict-ref cx ':precision 'real)) "real"))
          (format "\t~a = ~a;" var (round->sollya var cx)))))
  (define decl-list
    (set-subtract vars args))

  (define var-string
    (if (> (length decl-list) 0)
        (format "\n\tvar ~a;" (string-join decl-list ", "))
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

(define sollya-language (language sollya-name operator->sollya constant->sollya declaration->sollya assignment->sollya round->sollya function->sollya))

;;; Exports

(define (core->sollya  prog name) (parameterize ([*lang* sollya-language]) (convert-core prog name)))
(define-compiler '("sollya") (const "") core->sollya (const "") sollya-supported)