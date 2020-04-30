#lang racket

(provide parse-properties unparse-properties constants operators
         constant? operator? variable? define-by-match dictof property? property
         hex? hex->racket digits->float)

(define (property? symb)
  (and (symbol? symb) (string-prefix? (symbol->string symb) ":")))

(define (parse-properties lines)
  (let loop ([lines lines] [props '()])
    (match lines
      [(list (? property? prop) value rest ...)
       (loop rest (cons (cons prop value) props))]
      [(list _ ...)
       (values lines (reverse props))])))

(define/match (cons->list x)
  [((cons a b)) (list a b)])

(define (unparse-properties properties)
  (append-map cons->list properties))

(define operators
  (append
   '(+ - * / fabs fma exp exp2 expm1 log log10 log2 log1p pow sqrt
       cbrt hypot sin cos tan asin acos atan atan2 sinh cosh tanh
       asinh acosh atanh erf erfc tgamma lgamma ceil floor fmod
       remainder fmax fmin fdim copysign trunc round nearbyint cast)
   '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit)))

(define (operator? x)
  (set-member? operators x))

(define constants
  '(E LOG2E LOG10E LN2 LN10
      PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI
      SQRT2 SQRT1_2 MAXFLOAT HUGE_VAL
      INFINITY NAN TRUE FALSE))

(define (constant? x)
  (set-member? constants x))

(define (variable? var)
  (and (symbol? var) (not (constant? var))))

(define-syntax-rule (define-by-match name patterns ...)
  (define/contract name
    contract?
    (flat-named-contract
     'name
     (λ (var)
       (let name ([var var])
         (match var
           [patterns true] ...
           [_ false]))))))

(define-syntax-rule (property name statment)
  (void))

(define (dictof key/c value/c)
  (or/c (hash/c key/c value/c) (listof (cons/c key/c value/c))))

(define (hex? expr)
  (match expr
    [(? symbol?)
      (define s (symbol->string expr))
      (define rx #rx"[\\+\\-]?0x([0-9a-f]+(\\.[0-9a-f]+)?|\\.[0-9a-f]+)(p[\\+\\-]?[0-9]+)?")
      (let ([match (regexp-match* rx s)])
        (and (not (empty? match)) (equal? (first match) s)))]
    [else #f]))

(define (hex->racket sym)
  (define str (symbol->string sym))
  (define rx #rx"([\\+\\-])?0x([0-9a-f]+(\\.[0-9a-f]+)?|\\.[0-9a-f]+)(p([\\+\\-]?[0-9]+))?")
  (let* ([matches (regexp-match rx str)]
         [sign (if (equal? (second matches) #f) 1.0 -1.0)]
         [mant (string->number (third matches) 16)]
         [exp (if (equal? (sixth matches) #f) 0 (string->number (sixth matches)))])
    (* sign mant (expt 2 exp))))

(define (digits->float m e b)
  (* m (expt b e)))