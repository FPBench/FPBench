#lang racket

(require "imperative.rkt")

(provide rust-header core->rust rust-supported)

(define rust-header (lambda (_) "#![allow(unused_mut, unused_parens)]\n\n"))

(define rust-supported
  (supported-list
    (invert-op-proc
      (curry set-member?
            '(tgamma lgamma fdim erf erfc remainder
              array dim size ref for for* tensor tensor*)))
    (invert-const-proc (curry set-member? '(SQRT1_2)))
    (curry equal? 'binary64)
    (curry equal? 'nearestEven)
    #f))

(define rust-reserved   ; Language-specific reserved names (avoid name collisions)
  '(as break const continue crate else enum extern false fn for if impl in let loop match
  mod move mut pub ref return self Self static struct super trait true type unsafe use
  where while async await dyn abstract become box do final macro override priv typeof
  unsized virtual yield try))

(define (rust-fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string (char-downcase char))
         (format "_~a" (char->integer char))))
   ""))

(define/match (type->rust type)
  [('binary64) "f64"]
  [('binary32) "f32"]
  [('boolean) "bool"])

(define/match (rust-type->suffix type)
  [("f64") "f64"]
  [("f32") "f32"]
  [("boolean") ""])

(define (operator->rust op args ctx)
  (define args* (string-join args ", "))
  (match op
    ['/     (format "(~a / ~a)" (first args) (second args))]
    ['isfinite (format "f64::is_finite(~a)" args*)]
    ['isinf (format "f64::is_infinite(~a)" args*)]
    ['isnan (format "f64::is_nan(~a)" args*)]
    ['isnormal (format "f64::is_normal(~a)" args*)]
    ['log (format "f64::ln(~a)" args*)]
    ['log1p (format "f64::ln_1p(~a)" args*)]
    ['fma (format "f64::mul_add(~a)" args*)]
    ['fmax (format "f64::max(~a)" args*)]
    ['fmin (format "f64::min(~a)" args*)]
    ['nearbyint (format "f64::round(~a)" args*)]
    ['fabs (format "f64::abs(~a)" args*)]
    ['fmod (format "(~a % ~a)" (first args) (second args))]
    ['expm1 (format "f64::exp_m1(~a)" args*)]
    ['signbit (format "f64::is_sign_negative(~a)" args*)]
    ['pow (format "f64::powf(~a)" args*)]
    [(or 'exp 'exp2 'log10 'log2 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'acos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'ceil 'floor
         'copysign 'trunc 'round)
     (format "f64::~a(~a)" (~a op) args*)]))

(define (constant->rust x ctx)
  (define type (type->rust (ctx-lookup-prop ctx ':precision)))
  (match x
    ['E "std::f64::consts::E"]
    ['LOG2E "std::f64::consts::LOG2_E"]
    ['LOG10E "std::f64::consts::LOG10_E"]
    ['LN2 "std::f64::consts::LN_2"]
    ['LN10 "std::f64::consts::LN_10"]
    ['PI "std::f64::consts::PI"]
    ['PI_2 "std::f64::consts::FRAC_PI_2"]
    ['PI_4 "std::f64::consts::FRAC_PI_4"]
    ['M_1_PI "std::f64::consts::FRAC_1_PI"]
    ['M_2_PI "std::f64::consts::FRAC_2_PI"]
    ['M_2_SQRTPI "std::f64::consts::FRAC_2_SQRT_PI"]
    ['SQRT2 "std::f64::consts::SQRT_2"]
    ['MAXFLOAT "f64::MAX"]
    ['INFINITY "f64::INFINITY"]
    ['NAN "f64::NAN"]
    ['TRUE "true"]
    ['FALSE "false"]
    [(? hex?) (format "~a~a" (real->double-flonum (hex->racket x)) (rust-type->suffix type))]
    [(? number?) (format "~a~a" (real->double-flonum x) (rust-type->suffix type))]
    [(? symbol?) (~a x)]))

(define declaration->rust
  (case-lambda
   [(var ctx)
    (define type (type->rust (ctx-lookup-prop ctx ':precision)))
    (format "let mut ~a: ~a;" var type)]
   [(var val ctx)
    (define type (type->rust (ctx-lookup-prop ctx ':precision)))
    (format "let mut ~a: ~a = ~a;" var type val)]))

(define (params->rust args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->rust (ctx-lookup-prop ctx ':precision))])
        (format "~a: ~a" arg type)))
    ", "))

(define (program->rust name args arg-ctxs body return ctx vars)
  (define type (type->rust (ctx-lookup-prop ctx ':precision)))
  (format "fn ~a(~a) -> ~a {\n~a\t~a\n}\n"
          name (params->rust args arg-ctxs) type
          body return))

(define core->rust
  (make-imperative-compiler "rust"
    #:operator operator->rust
    #:constant constant->rust
    #:type type->rust
    #:declare declaration->rust
    #:program program->rust
    #:fix-name rust-fix-name
    #:flags '(no-parens-around-condition spaces-for-tabs)
    #:reserved rust-reserved))

(define-compiler '("rs") rust-header core->rust (const "") rust-supported)
