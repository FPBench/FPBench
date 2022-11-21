#lang racket

(require "imperative.rkt")

(provide js-header core->js js-runtime js-supported)

(define js-runtime (make-parameter "Math"))

;; JS

(define js-header
  (λ (x) 
   (apply string-append
    (append
      (map (curryr format x)
          '("function fmax(x, y) { if (x != x) { return y; } else if (y != y) { return x; } else { return ~a.max(x, y); }}\n"
            "function fmin(x, y) { if (x != x) { return y; } else if (y != y) { return x; } else { return ~a.min(x, y); }}\n"
            "function pow(x, y) { if (x == 1.0 && isNaN(y)) { return 1.0; } else { return ~a.pow(x, y); }}\n"))
     '("function fdim(x , y) { if (x != x || y != y) { return NaN; } else if (x > y) { return x - y; } else { return 0; }}\n\n")))))

(define js-supported 
  (supported-list
    (invert-op-proc 
      (curry set-member?
            '(!= copysign exp2 erf erfc fma fmod isfinite isnormal   
              lgamma nearbyint remainder signbit tgamma
              array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry equal? 'binary64)
    (curry equal? 'nearestEven)
    #f))

(define js-reserved   ; Language-specific reserved names (avoid name collisions)
  '(abstract arguments await boolean break byte case catch char
    class const continue debugger default delete do double
    else enum eval export extends false final finally float
    for function goto if implements import in instanceof int
    interface let long native new null package private protected
    public return short static super switch synchronized this
    throw throws transient true try typeof var void volatile
    while with yield))

(define (operator->js op args ctx)
  (define args* (string-join args ", "))
  (match op
    ['/     (format "(~a / ~a)" (first args) (second args))]
    ['fabs  (format "~a.abs(~a)" (js-runtime) args*)]
    ['fmax  (format "fmax(~a)" args*)]
    ['fmin  (format "fmin(~a)" args*)]
    ['fdim  (format "fdim(~a)" args*)]
    ['pow   (format "pow(~a)" args*)]
    ['isinf (format "(~a.abs(~a) === Infinity)" (js-runtime) args*)]
    ['isnan (format "isNaN(~a)" args*)]
    [_      (format "~a.~a(~a)" (js-runtime) op args*)]))

(define (constant->js expr ctx)
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
    [(? number?) (~a (real->double-flonum expr))]
    [(? symbol?) (~a expr)]))


(define core->js
  (make-imperative-compiler "js"
    #:operator operator->js
    #:constant constant->js
    #:reserved js-reserved))

(define-compiler '("js") js-header core->js (const "") js-supported)
