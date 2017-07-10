#lang racket

(require "common.rkt" math/flonum math/bigfloat math/special-functions)
(provide eval-expr eval-on-points)

(struct evaluator (real constant function))

(define ((eval-expr* evaltor rec) expr ctx)
  (match expr
    [(? number?) ((evaluator-real evaltor) expr)]
    [(? constant?) ((evaluator-constant evaltor) expr)]
    [(? symbol?) (dict-ref ctx expr)]
    [`(if ,test ,ift ,iff)
     (if (rec test ctx) (rec ift ctx) (rec iff ctx))]
    [`(let ([,vars ,vals] ...) ,body)
     (define vals* (for/list ([val vals]) (rec val ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (rec body ctx*)]
    [`(while ,test ([,vars ,inits ,updates] ...) ,res)
     (define vals* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (if (rec test ctx*)
         (let ([inits* (for/list ([update updates]) (rec update ctx*))])
           (rec
            `(while ,test
               ,(for/list ([var vars] [init inits] [update updates])
                  (list var (rec update ctx*) update))
               ,res)
            ctx))
         (rec res ctx*))]
    [(list (? operator? op) args ...)
     (apply ((evaluator-function evaltor) op)
            (map (curryr rec ctx) args))]))

(define ((eval-expr evaltor) expr ctx)
  (let eval ([expr expr] [ctx ctx])
    ((eval-expr* evaltor eval) expr ctx)))

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda [`var val] ...))

(define racket-double-evaltor
  (evaluator
   real->double-flonum
   (table-fn
    [E		2.71828182845904523540]
    [LOG2E	1.44269504088896340740]
    [LOG10E	0.43429448190325182765]
    [LN2	0.69314718055994530942]
    [LN10	2.30258509299404568402]
    [PI		3.14159265358979323846]
    [PI_2	1.57079632679489661923]
    [PI_4	0.78539816339744830962]
    [1_PI	0.31830988618379067154]
    [2_PI	0.63661977236758134308]
    [2_SQRTPI	1.12837916709551257390]
    [SQRT2	1.41421356237309504880]
    [SQRT1_2	0.70710678118654752440]
    [NAN	+nan.0]
    [INFINITY	+inf.0]
    [TRUE #t] [FALSE #f])
   (table-fn
    [+ fl+] [- fl-] [* fl*] [/ fl/] [fabs flabs]
    [fma (λ (x y z) (fl+ (fl* x y) z))] ; TODO: Incorrect rounding
    [exp flexp] [exp2 (λ (x) (flexpt 2.0 x))]
    [expm1 (λ (x) (fl- (flexp x) 1.0))] ; TODO: Incorrect rounding
    [log fllog] [log10 (λ (x) (fl/ (fllog x) (fllog 10.0)))]
    [log2 (λ (x) (fl/ (fllog x) (fllog 2.0)))]
    [log1p (λ (x) (fllog (fl+ 1.0 x)))] ; TODO: Incorrect rounding
    [pow flexpt] [sqrt flsqrt]
    [hypot flhypot] [sin flsin] [cos flcos] [tan fltan] [asin flasin]
    [acos flacos] [atan flatan] [atan2 atan] [sinh flsinh] [cosh flcosh]
    [tanh fltanh] [asinh flasinh] [acosh flacosh] [atanh flatanh]
    [erf flerf] [erfc flerfc] [tgamma flgamma] [lgamma fllog-gamma]
    [ceil flceiling] [floor flfloor]
    [fmax flmax] [fmin flmin]
    [fdim (λ (x y) (flabs (fl- x y)))]
    [< <] [> >] [<= <=] [>= >=] [== =] [!= (compose not =)]
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
    ; TODO: Currently unsupported
    [isfinite '?] [isinf '?] [isnan '?] [isnormal '?] [signbit '?]
    [fmod '?] [remainder '?]
    [copysign '?] [trunc '?] [round '?] [nearbyint '?])))
