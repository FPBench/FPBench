#lang racket

(require "common.rkt" math/flonum math/bigfloat math/special-functions math/base)
(provide
 (struct-out evaluator) racket-double-evaluator racket-single-evaluator
 fpcore? expr? context/c eval-expr* eval-expr racket-run-fpcore)

(struct evaluator (real constant function))

(define/contract (fpcore? thing)
  contract?
  (match thing
    [`(FPCore (,(? symbol?) ...) ,props ... ,(? expr?))
     (define-values (rest props*) (parse-properties props))
     (null? rest)]
    [_ false]))

(define-by-match expr?
  (? number?)
  (? constant?)
  (? symbol?)
  (list (? operator?) (? expr?) ...)
  `(if ,(? expr?) ,(? expr?) ,(? expr?))
  `(let ([,(? symbol?) ,(? expr?)] ...) ,(? expr?))
  `(while ,(? expr?) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?)))

(define/contract context/c contract? (dictof symbol? any/c))

(define/contract ((eval-expr* evaltor rec) expr ctx)
  (-> evaluator? (-> expr? context/c any/c) (-> expr? context/c any/c))
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

(define/contract ((eval-expr evaltor) expr ctx)
  (-> evaluator? (-> expr? context/c any/c))
  (let eval ([expr expr] [ctx ctx])
    ((eval-expr* evaltor eval) expr ctx)))

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda
   [`var val] ...
   [unsupported-value
    (error 'eval-expr "Unimplemented operation ~a"
           unsupported-value)]))

(define/contract racket-double-evaluator evaluator?
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
    [+ +] [- -] [* *] [/ /] [fabs abs]
    [exp exp] [exp2 (λ (x) (expt 2.0 x))]
    [log log] [log10 (λ (x) (/ (log x) (log 10.0)))]
    [log2 (λ (x) (/ (log x) (log 2.0)))]
    [pow expt] [sqrt sqrt]
    [hypot flhypot] [sin sin] [cos cos] [tan tan] [asin asin]
    [acos acos] [atan atan] [atan2 atan] [sinh sinh] [cosh cosh]
    [tanh tanh] [asinh asinh] [acosh acosh] [atanh atanh]
    [erf erf] [erfc erfc] [tgamma gamma] [lgamma log-gamma]
    [ceil ceiling] [floor floor] [trunc truncate] [round round]
    [fmax max] [fmin min]
    [fdim (λ (x y) (abs (- x y)))]
    [< <] [> >] [<= <=] [>= >=] [== =] [!= (compose not =)]
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
    [isnan nan?] [isinf infinite?]
    [isfinite (λ (x) (not (or (nan? x) (infinite? x))))]
    ; TODO: Currently unsupported
    ;[fma '?] [expm1 '?] [log1p '?] [isnormal '?] [signbit '?]
    ;[fmod '?] [remainder '?] [copysign '?] [nearbyint '?]
    )))

(define/contract racket-single-evaluator evaluator?
  (struct-copy evaluator racket-double-evaluator
               [real real->single-flonum]
               [constant (λ (x) (real->single-flonum ((evaluator-constant racket-double-evaluator) x)))]))

(define/contract (racket-run-fpcore prog vals)
  (-> fpcore? (listof real?) real?)
  (match-define `(FPCore (,vars ...) ,props* ... ,body) prog)
  (define-values (_ props) (parse-properties props*))
  (define evaltor
    (match (dict-ref props ':precision 'binary64)
      ['binary64 racket-double-evaluator]
      ['binary32 racket-single-evaluator]))
  ((eval-expr evaltor) body (map cons vars (map (evaluator-real evaltor) vals))))

(module+ main
  (command-line
   #:program "fpcore.rkt"
   #:args args
   (let ([vals (map (compose real->double-flonum string->number) args)])
     (for ([prog (in-port read)])
       (printf "~a\n" (racket-run-fpcore prog vals))))))
