#lang racket

(require math/bigfloat generic-flonum)
(provide (struct-out evaluator) get-evaluator get-evaluator-params set-evaluator-params!)

(struct evaluator (real->repr repr->real constant function))

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda
   [`var val] ...
   [unsupported-value
    (error 'table-fn "Unimplemented operation ~a"
           unsupported-value)]))

(define get-float-cnst
  (table-fn
    [TRUE         #t]
    [FALSE        #f]
    [INFINITY     +inf.gfl]
    [NAN          +nan.gfl]
    [E		        (gflexp 1.gfl)]
    [LOG2E	      (gfllog2 (gflexp 1.gfl))]
    [LOG10E	      (gfllog10 (gflexp 1.gfl))]
    [LN2	        (gfllog 2.gfl)]
    [LN10	        (gfllog 10.gfl)]
    [PI		        pi.gfl]
    [PI_2	        (gfl/ pi.gfl 2.gfl)]
    [PI_4	        (gfl/ pi.gfl 4.gfl)]
    [M_1_PI	      (gfl/ 1.gfl pi.gfl)]
    [M_2_PI	      (gfl/ 2.gfl pi.gfl)]
    [M_2_SQRTPI	  (gfl/ 2.gfl (gflsqrt pi.gfl))]
    [SQRT2	      (gflsqrt 2.gfl)]
    [SQRT1_2	    (gflsqrt (gfl/ 1.gfl 2.gfl))]))

(define get-float-fun
  (table-fn                      ; TODO: Bigfloat -> flonum causes -0.0 to become 0.0
    [+ gfl+] [- (λ (x [y #f]) (if y (gfl- x y) (gfl- x)))]
    [* gfl*] [/ gfl/] [fabs gflabs]
    [sqrt gflsqrt] [cbrt gflcbrt]
    [hypot gflhypot] [fmod gflmod]
    [remainder gflremainder]

    [exp gflexp] [exp2 gflexp2] [exp10 gflexp10] [expm1 gflexpm1] [pow gflexpt]
    [log gfllog] [log2 gfllog2] [log10 gfllog10] [log1p gfllog1p]

    [sin gflsin] [cos gflcos] [tan gfltan]
    [asin gflasin] [acos gflacos] [atan gflatan] [atan2 gflatan2]
    [sinh gflsinh] [cosh gflcosh] [tanh gfltanh]
    [asinh gflasinh] [acosh gflacosh] [atanh gflatanh]

    [erf gflerf] [erfc gflerfc]
    [tgamma gflgamma] [lgamma gfllgamma]

    [ceil gflceiling] [floor gflfloor] [trunc gfltruncate] [round gflround]
    [nearbyint gflrint]
    [fmax gflmax] [fmin gflmin] [fdim gfldim] [fma gflfma]

    [< gfl<] [> gfl>] [<= gfl<=] [>= gfl>=] [== gfl=] [!= (negate gfl=)]
    [and (λ args (andmap identity args))]
    [or (λ args (ormap identity args))]
    [not not]

    [isnan gflnan?] [isinf gflinfinite?] [isfinite (negate (disjoin gflnan? gflinfinite?))]
    [isnormal (negate (disjoin gflsubnormal? gflnan? gflinfinite? gflzero?))]

    [signbit gflnegative?]
    [copysign gflcopysign]))

(define/match (fpcore->gfl-round rnd)
  [('nearestEven) 'nearest]
  [('nearestAway) 'away]
  [('toPositive)  'up]
  [('toNegative)  'down]
  [('toZero)      'zero]) 

(define (real->float x)
  (match x
   [(? gfl?)      x]
   [(? real?)     (gfl x)]))

(struct float-evaluator evaluator (es nbits rnd))

(define (get-float-evaluator es nbits rnd)
  (float-evaluator real->float gfl->real get-float-cnst get-float-fun es nbits rnd))

(define (get-evaluator prec [rnd 'nearest])
  (match prec
   [(list 'float es nbits)
    (get-float-evaluator es nbits rnd)]
   [_
    (error 'get-evaluator "Evaluator for (~a ~a) not supported"
                          prec rnd)]))

(define (get-evaluator-params eval)
  (match eval
   [(float-evaluator _ _ _ _ es nbits rnd)
    (values (list 'float es nbits) rnd)]
   [_
    (error 'get-evaluator-params "Unknown evaluator ~a" eval)]))

(define (set-evaluator-params! eval)
  (match eval
   [(float-evaluator _ _ _ _ es nbits rnd)
    (gfl-exponent es)
    (gfl-bits nbits)
    (gfl-rounding-mode (fpcore->gfl-round rnd))]
   [_
    (error 'set-evaluator-params! "Unknown evaluator ~a" eval)]))