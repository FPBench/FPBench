#lang racket

(require math/bigfloat generic-flonum "tensor.rkt")
(provide (struct-out evaluator) get-evaluator
         get-evaluator-params set-evaluator-params!
         repr->real repr->integer)

(struct evaluator (real constant function))

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda
   [`var val] ...
   [unsupported-value
    (error 'table-fn "Unimplemented operation ~a"
           unsupported-value)]))

;;;;;;;;;;;;;;;;;;;;;;;;;; Float ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/match (fpcore->gfl-round rnd)
  [('nearestEven) 'nearest]
  [('nearestAway) 'away]
  [('toPositive)  'up]
  [('toNegative)  'down]
  [('toZero)      'zero]) 

(define (real->float x)
  (match x
   [(? gfl?)        x]
   [(? real?)       (gfl x)]))

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

(define (gfl-op fn)
  (λ args
    (define args*
      (parameterize ([gfl-exponent 32] [gfl-bits 1024])
        (map real->float args)))
    (apply fn args*)))

(define get-float-fun
  (table-fn                      ; TODO: Bigfloat -> flonum causes -0.0 to become 0.0
    [+ (gfl-op gfl+)] [- (gfl-op (λ (x [y #f]) (if y (gfl- x y) (gfl- x))))]
    [* (gfl-op gfl*)] [/ (gfl-op gfl/)] [fabs (gfl-op gflabs)]
    [sqrt (gfl-op gflsqrt)] [cbrt (gfl-op gflcbrt)]
    [hypot (gfl-op gflhypot)] [fmod (gfl-op gflmod)]
    [remainder (gfl-op gflremainder)]

    [exp (gfl-op gflexp)] [exp2 (gfl-op gflexp2)] [exp10 (gfl-op gflexp10)] 
    [log (gfl-op gfllog)] [log2 (gfl-op gfllog2)] [log10 (gfl-op gfllog10)]
    [expm1 (gfl-op gflexpm1)] [log1p (gfl-op gfllog1p)] [pow (gfl-op gflexpt)]

    [sin (gfl-op gflsin)] [cos (gfl-op gflcos)] [tan (gfl-op gfltan)]
    [asin (gfl-op gflasin)] [acos (gfl-op gflacos)] [atan (gfl-op gflatan)]
    [atan2 (gfl-op gflatan2)]
    [sinh (gfl-op gflsinh)] [cosh (gfl-op gflcosh)] [tanh (gfl-op gfltanh)]
    [asinh (gfl-op gflasinh)] [acosh (gfl-op gflacosh)] [atanh (gfl-op gflatanh)]

    [erf (gfl-op gflerf)] [erfc (gfl-op gflerfc)]
    [tgamma (gfl-op gflgamma)] [lgamma (gfl-op gfllgamma)]

    [ceil (gfl-op gflceiling)] [floor (gfl-op gflfloor)] [trunc (gfl-op gfltruncate)]
    [round (gfl-op gflround)] [nearbyint (gfl-op gflrint)]
    [fmax (gfl-op gflmax)] [fmin (gfl-op gflmin)] [fdim (gfl-op gfldim)]
    [fma (gfl-op gflfma)]

    [< (gfl-op gfl<)] [> (gfl-op gfl>)] [<= (gfl-op gfl<=)] [>= (gfl-op gfl>=)]
    [== (gfl-op gfl=)] [!= (gfl-op (negate gfl=))]
    [and (λ args (andmap identity args))]
    [or (λ args (ormap identity args))]
    [not not]

    [isnan (gfl-op gflnan?)] [isinf (gfl-op gflinfinite?)] 
    [isfinite (gfl-op (negate (disjoin gflnan? gflinfinite?)))]
    [isnormal (gfl-op (negate (disjoin gflsubnormal? gflnan? gflinfinite? gflzero?)))]

    [signbit (gfl-op gflnegative?)]
    [copysign (gfl-op gflcopysign)]))

(struct float-evaluator evaluator (es nbits rnd))

(define (get-float-evaluator es nbits rnd)
  (float-evaluator real->float get-float-cnst get-float-fun es nbits rnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Integer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (normalize-int x)
  (define r (inexact->exact (floor x)))
  (define shift (expt 2 63))
  (- (modulo (+ r shift) (expt 2 64)) shift))

(define (real->integer x)
  (define x*
    (match x
     [(? gfl?) (gfl->real x)]
     [(? real?) x]))
  (define r (floor (inexact->exact x*)))
  (define shift (expt 2 63))
   (- (modulo (+ r shift) (expt 2 64)) shift))

(define get-integer-const
  (table-fn
    [TRUE   #t]
    [FALSE  #f]))

(define get-integer-fun
  (table-fn
    [+ (int-op +)] [- (int-op (λ (x [y #f]) (if y (- x y) (- x))))]
    [* (int-op *)] [/ (int-op /)]

    [< (int-op <)] [> (int-op >)] [<= (int-op <=)] [>= (int-op >=)]
    [== (int-op =)] [!= (int-op (negate =))]
    [and (λ args (andmap identity args))]
    [or (λ args (ormap identity args))]
    [not not]))

(define (int-op fn)
  (λ args
    (define args* (map real->integer args))
    (define r (apply fn args*))
    (if (real? r) (normalize-int r) r)))

(struct integer-evaluator evaluator ())

(define default-integer-evaluator
  (integer-evaluator real->integer get-integer-const get-integer-fun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Exported ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-evaluator prec [rnd 'nearest])
  (match prec
   [(list 'float es nbits)
    (get-float-evaluator es nbits rnd)]
   ['integer
    default-integer-evaluator]
   [_
    (error 'get-evaluator "Evaluator for (~a ~a) not supported"
                          prec rnd)]))

(define (get-evaluator-params eval)
  (match eval
   [(float-evaluator _ _ _ es nbits rnd)
    (values (list 'float es nbits) rnd)]
   [(? integer-evaluator?)
    (values 'integer 'nearest)]
   [_
    (error 'get-evaluator-params "Unknown evaluator ~a" eval)]))

(define (set-evaluator-params! eval)
  (match eval
   [(float-evaluator _ _ _ es nbits rnd)
    (gfl-exponent es)
    (gfl-bits nbits)
    (gfl-rounding-mode (fpcore->gfl-round rnd))]
   [(? integer-evaluator?)
    (void)]
   [_
    (error 'set-evaluator-params! "Unknown evaluator ~a" eval)]))

(define (repr->real x)
  (match x
   [(? list?)   (map repr->real x)] ; tensor
   [(? gfl?)    (gfl->real x)]
   [(? real?)   x]
   [(? boolean?) x]))

(define (repr->integer x)
  (match x
   [(? list?) (error 'repr->integer "Cannot convert a tensor to an integer: ~a" x)]
   [(? gfl?)
    (define r (gfl->real x))
    (unless (integer? r)
      (error 'repr->integer "Expected an integer: ~a\n" r))
    (if (exact? r) r (inexact->exact r))]
   [(? real?)
    (unless (integer? x)
      (error 'repr->integer "Expected an integer: ~a\n" x))
    (if (exact? x) x (inexact->exact x))]
   [(? boolean?) x]))