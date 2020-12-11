#lang racket

(require "common.rkt" "tensor.rkt" "fpcore-checker.rkt")
(require math/flonum racket/extflonum math/bigfloat math/special-functions math/base)
(provide eval-expr eval-expr* racket-run-fpcore
         (struct-out evaluator) racket-integer-evaluator
         racket-double-evaluator racket-single-evaluator racket-binary80-evaluator)

(struct evaluator (real constant function))

(define/match (fpcore->bf-round roundmode)
  [('nearestEven) 'nearest]
  [('nearestAway) (error 'fpcore->bf-round "math/bigfloat does not support 'nearestAway")]
  [('toPositive)  'up]
  [('toNegative)  'down]
  [('toZero)      'zero]) 

(define (real->float x prec)
  (define x* (if (extflonum? x) (extfl->real x) x))
  (match prec
    ['binary80 (real->extfl x*)]
    ['binary64 (real->double-flonum x*)]
    ['binary32 (real->single-flonum x*)]
    ['integer  (inexact->exact x*)]))

(define/contract context/c contract? (dictof symbol? any/c))

(define/contract ((eval-expr* evaltor rec) expr ctx)
  (-> evaluator? (-> expr? context/c any/c) (-> expr? context/c any/c))
  (match expr
    [(? number?) ((evaluator-real evaltor) expr)]
    [(? hex?) ((evaluator-real evaltor) (hex->racket expr))]
    [`(digits ,m ,e ,b) (digits->number m e b)]
    [(? extflonum?) expr]
    [(? constant?) ((evaluator-constant evaltor) expr)]
    [(? tensor?) expr]
    [(? symbol?) (dict-ref ctx expr)]
    [`(if ,test ,ift ,iff)
     (if (rec test ctx) (rec ift ctx) (rec iff ctx))]
    [`(let ([,vars ,vals] ...) ,body)
     (define vals* (for/list ([val vals]) (rec val ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (rec body ctx*)]
    [`(let* () ,body)
     (rec body ctx)]
    [`(let* ([,var ,val] ,rest ...) ,body)
     (rec `(let* ,rest ,body) (dict-set ctx var (rec val ctx)))]
    [`(while ,test ([,vars ,inits ,updates] ...) ,res)
     (define vals* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (if (rec test ctx*)
         (rec
          `(while ,test
             ,(for/list ([var vars] [init inits] [update updates])
                (list var (rec update ctx*) update))
             ,res)
          ctx)
         (rec res ctx*))]
    [`(while* ,test ([,vars ,inits ,updates] ...) ,res)
     (define-values (vals* ctx*)
       (for/fold ([vals '()] [ctx ctx]) ([var vars] [init inits])
         (define val (rec init ctx))
         (values (cons val vals) (dict-set ctx var val))))
     (if (rec test ctx*)
         (rec `(let* ,(for/list ([var vars] [val (reverse vals*)]) (list var val))
                 (while* ,test ,(for/list ([var vars] [init inits] [update updates])
                                  (list var update update)) ,res))
              ctx*)
         (rec res ctx*))]
    [`(for ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (curryr rec ctx) vals))
     (define ranges (map (compose stream->list in-range) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*)))
     (for/fold ([cx ctx*] #:result (rec body cx)) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*]) ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx*)))))]
    [`(for* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (curryr rec ctx) vals))
     (define ranges (map (compose stream->list in-range) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*)))
     (for/fold ([cx ctx*] #:result (rec body cx)) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*]) ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx**)))))]
    [`(tensor ([,vars ,vals] ...) ,body)
     (define sizes (map (compose inexact->exact (curryr rec ctx)) vals))
     (define ranges (map (λ (x) (build-list x identity)) sizes))
     (define coords (apply cartesian-product ranges))
     (define vals* 
      (for/list ([coord coords])
        (let ([ctx* (apply dict-set* ctx (append-map list vars coord))])
          (rec body ctx*))))
     (tabulate->tensor (map inexact->exact sizes) vals*)]
    [`(tensor* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (compose inexact->exact (curryr rec ctx)) vals))
     (define ranges (map (λ (x) (build-list x identity)) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*))) ; should these be added before or after?
     (define vals* 
      (for/fold ([cx ctx*] [vals '()] #:result vals) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*] #:result (values cx** (append vals (list (rec body cx**))))) 
                   ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx**))))))
     (tabulate->tensor (map inexact->exact sizes) vals*)]
    [`(! ,props* ... ,body)
     (define-values (_ props) (parse-properties props*))
     (define-values (p evaltor*)
       (match (dict-ref props ':precision #f)
         ['binary80 (values 64 racket-binary80-evaluator)]
         ['binary64 (values 53 racket-double-evaluator)]
         ['binary32 (values 24 racket-single-evaluator)]
         ['integer  (values 128 racket-integer-evaluator)]
         [_         (values (bf-precision) evaltor)]))
      (parameterize ([bf-rounding-mode (fpcore->bf-round (dict-ref props ':round 'nearestEven))]
                     [bf-precision p])
          ((eval-expr* evaltor* rec) body ctx))]
    [`(cast ,expr) ((evaluator-real evaltor) ((eval-expr* evaltor rec) expr ctx))]
    [`(array ,vals ...) (for/list ([i vals]) ((eval-expr* evaltor rec) i ctx))]
    [`(dim ,val) (tensor-dim (rec val ctx))]
    [`(size ,val ,dim) (tensor-size (rec val ctx) (inexact->exact dim))]
    [`(ref ,val ,elems ...) (apply (curry tensor-ref (rec val ctx)) (map (compose inexact->exact (curryr rec ctx)) elems))]
    [(list (? (curry dict-has-key? (*fpcores*)) ident) args ...)
      (define args* (map (curryr rec ctx) args))
      (define core* (first (dict-ref (*fpcores*) ident)))
      (racket-run-fpcore core* (map ~a args*))]
    [(list (? operator? op) args ...)
      (apply ((evaluator-function evaltor) op) (map (curryr rec ctx) args))]))

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

;; float <==> bigfloat, TODO: separate location

(define (extfl->real x)
  (cond
    [(equal? x +inf.t)  +inf.0] 
    [(equal? x -inf.t)  -inf.0]
    [(equal? x +nan.t)  +nan.0] 
    [(equal? x -nan.t)  -nan.0]
    [(equal? x -0.0t0)  -0.bf]
    [else (extfl->exact x)])) 

(define/match (prec->bf-bits prec)
  [('binary80)  64]
  [('binary64)  53]
  [('binary32)  24]
  [('integer)   128]) ; compute at high precision, then round

(define (fl->bf arg [override? #f]) ; override converts to bf at current precision rather than the precision of arg
  (cond
    [override?            (bf (if (extflonum? arg) (extfl->real arg) arg))]
    [(integer? arg)       (parameterize ([bf-precision 128]) (bf arg))]
    [(extflonum? arg)     (parameterize ([bf-precision 64]) (bf (extfl->real arg)))]
    [(double-flonum? arg) (parameterize ([bf-precision 53]) (bf arg))]
    [(single-flonum? arg) (parameterize ([bf-precision 24]) (bf arg))]))

(define (bf->fl x [bits (bf-precision)])
  (define real->fl
    (match bits
      [64 real->extfl]
      [53 real->double-flonum]
      [24 real->single-flonum]))
  (define x*      ;; validate x > max or x < min for certain rounding modes
   (parameterize ([bf-precision bits])
    (define w (match bits [64 15] [53 11] [24 8]))
    (define max (bf* (bf 1 (sub1 (expt 2 (sub1 w)))) (bf- 2.bf (bf 1 (- (sub1 bits))))))
    (define min (bf* (bf 1 (- 2 (expt 2 (sub1 w)))) (bf 1 (- (sub1 bits)))))
    (cond
      [(or (bfinfinite? x) (bfnan? x)) x]
      [(and (bf> x max) (or (equal? (bf-rounding-mode) 'down) (equal? (bf-rounding-mode) 'zero))) max]
      [(and (bf> x 0.bf) (bf< x min) (equal? (bf-rounding-mode) 'up)) min]
      [(and (bf< x 0.bf) (bf> x (bf- min)) (equal? (bf-rounding-mode) 'down)) (bf- min)]
      [(and (bf< x (bf- max)) (or (equal? (bf-rounding-mode) 'up) (equal? (bf-rounding-mode) 'zero))) (bf- max)]
      [else x])))
  (real->fl (bigfloat->real x*)))

;; float <==> bigfloat

(define (constant-with-bf x)
  (match x
    ['NAN      +nan.0]
    ['INFINITY +inf.0]
    ['TRUE     #t]
    ['FALSE    #f]
    [_  
     (bf->fl 
       (fl->bf
         ((table-fn
            [E		2.71828182845904523540]
            [LOG2E	1.44269504088896340740]
            [LOG10E	0.43429448190325182765]
            [LN2	0.69314718055994530942]
            [LN10	2.30258509299404568402]
            [PI		3.14159265358979323846]
            [PI_2	1.57079632679489661923]
            [PI_4	0.78539816339744830962]
            [M_1_PI	0.31830988618379067154]
            [M_2_PI	0.63661977236758134308]
            [M_2_SQRTPI	1.12837916709551257390]
            [SQRT2	1.41421356237309504880]
            [SQRT1_2	0.70710678118654752440])
          x)
        #t))]))
  
(define (compute-with-bf fn)
  (lambda (arg) 
    (bf->fl (fn (fl->bf arg)))))

(define (compute-with-bf-2 fn)
  (lambda (arg1 arg2) (bf->fl (fn (fl->bf arg1) (fl->bf arg2)))))

(define (compute-with-bf-fma arg1 arg2 arg3)   
  (let ([old-prec (bf-precision)])
    (parameterize ([bf-precision (+ (* (bf-precision) 2) 1)])
      (bf->fl (bf+ (bf* (fl->bf arg1) (fl->bf arg2)) (fl->bf arg3)) old-prec))))

(define (my!= #:cmp [cmp =] . args) (not (check-duplicates args cmp)))
(define (my= #:cmp [cmp =] . args)
  (match args ['() true] [(cons hd tl) (andmap (curry cmp hd) tl)]))

;;; binary64 and binary32 evaluators

(define/contract racket-double-evaluator evaluator?
  (evaluator
   (λ (x) (if (real? x) (real->double-flonum x) (real->extfl x)))
   constant-with-bf 
   (table-fn                      ; TODO: Bigfloat -> flonum causes -0.0 to become 0.0
    [+ (compute-with-bf-2 bf+)] 
    [- (λ (x [y #f]) (if (equal? y #f) (- x) ((compute-with-bf-2 bf-) x y)))]  ; distinguish between negation and subtraction
    [* (λ (a b)
          (let ([x (if (real? a) a (extfl->exact a))]  ; <-- possibly redundant?
                [y (if (real? b) b (extfl->exact b))])   
                (if (and (or (zero? x) (zero? y))      ; to get around -0.0 (bigfloat) -> 0.0 (float)
                         (nor (infinite? x) (infinite? y) (nan? x) (nan? y)))
                    (if (xor (if (zero? x) (equal? x -0.0) (negative? x))
                             (if (zero? y) (equal? y -0.0) (negative? y)))
                        -0.0 0.0)
                    ((compute-with-bf-2 bf*) x y))))]
    [/ (compute-with-bf-2 bf/)]
    [fabs abs]

    [exp (compute-with-bf bfexp)] [log (compute-with-bf bflog)]
    [pow (compute-with-bf-2 bfexpt)] [sqrt (compute-with-bf bfsqrt)]
    [sin (compute-with-bf bfsin)] [cos (compute-with-bf bfcos)] [tan (compute-with-bf bftan)]
    [asin (compute-with-bf bfasin)] [acos (compute-with-bf bfacos)] 
    [atan (compute-with-bf bfatan)] [atan2 (compute-with-bf-2 bfatan2)]
    [ceil (compute-with-bf bfceiling)] [floor (compute-with-bf bffloor)] 
    [trunc (λ (x) (if (< -1 x 0) -0.0 ((compute-with-bf bftruncate) x)))]
    [< <] [> >] [<= <=] [>= >=] [== my=] [!= my!=]
    [and (λ args (andmap identity args))] 
    [or (λ args (ormap identity args))] 
    [not not]
    [isnan nan?] [isinf infinite?]
    [nearbyint 
      (match (bf-rounding-mode)
        ['nearest round]
        ['up      ceiling]
        ['down    floor]
        ['zero    truncate])]
    [cast identity]

    ;; emulate behavior
    [fmax (λ (x y)
           (cond [(nan? x) y]
                 [(nan? y) x] 
                 [(max x y)]))]
    [fmin (λ (x y)
           (cond [(nan? x) y]
                 [(nan? y) x] 
                 [(min x y)]))]
    [fdim (λ (x y)
           (cond [(or (nan? x) (nan? y)) +nan.0]
                 [(> x y) ((compute-with-bf-2 bf-) x y)]
                 [else 0.0]))]
    [isfinite (λ (x) (not (or (nan? x) (infinite? x))))]
    [signbit (λ (x) (= (bigfloat-signbit (bf x)) 1))]
    [copysign (λ (x y) (if (= (bigfloat-signbit (bf y)) 1)
                           (- (abs x))
                           (abs x)))]

    [cbrt (compute-with-bf bfcbrt)]
    [exp2 (compute-with-bf bfexp2)]
    [expm1 (compute-with-bf bfexpm1)]
    [log1p (compute-with-bf bflog1p)]
    [log10 (compute-with-bf bflog10)]
    [log2 (compute-with-bf bflog2)]

    [fma compute-with-bf-fma]
    [hypot (compute-with-bf-2 bfhypot)]

    [sinh (compute-with-bf bfsinh)]
    [cosh (compute-with-bf bfcosh)]
    [tanh (compute-with-bf bftanh)]
    [asinh (compute-with-bf bfasinh)]
    [acosh (compute-with-bf bfacosh)]
    [atanh (compute-with-bf bfatanh)]

    [erf (compute-with-bf bferf)]
    [erfc (compute-with-bf bferfc)]
    [tgamma (compute-with-bf bfgamma)]
    [lgamma (compute-with-bf bflog-gamma)]

    ;; TODO: known to be incorrect
    [round round]
    [isnormal (lambda (x) (not (or (nan? x) (infinite? x) (<= (abs x) 0.0))))]
    [fmod (lambda (x y) (let ([n (truncate (/ (abs x) (abs y)))])
                          (* (- (abs x) (* (abs y) n)) (sgn x))))]
    [remainder (lambda (x y) (let ([n (round (/ x y))])
                               (- x (* y n))))]
    )))

(define/contract racket-single-evaluator evaluator?
  (struct-copy evaluator racket-double-evaluator
               [real (λ (x) (if (real? x) (real->single-flonum x) (extfl->real x)))]))

;;; integer evaluator

(define (bf->integer x) 
  (bigfloat->real (bffloor x)))

(define/contract racket-integer-evaluator evaluator?
  (evaluator
   (λ (x) (bf->integer (bf x)))
   (λ (x) (bf->integer ((evaluator-constant racket-double-evaluator) x))) ; Integer constants other than TRUE and FALSE are nonsensical
   (table-fn
    [+ (λ (x y) (bf->integer (parameterize ([bf-rounding-mode 'down]) (bf+ (bf x) (bf y)))))]
    [- (λ (x [y #f]) (if (equal? y #f) (- x) 
                         (parameterize ([bf-rounding-mode 'down]) (bf->integer (bf- (bf x) (bf y))))))]
    [* (λ (x y) (bf->integer (parameterize ([bf-rounding-mode 'down]) (bf* (bf x) (bf y)))))]
    [/ (λ (x y) (bf->integer (parameterize ([bf-rounding-mode 'down]) (bf/ (bf x) (bf y)))))]

    [< <] [> >] [<= <=] [>= >=] [== my=] [!= my!=]
    [and (λ args (andmap identity args))] 
    [or (λ args (ormap identity args))] 
    [not not]
   )))

;;; binary80 evaluator 

; Since extflonums aren't consider "numbers", they need their own
; arithemtic functions

(define (extfl-zero? x) (or (equal? x -0.0t0) (equal? x 0.0t0)))
(define (extfl-inf? x) (or (extfl= x -inf.t) (extfl= x +inf.t)))
(define (extfl-nan? x) (not (extfl= x x)))
(define (extfl-neg? x) (not (extfl< x 0.0t0)))
(define (extfl-signbit x) (if (or (extfl< x 0.0t0) (equal? x -0.0t0)) #t #f))
(define (extfl-sgn x) (if (extfl< x 0.0t0) -1.0t0 (if (extfl> x 0.0t0) 1.0t0 x)))

(define (extfl-cmp cmp x y rest)
  (cond
    [(= (length rest) 0) (cmp x y)]
    [(= (length rest) 1) (and (cmp x y) (cmp y (first rest)))]
    [else (and (cmp x y) (cmp y (first rest))
               (for/and ([i (in-range (sub1 (length rest)))])
                  (cmp (list-ref rest i) (list-ref rest (add1 i)))))]))

(define/contract racket-binary80-evaluator evaluator?
  (evaluator
   real->extfl
   (λ (x) (let ([v ((evaluator-constant racket-double-evaluator) x)])
            (if (real? v) (real->extfl v) v)))
   (table-fn    
    [+ (compute-with-bf-2 bf+)] 
    [- (λ (x [y #f]) (if (equal? y #f) (extfl* x -1.0t0) ((compute-with-bf-2 bf-) x y)))]  ; distinguish between negation and subtraction
    [* (λ (x y) (if (and (or (extfl-zero? x) (extfl-zero? y))   ; to get around -0.0 (bigfloat) -> 0.0 (float)
                         (nor (extfl-inf? x) (extfl-inf? y) (extfl-nan? x) (extfl-nan? y)))
                    (if (xor (if (extfl-zero? x) (equal? x -0.0t0) (extfl-neg? x))
                             (if (extfl-zero? y) (equal? y -0.0t0) (extfl-neg? y)))
                        -0.0t0 0.0t0)
                    ((compute-with-bf-2 bf*) x y)))]
    [/ (compute-with-bf-2 bf/)]
    [fabs extflabs]

    [exp (compute-with-bf bfexp)] [log (compute-with-bf bflog)]
    [pow (compute-with-bf-2 bfexpt)] [sqrt (compute-with-bf bfsqrt)]
    [sin (compute-with-bf bfsin)] [cos (compute-with-bf bfcos)] [tan (compute-with-bf bftan)]
    [asin (compute-with-bf bfasin)] [acos (compute-with-bf bfacos)] 
    [atan (compute-with-bf bfatan)] [atan2 (compute-with-bf-2 bfatan2)]
    [ceil (compute-with-bf bfceiling)] [floor (compute-with-bf bffloor)] 
    [trunc (compute-with-bf bftruncate)]
    [<  (λ (x y . rest) (extfl-cmp extfl< x y rest))]
    [>  (λ (x y . rest) (extfl-cmp extfl> x y rest))]
    [<= (λ (x y . rest) (extfl-cmp extfl<= x y rest))]
    [>= (λ (x y . rest) (extfl-cmp extfl>= x y rest))]
    [== (λ (x y . rest) (extfl-cmp extfl= x y rest))]
    [!= (λ (x y . rest) (extfl-cmp (compose not extfl=) x y rest))]
    [and (λ args (andmap identity args))] 
    [or (λ args (ormap identity args))] 
    [not not]
    [isnan extfl-nan?] [isinf extfl-inf?]
    [cast identity]

    ;; emulate behavior
    [isfinite (λ (x) (not (or (extfl-nan? x) (extfl-inf? x))))]
    [fmax (λ (x y)
           (cond [(equal? x +nan.t) y]
                 [(equal? y +nan.t) x] 
                 [(extflmax x y)]))]
    [fmin (λ (x y)
           (cond [(equal? x +nan.t) y]
                 [(equal? y +nan.t) x] 
                 [(extflmin x y)]))]
    [fdim (λ (x y)
           (cond [(or (equal? x +nan.t) (equal? y +nan.t)) +nan.t]
                 [(extfl> x y) ((compute-with-bf-2 bf-) x y)]
                 [else    0.0t0]))]
    [signbit extfl-signbit]
    [copysign (λ (x y) (if (extfl-signbit y) (extfl* (extflabs x) -1.0t0) (extflabs x)))]
    [nearbyint 
      (match (bf-rounding-mode)
        ['nearest extflround]
        ['up      extflceiling]
        ['down    extflfloor]
        ['zero    extfltruncate])]

    [cbrt (compute-with-bf bfcbrt)]
    [exp2 (compute-with-bf bfexp2)]
    [expm1 (compute-with-bf bfexpm1)]
    [log1p (compute-with-bf bflog1p)]
    [log10 (compute-with-bf bflog10)]
    [log2 (compute-with-bf bflog2)]

    [fma compute-with-bf-fma]
    [hypot (compute-with-bf-2 bfhypot)]

    [sinh (compute-with-bf bfsinh)]
    [cosh (compute-with-bf bfcosh)]
    [tanh (compute-with-bf bftanh)]
    [asinh (compute-with-bf bfasinh)]
    [acosh (compute-with-bf bfacosh)]
    [atanh (compute-with-bf bfatanh)]

    [erf (compute-with-bf bferf)]
    [erfc (compute-with-bf bferfc)]
    [tgamma (compute-with-bf bfgamma)]
    [lgamma (compute-with-bf bflog-gamma)]

    ;; TODO: known to be incorrect
    [round (λ (x) (if (extfl-nan? x) +nan.t (extflround x)))]
    [isnormal (lambda (x) (not (or (extfl-nan? x) (extfl-inf? x) (extfl<= (extflabs x) 0.0t0))))]
    [fmod (lambda (x y) (let ([n (extfltruncate (extfl/ (extflabs x) (extflabs y)))])
                          (extfl* (extfl- (extflabs x) (extfl* (extflabs y) n)) (extfl-sgn x))))]
    [remainder (lambda (x y) (let ([n (extflround (extfl/ x y))])
                               (extfl- x (extfl* y n))))]
    )))

;; Main interpreter

(define (tensor-layer->size arr size ctx)
  (match size
   [(? symbol?) 
    (let ([size* (dict-ref ctx size #f)])
      (cond
       [(equal? size* #f) (list (cons size (exact->inexact (length arr))))]
       [else (unless (= (length arr) size*)
              (error 'tensor-layer->size (format "Dimension of tensor argument has incorrect size. Expected: ~a=~a, Actual: ~a" size (inexact->exact size*) (length arr))))
             '()]))]      
   [(? number?)
    (unless (= (length arr) size)
      (error 'tensor-layer->size (format "Dimension of tensor argument has incorrect size. Expected: ~a, Actual: ~a" (inexact->exact size) (length arr))))
    '()]
   [_  (error 'tensor-layer->size (format "Size of array must be a variable or number. Given ~a") size)]))

(define (arg->tensor name sizes arg evaltor ctx base-rounding base-precision)
  (define p (open-input-string arg))
  (define syn (read-syntax 'str p))
  (when (eof-object? syn)
    (error 'arg->tensor "Couldn't read tensor. Check input expression."))
  (define ten (syntax-e-rec syn))
  (define ten* 
    (parameterize ([bf-rounding-mode (fpcore->bf-round base-rounding)] 
                   [bf-precision (prec->bf-bits base-precision)])
      ((eval-expr evaltor) ten (hash)))) 
  (unless (tensor? ten*)
    (error 'arg->tensor "Expected a tensor"))
  (unless (= (tensor-dim ten*) (length sizes))
    (error 'arg->tensor "Tensor argument has incorrect dimension. Expected: ~a. Actual: ~a" (length sizes) (tensor-dim ten*)))
  (append
    (let loop ([ten** ten*] [sizes* sizes])
     (cond 
      [(= (length sizes*) 1) (tensor-layer->size ten** (first sizes*) ctx)]
      [else
       (append
        (tensor-layer->size ten** (first sizes*) ctx)
        (let ([ctx* (loop (first ten**) (drop sizes* 1))])
          (unless (for/and ([i (drop ten** 1)]) (equal? ctx* (loop i (drop sizes* 1))))
            (error 'arg->tensor "Ragged tensors not supported"))
          ctx*))]))
    (list (cons name ten*))))

(define (arg->expr arg evaltor base-rounding base-precision)
  (define syn (read-syntax 'str (open-input-string arg)))
  (when (eof-object? syn)
    (error 'arg->expr "Couldn't read expression"))
  (define expr (syntax-e-rec syn))
  (parameterize ([bf-rounding-mode (fpcore->bf-round base-rounding)] 
                 [bf-precision (prec->bf-bits base-precision)])
    ((eval-expr evaltor) expr (hash))))

(define (racket-run-fpcore* name vars props* body args)
  (-> fpcore? (listof string?) (or/c real? extflonum? tensor? boolean?))
  (define-values (_ props) (parse-properties props*))
  (define base-precision (dict-ref props ':precision 'binary64))
  (define base-rounding (dict-ref props ':round 'nearestEven))
  (define evaltor 
   (match base-precision
    ['binary80 racket-binary80-evaluator]
    ['binary64 racket-double-evaluator]
    ['binary32 racket-single-evaluator]
    ['integer  racket-integer-evaluator]))
  (define ctx
    (for/fold ([ctx '()]) ([var vars] [arg args])
     (append ctx
      (match var
        [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...)
         (arg->tensor name sizes arg evaltor ctx base-rounding base-precision)]
        [`(! ,var-props* ... ,(? symbol? var*))
         (define-values (_ var-props) (parse-properties var-props*))
         (list (cons var* (arg->expr arg (dict-ref var-props ':precision base-precision))))]
        [(? symbol?)
         (list (cons var (arg->expr arg evaltor base-rounding base-precision)))]))))

  (when name (check-argument name ctx))
  (when (dict-has-key? props ':pre)
    (define pre (dict-ref props ':pre))
    (unless ((eval-expr evaltor) pre ctx)
      (error 'racket-run-fpcore* "Precondtition not met: ~a" pre)))

  (parameterize ([bf-rounding-mode (fpcore->bf-round base-rounding)] 
                 [bf-precision (prec->bf-bits base-precision)])
    ((eval-expr evaltor) body ctx)))

(define/contract (racket-run-fpcore prog args)
  (-> fpcore? (listof string?) (or/c real? extflonum? tensor? boolean?))
  (match prog
   [`(FPCore ,name (,vars ...) ,properties ... ,body)  (racket-run-fpcore* name vars properties body args)]
   [`(FPCore (,vars ...) ,properties ... ,body) (racket-run-fpcore* #f vars properties body args)]))