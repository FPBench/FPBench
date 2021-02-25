#lang racket

(require "common.rkt" "tensor.rkt" "fpcore-checker.rkt")
(require math/flonum racket/extflonum math/bigfloat math/special-functions math/base generic-flonum)
(provide eval-expr eval-expr* racket-run-fpcore (struct-out evaluator))

(struct evaluator (real->repr repr->real constant function))

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
    [PI_4	        (gfl/ pi.gfl 2.gfl)]
    [1_PI	        (gfl/ 1.gfl pi.gfl)]
    [2_PI	        (gfl/ 2.gfl pi.gfl)]
    [2_SQRTPI	    (gfl/ 2 (gflsqrt pi.gfl))]
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

    [sin gflsin] [cos gflsin] [tan gfltan]
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
    [isnormal (negate gflsubnormal?)]
    [cast identity]

    [signbit (λ (x) (if (gflnegative? x) 1 0))]
    [copysign gflcopysign]))

(define/match (fpcore->gfl-round rnd)
  [('nearestEven) 'nearest]
  [('nearestAway) 'away]
  [('toPositive)  'up]
  [('toNegative)  'down]
  [('toZero)      'zero]) 

(struct float-evaluator evaluator (es nbits rnd))

(define (get-float-evaluator es nbits rnd)
  (float-evaluator gfl gfl->real get-float-cnst get-float-fun es nbits rnd))

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
    [(? number?) ((evaluator-real->repr evaltor) expr)]
    [(? hex?) ((evaluator-real->repr evaltor) (hex->racket expr))]
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
     (cond
      [(and (hash-has-key? props ':precision) (hash-has-key? props ':round))
       (define-values (prec rnd) (get-evaluator-params evaltor))
       (define nprec (hash-ref props ':precision prec))
       (define nrnd (hash-ref props ':round rnd))
       (define evaltor* (get-evaluator nprec nrnd))
       (set-evaluator-params! evaltor*)
       (define ret ((eval-expr* evaltor* rec) body ctx))
       (set-evaluator-params! evaltor)
       ret]
      [else
       ((eval-expr* evaltor rec) body ctx)])]
    [`(cast ,expr) ((evaluator-real->repr evaltor) ((eval-expr* evaltor rec) expr ctx))]
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

; Main interpreter

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

(define (arg->tensor name sizes arg evaltor ctx)
  (define p (open-input-string arg))
  (define syn (read-syntax 'str p))
  (when (eof-object? syn)
    (error 'arg->tensor "Couldn't read tensor. Check input expression."))
  (define ten (syntax-e-rec syn))
  (define ten* ((eval-expr evaltor) ten (hash)))
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

(define (arg->expr arg evaltor)
  (define syn (read-syntax 'str (open-input-string arg)))
  (when (eof-object? syn)
    (error 'arg->expr "Couldn't read expression"))
  (define expr (syntax-e-rec syn))
  ((eval-expr evaltor) expr (hash)))

(define (racket-run-fpcore* name vars props* body args)
  (-> fpcore? (listof string?) (or/c real? extflonum? tensor? boolean?))
  (define-values (_ props) (parse-properties props*))
  (define base-precision (expand-prec (dict-ref props ':precision 'binary64)))
  (define base-rounding (dict-ref props ':round 'nearestEven))
  (define evaltor (get-evaluator base-precision base-rounding))
  (define ctx
    (for/fold ([ctx '()]) ([var vars] [arg args])
     (append ctx
      (match var
        [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...)
         (set-evaluator-params! evaltor)
         (arg->tensor name sizes arg evaltor ctx)]
        [`(! ,var-props* ... ,(? symbol? var*))
         (define-values (_ var-props) (parse-properties var-props*))
         (define var-precision (expand-prec (dict-ref var-props ':precision base-precision)))
         (define var-rounding (dict-ref var-props ':round base-rounding))
         (define var-evaltor (get-evaluator var-precision var-rounding))
         (set-evaluator-params! evaltor)
         (list (cons var* (arg->expr arg var-evaltor)))]
        [(? symbol?)
         (set-evaluator-params! evaltor)
         (list (cons var (arg->expr arg evaltor)))]))))

  (when name (check-argument name ctx))
  (when (dict-has-key? props ':pre)
    (define pre (dict-ref props ':pre))
    (unless ((eval-expr evaltor) pre ctx)
      (error 'racket-run-fpcore* "Precondtition not met: ~a" pre)))

  (set-evaluator-params! evaltor)
  ((evaluator-repr->real evaltor) ((eval-expr evaltor) body ctx)))

(define/contract (racket-run-fpcore prog args)
  (-> fpcore? (listof string?) (or/c real? extflonum? tensor? boolean?))
  (match prog
   [`(FPCore ,name (,vars ...) ,properties ... ,body)  (racket-run-fpcore* name vars properties body args)]
   [`(FPCore (,vars ...) ,properties ... ,body) (racket-run-fpcore* #f vars properties body args)]))