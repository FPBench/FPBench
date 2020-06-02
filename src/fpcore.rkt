#lang racket

(require "common.rkt" math/flonum racket/extflonum math/bigfloat math/special-functions math/base)
(provide
 (struct-out evaluator) racket-integer-evaluator
  racket-binary80-evaluator racket-double-evaluator racket-single-evaluator
 fpcore? expr? context/c eval-expr* eval-expr racket-run-fpcore
 read-fpcore)

(struct evaluator (real constant function))

(define/contract (fpcore? thing)
  contract?
  (match thing
    [`(FPCore (,(? argument?) ...) ,props ... ,(? expr?))
     (properties? props)]
    [_ false]))

(define (properties? props)
  (define-values (rest props*) (parse-properties props))
  (null? rest))

(define/match (fpcore->bf-round roundmode)
  [('nearestEven) 'nearest]
  [('nearestAway) (error 'fpcore->bf-round "math/bigfloat does not support 'nearestAway")]
  [('toPositive)  'up]
  [('toNegative)  'down]
  [('toZero)      'zero])

(define (argument? arg)
  (match arg
    [(? symbol? arg) true]
    [`(! ,props ... ,(? symbol?))
     (properties? props)]
    [_ false]))

(define (expr? expr)
  (match expr
    [(? number?) true]
    [(? extflonum?) true]
    [(? constant?) true]
    [(? symbol?) true]
    [(list (? operator?) (? expr?) ...) true]
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) true]
    [`(,(or 'let 'let*) ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(,(or 'while 'while*) ,(? expr?) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?))
     true]
    [`(cast ,(? expr?)) true]
    [`(! ,props ... ,(? expr?))
      (properties? props)]
    [`(digits ,(? number?) ,(? number?) ,(? number?)) true]
    [_ false]))

(define type? (symbols 'boolean 'real))

(define/match (operator-type op args)
  [((or '- 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt
        'cbrt 'sin 'cos 'tan 'asin 'acos 'atan 'sinh 'cosh 'tanh
        'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
        'trunc 'round 'nearbyint 'cast)
    (list 'real))
   'real]
  [((or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
    (list 'real 'real))
   'real]
  [('fma (list 'real 'real 'real)) 'real]
  [((or '< '> '<= '>= '== '!=) (list 'real ...)) 'boolean]
  [((or 'isfinite 'isinf 'isnan 'isnormal 'signbit) (list 'real)) 'boolean]
  [((or 'and 'or) (list 'boolean ...)) 'boolean]
  [('not (list 'boolean)) 'boolean]
  [(_ _) #f])

(define/contract (check-expr stx ctx)
  (-> syntax? (dictof argument? type?) (cons/c expr? type?))
  (match (syntax-e stx)
    [(? number? val)
     (cons val 'real)]
    [(? hex? val)
     (cons val 'real)]
    [(? constant? val)
     (cons val (match val [(or 'TRUE 'FALSE) 'boolean] [_ 'real]))]
    [(? symbol? var)
     (unless (dict-has-key? ctx var)
       (raise-syntax-error #f "Undefined variable" stx))
     (cons var (dict-ref ctx var))]
    [(list (app syntax-e 'digits) m e b)
     (define m* (check-expr m ctx))
     (define e* (check-expr e ctx))
     (define b* (check-expr b ctx))
     (unless (and (integer? (car m*)) (integer? (car e*)) (integer? (car b*)))
      (raise-syntax-error #f "Values of digits must be integers" stx))
      (unless (>= (car b*) 2)
        (raise-syntax-error #f "Base of digits must be greater than 1" stx))
     (cons `(digits ,(car m*) ,(car e*) ,(car b*)) 'real)]
    [(list (app syntax-e 'if) test ift iff)
     (define test* (check-expr test ctx))
     (unless (equal? (cdr test*) 'boolean)
       (raise-syntax-error #f "Conditional test must return a boolean" stx test))
     (define ift* (check-expr ift ctx))
     (define iff* (check-expr iff ctx))
     (unless (equal? (cdr ift*) (cdr iff*))
       (raise-syntax-error #f "Conditional branches must have same type" stx))
     (cons `(if ,(car test*) ,(car ift*) ,(car iff*)) (cdr ift*))]
    [(cons (app syntax-e 'if) _)
     (raise-syntax-error #f "Invalid conditional statement" stx)]
    [(list (app syntax-e 'let) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by let binding" stx var))
         (syntax-e var)))
     (define vals* (map (curryr check-expr ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars* (map cdr vals*))))
     (define body* (check-expr body ctx*))
     (cons `(let (,@(map list vars* (map car vals*))) ,(car body*)) (cdr body*))]
    [(list (app syntax-e 'let*) (app syntax-e (list (app syntax-e (list vars vals)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by let binding" stx var))
         (syntax-e var)))
     (define-values (ctx* vals*)
       (for/fold ([ctx ctx] [vals '()]) ([var vars*] [val vals])
         (define val* (check-expr val ctx))
         (define ctx* (dict-set ctx var (cdr val*)))
         (values ctx* (cons val* vals))))
     (define body* (check-expr body ctx*))
     (cons `(let* (,@(map list vars* (map car (reverse vals*)))) ,(car body*)) (cdr body*))]
    [(cons (app syntax-e (or 'let 'let*)) _)
     (raise-syntax-error #f "Invalid let bindings" stx)]
    [(list (app syntax-e 'while) test (app syntax-e (list (app syntax-e (list vars inits updates)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by while loop" stx var))
         (syntax-e var)))
     (define inits* (map (curryr check-expr ctx) inits))
     (define ctx* (apply dict-set* ctx (append-map list vars* (map cdr inits*))))
     (define test* (check-expr test ctx*))
     (unless (equal? (cdr test*) 'boolean)
       (raise-syntax-error #f "While loop conditions must return a boolean" test))
     (define updates* (map (curryr check-expr ctx*) updates))
     (for ([var vars] [init inits*] [update updates*])
       (unless (equal? (cdr init) (cdr update))
         (raise-syntax-error #f "Initialization and update must have the same type in while loop" stx var)))
     (define body* (check-expr body ctx*))
     (cons `(while ,(car test*) (,@(map list vars* (map car inits*) (map car updates*))) ,(car body*)) (cdr body*))]
    [(list (app syntax-e 'while*) test (app syntax-e (list (app syntax-e (list vars inits updates)) ...)) body)
     (define vars*
       (for/list ([var vars])
         (unless (symbol? (syntax-e var))
           (raise-syntax-error #f "Only variables may be bound by while loop" stx var))
         (syntax-e var)))
     (define-values (ctx* inits-rev)
       (for/fold ([ctx ctx] [inits* '()]) ([var vars*] [init inits])
         (define init* (check-expr init ctx))
         (define ctx* (dict-set ctx var (cdr init*)))
         (values ctx* (cons init* inits*))))
     (define inits* (reverse inits-rev))
     (define test* (check-expr test ctx*))
     (unless (equal? (cdr test*) 'boolean)
       (raise-syntax-error #f "While loop conditions must return a boolean" test))
     (define updates* (map (curryr check-expr ctx*) updates))
     (for ([var vars] [init inits*] [update updates*])
       (unless (equal? (cdr init) (cdr update))
         (raise-syntax-error #f "Initialization and update must have the same type in while loop" stx var)))
     (define body* (check-expr body ctx*))
     (cons `(while* ,(car test*) (,@(map list vars* (map car inits*) (map car updates*))) ,(car body*)) (cdr body*))]
    [(cons (app syntax-e (or 'while 'while*)) _)
     (raise-syntax-error #f "Invalid while loop" stx)]
    [(list (app syntax-e '!) props ... expr)
     (define expr* (check-expr expr ctx))
     (define props* (map syntax-e props))
     (cons `(! ,@props* ,(car expr*)) (cdr expr*))]
    [(list op args ...)
     (unless (set-member? operators (syntax-e op))
       (raise-syntax-error #f "Unknown operator" op))
     (define children (map (curryr check-expr ctx) args))
     (define rtype (operator-type (syntax-e op) (map cdr children)))
     (unless rtype
       (raise-syntax-error #f (format "Invalid types for operator ~a" op) stx))
     (cons (list* (syntax-e op) (map car children)) rtype)]))

(define (syntax-e-rec stx)
  (match (syntax-e stx)
    [`(,stx-elem ...) (map syntax-e-rec stx-elem)]
    [stx* stx*]))

(define/contract (check-fpcore stx)
  (-> syntax? fpcore?)
  (match (syntax-e stx)
    [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) properties ... body)
     (define-values (annotated-args args)
       (for/lists (annotated-args args) ([var vars])
         (let ([var* (syntax-e-rec var)])
           (unless (argument? var*)
             (raise-syntax-error #f "FPCore parameters must be variables" stx var))
           (values var*
                   (if (list? var*) (last var*) var*)))))

     (define ctx
       (for/hash ([arg args])
         (values arg 'real)))

     (define properties*
       (let loop ([properties properties])
         (match properties
           [(list) (list)]
           [(list prop) (raise-syntax-error #f "Property with no value" prop)]
           [(list (app syntax-e (? property? prop)) value rest ...)
            (cons (cons prop value) (loop rest))]
           [(list prop _ ...) (raise-syntax-error #f "Invalid property" prop)])))

     (when (dict-has-key? properties* ':pre)
       (define pre (dict-ref properties* ':pre))
       (define pre* (check-expr pre ctx))
       (unless (equal? (cdr pre*) 'boolean)
         (raise-syntax-error #f "FPCore precondition must return a boolean" pre)))

     (define body* (check-expr body ctx))
     (unless (equal? (cdr body*) 'real)
       (raise-syntax-error #f "FPCore benchmark must return a real number" body))

     `(FPCore (,@annotated-args)
              ,@(apply append
                       (for/list ([(prop val) (in-dict properties*)])
                         (list prop (syntax->datum val))))
              ,(car body*))]))

(define/contract (read-fpcore name p)
  (-> any/c input-port? (or/c fpcore? eof-object?))
  (parameterize ([read-decimal-as-inexact #f])
    (define stx (read-syntax name p))
    (if (eof-object? stx) stx (check-fpcore stx))))

(define/contract context/c contract? (dictof symbol? any/c))

(define/contract ((eval-expr* evaltor rec) expr ctx)
  (-> evaluator? (-> expr? context/c any/c) (-> expr? context/c any/c))
  (match expr
    [(? number?) ((evaluator-real evaltor) expr)]
    [(? hex?) ((evaluator-real evaltor) (hex->racket expr))]
    [`(digits ,m ,e ,b) (digits->number m e b)]
    [(? extflonum?) expr]
    [(? constant?) ((evaluator-constant evaltor) expr)]
    [(? symbol?) ((evaluator-real evaltor) (dict-ref ctx expr))]
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
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
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
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
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
   (λ (x) (let ([v ((evaluator-constant racket-double-evaluator) x)])extfl->real
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
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
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

;; Interpreter from command line

(define (real->float x prec)
  (define x* (if (extflonum? x) (extfl->real x) x))
  (match prec
    ['binary80 (real->extfl x*)]
    ['binary64 (real->double-flonum x*)]
    ['binary32 (real->single-flonum x*)]
    ['integer  (inexact->exact x*)]))

(define (string->float x prec)
  (match prec
    ['binary80  (parameterize ([bf-precision 64])
                  (let ([f (bf x)])
                    (if (bf= f -0.bf) -0.0t0
                        (real->extfl (bigfloat->real f)))))]
    ['binary64  (real->double-flonum (string->number x))]
    ['binary32  (real->single-flonum (string->number x))]
    ['integer   (bf->integer (bf x))]))

(define/contract (racket-run-fpcore prog args)
  (-> fpcore? (listof string?) (or/c real? extflonum?))
  (match-define `(FPCore (,vars ...) ,props* ... ,body) prog)
  (define-values (_ props) (parse-properties props*))
  (define base-precision (dict-ref props ':precision 'binary64))
  (define base-rounding (dict-ref props ':round 'nearestEven))
  (define vars*
    (for/list ([var vars] [arg args])
      (match var
        [`(! ,var-props* ... ,(? symbol? var*))
         (define-values (_ var-props) (parse-properties var-props*))
         (cons var* (string->float arg (dict-ref var-props ':precision base-precision)))]
        [(? symbol?)
         (cons var (string->float arg base-precision))])))
  (define evaltor 
   (match base-precision
    ['binary80 racket-binary80-evaluator]
    ['binary64 racket-double-evaluator]
    ['binary32 racket-single-evaluator]
    ['integer  racket-integer-evaluator]))
  (parameterize ([bf-rounding-mode (fpcore->bf-round base-rounding)] 
                 [bf-precision (prec->bf-bits base-precision)])
        (real->float ((eval-expr evaltor) body vars*) base-precision)))

(module+ main
  (command-line
   #:program "fpcore.rkt"
   #:args args
    (port-count-lines! (current-input-port))
    (for ([prog (in-port (curry read-fpcore "stdin"))])
        (printf "~a\n" (racket-run-fpcore prog args)))))
