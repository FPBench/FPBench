#lang racket

(require "common.rkt" math/flonum math/bigfloat math/special-functions math/base)
(provide
 (struct-out evaluator) racket-double-evaluator racket-single-evaluator
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

(define (argument? arg)
  (match arg
    [(? symbol? arg) true]
    [`(! ,props ... ,(? symbol?))
     (properties? props)]
    [_ false]))

(define (expr? expr)
  (match expr
    [(? number?) true]
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
    [_ false]))

(define type? (symbols 'boolean 'real))

;; TODO: add updated number definition (hex, rational, and digits)

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
    [(? constant? val)
     (cons val (match val [(or 'TRUE 'FALSE) 'boolean] [_ 'real]))]
    [(? symbol? var)
     (unless (dict-has-key? ctx var)
       (raise-syntax-error #f "Undefined variable" stx))
     (cons var (dict-ref ctx var))]
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
     (define-values (ctx* inits*)
       (for/fold ([ctx ctx] [inits* '()]) ([var vars*] [init inits])
         (define init* (check-expr init ctx))
         (define ctx* (dict-set ctx var (cdr init*)))
         (values ctx* (cons init* inits*))))
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
     (define evaltor*
       (match (dict-ref props ':precision evaltor)
         ['binary64 racket-double-evaluator]
         ['binary32 racket-single-evaluator]
         [_ evaltor]))
     ((eval-expr* evaltor* rec) body ctx)]
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

(define (compute-with-bf fn)
  (lambda (arg)
    (let-values ([(p bf->float)
                  (cond
                    [(single-flonum? arg) (values 24 (compose real->single-flonum bigfloat->real))]
                    [(double-flonum? arg) (values 53 (compose real->double-flonum bigfloat->real))])])
      (parameterize ([bf-precision p])
        (bf->float (fn (bf arg)))))))

(define (compute-with-bf-2 fn)
  (lambda (arg1 arg2)
    (let-values ([(p bf->float)
                  (cond
                    [(single-flonum? arg1) (values 24 (compose real->single-flonum bigfloat->real))]
                    [(double-flonum? arg1) (values 53 (compose real->double-flonum bigfloat->real))])])
      (parameterize ([bf-precision p])
        (bf->float (fn (bf arg1) (bf arg2)))))))

(define (compute-with-bf-fma arg1 arg2 arg3)
  (let-values ([(p bf->float)
                (cond
                  [(single-flonum? arg1) (values 24 (compose real->single-flonum bigfloat->real))]
                  [(double-flonum? arg1) (values 53 (compose real->double-flonum bigfloat->real))])])
    (parameterize ([bf-precision (+ (* p 2) 1)])
      (bf->float (bf+ (bf* (bf arg1) (bf arg2)) (bf arg3))))))

(define (my!= #:cmp [cmp =] . args) (not (check-duplicates args cmp)))
(define (my= #:cmp [cmp =] . args)
  (match args ['() true] [(cons hd tl) (andmap (curry cmp hd) tl)]))

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
    [M_1_PI	0.31830988618379067154]
    [M_2_PI	0.63661977236758134308]
    [M_2_SQRTPI	1.12837916709551257390]
    [SQRT2	1.41421356237309504880]
    [SQRT1_2	0.70710678118654752440]
    [NAN	+nan.0]
    [INFINITY	+inf.0]
    [TRUE #t] [FALSE #f])
   (table-fn
    [+ +] [- -] [* *] [/ /] [fabs abs]
    ;; probably more of these should use mpfr
    [exp exp] [log log]
    [pow expt] [sqrt sqrt]
    [sin sin] [cos cos] [tan tan]
    [asin asin] [acos acos] [atan atan] [atan2 atan]
    [ceil ceiling] [floor floor] [trunc truncate]
    [fmax max] [fmin min]
    [< <] [> >] [<= <=] [>= >=] [== my=] [!= my!=]
    [and (λ (x y) (and x y))] [or (λ (x y) (or x y))] [not not]
    [isnan nan?] [isinf infinite?]
    [nearbyint round]
    [cast identity]

    ;; emulate behavior
    [isfinite (λ (x) (not (or (nan? x) (infinite? x))))]
    [fdim (λ (x y) (if (> x y)
                       (- x y)
                       +0.0))]
    [signbit (λ (x) (= (bigfloat-signbit (bf x)) 1))]
    [copysign (λ (x y) (if (= (bigfloat-signbit (bf y)) 1)
                           (- (abs x))
                           (abs x)))]

    ;; use mpfr

    ;[cbrt (lambda (x) (expt x 1/3))]
    [cbrt (compute-with-bf bfcbrt)]
    ;[exp2 (λ (x) (expt 2.0 x))]
    [exp2 (compute-with-bf bfexp2)]
    ;[expm1 (lambda (x) (- (exp x) 1.0))]
    [expm1 (compute-with-bf bfexpm1)]
    ;[log1p (lambda (x) (log (+ 1.0 x)))]
    [log1p (compute-with-bf bflog1p)]
    ;[log10 (λ (x) (/ (log x) (log 10.0)))]
    [log10 (compute-with-bf bflog10)]
    ;[log2 (λ (x) (/ (log x) (log 2.0)))]
    [log2 (compute-with-bf bflog2)]

    ;[fma (lambda (a b c) (+ (* a b) c))]
    [fma compute-with-bf-fma]
    ;[hypot flhypot]
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
               [real real->single-flonum]
               [constant (λ (x) (real->single-flonum ((evaluator-constant racket-double-evaluator) x)))]))

(define/contract (racket-run-fpcore prog vals)
  (-> fpcore? (listof real?) real?)
  (match-define `(FPCore (,vars ...) ,props* ... ,body) prog)
  (define-values (_ props) (parse-properties props*))
  (define base-precision (dict-ref props ':precision 'binary64))
  (define vars*
    (for/list ([var vars] [val vals])
      (match var
        [`(! ,var-props* ... ,(? symbol? var*))
         (define-values (_ var-props) (parse-properties var-props*))
         (cons var*
           (match (dict-ref var-props ':precision base-precision)
             ['binary64 (real->double-flonum val)]
             ['binary32 (real->single-flonum val)]))]
        [(? symbol?)
         (cons var val)])))
  (define evaltor (match base-precision
    ['binary64 racket-double-evaluator]
    ['binary32 racket-single-evaluator]))
  ((eval-expr evaltor) body vars*))

(module+ main
  (command-line
   #:program "fpcore.rkt"
   #:args args
   (port-count-lines! (current-input-port))
   (let ([vals (map (compose real->double-flonum string->number) args)])
     (for ([prog (in-port (curry read-fpcore "stdin"))])
       (printf "~a\n" (racket-run-fpcore prog vals))))))
