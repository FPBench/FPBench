#lang racket

(require "imperative.rkt"
         "fpcore-extra.rkt")

;; For reference: https://github.com/nasa/PRECiSA/blob/main/PRECiSA/src/MapRealPVSLangAST.hs

;; Missing operators:
;; Can be added:
;; | fma, exp2, expm1, cbrt, hypot,
;; | log, log10, log2, log1p,
;; | sinh, cosh, tanh,
;; | asinh, acosh, atanh,
;; | remainder, max, min,
;; | atan2,
;; | fdim, copysign.

;; Potentially can not be added:
;; | erf, erfc,
;; | tgamma, lgamma,
;; | (! <property>* <expr>),
;; | while, while*, digits
;; | PI

;; No need to add probably as the input program is strictly real type
;; | floor, trunc, round, nearbyint, cast,
;; | isfinite, isinf, isnan, isnormal, signbit.

;: FPCore supported operators
#;(define operators
    (append '(+ -
                *
                /
                fabs
                fma
                exp
                exp2
                expm1
                log
                log10
                log2
                log1p
                pow
                sqrt
                cbrt
                hypot
                sin
                cos
                tan
                asin
                acos
                atan
                atan2
                sinh
                cosh
                tanh
                asinh
                acosh
                atanh
                erf
                erfc
                tgamma
                lgamma
                ceil
                floor
                fmod
                remainder
                fmax
                fmin
                fdim
                copysign
                trunc
                round
                nearbyint
                cast)
            '(array dim size ref)
            '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit)
            '(! if let let* while while* digits)))

;; FPCore supported constants
#;(define constants
    '(E LOG2E
        LOG10E
        LN2
        LN10
        PI
        PI_2
        PI_4
        M_1_PI
        M_2_PI
        M_2_SQRTPI
        SQRT2
        SQRT1_2
        MAXFLOAT
        HUGE_VAL
        INFINITY
        NAN
        TRUE
        FALSE))

;; pow should be parsed as ^
;; mod should be parsed as %
;; == should be parsed as =
;; != should be parsed as NOT(... = ...)
;; let and let* is the same in PVS

(define pvs-supported
  (supported-list (curry set-member?
                         (append '(+ - * / fabs exp ln pow sqrt sin cos tan acos asin atan fmod)
                                 '(< > <= >= == != and or not)
                                 '(if let let*)))
                  (curry set-member? '(TRUE FALSE))
                  (curry set-member? '(binary32 binary64))
                  (curry equal? 'nearestEven)
                  #f)) ;; to check

; TODO
(define pvs-reserved ; Language-specific reserved names (avoid name collisions)
  '(f)) ;; f IS GONNA BE RESERVED TEMPORARILY AS I USE IT AS A FUNCTION

(define (operator->pvs op args ctx)
  (match op
    ['or (format "(~a OR ~a)" (car args) (cadr args))]
    ['and (format "(~a AND ~a)" (car args) (cadr args))]
    ['== (format "(~a = ~a)" (car args) (cadr args))]
    ['!= (format "(NOT(~a = ~a))" (car args) (cadr args))]
    [(or '+ '- '* '/ '< '> '<= '>=) (format "(~a ~a ~a)" (car args) op (cadr args))]
    ['pow (format "(~a ^ ~a)" (car args) (cadr args))]
    ['fmod (format "(~a % ~a)" (car args) (cadr args))]
    [(or 'exp 'ln 'sqrt 'sin 'cos 'tan 'acos 'asin 'atan) (format "(~a(~a))" op (car args))]
    ['not (format "(NOT(~a))" (car args))]
    ['fabs (format "(abs(~a))" (car args))]
    [_ (error 'operator->pvs "parsing for ~a operator is not implemented in core2pvs" op)]))

(define (constant->pvs x ctx)
  (match x
    [(or 'TRUE 'FALSE) (~a x)]
    [(? number?) (format-number x)]
    [_ (error 'constant->pvs "parsing for ~a constant is not implemented in core2pvs" x)]))

(define (type->pvs type)
  (match type
    ['binary64 ""]
    ['binary32 ""]
    ['boolean ""] ;; it is not really supported
    [_ (error 'type->pvs "unsupported type ~a in core2pvs" type)]))
  
(define (round->pvs x ctx)
  (~a x))

(define (implicit-round->pvs op arg arg-ctx ctx)
  (~a arg))

(define (round-mode->pvs mode ctx)
  (unless (equal? mode 'nearestEven)
    (error 'round-mode->pvs (format "Unsupported rounding mode ~a in core2pvs" mode)))
  (~a mode))

(define (params->pvs args)
  (string-join (map (curry format "~a: real") args)  ", "))

(define (program->pvs name args arg-ctxs body ret ctx used-vars)
  (format "~a: THEORY\nBEGIN\nf(~a): real = \n~a\n~a\nEND ~a"
            name (params->pvs args) body ret name))

; Override visitor behavior
(define-expr-visitor imperative-visitor pvs-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
   (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
   (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
   (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx))
   (define if-format "IF ~a THEN ~a ELSE ~a ENDIF")
   (values (format if-format cond* ift* iff*) ift-ctx)]
  [(visit-let vtor vars vals body #:ctx ctx)
   (define-values (ctx* vars* vals*)
      (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                #:result (values ctx* (reverse vars*) (reverse vals*)))
                ([var (in-list vars)] [val (in-list vals)])
        (define-values (val* val-ctx) (visit/ctx vtor val ctx))
        (define prec (ctx-lookup-prop val-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))

   (define let-bind-format "~a = ~a")
   (define let-format "LET ~a IN\n~a")
   (define let-bind-separator ",\n")

   (values (format let-format
                   (string-join (map (curry format let-bind-format) vars* vals*)
                                let-bind-separator)
                   body*)
           body-ctx)]
  [(visit-let* vtor vars vals body #:ctx ctx)
    (visit/ctx
      vtor
      (let loop ([vars vars] [vals vals])
        (cond [(null? vars) body]
              [else `(let (,(list (car vars) (car vals)))
                      ,(loop (cdr vars) (cdr vals)))]))
      ctx)])

(define core->pvs
  (make-imperative-compiler "pvs"
                            #:operator operator->pvs
                            #:constant constant->pvs
                            #:type type->pvs
                            #:round round->pvs
                            #:implicit-round implicit-round->pvs
                            #:round-mode round-mode->pvs
                            #:program program->pvs
                            #:flags '(never-declare)
                            #:reserved pvs-reserved
                            #:visitor pvs-visitor))

(define-compiler '(PVS) (const "") core->pvs (const "") pvs-supported)

(module+ test
  (require rackunit)
  (define compile0 core->pvs)
  (define (compile* . exprs)
    (for/list ([expr exprs] [i (in-naturals 1)])
      (compile0 expr (format "fn~a" i))))
  
  (compile*
    '(FPCore (x) (if (< x 0) (+ x 1) (- x 1)))
    '(FPCore (x) (let ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (let* ([x 1] [y x]) (+ x y)))
    ;'(FPCore (x) (while (< x 4) ([x 0.0 (+ x 1.0)]) x))
    ;'(FPCore (x) (while* (< x 4) ([x 0.0 (+ x 1.0)]) x))
    ;'(FPCore (x) (+ (foo x) 1))
    '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))
    '(FPCore (a b) (+ (* a b) (- a b))))
)
