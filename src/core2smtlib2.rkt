#lang racket

(require math/bigfloat)
(require "common.rkt" "compilers.rkt" "supported.rkt")
(provide core->smtlib2 smt-supported rm->smt number->smt)

(define smt-supported 
  (supported-list
    (invert-op-proc 
      (curry set-member? '(while* while exp exp2 expm1 log log10 log2 log1p pow cbrt
                           hypot sin cos tan asin acos atan atan2 sinh cosh tanh asinh acosh 
                           atanh erf erfc tgamma lgamma ceil floor fmod fdim copysign isfinite)))
    (invert-const-proc (curry set-member? '(LOG2E LOG10E M_1_PI M_2_PI M_2_SQRTPI)))
    (curry set-member? '(binary32 binary64))
    ieee754-rounding-modes))

(define smt-reserved '())  ; Language-specific reserved names (avoid name collisions)

;; Extremely simple html-inspired escapes. The understanding is that the
;; only difference between symbols is that FPCore allows : in names,
;; while SML-LIB does not.
(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (match char
       [#\& "&amp"]
       [#\: "&col"]
       [_ (string char)]))
   ""))

(define/match (fpbits type)
  [('binary16) (values 5 11)]
  [('binary32) (values 8 24)]
  [('binary64) (values 11 53)]
  [('binary128) (values 15 113)])

(define (bits->prec es sig)
  (match (list es sig)
   ['(5 11)   'binary16]
   ['(8 24)   'binary32]
   ['(11 53)  'binary64]
   ['(15 113) 'binary128]))

(define (fptype type)
  (define-values (w p) (fpbits type))
  (format "(_ FloatingPoint ~a ~a)" w p))

(define/match (rm->bf rm)
  [('nearestEven) 'nearest]
  ;[('nearestAway) ] ;; math/bigfloat does not support this mode
  [('toPositive) 'up]
  [('toNegative) 'down]
  [('toZero) 'zero]
  [(_) (error 'rm->bf "Unsupported rounding mode ~a" rm)])

(define/match (rm->smt rm)
  [('nearestEven) "roundNearestTiesToEven"]
  [('nearestAway) "roundNearestTiesToAway"]
  [('toPositive) "roundTowardPositive"]
  [('toNegative) "roundTowardNegative"]
  [('toZero) "roundTowardZero"]
  [(_) (error 'rm->smt "Unsupported rounding mode ~a" rm)])

(define (cast->smt x w p rm)
  (format "((_ to_fp ~a ~a) ~a ~a)" w p rm x))

;; Any exact number can be written out (exactly) as a division. In general, adding
;; lots of real divisions to a formula is probably not wise.
;; However, at least with z3, defining constants in this way does not seem to cause
;; any significant issues.
(define (number->smt x w p rm)
  (cond
   [(and (infinite? x) (positive? x)) (format "(_ +oo ~a ~a)" w p)]
   [(and (infinite? x) (negative? x)) (format "(_ -oo ~a ~a)" w p)]
   [(nan? x) (format "(_ NaN ~a ~a)" w p)]
   [else 
      (let* ([q (if (single-flonum? x)
                     ;; Workaround for misbehavior of inexact->exact with single-flonum inputs
                     (inexact->exact (real->double-flonum x))
                     (inexact->exact x))]
              [n (numerator q)]
              [d (denominator q)])
         (if (= d 1)
             (format "((_ to_fp ~a ~a) ~a ~a)" w p (rm->smt rm) n)
             (format "((_ to_fp ~a ~a) ~a (/ ~a ~a))" w p (rm->smt rm) n d)))]))

;; Macro should grab the computation specified in the constants table
;; and wrap it with the right precision context.
(define-syntax-rule (bf-constant expr w p rm)
  (begin
   (bf-precision p)
   ;; It is critical that we use the correct rounding mode, and the correct number of bits,
   ;; here where we are generating the exact (but rounded) value of the constant.
   (bf-rounding-mode (rm->bf rm))
   (let ([qbf expr])
     ;; In theory the rounding mode used here doesn't matter.
     (number->smt (bigfloat->rational qbf) w p rm)))
  )

(define (constant->smt expr ctx)
  (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision 'binary64)))
  (define rm (ctx-lookup-prop ctx ':round 'nearestEven))
  (match expr
    ['E (bf-constant (bfexp 1.bf) w p rm)]
    ['LN2 (bf-constant (bflog 2.bf) w p rm)]
    ['LN10 (bf-constant (bflog 10.bf) w p rm)]
    ['PI (bf-constant pi.bf w p rm)]
    ['PI_2 (bf-constant (bf/ pi.bf 2.bf) w p rm)] ; ok since division by 2 is exact
    ['PI_4 (bf-constant (bf/ pi.bf 4.bf) w p rm)] ; ok since division by 4 is exact
    ['SQRT2 (bf-constant (bfsqrt 2.bf) w p rm)]
    ['SQRT1_2 (bf-constant (bfsqrt (bf/ 1.bf 2.bf)) w p rm)]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY (format "(_ +oo ~a ~a)" w p)]
    ['NAN (format "(_ NaN ~a ~a)" w p)]
    [(? hex?) (hex->racket expr)]
    [(? number?) (number->smt expr w p rm)]
    [(? symbol?) (format "~a" expr)]))

(define (operator->smt op rm)
  (match op
    ['+ (format "(fp.add ~a ~~a ~~a)" (rm->smt rm))]
    ['- (format "(fp.sub ~a ~~a ~~a)" (rm->smt rm))]
    ['* (format "(fp.mul ~a ~~a ~~a)" (rm->smt rm))]
    ['/ (format "(fp.div ~a ~~a ~~a)" (rm->smt rm))]
    ['fabs "(fp.abs ~a)"]
    ['fma (format "(fp.fma ~a ~~a ~~a ~~a)" (rm->smt rm))]
    ['sqrt (format "(fp.sqrt ~a ~~a)" (rm->smt rm))]
    ;; The behavior of fp.rem may not be fully compliant with C11
    ;; remainder function, however based on the documentation things
    ;; seem promising.
    ['remainder "(fp.rem ~a ~a)"]
    ['fmax "(fp.max ~a ~a)"]
    ['fmin "(fp.min ~a ~a)"]
    ['trunc (format "(fp.roundToIntegral ~a ~~a)" (rm->smt 'toZero))]
    ['round (format "(fp.roundToIntegral ~a ~~a)" (rm->smt 'nearestAway))]
    ['nearbyint (format "(fp.roundToIntegral ~a ~~a)" (rm->smt rm))]
    ;; Comparisons and logical ops take one format argument,
    ;; which is a pre-concatenated string of inputs.
    ['< "(fp.lt ~a)"]
    ['> "(fp.gt ~a)"]
    ['<= "(fp.leq ~a)"]
    ['>= "(fp.geq ~a)"]
    ['== "(fp.eq ~a)"]
    ;['!= ""] ;; needs special logic
    ['and "(and ~a)"]
    ['or "(or ~a)"]
    ['not "(not ~a)"]
    ;['isfinite ""] ;; needs special logic to avoid computing inner expression twice
    ['isinf "(fp.isInfinite ~a)"]
    ['isnan "(fp.isNaN ~a)"]
    ['isnormal "(fp.isNormal ~a)"]
    ['signbit "(fp.isNegative ~a)"]))

(define (application->smt operator args ctx rrnd)
  (define rm (ctx-lookup-prop ctx ':round 'nearestEven))
  (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision 'binary64)))
  (match (cons operator args)
    [(list (or '< '> '<= '>= '== 'and 'or) args ...)
     (format (operator->smt operator rm)
             (string-join
              (for/list ([a args]) (format "~a" a))
              " "))]
    [(list '!= args ...)
     (format "(and ~a)"
             (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                     (for/list ([b (cdr args)])
                       (format "(not (fp.eq ~a ~a))" (car args) b))
                     (loop (cdr args)))))
              " "))]
    [(list '- a)
     (format "(fp.neg ~a)" a)]
    [(list (or 'not 'isinf 'isnan 'isnormal 'signbit) a) ;; avoid rounding on a boolean
     (format (operator->smt operator rm) a)]
    [(list (? operator? op) args ...)
     (let ([expr_c (apply format (operator->smt op rm) args)])
       (if rrnd expr_c (format "((_ to_fp ~a ~a) ~a ~a)" w p (rm->smt rm) expr_c)))]))

;; Compiler for SMT
; Separate from functional compiler since SMT can only perform operations on 
; arguments of the same precision. The best (and not ideal) solution is to
; round the arguments to the highest precision neccesary to preform the operation,
; and then round down. Thus, we need to track the "highest precision" during the
; compilation.

(define (expr->smt expr ctx w p)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define rm (ctx-lookup-prop ctx ':round 'nearestEven))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                  #:result (values ctx* (reverse vars*) (reverse vals*)))
                  ([var vars] [val vals])
          (let*-values ([(val_c w* p*) (expr->smt val ctx w p)]
                        [(cx name) (ctx-unique-name ctx* var (bits->prec w* p*))])
            (values cx (cons name vars*) (cons val_c vals*)))))
      (define-values (body* w* p*) (expr->smt body ctx* w p))
      (values
        (format "(let (~a) ~a)"
          (string-join
            (for/list ([var* vars*] [val* vals*])
              (let-values ()
                (format "(~a ~a)" var* val*)))
            " ")
          body*)
        w* p*)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define rm (ctx-lookup-prop ctx ':round 'nearestEven))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                  #:result (values ctx* (reverse vars*) (reverse vals*)))
                  ([var vars] [val vals])
          (let*-values ([(val_c w* p*) (expr->smt val ctx* w p)]
                        [(cx name) (ctx-unique-name ctx* var (bits->prec w* p*))])
            (values cx (cons name vars*) (cons val_c vals*)))))
      (define-values (body* w* p*) (expr->smt body ctx* w p))
      (values
        (let loop ([vars vars*] [vals vals*])
          (cond
           [(null? vars) body*]
           [else
            (format "(let ((~a ~a)) ~a)" (car vars) (car vals)
                    (loop (cdr vars) (cdr vals)))]))
        w* p*)]

    [`(if ,cond ,ift ,iff)
      (define li (list cond ift iff))
      (define-values (li* max-w max-p)
        (for/fold ([li* '()] [max-w w] [max-p p]) 
                  ([v li])
          (let-values ([(v_c w* p*) (expr->smt v ctx w p)])
            (values (flatten (cons li* v_c)) (if (> w* max-w) w* max-w) (if (> p* max-p) p* max-p)))))
      (values (format "(ite ~a ~a ~a)" (first li*) (second li*) (third li*)) w p)]

    [`(cast ,body)
      (define rm (ctx-lookup-prop ctx ':round 'nearestEven))
      (let-values ([(body* w* p*)  (expr->smt body ctx w p)])
        (values (format "((_ to_fp ~a ~a) ~a ~a)" w p (rm->smt rm) body*) w p))]

    [(list '! props ... body)
      (define ctx* (ctx-update-props ctx props))
      (define-values (w* p*) (fpbits (ctx-lookup-prop ctx* ':precision)))
      (define rm (ctx-lookup-prop ctx* ':round 'nearestEven))
      (let-values ([(body* w* p*) (expr->smt body ctx* w* p*)])
        (values body* w* p*))]
      
    [(list (? operator? operator) args ...)
      (define-values (args* ws ps max-w max-p)
        (for/fold ([args* '()] [ws '()] [ps '()] [max-w w] [max-p p]) 
                  ([arg args])
          (let-values ([(arg* w* p*) (expr->smt arg ctx w p)])
            (values (flatten (cons args* arg*)) (flatten (cons ws w*)) (flatten (cons ps p*))
                    (if (> w* max-w) w* max-w) (if (> p* max-p) p* max-p))))) 
      (define args_r
        (for/list ([arg* args*] [w* ws] [p* ps])
          (if (and (equal? w* max-w) (equal? p* max-p))
              arg*
              (format "((_ to_fp ~a ~a) ~a ~a)" max-w max-p (rm->smt (ctx-lookup-prop ctx ':round 'nearestEven)) arg*))))
      (values (application->smt operator args_r ctx (and (equal? w max-w) (equal? p max-p))) w p)] 

    [(list 'digits m e b) (values (constant->smt (digits->number m e b) ctx) w p)]
    [(? constant?) (values (constant->smt expr ctx) w p)]
    [(? hex?) (values (constant->smt (hex->racket expr) ctx) w p)]
    [(? number?) (values (constant->smt expr ctx) w p)]
    [(? symbol?) 
      (let*-values ([(decl-prec) (ctx-lookup-prec ctx expr)]
                    [(w* p*) (fpbits decl-prec)])
        (values (ctx-lookup-name ctx expr) w* p*))]))

;; Exports

(define (core->smtlib2 prog name) 
  (parameterize ([*gensym-fix-name* fix-name] 
                 [*used-names* (mutable-set)] 
                 [*gensym-collisions* 1]) 
    (define-values (args props body)
     (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define default-ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
    (define ctx (ctx-reserve-names default-ctx smt-reserved))
    (define prec (ctx-lookup-prop ctx ':precision 'binary64))
    (define type-str (fptype prec))
    (define-values (w p) (fpbits prec))

    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
        (set! ctx cx)
        fname))  
    (define-values (ctx* args*)
      (for/fold ([ctx* ctx] [args* '()]) 
                ([arg args])
        (let-values ([(cx name) (ctx-unique-name ctx* arg)])
            (values cx (flatten (cons args* (format "(~a ~a)" name type-str)))))))  
    (define-values (body_c w* p*) (expr->smt body ctx* w p))
  
    (format "(define-fun ~a (~a) ~a\n ~a)\n"
            func-name
            (string-join args* " ")
            type-str
            (if (and (= w w*) (= p p*))
                body_c
                (format "((_ to_fp ~a ~a) ~a ~a)" w p
                        (rm->smt (ctx-lookup-prop ctx ':round)) body_c)))))

(define-compiler '("smt" "smt2" "smtlib" "smtlib2") (const "") core->smtlib2 (const "") smt-supported)
