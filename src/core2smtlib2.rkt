#lang racket

(require math/bigfloat)
(require "lisp.rkt")

(provide core->smtlib2 smt-supported rm->smt number->smt)

(define smt-supported 
  (supported-list
    (disjoin ieee754-ops
      (curry set-member?
        '(remainder fmax fmin trunc round nearbyint
          < > <= >= == and or not
          isinf isnan isnormal signbit
          let let* array dim size ref for for* tensor tensor*)))
    (invert-const-proc (curry set-member? '(LOG2E LOG10E M_1_PI M_2_PI M_2_SQRTPI)))
    (curry set-member? '(binary32 binary64))
    ieee754-rounding-modes
    #f))

(define smt-reserved  ; Language-specific reserved names (avoid name collisions)
  '(BINARY DECIMAL HEXADECIMAL NUMERAL STRING
    _ ! as exists forall let match par
    assert check-sate check-sat-assuming declare-const
    declare-datatype declare-datatypes declare-fun
    declare-sort define-fun defined-fun-rec define-funs-rec
    define-sort echo exit get-assertions get-assignment
    get-info get-model get-option get-proof get-unsat-assumptions
    get-unsat-core get-value pop push reset reset-assertions
    set-info set-logic set-option))

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

(define (round->smt expr ctx)
  (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision)))
  (define rm (ctx-lookup-prop ctx ':round))
  (format "((_ to_fp ~a ~a) ~a ~a)" w p (rm->smt rm) expr))

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
      (let* ([q (inexact->exact (real->double-flonum x))]
             [n (numerator q)]
             [d (denominator q)])
         (if (= d 1)
             (format "((_ to_fp ~a ~a) ~a ~a)" w p (rm->smt rm) n)
             (format "((_ to_fp ~a ~a) ~a (/ ~a ~a))" w p (rm->smt rm) n d)))]))

;; Macro should grab the computation specified in the constants table
;; and wrap it with the right precision context.
;; It is critical that we use the correct rounding mode, and the correct number of bits,
;; here where we are generating the exact (but rounded) value of the constant.
;; In theory the rounding mode used here doesn't matter.
(define-syntax-rule (bf-constant expr w p rm)
  (parameterize ([bf-precision p] [bf-rounding-mode (rm->bf rm)])
    (let ([qbf expr]) (number->smt (bigfloat->rational qbf) w p rm))))

(define (constant->smt expr ctx)
  (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision)))
  (define rm (ctx-lookup-prop ctx ':round))
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
   [(? hex?) (~a (hex->racket expr))]
   [(? number?) (~a (number->smt expr w p rm))]
   [(? symbol?) (~a expr)]))

(define (operator-format op rm)
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

(define (operator->smt op args ctx)
  (define rm (ctx-lookup-prop ctx ':round))
  (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision)))
  (match (cons op args)
   [(list (or '< '> '<= '>= '== 'and 'or) args ...)
    (format (operator-format op rm)
            (string-join (map ~a args) " "))]
   [(list '!= args ...)
    (format "(and ~a)"
            (string-join
              (let loop ([args args])
                (cond
                 [(null? args) '()]
                 [else (append
                        (for/list ([b (cdr args)])
                          (format "(not (fp.eq ~a ~a))" (car args) b))
                        (loop (cdr args)))]))
            " "))]
   [(list '- a)
    (format "(fp.neg ~a)" a)]
   [(list (or 'not 'isinf 'isnan 'isnormal 'signbit) a) ;; avoid rounding on a boolean
    (format (operator-format op rm) a)]
   [(list (? operator? op) args ...)
    (round->smt (apply format (operator-format op rm) args) ctx)]))

(define (program->smt name args arg-ctxs body ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define rm (ctx-lookup-prop ctx ':round))
  (define-values (w p) (fpbits prec))
  (define type-str (fptype prec))
  (define args*
    (for/list ([arg args] [ctx arg-ctxs])
      (define-values (w p) (fpbits (ctx-lookup-prop ctx ':precision)))
      (define rm (ctx-lookup-prop ctx ':round))
      (define type-str (fptype prec))
      (format "(~a ~a)" arg type-str)))
  (format "(define-fun ~a (~a) ~a\n ~a)\n"
          name (string-join args* " ") type-str
          (round->smt body ctx)))

(define core->smtlib2
  (make-lisp-compiler "lisp"
    #:operator operator->smt
    #:constant constant->smt
    #:if-format "(ife ~a ~a ~a)"
    #:round round->smt
    #:program program->smt
    #:flags '(round-after-operation)
    #:reserved smt-reserved))

(define-compiler '("smt" "smt2" "smtlib" "smtlib2") (const "") core->smtlib2 (const "") smt-supported)
