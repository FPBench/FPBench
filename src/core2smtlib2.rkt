#lang racket

(require math/bigfloat)
(require "common.rkt" "compilers.rkt" "functional.rkt" "supported.rkt")
(provide core->smtlib2 smt-supported rm->smt number->smt)

(define smt-supported (supported-list
  (invert-op-list '(while* let* while != exp exp2 expm1 log log10 log2 log1p pow cbrt
                    hypot sin cos tan asin acos atan atan2 sinh cosh tanh asinh acosh 
                    atanh erf erfc tgamma lgamma ceil floor fmod fdim copysign isfinite))
  (invert-const-list '(LOG2E LOG10E M_1_PI M_2_PI M_2_SQRTPI))
  '(binary32 binary64)))

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

; external use in functional.rkt
(define (smt-fix-name name names)
  (fix-name name))

(define/match (fpbits type)
  [('binary16) (values 5 11)]
  [('binary32) (values 8 24)]
  [('binary64) (values 11 53)]
  [('binary128) (values 15 113)])

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

;; Any exact number can be written out (exactly) as a division. In general, adding
;; lots of real divisions to a formula is probably not wise.
;; However, at least with z3, defining constants in this way does not seem to cause
;; any significant issues.
(define (number->smt x w p rm)
  (match x
    [(or +inf.0 +inf.f) (format "(_ +oo ~a ~a)" w p)]
    [(or -inf.0 -inf.f) (format "(_ -oo ~a ~a)" w p)]
    [(or +nan.0 +nan.f) (format "(_ NaN ~a ~a)" w p)]
    [_ (let* ([q (if (single-flonum? x)
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
  (define-values (w p) (fpbits (dict-ref ctx ':precision 'binary64)))
  (define rm (dict-ref ctx ':round 'nearestEven))
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
    [(? number?) (number->smt expr w p rm)]
    [(? symbol?) (format "~a" expr)]))

(define (operator->smt op args ctx)
  (define arg-list (string-join args " "))
  (define rm (dict-ref ctx ':round 'nearestEven))
  (match op
    ['neg (format "(fp.neg ~a)" arg-list)]
    ['+ (format "(fp.add ~a ~a)" (rm->smt rm) arg-list)]
    ['- (format "(fp.sub ~a ~a)" (rm->smt rm) arg-list)]
    ['* (format "(fp.mul ~a ~a)" (rm->smt rm) arg-list)]
    ['/ (format "(fp.div ~a ~a)" (rm->smt rm) arg-list)]
    ['fabs (format "(fp.abs ~a)" arg-list)]
    ['fma (format "(fp.fma ~a ~a)" (rm->smt rm) arg-list)]
    ['sqrt (format "(fp.sqrt ~a ~a)" (rm->smt rm) arg-list)]
    ;; The behavior of fp.rem may not be fully compliant with C11
    ;; remainder function, however based on the documentation things
    ;; seem promising.
    ['remainder (format "(fp.rem ~a)" arg-list)]
    ['fmax (format "(fp.max ~a)" arg-list)]
    ['fmin (format "(fp.min ~a)" arg-list)]
    ['trunc (format "(fp.roundToIntegral ~a ~a)" (rm->smt 'toZero) arg-list)]
    ['round (format "(fp.roundToIntegral ~a ~a)" (rm->smt 'nearestAway) arg-list)]
    ['nearbyint (format "(fp.roundToIntegral ~a ~a)" (rm->smt rm) arg-list)]
    ;; Comparisons and logical ops take one format argument,
    ;; which is a pre-concatenated string of inputs.
    ['< (format "(fp.lt ~a)" arg-list)]
    ['> (format "(fp.gt ~a)" arg-list)]
    ['<= (format "(fp.leq ~a)" arg-list)]
    ['>= (format "(fp.geq ~a)" arg-list)]
    ['== (format "(fp.eq ~a)" arg-list)]
    ;['!= ""] ;; needs special logic
    ['and (format "(and ~a)" arg-list)]
    ['or (format "(or ~a)" arg-list)]
    ['not (format "(not ~a)" arg-list)]
    ;['isfinite ""] ;; needs special logic to avoid computing inner expression twice
    ['isinf (format "(fp.isInfinite ~a)" arg-list)]
    ['isnan (format "(fp.isNaN ~a)" arg-list)]
    ['isnormal (format "(fp.isNormal ~a)" arg-list)]
    ['signbit (format "(fp.isNegative ~a)" arg-list)]))

(define (declaration->smt var val)
  (format "(~a ~a)" var val))

(define (block->smt name indent)
  (match name
    ['let   "(let (~a) ~a)"]
    ['if    "(ite ~a ~a ~a)"]
    [_ (error 'block->smt "Unsupported block ~a" name)]))

(define (function->smt name args body ctx)
  (define type-str (fptype (dict-ref ctx ':precision 'binary64)))
  (define arg-strings
    (for/list ([var args])
      (format "(~a ~a)" (fix-name (if (list? var) (car var) var)) type-str)))
  (format "(define-fun ~a (~a) ~a\n ~a)"
          (fix-name name)
          (string-join arg-strings " ")
          type-str
          body))

(define smt-language (functional "smt" smt-fix-name operator->smt constant->smt declaration->smt block->smt function->smt))

;;; Exports

(define (core->smtlib2 prog name) (parameterize ([*func-lang*  smt-language]) (core->functional prog name)))
(define-compiler '("smt" "smt2" "smtlib" "smtlib2") (const "") core->smtlib2 (const "") smt-supported)
