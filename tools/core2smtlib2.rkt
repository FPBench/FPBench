#lang racket

(require math/bigfloat)
(require "common.rkt" "fpcore.rkt")
(provide compile-program rm->smt number->smt)

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

(define (fptype w p)
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

(define (constant->smt c w p rm)
  (match c
    ['E (bf-constant (bfexp 1.bf) w p rm)]
    ;['LOG2E ""]
    ;['LOG10E ""]
    ['LN2 (bf-constant (bflog 2.bf) w p rm)]
    ['LN10 (bf-constant (bflog 10.bf) w p rm)]
    ['PI (bf-constant pi.bf w p rm)]
    ['PI_2 (bf-constant (bf/ pi.bf 2.bf) w p rm)] ; ok since division by 2 is exact
    ['PI_4 (bf-constant (bf/ pi.bf 4.bf) w p rm)] ; ok since division by 4 is exact
    ;['1_PI ""]
    ;['2_PI ""]
    ;['2_SQRTPI ""]
    ['SQRT2 (bf-constant (bfsqrt 2.bf) w p rm)]
    ['SQRT1_2 (bf-constant (bfsqrt (bf/ 1.bf 2.bf)) w p rm)]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY (format "(_ +oo ~a ~a)" w p)]
    ['NAN (format "(_ NaN ~a ~a)" w p)]
    [_ (error 'constant->smt "Unsupported constant ~a" c)]))

(define (operator->smt op rm)
  (match op
    ['+ (format "(fp.add ~a ~~a ~~a)" (rm->smt rm))]
    ['- (format "(fp.sub ~a ~~a ~~a)" (rm->smt rm))]
    ['* (format "(fp.mul ~a ~~a ~~a)" (rm->smt rm))]
    ['/ (format "(fp.div ~a ~~a ~~a)" (rm->smt rm))]
    ['fabs "(fp.abs ~a)"]
    ['fma (format "(fp.fma ~a ~~a ~~a ~~a)" (rm->smt rm))]
    ;['exp ""]
    ;['exp2 ""]
    ;['expm1 ""]
    ;['log ""]
    ;['log10 ""]
    ;['log2 ""]
    ;['log1p ""]
    ;['pow ""]
    ['sqrt (format "(fp.sqrt ~a ~~a)" (rm->smt rm))]
    ;['cbrt ""]
    ;['hypot ""]
    ;['sin ""]
    ;['cos ""]
    ;['tan ""]
    ;['asin ""]
    ;['acos ""]
    ;['atan ""]
    ;['atan2 ""]
    ;['sinh ""]
    ;['cosh ""]
    ;['tanh ""]
    ;['asinh ""]
    ;['acosh ""]
    ;['atanh ""]
    ;['erf ""]
    ;['erfc ""]
    ;['tgamma ""]
    ;['lgamma ""]
    ;['ceil ""]
    ;['floor ""]
    ;['fmod ""]
    ;; The behavior of fp.rem may not be fully compliant with C11
    ;; remainder function, however based on the documentation things
    ;; seem promising.
    ['remainder "(fp.rem ~a ~a)"]
    ['fmax "(fp.max ~a ~a)"]
    ['fmin "(fp.min ~a ~a)"]
    ;['fdim ""]
    ;['copysign ""]
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
    ['isfinite "(not (fp.isInfinite ~a))"]
    ['isinf "(fp.isInfinite ~a)"]
    ['isnan "(fp.isNaN ~a)"]
    ['isnormal "(fp.isNormal ~a)"]
    ['signbit "(fp.isNegative ~a)"]
    [_ (error 'operator->smt "Unsupported operator ~a" op)]))

(define (application->smt operator args rm)
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
    [(list (? operator? op) args ...)
     (apply format (operator->smt op rm) args)]
    [_ (error 'application->smt "Unsupported application ~a ~a" operator args)]))

(define (expr->smt expr w p rm)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (format "(let (~a) ~a)"
             (string-join
              (for/list ([var vars] [val vals])
                (format "(~a ~a)"
                        (fix-name var) (expr->smt val w p rm)))
              " ")
             (expr->smt body w p rm))]
    [`(if ,condition ,true-branch ,false-branch)
     (format "(ite ~a ~a ~a)"
             (expr->smt condition w p rm)
             (expr->smt true-branch w p rm)
             (expr->smt false-branch w p rm))]
    [`(while ,condition ([,vars ,inits ,updates] ...) ,body)
     (error 'expr->smt "Loop unrolling not supported: ~a" expr)]
    [(list (? operator? operator) args ...)
     (application->smt operator
                       (for/list ([arg args])
                         (expr->smt arg w p rm))
                       rm)]
    [(? constant?)
     (constant->smt expr w p rm)]
    [(? symbol?)
     (fix-name expr)]
    [(? number?)
     (number->smt expr w p rm)]
    [_ (error 'expr->smt "Unsupported expr ~a" expr)]))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))
  (define rm (dict-ref properties ':round 'nearestEven))

  (define-values (w p)
    (match type
      ['binary16 (values 5 11)]
      ['binary32 (values 8 24)]
      ['binary64 (values 11 53)]
      ['binary128 (values 15 113)]
      [_ (error 'compile-program "Unsupported precision type ~a" type)]))

  (define arg-strings
    (for/list ([var args])
      (format "(~a ~a)" (fix-name (if (list? var) (car var) var)) (fptype w p))))

  (format "(define-fun ~a (~a) ~a\n ~a)"
          (fix-name name)
          (string-join arg-strings " ")
          (fptype w p)
          (expr->smt body w p rm)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
