#lang racket

(require math/bigfloat)
(require "common.rkt" "compilers.rkt" "functional.rkt" "supported.rkt")
(provide core->wls wls-supported number->wls prec->wls)

(define wls-supported (supported-list
  fpcore-ops
  fpcore-consts
  (curry set-member? '(binary64 real integer))
  (curry set-member? '(nearestEven))))

(define wls-reserved '(E Pi))  ; Language-specific reserved names (avoid name collisions)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9$]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (prec->wls prec)
  (match prec 
    ['binary64 "MachinePrecision"]
    ['real     "Infinity"]
    ['integer  "Infinity"]))

(define (number->wls x)
  (match x
    [(or +inf.0 +inf.f) "Infinity"]
    [(or -inf.0 -inf.f) "(-Infinity)"]
    [(or +nan.0 +nan.f) "Indeterminate"]
    [_ (let* ([q (if (single-flonum? x)
                     ;; Workaround for misbehavior of inexact->exact with single-flonum inputs
                     (parameterize ([bf-precision 24]) (inexact->exact (bigfloat->flonum (bf x))))
                     (inexact->exact x))]
              [n (numerator q)]
              [d (denominator q)])
        (if (= d 1)
            (format "~a" n)
            (format "(~a/~a)" n d)))]))

(define (constant->wls expr ctx)
  (define wls-prec (prec->wls (ctx-lookup-prop ctx ':precision 'binary64)))
  (match expr
    ['E       (format "N[E, ~a]" wls-prec)]
    ['LOG2E   (format "N[Log[2, E], ~a]" wls-prec)]
    ['LOG10E  (format "N[Log[10, E], ~a]" wls-prec)]
    ['LN2     (format "N[Log[2], ~a]" wls-prec)]
    ['LN10    (format "N[Log[10], ~a]" wls-prec)]
    ['PI      (format "N[Pi, ~a]" wls-prec)]
    ['PI_2    (format "N[(Pi / 2), ~a]" wls-prec)]
    ['PI_4    (format "N[(Pi / 4), ~a]" wls-prec)]
    ['M_1_PI  (format "N[(1 / Pi), ~a]" wls-prec)]
    ['M_2_PI  (format "N[(2 / Pi), ~a]" wls-prec)]
    ['M_2_SQRTPI  (format "N[(2 / Sqrt[Pi]), ~a]" wls-prec)]
    ['SQRT2   (format "N[Sqrt[2], ~a]" wls-prec)]
    ['SQRT1_2 (format "N[Sqrt[1 / 2], ~a]" wls-prec)]
    ['TRUE "True"]
    ['FALSE "False"]
    ['INFINITY "Infinity"]
    ['NAN "Indeterminate"]
    [(list 'digits (? number? m) (? number? e) (? number? b))
     (format "N[~a * ~a ^ ~a, ~a]"
             (number->wls m)
             (number->wls b)
             (number->wls e)
             wls-prec)]
    [(? hex?) (hex->racket expr)]
    [(? number?) (format "N[~a, ~a]" (number->wls expr) wls-prec)]
    [(? symbol?) expr]))

(define/match (operator->wls op)
  [('+) "(~a + ~a)"]
  [('-) "(~a - ~a)"]
  [('*) "(~a * ~a)"]
  [('/) "(~a / ~a)"]
  [('fabs) "Abs[~a]"]
  [('fma) "(~a * ~a + ~a)"]
  [('exp) "Exp[~a]"]
  [('exp2) "Power[2, ~a]"]
  [('expm1) "(Exp[~a] - 1)"]
  [('log) "Log[~a]"]
  [('log10) "Log[10, ~a]"]
  [('log2) "Log[2, ~a]"]
  [('log1p) "Log[1 + ~a]"]
  [('pow) "Power[~a, ~a]"]
  [('sqrt) "Sqrt[~a]"]
  [('cbrt) "Power[~a, 1/3]"]
  [('hypot) "Sqrt[~a ^ 2 + ~a ^ 2]"]
  [('sin) "Sin[~a]"]
  [('cos) "Cos[~a]"]
  [('tan) "Tan[~a]"]
  [('asin) "ArcSin[~a]"]
  [('acos) "ArcCos[~a]"]
  [('atan) "ArcTan[~a]"]
  [('atan2) "ArcTan[~a / ~a]"]
  [('sinh) "Sinh[~a]"]
  [('cosh) "Cosh[~a]"]
  [('tanh) "Tanh[~a]"]
  [('asinh) "ArcSinh[~a]"]
  [('acosh) "ArcCosh[~a]"]
  [('atanh) "ArcTanh[~a]"]
  [('erf) "Erf[~a]"]
  [('erfc) "Erfc[~a]"]
  [('tgamma) "Gamma[~a]"]
  [('lgamma) "LogGamma[~a]"]
  [('ceil) "Ceiling[~a]"]
  [('floor) "Floor[~a]"]
  ;; Both remainders have to be emulated
  [('fmod) "With[{TMP1 = ~a, TMP2 = ~a}, Mod[Abs[TMP1], Abs[TMP2]] * Sign[TMP1]]"]
  [('remainder) "With[{TMP1 = ~a, TMP2 = ~a}, TMP1 - Round[TMP1 / TMP2] * TMP2]"]
  [('fmax) "Max[~a, ~a]"]
  [('fmin) "Min[~a, ~a]"]
  [('fdim) "Max[0, ~a - ~a]"]
  ;; Sign[0] is 0, so we can't just multiply
  [('copysign) "With[{TMP1 = Abs[~a], TMP2 = Sign[~a]}, TMP1 * If[TMP2 == 0, 1, TMP2]]"]
  ;; Mathematica's Round[] rounds to even integers, like
  ;; nearbyint with the rounding mode set to RNE. So we have to emulate
  ;; the other rounding functions.
  [('trunc) "With[{TMP1 = ~a}, Floor[Abs[TMP1]] * Sign[TMP1]]"]
  [('round) "With[{TMP1 = ~a}, If[Abs[TMP1] - Floor[Abs[TMP1]] < 1/2, Floor[Abs[TMP1]] * Sign[TMP1], Ceiling[Abs[TMP1]] * Sign[TMP1]]]"]
  [('nearbyint) "Round[~a]"]
  ;; Comparisons and logical ops take one format argument,
  ;; which is a pre-concatenated string of inputs
  ;; (with commas separating them!)
  [('<) "Less[~a]"]
  [('>) "Greater[~a]"]
  [('<=) "LessEqual[~a]"]
  [('>=) "GreaterEqual[~a]"]
  [('==) "Equal[~a]"]
  [('!=) "Unequal[~a]"]
  [('and) "And[~a]"]
  [('or) "Or[~a]"]
  [('not) "Not[~a]"]
  ;; Arbitrary decision: simplify here, as the rest of the enclosing expression is just going
  ;; to be boolean logic.
  ;; Some notes:
  ;;  - Abs[Simplify[x]] is not always the same as Simplify[Abs[x]]. In cases where division
  ;;    by zero produces ComplexInfinity, which we probably ??? want to detect as isinf, the Abs[]
  ;;    can be pushed through i.e. into the bottom of the division, resulting in the ComplexInfinity
  ;;    escaping outside the Abs[].
  ;;  - Indeterminate == Anything will be kept symbolic, so we have to use structural equality with ===
  ;;    to check if something is Indeterminate. This might result in false negatives, where a quantity
  ;;    that should have simplified to Indeterminate but hasn't yet, is not recognized as Indeterminate.
  ;;  - Some comparisons may remain symbolic, which would then be carried through boolean logic and
  ;;    produce a final result like Not[foo == 0] where foo is complicated.
  ;;  - There are almost certainly bugs and inconsistencies in the behavior of these functions; it would
  ;;    be highly unwise to build anything important that depends on them, though they will probably
  ;;    work in most reasonable cases.
  [('isinf) "With[{TMP1 = Simplify[~a]}, And[Not[TMP1 === Indeterminate], Abs[TMP1] == Infinity]]"]
  [('isnan) "(Simplify[~a] === Indeterminate)"]
  [('isfinite) "With[{TMP1 = Simplify[~a]}, Not[Or[Abs[TMP1] == Infinity, TMP1 === Indeterminate]]]"]
  [('isnormal) "With[{TMP1 = Simplify[~a]}, Not[Or[Abs[TMP1] == Infinity, TMP1 === Indeterminate, TMP1 == 0]]]"]
  ;; This will not distinguish negative zero, which mostly makes sense for actual reals
  [('signbit) "(Sign[Simplify[~a]] == -1)"])

(define (application->wls operator args ctx)
  (match (cons operator args)
    [(list (or '< '> '<= '>= '== '!= 'and 'or) args ...)
     (format (operator->wls operator)
        (string-join
          (for/list ([a args])
            (format "~a" a))
          ", "))]
    [(list '- a)
     (format "(-~a)" a)]
    [(list (? operator? op) args ...)
     (apply format (operator->wls op) args)]))

(define (declaration->wls var val)
  (format "~a = ~a" var val))

(define (normal-let->wls name vars vals body)
  (format "~a[{~a}, ~a]" 
    name
    (string-join
      (for/list ([var vars] [val vals])
          (declaration->wls var val))
      ", ")
    body))
    
(define (nested-let->wls name vars vals body)
  (cond
    [(> (length vars) 0) 
      (format "~a[{~a}, ~a]" name (declaration->wls (first vars) (first vals)) 
                             (nested-let->wls name (drop vars 1) (drop vals 1) body))]
    [else body]))

(define (let->wls vars vals body indent nested)
  (if nested
    (format (nested-let->wls "With" vars vals body))
    (format (normal-let->wls "With" vars vals body))))                         

(define (if->wls cond ift iff tmp indent)
  (format "If[~a, ~a, ~a]" cond ift iff))

(define (while->wls vars inits cond updates updatevars body loop indent nested)
  (if nested
    (nested-let->wls "Block" vars inits   ; Block[{~a}, While[~a, ~a]; ~a]
      (format "While[~a, ~a]; ~a"
        cond
        (string-join
          (for/list ([var vars] [val updates])
            (declaration->wls var val))
          "; ")
        body))
    (normal-let->wls "Block" vars inits   ; Block[{~a}, While[~a, With[{~a}, ~a]]; ~a]
      (format "While[~a, ~a]; ~a"
        cond
        (normal-let->wls "With" updatevars updates
          (string-join
            (for/list ([var vars] [val updatevars])
              (declaration->wls var val))
            "; "))
        body))))

(define (function->wls name args body ctx names)
  (define wls-prec (prec->wls (ctx-lookup-prop ctx ':precision 'binary64)))
  (define arg-strings
    (for/list ([var args])
      (format "~a_" (if (list? var) (car var) var))))
  (format "~a[~a] := Block[{$MinPrecision=~a, $MaxPrecision=~a, $MaxExtraPrecision=0}, ~a]\n"
          name
          (string-join arg-strings ", ")
          wls-prec
          wls-prec
          body))

(define wls-language (functional "wls" application->wls constant->wls declaration->wls let->wls if->wls while->wls function->wls))

;;; Exports

(define (core->wls prog name) 
  (parameterize ([*func-lang*  wls-language] [*gensym-divider* #\$] 
                 [*gensym-fix-name* fix-name] [*reserved-names* wls-reserved])
    (core->functional prog name)))

(define-compiler '("wl" "wls") (const "") core->wls (const "") wls-supported)