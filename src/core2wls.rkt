#lang racket

(require math/bigfloat)
(require "lisp.rkt")
(provide core->wls wls-supported number->wls prec->wls)

; Note: MachinePrecision is kind of broken since Mathematica will
; just arbitrarily ignore it at times

(define wls-supported
  (supported-list
    (invert-op-proc (curry set-member? '(array size ref dim for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary64 real integer))
    (curry set-member? '(nearestEven))
    #f))

(define wls-reserved '(E PI))  ; Language-specific reserved names (avoid name collisions)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9$]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (prec->wls prec)
  (match prec 
    ['binary64 "$MachinePrecision"]
    ['real     "Infinity"]
    ['integer  "Infinity"]))

(define (number->wls x)
  (match x
    [(or +inf.0 +inf.f) "Infinity"]
    [(or -inf.0 -inf.f) "(-Infinity)"]
    [(or +nan.0 +nan.f) "Indeterminate"]
    [_ (let* ([q ;; Workaround for misbehavior of inexact->exact with single-flonum inputs
                 (parameterize ([bf-precision 24]) (inexact->exact (bigfloat->flonum (bf x))))]
              [n (numerator q)]
              [d (denominator q)])
        (if (= d 1)
            (format "~a" n)
            (format "(~a/~a)" n d)))]))

(define (constant->wls expr ctx)
  (match expr
    ['E           "E"]
    ['LOG2E       "Log[2, E]"]
    ['LOG10E      "Log[10, E]"]
    ['LN2         "Log[2]"]
    ['LN10        "Log[10]"]
    ['PI          "Pi"]
    ['PI_2        "(Pi / 2)"]
    ['PI_4        "(Pi / 4)"]
    ['M_1_PI      "(1 / Pi)"]
    ['M_2_PI      "(2 / Pi)"]
    ['M_2_SQRTPI  "(2 / Sqrt[Pi])"]
    ['SQRT2       "Sqrt[2]"]
    ['SQRT1_2     "Sqrt[1 / 2]"]
    ['TRUE        "True"]
    ['FALSE       "False"]
    ['INFINITY    "Infinity"]
    ['NAN         "Indeterminate"]
    [(? hex?)     (~a (hex->racket expr))]
    [(? number?)  (~a expr)]
    [(? symbol?)  (~a expr)]))

(define/match (operator-format op)
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

(define (operator->wls op args ctx)
  (match (cons op args)
    [(list (or '< '> '<= '>= '== '!= 'and 'or) args ...)
     (format (operator-format op) (string-join (map ~a args) ", "))]
    [(list '- a)
     (format "(-~a)" a)]
    [(list (? operator? op) args ...)
     (define wls-prec (prec->wls (ctx-lookup-prop ctx ':precision)))
     (if (equal? wls-prec "Infinity")
         (apply format (operator-format op) args)
         (format "N[~a, ~a]" (apply format (operator-format op) args) wls-prec))]))

(define (program->wls name args arg-ctxs body ctx)
  (define args* (map (curry format "~a_") args))
  (format "~a[~a] := ~a\n" name (string-join args* ", ") body))

; Override visitor behavior
(define-expr-visitor lisp-visitor wls-visitor
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx)
    (define-values (ctx* vars* vals*)                             ; initialization
      (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                #:result (values ctx* (reverse vars*) (reverse vals*)))
                ([var (in-list vars)] [val (in-list inits)])
        (define val-ctx (match while_ ['while ctx] ['while* ctx*]))
        (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
        (define prec (ctx-lookup-prop val*-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (cond* cond*-ctx) (visit/ctx vtor cond ctx*))  ; condition
    (define-values (vars** updates*)                              ; updates
      (for/fold ([ctx** ctx*] [vars* '()]  [vals* '()]
                #:result (values (reverse vars*) (reverse vals*)))
                ([var (in-list vars)] [val (in-list updates)])
        (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
        (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
        (define prec (ctx-lookup-prop val*-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx** var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (values
      (match while_
       ['while
        (format "Block[{~a}, While[~a, Block[{~a}, ~a]]; ~a]"
                (string-join (map (curry format "~a = ~a") vars* vals*) ", ")
                cond* (string-join (map (curry format "~a = ~a") vars** updates*) ", ")
                (string-join (map (curry format "~a = ~a") vars* vars**) "; ") body*)]
       [while*
        (let loop ([vars vars*] [vals* vals*])
          (if (= (length vars) 1)
              (format "Block[{~a = ~a}, While[~a, ~a]; ~a]"
                      (car vars) (car vals*) cond*
                      (let loop2 ([vars vars**] [updates* updates*])
                        (if (null? vars)
                            (string-join (map (curry format "~a = ~a") vars* vars**) "; ")
                            (format "Block[{~a = ~a}, ~a]" (car vars) (car updates*)
                                    (loop2 (cdr vars) (cdr updates*)))))
                      body*)
              (format "Block[{~a = ~a}, ~a]" (car vars) (car vals*)
                      (loop (cdr vars) (cdr vals*)))))])
      body-ctx)])

(define core->wls*
  (make-lisp-compiler "lisp"
    #:operator operator->wls
    #:constant constant->wls
    #:if-format "If[~a, ~a, ~a]"
    #:let-format "Block[{~a}, ~a]"
    #:let-bind-format (cons "~a = ~a" ", ")
    #:program program->wls
    #:reserved wls-reserved
    #:visitor wls-visitor
    #:fix-name fix-name))

(define (core->wls prog name)
  (parameterize ([*gensym-divider* #\$])
    (core->wls* prog name)))

(define-compiler '("wl" "wls") (const "") core->wls (const "") wls-supported)
