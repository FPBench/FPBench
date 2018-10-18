#lang racket

(require "common.rkt" "fpcore.rkt")
(provide compile-program number->wls)

(define bad-chars (regexp "^[0-9]+|[^a-z0-9]+"))

(define var-counter (box 1))
(define (fix-name name names)
  (if (hash-has-key? names name)
      (hash-ref names name)
      (let ([shortened (regexp-replace* bad-chars name "")])
        (if (string=? name shortened)
            (begin
              (hash-set! names name name)
              name)
            (let* ([counter (unbox var-counter)]
                   [uniquified (string-append shortened "VAR" (number->string counter))])
              (set-box! var-counter (+ counter 1))
              (hash-set! names name uniquified)
              uniquified)))))
(define (loopvar-name)
  (let ([counter (unbox var-counter)])
    (set-box! var-counter (+ counter 1))
    (string-append "LOOPVAR" (number->string counter))))

(define (number->wls x)
  (match x
    [(or +inf.0 +inf.f) "Infinity"]
    [(or -inf.0 -inf.f) "(-Infinity)"]
    [(or +nan.0 +nan.f) "Indeterminate"]
    [_ (let* ([q (if (single-flonum? x)
                     ;; Workaround for misbehavior of inexact->exact with single-flonum inputs
                     (inexact->exact (real->double-flonum x))
                     (inexact->exact x))]
              [n (numerator q)]
              [d (denominator q)])
         (if (= d 1)
             (format "~a" n)
             (format "(~a/~a)" n d)))]))

(define (constant->wls c)
  (match c
    ['E "E"]
    ['LOG2E "Log[2, E]"]
    ['LOG10E "Log[10, E]"]
    ['LN2 "Log[2]"]
    ['LN10 "Log[10]"]
    ['PI "Pi"]
    ['PI_2 "(Pi / 2)"]
    ['PI_4 "(Pi / 4)"]
    ['1_PI "(1 / Pi)"]
    ['2_PI "(2 / Pi)"]
    ['2_SQRTPI "(2 / Sqrt[Pi])"]
    ['SQRT2 "Sqrt[2]"]
    ['SQRT1_2 "Sqrt[1 / 2]"]
    ['TRUE "True"]
    ['FALSE "False"]
    ['INFINITY "Infinity"]
    ['NAN "Indeterminate"]
    [_ (error 'constant->wls "Unsupported constant ~a" c)]))

(define (operator->wls op)
  (match op
    ['+ "(~a + ~a)"]
    ['- "(~a - ~a)"]
    ['* "(~a * ~a)"]
    ['/ "(~a / ~a)"]
    ['fabs "Abs[~a]"]
    ['fma "(~a * ~a + ~a)"]
    ['exp "Exp[~a]"]
    ['exp2 "Power[2, ~a]"]
    ['expm1 "(Exp[~a] - 1)"]
    ['log "Log[~a]"]
    ['log10 "Log[10, ~a]"]
    ['log2 "Log[2, ~a]"]
    ['log1p "Log[1 + ~a]"]
    ['pow "Power[~a, ~a]"]
    ['sqrt "Sqrt[~a]"]
    ['cbrt "Power[~a, 1/3]"]
    ['hypot "Sqrt[~a ^ 2 + ~a ^ 2]"]
    ['sin "Sin[~a]"]
    ['cos "Cos[~a]"]
    ['tan "Tan[~a]"]
    ['asin "ArcSin[~a]"]
    ['acos "ArcCos[~a]"]
    ['atan "ArcTan[~a]"]
    ['atan2 "ArcTan[~a / ~a]"]
    ['sinh "Sinh[~a]"]
    ['cosh "Cosh[~a]"]
    ['tanh "Tanh[~a]"]
    ['asinh "ArcSinh[~a]"]
    ['acosh "ArcCosh[~a]"]
    ['atanh "ArcTanh[~a]"]
    ['erf "Erf[~a]"]
    ['erfc "Erfc[~a]"]
    ['tgamma "Gamma[~a]"]
    ['lgamma "LogGamma[~a]"]
    ['ceil "Ceiling[~a]"]
    ['floor "Floor[~a]"]
    ;; Both remainders have to be emulated
    ['fmod "With[{TMP1 = ~a, TMP2 = ~a}, Mod[Abs[TMP1], Abs[TMP2]] * Sign[TMP1]]"]
    ['remainder "With[{TMP1 = ~a, TMP2 = ~a}, TMP1 - Round[TMP1 / TMP2] * TMP2]"]
    ['fmax "Max[~a, ~a]"]
    ['fmin "Min[~a, ~a]"]
    ['fdim "Max[0, ~a - ~a]"]
    ;; Sign[0] is 0, so we can't just multiply
    ['copysign "With[{TMP1 = Abs[~a], TMP2 = Sign[~a]}, TMP1 * If[TMP2 == 0, 1, TMP2]]"]
    ;; Mathematica's Round[] rounds to even integers, like
    ;; nearbyint with the rounding mode set to RNE. So we have to emulate
    ;; the other rounding functions.
    ['trunc "With[{TMP1 = ~a}, Floor[Abs[TMP1]] * Sign[TMP1]]"]
    ['round "With[{TMP1 = ~a}, If[Abs[TMP1] - Floor[Abs[TMP1]] < 1/2, Floor[Abs[TMP1]] * Sign[TMP1], Ceiling[Abs[TMP1]] * Sign[TMP1]]]"]
    ['nearbyint "Round[~a]"]
    ;; Comparisons and logical ops take one format argument,
    ;; which is a pre-concatenated string of inputs
    ;; (with commas separating them!)
    ['< "Less[~a]"]
    ['> "Greater[~a]"]
    ['<= "LessEqual[~a]"]
    ['>= "GreaterEqual[~a]"]
    ['== "Equal[~a]"]
    ['!= "Unequal[~a]"]
    ['and "And[~a]"]
    ['or "Or[~a]"]
    ['not "Not[~a]"]
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
    ['isinf "With[{TMP1 = Simplify[~a]}, And[Not[TMP1 === Indeterminate], Abs[TMP1] == Infinity]]"]
    ['isnan "(Simplify[~a] === Indeterminate)"]
    ['isfinite "With[{TMP1 = Simplify[~a]}, Not[Or[Abs[TMP1] == Infinity, TMP1 === Indeterminate]]]"]
    ['isnormal "With[{TMP1 = Simplify[~a]}, Not[Or[Abs[TMP1] == Infinity, TMP1 === Indeterminate, TMP1 == 0]]]"]
    ;; This will not distinguish negative zero, which mostly makes sense for actual reals
    ['signbit "(Sign[Simplify[~a]] == -1)"]
    [_ (error 'operator->wls "Unsupported operator ~a" op)]))

(define (application->wls operator args)
  (match (cons operator args)
    [(list (or '< '> '<= '>= '== '!= 'and 'or) args ...)
     (format (operator->wls operator)
             (string-join
              (for/list ([a args]) (format "~a" a))
              ", "))]
    [(list '- a)
     (format "(-~a)" a)]
    [(list (? operator? op) args ...)
     (apply format (operator->wls op) args)]
    [_ (error 'application->wls "Unsupported application ~a ~a" operator args)]))

(define (expr->wls expr names)
  (match expr
    [`(if ,condition ,true-branch ,false-branch)
     (format "If[~a, ~a, ~a]"
             (expr->wls condition names)
             (expr->wls true-branch names)
             (expr->wls false-branch names))]
    [`(let ([,vars ,vals] ...) ,body)
     (format "With[{~a}, ~a]"
             (string-join
              (for/list ([var vars] [val vals])
                (format "~a = ~a"
                        (fix-name (symbol->string var) names) (expr->wls val names)))
              ", ")
             (expr->wls body names))]
    [`(while ,condition ([,vars ,inits ,updates] ...) ,body)
     (let ([varnames (for/list ([var vars]) (fix-name (symbol->string var) names))]
           [loopvarnames (for/list ([var vars]) (loopvar-name))])
       (format "Block[{~a}, While[~a, With[{~a}, ~a]]; ~a]"
               (string-join
                (for/list ([var varnames] [init-expr inits])
                  (format "~a = ~a" var (expr->wls init-expr names)))
                ", ")
               (expr->wls condition names)
               (string-join
                (for/list ([loopvar loopvarnames] [update-expr updates])
                  (format "~a = ~a" loopvar (expr->wls update-expr names)))
                ", ")
               (string-join
                (for/list ([var varnames] [loopvar loopvarnames])
                  (format "~a = ~a" var loopvar))
                "; ")
               (expr->wls body names)))]

    ;; Ignore all casts and precision contexts
    [`(cast ,body)
     (expr->wls body names)]
    [(list '! props ... body)
     (expr->wls body names)]

    [(list (? operator? operator) args ...)
     (application->wls operator
                       (for/list ([arg args])
                         (expr->wls arg names)))]
    [(? constant?)
     (constant->wls expr)]
    [(? symbol?)
     (fix-name (symbol->string expr) names)]
    [(? number?)
     (number->wls expr)]
    [(list 'digits (? number? m) (? number? e) (? number? b))
     (format "(~a * ~a ^ ~a)"
             (number->wls m)
             (number->wls e)
             (number->wls b))]
    [_ (error 'expr->wls "Unsupported expr ~a" expr)]))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)

  (define names (make-hash))

  (define progname (fix-name name names))

  (define argnames
    (for/list ([var args])
      (format "~a_" (fix-name (symbol->string (if (list? var) (car var) var)) names))))

  (format "~a[~a] :=\n~a"
          progname
          (string-join argnames ", ")
          (expr->wls body names)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
