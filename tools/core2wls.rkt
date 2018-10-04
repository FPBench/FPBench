#lang racket

(require "common.rkt" "fpcore.rkt")
(provide compile-program)

(define bad-chars (regexp "^[0-9]+|[^a-z0-9]+"))

(define var-counter (box 1))
(define (fix-name name names)
  (if (hash-has-key? names name)
      (hash-ref names name)
      (let ([shortened (regexp-replace bad-chars name "")])
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

(define (number->math x)
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

(define (constant->math c)
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
    [_ (error 'constant->math "Unsupported constant ~a" c)]))

(define (operator->math op)
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
    ;; Mathematica's Mod[] function does not have the same
    ;; behavior of either of these operations (as far as I
    ;; can tell).
    ;['fmod ""]
    ;['remainder "(fp.rem ~a ~a)"]
    ['fmax "Max[~a, ~a]"]
    ['fmin "Min[~a, ~a]"]
    ['fdim "Max[0, ~a - ~a]"]
    ['copysign "(~a * Sign[~a])"]
    ;; Mathematica's Round[] rounds to even integers, like
    ;; nearbyint with the rounding mode set to RNE.
    ;['trunc ""]
    ;['round ""]
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
    ;; These are a little complicated...
    ;['isfinite ""]
    ;['isinf ""]
    ;['isnan "(~a === Indeterminate)"]
    ;['isnormal ""]
    ;['signbit "(Sign[~a] == -1)"]
    [_ (error 'operator->math "Unsupported operator ~a" op)]))

(define (application->math operator args)
  (match (cons operator args)
    [(list (or '< '> '<= '>= '== '!= 'and 'or) args ...)
     (format (operator->math operator)
             (string-join
              (for/list ([a args]) (format "~a" a))
              ", "))]
    [(list '- a)
     (format "(-~a)" a)]
    [(list (? operator? op) args ...)
     (apply format (operator->math op) args)]
    [_ (error 'application->math "Unsupported application ~a ~a" operator args)]))

(define (expr->math expr names)
  (match expr
    [`(if ,condition ,true-branch ,false-branch)
     (format "If[~a, ~a, ~a]"
             (expr->math condition names)
             (expr->math true-branch names)
             (expr->math false-branch names))]
    [`(let ([,vars ,vals] ...) ,body)
     (format "With[{~a}, ~a]"
             (string-join
              (for/list ([var vars] [val vals])
                (format "~a = ~a"
                        (fix-name (symbol->string var) names) (expr->math val names)))
              ", ")
             (expr->math body names))]
    [`(while ,condition ([,vars ,inits ,updates] ...) ,body)
     (let ([varnames (for/list ([var vars]) (fix-name (symbol->string var) names))]
           [loopvarnames (for/list ([var vars]) (loopvar-name))])
       (format "Block[{~a}, While[~a, With[{~a}, ~a]]; ~a]"
               (string-join
                (for/list ([var varnames] [init-expr inits])
                  (format "~a = ~a" var (expr->math init-expr names)))
                ", ")
               (expr->math condition names)
               (string-join
                (for/list ([loopvar loopvarnames] [update-expr updates))
                  (format "~a = ~a" loopvar (expr->math update-expr names)))
                ", ")
               (string-join
                (for/list ([var varnames] [loopvar loopvarnames])
                  (format "~a = ~a" var loopvar))
                "; ")
               (expr->math body names)))]

    ;; Ignore all casts and precision contexts
    [`(cast ,body)
     (expr->math body names)]
    [(list '! props ... body)
     (expr->math body names)]

    [(list (? operator? operator) args ...)
     (application->math operator
                       (for/list ([arg args])
                         (expr->math arg names)))]
    [(? constant?)
     (constant->math expr)]
    [(? symbol?)
     (fix-name (symbol->string expr) names)]
    [(? number?)
     (number->math expr)]
    [(list 'digits (? number? m) (? number? e) (? number? b))
     (format "(~a * ~a ^ ~a)"
             (number->math m)
             (number->math e)
             (number->math b))]
    [_ (error 'expr->math "Unsupported expr ~a" expr)]))

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
          (expr->math body names)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
