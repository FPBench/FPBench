#lang racket

(require "imperative.rkt")
(provide core->julia julia-supported type->julia)

(define julia-supported 
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(erf erfc fdim isnormal lgamma nearbyint remainder tgamma
          array dim size ref for for* tensor tensor*)))
    (invert-const-proc
      (curry set-member?
        '(MAXFLOAT)))
    (curry set-member? '(binary16 binary32 binary64))
    (curry equal? 'nearestEven)
    #f))

(define julia-reserved      ; Language-specific reserved names (avoid name collisions)
  '(abstract baremodule begin break catch const continue do else elseif
    end export false finally for function global if import let local
    macro module mutable primitive quote return struct true try type
    using while))

(define/match (type->julia prec)
  [('binary64) "Float64"]
  [('binary32) "Float32"]
  [('binary16) "Float16"]
  [('boolean) "Bool"])

(define (operator->julia op args ctx)
  (match (cons op args)
   [(list 'atan2 a b) (format "atan(~a, ~a)" a b)]
   [(list 'fabs a) (format "abs(~a)" a)]
   [(list 'fmax a b) (format "((~a != ~a) ? ~a : ((~a != ~a) ? ~a : max(~a, ~a)))"
                             a a b b b a a b)]
   [(list 'fmin a b) (format "((~a != ~a) ? ~a : ((~a != ~a) ? ~a : min(~a, ~a)))"
                             a a b b b a a b)]
   [(list 'fmod a b) (format "rem(~a, ~a)" a b)]
   [(list 'pow a b) (format "(~a ^ ~a)" a b)]
   [_ (format "~a(~a)" op (string-join args ", "))]))

(define (constant->julia x ctx)
  (define fmt
    (match (ctx-lookup-prop ctx ':precision)
     ['binary32 "Float32(~a)"]
     ['binary16 "Float16(~a)"]
     [_ "~a"]))
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   ['E (format fmt "exp(1)")]
   ['LOG2E (format fmt "log2(exp(1))")]
   ['LOG10E (format fmt "log10(exp(1))")]
   ['LN2 (format fmt "log(2)")]
   ['LN10 (format fmt "log(10)")]
   ['PI (format fmt "pi")]
   ['PI_2 (format fmt "(pi / 2)")]
   ['PI_4 (format fmt "(pi / 4)")]
   ['M_1_PI (format fmt "(1 / pi)")]
   ['M_2_PI (format fmt "(2 / pi)")]
   ['M_2_SQRTPI (format fmt "(2 / sqrt(pi))")]
   ['SQRT2 (format fmt "sqrt(2)")]
   ['SQRT1_2 (format fmt "(1 / sqrt(2))")]
   ['INFINITY (format fmt "Inf")]
   ['NAN (format fmt "NaN")]
   [(? hex?) (format fmt (real->double-flonum (hex->racket x)))]
   [(? number?) (format fmt (real->double-flonum x))]
   [(? symbol?) (format fmt x)]))

(define declaration->julia
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (constant->julia (match prec ['boolean 'TRUE] [_ 0]) ctx))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->julia var val ctx)
  (format "~a = ~a" var val))

(define (round->julia x ctx)
  (define type (type->julia (ctx-lookup-prop ctx ':precision)))
  (format "~a(~a)" type (trim-infix-parens x)))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 3]
    [('binary32) 2]
    [('binary16) 1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->julia op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->julia arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (program->julia name args arg-ctxs body ret ctx used-vars)
  (format "function ~a(~a)\n~a\treturn ~a\nend\n"
          name (string-join args ", ") body (trim-infix-parens ret)))

(define core->julia
  (make-imperative-compiler "julia"
    #:operator operator->julia
    #:constant constant->julia
    #:declare declaration->julia
    #:round round->julia
    #:implicit-round implicit-round->julia
    #:program program->julia
    #:flags '(end-block-with-end
              use-elseif
              round-after-operation)    ; Julia is loose about types, force rounding
    #:reserved julia-reserved))

(define-compiler '("jl") (const "") core->julia (const "") julia-supported)
