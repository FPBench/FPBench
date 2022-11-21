#lang racket

(require "imperative.rkt")

(provide java-header java-footer java-supported
         core->java java-namespace)

(define java-namespace (make-parameter "FPBench"))
(define java-math-lib (make-parameter "Math"))

(define (java-header namespace)
  (string-append
    (format "import java.lang.*;\n\npublic class ~a {\n\n" namespace)
    "public static double fmin(double x, double y) {\n"
    (format "\treturn (Double.isNaN(x) ? y : (Double.isNaN(y) ? x : (~a.min(x, y))));\n}\n\n"
            (java-math-lib))
    "public static double fmax(double x, double y) {\n"
    (format "\treturn (Double.isNaN(x) ? y : (Double.isNaN(y) ? x : (~a.max(x, y))));\n}\n\n"
            (java-math-lib))))

(define java-footer
  (const "}\n"))

(define java-supported 
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(acosh asinh atanh erf erfc exp2 fdim fma fmod isnormal
          lgamma log2 nearbyint round signbit tgamma trunc
          array dim size ref for for* tensor tensor*))) ; round is not C99 round
    (curry set-member? '(E PI INFINITY NAN TRUE FALSE MAXFLOAT))
    (curry equal? 'binary64)    ; binary32 only supported as storage format
    (curry equal? 'nearestEven)
    #f))

(define java-reserved  ; Language-specific reserved names (avoid name collisions)
  '(abstract assert boolean break byte case catch char class
    const continue default do double else enum extends final
    finally float for goto if implements import instanceof int
    interface long native new package private protected public
    return short static strictfp super switch synchronized this
    throw throws transient try void volatile while))

(define/match (java-type->suffix type)
  [("double") ""]
  [("float") "f"]
  [("boolean") ""])

(define (java-type->class type)
  (string-titlecase type))

(define/match (type->java prec)
  [('binary64) "double"]
  [('binary32) "float"]
  [('boolean) "boolean"])

(define (operator->java op args ctx)
  (define type (type->java (ctx-lookup-prop ctx ':precision)))
  (define class (java-type->class type))
  (define args* (string-join args ", "))
  (match op
   ['isinf (format "~a.isInfinite(~a)" class args*)]
   ['isnan (format "~a.isNaN(~a)" class args*)]
   ['isfinite (format "(!~a.isNaN(~a) && !~a.isInfinite(~a))" class args* class args*)]
   ['copysign (format "~a.copySign(~a)" (java-math-lib) args*)]
   ['fabs (format "~a.abs(~a)" (java-math-lib) args*)]
   ['fmax (format "fmax(~a)" args*)]
   ['fmin (format "fmin(~a)" args*)]
   ['remainder (format "~a.IEEEremainder(~a)" (java-math-lib) args*)]
   [_ (format "~a.~a(~a)" (java-math-lib) op args*)]))

(define (constant->java x ctx)
  (define type (type->java (ctx-lookup-prop ctx ':precision)))
  (define class (java-type->class type))
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   ['E (format "~a.E" (java-math-lib))]
   ['PI (format "~a.PI" (java-math-lib))]
   ['INFINITY (format "~a.POSITIVE_INFINITY" class)]
   ['NAN (format "~a.NaN" class)]
   ['MAXFLOAT (format "~a.MAX_VALUE" class)]
   [(? hex?) (format "~a~a" (real->double-flonum (hex->racket x)) (java-type->suffix type))]
   [(? number?) (format "~a~a" (real->double-flonum x) (java-type->suffix type))]
   [(? symbol?) (~a x)]))

(define (round->java x ctx)
  (define type (type->java (ctx-lookup-prop ctx ':precision)))
  (format "((~a) ~a)" type x))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 2]
    [('binary32) 1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->java op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->java arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (program->java name args arg-ctxs body ret ctx used-vars)
  (define type (type->java (ctx-lookup-prop ctx ':precision)))
  (define args*
    (for/list ([arg args] [ctx arg-ctxs])
      (let ([type (type->java (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg))))
  (format "public static ~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name (string-join args* ", ")
          body (trim-infix-parens ret)))


(define core->java
  (make-imperative-compiler "java"
    #:operator operator->java
    #:constant constant->java
    #:type type->java
    #:round round->java
    #:implicit-round implicit-round->java
    #:program program->java
    #:reserved java-reserved))

(define-compiler '("java") java-header core->java java-footer java-supported)
