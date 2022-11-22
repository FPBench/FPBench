#lang racket

(require generic-flonum)
(require "imperative.rkt")

(provide python-header core->python python-supported)

(define python-header
  (const
    (string-append
      "import math;\n\n"
      "def fmax(x, y):\n\tif math.isnan(x):\n\t\treturn y\n\t"
      "elif math.isnan(y):\n\t\treturn x\n\telse:\n\t\treturn max(x, y)\n\n"
      "def fmin(x, y):\n\tif math.isnan(x):\n\t\treturn y\n\t"
      "elif math.isnan(y):\n\t\treturn x\n\telse:\n\t\treturn min(x, y)\n\n")))

(define python-supported
  (supported-list
    (invert-op-proc
      (curry set-member?
            '(cbrt exp2 fdim fma isnormal nearbyint round signbit
              array dim size ref for for* tensor tensor*)))
    (curry set-member? '(TRUE FALSE INFINITY NAN PI E))
    (curry equal? 'binary64)
    (curry equal? 'nearestEven)
    #f))

(define python-reserved   ; Language-specific reserved names (avoid name collision)
  '(False None True and as assert break class continue def del
    elif else except finally for from global if import in is
    lambda nonlocal not or pass raise return try while with yield))

(define (operator->python op args ctx)
  (match (cons op args)
   [(list 'fmax a b) (format "fmax(~a, ~a)" a b)]
   [(list 'fmin a b) (format "fmin(~a, ~a)" a b)]
   [(list 'tgamma a) (format "math.gamma(~a)" a)]
   [_ (format "math.~a(~a)" op (string-join args ", "))]))

(define (constant->python x ctx)
  (match x
   ['TRUE "True"]
   ['FALSE "False"]
   ['INFINITY "math.inf"]
   ['NAN "math.nan"]
   ['PI "math.pi"]
   ['E "math.e"]
   [(? hex?) (~a (real->double-flonum (hex->racket x)))]
   [(? number?) (~a (real->double-flonum x))]
   [(? symbol?) (~a x)]))

(define declaration->python
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a" var (match prec ['binary64 "0"] ['boolean "True"]))]
   [(var val ctx)
    (format "~a = ~a" var val)]))

(define (assignment->python var val ctx)
  (format "~a = ~a" var val))

(define (program->python name args arg-ctxs body ret ctx used-vars)
  (format "def ~a(~a):\n~a\treturn ~a\n" name
          (string-join args ", ") body ret))

(define core->python
  (make-imperative-compiler "python"
    #:operator operator->python
    #:constant constant->python
    #:declare declaration->python
    #:assign assignment->python
    #:program program->python
    #:flags '(colon-instead-of-brace
              no-parens-around-condition
              use-elif
              boolean-ops-use-name)
    #:reserved python-reserved))

(define-compiler '("py") python-header core->python (const "") python-supported)
