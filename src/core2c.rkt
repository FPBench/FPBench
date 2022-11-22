#lang racket

(require generic-flonum)
(require "imperative.rkt")

(provide c-header core->c c-supported
         c-reserved type->c params->c) ; required by core2h.rkt

(define c-header (const "#include <fenv.h>\n#include <math.h>\n#include <stdint.h>\n#define TRUE 1\n#define FALSE 0\n\n"))
(define c-supported 
  (supported-list
    (invert-op-proc (curry set-member? '(array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64 binary80 integer))
    (invert-rnd-mode-proc (curry equal? 'nearestAway))
    #f))

(define c-reserved  ; Language-specific reserved names (avoid name collisions)
  '(auto break case char const continue default
    do double else enum extern float for goto if
    inline int long register restrict return
    short signed sizeof static struct switch
    typedef union unsigned void volatile while))

(define/match (c-type->suffix type)
  [("int64_t") ""]
  [("double") ""]
  [("float") "f"]
  [("long double") "l"])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"]
  [('integer) "int64_t"])

(define (operator->c op args ctx)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (match op
   [(or 'isinf 'isnan 'isfinite 'isnormal 'signbit)
    (format "~a~a" op args)]
   [_
    (format "~a~a(~a)" op (c-type->suffix type) (string-join args ", "))]))
  
(define (binary80->string x)
  (parameterize ([gfl-exponent 15] [gfl-bits 80])
    (let ([s (gfl->string (gfl x))])
      (if (string-contains? s ".") s (string-append s ".0")))))

(define (constant->c x ctx)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (match x
   [(or 'TRUE 'FALSE (? hex?)) (~a x)]
   [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN)
    (format "((~a) ~a)" type x)]
   [(? number?)
    (match type
     ["int64_t" (~a (inexact->exact x))]
     ["long double" (format "~a~a" (binary80->string x) (c-type->suffix type))]
     [_ (format "~a~a" (real->double-flonum x) (c-type->suffix type))])]
   [(? symbol?) (format "((~a) M_~a)" type x)]))
  
(define (round->c x ctx)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (format "((~a) ~a)" type x))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary80) 4]
    [('binary64) 3]
    [('binary32) 2]
    [('integer)  1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->c op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->c arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (round-mode->c mode ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (format "~afesetround(~a);\n" indent
    (match mode
     ['nearestEven  "FE_TONEAREST"]
     ['toPositive   "FE_UPWARD"]
     ['toNegative   "FE_DOWNWARD"]
     ['toZero       "FE_TOWARDZERO"]
     [_             (error 'round-mode->c (format "Unsupported rounding mode ~a" mode))])))

(define (params->c args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->c (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg)))
    ", "))

(define (program->c name args arg-ctxs body ret ctx used-vars)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (define rnd-mode (ctx-lookup-prop ctx ':round))
  (match rnd-mode
   ['nearestEven
    (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
            type name (params->c args arg-ctxs)
            body (trim-infix-parens ret))]
   [_
    (define-values (_ ret-var) (ctx-random-name ctx))
    (format "~a ~a(~a) {\n~a~a~a ~a = ~a;\n~a\treturn ~a;\n}\n"
            type name (params->c args arg-ctxs)
            (round-mode->c rnd-mode ctx) body
            type ret-var (trim-infix-parens ret) 
            (round-mode->c 'nearestEven ctx) ret-var)]))


(define core->c
  (make-imperative-compiler "c"
    #:operator operator->c
    #:constant constant->c
    #:type type->c
    #:round round->c
    #:implicit-round implicit-round->c
    #:round-mode round-mode->c
    #:program program->c
    #:reserved c-reserved))

(define-compiler '("c") c-header core->c (const "") c-supported)
