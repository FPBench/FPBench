#lang racket

(require generic-flonum)
(require "imperative.rkt")

(provide cuda-header
         cuda-supported
         cuda-reserved
         core->cuda
         type->cuda
         params->cuda      ; required by core2h.rkt
         set-cuda-header!
         set-unknown->cuda!)

(define cuda-header (const "#include <fenv.h>\n#include <math.h>\n#include <stdint.h>\n__device__ double e = 2.71828182845904523536;\n\n"))
(define cuda-supported
  (supported-list
    (invert-op-proc (curry set-member? '(array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64 binary80 integer))
    (invert-rnd-mode-proc (curry equal? 'nearestAway))
    #f))

(define (default-unknown->cuda ctx op args)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (format "~a~a(~a)"
          op
          (cuda-type->suffix type)
          (string-join args ", ")))

(define unknown->cuda (make-parameter default-unknown->cuda))

(define (set-cuda-header! gen-proc)
  (set! cuda-header (gen-proc cuda-header)))

(define (set-unknown->cuda! gen-proc)
  (unknown->cuda (gen-proc (unknown->cuda))))

(define cuda-reserved  ; Language-specific reserved names (avoid name collisions)
  '(auto break case char const continue default
    do double else enum extern float for goto if
    inline int long register restrict return
    short signed sizeof static struct switch
    typedef union unsigned void volatile while))

(define/match (cuda-type->suffix type)
  [("int64_t") ""]
  [("double") ""]
  [("float") "f"]
  [("long double") "l"])

(define/match (type->cuda type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"]
  [('integer) "int64_t"])

(define (operator->cuda op args ctx)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (match op
   [(or 'isinf 'isnan 'isfinite 'isnormal 'signbit)
    (format "~a~a" op args)]
   [_
    ((unknown->cuda) ctx op args)]))
  
(define (binary80->string x)
  (parameterize ([gfl-exponent 15] [gfl-bits 80])
    (let ([s (gfl->string (gfl x))])
      (if (string-contains? s ".") s (string-append s ".0")))))

(define (constant->cuda x ctx)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (match x
   [(or 'TRUE 'FALSE (? hex?)) (~a x)]
   [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN)
    (format "((~a) ~a)" type x)]
   [(? number?)
    (match type
     ["int64_t" (~a (inexact->exact x))]
     ["long double" (format "~a~a" (binary80->string x) (cuda-type->suffix type))]
     [_ (format "~a~a" (real->double-flonum x) (cuda-type->suffix type))])]
   [(? symbol?) (format "((~a) M_~a)" type x)]))
  
(define (round->cuda x ctx)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (format "((~a) ~a)" type x))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary80) 4]
    [('binary64) 3]
    [('binary32) 2]
    [('integer)  1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->cuda op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->cuda arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (round-mode->cuda mode ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (format "~afesetround(~a);\n" indent
    (match mode
     ['nearestEven  "FE_TONEAREST"]
     ['toPositive   "FE_UPWARD"]
     ['toNegative   "FE_DOWNWARD"]
     ['toZero       "FE_TOWARDZERO"]
     [_             (error 'round-mode->cuda (format "Unsupported rounding mode ~a" mode))])))

(define (params->cuda args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->cuda (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg)))
    ", "))

(define (program->cuda name args arg-ctxs body ret ctx used-vars)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (define rnd-mode (ctx-lookup-prop ctx ':round))
  (match rnd-mode
   ['nearestEven
    (format "__device__ ~a ~a(~a) {\n~a\treturn ~a;\n}\n"
            type name (params->cuda args arg-ctxs)
            body (trim-infix-parens ret))]
   [_
    (define-values (_ ret-var) (ctx-random-name ctx))
    (format "__device__ ~a ~a(~a) {\n~a~a~a ~a = ~a;\n~a\treturn ~a;\n}\n"
            type name (params->cuda args arg-ctxs)
            (round-mode->cuda rnd-mode ctx) body
            type ret-var (trim-infix-parens ret) 
            (round-mode->cuda 'nearestEven ctx) ret-var)]))


(define core->cuda
  (make-imperative-compiler "cuda"
    #:operator operator->cuda
    #:constant constant->cuda
    #:type type->cuda
    #:round round->cuda
    #:implicit-round implicit-round->cuda
    #:round-mode round-mode->cuda
    #:program program->cuda
    #:reserved cuda-reserved))

(define-compiler '("cu") cuda-header core->cuda(const "") cuda-supported)
