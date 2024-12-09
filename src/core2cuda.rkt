#lang racket

(require "imperative.rkt")

(provide cuda-header
         cuda-supported
         cuda-reserved
         core->cuda
         type->cuda
         params->cuda      ; required by core2h.rkt
         set-cuda-header!
         set-unknown->cuda!)

(define cuda-header (const "#include <fenv.h>\n#include <math_constants.h>\n#include <cuda_runtime.h>\n#include <cuda_fp16.h>\n#include <math.h>\n#include <stdint.h>\n#define TRUE 1\n#define FALSE 0\n\n"))
(define cuda-supported
  (supported-list
    (invert-op-proc (curry set-member? '(array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64 integer))
    (curry equal? 'nearestEven)
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
  [("float") "f"])

(define/match (type->cuda type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('boolean) "int"]
  [('integer) "int64_t"])

(define (operator->cuda op args ctx)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (match op
   [(or 'isinf 'isnan 'isfinite 'isnormal 'signbit)
    (format "~a~a" op args)]
   [_
    ((unknown->cuda) ctx op args)]))

; (define (operator->cuda op args ctx)
;  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
;  (match op
;    [(and (or 'isinf 'isnan 'isfinite 'isnormal 'signbit) x)
;     (format "__~a~a" x args)]  ; Prefix any of these functions with __
;    [_
;     ((unknown->cuda) ctx op args)]))

(define (constant->cuda x ctx)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
  (match x
    [(or 'TRUE 'FALSE (? hex?)) (~a x)]
    ; Special case the CUDA-specific infinity and nan constants
    ['INFINITY (format "((~a) CUDART_INF)" type)]
    ['NAN (format "((~a) CUDART_NAN)" type)]
    ; Handle the derived PI constants using CUDART_PI
    ['M_1_PI (format "((~a) (1.0 / CUDART_PI))" type)]
    ['M_2_PI (format "((~a) (2.0 / CUDART_PI))" type)]
    ['M_2_SQRTPI (format "((~a) (2.0 / sqrt(CUDART_PI)))" type)]
    [(? number?)
     (match type
       ["int64_t" (~a (inexact->exact x))]
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

(define (params->cuda args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->cuda (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg)))
    ", "))

(define (program->cuda name args arg-ctxs body ret ctx used-vars)
  (define type (type->cuda (ctx-lookup-prop ctx ':precision)))
    (format "__device__ ~a ~a(~a) {\n~a\treturn ~a;\n}\n"
            type name (params->cuda args arg-ctxs)
            body (trim-infix-parens ret)))


(define core->cuda
  (make-imperative-compiler "cuda"
    #:operator operator->cuda
    #:constant constant->cuda
    #:type type->cuda
    #:round round->cuda
    #:implicit-round implicit-round->cuda
    #:program program->cuda
    #:reserved cuda-reserved))

(define-compiler '("cu") cuda-header core->cuda(const "") cuda-supported)
