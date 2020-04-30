#lang racket

(require math/bigfloat)
(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide c-header core->c c-supported)

(define c-header (const "#include <fenv.h>\n#include <math.h>\n#include <stdint.h>\n#define TRUE 1\n#define FALSE 0\n\n"))
(define c-supported (supported-list
  (invert-op-list '(digits))
  fpcore-consts
  '(binary32 binary64 binary80 integer)
  (invert-round-modes-list '(nearestAway))))

(define/match (type->c-suffix type)
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

(define (operator->c props op args)
  (define type (type->c (dict-ref props ':precision 'binary64)))
  (match op
    ['isinf     (format "isinf(~a)" args)]
    ['isnan     (format "isnan(~a)" args)]
    ['isfinite  (format "isfinite(~a)" args)]
    ['isnormal  (format "isnormal(~a)" args)]
    ['signbit   (format "signbit(~a)" args)]
    [_          (format "((~a) ~a~a(~a))" type op (type->c-suffix type) (string-join args ", "))]))

(define (constant->c props expr)
  (define prec (dict-ref props ':precision 'binary64))
  (define type (type->c prec))
  (match expr
    [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'TRUE 'FALSE 'INFINITY 'NAN)
     (format "((~a) ~a)" type expr)]
    [(? hex?) (format "~a" expr)]
    [(? number?)
      (match prec
        ['integer   (format "~a" (inexact->exact expr))]
        ['binary80  (parameterize ([bf-precision 64]) 
                        (format "~a~a" (bigfloat->string (bf expr)) (type->c-suffix type)))]
        [_          (format "~a~a" (real->double-flonum expr) (type->c-suffix type))])]
    [(? symbol?) (format "((~a) M_~a)" type expr)]))

(define (declaration->c props var [val #f])
  (define type (type->c (dict-ref props ':precision 'binary64)))
  (if val
      (format "~a ~a = ~a;" type var val)
      (format "~a ~a;" type var)))

(define (assignment->c var val)
  (format "~a = ~a;" var val))

(define (round->c val props) 
  (define type (type->c (dict-ref props ':precision 'binary64)))
  (format "((~a) ~a)" type val))

(define (round-mode->c mode indent)
  (format "~afesetround(~a);\n"
    indent
    (match mode
      ['nearestEven   "FE_TONEAREST"]
      ['toPositive    "FE_UPWARD"]
      ['toNegative    "FE_DOWNWARD"]
      ['toZero        "FE_TOWARDZERO"]
      [_              (error 'round-mode->c (format "Unsupported rounding mode ~a" mode))])))

(define (function->c name args arg-props body return ctx vars)
  (define type (type->c (ctx-lookup-prop ctx ':precision 'binary64)))
  (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name
          (string-join
           (map (λ (arg) (format "~a ~a" type arg)) args)
           ", ")
          body return))

(define c-language (language (const "c") operator->c constant->c declaration->c assignment->c
                             round->c round-mode->c function->c))

;;; Exports

(define (core->c  prog name) (parameterize ([*lang*  c-language]) (convert-core prog name)))
(define-compiler '("c") c-header core->c (const "") c-supported)