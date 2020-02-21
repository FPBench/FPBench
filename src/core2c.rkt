#lang racket

(require "common.rkt" "compilers.rkt" "imperative.rkt" "supported.rkt")
(provide c-header core->c c-supported)

(define c-header (const "#include <math.h>\n#define TRUE 1\n#define FALSE 0\n\n"))
(define c-supported (supported-list
   (invert-op-list '(!))
   (invert-const-list '())
   '(binary32 binary64)))

(define/match (type->c-suffix type)
  [("double") ""]
  [("float") "f"]
  [("long double") "l"])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"])

(define (operator->c ctx operator args)
  (define type (type->c (dict-ref ctx ':precision 'binary64)))
  (format "~a~a(~a)" operator (type->c-suffix type) (string-join args ", ")))

(define (constant->c ctx expr)
  (define type (type->c (dict-ref ctx ':precision 'binary64)))
  (match expr
    [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'TRUE 'FALSE 'INFINITY 'NAN)
     (format "((~a) ~a)" type expr)]
    [(? symbol?) (format "((~a) M_~a)" type expr)]
    [(? number?)
     (format "~a~a" (real->double-flonum expr) (type->c-suffix type))]))

(define (declaration->c ctx var [val #f])
  (define type (type->c (dict-ref ctx ':precision 'binary64)))
  (if val
      (format "~a ~a = ~a;" type var val)
      (format "~a ~a;" type var)))

(define (assignment->c var val)
  (format "~a = ~a;" var val))

(define (round->c val ctx) (format "~a" val)) ; round(val) = val

(define (function->c name args arg-ctx body return ctx vars)
  (define type (type->c ))
  (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name
          (string-join
           (map (λ (arg) (format "~a ~a" type arg)) args)
           ", ")
          body return))

(define c-language (language (const "c") operator->c constant->c declaration->c assignment->c round->c function->c))

;;; Exports

(define (core->c  prog name) (parameterize ([*lang*  c-language]) (convert-core prog name)))
(define-compiler '("c") c-header core->c (const "") c-supported)