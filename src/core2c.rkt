#lang racket

(require "common.rkt" "imperative.rkt" "compilers.rkt")
(provide c-header core->c c-unsupported)

;; C

(define c-name (const "c"))
(define c-header (const "#include <math.h>\n#define TRUE 1\n#define FALSE 0\n\n"))
(define c-unsupported '()) 

(define/match (type->c-suffix type)
  [("double") ""]
  [("float") "f"]
  [("long double") "l"])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"])

(define (operator->c type operator)
  (format "~a~a(~a)" operator (type->c-suffix type) "~a"))

(define (constant->c type expr)
  (match expr
    [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'TRUE 'FALSE 'INFINITY 'NAN)
     (format "((~a) ~a)" type expr)]
    [(? symbol?) (format "((~a) M_~a)" type expr)]
    [(? number?)
     (format "~a~a" (real->double-flonum expr) (type->c-suffix type))]))

(define (declaration->c type var [val #f])
  (if val
      (format "~a ~a = ~a;" type var val)
      (format "~a ~a;" type var)))

(define (assignment->c var val)
  (format "~a = ~a;" var val))

(define (function->c type name args body return)
  (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name
          (string-join
           (map (λ (arg) (format "~a ~a" type arg)) args)
           ", ")
          body return))

(define c-language (language c-name type->c operator->c constant->c declaration->c assignment->c function->c))

;;; Exports

(define (core->c  prog name) (parameterize ([*lang*  c-language]) (convert-core prog name)))

(define-compiler '("c") c-header core->c (const "") c-unsupported)