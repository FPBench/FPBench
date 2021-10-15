#lang racket

(require "imperative.rkt" "core2c.rkt")
(provide core->h)

(define (program->h name args arg-ctxs body ret ctx used-vars)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (format "~a ~a(~a);\n" type name (params->c args arg-ctxs)))

(define core->h
  (make-imperative-compiler "h"
    #:program program->h
    #:reserved c-reserved
    #:flags '(no-body)))

(define-compiler '("h") (const "") core->h (const "") c-supported)
