#lang racket

(require "imperative.rkt" "core2c.rkt")
(provide core->h)

;; ignore body
(define-expr-visitor imperative-visitor h-visitor
  [(visit-if vtor cond ift iff #:ctx ctx) (values "" ctx)]
  [(visit-let_ vtor let_ vars vals body #:ctx ctx) (values "" ctx)]
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx) (values "" ctx)]
  [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx ctx) (values "" ctx)]
  [(visit-tensor vtor vars vals body #:ctx ctx) (values "" ctx)]
  [(visit-tensor* vtor vars vals accums inits updates body #:ctx ctx) (values "" ctx)]
  [(visit-! vtor props body #:ctx ctx) (values "" ctx)]
  [(visit-op_ vtor op_ args body #:ctx ctx) (values "" ctx)]
  [(visit-terminal_ vtor x #:ctx ctx) (values "" ctx)])

(define (program->h name args arg-ctxs body ret ctx used-vars)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (format "~a ~a(~a);\n" type name (params->c args arg-ctxs)))

(define core->h
  (make-imperative-compiler "h"
    #:program program->h
    #:visitor h-visitor
    #:reserved c-reserved))

(define-compiler '("h") (const "") core->h (const "") c-supported)
