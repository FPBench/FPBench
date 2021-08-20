#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt")
(provide *imperative-lang* imperative compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *imperative-lang* (make-parameter #f))

(struct imperative
  (name
   operator
   constant
   type
   declare
   assign
   round
   round-mode
   function))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-operator op args ctx)
  ((imperative-operator (*imperative-lang*)) op args (ctx-props ctx)))

(define (compile-constant x ctx)
  ((imperative-constant (*imperative-lang*)) x ctx))

(define (compile-type x)
  ((imperative-type (*imperative-lang*)) x))

(define compile-declaration
  (case-lambda
   [(var ctx) ((imperative-declare (*imperative-lang*)) var #f ctx)]
   [(var val ctx) ((imperative-declare (*imperative-lang*)) var val ctx)]))

(define (compile-assignment var val ctx)
  ((imperative-assign (*imperative-lang*)) var val ctx))

(define (compile-round expr ctx)
  ((imperative-round (*imperative-lang*)) expr ctx))

(define (compile-round-mode expr ctx)
  ((imperative-round-mode (*imperative-lang*)) expr ctx))

(define (compile-function name args arg-ctxs body ret ctx used-vars)
  ((imperative-function (*imperative-lang*)) name args arg-ctxs body ret ctx used-vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-compile-operator op args ctx)
  (format "~a(~a)" op (string-join (map ~a args) ", ")))

(define (default-compile-constant x ctx)
  (~a x))

(define (default-compile-type type)
  "var")

(define default-compile-declaration
  (case-lambda
   [(var ctx) (format "~a ~a;" (compile-type (ctx-lookup-prop ctx ':precision)) var)]
   [(var val ctx) (format "~a ~a = ~a;" (compile-type (ctx-lookup-prop ctx ':precision)) var val)]))

(define (default-compile-assignment var val ctx)
  (format "~a = ~a;" var val))

(define (default-compile-function name args arg-ctxs body ret ctx used-vars)
  (if (non-empty-string? body)
      (format "function ~a(~a) = {\n\t~a\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              body ret)
      (format "function ~a(~a) = {\n\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; language constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-imperative-lang name
                              #:operator [operator default-compile-operator]
                              #:constant [constant default-compile-constant]
                              #:type [type default-compile-type]
                              #:declare [declare default-compile-declaration]
                              #:assign [assign default-compile-assignment]
                              #:round [round (const "")]
                              #:round-mode [round-mode (const "")]
                              #:function [function default-compile-function])
  (imperative name operator constant type declare assign round round-mode function))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-ctx
  (ctx-set-extra
    (ctx-update-props
      (make-compiler-ctx)
      '(:precision binary64 :round nearestEven))
    'indent "\t"))

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;; visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visit-op_/imperative vtor op args #:ctx ctx)
  (define args*
    (for/list ([arg args])
      (define-values (arg* _) (visit/ctx vtor arg ctx))
      arg*))
  (values (format "~a(~a)" op (string-join (map ~a args*) ", "))
          ctx))

(define (visit-symbol/imperative vtor x #:ctx ctx)
  (values (ctx-lookup-name ctx x) ctx))

(define-transform-visitor imperative-visitor
  [visit-op_ visit-op_/imperative]
  [visit-symbol visit-symbol/imperative])

;;;;;;;;;;;;;;;;;;;;;;;;;;; top-level compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((compile lang)  prog name)
  (parameterize ([*gensym-used-names* (mutable-set)] 
                 [*gensym-collisions* 1]
                 [*gensym-fix-name* fix-name])
    (define-values (args props body)
     (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define ctx (ctx-update-props default-ctx props))

    ; compiled function name
    (define fname
      (let-values ([(cx fname) (ctx-unique-name ctx name)])
        (begin0 fname (set! ctx cx))))

    ; compiled argument names
    (define-values (arg-names arg-ctxs)
      (for/lists (ns ps) ([arg (in-list args)])
        (match arg
         [(list '! props ... name)
          (let ([ctx* (ctx-update-props ctx props)])
            (values
              (let-values ([(cx aname) (ctx-unique-name ctx name)])
                (begin0 name (set! ctx cx)))
              ctx*))]
         [name
          (values
            (let-values ([(cx aname) (ctx-unique-name ctx name)])
              (begin0 name (set! ctx cx)))
            ctx)])))

    ;(define non-varnames (map (curry ctx-lookup-name ctx) (*reserved-names*)))
    (define p (open-output-string))
    (parameterize ([current-output-port p]
                   [*imperative-lang* (or (*imperative-lang*)
                                          (make-imperative-lang "default"))])
      (define-values (o cx) (visit/ctx imperative-visitor body ctx))
      (compile-function fname arg-names arg-ctxs
                        (get-output-string p) o
                        cx (set->list (*gensym-used-names*))))))

(module+ test
  (require rackunit)
  (define lang (make-imperative-lang "test"))
  ((compile lang) '(FPCore (a b) (+ (* a b) (- a b))) "foo")
)
