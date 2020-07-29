#lang racket

(require syntax/parse/define
         (for-syntax racket/syntax)
         racket/hash
         racket/extflonum)

(require "common.rkt")

(provide (except-out (all-defined-out)
                     vterm))

;; common visitor object

(struct visitor
  (visit-expr
   visit-if
   visit-let_
   visit-let
   visit-let*
   visit-while_
   visit-while
   visit-while*
   visit-!
   visit-op_
   visit-cast
   visit-op
   visit-terminal_
   visit-number
   visit-constant
   visit-symbol
   reduce))

(define-syntax-parser vterm
  [(vterm vtor ctx (term args ...))
   #:with accessor (format-id #'term "visitor-~a" #'term)
   #'((accessor vtor) vtor args ... #:ctx ctx)]
  [(vterm vtor ctx (term args ...) (term2 args2 ...))
   #:with accessor (format-id #'term "visitor-~a" #'term)
   #:with accessor2 (format-id #'term2 "visitor-~a" #'term2)
   #'(if (accessor vtor)
         ((accessor vtor) vtor args ... #:ctx ctx)
         ((accessor2 vtor) vtor args2 ... #:ctx ctx))])

(define (visit-expr visitor expr #:ctx [ctx '()])
  (match expr
    [`(if ,cond ,ift ,iff)
     (vterm visitor ctx (visit-if cond ift iff))]
    [`(let ([,vars ,vals] ...) ,body)
     (vterm visitor ctx
            (visit-let vars vals body)
            (visit-let_ 'let vars vals body))]
    [`(let* ([,vars ,vals] ...) ,body)
     (vterm visitor ctx
            (visit-let* vars vals body)
            (visit-let_ 'let* vars vals body))]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,body)
     (vterm visitor ctx
            (visit-while cond vars inits updates body)
            (visit-while_ 'while cond vars inits updates body))]
    [`(while* ,cond ([,vars ,inits ,updates] ...) ,body)
     (vterm visitor ctx
            (visit-while* cond vars inits updates body)
            (visit-while_ 'while* cond vars inits updates body))]
    [`(! ,props ... ,body)
     (vterm visitor ctx (visit-! props body))]
    [`(cast ,body)
     (vterm visitor ctx
            (visit-cast body)
            (visit-op_ 'cast (list body)))]
    ; TODO: digits probably doesn't go here
    [(list (? (λ (x) (or (operator? x) (equal? x 'digits))) operator) args ...) 
     (vterm visitor ctx 
            (visit-op operator args)
            (visit-op_ operator args))]
    [(or (? number? n) (? extflonum? n))
     (vterm visitor ctx
            (visit-number n)
            (visit-terminal_ n))]
    [(? constant? c)
     (vterm visitor ctx
            (visit-constant c)
            (visit-terminal_ c))]
    [(? symbol? s)
     (vterm visitor ctx
            (visit-symbol s)
            (visit-terminal_ s))]))

(define-syntax-rule (visit vtor arg)
  ((visitor-visit-expr vtor) vtor arg))

(define-syntax-rule (visit/ctx vtor arg ctx)
  ((visitor-visit-expr vtor) vtor arg #:ctx ctx))

(define-syntax-rule (reduce vtor args ...)
  ((visitor-reduce vtor) args ...))

;; AST transformers

(define (visit-if/transform visitor cond ift iff #:ctx [ctx '()])
  `(if ,(visit/ctx visitor cond ctx)
       ,(visit/ctx visitor ift ctx)
       ,(visit/ctx visitor iff ctx)))

(define (visit-let_/transform visitor let_ vars vals body #:ctx [ctx '()])
  `(,let_ (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
          ,(visit/ctx visitor body ctx)))

(define (visit-let/transform visitor vars vals body #:ctx [ctx '()])
  `(let (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-let*/transform visitor vars vals body #:ctx [ctx '()])
  `(let* (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-while_/transform visitor while_ cond vars inits updates body #:ctx [ctx '()])
  `(,while_ ,(visit/ctx visitor cond ctx)
            (,@(for/list ([var vars] [init inits] [update updates])
                 (list var (visit/ctx visitor init ctx) (visit/ctx visitor update ctx))))
            ,(visit/ctx visitor body ctx)))

(define (visit-while/transform visitor cond vars inits updates body #:ctx [ctx '()])
  `(while ,(visit/ctx visitor cond ctx)
     (,@(for/list ([var vars] [init inits] [update updates])
          (list var (visit/ctx visitor init ctx) (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-while*/transform visitor cond vars inits updates body #:ctx [ctx '()])
  `(while* ,(visit/ctx visitor cond ctx)
     (,@(for/list ([var vars] [init inits] [update updates])
          (list var (visit/ctx visitor init ctx) (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-!/transform visitor props body #:ctx [ctx '()])
  `(! ,@props ,(visit/ctx visitor body ctx)))

(define (visit-op_/transform visitor operator args #:ctx [ctx '()])
  `(,operator ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-cast/transform visitor body #:ctx [ctx '()])
  `(cast ,(visit/ctx visitor body ctx)))

(define (visit-op/transform visitor operator args #:ctx [ctx '()])
  `(,operator ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-terminal_/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-number/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-constant/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-symbol/transform visitor x #:ctx [ctx '()])
  x)

;; returns the AST unchanged
(define default-transform-visitor
  (visitor
   visit-expr
   visit-if/transform
   visit-let_/transform
   #f
   #f
   visit-while_/transform
   #f
   #f
   visit-!/transform
   visit-op_/transform
   #f
   #f
   visit-terminal_/transform
   #f
   #f
   #f
   #f))

;; AST reductions

(define (visit-if/reduce visitor cond ift iff #:ctx [ctx '()])
  (reduce visitor
          (list (visit/ctx visitor cond ctx)
                (visit/ctx visitor ift ctx)
                (visit/ctx visitor iff ctx))))

(define (visit-let_/reduce visitor let_ vars vals body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-let/reduce visitor vars vals body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-let*/reduce visitor vars vals body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-while_/reduce visitor while_ cond vars inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (list (visit/ctx visitor cond ctx))
                  (for/list ([init inits]) (visit/ctx visitor init ctx))
                  (for/list ([update updates]) (visit/ctx visitor update ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-while/reduce visitor cond vars inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (list (visit/ctx visitor cond ctx))
                  (for/list ([init inits]) (visit/ctx visitor init ctx))
                  (for/list ([update updates]) (visit/ctx visitor update ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-while*/reduce visitor cond vars inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (list (visit/ctx visitor cond ctx))
                  (for/list ([init inits]) (visit/ctx visitor init ctx))
                  (for/list ([update updates]) (visit/ctx visitor update ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-!/reduce visitor props body #:ctx [ctx '()])
  (reduce visitor (list (visit/ctx visitor body ctx))))

(define (visit-op_/reduce visitor operator args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-cast/reduce visitor body #:ctx [ctx '()])
  (reduce visitor (list (visit/ctx visitor body ctx))))

(define (visit-op/reduce visitor operator args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-terminal_/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-number/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-constant/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-symbol/reduce visitor x #:ctx [ctx '()])
  1)

;; counts terminals
(define default-reduce-visitor
  (visitor
   visit-expr
   visit-if/reduce
   visit-let_/reduce
   #f
   #f
   visit-while_/reduce
   #f
   #f
   visit-!/reduce
   visit-op_/reduce
   #f
   #f
   visit-terminal_/reduce
   #f
   #f
   #f
   (curry apply +)))

;; macro interface for creating visitors

(define-simple-macro (define-expr-visitor default-visitor new-visitor [method impl] ...)
  (define new-visitor
    (struct-copy visitor default-visitor
                 [method impl] ...)))

(define-simple-macro (define-transform-visitor new-visitor [method impl] ...)
  (define-expr-visitor default-transform-visitor new-visitor [method impl] ...))

(define-simple-macro (define-reduce-visitor new-visitor [method impl] ...)
  (define-expr-visitor default-reduce-visitor new-visitor [method impl] ...))

;; macro interface for defining expr visiting functions directly
;; i.e. like define/match, but specifically for FPCore expressions

(begin-for-syntax
  (define-syntax-class method-clause
    (pattern [(name args ...) body ...+]
             #:with impl #'(lambda (args ...) body ...))
    (pattern [name impl])))

(define-simple-macro (define/visit-expr default-visitor (fname expr ctx)
                       method:method-clause ...)
  (begin
    (define expr-visitor
      (struct-copy visitor default-visitor
                   [method.name method.impl] ...))
    (define (fname expr ctx)
      (visit/ctx expr-visitor expr ctx))))

(define-simple-macro (define/transform-expr (fname expr ctx) method:method-clause ...)
  (define/visit-expr default-transform-visitor (fname expr ctx) method ...))

(define-simple-macro (define/reduce-expr (fname expr ctx) method:method-clause ...)
  (define/visit-expr default-reduce-visitor (fname expr ctx) method ...))

;; quick tests

(module+ test
  (require rackunit)

  (define es
    (list
     '(let ([x (+ a b)]) (+ x a))
     '(! :precision (float 8 16) (let* ([y (+ a b)] [z PI]) (sin (* y z))))
     '(let ([x (+ a b)])
        (if (let ([y (- x 0)]) (== y x))
            (- x (let ([z a]) z))
            y))
     '(while (< a 3)
        ([a 0.0 (+ a 1.0)]
         [b 1.0 (* b 2.0)])
        b)
     '(while* (<= a 3)
              ([a 0.0 (+ a 1.0)]
               [b 0.0 (while* (<= i a)
                              ([i 0 (+ i 1)]
                               [x 0.0 (+ x i)])
                              x)])
              b)
     ))

  (for ([e es])
    (check-equal?
     e
     (visit default-transform-visitor e)))

  (check-equal?
   (for/list ([e es]) (visit default-reduce-visitor e))
   '(4 5 10 9 16))
)
