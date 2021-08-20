#lang racket

(require syntax/parse/define
         (for-syntax racket/syntax)
         racket/hash)

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
   visit-for_
   visit-for
   visit-for*
   visit-tensor
   visit-tensor*

   visit-!

   visit-terminal_
   visit-number
   visit-constant
   visit-symbol
   visit-digits

   visit-op_
   visit-cast
   visit-array
   visit-op
   visit-call

   reduce))

;; Visit functions can take an optional context as a keyword argument.
;; An empty context or "no context" is assumed to be an empty list.

;; The main dispatch function, visit-expr, always accepts this keyword
;; and defaults to an empty list if it is not provided;
;; however it only passes it along to the actual user-provided functions
;; if it's not an empty list, so it is safe to provide user-defined
;; visit functions that do not expect a #:ctx argument.

(define-syntax-parser vterm
  [(vterm vtor ctx (term args ...))
   #:with accessor (format-id #'term "visitor-~a" #'term)
   #'(if (null? ctx)
         ((accessor vtor) vtor args ...)
         ((accessor vtor) vtor args ... #:ctx ctx))]
  [(vterm vtor ctx (term args ...) (term2 args2 ...))
   #:with accessor (format-id #'term "visitor-~a" #'term)
   #:with accessor2 (format-id #'term2 "visitor-~a" #'term2)
   #'(if (accessor vtor)
         (if (null? ctx)
             ((accessor vtor) vtor args ...)
             ((accessor vtor) vtor args ... #:ctx ctx))
         (if (null? ctx)
             ((accessor2 vtor) vtor args2 ...)
             ((accessor2 vtor) vtor args2 ... #:ctx ctx)))])

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
    [`(for ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (vterm visitor ctx
            (visit-for vars vals accums inits updates body)
            (visit-for_ 'for vars vals accums inits updates body))]
    [`(for* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (vterm visitor ctx
            (visit-for* vars vals accums inits updates body)
            (visit-for_ 'for* vars vals accums inits updates body))]
    [`(tensor ([,vars ,vals] ...) ,body)
     (vterm visitor ctx (visit-tensor vars vals body))]
    [`(tensor* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (vterm visitor ctx (visit-tensor* vars vals accums inits updates body))]

    [`(! ,props ... ,body)
     (vterm visitor ctx (visit-! props body))]

    [(or (? number?) (? hex?))
     (vterm visitor ctx
            (visit-number expr)
            (visit-terminal_ expr))]
    [(? constant? c)
     (vterm visitor ctx
            (visit-constant c)
            (visit-terminal_ c))]
    [(? symbol? s)
     (vterm visitor ctx
            (visit-symbol s)
            (visit-terminal_ s))]
    [`(digits ,m ,e ,b)
     (vterm visitor ctx
            (visit-digits m e b)
            (visit-terminal_ expr))]

    [`(cast ,body)
     (vterm visitor ctx
            (visit-cast body)
            (visit-op_ 'cast (list body)))]
    [`(array ,args ...)
     (vterm visitor ctx
            (visit-array args)
            (visit-op_ 'array args))]
    [(list (? operator? operator) args ...)
     (vterm visitor ctx
            (visit-op operator args)
            (visit-op_ operator args))]
    [(list func args ...)
     (vterm visitor ctx
            (visit-call func args)
            (visit-op_ func args))]))

;; Useful shorthands

(define (visit vtor arg)
  ((visitor-visit-expr vtor) vtor arg))

(define (visit/ctx vtor arg ctx)
  ((visitor-visit-expr vtor) vtor arg #:ctx ctx))

(define (reduce vtor . args)
  (apply (visitor-reduce vtor) args))

;; default visitor

(define (visit-if visitor cond ift iff #:ctx [ctx '()])
  (visit/ctx visitor cond ctx)
  (visit/ctx visitor ift ctx)
  (visit/ctx visitor iff ctx)
  (void))

(define (visit-let_ visitor let_ vars vals body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-let visitor vars vals body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-let* visitor vars vals body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-while_ visitor while_ cond vars inits updates body #:ctx [ctx '()])
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-while visitor cond vars inits updates body #:ctx [ctx '()])
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-while* visitor cond vars inits updates body #:ctx [ctx '()])
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-for_ visitor for_ vars vals accums inits updates body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-for visitor vars vals accums inits updates body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-for* visitor vars vals accums inits updates body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-tensor visitor vars vals body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-tensor* visitor vars vals accums inits updates body #:ctx [ctx '()])
  (for ([val vals]) (visit/ctx visitor val ctx))
  (for ([init inits] [update updates])
    (visit/ctx visitor init ctx)
    (visit/ctx visitor update ctx))
  (visit/ctx visitor body ctx)
  (void))

(define (visit-! visitor props body #:ctx [ctx '()])
  (visit/ctx visitor body ctx)
  (void))

(define (visit-terminal_ visitor x #:ctx [ctx '()])
  (void))

(define (visit-number visitor x #:ctx [ctx '()])
  (void))

(define (visit-constant visitor x #:ctx [ctx '()])
  (void))

(define (visit-symbol visitor x #:ctx [ctx '()])
  (void))

(define (visit-digits visitor m e b #:ctx [ctx '()])
  (void))

(define (visit-op_ visitor operator args #:ctx [ctx '()])
  (for ([arg args]) (visit/ctx visitor arg ctx))
  (void))

(define (visit-cast visitor body #:ctx [ctx '()])
  (visit/ctx visitor body ctx)
  (void))

(define (visit-array visitor args #:ctx [ctx '()])
  (for ([arg args]) (visit/ctx visitor arg ctx))
  (void))

(define (visit-op visitor operator args #:ctx [ctx '()])
  (for ([arg args]) (visit/ctx visitor arg ctx))
  (void))

(define (visit-call visitor func args #:ctx [ctx '()])
  (for ([arg args]) (visit/ctx visitor arg ctx))
  (void))

;; visits the AST but does nothing
(define default-visitor
  (visitor
   visit-expr
   visit-if
   visit-let_
   #f
   #f
   visit-while_
   #f
   #f
   visit-for_
   #f
   #f
   visit-tensor
   visit-tensor*

   visit-!

   visit-terminal_
   #f
   #f
   #f
   #f

   visit-op_
   #f
   #f
   #f
   #f

   #f))

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
                 (list var
                       (visit/ctx visitor init ctx)
                       (visit/ctx visitor update ctx))))
            ,(visit/ctx visitor body ctx)))

(define (visit-while/transform visitor cond vars inits updates body #:ctx [ctx '()])
  `(while ,(visit/ctx visitor cond ctx)
     (,@(for/list ([var vars] [init inits] [update updates])
          (list var
                (visit/ctx visitor init ctx)
                (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-while*/transform visitor cond vars inits updates body #:ctx [ctx '()])
  `(while* ,(visit/ctx visitor cond ctx)
     (,@(for/list ([var vars] [init inits] [update updates])
          (list var
                (visit/ctx visitor init ctx)
                (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-for_/transform visitor for_ vars vals accums inits updates body #:ctx [ctx '()])
  `(,for_ (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
          (,@(for/list ([accum accums] [init inits] [update updates])
               (list accum
                     (visit/ctx visitor init ctx)
                     (visit/ctx visitor update ctx))))
          ,(visit/ctx visitor body ctx)))

(define (visit-for/transform visitor vars vals accums inits updates body #:ctx [ctx '()])
  `(for (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
     (,@(for/list ([accum accums] [init inits] [update updates])
          (list accum
                (visit/ctx visitor init ctx)
                (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-for*/transform visitor vars vals accums inits updates body #:ctx [ctx '()])
  `(for* (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
     (,@(for/list ([accum accums] [init inits] [update updates])
          (list accum
                (visit/ctx visitor init ctx)
                (visit/ctx visitor update ctx))))
     ,(visit/ctx visitor body ctx)))

(define (visit-tensor/transform visitor vars vals body #:ctx [ctx '()])
  `(tensor (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
           ,(visit/ctx visitor body ctx)))

(define (visit-tensor*/transform visitor vars vals accums inits updates body #:ctx [ctx '()])
  `(tensor* (,@(for/list ([var vars] [val vals]) (list var (visit/ctx visitor val ctx))))
            (,@(for/list ([accum accums] [init inits] [update updates])
                 (list accum
                       (visit/ctx visitor init ctx)
                       (visit/ctx visitor update ctx))))
            ,(visit/ctx visitor body ctx)))

(define (visit-!/transform visitor props body #:ctx [ctx '()])
  `(! ,@props ,(visit/ctx visitor body ctx)))

(define (visit-terminal_/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-number/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-constant/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-symbol/transform visitor x #:ctx [ctx '()])
  x)

(define (visit-digits/transform visitor m e b #:ctx [ctx '()])
  `(digits ,m ,e ,b))

(define (visit-op_/transform visitor operator args #:ctx [ctx '()])
  `(,operator ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-cast/transform visitor body #:ctx [ctx '()])
  `(cast ,(visit/ctx visitor body ctx)))

(define (visit-array/transform visitor args #:ctx [ctx '()])
  `(array ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-op/transform visitor operator args #:ctx [ctx '()])
  `(,operator ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-call/transform visitor func args #:ctx [ctx '()])
  `(,func ,@(for/list ([arg args]) (visit/ctx visitor arg ctx))))

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
   visit-for_/transform
   #f
   #f
   visit-tensor/transform
   visit-tensor*/transform

   visit-!/transform

   visit-terminal_/transform
   #f
   #f
   #f
   #f

   visit-op_/transform
   #f
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
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-while/reduce visitor cond vars inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (list (visit/ctx visitor cond ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-while*/reduce visitor cond vars inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (list (visit/ctx visitor cond ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-for_/reduce visitor for_ vars vals accums inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-for/reduce visitor vars vals accums inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-for*/reduce visitor vars vals accums inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-tensor/reduce visitor vars vals body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-tensor*/reduce visitor vars vals accums inits updates body #:ctx [ctx '()])
  (reduce visitor
          (append (for/list ([val vals]) (visit/ctx visitor val ctx))
                  (apply append (for/list ([init inits] [update updates])
                                  (list (visit/ctx visitor init ctx)
                                        (visit/ctx visitor update ctx))))
                  (list (visit/ctx visitor body ctx)))))

(define (visit-!/reduce visitor props body #:ctx [ctx '()])
  (reduce visitor (list (visit/ctx visitor body ctx))))

(define (visit-terminal_/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-number/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-constant/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-symbol/reduce visitor x #:ctx [ctx '()])
  1)

(define (visit-digits/reduce visitor m e b #:ctx [ctx '()])
  1)

(define (visit-op_/reduce visitor operator args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-cast/reduce visitor body #:ctx [ctx '()])
  (reduce visitor (list (visit/ctx visitor body ctx))))

(define (visit-array/reduce visitor args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-op/reduce visitor operator args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

(define (visit-call/reduce visitor func args #:ctx [ctx '()])
  (reduce visitor
          (for/list ([arg args]) (visit/ctx visitor arg ctx))))

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
   visit-for_/reduce
   #f
   #f
   visit-tensor/reduce
   visit-tensor*/reduce

   visit-!/reduce

   visit-terminal_/reduce
   #f
   #f
   #f
   #f

   visit-op_/reduce
   #f
   #f
   #f
   #f

   (curry apply +)))

;; syntax helpers

(begin-for-syntax
  (define-syntax-class method-clause
    (pattern [(name args ...) body ...+]
             #:with impl #'(lambda (args ...) body ...))
    (pattern [name impl])))

;; macro interface for creating visitors

(define-syntax-parser define-expr-visitor
  [(define-expr-visitor new-vtor method:method-clause ...)
   #'(define new-vtor
       (struct-copy visitor default-visitor
                    [method.name method.impl] ...))]
  [(define-expr-visitor default-vtor new-vtor method:method-clause ...)
   #'(define new-vtor
       (struct-copy visitor default-vtor
                    [method.name method.impl] ...))])

(define-simple-macro (define-transform-visitor new-vtor method:method-clause ...)
  (define-expr-visitor default-transform-visitor new-vtor method ...))

(define-simple-macro (define-reduce-visitor new-vtor method:method-clause ...)
  (define-expr-visitor default-reduce-visitor new-vtor method ...))

;; macro interface for defining expr visiting functions directly
;; i.e. like define/match, but specifically for FPCore expressions

(define-syntax-parser define/visit-expr
  [(define/visit-expr (fname expr) method:method-clause ...)
   #'(begin
       (define-expr-visitor default-visitor new-vtor method ...)
       (define (fname expr)
         (visit new-vtor expr)))]
  [(define/visit-expr (fname expr ctx) method:method-clause ...)
   #'(begin
       (define-expr-visitor default-visitor new-vtor method ...)
       (define (fname expr ctx)
         (visit/ctx new-vtor expr ctx)))]
  [(define/visit-expr default-vtor (fname expr) method:method-clause ...)
   #'(begin
       (define-expr-visitor default-vtor new-vtor method ...)
       (define (fname expr)
         (visit new-vtor expr)))]
  [(define/visit-expr default-vtor (fname expr ctx) method:method-clause ...)
   #'(begin
       (define-expr-visitor default-vtor new-vtor method ...)
       (define (fname expr ctx)
         (visit/ctx new-vtor expr ctx)))])

(define-syntax-parser define/transform-expr
  [(define/transform-expr (fname expr) method:method-clause ...)
   #'(define/visit-expr default-transform-visitor (fname expr) method ...)]
  [(define/transform-expr (fname expr ctx) method:method-clause ...)
   #'(define/visit-expr default-transform-visitor (fname expr ctx) method ...)])

(define-syntax-parser define/reduce-expr
  [(define/reduce-expr (fname expr) method:method-clause ...)
   #'(define/visit-expr default-reduce-visitor (fname expr) method ...)]
  [(define/reduce-expr (fname expr ctx) method:method-clause ...)
   #'(define/visit-expr default-reduce-visitor (fname expr ctx) method ...)])

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
    (check-equal? (void) (visit default-visitor e))
    (check-equal? e (visit default-transform-visitor e)))

  (check-equal?
   (for/list ([e es]) (visit default-reduce-visitor e))
   '(4 5 10 9 16))

  (define (visit-terminal_/maxdepth vtor x #:ctx ctx)
    (match-define (list current-depth max-depth) ctx)
    (when (> current-depth (unbox max-depth))
      (set-box! max-depth current-depth)))

  (define/visit-expr (maxdepth-helper expr ctx)
    [(visit-expr vtor expr #:ctx ctx)
     (match-define (list current-depth max-depth) ctx)
     (visit-expr vtor expr #:ctx (list (+ 1 current-depth) max-depth))]
    [visit-terminal_ visit-terminal_/maxdepth])

  (define (maxdepth expr)
    (let ([depth-box (box 0)])
      (maxdepth-helper expr (list 0 depth-box))
      (unbox depth-box)))

  (check-equal?
   (for/list ([e es]) (maxdepth e))
   '(3 5 5 3 4))

  (define/reduce-expr (hascontract? expr)
    [(visit-! vtor props body) #t]
    [(visit-terminal_ vtor x) #f]
    [(reduce args) (ormap values args)])

  (define/transform-expr (killcontracts expr)
    [(visit-! vtor props body) (visit vtor body)])

  (check-equal?
   (for/list ([e es]) (hascontract? e))
   '(#f #t #f #f #f))

  (for ([e es])
    (unless (hascontract? e)
      (check-equal? e (killcontracts e))))
)
