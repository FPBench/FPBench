#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt")
(provide *imperative-lang* imperative compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *imperative-lang* (make-parameter #f))

(struct imperative
  (name
   infix
   operator
   constant
   type
   declare
   assign
   round
   round-mode
   program))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-infix-operator op args ctx)
  (match (cons op args)
   [(list '- a)
    (compile-round (format (if (string-prefix? a "-") "-(~a)" "-~a") a) ctx)]
   [(list 'not a)
    (format "!~a" a)]
   [(list (or '== '!= '< '> '<= '>=))
    "TRUE"]
   [(list (or '+ '- '* '/) a b) ; binary arithmetic 
    (compile-round (format "(~a ~a ~a)" a op b) ctx)]
   [(list (or '== '< '> '<= '>=) arg args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons arg args)] [b args])
                (format "~a ~a ~a" a op b))
              " && "))]
   [(list '!= args ...)
     (format "(~a)"
             (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                     (for/list ([b (cdr args)])
                       (format "~a != ~a" (car args) b))
                     (loop (cdr args)))))
              " && "))]
   [(list 'and a ...)
    (format "(~a)" (string-join (map ~a a) " && "))]
   [(list 'or a ...)
    (format "(~a)" (string-join (map ~a a) " || "))]))

(define (compile-operator op args ctx)
  (if (set-member? (imperative-infix (*imperative-lang*)) op)
      (compile-infix-operator op args ctx)
      ((imperative-operator (*imperative-lang*)) op args ctx)))

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

(define (compile-program name args arg-ctxs body ret ctx used-vars)
  ((imperative-program (*imperative-lang*)) name args arg-ctxs body ret ctx used-vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-infix-ops
  '(+ - * == != < > <= >= not and or))

(define (default-compile-operator fn args ctx)
  (format "~a(~a)" fn (string-join (map ~a args) ", ")))

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

(define (default-compile-round expr ctx)
  expr)

(define (define-compile-round-mode expr ctx)
  expr)

(define (default-compile-program name args arg-ctxs body ret ctx used-vars)
  (if (non-empty-string? body)
      (format "function ~a(~a) = {\n\t~a\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              body ret)
      (format "function ~a(~a) = {\n\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; language constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-imperative-lang name
                              #:infix-ops [infix default-infix-ops]
                              #:operator [operator default-compile-operator]
                              #:constant [constant default-compile-constant]
                              #:type [type default-compile-type]
                              #:declare [declare default-compile-declaration]
                              #:assign [assign default-compile-assignment]
                              #:round [round default-compile-round]
                              #:round-mode [round-mode define-compile-round-mode]
                              #:program [program default-compile-program])
  (imperative name infix
              operator constant type
              declare assign round round-mode
              program))

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

(define/match (collect-branches expr)
  [((list cond ift iff)) (cons (list cond ift) (collect-branches iff))]
  [(_) (list (list #t expr))])

;;;;;;;;;;;;;;;;;;;;;;;;;;; visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visit-if/imperative vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define branches (collect-branches (list cond ift iff)))
  (let loop ([branches branches] [first? #t] [ctx ctx] [ret #f])
    (match* (first? (car branches))
     [(#t (list cond ift))
      (define-values (ctx* tmpvar)   ; messy workaround to get ift context
        (parameterize ([current-output-port (open-output-nowhere)])
          (define ctx* (ctx-set-extra ctx 'indent (format "~a\t" indent)))
          (define-values (_ ift-ctx) (visit/ctx vtor ift ctx*))
          (define prec (ctx-lookup-prop ift-ctx ':precision))
          (ctx-random-name (ctx-update-props ctx `(:precision ,prec)))))
      (printf "~a~a\n~aif ~a {\n"
              indent (compile-declaration ctx* tmpvar)
              indent (format "(~a)" (trim-infix-parens cond)))
      (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx*))
      (printf "~a\t~a\n" indent (convert-assignment tmpvar ift*))
      (loop (cdr branches) #f ctx* tmpvar)]
     [(_ (list #t last))
      (printf "~a} else {\n" indent)
      (define-values (last* else-ctx) (visit/ctx vtor last ctx))
      (printf "~a\t~a\n" indent (compile-assignment ctx last*))
      (printf "~a}\n" indent)
      (values ret else-ctx)]
     [(_ (list cond elif))
      (printf "a} else if (~a) {\n"i indent (trim-infix-parens cond))
      
      

(define (visit-let_/imperative vtor let_ vars vals body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define ctx*
    (for/fold ([ctx* ctx]) ([var (in-list vars)] [val (in-list vals)])
      (define-values (val* val-ctx) (visit/ctx vtor val (match let_ ['let ctx] ['let* ctx*])))
      (define prec (ctx-lookup-prop val-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (define decl-ctx (ctx-update-props ctx* `(:precision ,prec)))
      (printf "~a~a\n" indent (compile-declaration name val* decl-ctx))
      name-ctx))
  (visit/ctx vtor body ctx*))

(define (visit-for_/imperative vtor for_ vars vals accums inits updates #:ctx ctx)
  (error 'imperative-visitor "unsupported operator: ~a" for_))

(define (visit-tensor/imperative vtor vars vals #:ctx ctx)
  (error 'imperative-visitor "unsupported operator: tensor"))

(define (visit-tensor*/imperative vtor vars vals accums inits updates #:ctx ctx)
  (error 'imperative-visitor "unsupported operation: tensor*"))

(define (visit-array/imperative vtor args #:ctx ctx)
  (error 'imperative-visitor "unsupported operation: array"))

(define (visit-op_/imperative vtor op args #:ctx ctx)
  (define args*
    (for/list ([arg args])
      (define-values (arg* _) (visit/ctx vtor arg ctx))
      arg*))
  (values (compile-operator op args* ctx) ctx))

(define (visit-number/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx) ctx))

(define (visit-constant/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx)
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props ':precision 'boolean)
              ctx)))

(define (visit-symbol/imperative vtor x #:ctx ctx)
  (values (ctx-lookup-name ctx x) ctx))

(define-transform-visitor imperative-visitor
  [visit-if visit-if/imperative]
  [visit-let_ visit-let_/imperative]
  [visit-for_ visit-for_/imperative]
  [visit-tensor visit-tensor/imperative]
  [visit-tensor* visit-tensor*/imperative]
  [visit-op_ visit-op_/imperative]
  [visit-number visit-number/imperative]
  [visit-constant visit-constant/imperative]
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
      (compile-program fname arg-names arg-ctxs
                       (get-output-string p) o
                       cx (set->list (*gensym-used-names*))))))

(module+ test
  (require rackunit)
  (define lang (make-imperative-lang "test"))
  (define (compile* . exprs)
    (let ([compile0 (compile lang)])
      (apply values (for/list ([expr exprs] [i (in-naturals 1)])
                      (compile0 expr (format "fn~a" i))))))
  
  (compile*
    '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))
    '(FPCore (a b) (+ (* a b) (- a b)))
    '(FPCore (x) (let ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (let* ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (if (< x 0) (+ x 1) (- x 1))))
)
