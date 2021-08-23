#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt")

(provide make-imperative-lang
         make-imperative-compiler
         visit-if/imperative
         visit-let_/imperative
         visit-while_/imperative
         visit-for_/imperative
         visit-tensor/imperative
         visit-tensor*/imperative
         visit-array/imperative
         visit-cast/imperative
         visit-!/imperative
         visit-call/imperative
         visit-op_/imperative
         visit-digits/imperative
         visit-number/imperative
         visit-constant/imperative
         visit-symbol/imperative)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *imperative-lang* (make-parameter #f))

(struct imperative
  (name infix while-name operator constant type
   declare assign round implicit-round round-mode
   use-vars program flags))

;;;;;;;;;;;;;;;;;;;;;;;;;;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-flags
  '(no-parens-around-condition))

(define (valid-flag? maybe-flag)
  (set-member? valid-flags maybe-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (while-name)
  (imperative-while-name (*imperative-lang*)))

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

(define (compile-function fn args ctx)
  ((imperative-operator (*imperative-lang*)) fn args ctx))

(define (compile-constant x ctx)
  ((imperative-constant (*imperative-lang*)) x ctx))

(define (compile-type x)
  ((imperative-type (*imperative-lang*)) x))

(define compile-declaration
  (case-lambda
   [(var ctx) ((imperative-declare (*imperative-lang*)) var ctx)]
   [(var val ctx) ((imperative-declare (*imperative-lang*)) var (trim-infix-parens val) ctx)]))

(define (compile-assignment var val ctx)
  ((imperative-assign (*imperative-lang*)) var (trim-infix-parens val) ctx))

(define (compile-round expr ctx)
  ((imperative-round (*imperative-lang*)) expr ctx))

(define (compile-implicit-round op arg ctx arg-ctx)
  ((imperative-implicit-round (*imperative-lang*)) op arg ctx arg-ctx))

(define (compile-round-mode mode ctx)
  ((imperative-round-mode (*imperative-lang*)) mode ctx))

(define (compile-use-vars vars ctx)
  (define vars* (map (curry ctx-lookup-name ctx) vars))
  ((imperative-use-vars (*imperative-lang*)) vars* ctx))

(define (compile-program name args arg-ctxs body ret ctx used-vars)
  ((imperative-program (*imperative-lang*)) name args arg-ctxs body ret ctx used-vars))

(define (compile-flag-raised? flag)
  (set-member? (imperative-flags (*imperative-lang*)) flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-infix-ops '(+ - * / == != < > <= >= not and or))
(define default-while-name "while")

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

(define (default-compile-implicit-round op arg ctx arg-ctx)
  arg)

(define (default-compile-round-mode expr ctx)
  expr)

(define (default-use-vars vars ctx)
  "")

(define (default-compile-program name args arg-ctxs body ret ctx used-vars)
  (if (non-empty-string? body)
      (format "function ~a(~a) = {\n~a\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              body ret)
      (format "function ~a(~a) = {\n\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; language constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-imperative-lang name
                              #:infix-ops [infix default-infix-ops]
                              #:while-name [while-name default-while-name]
                              #:operator [operator default-compile-operator]
                              #:constant [constant default-compile-constant]
                              #:type [type default-compile-type]
                              #:declare [declare default-compile-declaration]
                              #:assign [assign default-compile-assignment]
                              #:round [round default-compile-round]
                              #:implicit-round [implicit-round default-compile-implicit-round]
                              #:round-mode [round-mode default-compile-round-mode]
                              #:use-vars [use-vars default-use-vars]
                              #:program [program default-compile-program]
                              #:flags [flags '()])
  (unless (andmap valid-flag? flags)
    (error 'make-imperative-lang "Undefined imperative flags: ~a" flags))
  (imperative name infix while-name operator constant type
              declare assign round implicit-round round-mode
              use-vars program flags))

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

(define (format-condition cond)
  (if (compile-flag-raised? 'no-parens-around-condition)
      (format "~a" cond)
      (format "(~a)" cond)))

(define bool-ops '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit))

;;;;;;;;;;;;;;;;;;;;;;;;;;; visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visit-if/imperative vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define branches 
    (let loop ([expr (list 'if cond ift iff)])
      (match expr
       [(list 'if cond ift iff)
        (define-values (cond* _) (visit/ctx vtor cond ctx))
        (cons (list cond* ift) (loop iff))]
       [_ (list (list #t expr))])))
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
              indent (compile-declaration tmpvar ctx*)
              indent (format-condition (trim-infix-parens cond)))
      (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx*))
      (printf "~a\t~a\n" indent (compile-assignment tmpvar ift* ctx))
      (loop (cdr branches) #f ctx* tmpvar)]
     [(_ (list #t last))
      (printf "~a} else {\n" indent)
      (define ctx* (ctx-set-extra ctx 'indent (format "~a\t" indent)))
      (define-values (last* else-ctx) (visit/ctx vtor last ctx*))
      (printf "~a\t~a\n" indent (compile-assignment ret last* ctx))
      (printf "~a}\n" indent)
      (values ret else-ctx)]
     [(_ (list cond elif))
      (printf "~a} else if ~a {\n" indent (format-condition (trim-infix-parens cond)))
      (define ctx* (ctx-set-extra ctx 'indent (format "~a\t" indent)))
      (define-values (elif* elif-ctx) (visit/ctx vtor elif ctx*))
      (printf "~a\t~a\n" indent (compile-assignment ret elif* ctx))
      (loop (cdr branches) #f ctx ret)])))

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
  (printf "~a" (compile-use-vars vars ctx*))
  (visit/ctx vtor body ctx*))

(define (visit-while_/imperative vtor while_ cond vars inits updates body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (ctx* vars*)
    (for/fold ([ctx* ctx] [vars* '()] #:result (values ctx* (reverse vars*)))
              ([var (in-list vars)] [val (in-list inits)])
      (define val-ctx (match while_ ['while ctx] ['while* ctx*]))
      (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
      (define prec (ctx-lookup-prop val*-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (define decl-ctx (ctx-update-props ctx* `(:precision ,prec)))
      (printf "~a~a\n" indent (compile-declaration name val* decl-ctx))
      (values name-ctx (cons name vars*))))
  (define tmpvar
    (let-values ([(cx name) (ctx-random-name ctx)])
      (begin0 name (set! ctx cx))))
  (printf "~a" (compile-use-vars vars ctx*))
  (define-values (cond* cond*-ctx) (visit/ctx vtor cond ctx*))
  (printf "~a~a\n" indent (compile-declaration tmpvar cond* cond*-ctx))
  (printf "~a~a ~a {\n" indent (while-name) (format-condition tmpvar))
  (define ctx**
    (match while_
     ['while
      (define val-ctx (ctx-set-extra ctx* 'indent (format "~a\t" indent)))
      (define-values (ctx** vars**)
        (for/fold ([ctx** ctx*] [vars* '()]
                  #:result (values (ctx-set-extra ctx* 'indent (format "~a\t" indent))
                                   (reverse vars*)))
                  ([var (in-list vars)] [val (in-list updates)])
          (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
          (define prec (ctx-lookup-prop val*-ctx ':precision))
          (define-values (name-ctx name) (ctx-unique-name ctx** var prec))
          (define decl-ctx (ctx-update-props ctx** `(:precision ,prec)))
          (printf "~a\t~a\n" indent (compile-declaration name val* decl-ctx))
          (values name-ctx (cons name vars*))))
      (printf "~a" (compile-use-vars vars ctx**))
      (for ([var* (in-list vars*)] [var** (in-list vars**)])
        (printf "~a\t~a\n" indent (compile-assignment var* var** ctx**)))
      ctx**]
     ['while*
      (define ctx** (ctx-set-extra ctx* 'indent (format "~a\t" indent)))
      (for ([var* (in-list vars*)] [val (in-list updates)])
        (let-values ([(val* _) (visit/ctx vtor val ctx**)])
          (printf "~a\t~a\n" indent (compile-assignment var* val* ctx**))))
      ctx**]))
  (define-values (cond** cond**-ctx) (visit/ctx vtor cond ctx**))
  (printf "~a\t~a\n" indent (compile-assignment tmpvar cond** cond**-ctx))
  (printf "~a}\n" indent)
  (visit/ctx vtor body ctx*))
  
(define (visit-for_/imperative vtor for_ vars vals accums inits updates #:ctx ctx)
  (error 'imperative-visitor "unsupported operator: ~a" for_))

(define (visit-tensor/imperative vtor vars vals #:ctx ctx)
  (error 'imperative-visitor "unsupported operator: tensor"))

(define (visit-tensor*/imperative vtor vars vals accums inits updates #:ctx ctx)
  (error 'imperative-visitor "unsupported operation: tensor*"))

(define (visit-array/imperative vtor args #:ctx ctx)
  (error 'imperative-visitor "unsupported operation: array"))

(define (visit-cast/imperative vtor x #:ctx ctx)
  (define-values (body* body-ctx) (visit/ctx vtor x ctx))
  (values (compile-round body* ctx) body-ctx))

(define (visit-!/imperative vtor props body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define curr-prec (ctx-lookup-prop ctx ':precision))
  (define curr-round (ctx-lookup-prop ctx ':round))
  (define ctx* (ctx-update-props ctx props))
  (define new-prec (ctx-lookup-prop ctx* ':precision))
  (define new-round (ctx-lookup-prop ctx* ':round))
  (define body-ctx
    (parameterize ([current-output-port (open-output-nowhere)])
      (let-values ([(_ body-ctx) (visit/ctx vtor body ctx*)])
        body-ctx)))
  (define body-prec (ctx-lookup-prop body-ctx ':precision))
  (define-values (ctx** tmpvar)
    (let ([ctx** (ctx-update-props ctx* `(:precision ,body-prec))])
      (ctx-random-name ctx**)))
  (unless (equal? curr-round new-round)
    (printf "~a" (compile-round-mode new-round ctx)))
  (define-values (body* _) (visit/ctx vtor body ctx*))
  (printf "~a~a\n" indent (compile-declaration tmpvar body* ctx**))
  (unless (equal? curr-round new-round)
    (printf "~a" (compile-round-mode curr-round ctx)))
  (values tmpvar body-ctx))

(define (visit-op_/imperative vtor op args #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define args*
    (for/list ([arg args])
      (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
      (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
      (if (equal? prec arg-prec)
          arg*
          (compile-implicit-round op arg* arg-ctx ctx))))
  (values (compile-operator op args* ctx)
          (if (set-member? bool-ops op)
              (ctx-update-props ctx (list ':precision 'boolean))
              ctx)))

(define (visit-call/imperative vtor fn args #:ctx ctx)
  (define args*
    (for/list ([arg args])
      (define-values (arg* _) (visit/ctx vtor arg ctx))
      arg*))
  (values (compile-function fn args ctx) ctx))

(define (visit-digits/imperative vtor m e b #:ctx ctx)
  (visit/ctx vtor (digits->number m e b) ctx))

(define (visit-number/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx) ctx))

(define (visit-constant/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx)
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props ctx (list ':precision 'boolean))
              ctx)))

(define (visit-symbol/imperative vtor x #:ctx ctx)
  (define var-prec (ctx-lookup-prec ctx x))
  (values (ctx-lookup-name ctx x) (ctx-update-props ctx `(:precision ,var-prec))))

(define-transform-visitor imperative-visitor
  [visit-if visit-if/imperative]
  [visit-let_ visit-let_/imperative]
  [visit-while_ visit-while_/imperative]
  [visit-for_ visit-for_/imperative]
  [visit-tensor visit-tensor/imperative]
  [visit-tensor* visit-tensor*/imperative]
  [visit-array visit-array/imperative]
  [visit-cast visit-cast/imperative]
  [visit-! visit-!/imperative]
  [visit-call visit-call/imperative]
  [visit-op_ visit-op_/imperative]
  [visit-digits visit-digits/imperative]
  [visit-number visit-number/imperative]
  [visit-constant visit-constant/imperative]
  [visit-symbol visit-symbol/imperative])

;;;;;;;;;;;;;;;;;;;;;;;;;;; top-level compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-imperative-compiler lang
                                  #:visitor [vtor imperative-visitor]
                                  #:reserved [reserved '()])
  (lambda (prog name)
    (parameterize ([*gensym-used-names* (mutable-set)] 
                   [*gensym-collisions* 1]
                   [*gensym-fix-name* fix-name]
                   [*imperative-lang* lang])
      (define-values (args props body)
      (match prog
       [(list 'FPCore (list args ...) props ... body) (values args props body)]
       [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
      (define ctx (ctx-reserve-names (ctx-update-props default-ctx props) reserved))

      ; compiled function name
      (define fname
        (let-values ([(cx fname) (ctx-unique-name ctx name)])
          (begin0 fname (set! ctx cx))))

      ; compiled argument names
      (define-values (arg-names arg-ctxs)
        (for/lists (ns ps) ([arg (in-list args)])
          (match arg
           [(list '! props ... name)
            (define arg-ctx (ctx-update-props ctx props))
            (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
            (define-values (cx aname) (ctx-unique-name ctx name arg-prec))
            (begin0 (values aname arg-ctx) (set! ctx cx))]
           [name
            (define-values (cx aname) (ctx-unique-name ctx name))
            (begin0 (values aname ctx) (set! ctx cx))])))

      (define non-varnames (map (curry ctx-lookup-name ctx) reserved))
      (define p (open-output-string))
      (parameterize ([current-output-port p])
        (define-values (o cx) (visit/ctx vtor body ctx))
        (compile-program fname arg-names arg-ctxs
                         (get-output-string p) o
                         cx (remove* non-varnames (set->list (*gensym-used-names*))))))))

(module+ test
  (require rackunit)
  (define lang (make-imperative-lang "default"))
  (define (compile* . exprs)
    (let ([compile0 (make-imperative-compiler lang)])
      (apply values (for/list ([expr exprs] [i (in-naturals 1)])
                      (compile0 expr (format "fn~a" i))))))
  
  (compile*
    '(FPCore (x) (if (< x 0) (+ x 1) (- x 1)))
    '(FPCore (x) (let ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (let* ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (while (< x 4) ([x 0.0 (+ x 1.0)]) x))
    '(FPCore (x) (while* (< x 4) ([x 0.0 (+ x 1.0)]) x))
    '(FPCore (x) (+ (foo x) 1))
    '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))
    '(FPCore (a b) (+ (* a b) (- a b))))
)
