#lang racket 

(require "common.rkt" "compilers.rkt")
(provide core->functional *func-lang* functional)

;;; Abstraction for different languages

(struct functional (name fix-name operator constant declaration let if while function))
(define *func-lang* (make-parameter #f))

(define (fix-name name)
  ((functional-fix-name (*func-lang*)) name))

(define (convert-constant expr ctx)
  ((functional-constant (*func-lang*)) expr ctx))

(define (convert-operator op args ctx)
  ((functional-operator (*func-lang*)) op args ctx))

(define (convert-declaration var val)
  ((functional-declaration (*func-lang*)) var val))

(define (convert-let decls body indent)
  ((functional-let (*func-lang*)) decls body indent))

(define (convert-if cond ift iff indent)
  ((functional-if (*func-lang*)) cond ift iff indent))

(define (convert-while vars inits cond updates updatevars body loop indent)
  ((functional-while (*func-lang*)) vars inits cond updates updatevars body loop indent))

(define (convert-function name args body ctx names)
  ((functional-function (*func-lang*)) name args body ctx names))

(define (declaration-divider indent)
  (match (functional-name (*func-lang*))
    ["smtlib2" " "]
    ["cml" (format "\n~a" indent)]
    [_ ", "]))

;;; Compiler for functional languages

(define (func-gensym name)
  (if (or (equal? (functional-name (*func-lang*)) "smtlib2"))
      (fix-name name)  ; smtlib2 does not need to generate new names
      (gensym name fix-name)))

(define (convert-expr expr #:names [names #hash()] #:ctx [ctx #hash()] #:indent [indent "\t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define vars* (map func-gensym vars))
      (define names*
        (for/fold ([names* names]) ([var vars] [var* vars*])
          (dict-set names* var var*)))
      (convert-let
        (string-join
          (for/list ([var* vars*] [val vals])
            (convert-declaration var* (convert-expr val #:names names #:ctx ctx #:indent (format "\t~a" indent))))
          (declaration-divider (format "\t~a" indent)))
        (convert-expr body #:names names* #:ctx ctx #:indent (format "\t~a" indent))
        indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define vals*
        (for/list ([val vals])
          (convert-expr val #:names names #:ctx ctx #:indent (format "\t~a" indent))))
      (convert-let
        (string-join
          (for/list ([var vars] [val vals])
            (convert-declaration (dict-ref names var var) (convert-expr val #:names names #:ctx ctx #:indent (format "\t~a" indent))))
          (declaration-divider (format "\t~a" indent)))
        (convert-expr body #:names names #:ctx ctx #:indent (format "\t~a" indent))
        indent)]

    [`(if ,cond ,ift ,iff)
      (convert-if
        (convert-expr cond #:names names #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr ift #:names names #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr iff #:names names #:ctx ctx #:indent (format "\t~a" indent))
        indent)]        
    
    [`(while ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop (func-gensym 'loop)) ; CakeML
      (define vars* (map func-gensym vars))
      (define vars** (map func-gensym vars*))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      (define vals*
        (for/list ([val inits])
            (convert-expr val #:names names #:ctx ctx #:indent (format "\t~a" indent))))
      (convert-while
        vars*
        vals*
        (convert-expr cond #:names names* #:indent (format "\t~a" indent))
        (for/list ([update updates])
            (convert-expr update #:names names #:ctx ctx #:indent (format "\t\t\t\t~a" indent))) ; tabs for CakeML
        vars**
        (convert-expr body #:names names* #:ctx ctx #:indent (format "\t\t\t~a" indent))
        loop
        indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop (func-gensym 'loop)) ; CakeML
      (define vars* (map func-gensym vars))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      (define vals*
        (for/list ([val inits])
            (convert-expr val #:names names #:ctx ctx #:indent (format "\t~a" indent))))
      (convert-while
        vars*
        vals*
        (convert-expr cond #:names names* #:indent (format "\t~a" indent))
        (for/list ([update updates])
            (convert-expr update #:names names #:ctx ctx #:indent (format "\t\t\t\t~a" indent))) ; tabs for CakeML
        vars*
        (convert-expr body #:names names* #:ctx ctx #:indent (format "\t\t\t~a" indent))
        loop
        indent)]

    ;; Ignore all casts and precision contexts
    [`(cast ,body) (convert-expr body #:names names #:ctx ctx #:indent indent)]
    [(list '! props ... body) (convert-expr body #:names names #:ctx ctx #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:names names #:ctx ctx #:indent indent)) args))
     (convert-operator operator args_c ctx)] 
    [(list 'digits (? number? m) (? number? e) (? number? b)) (convert-constant expr ctx)] ; WLS
    [(? constant?) (convert-constant expr ctx)]
    [(? number?) (convert-constant expr ctx)]
    [(? symbol?) (dict-ref names expr expr)]))

(define (core->functional prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define ctx (apply hash-set* #hash() (append '(:precision binary64 :round nearestEven) props)))
  
  (parameterize ([*names* (mutable-set)])
    (define arg-names (map func-gensym args))
    (define names
      (for/fold ([names* (make-immutable-hash)]) ([arg args] [arg* arg-names])
          (dict-set names* arg arg*)))
    (convert-function
      (func-gensym name)
      arg-names
      (convert-expr body #:ctx ctx #:names names)
      ctx
      names)))