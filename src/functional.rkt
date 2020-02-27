#lang racket 

(require "common.rkt" "compilers.rkt")
(provide core->functional *func-lang* functional)

;;; Abstraction for different languages

(struct functional (name fix-name operator constant declaration block function))
(define *func-lang* (make-parameter #f))

(define (fix-name name)
  ((functional-fix-name (*func-lang*)) name))

(define (convert-constant expr ctx)
  ((functional-constant (*func-lang*)) expr ctx))

(define (convert-operator op args ctx)
  ((functional-operator (*func-lang*)) op args ctx))

(define (convert-declaration var val)
  ((functional-declaration (*func-lang*)) var val))

(define (convert-block name indent)
  ((functional-block (*func-lang*)) name indent))

(define (convert-function name args body ctx names)
  ((functional-function (*func-lang*)) name args body ctx names))

(define (declaration-divider)
  (if (equal? (functional-name (*func-lang*)) "smtlib2") " " ", "))

;;; Compiler for functional languages

(define (func-gensym name)
  (if (or (equal? (functional-name (*func-lang*)) "smtlib2"))
      (fix-name name)  ; smtlib2 does not need to generate new names
      (gensym name fix-name)))

(define (convert-expr expr #:names [names #hash()] #:ctx [ctx #hash()] #:indent [indent "/t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define vars* (map func-gensym vars))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      (format (convert-block 'let indent) ; var val
          (string-join
            (for/list ([var* vars*] [val vals])
              (convert-declaration var* (convert-expr val #:names names #:ctx ctx #:indent indent)))
            (declaration-divider))
          (convert-expr body #:names names* #:ctx ctx #:indent indent))]

    [`(if ,cond ,ift ,iff)
      (format (convert-block 'if indent) ; cond ift iff
        (convert-expr cond #:names names #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr ift #:names names #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr iff #:names names #:ctx ctx #:indent (format "\t~a" indent)))]        
    
    [`(while ,cond ([,vars ,inits ,updates] ...) ,body)
      ;(define loop (func-gensym 'loop)) CakeML
      (define vars* (map func-gensym vars))
      (define vars** (map func-gensym vars*))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      ;(define arg-list  CakeML
      ;  (string-join
      ;    (for/list ([var* vars*])
      ;      (format "~a" var*))
      ;    " "))
      (format (convert-block 'while indent) ; inits cond update assign body
          (string-join
            (for/list ([var* vars*] [val inits])
              (convert-declaration var* (convert-expr val #:names names* #:ctx ctx #:indent indent)))
            ", ")
          (convert-expr cond #:names names* #:ctx ctx #:indent indent)
          (string-join
            (for/list ([var** vars**] [update updates])
              (convert-declaration var** (convert-expr update #:names names* #:ctx ctx #:indent indent)))
            ", ")
          (string-join
            (for/list ([var* vars*] [var** vars**])
              (convert-declaration var* var**))
            "; ")
          (convert-expr body #:names names* #:ctx ctx #:indent indent))]

    ;; Ignore all casts and precision contexts
    [`(cast ,body) (convert-expr body #:names names #:ctx ctx #:indent indent)]
    [(list '! props ... body) (convert-expr body #:names names #:ctx ctx #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:names names #:ctx ctx #:indent indent)) args))
     (convert-operator operator args_c ctx)] 
    [(list 'digits (? number? m) (? number? e) (? number? b)) (convert-constant expr ctx)]
    [(? constant?) (convert-constant expr ctx)]
    [(? number?) (convert-constant expr ctx)]
    [(? symbol?) (dict-ref names expr expr)]))

(define (core->functional prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define ctx (apply hash-set* #hash() (append '(:precision binary64 :round nearestEven) props)))
  (define names #hash())
  
  (parameterize ([*names* (mutable-set)])
    (define arg-names 
      (for/list ([arg args])
          (let ([arg* (func-gensym arg)])
            (dict-set names arg arg*)
            arg*))) 
    (convert-function
      (func-gensym name)
      arg-names
      (convert-expr body #:ctx ctx #:names names)
      ctx
      names)))