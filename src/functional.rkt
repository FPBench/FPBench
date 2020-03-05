#lang racket 

(require "common.rkt" "compilers.rkt")
(provide core->functional *func-lang* functional)

;;; Abstraction for different languages

(struct functional (name fix-name operator constant declaration let if while function))
(define *func-lang* (make-parameter #f))

(define (fix-name name)
  ((functional-fix-name (*func-lang*)) name))

(define (convert-constant expr ctx)
  ((functional-constant (*func-lang*)) expr (ctx-props ctx)))

(define (convert-operator op args ctx)
  ((functional-operator (*func-lang*)) op args (ctx-props ctx)))

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

(define (convert-expr expr #:ctx [ctx (make-compiler-ctx)] #:indent [indent "\t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var fix-name)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx #:indent (format "\t~a" indent))))))))
      (convert-let
        (string-join
          (for/list ([var* vars*] [val vals])
            (convert-declaration var* (convert-expr val #:ctx ctx #:indent (format "\t~a" indent))))
          (declaration-divider (format "\t~a" indent)))
        (convert-expr body #:ctx ctx* #:indent (format "\t~a" indent))
        indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var fix-name)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx* #:indent (format "\t~a" indent))))))))
      (convert-let
        (string-join
          (for/list ([var* vars*] [val* vals*])
            (convert-declaration var* val*))
          (declaration-divider (format "\t~a" indent)))
        (convert-expr body #:ctx ctx* #:indent (format "\t~a" indent))
        indent)]

    [`(if ,cond ,ift ,iff)
      (convert-if
        (convert-expr cond #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr ift #:ctx ctx #:indent (format "\t~a" indent))
        (convert-expr iff #:ctx ctx #:indent (format "\t~a" indent))
        indent)]        
    
    [`(while ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop ; CakeML
        (let-values ([(cx name) (ctx-unique-name ctx 'loop fix-name)])
          (set! ctx cx)
          name))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var fix-name)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx #:indent (format "\t~a" indent))))))))
      (define cond* (convert-expr cond #:indent (format "\t~a" indent)))
      (define-values (ctx** vars** updates*)
        (for/fold ([ctx** ctx*] [vars** '()] [updates* '()]) ([var vars] [val updates])
          (let-values ([(cx name) (ctx-unique-name ctx** var fix-name)])
            (values cx (flatten (cons vars** name)) 
                    (flatten (cons updates* (convert-expr val #:ctx ctx* #:indent (format "\t\t\t\t~a" indent)))))))) ;; tabs for Cake
      (convert-while 
          vars* vals* cond* updates* vars**
          (convert-expr body #:ctx ctx* #:indent (format "\t\t\t~a" indent)) loop indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop ; CakeML
        (let-values ([(cx name) (ctx-unique-name ctx 'loop fix-name)])
          (set! ctx cx)
          name))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var fix-name)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx* #:indent (format "\t~a" indent))))))))
      (define cond* (convert-expr cond #:indent (format "\t~a" indent)))
      (define-values (ctx** vars** updates*)
        (for/fold ([ctx** ctx*] [vars** '()] [updates* '()]) ([var vars] [val updates])
          (let-values ([(cx name) (ctx-unique-name ctx** var fix-name)])
            (values cx (flatten (cons vars** name)) 
                    (flatten (cons updates* 
                      (convert-expr 
                          val 
                          #:ctx (if (equal? (functional-name (*func-lang*)) "cml") ctx* ctx**)
                          #:indent (format "\t\t\t\t~a" indent)))))))) ;; tabs for Cake
      (convert-while 
          vars* vals* cond* updates* 
          (if (equal? (functional-name (*func-lang*)) "cml") vars* vars**)
          (convert-expr body #:ctx ctx* #:indent (format "\t\t\t~a" indent)) loop indent)]

    ;; Ignore all casts and precision contexts
    [`(cast ,body) (convert-expr body #:ctx ctx #:indent indent)]
    [(list '! props ... body) (convert-expr body #:ctx ctx #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:ctx ctx #:indent indent)) args))
     (convert-operator operator args_c ctx)] 
    [(list 'digits (? number? m) (? number? e) (? number? b)) (convert-constant expr ctx)] ; WLS
    [(? constant?) (convert-constant expr ctx)]
    [(? number?) (convert-constant expr ctx)]
    [(? symbol?) (ctx-lookup-name ctx expr)]))

(define (core->functional prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
  
  (parameterize ([*used-names* (mutable-set)] [*gensym-collisions* 1])
    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name) fix-name)])
        (set! ctx cx)
        fname))
    (define-values (ctx* args*)
      (for/fold ([ctx* ctx] [args* '()]) ([arg args])
        (let-values ([(cx name) (ctx-unique-name ctx* arg fix-name)])
          (values cx (flatten (cons args* name))))))  

    (convert-function func-name args* (convert-expr body #:ctx ctx*) ctx* (set->list (*used-names*)))))