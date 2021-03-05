#lang racket 

(require "common.rkt" "compilers.rkt")
(provide core->functional *func-lang* functional *reserved-names*)

;;; Abstraction for different languages

(struct functional (name operator constant declaration let if while function))
(define *func-lang* (make-parameter #f))

(define (convert-constant expr ctx)
  ((functional-constant (*func-lang*)) expr ctx))

(define (convert-operator op args ctx)
  ((functional-operator (*func-lang*)) op args ctx))

(define (convert-declaration var val)
  ((functional-declaration (*func-lang*)) var val))

(define (convert-let vars vals body indent nested)
  ((functional-let (*func-lang*)) vars vals body indent nested))

(define (convert-if cond ift iff tmp indent)
  ((functional-if (*func-lang*)) cond ift iff tmp indent))

(define (convert-while vars inits cond updates updatevars body loop indent nested)
  ((functional-while (*func-lang*)) vars inits cond updates updatevars body loop indent nested))

(define (convert-function name args body ctx names)
  ((functional-function (*func-lang*)) name args body ctx names))

;; Indenting rules

(define (match-indent type indent)   ;; TODO: make this better
  (match (functional-name (*func-lang*))
   ["cml"
    (match type
     ['let-decl (format "~a  " indent)] ; 2 spaces
     ['let-body (format "~a  " indent)]
     ['if  (format "~a  " indent)]
     ['while (format "~a      " indent)] ; 6 spaces
     ['while-decl (format "~a        " indent)])] ; 8 spaces
   [_
    (match type
     ['let-decl ""]
     ['let-body ""]
     ['if  ""]
     ['while ""]
     ['while-decl ""])]))

;;; Compiler for functional languages

(define *reserved-names* (make-parameter '()))

(define (convert-expr expr #:ctx [ctx (make-compiler-ctx)] #:indent [indent "\t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (values cx (flatten (cons vars* name))))))
      (convert-let
        vars*
        (for/list ([val vals]) (convert-expr val #:ctx ctx #:indent (match-indent 'let-decl indent)))
        (convert-expr body #:ctx ctx* #:indent (match-indent 'let-body indent))
        indent
        #f)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx* #:indent (match-indent 'let-decl indent))))))))
      (convert-let
        vars*
        vals*
        (convert-expr body #:ctx ctx* #:indent (match-indent 'let-body indent))
        indent 
        #t)]

    [`(if ,cond ,ift ,iff)
      (define-values (ctx* temp) (ctx-unique-name ctx 'test))
      (convert-if
        (convert-expr cond #:ctx ctx* #:indent (match-indent 'if indent))
        (convert-expr ift #:ctx ctx* #:indent (match-indent 'if indent))
        (convert-expr iff #:ctx ctx* #:indent (match-indent 'if indent))
        temp
        indent)]        
    
    [`(while ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop
        (let-values ([(cx name) (ctx-unique-name ctx 'loop)])
          (set! ctx cx)
          name))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx #:indent (match-indent 'let-decl indent))))))))
      (define cond* (convert-expr cond #:indent (match-indent 'if indent)))
      (define-values (ctx** vars** updates*)
        (for/fold ([ctx** ctx*] [vars** '()] [updates* '()]) ([var vars] [val updates])
          (let-values ([(cx name) (ctx-unique-name ctx** var)])
            (values cx (flatten (cons vars** name)) 
                       (flatten (cons updates*
                                (convert-expr val #:ctx ctx* 
                                              #:indent (match-indent 'while-decl indent))))))))
      (convert-while 
          vars* vals* cond* updates* vars**
          (convert-expr body #:ctx ctx* #:indent (match-indent 'while indent)) loop indent #f)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,body)
      (define loop
        (let-values ([(cx name) (ctx-unique-name ctx 'loop)])
          (set! ctx cx)
          name))
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx* ctx] [vars* '()] [vals* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (values cx (flatten (cons vars* name)) 
                    (flatten (cons vals* (convert-expr val #:ctx ctx* #:indent (match-indent 'let-decl indent))))))))
      (define cond* (convert-expr cond #:indent (match-indent 'if indent)))
      (define-values (ctx** vars** updates*)
        (for/fold ([ctx** ctx*] [vars** '()] [updates* '()]) ([var vars] [val updates])
          (let-values ([(cx name) (ctx-unique-name ctx** var)])
            (values cx (flatten (cons vars** name)) 
                    (flatten (cons updates* 
                      (convert-expr 
                          val 
                          #:ctx ctx*
                          #:indent (match-indent 'while-decl indent))))))))
      (convert-while 
          vars* vals* cond* updates* vars*
          (convert-expr body #:ctx ctx* #:indent (match-indent 'while indent)) loop indent #t)]

    ;; Ignore all casts
    [`(cast ,body) (convert-expr body #:ctx ctx #:indent indent)]
    [(list '! props ... body) (convert-expr body #:ctx (ctx-update-props ctx props) #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:ctx ctx #:indent indent)) args))
     (convert-operator operator args_c ctx)] 
    [(list 'digits m e b) (convert-constant (digits->number m e b) ctx)]
    [(? constant?) (convert-constant expr ctx)]
    [(or (? number?) (? hex?)) (convert-constant expr ctx)]
    [(? symbol?) (ctx-lookup-name ctx expr)]))


(define (core->functional prog name)
  (parameterize ([*used-names* (mutable-set)] [*gensym-collisions* 1])
    (define-values (args props body)
     (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define default-ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
    (define ctx (ctx-reserve-names default-ctx (*reserved-names*)))

    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
        (set! ctx cx)
        fname))
    
    (define-values (args* arg-props)
      (for/lists (n p) ([var args])
        (match var
          [(list '! props ... name) 
            (let ([props* (apply hash-set* (ctx-props ctx) props)])
              (values 
                (let-values ([(cx name) (ctx-unique-name ctx name (dict-ref props* ':precision 'binary64))])
                            (set! ctx cx)
                            name)
                props*))]
          [name 
            (values 
                (let-values ([(cx name) (ctx-unique-name ctx name)])
                            (set! ctx cx)
                            name)
                (ctx-props ctx))])))

    (define indent-level 
      (match (functional-name (*func-lang*))
       ["cml" "  "] 
       [_ "\t"]))
       
    (convert-function func-name args* (convert-expr body #:ctx ctx #:indent indent-level)
                      ctx (set->list (*used-names*)))))