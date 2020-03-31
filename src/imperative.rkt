#lang racket

(require "common.rkt" "compilers.rkt")
(provide convert-core *lang* language)

;;; Abstraction for different languages

(struct language (name operator constant declaration assignment round round-mode function))
(define *lang* (make-parameter #f))

(define (convert-operator ctx operator args)
  ((language-operator (*lang*)) (ctx-props ctx) operator args))

(define (convert-constant ctx expr)
  ((language-constant (*lang*)) (ctx-props ctx) expr))

(define (convert-declaration ctx var [val #f])
  (if (equal? ((language-name (*lang*))) "sollya")
    ((language-assignment (*lang*)) var val)
    ((language-declaration (*lang*)) (ctx-props ctx) var val)))
    
(define (convert-assignment var val)
  ((language-assignment (*lang*)) var val))

(define (round-expr val ctx)
  ((language-round (*lang*)) val (ctx-props ctx)))

(define (change-round-mode mode)
  ((language-round-mode (*lang*)) mode))

(define (convert-function name args arg-props body return ctx vars)
  ((language-function (*lang*)) name args arg-props body return ctx vars))

(define (while-name) ; Go is weird
  (if (equal? ((language-name (*lang*))) "go") "for" "while"))

;;; Compiler for imperative languages

(define (fix-name name) ;; Common fix-name
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define (convert-application ctx operator args)
  (match (cons operator args)
    [(list '- a) (round-expr (format "-(~a)" a) ctx)]
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) "TRUE"]
    [(list (or '+ '- '* '/) a b) (round-expr (format "(~a ~a ~a)" a operator b) ctx)]
    [(list (or '== '< '> '<= '>=) head args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "~a ~a ~a" a operator b))
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
     (format "(~a)" (string-join (map ~a a) " || "))]
    [(list (? operator? f) args ...)
     (convert-operator ctx operator args)]))

(define (convert-expr expr #:ctx [ctx (make-compiler-ctx)] #:indent [indent "\t"]) ;; need default for context
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                (convert-declaration cx name (convert-expr val #:ctx ctx #:indent indent)))        
            cx)))
      (convert-expr body #:ctx ctx* #:indent indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                 (convert-declaration cx name (convert-expr val #:ctx ctx* #:indent indent)))
            cx)))
      (convert-expr body #:ctx ctx* #:indent indent)]

    [`(if ,cond ,ift ,iff)
      (define test (convert-expr cond #:ctx ctx #:indent indent))
      (define outvar (ctx-random-name))
      ; Sollya has slightly different if
      (if (equal? ((language-name (*lang*))) "sollya")
          (printf "~aif (~a) then {\n" indent test)
          (printf "~a~a\n~aif (~a) {\n" indent (convert-declaration ctx outvar) indent test))
      (printf "~a\t~a\n" indent
          (convert-assignment outvar (convert-expr ift #:ctx ctx #:indent (format "~a\t" indent))))
      (printf "~a} else {\n" indent)
      (printf "~a\t~a\n" indent
          (convert-assignment outvar (convert-expr iff #:ctx ctx #:indent (format "~a\t" indent))))
      (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
      outvar]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                (convert-declaration cx name (convert-expr val #:ctx ctx #:indent indent)))
            (values cx (flatten (cons vars* name))))))
      (define test-var (ctx-random-name))
      (printf "~a~a\n" indent
          (convert-declaration
              (ctx-update-props ctx '(:precision boolean))
              test-var
              (convert-expr cond #:ctx ctx* #:indent indent)))
      ; Sollya has slightly different while
      (if (equal? ((language-name (*lang*))) "sollya")
          (printf "~awhile (~a) do {\n" indent test-var)
          (printf "~a~a (~a) {\n" indent (while-name) test-var))
      (define-values (ctx** vars**)
        (for/fold ([ctx** ctx*] [vars** '()]) ([var vars] [update updates])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a\t~a\n" indent
                (convert-declaration cx name (convert-expr update #:ctx ctx* #:indent indent*)))
          (values cx (flatten (cons vars** name))))))
      (for ([var* vars*] [var** vars**])
          (printf "~a\t~a\n" indent (convert-assignment var* var**)))
      (printf "~a\t~a\n" indent
          (convert-assignment test-var (convert-expr cond #:ctx ctx** #:indent indent*)))
      (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                (convert-declaration cx name (convert-expr val #:ctx ctx* #:indent indent)))
            (values cx (flatten (cons vars* name))))))
      (define test-var (ctx-random-name))
      (printf "~a~a\n" indent
          (convert-declaration
              (ctx-update-props ctx '(:precision boolean))
              test-var
              (convert-expr cond #:ctx ctx* #:indent indent)))
     ; Sollya has slightly different while*
      (if (equal? ((language-name (*lang*))) "sollya")
          (printf "~awhile (~a) do {\n" indent test-var)
          (printf "~a~a (~a) {\n" indent (while-name) test-var))
      (for ([var* vars*] [update updates])
        (printf "~a\t~a\n" indent
            (convert-assignment var* (convert-expr update #:ctx ctx* #:indent indent*))))
      (printf "~a\t~a\n" indent
          (convert-assignment test-var (convert-expr cond #:ctx ctx* #:indent indent*)))
       (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    [`(! ,props ... ,body)
      (define curr-round (ctx-lookup-prop ctx ':round 'binary64))
      (define new-round (dict-ref (apply hash-set* #hash() props) ':round curr-round))
      (if (not (equal? curr-round new-round))
          (begin 
            (printf "~a~a" indent (change-round-mode new-round))
            (let ([ret (convert-expr body #:ctx (ctx-update-props ctx props) #:indent indent)])
                 (printf "~a~a" indent (change-round-mode curr-round))
                 ret))
          (convert-expr body #:ctx (ctx-update-props ctx props) #:indent indent))]

    [(list (? operator? operator) args ...)
      (define args_c
          (map (λ (arg) (convert-expr arg #:ctx ctx #:indent indent)) args))
     (convert-application ctx operator args_c)] 
    [(? constant?)
     (convert-constant ctx expr)]
    [(? number?)
     (convert-constant ctx expr)]
    [(? symbol?)
     (ctx-lookup-name ctx expr)]))

(define (convert-core prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))

  (parameterize ([*used-names* (mutable-set)] [*gensym-collisions* 1] [*gensym-fix-name* fix-name])
    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
        (set! ctx cx)
        fname))

    (define-values (arg-names arg-props)
      (for/lists (n p) ([var args])
        (match var
          [(list '! props ... name) 
            (values 
                (let-values ([(cx name) (ctx-unique-name ctx name)])
                            (set! ctx cx)
                            name)
                (apply hash-set* (ctx-props ctx) props))]
          [name 
            (values 
                (let-values ([(cx name) (ctx-unique-name ctx name)])
                            (set! ctx cx)
                            name)
                (ctx-props ctx))])))

    (define-values (body-out return-out) 
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p])    
          (define out (convert-expr body #:ctx ctx))
          (values (get-output-string p) out))))

    (convert-function func-name arg-names arg-props body-out return-out ctx (set->list (*used-names*)))))