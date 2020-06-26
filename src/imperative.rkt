#lang racket

(require "common.rkt" "compilers.rkt")
(provide convert-core convert-expr *lang* language *reserved-names* *fix-name-format*)

;;; Abstraction for different languages

(struct language (name operator constant declaration assignment round round-mode function))
(define *lang* (make-parameter #f))

(define (convert-operator ctx operator args)
  ((language-operator (*lang*)) (ctx-props ctx) operator args))

(define (convert-constant ctx expr)
  ((language-constant (*lang*)) (ctx-props ctx) expr))

(define (convert-declaration ctx var [val #f])
  (if (equal? (language-name (*lang*)) "sollya")
    ((language-assignment (*lang*)) var val)
    ((language-declaration (*lang*)) (ctx-props ctx) var val)))
    
(define (convert-assignment var val)
  ((language-assignment (*lang*)) var val))

(define (round-expr val ctx) ; Sollya will never ignore but C can
  ((language-round (*lang*)) val (ctx-props ctx)))

(define (change-round-mode mode indent) ; C only
  ((language-round-mode (*lang*)) mode indent))

(define (convert-function name args arg-props body return ctx vars)
  ((language-function (*lang*)) name args arg-props body return ctx vars))

(define (while-name) ; Go only
  (if (equal? (language-name (*lang*)) "go") "for" "while"))

(define (use-vars vars indent) ; Go doesn't like unused variables
  (if (equal? (language-name (*lang*)) "go")
    (format "~aUse(~a)\n" indent (string-join vars ", "))
    ""))

;;; Compiler for imperative languages

(define *reserved-names* (make-parameter '()))

(define *fix-name-format* (make-parameter "_~a_"))

(define (fix-name name) ;; Common fix-name
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format (*fix-name-format*) (char->integer char))))
   ""))

(define (convert-application ctx operator args)
  (match (cons operator args)
    [(list '- a) (round-expr (format "-(~a)" a) ctx)]
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) "TRUE"]
    [(list (or '+ '- '*) a b) (round-expr (format "(~a ~a ~a)" a operator b) ctx)]  ; Division not included!! (Sollya issues)
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


(define (convert-expr expr #:ctx [ctx (make-compiler-ctx)] #:indent [indent "\t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent 
                (convert-declaration cx name (convert-expr val #:ctx ctx #:indent indent)))
            cx)))
      (printf "~a" (use-vars (for/list ([var vars]) (ctx-lookup-name ctx* var)) indent))
      (convert-expr body #:ctx ctx* #:indent indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                 (convert-declaration cx name (convert-expr val #:ctx ctx* #:indent indent)))
            cx)))
      (printf "~a" (use-vars (for/list ([var vars]) (ctx-lookup-name ctx* var)) indent))
      (convert-expr body #:ctx ctx* #:indent indent)]

    [`(if ,cond ,ift ,iff)
      (define test (convert-expr cond #:ctx ctx #:indent indent))
      (define outvar
        (let-values ([(cx name) (ctx-random-name ctx)])
            (set! ctx cx)
            name))
      (if (equal? (language-name (*lang*)) "scala")   ; Scala/Daisy has a very different if
        (printf "~a~a\n"
          indent
          (convert-declaration
            ctx 
            outvar
            (format "\n~a\tif (~a) {\n~a\t\t~a\n~a\t} else {\n~a\t\t~a\n~a\t}"
                indent test 
                indent (convert-expr ift #:ctx ctx #:indent (format "\t\t~a" indent)) indent 
                indent (convert-expr iff #:ctx ctx #:indent (format "\t\t~a" indent)) indent)))
        (begin
          (if (equal? (language-name (*lang*)) "sollya") ; Sollya has slightly different if
              (printf "~aif (~a) then {\n" indent test)
              (printf "~a~a\n~aif (~a) {\n" indent (convert-declaration ctx outvar) indent test))
          (printf "~a\t~a\n" indent
              (convert-assignment outvar (convert-expr ift #:ctx ctx #:indent (format "~a\t" indent))))
          (printf "~a} else {\n" indent)
          (printf "~a\t~a\n" indent
              (convert-assignment outvar (convert-expr iff #:ctx ctx #:indent (format "~a\t" indent))))
          (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))))
      outvar]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                (convert-declaration cx name (convert-expr val #:ctx ctx #:indent indent)))
            (values cx (flatten (cons vars* name))))))
      (define test-var
        (let-values ([(cx name) (ctx-random-name ctx)])
            (set! ctx cx)
            name))
      (printf "~a" (use-vars vars* indent))
      (printf "~a~a\n" indent
          (convert-declaration
              (ctx-update-props ctx '(:precision boolean))
              test-var
              (convert-expr cond #:ctx ctx* #:indent indent)))
      ; Sollya has slightly different while
      (if (equal? (language-name (*lang*)) "sollya")
          (printf "~awhile (~a) do {\n" indent test-var)
          (printf "~a~a (~a) {\n" indent (while-name) test-var))
      (define-values (ctx** vars**)
        (for/fold ([ctx** ctx*] [vars** '()]) ([var vars] [update updates])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a\t~a\n" indent
                (convert-declaration cx name (convert-expr update #:ctx ctx* #:indent indent*)))
          (values cx (flatten (cons vars** name))))))
      (printf "~a" (use-vars vars** indent*))
      (for ([var* vars*] [var** vars**])
          (printf "~a\t~a\n" indent (convert-assignment var* var**)))
      (printf "~a\t~a\n" indent
          (convert-assignment test-var (convert-expr cond #:ctx ctx** #:indent indent*)))
      (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let-values ([(cx name) (ctx-unique-name ctx* var)])
            (printf "~a~a\n" indent
                (convert-declaration cx name (convert-expr val #:ctx ctx* #:indent indent)))
            (values cx (flatten (cons vars* name))))))
      (define test-var
        (let-values ([(cx name) (ctx-random-name ctx)])
            (set! ctx cx)
            name))
      (printf "~a" (use-vars vars* indent))
      (printf "~a~a\n" indent
          (convert-declaration
              (ctx-update-props ctx '(:precision boolean))
              test-var
              (convert-expr cond #:ctx ctx* #:indent indent)))
     ; Sollya has slightly different while*
      (if (equal? (language-name (*lang*)) "sollya")
          (printf "~awhile (~a) do {\n" indent test-var)
          (printf "~a~a (~a) {\n" indent (while-name) test-var))
      (for ([var* vars*] [update updates])
        (printf "~a\t~a\n" indent
            (convert-assignment var* (convert-expr update #:ctx ctx* #:indent indent*))))
      (printf "~a\t~a\n" indent
          (convert-assignment test-var (convert-expr cond #:ctx ctx* #:indent indent*)))
       (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    ; Ignore all casts
    [`(cast ,body) (round-expr (convert-expr body #:ctx ctx #:indent indent) ctx)]

    [`(! ,props ... ,body)
      (define curr-prec (ctx-lookup-prop ctx ':precision 'nearestEven))
      (define curr-round (ctx-lookup-prop ctx ':round 'nearestEven))
      (define new-prec (dict-ref (apply hash-set* #hash() props) ':precision curr-prec))
      (define new-round (dict-ref (apply hash-set* #hash() props) ':round curr-round))
      (cond
       [(and (equal? (language-name (*lang*)) "c")  ; Only C needs to emit a temporary variable for different rounding
            (not (equal? curr-round new-round)))
        (let-values ([(ctx* tmp-var) (ctx-random-name ctx)])
          (unless (equal? curr-round new-round) 
            (printf "~a~a\n" indent (change-round-mode new-round indent)))
          (printf "~a~a\n" indent 
            (convert-declaration (ctx-update-props ctx* props) tmp-var (convert-expr body #:ctx (ctx-update-props ctx* props) #:indent indent)))
          (unless (equal? curr-round new-round)
            (printf "~a~a\n" indent (change-round-mode curr-round indent)))
          tmp-var)]
       [(and (equal? (language-name (*lang*)) "c")  ; Only C needs to emit a temporary variable for different precision
             (not (equal? curr-prec new-prec)))
        (let-values ([(ctx* tmp-var) (ctx-random-name ctx)])
          (printf "~a~a\n" indent 
              (convert-declaration (ctx-update-props ctx* props) tmp-var (convert-expr body #:ctx (ctx-update-props ctx* props) #:indent indent)))
          tmp-var)]
      [else
       (convert-expr body #:ctx (ctx-update-props ctx props) #:indent indent)])]

    [(list (? operator? operator) args ...)
      (define args_c
          (map (λ (arg) (convert-expr arg #:ctx ctx #:indent indent)) args))
     (convert-application ctx operator args_c)] 
    [(list digits m e b) (convert-constant ctx (digits->number m e b))]
    [(? constant?) (convert-constant ctx expr)]
    [(or (? number?) (? hex?)) (convert-constant ctx expr)]
    [(? symbol?) (ctx-lookup-name ctx expr)]))

(define (convert-core prog name)
  (parameterize ([*used-names* (mutable-set)]
                 [*gensym-collisions* 1]
                 [*gensym-fix-name* fix-name])
    (match-define (list 'FPCore (list args ...) props ... body) prog)
    (define default-ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
    (define ctx (ctx-reserve-names default-ctx (*reserved-names*)))

    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
        (set! ctx cx)
        fname))

    (define non-varnames 
      (for/list ([name (*reserved-names*)]) 
        (ctx-lookup-name ctx name)))

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

    (define indent
      (if (equal? (language-name (*lang*)) "scala")
          "\t\t"
          "\t"))

    (define-values (body-out return-out) 
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p])
          (define out (convert-expr body #:ctx ctx #:indent indent))
          (values (get-output-string p) out))))
    (convert-function func-name arg-names arg-props body-out return-out ctx (remove* non-varnames (set->list (*used-names*))))))