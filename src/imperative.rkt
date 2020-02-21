#lang racket

(require "common.rkt")
(provide convert-core *lang* language)

;;; Abstraction for different languages

(struct language (name operator constant declaration assignment round function))
(define *lang* (make-parameter #f))

(define (convert-operator ctx operator args)
  ((language-operator (*lang*)) ctx operator args))

(define (convert-constant ctx expr)
  ((language-constant (*lang*)) ctx expr))

(define (convert-declaration ctx var [val #f])
  (if (equal? ((language-name (*lang*))) "sollya")
    ((language-assignment (*lang*)) var val)
    ((language-declaration (*lang*)) ctx var val)))
    
(define (convert-assignment var val)
  ((language-assignment (*lang*)) var val))

(define (round-expr ctx val)
  ((language-round (*lang*)) ctx val))

(define (convert-function name args arg-ctx body return ctx vars)
  ((language-function (*lang*)) name args arg-ctx body return ctx vars))

(define (while-name) ; Go is weird
  (if (equal? ((language-name (*lang*))) "go") "for" "while"))

;;; Compiler for imperative languages

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (last (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define (convert-application ctx operator args)
  (match (cons operator args)
    [(list '- a) (round-expr (format "-~a" a) ctx)]
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

(define (convert-expr expr #:names [names #hash()] #:ctx [ctx #hash()] #:indent [indent "\t"]) ;; need default for context
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~a~a\n" indent
               (convert-declaration
                ctx
                (fix-name var*)
                (convert-expr val #:names names #:ctx ctx #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (convert-expr body #:names names* #:ctx ctx #:indent indent)]

    [`(let* ([,vars ,vals] ...) ,body)
     (define names*
       (for/fold ([names* names]) ([var vars] [val vals])
         (define var* (gensym var))
         (printf "~a~a\n" indent
                 (convert-declaration
                  ctx
                  (fix-name var*)
                  (convert-expr val #:names names* #:ctx ctx #:indent indent)))
         (dict-set names* var var*)))
     (convert-expr body #:names names* #:ctx ctx #:indent indent)]

    [`(if ,cond ,ift ,iff)
     (define test (convert-expr cond #:names names #:ctx ctx #:indent indent))
     (define outvar (gensym 'temp))
      ; Sollya has slightly different if
     (if (equal? ((language-name (*lang*))) "sollya")
         (printf "~aif (~a) then {\n" indent test)
         (printf "~a~a\n~aif (~a) {\n" indent (convert-declaration ctx (fix-name outvar)) indent test))
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name outvar)
              (convert-expr ift #:names names #:ctx ctx #:indent (format "~a\t" indent))))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name outvar)
              (convert-expr iff #:names names #:ctx ctx #:indent (format "~a\t" indent))))
     (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
     (fix-name outvar)]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~a~a\n" indent
               (convert-declaration
                ctx
                (fix-name var*)
                (convert-expr val #:names names #:ctx ctx #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~a~a\n" indent
             (convert-declaration
              (apply hash-set* ctx '(:precision boolean))
              (fix-name test-var)
              (convert-expr cond #:names names* #:ctx ctx #:indent indent)))
     ; Sollya has slightly different while
     (if (equal? ((language-name (*lang*))) "sollya")
         (printf "~awhile (~a) do {\n" indent (fix-name test-var))
         (printf "~a~a (~a) {\n" indent (while-name) (fix-name test-var)))
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\t~a\n" indent
               (convert-declaration
                ctx
                (fix-name temp-var)
                (convert-expr update #:names names* #:ctx ctx #:indent (format "~a\t" indent)))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a\n" indent (convert-assignment (fix-name var*) (fix-name temp-var))))
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name test-var)
              (convert-expr cond #:names names* #:ctx ctx #:indent (format "~a\t" indent))))
     (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
     (convert-expr retexpr #:names names* #:ctx ctx #:indent indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define-values (vars* names*)
       (for/fold ([vars* '()] [names* names]) ([var vars] [val inits])
         (define var* (gensym var))
         (printf "~a~a\n" indent
                 (convert-declaration
                  ctx
                  (fix-name var*)
                  (convert-expr val #:names names #:ctx ctx #:indent indent)))
         (values (cons var* vars*) (dict-set names* var var*))))
     (set! vars* (reverse vars*))
     (define test-var (gensym 'test))
     (printf "~a~a\n" indent
             (convert-declaration
              (apply hash-set* ctx '(:precision boolean))
              (fix-name test-var)
              (convert-expr cond #:names names* #:ctx ctx #:indent indent)))
     ; Sollya has slightly different while*
     (if (equal? ((language-name (*lang*))) "sollya")
         (printf "~awhile (~a) do {\n" indent (fix-name test-var))
         (printf "~a~a (~a) {\n" indent (while-name) (fix-name test-var)))
     (for ([var* vars*] [update updates])
       (printf "~a\t~a\n" indent
               (convert-assignment
                (fix-name var*)
                (convert-expr update #:names names* #:ctx ctx #:indent (format "~a\t" indent)))))
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name test-var)
              (convert-expr cond #:names names* #:ctx ctx #:indent (format "~a\t" indent))))
     (printf "~a}~a\n" indent (if (equal? ((language-name (*lang*))) "sollya") ";" ""))
     (convert-expr retexpr #:names names* #:ctx ctx #:indent indent)]

    [`(! ,props ... ,body)
     (convert-expr body #:names names #:ctx (apply hash-set* ctx props) #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:names names #:ctx ctx #:indent indent)) args))
     (convert-application ctx operator args_c)] 
    [(? constant?)
     (convert-constant ctx expr)]
    [(? number?)
     (convert-constant ctx expr)]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]))

(define (convert-core prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define ctx (apply hash-set* #hash() (append '(:precision binary64 :round nearestEven) props)))

  (define-values (arg-names arg-ctxs)
    (for/lists (l1 l2) ([var args])
      (match var
        [(list '! props ... name) (values name (apply hash-set* ctx props))]
        [name (values name ctx)])))

  (define-values (body-out return-out vars-used) 
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p] [*names* (apply mutable-set arg-names)])
        (define out (convert-expr body #:ctx ctx))
        (values (get-output-string p) out (set->list (*names*))))))

  (convert-function
   (fix-name name)
   (map fix-name arg-names)
   arg-ctxs
   body-out
   return-out
   ctx
   (map fix-name vars-used)))