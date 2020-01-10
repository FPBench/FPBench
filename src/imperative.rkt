#lang racket

(require "common.rkt")
(provide language convert-core *lang*)

;;; Abstraction for different languages

(struct language (name header type operator constant declaration assignment function))
(define *lang* (make-parameter #f))

(define (convert-operator type operator)
  ((language-operator (*lang*)) type operator))

(define (convert-constant type expr)
  ((language-constant (*lang*)) type expr))

(define (convert-type type)
  ((language-type (*lang*)) type))

(define (convert-declaration type var [val #f])
  ((language-declaration (*lang*)) type var val))

(define (convert-assignment var val)
  ((language-assignment (*lang*)) var val))

(define (convert-function type name args body return)
  ((language-function (*lang*)) type name args body return))

(define (while-name) ; Go is weird
  (if (equal? ((language-name (*lang*))) "go") "for" "while"))

;;; Compiler for imperative languages (C, Go, etc.)

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

(define (convert-application type operator args)
  (match (cons operator args)
    [(list '- a) (format "-~a" a)]
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) "TRUE"]
    [(list (or '+ '- '* '/) a b) (format "(~a ~a ~a)" a operator b)]
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
     (if (equal? ((language-name (*lang*))) "js") ; JS is also weird: isinf(x)
       (apply format (convert-operator "" operator) args)
       (format (convert-operator (convert-type type) operator) (string-join args ", ")))]))

(define (convert-expr expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~a~a\n" indent
               (convert-declaration
                (convert-type type)
                (fix-name var*)
                (convert-expr val #:names names #:type type #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (convert-expr body #:names names* #:type type #:indent indent)]

    [`(let* ([,vars ,vals] ...) ,body)
     (define names*
       (for/fold ([names* names]) ([var vars] [val vals])
         (define var* (gensym var))
         (printf "~a~a\n" indent
                 (convert-declaration
                  (convert-type type)
                  (fix-name var*)
                  (convert-expr val #:names names* #:type type #:indent indent)))
         (dict-set names* var var*)))
     (convert-expr body #:names names* #:type type #:indent indent)]

    [`(if ,cond ,ift ,iff)
     (define test (convert-expr cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~a~a\n" indent (convert-declaration (convert-type type) (fix-name outvar)))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name outvar)
              (convert-expr ift #:names names #:type type #:indent (format "~a\t" indent))))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name outvar)
              (convert-expr iff #:names names #:type type #:indent (format "~a\t" indent))))
     (printf "~a}\n" indent)
     (fix-name outvar)]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~a~a\n" indent
               (convert-declaration
                (convert-type type)
                (fix-name var*)
                (convert-expr val #:names names #:type type #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~a~a\n" indent
             (convert-declaration
              (convert-type 'boolean)
              (fix-name test-var)
              (convert-expr cond #:names names* #:type type #:indent indent)))
     (printf "~a~a (~a) {\n" indent (while-name) (fix-name test-var)) ;; TODO need `for` in Go mode
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\t~a\n" indent
               (convert-declaration
                (convert-type type)
                (fix-name temp-var)
                (convert-expr update #:names names* #:type type #:indent (format "~a\t" indent)))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a\n" indent (convert-assignment (fix-name var*) (fix-name temp-var))))
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name test-var)
              (convert-expr cond #:names names* #:type type #:indent (format "~a\t" indent))))
     (printf "~a}\n" indent)
     (convert-expr retexpr #:names names* #:type type #:indent indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define-values (vars* names*)
       (for/fold ([vars* '()] [names* names]) ([var vars] [val inits])
         (define var* (gensym var))
         (printf "~a~a\n" indent
                 (convert-declaration
                  (convert-type type) (fix-name var*)
                  (convert-expr val #:names names #:type type #:indent indent)))
         (values (cons var* vars*) (dict-set names* var var*))))
     (set! vars* (reverse vars*))
     (define test-var (gensym 'test))
     (printf "~a~a\n" indent
             (convert-declaration
              (convert-type 'boolean)
              (fix-name test-var)
              (convert-expr cond #:names names* #:type type #:indent indent)))
     (printf "~a~a (~a) {\n" indent (while-name) (fix-name test-var))
     (for ([var* vars*] [update updates])
       (printf "~a\t~a\n" indent
               (convert-assignment
                (fix-name var*)
                (convert-expr update #:names names* #:type type #:indent (format "~a\t" indent)))))
     (printf "~a\t~a\n" indent
             (convert-assignment
              (fix-name test-var)
              (convert-expr cond #:names names* #:type type #:indent (format "~a\t" indent))))
     (printf "~a}\n" indent)
     (convert-expr retexpr #:names names* #:type type #:indent indent)]

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (convert-expr arg #:names names #:type type #:indent indent)) args))
     (convert-application type operator args_c)]
    [(? constant?)
     (convert-constant (convert-type type) expr)]
    [(? number?)
     (convert-constant (convert-type type) expr)]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]))

(define (convert-core prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))

  (define-values (body-out return-out)
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p] [*names* (apply mutable-set args)])
        (define out (convert-expr body #:type type))
        (values (get-output-string p) out))))

  (convert-function
   (convert-type type)
   (fix-name name)
   (map fix-name args)
   body-out
   return-out))