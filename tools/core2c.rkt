#lang racket

(require "common.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "$~a" (char->integer char))))
   ""))

(define (application->c type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a operator b)]
    [(list (or '== '!= '< '> '<= '>=))
     "TRUE"]
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
    [(list 'and a b)
     (format "(~a && ~a)" a b)]
    [(list 'or a b)
     (format "(~a || ~a)" a b)]
    [(list (? operator? f) args ...)
     (format "~a~a(~a)" f (type->suffix type) (string-join args ", "))]))

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"])

(define/match (type->suffix type)
  [('binary64) ""]
  [('binary32) "f"]
  [('binary80) "l"])

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->c expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~a~a ~a = ~a;\n" indent (type->c type) (fix-name var*)
               (expr->c val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->c body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->c cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~a~a ~a;\n" indent (type->c type) (fix-name outvar))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a;\n" (fix-name outvar)
             (expr->c ift #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" (fix-name outvar)
             (expr->c iff #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~a~a ~a = ~a;\n" indent (type->c type) (fix-name var*)
               (expr->c val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~aint ~a = ~a;\n" indent (fix-name test-var)
             (expr->c cond #:names names* #:type type #:indent indent))
     (printf "~awhile (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\t~a ~a = ~a;\n" indent (type->c type) (fix-name temp-var)
               (expr->c update #:names names* #:type type #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a;" (fix-name test-var)
             (expr->c cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->c retexpr #:names names* #:type type #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->c arg #:names names #:type type #:indent indent)) args))
     (application->c type operator args_c)]
    [(? constant?)
     (format "((~a) ~a)" (type->c type) expr)]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a~a" (real->double-flonum expr) (type->suffix type))]))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':type 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" (type->c type) (fix-name (if (list? var) (car var) var)))))
  (define c-body
    (with-output-to-string
      (λ ()
        (parameterize ([*names* (mutable-set args)])
          (printf "\treturn ~a;\n" (expr->c body #:type type))))))
  (format "~a ~a(~a) {\n~a}\n" (type->c type) (fix-name name) (string-join arg-strings ", ") c-body))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (printf "#include <math.h>\n\n")
   (for ([expr (in-port read (current-input-port))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
