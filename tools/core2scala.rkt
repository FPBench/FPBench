#lang racket

(require "common.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (application->scala type operator args)
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

(define/match (type->scala type)
  [('binary64) "Double"]
  [('binary32) "Float"])

(define/match (type->suffix type)
  [('binary64) ""]
  [('binary32) "f"])

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

(define (expr->scala expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~aval ~a : ~a = ~a\n" indent (fix-name var*) (type->scala type)
               (expr->scala val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->scala body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->scala cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~avar ~a : ~a\n" indent (fix-name outvar) (type->scala type))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->scala ift #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->scala iff #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~avar ~a : ~a = ~a\n" indent (fix-name var*) (type->scala type)
               (expr->scala val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~avar ~a : Boolean = ~a\n" indent (fix-name test-var)
             (expr->scala cond #:names names* #:type type #:indent indent))
     (printf "~awhile (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\tval ~a : ~a = ~a\n" indent (fix-name temp-var) (type->scala type)
               (expr->scala update #:names names* #:type type #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a" indent (fix-name test-var)
             (expr->scala cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->scala retexpr #:names names* #:type type #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->scala arg #:names names #:type type #:indent indent)) args))
     (application->scala type operator args_c)]
    [(? constant?)
     (format "(~a).to~a" expr (type->scala type))]
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
      (format "~a: ~a" (fix-name (if (list? var) (car var) var)) (type->scala type))))
  (with-output-to-string
    (λ ()
      (printf "object ~a {\n" (fix-name name))
      (printf "\tdef ~a(~a): ~a = {\n" (fix-name name) (string-join arg-strings ", ") (type->scala type))
      (parameterize ([*names* (apply mutable-set args)])
        (printf "\t\t~a;\n" (expr->scala body #:type type #:indent "\t\t")))
      (printf "\t}\n}\n"))))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (printf "import daisy.lang._\nimport Real._\n\n")
   (for ([expr (in-port read (current-input-port))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
