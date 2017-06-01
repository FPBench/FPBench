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

(define/match (application->c type . args)
  [(type (list '+ a b))     (format "(~a + ~a)" a b)]
  [(type (list '- a))       (format "-(~a)" a)]
  [(type (list '- a b))     (format "(~a - ~a)" a b)]
  [(type (list '* a b))     (format "(~a * ~a)" a b)]
  [(type (list '/ a b))     (format "(~a / ~a)" a b)]
  [(type (list 'sqr a))     (format "(~a * ~a)" a a)]
  [(type (list 'abs a))     (format "fabs~a(~a)" (type->suffix type) a)]

  [(type (list '> a b))     (format "(~a > ~a)" a b)]
  [(type (list '< a b))     (format "(~a < ~a)" a b)]
  [(type (list '== a b))    (format "(~a == ~a)" a b)]
  [(type (list '<= a b))    (format "(~a <= ~a)" a b)]
  [(type (list '>= a b))    (format "(~a >= ~a)" a b)]
  [(type (list 'and a b))   (format "(~a && ~a)" a b)]
  [(type (list 'or a b))    (format "(~a || ~a)" a b)]
  [(type (list 'mod a b))   (format "fmod2~a(~a, ~a)" (type->suffix type) a b)]

  [(type (list (and (or 'sqrt 'hypot 'exp 'expm1 'pow 'log 'log1p 'sin 'cos 'tan 'asin 'acos 'atan 'sinh 'cosh 'tanh 'atan2) f) args ...))
   (format "~a~a(~a)" f (type->suffix type) (string-join args ", "))])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"])

(define/match (type->suffix type)
  [('binary64) ""]
  [('binary32) "f"]
  [('binary80) "l"])

(define *names* (make-parameter (make-hash)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (dict-keys (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (dict-set! (*names*) name* #t)
  name*)

(define/match (expr->c expr #:names [names #hash()] #:to [destination #f] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  [(`(let ([,vars ,vals] ...) ,body) _ _ _ _)
   (define vars* (map gensym vars))
   (for ([var vars] [var* vars*] [val vals])
     (printf "~a~a ~a;\n" indent (type->c type) (fix-name var*))
     (expr->c val #:names names #:to var* #:type type #:indent indent))
   (define names*
     (for/fold ([names* names]) ([var vars] [var* vars*])
       (dict-set names* var var*)))
   (expr->c body #:names names* #:to destination #:type type #:indent indent)]
  [(`(if ,cond ,ift ,iff) _ _ _ _)
   (define test (expr->c cond #:names names #:to #f #:type type #:indent indent))
   (define outvar (or destination (gensym 'temp)))
   (unless destination ; otherwise already defined
     (printf "~a~a ~a;\n" indent (type->c type) (fix-name outvar)))
   (printf "~aif (~a) {\n" indent test)
   (expr->c ift #:names names #:to outvar #:type type #:indent (format "~a\t" indent))
   (printf "~a} else {\n" indent)
   (expr->c iff #:names names #:to outvar #:type type #:indent (format "~a\t" indent))
   (printf "~a}\n" indent)
   (fix-name outvar)]
  [(`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr) _ _ _ _)
   (define vars* (map gensym vars))
   (for ([var vars] [var* vars*] [val inits])
     (printf "~a~a ~a;\n" indent (type->c type) (fix-name var*))
     (expr->c val #:names names #:to var* #:type type #:indent indent))
   (define names*
     (for/fold ([names* names]) ([var vars] [var* vars*])
       (dict-set names* var var*)))
   (define test-var (gensym 'test))
   (printf "~aint ~a;\n" indent (fix-name test-var))
   (expr->c cond #:names names* #:to test-var #:type type #:indent indent)
   (printf "~awhile (~a) {\n" indent test-var)
   (define temp-vars (map gensym vars))
   (for ([temp-var temp-vars] [update updates])
     (printf "~a\t~a ~a;\n" indent (type->c type) (fix-name temp-var))
     (expr->c update #:names names* #:to temp-var #:type type #:indent (format "~a\t" indent)))
   (for ([var* vars*] [temp-var temp-vars])
     (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
   (expr->c cond #:names names* #:to test-var #:type type #:indent (format "~a\t" indent))
   (printf "~a}\n" indent)
   (expr->c retexpr #:names names* #:to destination #:type type #:indent indent)]
  [((list operator args ...) _ _ _ _)
   (define args_c
     (map (λ (arg) (expr->c arg #:names names #:to #f #:type type #:indent indent)) args))
   (define out (apply application->c type operator args_c))
   (when destination
     (printf "~a~a = ~a;\n" indent (fix-name destination) out))
   out]
  [((? symbol?) _ _ _ _)
   (define out (fix-name (dict-ref names expr expr)))
   (when destination
     (printf "~a~a = ~a;\n" indent (fix-name destination) out))
   out]
  [((? number?) _ _ _ _)
   (define out (format "~a~a" (real->double-flonum expr) (type->suffix type)))
   (when destination
     (printf "~a~a = ~a;\n" indent (fix-name destination) out))
   out])

(define (function->c args body #:type [type 'binary64] #:name [name 'f])
  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" (type->c type) (fix-name (if (list? var) (car var) var)))))
  (format "~a ~a(~a) {\n~a}\n"
          (type->c type) name (string-join arg-strings ", ")
          (with-output-to-string
            (λ ()
              (define hash (make-hash))
              (for ([var args]) (dict-set! hash var #t))
              (define out 
                (parameterize ([*names* hash])
                  (expr->c body #:type type)))
              (printf "\treturn ~a;\n" out)))))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (function->c args body #:type (dict-ref properties ':type 'binary64) #:name name))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (printf "#include <math.h>\n\n")
   (for ([expr (in-port read (current-input-port))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
