#lang racket

(require "common.rkt" "fpcore.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (application->scala operator args)
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
    [(list 'and as ...)
     (format "(~a)" (string-join as " && "))]
    [(list 'or as ...)
     (format "(~a)" (string-join as " || "))]
    [(list (? operator? f) args ...)
     (format "~a(~a)" f (string-join args ", "))]))

(define/match (type->scala type)
  [('binary64) "Double"]
  [('binary32) "Float"])

(define *names* (make-parameter (mutable-set)))

(define (dict-remove-many bindings vars)
  (for/fold ([bindings bindings]) ([var vars])
    (dict-remove bindings var)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->scala expr #:names [names #hash()] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~aval ~a : Real = ~a\n" indent (fix-name var*)
               (expr->scala val #:names names #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->scala body #:names names* #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->scala cond #:names names #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~avar ~a : Real\n" indent (fix-name outvar))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->scala ift #:names names #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a\n" indent (fix-name outvar)
             (expr->scala iff #:names names #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~avar ~a : Real = ~a\n" indent (fix-name var*)
               (expr->scala val #:names names #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~avar ~a : Boolean = ~a\n" indent (fix-name test-var)
             (expr->scala cond #:names names* #:indent indent))
     (printf "~awhile (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\tval ~a : Real = ~a\n" indent (fix-name temp-var)
               (expr->scala update #:names names* #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a\n" indent (fix-name test-var)
             (expr->scala cond #:names names* #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->scala retexpr #:names names* #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->scala arg #:names names #:indent indent)) args))
     (application->scala operator args_c)]
    [(? constant?)
     (format "~a" expr)]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a" (real->double-flonum expr))]))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))

  (define arg-strings
    (for/list ([var args])
      (format "~a: Real" (fix-name (if (list? var) (car var) var)))))
  (with-output-to-string
    (λ ()
      (printf "\tdef ~a(~a): Real = {\n" (fix-name name) (string-join arg-strings ", "))
      (parameterize ([*names* (apply mutable-set args)])
        (when (dict-has-key? properties ':pre)
          (printf "\t\trequire(~a)\n" (expr->scala (dict-ref properties ':pre) #:indent "\t\t")))
        (printf "\t\t~a;\n" (expr->scala body #:indent "\t\t")))
      (printf "\t}\n"))))

(define (unroll-loops expr n)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     `(let (,@(for/list ([var vars] [val vals]) (list var (unroll-loops vals n))))
        ,(unroll-loops body n))]
    [`(if ,cond ,ift ,iff)
     `(if ,(unroll-loops cond n) ,(unroll-loops ift n) ,(unroll-loops iff n))]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     `(let (,@(map list vars inits))
        ,(if (= n 0)
             retexpr
             (unroll-loops `(while ,cond (,@(map list vars updates updates)) ,retexpr)
                           (- n 1))))]
    [`(,(? operator? op) ,args ...)
     (cons op (map (curryr unroll-loops n) args))]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]))

(module+ main
  (require racket/cmdline)
  (define unroll #f)

  (command-line
   #:program "compile.rkt"
   #:once-each
   ["--unroll" n "How many iterations to unroll any loops to"
    (set! unroll (string->number n))]
   #:args ()
   (printf "import daisy.lang._\nimport Real._\n\n")
   (printf "object fpcore {\n")
   (for ([prog (in-port read (current-input-port))] [n (in-naturals)])
     (when unroll
       (match-define (list 'FPCore (list args ...) props ... body) prog)
       (set! prog `(FPCore ,args ,@props ,(unroll-loops body unroll))))
     (printf "~a\n" (compile-program prog #:name (format "ex~a" n))))
   (printf "}\n")))
