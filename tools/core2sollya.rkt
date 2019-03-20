#lang racket

(require "common.rkt" "fpcore.rkt")
(provide core->sollya)

;; sollya identifiers have the same rules as in C
(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

;; this does not insert any rounding code
(define (application->sollya operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a operator b)]
    [(list 'pow a b)
     (format "(~a ^ ~a)" a b)]
    [(list (or '== '!= '< '> '<= '>=))
     "true"]
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
    ;; TODO some operators will have to be renamed or emulated
    [(list (? operator? f) args ...)
     (format "~a(~a)" f (string-join args ", "))]))

(define (precision->sollya type)
  (match type
    ['binary16 "halfprecision"]
    ['binary32 "single"]
    ['binary64 "double"]
    ['binary80 "doubleextended"]
    ['binary128 "quad"]
    ;; this will round to the right amount of precision, but not limit the exponent
    [(list 'float w p) (~a p)]
    ;; real is just a keyword that tells us to omit rounding
    ['real "real"]
    ;; hopefully integers have been selected to behave like reals
    ['integer "real"]
    ;; don't bother rounding anything else
    [_ "real"]))

(define (constant->sollya expr)
  (match expr
    ['E "exp(1)"]
    ['LOG2E "log2(exp(1))"]
    ['LOG10E "log10(exp(1))"]
    ['LN2 "log(2)"]
    ['LN10 "log(10)"]
    ['PI "pi"]
    ['PI_2 "(pi/2)"]
    ['PI_4 "(pi/4)"]
    ['M_1_PI "(1/pi)"]
    ['M_2_PI "(2/pi)"]
    ['M_2_SQRTPI "(2/sqrt(pi))"]
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "sqrt(1/2)"]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY "infty"]
    ['NAN "nan"]
    [_ (error 'constant->smt "Unsupported constant ~a" expr)]))


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

(define (expr->sollya expr #:names [names #hash()] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~a~a = ~a;\n" indent (fix-name var*)
               (expr->sollya val #:names names #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->sollya body #:names names* #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->sollya cond #:names names #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~aif (~a) then {\n" indent test)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->sollya ift #:names names #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->sollya iff #:names names #:indent (format "~a\t" indent)))
     (printf "~a};\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~a~a = ~a;\n" indent (fix-name var*)
               (expr->sollya val #:names names #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~a~a = ~a;\n" indent (fix-name test-var)
             (expr->sollya cond #:names names* #:indent indent))
     (printf "~awhile (~a) do {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\t~a = ~a;\n" indent (fix-name temp-var)
               (expr->sollya update #:names names* #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a;" indent (fix-name test-var)
             (expr->sollya cond #:names names* #:indent (format "~a\t" indent)))
     (printf "~a};\n" indent)
     (expr->sollya retexpr #:names names* #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->sollya arg #:names names #:indent indent)) args))
     (application->sollya operator args_c)]
    [(? constant?)
     (format "~a" (constant->sollya expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a" expr)]))

(define (core->sollya prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  ;; (define type (dict-ref properties ':precision 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a" (fix-name (if (list? var) (car var) var)))))
  (define-values (sollya-body sollya-names)
    (parameterize ([*names* (apply mutable-set args)])
      (values
       (with-output-to-string
         (λ () (printf "\t~a;\n" (expr->sollya body))))
       (*names*))))
  (define sollya-vars
    (set-subtract
     (for/list ([name sollya-names])
       (format "~a" (fix-name name)))
     arg-strings))

  (define var-string
    (if (> (length sollya-vars) 0)
        (format "\n\tvar ~a;" (string-join sollya-vars ", "))
        ""))

  (format "procedure ~a(~a) {~a\n~a};\n"
          (fix-name name)
          (string-join arg-strings ", ")
          var-string
          sollya-body))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (core->sollya expr #:name (format "ex~a" n))))))
