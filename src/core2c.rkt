#lang racket

(require "common.rkt" "fpcore.rkt" "compilers.rkt")
(provide c-header core->c go-header core->go)

;;; Abstraction for different languages

(struct language (header type operator constant declaration assignment function))
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
  (if (equal? (*lang*) go-language) "for" "while"))

;; C

(define c-header (const "#include <math.h>\n#define TRUE 1\n#define FALSE 0\n\n"))

(define/match (type->c-suffix type)
  [('binary64) ""]
  [('binary32) "f"]
  [('binary80) "l"])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"])

(define (operator->c type operator)
  (format "~a~a" operator (type->c-suffix type)))

(define (constant->c type expr)
  (match expr
    [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'TRUE 'FALSE 'INFINITY 'NAN)
     (format "((~a) ~a)" type expr)]
    [(? symbol?) (format "((~a) M_~a)" type expr)]
    [(? number?)
     (format "~a~a" (real->double-flonum expr) (type->c-suffix type))]))

(define (declaration->c type var [val #f])
  (if val
      (format "~a ~a = ~a;" type var val)
      (format "~a ~a;" type var)))

(define (assignment->c var val)
  (format "~a = ~a;" var val))

(define (function->c type name args body return)
  (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name
          (string-join
           (map (λ (arg) (format "~a ~a" type arg)) args)
           ", ")
          body return))

(define c-language (language c-header type->c operator->c constant->c declaration->c assignment->c function->c))

;; Go

(define go-header (curry format "package ~a\n\nimport \"math\"\n\n"))

(define/match (type->go type)
  [('binary64) "float64"]
  [('binary32) "float32"]
  [('boolean) "bool"])

(define (operator->go type operator)
  (match operator
    [(or 'fabs 'fmax 'fmin 'fdim)
     (format "math.~a" (string-titlecase (substring (~a operator) 1)))]
    [(or 'isinf 'isnan)
     (format "math.Is~a" (string-titlecase (substring (~a operator) 2)))]
    [(or 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'pow 'sqrt 'cbrt 'hypot
         'sin 'cos 'tan 'asin 'cos 'atan 'atan2 'sinh 'cosh 'tanh
         'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
         'remainder 'copysign 'trunc 'round)
     (format "math.~a" (string-titlecase (~a operator)))]))

(define (constant->go type expr)
  (match expr
    ['TRUE "true"]
    ['FALSE "false"]
    [(? symbol?)
     (define name
       (match expr
         ['E "E"] ['LOG2E "Log2E"] ['LOG10E "Log10E"] ['LN2 "Ln2"] ['LN10 "Ln10"]
         ['PI "Pi"] ['PI_2 "Pi/2"] ['PI_4 "Pi/4"] ['SQRT2 "Sqrt2"]
         ['MAXFLOAT "MaxFloat64"] ['INFINITY "Inf(1)"] ['NAN "Nan()"]
         [_ (error 'constant->go "Unsupported constant ~a" expr)]))
     (format "((~a) Math.~a)" (type->go type) name)]
    [(? number?) (~a (real->double-flonum expr))]))

(define (declaration->go type var [val #f])
  (if val
      (format "var ~a = ~a(~a)" var type val)
      (format "var ~a ~a" var type)))

(define (assignment->go var val)
  (format "~a = ~a" var val))

(define (function->go type name args body return)
  (format "func ~a(~a) ~a {\n~a\treturn ~a;\n}\n"
          name
          (string-join
           (map (λ (arg) (format "~a ~a" arg type)) args)
           ", ")
          type
          body return))

(define go-language (language go-header type->go operator->go constant->go declaration->go assignment->go function->go))

;;; Compilation portion

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
    [(list 'and a ...)
     (format "(~a)" (string-join (map ~a a) " && "))]
    [(list 'or a ...)
     (format "(~a)" (string-join (map ~a a) " || "))]
    [(list (? operator? f) args ...)
     (format "~a(~a)" (convert-operator type operator) (string-join args ", "))]))

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
     (convert-constant type expr)]
    [(? number?)
     (convert-constant type expr)]
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

;;; Exports

(define (core->c  prog name) (parameterize ([*lang*  c-language]) (convert-core prog name)))
(define (core->go prog name) (parameterize ([*lang* go-language]) (convert-core prog name)))

(define-compiler '("c")
  c-header core->c (const "")
  '(!))

(define-compiler '("go")
  go-header core->go (const "")
  '(!))
