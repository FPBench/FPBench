#lang racket

(require "common.rkt")

(define (attribute? symb)
  (and (symbol? symb) (string-prefix? (symbol->string symb) ":")))

(define (canonicalize-program prog)
  (match-define (list 'lambda (list args ...) props&body ...) prog)
  (let loop ([props&body props&body] [props '()])
    (match props&body
      [(list (? attribute? propname) propval rest ...)
       (loop rest (cons (cons propname propval) props))]
      [(list body rest ...)
       (values args body (append (reverse props) rest))])))

(define (fix-name name)
  (string-replace (~a name) #rx"[^a-zA-Z0-9]" "_"))

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

(define/match (typeof expr #:fptype [fptype 'binary64])
  [((list (or '> '< '>= '<= 'and 'or) _ ...) ftype) 'bool]
  [(_ fptype) (type->c fptype)])

(define/match (type->c type)
  [('binary64) 'double]
  [('binary32) 'float]
  [('binary80) '|long double|])

(define/match (type->suffix type)
  [('binary64) ""]
  [('binary32) "f"]
  [('binary80) "l"])

(define/match (value->c expr #:type [type 'binary64])
  [('E _) "exp(1.0)"]
  [('PI _) "atan2(1.0, 0.0)"]
  [((? symbol?) _) (fix-name expr)]
  [((? number?) 'binary32) (format "~af" (real->double-flonum expr))]
  [((? number?) 'binary64) (format "~a" (real->double-flonum expr))])

(define/match (if->c expr #:type [type 'binary64])
  [(`(if ,cond ,ift ,iff) _)
   (format "(~a ? ~a : ~a)" (expr->c cond #:type type) (expr->c ift #:type type) (expr->c iff #:type type))])

(define/match (expr->c expr #:type [type 'binary64])
  [(`(if ,cond ,ift ,iff) _) (if->c expr #:type type)]
  [((? list?) _) (apply application->c type (car expr) (map (λ (x) (expr->c x #:type type)) (cdr expr)))]
  [(_ _) (value->c expr #:type type)])

(define (function->c args body #:type [type 'binary64] #:name [name 'f])
  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" (type->c type) (fix-name (if (list? var) (car var) var)))))
  (format "~a ~a(~a) {\n~a}\n"
          (type->c type) name (string-join arg-strings ", ")
          (with-output-to-string (λ () (program->c body #:type type)))))

(define (program->c body #:type [type 'binary64])
  (match body
    [`(let ([,vars ,vals] ...) ,retexpr)
     (for ([var vars] [val vals])
       (printf "\t~a next_~a = ~a;\n" (typeof val #:fptype type) var (expr->c val #:type type)))
     (for ([var vars] [val vals])
       (printf "\t~a ~a = next_~a;\n" (typeof val #:fptype type) var var))
     (program->c retexpr #:type type)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (for ([var vars] [init inits])
       (printf "\t~a ~a = ~a;\n" (typeof init #:fptype type) var (expr->c init #:type type)))
     (printf "\twhile (~a) {\n" (expr->c cond #:type type))
     (for ([var vars] [update updates])
       (printf "\t\t~a next_~a = ~a;\n" (typeof update #:fptype type) var (expr->c update #:type type)))
     (for ([var vars])
       (printf "\t\t~a = next_~a;\n" var var))
     (printf "\t}\n")
     (program->c retexpr #:type type)]
    [_
     (printf "\treturn ~a;\n" (expr->c body #:type type))]))

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
