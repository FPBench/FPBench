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

(define/match (application->c . args)
  [((list '+ a b))     (format "(~a + ~a)" a b)]
  [((list '- a))       (format "-(~a)" a)]
  [((list '- a b))     (format "(~a - ~a)" a b)]
  [((list '* a b))     (format "(~a * ~a)" a b)]
  [((list '/ a b))     (format "(~a / ~a)" a b)]
  [((list 'abs a))     (format "fabs(~a)" a)]
  [((list 'sqr a))     (format "(~a * ~a)" a a)]
  [((list 'sqrt a))    (format "sqrt(~a)" a)]
  [((list 'hypot a b)) (format "hypot(~a, ~a)" a b)]
  [((list 'exp a))     (format "exp(~a)" a)]
  [((list 'expm1 a))   (format "expm1(~a)" a)]
  [((list 'pow a b))   (format "pow(~a, ~a)" a b)]
  [((list 'log a))     (format "log(~a)" a)]
  [((list 'log1p a))   (format "log1p(~a)" a)]
  [((list 'sin a))     (format "sin(~a)" a)]
  [((list 'cos a))     (format "cos(~a)" a)]
  [((list 'tan a))     (format "tan(~a)" a)]
  [((list 'cotan a))   (format "(1.0 / tan(~a))" a)]
  [((list 'asin a))    (format "asin(~a)" a)]
  [((list 'acos a))    (format "acos(~a)" a)]
  [((list 'atan a))    (format "atan(~a)" a)]
  [((list 'sinh a))    (format "sinh(~a)" a)]
  [((list 'cosh a))    (format "cosh(~a)" a)]
  [((list 'tanh a))    (format "tanh(~a)" a)]
  [((list 'atan2 a b)) (format "atan2(~a, ~a)" a b)]
  [((list '> a b))     (format "(~a > ~a)" a b)]
  [((list '< a b))     (format "(~a < ~a)" a b)]
  [((list '== a b))    (format "(~a == ~a)" a b)]
  [((list '<= a b))    (format "(~a <= ~a)" a b)]
  [((list '>= a b))    (format "(~a >= ~a)" a b)]
  [((list 'and a b))   (format "(~a && ~a)" a b)]
  [((list 'or a b))    (format "(~a || ~a)" a b)]
  [((list 'mod a b))   (format "fmod2(~a, ~a)" a b)])

(define/match (typeof expr #:fptype [fptype 'double])
  [((list (or '> '< '>= '<= 'and 'or) _ ...) ftype) 'bool]
  [(_ ftype) fptype])

(define/match (value->c expr #:type [type 'double])
  [('E _) "exp(1.0)"]
  [('PI _) "atan2(1.0, 0.0)"]
  [((? symbol?) _) (fix-name expr)]
  [((? number?) 'float) (format "~af" (real->double-flonum expr))]
  [((? number?) 'double) (format "~a" (real->double-flonum expr))])

(define/match (if->c expr #:type [type 'double])
  [(`(if ,cond ,ift ,iff) _)
   (format "(~a ? ~a : ~a)" (expr->c cond #:type type) (expr->c ift #:type type) (expr->c iff #:type type))])

(define/match (expr->c expr #:type [type 'double])
  [(`(if ,cond ,ift ,iff) _) (if->c expr #:type type)]
  [((? list?) _) (apply application->c (car expr) (map (λ (x) (expr->c x #:type type)) (cdr expr)))]
  [(_ _) (value->c expr #:type type)])

(define (function->c args body #:type [type 'double] #:name [name 'f])
  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" type (fix-name (if (list? var) (car var) var)))))
  (format "~a ~a(~a) {\n~a}\n"
          type name (string-join arg-strings ", ")
          (with-output-to-string (λ () (program->c body #:type type)))))

(define (program->c body #:type [type 'double])
  (match body
    [`(let ([,vars ,vals] ...) ,retexpr)
     (for ([var vars] [val vals])
       (printf "\t~a next_~a = ~a;\n" (typeof val #:fptype type) var (expr->c val #:type type)))
     (for ([var vars])
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
  (match-define (list 'lambda (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (function->c args body #:type (dict-ref properties ':type 'double) #:name name))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
