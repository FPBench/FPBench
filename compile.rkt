#lang racket

(define (canonicalize-program prog)
  (match-define (list 'lambda (list args ...) props&body ...) prog)
  (let loop ([props&body props&body] [props '()])
    (match props&body
      [(list (? keyword? propname) propval rest ...)
       (loop rest (cons (cons propname propval) props))]
      [(list body rest ...)
       (values args body (append (reverse props) rest))])))

(define constants '(E PI))

(define (compile/gvn expr)
  (define index '())
  (define names (make-hash))

  (define (compile-one expr)
    (hash-update!
     names expr
     (λ (rec) (cons (+ 1 (car rec)) (cdr rec)))
     (λ ()
       (if (list? expr)
         (let ([expr* (cons (car expr) (map compile-one (cdr expr)))]
               [name (gensym "r")])
           (set! index (cons (list name expr*) index))
           (cons 0 name))
         (cons 0 expr))))
    (cdr (hash-ref names expr)))

  (let ([reg (compile-one expr)])
    (values reg names index)))

(define (program->cse expr #:break-at [break-at '(sqr)])
  (define-values (out-reg names index) (compile/gvn expr))
  (define index* '())

  (define (lookup reg)
    (second (assoc reg index)))

  (define (cse! expr spill?)
    (cond
     [(not (list? expr))
      expr]
     [else
      (match-define (cons count name) (hash-ref names expr))
      (define def (lookup name))
      (define def*
        (cons (car expr)
              (map (curryr cse! (member (car expr) break-at))
                   (cdr expr))))

      (if (or (> count 1) spill?)
          (begin
            (set! index* (cons (list name def) index*))
            name)
          def*)]))

  (let ([expr* (cse! expr #f)])
    `(let* ,(reverse (remove-duplicates index*)) ,expr*)))

(define (fix-name name)
  (string-replace (~a name) #rx"[^a-zA-Z0-9]" "_"))

(define (apply-converter conv args)
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply format (list-ref conv (length args)) args)]
   [(procedure? conv) (apply conv args)]
   [else (error "Unknown syntax entry" conv)]))

(define-syntax-rule (define-table name [key values ...] ...)
  (define name
    (let ([hash (make-hasheq)])
      (for ([rec (list (list 'key values ...) ...)])
        (hash-set! hash (car rec) (cdr rec)))
      hash)))

(define-table operators->c
  [+        "(~a + ~a)"]
  [-        '(#f "-(~a)" "(~a - ~a)")]
  [*        "(~a * ~a)"]
  [/        '(#f "(1.0/~a)" "(~a / ~a)")]
  [abs      "fabs(~a)"]
  [sqrt     "sqrt(~a)"]
  [hypot    "hypot(~a, ~a)"]
  [sqr      (λ (x) (format "(~a * ~a)" x x))]
  [exp      "exp(~a)"]
  [expm1    "expm1(~a)"]
  [expt     "pow(~a, ~a)"]
  [log      "log(~a)"]
  [log1p    "log1p(~a)"]
  [sin      "sin(~a)"]
  [cos      "cos(~a)"]
  [tan      "tan(~a)"]
  [cotan    "(1.0 / tan(~a))"]
  [asin     "asin(~a)"]
  [acos     "acos(~a)"]
  [atan     "atan(~a)"]
  [sinh     "sinh(~a)"]
  [cosh     "cosh(~a)"]
  [tanh     "tanh(~a)"]
  [atan2    "atan2(~a, ~a)"]
  [if       "(~a ? ~a : ~a)"]
  [>        "(~a > ~a)"]
  [<        "(~a < ~a)"]
  [<=       "(~a <= ~a)"]
  [>=       "(~a >= ~a)"]
  [and      "(~a && ~a)"]
  [or       "(~a || ~a)"]
  [mod      "fmod2(~a, ~a)"])

(define-table constants->c
  [PI    "atan2(1.0, 0.0)"]
  [E     "exp(1.0)"])

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (if->c expr)
  (match-define `(if ,cond ,ift ,iff) expr)
  (format "(~a ? ~a : ~a)" (expr->c cond) (expr->c ift) (expr->c iff)))

(define (value->c expr)
  (cond
   [(member expr constants) (apply-converter (car (hash-ref constants->c expr)) '())]
   [(symbol? expr) (fix-name expr)]
   [(number? expr) (real->double-flonum expr)]
   [else (error "Invalid value" expr)]))

(define (app->c expr)
  (if (list? expr)
      (let* ([rec (list-ref (hash-ref operators->c (car expr)) 0)]
             [args (map app->c (cdr expr))])
        (apply-converter rec args))
      (value->c expr)))

(define (expr->c expr)
  (match expr
    [`(if ,cond ,ift ,iff) (if->c expr)]
    [_ (app->c expr)]))

(define (program->c prog [type "double"] [fname "f"])
  (define-values (args body props) (canonicalize-program prog))

  (match-define `(let* ([,vars ,vals] ...) ,retexpr) (program->cse body))
  (printf "double ~a(~a) {\n" fname
          (string-join (for/list ([var args])
                         (format "~a ~a" type (fix-name var)))
                       ", "))
  
  (for ([var vars] [val vals])
    (define type* (if (comparison? (car val)) 'bool type))
    (printf "        ~a ~a = ~a;\n" type* var (app->c val)))
  (printf "        return ~a;\n" (app->c retexpr))
  (printf "}\n\n"))

(module+ main
  (require racket/cmdline)
  
  (command-line
   #:program "compile.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (program->c expr))))
