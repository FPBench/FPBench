#lang racket

(require "common.rkt" "fpcore.rkt")
(provide core->js export-js)

(define *runtime* (make-parameter #f))

(define (fix-name name)
  (string-join
    (for/list ([char (~a name)])
      (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
          (string char)
          (format "_~a_" (char->integer char))))
    ""))

(define/match (constant->js c)
  [('E) "Math.E"]
  [('LOG2E) "Math.LOG2E"]
  [('LOG10E) "Math.LOG10E"]
  [('LN2) "Math.LN2"]
  [('LN10) "Math.LN10"]
  [('PI) "Math.PI"]
  [('PI_2) "(Math.PI/2)"]
  [('PI_4) "(Math.PI/4)"]
  [('M_1_PI) "(1/Math.PI)"]
  [('M_2_PI) "(2/Math.PI)"]
  [('M_2_SQRTPI) "(2/Math.sqrt(Math.PI))"]
  [('SQRT2) "Math.SQRT2"]
  [('SQRT1_2) "(Math.SQRT1_2)"]
  [('MAXFLOAT) "Number.MAX_VALUE"]
  [('TRUE) "true"]
  [('FALSE) "false"]
  [('INFINITY) "Infinity"]
  [('NAN) "NaN"]
  [(_) (error 'constant->js "Unsupported constant ~a" c)])

(define/match (operator->js op)
  [((or '== '+ '- '* '/  '< '> '<= '>=)) (format "(~a ~a ~a)" "~a" op "~a")]
  [('and) "~a && ~a"]
  [('or) "~a || ~a"]
  [('not) "!~a"]
  [('fabs) "Math.abs(~a)"]
  [('exp) "Math.exp(~a)"]
  ;[('exp2) "math.pow(2, ~a)"]
  [('expm1) "Math.expm1(~a)"]
  [('log) "Math.log(~a)"]
  [('log10) "Math.log10(~a)"]
  [('log2) "Math.log2(~a)"]
  [('log1p) "Math.log1p(~a)"]
  ;[('logb) "math.floor(math.log2(math.abs(~a)))"]
  [('pow) "Math.pow(~a, ~a)"]
  [('sqrt) "Math.sqrt(~a)"]
  [('cbrt) "Math.cbrt(~a)"]
  [('hypot) "Math.hypot(~a, ~a)"]
  [('sin) "Math.sin(~a)"]
  [('cos) "Math.cos(~a)"]
  [('tan) "Math.tan(~a)"]
  [('asin) "Math.asin(~a)"]
  [('acos) "Math.acos(~a)"]
  [('atan) "Math.atan(~a)"]
  [('atan2) "Math.atan2(~a, ~a)"]
  [('sinh) "Math.sinh(~a)"]
  [('cosh) "Math.cosh(~a)"]
  [('tanh) "Math.tanh(~a)"]
  [('asinh) "Math.asinh(~a)"]
  [('acosh) "Math.acosh(~a)"]
  [('atanh) "Math.atanh(~a)"]
  ;[('erf) "math.erf(~a)"]
  ;[('erfc) "1 - math.erf(~a)"] ;; TODO: This implementation has large error for large inputs
  ;[('tgamma) "math.gamma(~a)"]
  ;[('lgamma) "math.log(math.gamma(~a))"]
  [('ceil) "Math.ceil(~a)"]
  [('floor) "Math.floor(~a)"]
  ;[('remainder) "math.mod(~a, ~a)"]
  [('fmax) "Math.max(~a, ~a)"]
  [('fmin) "Math.min(~a, ~a)"]
  ;[('fdim) "math.max(0, ~a - ~a)"]
  ;[('copysign) "math.abs(~a) * math.sign(~a)"]
  [('trunc) "Math.trunc(~a)"]
  [('round) "Math.round(~a)"]
  [('isinf)  "(Math.abs(~a) === Infinity)"]
  [('isnan) "isNaN(~a)"]
  [(_) (error 'operator->js "Unsupported operator ~a" op)])

(define (application->js type operator args)
  (if (and (eq? operator '-) (= (length args) 1))
    (format "(- ~a)" (car args))
    (if (and (*runtime*) (not (set-member? '(== + - * /  < > <= >= and or not) operator)))
        (format "~a.~a(~a)" (*runtime*) operator (string-join args ", "))
        (apply format (operator->js operator) args))))

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)])
                         (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->js expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~avar ~a = ~a;\n" indent (fix-name var*)
               (expr->js val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->js body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->js cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~avar ~a;\n" indent (fix-name outvar))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->js ift #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->js iff #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~avar ~a = ~a;\n" indent (fix-name var*)
               (expr->js val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (fix-name (gensym 'test)))
     (printf "~avar ~a = ~a\n" indent test-var
             (expr->js cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~awhile (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\tvar ~a = ~a;\n" indent (fix-name temp-var)
               (expr->js update #:names names* #:type type #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a;" indent test-var
             (expr->js cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->js retexpr #:names names* #:type type #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->js arg #:names names #:type type #:indent indent)) args))
     (application->js type operator args_c)]
    [(? constant?)
     (format "(~a)" (constant->js expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a" (real->double-flonum expr) )]))

(define (core->js prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a" (fix-name (if (list? var) (car var) var)))))
  (define func-body
    (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (printf "\treturn ~a;\n" (expr->js body #:type type))))))
  (format "function ~a(~a) {\n~a}\n"
          (fix-name name)
          (string-join arg-strings ", ")
          func-body))

(define (export-js input-port output-port
                   #:fname [fname "stdin"]
                   #:runtime [runtime #f])
  (when runtime (*runtime* runtime))
  (for ([expr (in-port (curry read-fpcore fname) input-port)] [n (in-naturals)])
    (fprintf output-port "~a\n" (core->js expr #:name (format "ex~a" n)))))
