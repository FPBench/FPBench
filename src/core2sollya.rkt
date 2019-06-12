#lang racket

(require "common.rkt" "fpcore.rkt")
(provide export-sollya)

;; sollya identifiers have the same rules as in C
(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define (precision->sollya prec)
  (match prec
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
    [_ (error 'constant->sollya "Unsupported constant precision ~a" prec)]))

(define (round->sollya rm)
  (match rm
    ['nearestEven "RN"]
    ['nearestAway (error 'round->sollya "Nearest away rounding is not supported by Sollya tool.")]
    ['toPositive "RU"]
    ['toNegative "RD"]
    ['toZero "RZ"]
    [_ (error 'round->sollya "Unsupported rounding mode ~a" rm)]))

(define (rounded expr ctx)
  (let ([prec (precision->sollya (dict-ref ctx ':precision 'real))]
        [rm (round->sollya (dict-ref ctx ':round 'nearestEven))])
    (if (equal? prec "real")
        expr
        (format "round(~a, ~a, ~a)" expr prec rm))))

;; this does not insert any rounding code
(define (application->sollya operator args ctx)
  (match (cons operator args)
    [(list '- a)
     (rounded (format "-~a" a) ctx)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (rounded (format "(~a ~a ~a)" a operator b) ctx)]
    [(list 'fabs a)
     (rounded (format "abs(~a)" a) ctx)]
    [(list 'pow a b)
     (rounded (format "(~a ^ ~a)" a b) ctx)]
    ;; this is a good cheat
    [(list 'fma a b c)
     (rounded (format "((~a * ~a) + ~a)" a b c) ctx)]
    [(list 'exp2 a)
     (rounded (format "(2 ^ ~a)" a) ctx)]
    [(list 'cbrt a)
     (rounded (format "(~a ^ (1/3))" a) ctx)]
    [(list 'hypot a b)
     (rounded (format "sqrt((~a ^ 2) + (~a ^ 2))" a b) ctx)]
    [(list 'atan2 a b)
     (rounded (format "atan(~a / ~a)" a b) ctx)]
    [(list 'fmax a b)
     (rounded (format "max(~a, ~a)" a b) ctx)]
    [(list 'fmin a b)
     (rounded (format "min(~a, ~a)" a b) ctx)]
    [(list 'nearbyint a)
     (let ([rm (round->sollya (dict-ref ctx ':round 'nearestEven))])
       (if (equal? rm "RN")
           (rounded (format "nearestint(~a)" a) ctx)
           (error 'application->sollya "Unsupported rounding mode ~a for nearbyint" rm))
       )]
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
     (rounded (format "~a(~a)" f (string-join args ", ")) ctx)]))

(define (constant->sollya expr ctx)
  (match expr
    ['E (rounded "exp(1)" ctx)]
    ['LOG2E (rounded "log2(exp(1))" ctx)]
    ['LOG10E (rounded "log10(exp(1))" ctx)]
    ['LN2 (rounded "log(2)" ctx)]
    ['LN10 (rounded "log(10)" ctx)]
    ['PI (rounded "pi" ctx)]
    ['PI_2 (rounded "(pi/2)" ctx)]
    ['PI_4 (rounded "(pi/4)" ctx)]
    ['M_1_PI (rounded "(1/pi)" ctx)]
    ['M_2_PI (rounded "(2/pi)" ctx)]
    ['M_2_SQRTPI (rounded "(2/sqrt(pi))" ctx)]
    ['SQRT2 (rounded "sqrt(2)" ctx)]
    ['SQRT1_2 (rounded "sqrt(1/2)" ctx)]
    ['TRUE "true"]
    ['FALSE "false"]
    ['INFINITY "infty"]
    ['NAN "nan"]
    [_ (error 'constant->sollya "Unsupported constant ~a" expr)]))


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

(define (expr->sollya expr #:names [names #hash()] #:ctx [ctx #hash()] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression. Prints things and updates the set of names via gensym.
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~a~a = ~a;\n" indent (fix-name var*)
               (expr->sollya val #:names names #:ctx ctx #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->sollya body #:names names* #:ctx ctx #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->sollya cond #:names names #:ctx ctx #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~aif (~a) then {\n" indent test)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->sollya ift #:names names #:ctx ctx #:indent (format "~a\t" indent)))
     (printf "\n~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->sollya iff #:names names #:ctx ctx #:indent (format "~a\t" indent)))
     (printf "\n~a};\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~a~a = ~a;\n" indent (fix-name var*)
               (expr->sollya val #:names names #:ctx ctx #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (printf "~a~a = ~a;\n" indent (fix-name test-var)
             (expr->sollya cond #:names names* #:ctx ctx #:indent indent))
     (printf "~awhile (~a) do {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\t~a = ~a;\n" indent (fix-name temp-var)
               (expr->sollya update #:names names* #:ctx ctx #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a;" indent (fix-name test-var)
             (expr->sollya cond #:names names* #:ctx ctx #:indent (format "~a\t" indent)))
     (printf "\n~a};\n" indent)
     (expr->sollya retexpr #:names names* #:ctx ctx #:indent indent)]
    [`(! ,props ... ,body)
     (expr->sollya body #:names names #:ctx (apply hash-set* ctx props) #:indent indent)]
    ;; some operations need special translations
    [`(isnan ,body)
     (define tempvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name tempvar)
             (expr->sollya body #:names names #:ctx ctx #:indent indent))
     (format "(~a != ~a)" (fix-name tempvar) (fix-name tempvar))]
    [`(isinf ,body)
     (define tempvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name tempvar)
             (expr->sollya body #:names names #:ctx ctx #:indent indent))
     (format "(abs(~a) == infty)" (fix-name tempvar))]
    [`(isfinite ,body)
     (define tempvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name tempvar)
             (expr->sollya body #:names names #:ctx ctx #:indent indent))
     (format "(~a == ~a && abs(~a) != infty)" (fix-name tempvar) (fix-name tempvar) (fix-name tempvar))]
    [`(fdim ,a ,b)
     (define temp_a (gensym 'temp_a))
     (define temp_b (gensym 'temp_b))
     (define outvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name temp_a)
             (expr->sollya a #:names names #:ctx ctx #:indent indent))
     (printf "~a~a = ~a;\n" indent (fix-name temp_b)
             (expr->sollya b #:names names #:ctx ctx #:indent indent))
     (printf "~aif (~a > ~a) then {\n" indent (fix-name temp_a) (fix-name temp_b))
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (rounded (format "~a - ~a" (fix-name temp_a) (fix-name temp_b)) ctx))
     (printf "\n~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar) (rounded "0" ctx))
     (printf "\n~a};\n" indent)
     (fix-name outvar)]
    [`(copysign ,a ,b)
     (define temp_a (gensym 'temp_a))
     (define temp_b (gensym 'temp_b))
     (define outvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name temp_a)
             (expr->sollya a #:names names #:ctx ctx #:indent indent))
     (printf "~a~a = ~a;\n" indent (fix-name temp_b)
             (expr->sollya b #:names names #:ctx ctx #:indent indent))
     (printf "~aif (~a < 0) then {\n" indent (fix-name temp_b))
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (rounded (format "-abs(~a)" (fix-name temp_a)) ctx))
     (printf "\n~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (rounded (format "abs(~a)" (fix-name temp_a)) ctx))
     (printf "\n~a};\n" indent)
     (fix-name outvar)]
    [`(trunc ,a)
     (define temp_a (gensym 'temp_a))
     (define outvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name temp_a)
             (expr->sollya a #:names names #:ctx ctx #:indent indent))
     (printf "~aif (~a < 0) then {\n" indent (fix-name temp_a))
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (rounded (format "ceil(~a)" (fix-name temp_a)) ctx))
     (printf "\n~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (rounded (format "floor(~a)" (fix-name temp_a)) ctx))
     (printf "\n~a};\n" indent)
     (fix-name outvar)]
    ;; this will do the wrong thing for negative zero, since sollya doesn't support it
    [`(signbit ,body)
     (define tempvar (gensym 'temp))
     (printf "~a~a = ~a;\n" indent (fix-name tempvar)
             (expr->sollya body #:names names #:ctx ctx #:indent indent))
     (format "(~a < 0)" (fix-name tempvar))]
    ;; most operators can go to application->sollya, which also fixes up a few that need different names
    [(list (? operator? operator) args ...)
     (define args_sollya
       (map (λ (arg) (expr->sollya arg #:names names #:ctx ctx #:indent indent)) args))
     (application->sollya operator args_sollya ctx)]
    [(? constant?)
     (format "~a" (constant->sollya expr ctx))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (rounded (format "~a" expr) ctx)]))

(define (export-sollya prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)

  (define arg-strings
    (for/list ([var args])
      (match var
        [(list '! props ... name) (format "~a" (fix-name name))]
        [name (format "~a" (fix-name name))])))

  ;; put defaults of binary64 precision and round nearestEven first,
  ;; so that if other values are given explicitly in props,
  ;; they will overwrite these
  (define ctx
    (apply hash-set* #hash() (append '(:precision binary64 :round nearestEven) props)))

  (define arg-rounding
    (filter (compose not void?)
            (for/list ([var args])
              (let-values ([(arg-name arg-ctx)
                            (match var
                              [(list '! props ... name)
                               (values (fix-name name) (apply hash-set* ctx props))]
                              [name (values (fix-name name) ctx)])])
                (unless (equal? (precision->sollya (dict-ref arg-ctx ':precision 'real)) "real")
                  (format "\t~a = ~a;" arg-name (rounded arg-name arg-ctx)))))))
  (define-values (sollya-body sollya-names)
    (parameterize ([*names* (apply mutable-set
                                   (for/list ([var args])
                                     (match var
                                       [(list '! props ... name) name]
                                       [name name])))])
      (values
       (with-output-to-string
         (λ () (printf "\t~a;\n" (expr->sollya body #:ctx ctx))))
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
  (define rounding-string
    (if (> (length arg-rounding) 0)
        (format "\n~a" (string-join arg-rounding "\n"))
        ""))

  (format "procedure ~a(~a) {~a~a\n~a};\n\n"
          (fix-name name)
          (string-join arg-strings ", ")
          var-string
          rounding-string
          sollya-body))
