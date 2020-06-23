#lang racket

(require "common.rkt" "compilers.rkt" "functional.rkt" "range-analysis.rkt" "supported.rkt")
(provide scala-header scala-footer core->scala scala-supported *scala-suppress*)

(define *scala-suppress* (make-parameter #f))

(define scala-header (λ (x) (format "import daisy.lang._\nimport Real._\n\nobject ~a {\n" x)))
(define scala-footer (const "}\n"))

(define scala-supported
  (supported-list
   '(+ - * / sqrt sin cos tan asin acos atan exp log fma      ;; pow has partial support
     < > <= >= == != and or not
     let if digits)
   '(TRUE FALSE)
   '(binary32 binary64)
   '(nearestEven)))

(define scala-reserved '())

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (operator->scala op args)
  (format "~a(~a)" op (string-join args ", ")))

(define (application->scala operator args ctx)
  (match (cons operator args)
    [(list '- a)
     (format "-(~a)" a)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a operator b)]
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
    [(list 'and as ...)
     (format "(~a)" (string-join as " && "))]
    [(list 'or as ...)
     (format "(~a)" (string-join as " || "))]
    [(list (? operator? f) args ...)
     (operator->scala operator args)]))

(define (constant->scala expr ctx)
  (match expr
    [(or 'TRUE 'FALSE) (string-downcase (~a expr))]
    [(? hex?) (hex->racket expr)]
    [(? number?) 
     (if (= expr (round expr))
         (format "~a" (round expr))
         (format "~a" (real->double-flonum expr)))]
    [_  expr]))

(define (declaration->scala var [val 0])
  (format "val ~a: Real = ~a" var val))

(define (let->scala vars vals body indent nested)
  (format "~a\n~a~a"
    (string-join 
      (for/list ([var vars] [val vals])
        (declaration->scala var val))
      (format "\n~a" indent))
    indent
    body))

(define (if->scala cond ift iff tmp indent)
  (format "if(~a) {\n~a\n~a} else {\n~a\n~a}"
    cond 
    (if (string-prefix? ift "\t") ift (format "~a\t~a" indent ift)) 
    indent 
    (if (string-prefix? iff "\t") ift (format "~a\t~a" indent iff)) 
    indent))

(define (while->scala vars inits cond updates updatevars body loop indent nested)
  (error 'while->scala "Daisy does not support while loops"))

;; Precondition checking

(define (expand-precond pre)
  (match pre
   [`(let ([,vars ,vals] ...) ,body) 
    `(let (,@(map list vars vals)) ,(expand-precond body))]
   [(list (or 'and 'or) args ...)
    `(,(car pre) ,@(map expand-precond args))]
   [(list 'not arg) 
    `(not ,(expand-precond arg))]
   [(list (or '> '< '>= '<= '!=) args ...)
    `(,(car pre) ,@args)]
   [(list '== args ...)
    (cons
      'and
      (let loop ([args args])
        (cond
         [(null? args)  '()]
         [(append
            (for/fold ([exprs '()]) ([b (cdr args)])
              (append exprs (list (list '<= b (car args)) (list '<= (car args) b))))
            (loop (cdr args)))])))]))

(define (precond->scala pre args ctx) 
  (define var-ranges (condition->range-table pre))
  (define pre* (expand-precond pre))
  (define valid?
    (for/and ([var (map string->symbol args)])
      (let ([val (hash-ref var-ranges var #f)])
        (if val (nonempty-bounded? val) #f))))
  (unless (or valid? *scala-suppress*)
    (printf "Removed unbounded precondition: ~a\n" pre))
  (if valid?
      (format "\t\trequire(~a)\n" (convert-expr pre* #:ctx ctx #:indent "\t\t"))
      ""))

;;

(define (function->scala name args body ctx names)
  (define arg-list
    (for/list ([arg args])
      (format "~a: Real" arg)))
  (define precond
    (let ([pre 
            (if (hash-has-key? (ctx-props ctx) ':daisy-pre)
                (ctx-lookup-prop ctx ':daisy-pre #f)
                (ctx-lookup-prop ctx ':pre #f))])
      (if pre (precond->scala pre args ctx) "")))
  (format "\tdef ~a(~a): Real = {\n~a\t\t~a\n\t}\n"
          name
          (string-join arg-list ", ")
          precond
          body))

(define scala-language (functional "scala" application->scala constant->scala declaration->scala 
                                   let->scala if->scala while->scala function->scala))

;;; Exports

(define (core->scala prog name)
  (parameterize ([*func-lang* scala-language] 
                 [*gensym-fix-name* fix-name] 
                 [*reserved-names* scala-reserved])
    (core->functional prog name)))

(define-compiler '("scala") scala-header core->scala scala-footer scala-supported)