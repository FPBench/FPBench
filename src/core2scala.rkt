#lang racket

(require generic-flonum)
(require "imperative.rkt" "range-analysis.rkt")

(provide scala-header scala-footer core->scala scala-supported
         *scala-suppress* *scala-prec-file*)

(define *scala-suppress* (make-parameter #f))
(define *scala-prec-file* (make-parameter #f))

(define scala-header (λ (x) (format "import daisy.lang._\nimport Real._\n\nobject ~a {\n" x)))
(define scala-footer (const "}\n"))

(define scala-supported
  (supported-list
    (curry set-member?
          '(+ - * / sqrt sin cos tan asin acos atan exp log fma      ;; pow has partial support
            < > <= >= == != and or not
            if let let* digits !    ;; benchmarks with if statements break with --mixed-precision flag
            array dim size ref for for* tensor tensor*))
    (curry set-member? '(TRUE FALSE))
    (curry set-member? '(binary32 binary64 binary128 binary256))        
    (curry equal? 'nearestEven)
    #f))

(define scala-reserved  ; Language-specific reserved names (avoid name collisions)
  '(and abstract case catch class def do else extends false
    final finally for forSome if implicit import lazy
    match new null object override package private
    protected return sealed super this throw trait
    try true type val var while with yield))

(define (scala-fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a" (char->integer char))))
   ""))

(define/match (type->scala type)
  [('binary256) "QuadDouble"]
  [('binary128) "Quad"]
  [('binary64) "Float64"]
  [('binary32) "Float32"])

(define/match (prec->bits type)
  [('binary256) (values 19 256)]
  [('binary128) (values 15 128)]
  [('binary64) (values 11 64)]
  [('binary32) (values 8 32)])

(define (constant->scala expr ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match expr
   [(or 'TRUE 'FALSE) (string-downcase (~a expr))]
   [(? hex?) (~a (hex->racket expr))]
   [(? number?)
    (define-values (e n) (prec->bits prec))
    (parameterize ([gfl-exponent e] [gfl-bits n])
      (let ([str (gfl->string (gfl expr))])
        (if (string-contains? str ".")
            str
            (string-append str ".0"))))]
    [_ (~a expr)]))

(define declaration->scala
  (case-lambda
   [(var ctx) (declaration->scala var 0 ctx)]
   [(var val ctx)
    (define type (type->scala (ctx-lookup-prop ctx ':precision)))
    (fprintf (*scala-prec-file*) "\t~a: ~a\n" var type)
    (format "val ~a: Real = ~a" var val)]))

(define (assignment->scala var val ctx)
  (error 'assignment->scala "Daisy compiler using 'val': assignment not supported"))

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
  (define var-ranges 
    (let ([var-ranges (condition->range-table pre)])
      (for/hash ([key (hash-keys var-ranges)])
        (values (ctx-lookup-name ctx key) (hash-ref var-ranges key)))))
  (define pre* (expand-precond pre))
  (define valid?
    (for/and ([var args])
      (let ([val (hash-ref var-ranges var #f)])
        (if val (nonempty-bounded? val) #f))))
  (unless (or valid? (*scala-suppress*))
    (printf "Removed invalid precondition: ~a\n" pre))
  (if valid?
      (format "\t\trequire(~a)\n"
        (let-values ([(pre* prec) (visit/ctx scala-visitor pre* ctx)])
          pre*))
      (format "\t\t// Invalid precondition: ~a\n" pre)))

(define (program->scala name args arg-ctxs body return ctx vars)
  (define type (type->scala (ctx-lookup-prop ctx ':precision)))
  (define arg-list
    (for/list ([arg args] [ctx arg-ctxs])
      (fprintf (*scala-prec-file*) "\t~a: ~a\n"
                arg (type->scala (ctx-lookup-prop ctx ':precision)))
      (format "~a: Real" arg)))
  (define precond
    (let ([pre (if (hash-has-key? (ctx-props ctx) ':daisy-pre)
                   (ctx-lookup-prop ctx ':daisy-pre #f)
                   (ctx-lookup-prop ctx ':pre #f))])
      (if pre (precond->scala pre args ctx) "")))
  (format "\tdef ~a(~a): Real = {\n~a~a\t\t~a\n\t}\n"
          name
          (string-join arg-list ", ")
          precond
          body
          return))

; Override visitor behavior
(define-expr-visitor imperative-visitor scala-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define-values (cond* _) (visit/ctx vtor cond ctx))
    (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
    (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx))
    (define-values (ctx* tmpvar)
      (let ([ift-prec (ctx-lookup-prop ift-ctx ':precision)])
        (ctx-random-name (ctx-update-props ctx `(:precision ,ift-prec)))))
    (printf "~a~a\n" indent
            (declaration->scala tmpvar
                                (format "\n~a\tif (~a) {\n~a\t\t~a\n~a\t} else {\n~a\t\t~a\n~a\t}"
                                        indent cond* indent ift* indent indent iff* indent)
                                ctx*))
    (values tmpvar ift-ctx)])

(define core->scala*
  (make-imperative-compiler "scala"
    #:constant constant->scala
    #:declare declaration->scala
    #:assign assignment->scala
    #:program program->scala
    #:reserved scala-reserved
    #:visitor scala-visitor
    #:fix-name scala-fix-name
    #:indent "\t\t"))

(define (core->scala prog name)
  (fprintf (*scala-prec-file*) "~a = {\n" name)
  (begin0 (core->scala* prog name)
    (fprintf (*scala-prec-file*) "}\n")))
  
(define-compiler '("scala") scala-header core->scala scala-footer scala-supported)
