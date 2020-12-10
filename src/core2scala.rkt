#lang racket

(require math/bigfloat)
(require "common.rkt" "compilers.rkt" "imperative.rkt" "range-analysis.rkt"
         "supported.rkt")
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
            if let let* digits !))   ;; benchmarks with if statements break with --mixed-precision flag
    (curry set-member? '(TRUE FALSE))
    (curry set-member? '(binary32 binary64 binary128 binary256))        
    (curry equal? 'nearestEven)))

(define scala-reserved '())

(define/match (type->scala type)
  [('binary256) "QuadDouble"]
  [('binary128) "Quad"]
  [('binary64) "Float64"]
  [('binary32) "Float32"])

(define (operator->scala props op args)
  (match op
    ['/   (format "(~a / ~a)" (first args) (second args))]
    [_    (format "~a(~a)" op (string-join args ", "))]))

(define (constant->scala props expr)
  (define prec (dict-ref props ':precision 'binary64))
  (match expr
    [(or 'TRUE 'FALSE) (string-downcase (~a expr))]
    [(? hex?) (hex->racket expr)]
    [(? number?)
      (match prec
        ['binary128  (parameterize ([bf-precision 237]) 
                        (format "~a" (bigfloat->string (bf expr))))]
        ['binary128  (parameterize ([bf-precision 113]) 
                        (format "~a" (bigfloat->string (bf expr))))]
        [_          (format "~a" (real->double-flonum expr))])]
    [_  expr]))

(define (declaration->scala props var [val 0])
  (define type (type->scala (dict-ref props ':precision 'binary64)))
  (fprintf (*scala-prec-file*) "\t~a: ~a\n" var type)
  (format "val ~a: Real = ~a" var val))

(define (assignment->scala var val)
  (error 'assignment->scala "Daisy compiler using 'val'. Therefore, it does not support assignment"))

(define (round->scala val props) (~a val)) ; round(val) = val

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
        (let-values ([(pre* prec) (convert-expr pre* #:ctx ctx #:indent "\t\t")])
          pre*))
      (format "\t\t// Invalid precondition: ~a\n" pre)))

(define (function->scala name args arg-props body return ctx vars)
  (define type (type->scala (ctx-lookup-prop ctx ':precision 'binary64)))
  (define arg-list
    (for/list ([arg args] [prop arg-props])
      (fprintf (*scala-prec-file*) "\t~a: ~a\n" arg (type->scala (dict-ref prop ':precision)))
      (format "~a: Real" arg)))
  (define precond
    (let ([pre 
            (if (hash-has-key? (ctx-props ctx) ':daisy-pre)
                (ctx-lookup-prop ctx ':daisy-pre #f)
                (ctx-lookup-prop ctx ':pre #f))])
      (if pre (precond->scala pre args ctx) "")))
  (format "\tdef ~a(~a): Real = {\n~a~a\t\t~a\n\t}\n"
          name
          (string-join arg-list ", ")
          precond
          body
          return))

(define scala-language (language "scala" operator->scala constant->scala declaration->scala assignment->scala round->scala (const "") function->scala))

;;; Exports

(define (core->scala prog name)
  (parameterize ([*lang* scala-language]  
                 [*reserved-names* scala-reserved]
                 [*fix-name-format* "_~a"])
    (fprintf (*scala-prec-file*) "~a = {\n" name)
    (let ([core* (convert-core prog name)])
      (fprintf (*scala-prec-file*) "}\n")
      core*)))

(define-compiler '("scala") scala-header core->scala scala-footer scala-supported)
