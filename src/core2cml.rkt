#lang racket

(require "common.rkt" "compilers.rkt" "functional.rkt" "supported.rkt")
(provide core->cml cml-supported)

(define cml-supported (supported-list
  (disjoin ieee754-ops (negate (curry equal? 'fma))
           (curry set-member? '(! if let let* while while* not and or digits)))
  (curry set-member? '(TRUE FALSE INFINITY NAN))
  (curry equal? 'binary64)
  (curry equal? 'nearestEven))) ; bool

(define cml-reserved '()) ; Language-specific reserved names (avoid name collisions)

(define (fix-name name)
  (define str 
    (string-join
      (for/list ([char (~a name)])
        (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
            (string char)
            (format "_~a_" (char->integer char))))
              ""))
  (string-set! str 0 (char-downcase (string-ref str 0)))
  str)

(define/match (operator->sml op)
  [('==) "Double.="]
  [((or '+ '- '* '/ '> '< '>= '<=)) (format "Double.~a" op)])

(define (application->cml operator args ctx)
  (match (cons operator args)
    [(list '- a) (format "(Double.~~ ~a)" a)]
    [(list (or '== '!= '< '> '<= '>=)) "True"]
    [(list (or '+ '- '* '/) a b) (format "(~a ~a ~a)" (operator->sml operator) a b)]
    [(list (or '== '< '> '<= '>=) head args ...)
     (format "~a"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "(~a ~a ~a)" (operator->sml operator) a b))
              " andalso "))]
    [(list '!= args ...)
      (format "~a"
        (string-join
          (let loop ([args args])
            (if (null? args)
                '()
                (append
                    (for/list ([b (cdr args)])
                      (format "(not (Double.= ~a ~a))" (car args) b))
                    (loop (cdr args)))))
        " andalso "))]
    [(list 'not a) (format "(not ~a)" a)]
    [(list 'and a ...)
     (format "~a" (string-join (map ~a a) " andalso "))]
    [(list 'or a ...)
     (format "~a" (string-join (map ~a a) " orelse "))]
    [(list 'fabs a) (format "(Double.abs ~a)" a)]
    [(list 'sqrt a) (format "(Double.sqrt ~a)" a)]
    [(list 'fma a b c) (format "(Double.fma ~a ~a ~a)" a b c)]))

(define (constant->cml expr ctx)
  (match expr
    ['INFINITY "(Double.fromString \"inf\")"]
    ['NAN "(Double.fromString \"nan\")"]
    [(or 'TRUE 'FALSE) (string-titlecase (format "~a" expr))]
    [(? hex?) (format "(Double.fromString \"~a\")" (real->double-flonum (hex->racket expr)))]
    [(? number?) (format "(Double.fromString \"~a\")" (real->double-flonum expr))]
    [_  expr]))

(define (declaration->cml var [val 0])
  (format "val ~a = ~a" var val))

(define (let->cml vars vals body indent nested)
  (format "let\n~a  ~a\n~ain\n~a  ~a\n~aend" 
    indent
    (string-join
      (for/list ([var vars] [val vals])
        (declaration->cml var val))
      (format "\n  ~a" indent))
    indent indent body indent)) ; todo fix

(define (if->cml cond ift iff tmp indent)
  (format "if ~a\n~athen ~a\n~aelse ~a\n"
          cond indent ift indent iff))

(define (while->cml vars inits cond updates updatevars body loop indent nested)
  (define loopdef
    (string-append
      (format "~a " loop)
      (string-join
        (for/list ([var vars])
          (format "~a" var))
        " ")))
  (format "let\n~a  ~a\n  ~afun ~a =\n    ~aif ~a\n~a    then ~a\n~a    else ~a\n~ain\n~a  ~a\n~aend"
      indent
      (string-join
        (for/list ([var vars] [val inits])
          (declaration->cml var val))
        (format "\n  ~a" indent))
      indent loopdef indent cond indent
      (format "\n      ~alet\n        ~a~a\n      ~ain\n        ~a~a ~a\n      ~aend"
          indent indent
          (string-join
            (for/list ([updatevar updatevars] [update updates])
              (declaration->cml updatevar update))
            (format "\n~a        " indent))
          indent indent loop 
          (string-join
            (for/list ([updatevar updatevars])
              (format "~a" updatevar))
            " ")
          indent)
      indent body indent indent loopdef indent))

(define (function->cml name args body ctx names)
  (define arg-list
    (for/list ([arg args])
      (format "(~a : word64)" arg)))
  (format "fun ~a ~a = ~a : word64;\n"
          name
          (if (empty? args) "()" (string-join arg-list " "))
          (if (or (string-prefix? body "if") (string-prefix? body "let"))
            (format "\n  ~a" body)
            body)))

(define cml-language (functional "cml" application->cml constant->cml declaration->cml let->cml if->cml while->cml function->cml))

;;; Exports

(define (core->cml prog name) 
  (parameterize ([*func-lang*  cml-language] [*gensym-fix-name* fix-name] [*reserved-names* cml-reserved])
    (core->functional prog name)))

(define-compiler '("cml") (const "") core->cml (const "") cml-supported)