#lang racket

(require "ml.rkt")
(provide core->cml cml-supported)

; 'cast' is a no-op since only one precision is supported
(define cml-supported
  (supported-list
    (disjoin (conjoin ieee754-ops (negate (curry equal? 'fma)))
             (curry set-member? '(! cast if let let* while while*
                                  not and or digits)))
    (curry set-member? '(TRUE FALSE INFINITY NAN))
    (curry equal? 'binary64)
    (curry equal? 'nearestEven) ; bool
    #f))

(define cml-reserved    ; Language-specific reserved names (avoid name collisions)
  '(datatype fun else end if in let local
    raise struct structure then type))

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

(define/match (cml-op op)
  [('==) "Double.="]
  [((or '+ '- '* '/ '> '< '>= '<=)) (format "Double.~a" op)])

(define (operator->cml op args ctx)
  (match (cons op args)
   [(list '- a) (format "(Double.~~ ~a)" a)]
   [(list (or '== '!= '< '> '<= '>=)) "True"]
   [(list (or '+ '- '* '/) a b) (format "(~a ~a ~a)" (cml-op op) a b)]
   [(list (or '== '< '> '<= '>=) head args ...)
    (string-join
      (for/list ([a (cons head args)] [b args])
        (format "(~a ~a ~a)" (cml-op op) a b))
    " andalso ")]
   [(list '!= args ...)
    (string-join
      (let loop ([args args])
        (if (null? args)
            '()
            (append
              (for/list ([b (cdr args)])
                (format "(not (Double.= ~a ~a))" (car args) b))
                  (loop (cdr args)))))
      " andalso ")]
   [(list 'not a) (format "(not ~a)" a)]
   [(list 'and a ...)
    (string-join (map ~a a) " andalso ")]
   [(list 'or a ...)
    (string-join (map ~a a) " orelse ")]
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
   [_  (~a expr)]))

(define (params->cml args)
  (if (null? args)
      "()"
      (string-join args " ")))

(define (body-is-multi-lined? body)
  (or (string-contains? body "if")
      (string-contains? body "let")))

(define (program->cml name args arg-ctxs body ctx)
  (if (body-is-multi-lined? body)
      (format "fun ~a ~a =\n  ~a;\n" name (params->cml args) body)
      (format "fun ~a ~a = ~a;\n" name (params->cml args) body)))

(define core->cml
  (make-ml-compiler "cakeml"
    #:infix-ops '()           ; process all ops here
    #:operator operator->cml
    #:constant constant->cml
    #:program program->cml
    #:fix-name fix-name))

(define-compiler '("cakeml" "cml") (const "") core->cml (const "") cml-supported)
