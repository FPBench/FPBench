#lang racket

(require "common.rkt" "fpcore.rkt" "supported.rkt")
(provide core->cml cml-supported)

(define cml-supported (supported-list
  '(if let let* while while* + - * / < > <= >= == != not and or abs sqrt)    
  '(TRUE FALSE INFINITY NAN)
  '(binary64))) ; bool

(define/match (operator->sml op)
  [('==) "Double.="]
  [((or '+ '- '* '/ '> '< '>= '<=)) (format "Double.~a" op)])

(define/match (constant->cml expr)
  [('INFINITY) "inf"]
  [('NAN) "nan"]
  [((or 'TRUE 'FALSE)) (string-titlecase (format "~a" expr))]
  [((? number?)) (format "(Double.fromString \"~a\")" (real->double-flonum expr))])

(define (decleration->cml var [val 0])
  (format "val ~a = ~a" var val))

(define (assignment->cml var val)
  (format "~a = ~a" var val))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (last (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

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

(define *names* (make-parameter (mutable-set)))

(define (application->cml operator args)
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
    [(list 'abs a) (format "(Double.abs ~a)" a)]
    [(list 'sqrt a) (format "(Double.sqrt ~a)" a)]
    [(list 'fma a b c) (format "(Double.fma ~a ~a ~a)" a b c)]))

(define (expr->cml expr #:names [names #hash()] #:indent [indent "  "])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define vars* (map gensym vars))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      (format "let\n~a\n~ain\n~a  ~a\n~aend"
        (string-join
          (for/list ([var* vars*] [val vals])
            (format "  ~a~a" indent (decleration->cml (fix-name var*) (expr->cml val #:names names #:indent indent))))
          "\n")
        indent
        indent
        (expr->cml body #:names names* #:indent (format "  ~a" indent))
        indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (format "let\n~a\n~ain\n~a  ~a\n~aend"
        (string-join
          (for/list ([var vars] [val vals])
            (format "  ~a~a" indent (decleration->cml (fix-name var) (expr->cml val #:names names #:indent indent))))
          "\n")
        indent
        indent
        (expr->cml body #:names names #:indent (format "  ~a" indent))
        indent)]

    [`(if ,cond ,ift ,iff)
      (format "if ~a\n~athen ~a\n~aelse ~a"
        (expr->cml cond #:names names #:indent (format "  ~a" indent))
        indent
        (expr->cml ift #:names names #:indent (format "  ~a" indent))
        indent
        (expr->cml iff #:names names #:indent (format "  ~a" indent)))]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define loop (gensym 'loop))
      (define vars* (map gensym vars))
      (define vars** (map gensym vars*))
      (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
      (define arg-list 
        (string-join
          (for/list ([var* vars*])
            (format "~a" var*))
          " "))
      (format "let\n~a\n~a\n~ain\n~a  ~a ~a\n~aend"
        (string-join
          (for/list ([var* vars*] [val inits])
            (format "  ~a~a" indent (decleration->cml (fix-name var*) (expr->cml val #:names names* #:indent (format "    ~a" indent)))))
          "\n")
        (format "  ~afun ~a ~a =\n    ~aif ~a\n~a    then ~a\n~a    else ~a"
            indent loop arg-list indent (expr->cml cond #:names names* #:indent (format "  ~a" indent)) indent
            (format "\n      ~alet\n        ~a~a\n      ~ain\n        ~a~a ~a\n      ~aend"
                indent indent
                (string-join
                  (for/list ([var** vars**] [update updates])
                    (decleration->cml var** (expr->cml update #:names names* #:indent (format "        ~a" indent))))
                  (format "\n~a        " indent))
                indent indent loop 
                (string-join
                  (for/list ([var** vars**])
                    (format "~a" var**))
                  " ")
                indent)
            indent (expr->cml retexpr #:names names #:indent (format "      ~a" indent)))
        indent indent loop arg-list indent)] 

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define loop (gensym 'loop))
      (define vars* (map (λ (x) (fix-name (gensym x))) vars))
      (define arg-list 
        (string-join
          (for/list ([var* vars*])
            (format "~a" var*))
        " "))
      (format "let\n~a\n~a\n~ain\n~a  ~a ~a\n~aend"
        (string-join
          (for/list ([var* vars*] [val inits])
            (format "  ~a~a" indent (decleration->cml var* (expr->cml val #:names names #:indent (format "    ~a" indent)))))
          "\n")
        (format "  ~afun ~a ~a =\n    ~aif ~a\n~a    then ~a\n~a    else ~a"
            indent loop arg-list indent (expr->cml cond #:names names #:indent (format "  ~a" indent)) indent
            (format "\n      ~alet\n        ~a~a\n      ~ain\n        ~a~a ~a\n      ~aend"
                indent indent
                (string-join
                  (for/list ([var* vars*] [update updates])
                    (decleration->cml var* (expr->cml update #:names names #:indent (format "        ~a" indent))))
                  (format "\n~a        " indent))
                indent indent loop arg-list indent)
            indent (expr->cml retexpr #:names names #:indent (format "      ~a" indent)))
        indent indent loop arg-list indent)]  

    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->cml arg #:names names)) args))
     (application->cml operator args_c)]
    [(? constant?) (constant->cml expr)]
    [(? number?) (constant->cml expr)]
    [(? symbol?) (fix-name (dict-ref names expr expr))]))

(define (core->cml prog name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  
  (parameterize ([*names* (apply mutable-set args)])
    (define arg-list
    (for/list ([arg args])
      (format "(~a : word64)" (fix-name arg))))
    (define body-out (expr->cml body))
    (format "fun ~a ~a = ~a : word64;\n"
      (fix-name name)
      (if (empty? args) "()" (string-join arg-list " "))
      (if (or (string-prefix? body-out "if") (string-prefix? body-out "let"))
          (format "\n  ~a" body-out)
          body-out))))