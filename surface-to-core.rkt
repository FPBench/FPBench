#lang racket

(define (canonicalize body)
  (match body
    [(or 'E 'PI) body]
    [`(+ ,branch) (canonicalize branch)]
    [`(+ ,args ... ,arg) `(+ ,(canonicalize (cons '+ args)) ,arg)]
    [`(* ,branch) (canonicalize branch)]
    [`(* ,args ... ,arg) `(* ,(canonicalize (cons '* args)) ,arg)]
    [`(- ,head) (canonicalize head)]
    [`(- ,head ,arg) `(- ,(canonicalize head) ,(canonicalize arg))]
    [`(- ,args ... ,tail) `(- ,(canonicalize (cons '- args)) ,tail)]
    [`(/ ,head ,arg) `(/ ,(canonicalize head) ,(canonicalize arg))]
    [`(/ ,args ... ,tail) `(/ ,(canonicalize (cons '/ args)) ,tail)]
    [(? list?) (cons (car body) (map canonicalize (cdr body)))]
    [(? symbol?) body]
    [(? number?) body]))

(define (substitute body bindings)
  (match body
    [(or 'E 'PI) body]
    [(? (λ (x) (assoc x bindings))) (cdr (assoc body bindings))]
    [`(,op ,args ...) (cons op (map (curryr substitute bindings) args))]
    [(? symbol?) body]
    [(? number?) body]))

(define (appears? body variable)
  (match body
    [(or 'E 'PI) #f]
    [(== variable) #t]
    [`(,op ,args ...) (ormap (curryr appears? variable) args)]
    [(? symbol?) #f]
    [(? number?) #f]))

(define (clean-unused bindings)
  (let loop ([bindings (reverse bindings)] [clean '()])
    (match bindings
      [(cons (cons var value) bindings*)
       (if (or (ormap (curryr appears? var) bindings*)
               (appears? (cons 'self value) var))
           (cons (cons var value) (loop bindings* clean))
           (cons (cons var 'UNUSED) (loop bindings* clean)))]
      ['() clean])))

;; Bindings store variable-value bindings in an alist.

(define (compile-statements statements)
  (let loop ([bindings '()] [statements statements])
    (if (null? statements)
        (values 'UNUSED (clean-unused bindings))
        (match (car statements)
          [`[= ,(? symbol? var) ,value]
           (loop (cons (cons var (substitute (canonicalize value) bindings))
                       bindings)
                 (cdr statements))]
          [`(output ,vals ...)
           (values
            (canonicalize
             (cons '+
                   (map (λ (value) (substitute (canonicalize value) bindings))
                        vals)))
            bindings)]
          [`(while ,cond ,substatements ...)
           (define-values (outexpr outb) (compile-statements substatements))
           (when (not (equal? outexpr 'UNUSED))
             (error "(while) loop cannot contain (output) expression."))
           (define loopvars
             (for/list ([bind outb] #:when (not (equal? (cdr bind) 'UNUSED)))
               (match (assoc (car bind) bindings)
                 [(cons var init)
                  `[,var ,init ,(cdr bind)]]
                 [#f
                  `[,(car bind) UNUSED ,(cdr bind)]])))
           (define-values (out bindings*)
             (loop (filter (λ (x) (not (assoc (car x) outb))) bindings)
                   (cdr statements)))
           (values
            `(while ,(canonicalize cond)
               ,loopvars
               ,out)
            bindings*)]
          [`(if cond (then ,iftstatements ...))
           '???]
          [`(if cond (then ,iftstatements ...) (else ,iffstatements ...))
           '???]))))

(define (compile-program body)
  (match-define `(function ,statements ...) body)
  (define-values (out bs) (compile-statements statements))
  `(lambda (,@(for/list ([b bs] #:when (number? (cdr b)))
                `[,(car b) ,(cdr b)]))
     ,out))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "surface-to-core.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (pretty-print (compile-program expr))
     (newline))))
