#lang racket
(require "common.rkt" "fpcore.rkt" "fpimp.rkt")
(provide compile-program)

(define (canonicalize body)
  (match body
    [(or 'E 'PI) body]
    [`(+ ,branch) (canonicalize branch)]
    [`(+ ,args ... ,arg) `(+ ,(canonicalize (cons '+ args)) ,(canonicalize arg))]
    [`(* ,branch) (canonicalize branch)]
    [`(* ,args ... ,arg) `(* ,(canonicalize (cons '* args)) ,(canonicalize arg))]
    [`(- ,head) (canonicalize head)]
    [`(- ,head ,arg) `(- ,(canonicalize head) ,(canonicalize arg))]
    [`(- ,args ... ,tail) `(- ,(canonicalize (cons '- args)) ,(canonicalize tail))]
    [`(/ ,head ,arg) `(/ ,(canonicalize head) ,(canonicalize arg))]
    [`(/ ,args ... ,tail) `(/ ,(canonicalize (cons '/ args)) ,(canonicalize tail))]
    [(? list?) (cons (car body) (map canonicalize (cdr body)))]
    [(? symbol?) body]
    [(? number?) body]))

(define (substitute body bindings)
  (match body
    [(? constant?) body]
    [(? (λ (x) (dict-has-key? bindings x)))
     ;; The (if) handles the case when we don't have a value bound for the variable
     (if (null? (cdr (assoc body bindings)))
         body
         (cdr (assoc body bindings)))]
    [`(,op ,args ...) (cons op (map (curryr substitute bindings) args))]
    [(? symbol?) body]
    [(? number?) body]))

(define (appears? expr variable)
  (match expr
    [(or 'E 'PI) #f]
    [(== variable) #t]
    [`(if ,cond ,ift ,iff)
     (or (curryr appears? variable) (list cond ift iff))]
    [`(let ([,vars ,vals] ...) ,body)
     (or (ormap (curryr appears? variable) vals)
         (and (not (member variable vars)) (appears? body variable)))]
    [`(while ,test ([,vars ,inits ,updates] ...) ,return)
     (or (appears? test variable)
         (ormap (curryr appears? variable) inits)
         (and (not (member variable vars))
              (or (appears? return variable)
                  (ormap (curryr appears? variable) updates))))]
    [`(,op ,args ...) (ormap (curryr appears? variable) args)]
    [(? symbol?) #f]
    [(? number?) #f]))

(define (merge-values conds vals)
  (if (andmap (curryr equal? (car vals)) vals)
      (car vals)
      (merge-values* conds vals)))

(define/match (merge-values* conds vals)
  [((list 'else) (list x)) x]
  [((list c cs ...) (list x xs ...))
   `(if ,c ,x ,(merge-values cs xs))])

;; Bindings store variable-value bindings in an alist.

(define (assigned statements)
  (match statements
    ['() '()]
    [(list `[= ,(? symbol? var) ,_] rest ...)
     (cons var (assigned rest))]
    [(list `(while ,_ ,sub ...) rest ...)
     (append (assigned sub) (assigned rest))]
    [(list `(if [,_ ,subs ...] ...) rest ...)
     (append (append-map assigned subs) (assigned rest))]))

(define/contract (compile-statements statements variables outexprs)
  (-> (listof statement?) (listof symbol?) (listof expr?) (values expr? any/c))
  (let loop ([statements statements] [bindings '()])
    (match statements
      ['()
       (values (substitute (canonicalize (cons '+ outexprs)) bindings)  bindings)]
      [(list `[= ,(? symbol? var) ,value] rest ...)
       (define value* (substitute (canonicalize value) bindings))
       (loop rest (cons (cons var value*) bindings))]
      [(list `(while ,test ,substatements ...) rest ...)
       (define trashed (assigned substatements))
       (define sub-bindings (map (λ (x) (if (member (car x) trashed) (list (car x)) x)) bindings))

       (define-values (_ loop-bindings) (loop substatements sub-bindings))
       (define cond-expr (substitute (canonicalize test) sub-bindings))
       (define-values (out end-bindings) (loop rest sub-bindings))

       (define loop-bound (drop-right loop-bindings (length sub-bindings)))
       (define loop-vars
         (for/fold ([vars '()]) ([bind loop-bound] #:when (not (null? (cdr bind))))
           (define appears-in-rest
             (or (appears? cond-expr (car bind))
                 (appears? out (car bind))
                 (for/or ([other loop-bound] #:when (not (null? (cdr other))))
                   (appears? (cdr other) (car bind)))))

           (define init-value (assoc (car bind) bindings))

           (if appears-in-rest
               (cons (list
                      (car bind)
                      (cond
                       [(not init-value)
                        (if (member (car bind) variables)
                            (car bind)
                            0.0)]
                       [(null? (cdr init-value)) (car bind)]
                       [else (cdr init-value)])
                      (cdr bind))
                     vars)
               vars)))

       (values
        `(while ,cond-expr ,(remove-duplicates loop-vars #:key car) ,out)
        end-bindings)]
      [(list `(if [,conds ,stmtss ...] ...) rest ...)
       (when (or (null? conds) (not (equal? (last conds) 'else)))
         (set! conds (append conds (list 'else)))
         (set! stmtss (append conds (list))))

       (define conds*
         (for/list ([cond conds])
           (substitute (canonicalize cond) bindings)))

       (define outbs
         (for/list ([stmts stmtss])
           (define-values (_ outb) (loop stmts bindings))
           (drop-right outb (length bindings))))

       (define assigned-vars (remove-duplicates (append-map (curry map car) outbs)))
       (define joined
         (for/list ([var assigned-vars])
           (define options (for/list ([outb outbs])
                             (dict-ref outb var var)))
           (cons var (merge-values conds* options))))

       (loop rest (append joined bindings))])))

(define/contract (compile-program body)
  (-> fpimp? fpcore?)
  (match-define `(FPImp (,variables ...) ,lines ... (output ,outexprs ...)) body)
  (define-values (statements properties) (parse-properties lines))
  (define attributes*
    (if (dict-has-key? properties ':pre)
        (dict-update properties ':pre canonicalize)
        properties))

  (define-values (out bindings)
    (compile-statements statements variables outexprs))

  `(FPCore (,@variables) ,@(unparse-properties attributes*) ,out))

(property compilation-valid
  ;; These properties aren't checked, but can be useful to write down
  (let ((x fpimp?))
    (= (apply + (racket-run-fpcore (compile-program x))) (racket-run-fpimp x))))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "imp2core.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (pretty-print (compile-program expr) (current-output-port) 1)
     (newline))))
