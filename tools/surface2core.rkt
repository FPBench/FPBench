#lang racket

(define (attribute? symb)
  (and (symbol? symb) (string-prefix? (symbol->string symb) ":")))

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
    [(list (and (or '< '> '<= '>= '==) op) fst snd)
     `(,op ,fst ,snd)]
    [(list (and (or '< '> '<= '>= '==) op) args ...)
     `(and ,@(for/list ([fst args] [snd (cdr args)]) `(,op ,fst ,snd)))]
    [(? list?) (cons (car body) (map canonicalize (cdr body)))]
    [(? symbol?) body]
    [(? number?) body]))

(define (substitute body bindings)
  (match body
    [(or 'E 'PI) body]
    [(? (λ (x) (assoc x bindings)))
     ;; The (if) handles the case when we don't have a value bound for the variable
     (if (null? (cdr (assoc body bindings)))
         body
         (cdr (assoc body bindings)))]
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
       (if (and (not (null? value))
                (or (ormap (curryr appears? var) bindings*)
                    (appears? (cons 'self value) var)))
           (cons (cons var value) (loop bindings* clean))
           (cons (list var) (loop bindings* clean)))]
      ['() clean])))

(define (merge-values conds vals)
  (if (andmap (curryr equal? (car vals)) vals)
      (car vals)
      (merge-values* conds vals)))

(define/match (merge-values* conds vals)
  [((list 'else ...) (list x _ ...)) x]
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
     (append (append-map assigned subs) (assigned rest))]
    ))

(define (compile-statements statements variables outexprs)
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

(define (compile-program body)
  (match-define `(function (,variables ...) ,lines ... (output ,outexprs ...)) body)

  (define-values (attributes statements)
    (let loop ([attrs '()] [stats '()] [lines lines])
      (cond
       [(null? lines) (values (reverse attrs) (reverse stats))]
       [(equal? (car lines) ':pre)
        (loop (cons (list (car lines) (canonicalize (cadr lines))) attrs) stats (cddr lines))]
       [(attribute? (car lines))
        (loop (cons (list (car lines) (cadr lines)) attrs) stats (cddr lines))]
       [else
        (loop attrs (cons (car lines) stats) (cdr lines))])))

  (define-values (out bindings) (compile-statements statements (map car variables) outexprs))
  `(lambda (,@variables) ,@(apply append attributes) ,out))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "surface-to-core.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (pretty-print (compile-program expr) (current-output-port) 1
)
     (newline))))
