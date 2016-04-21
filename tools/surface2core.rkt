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

(define/match (merge-values conds vals)
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
     (append (append-map assigned subs) (assigned rest))]
    ))

(define (compile-statements statements [bindings '((E) (PI))] [constants '()])
  (match statements
    ['() (values 'UNUSED (reverse bindings) constants)]
    [(list `(output ,vals ...))
     (values
      (canonicalize
       (cons '+ (map (λ (value) (substitute (canonicalize value) bindings)) vals)))
      bindings
      constants)]
    [(list `[= ,(? symbol? var) ,(? number? value)] rest ...)
     (compile-statements rest (cons (list var) bindings) (cons (cons var value) constants))]
    [(list `[= ,(? symbol? var) ,value] rest ...)
     (define value* (substitute (canonicalize value) bindings))
     (compile-statements rest (cons (cons var value*) bindings) constants)]
    [(list `(while ,cond ,substatements ...) rest ...)
     (define trashed (assigned substatements))
     (define-values (notc truec)
       (partition (λ (x) (member (car x) trashed)) constants))
     (define subb
       (append (map list trashed) bindings))

     (define-values (outexpr outb outc)
       (compile-statements substatements subb truec))

     (when (not (equal? outexpr 'UNUSED))
       (error "(while) loop cannot contain (output) expression."))
     
     (define binds-extension (drop outb (length subb)))
     
     (define cond-expr (substitute (canonicalize cond) subb))

     (define binds-extension*
       (for/list ([bind binds-extension] #:when (not (null? (cdr bind))))
         (define appears-in-rest
           (or (appears? cond-expr (car bind))
               (for/or ([other binds-extension] #:when (not (null? (cdr other))))
                 (appears? (cdr other) (car bind)))))
         (if appears-in-rest
             bind
             (list (car bind)))))

     (define loopvars
       (for/list ([bind binds-extension*] #:when (not (null? (cdr bind))))
         (define init-rec (assoc (car bind) (append notc bindings)))
         (define init (if (null? (cdr init-rec)) (car bind) (cdr init-rec)))
         `[,(car bind) ,init ,(cdr bind)]))

     (define newb
       (append (map list trashed) subb))

     (define-values (out endb endc)
       (compile-statements rest newb outc))

     (values `(while ,(canonicalize cond) ,loopvars ,out) endb endc)]
    [(list `(if [,conds ,stmtss ...] ...) rest ...)
     (define conds*
       (for/list ([cond conds])
         (substitute (canonicalize cond) bindings)))

     (define-values (outexprs outbs outcs)
       (for/lists (outexprs outbs outcs) ([stmts stmtss])
         (define-values (outexpr outb outc)
           (compile-statements stmts bindings constants))
         (values outexpr (drop outb (length bindings)) outc)))
     
     (when (not (andmap (curryr equal? 'UNUSED) outexprs))
       (error "(if) statement cannot contain (output) expression."))

     ;; We've now run every branch and have to merge them.
     (define constants* (set-intersect outcs))

     (define vars
       (remove-duplicates
        (append
         (append-map (curry map car) outbs)
         (apply append
                (for/list ([outc outcs])
                  (map car (set-subtract outc constants*)))))))

     (define joined
       (for/list ([var vars])
         (cons var
               (merge-values
                conds*
                (for/list ([outb outbs] [outc outcs])
                  (match* ((assoc var outb) (assoc var outc))
                    [((list _) (cons _ val)) val]
                    [((cons _ val) _) val]
                    [(_ _) var]))))))

     (values 'UNUSED (append joined bindings) constants*)]))

(define (compile-program body)
  (match-define `(function ,lines ...) body)

  (define-values (attributes statements)
    (let loop ([attrs '()] [stats '()] [lines lines])
      (cond
       [(null? lines) (values (reverse attrs) (reverse stats))]
       [(equal? (car lines) ':pre)
        (loop (cons (cons (car lines) (canonicalize (cadr lines))) attrs) stats (cddr lines))]
       [(attribute? (car lines))
        (loop (cons (cons (car lines) (cadr lines)) attrs) stats (cddr lines))]
       [else
        (loop attrs (cons (car lines) stats) (cdr lines))])))

  (define-values (out bs cs) (compile-statements statements))
  `(lambda (,@(for/list ([c cs]) `[,(car c) ,(cdr c)]))
     ,@(apply append (for/list ([pair attributes]) (list (car pair) (cdr pair))))
     ,out))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "surface-to-core.rkt"
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (pretty-print (compile-program expr) (current-output-port) 1)
     (newline))))
