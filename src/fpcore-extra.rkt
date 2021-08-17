#lang racket

(require "common.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt" "range-analysis.rkt")
(provide
  ; contractless first
  fix-file-name round-decimal format-number
  split-expr to-dnf all-subexprs skip-loops
  expand-let* expand-while* precondition-ranges
  fpcore-split-or fpcore-all-subexprs fpcore-split-intervals
  fpcore-unroll-loops fpcore-skip-loops
  fpcore-expand-let* fpcore-expand-while* fpcore-precondition-ranges
  fpcore-override-props fpcore-override-arg-precision
  fpcore-transform
  fpcore-name
  (contract-out
   [free-variables (-> expr? (listof symbol?))]
   [remove-let (->* (expr?) ((dictof symbol? expr?)) expr?)]
   [canonicalize (->* (expr?) (#:neg boolean?) expr?)]
   [unroll-loops (-> exact-nonnegative-integer? expr? expr?)]))

(define (fix-file-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_.,-]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
   ""))

(define (factor n k)
  (if (or (= n 0) (< k 2))
      (values n 0)
      (let loop ([n (inexact->exact n)] [e 0])
        (define-values (q r) (quotient/remainder n k))
        (if (= r 0)
            (loop q (+ e 1))
            (values n e)))))

(define/contract (round-decimal n digits direction)
  (-> rational? exact-positive-integer? (or/c 'up 'down) rational?)
  (case (sgn n)
    [(0) 0]
    [(-1) (- (round-decimal (- n) digits (if (eq? direction 'up) 'down 'up)))]
    [else
     (let* ([t (inexact->exact n)]
            [e (- digits (add1 (order-of-magnitude t)))]
            [p (expt 10 e)]
            [t2 (* t p)]
            [r (exact-floor t2)])
       (if (= r t2) t
           (/ (if (eq? direction 'up) (add1 r) r) p)))]))

; TODO: use (order-of-magnitude n) from racket/math to get normalized results
(define/contract (format-number n #:digits [digits 16] #:direction [direction #f])
  (->* (rational?)
       (#:digits exact-positive-integer?
        #:direction (or/c #f 'up 'down))
       string?)
  (define t (inexact->exact n))
  (let*-values ([(d e10) (factor (denominator t) 10)]
                [(d e5) (factor d 5)]
                [(d e2) (factor d 2)])
    (cond
      [(= t 0) (~a t)]
      [(> d 1) (if direction
                   (format-number (round-decimal t digits direction))
                   (format "(~a)" t))]
      [else
       (let*-values ([(m) (if (> e2 e5) (expt 5 e2) (expt 2 e5))]
                     [(n en10) (factor (* (numerator t) m) 10)]
                     [(e) (- en10 (+ e2 e5 e10))])
         (cond
           [(<= 0 e 3)
            (format "~a~a" n (make-string e #\0))]
           [else (format "~ae~a" n e)]))])))

;; more powerful version
(define (free-variables* expr)
  (define varhash (make-hash))

  (define (add-var var)
    (let ([cell (box #t)])
      (hash-update! varhash var (curry cons cell) (list))
      cell))

  (define (unmark-var! dict var)
    (when (dict-has-key? dict var)  ; do nothing on miss
      (dict-update dict var (curryr set-box! #f))))

  (define/visit-expr (search! visit ctx)
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
      (visit/ctx vtor cond ctx)
      (visit/ctx vtor ift ctx)
      (visit/ctx vtor iff ctx)]
    [(visit-let vtor vars vals body #:ctx [ctx '()])
      (for ([val vals]) (visit/ctx vtor val ctx))
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars])
          (dict-set ctx var (add-var var))))
      (visit/ctx vtor body ctx*)]
    [(visit-let* vtor vars vals body #:ctx [ctx '()])
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars] [val vals])
          (visit/ctx vtor val ctx)
          (dict-set ctx var (add-var var))))
      (visit/ctx vtor body ctx*)]
    [(visit-while vtor cond vars inits updates body #:ctx [ctx '()])
      (for ([init inits]) (visit/ctx vtor init ctx))
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars])
          (dict-set ctx var (add-var var))))
      (for ([update updates]) (visit/ctx vtor update ctx*))
      (visit/ctx vtor cond ctx*)
      (visit/ctx vtor body ctx*)]
    [(visit-while* vtor cond vars inits updates body #:ctx [ctx '()])
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars] [init inits])
          (visit/ctx vtor init ctx)
          (dict-set ctx var (add-var var))))
      (for ([update updates]) (visit/ctx vtor update ctx*))
      (visit/ctx vtor cond ctx*)
      (visit/ctx vtor body ctx*)]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [ctx '()]) ; convert to while loop
      (define while_ (if (equal? for_ 'for) 'while 'while*))
      (visit/ctx vtor
                `(,while_ (and ,@(map (curry list '<) vars vals))
                          (,@(map (λ (v) (list v 0 `(+ ,v 1))) vars)
                           ,@(map list accums inits updates))
                          ,body)
                 ctx)]
    [(visit-tensor vtor vars vals body #:ctx [ctx '()])
      (define ctx*    ; iteration variables are automatically bound
        (for/fold ([ctx ctx]) ([var vars])
          (dict-set ctx var (add-var var))))
      (for ([var vars]) (unmark-var! ctx* var))
      (visit/ctx vtor body ctx*)]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [ctx '()])
      (define ctx*    ; iteration variables are automatically bound
        (for/fold ([ctx ctx]) ([var vars])
          (dict-set ctx var (add-var var))))
      (for ([var vars]) (unmark-var! ctx* var))
      (define ctx**
        (for/fold ([ctx ctx*]) ([accum accums] [init inits])
          (visit/ctx vtor init ctx)
          (dict-set ctx accum (add-var accum))))
     (for ([update updates]) (visit/ctx vtor update ctx**))
     (visit/ctx vtor body ctx**)]
    [(visit-op_ vtor op args #:ctx [ctx '()])
      (for ([arg args]) (visit/ctx vtor arg ctx))]
    [(visit-symbol vtor x #:ctx [ctx '()])
      (unmark-var! ctx x)])
  
  (search! expr '())
  (for/fold ([free '()]) ([(var counts) (in-hash varhash)])
    (append free
      (for/list ([free? (in-list (reverse counts))] [i (in-naturals 1)] #:when (unbox free?))
        (cons var i)))))

; only return names
(define (free-variables expr)
  (remove-duplicates (map car (free-variables* expr))))

(define (remove-let expr [bindings '()])
  (define/transform-expr (visit expr ctx)
    [(visit-let vtor vars vals body #:ctx [ctx '()])
      (define vals* (for/list ([val vals]) (visit/ctx vtor val ctx)))
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars] [val vals*])
          (dict-set ctx var val)))
      (visit/ctx vtor body ctx*)]
    [(visit-let* vtor vars vals body #:ctx [ctx '()])
      (define ctx*
        (for/fold ([ctx ctx]) ([var vars] [val vals])
          (dict-set ctx var (visit/ctx vtor val ctx))))
      (visit/ctx vtor body ctx*)]
    [(visit-op_ vtor op args #:ctx [ctx '()])
     `(,op ,@(for/list ([arg args]) (visit/ctx vtor arg ctx)))]
    [(visit-symbol vtor x #:ctx [ctx '()])
      (dict-ref ctx x x)])
  (visit expr bindings))

(define/match (negate-cmp cmp)
  [('==) '!=]
  [('!=) '==]
  [('<) '>=]
  [('>) '<=]
  [('<=) '>]
  [('>=) '<]
  [(_) (error 'negate-cmp "Unsupported operation ~a" cmp)])

;; Transforms multiple argument operations (comparisons, and, or) into binary operations.
;; Removes logical negations.
; TODO: negation of 'if and 'while
(define (canonicalize expr #:neg [neg #f])
  (define/transform-expr (->canon expr neg)
    [(visit-let_ vtor let_ vars vals body #:ctx [neg #f])
     `(,let_ ,(for/list ([var vars] [val vals]) (list var (visit vtor val)))
             ,(visit/ctx vtor body neg))]
    [(visit-op vtor op args #:ctx [neg #f])
      (match* (op args)
       [((or '== '< '> '<= '>=) _)
        (define args* (map (curry visit vtor) args))
        (define op* (if neg (negate-cmp op) op))
        (define conj (if neg 'or 'and))
        (for/fold ([expr* (if neg 'FALSE 'TRUE)]) ([a args*] [b (cdr args*)])
          (if (constant? expr*) `(,op* ,a ,b) `(,conj ,expr* (,op* ,a ,b))))]
       [('!= _)
        (define args* (map (curry visit vtor) args))
        (define op (if neg '== '!=))
        (define conj (if neg 'or 'and))
        (let loop ([args args*] [expr (if neg 'FALSE 'TRUE)])
          (if (<= (length args) 1)
              expr
              (loop (cdr args)
                    (for/fold ([expr expr]) ([b (cdr args)])
                      (if (constant? expr)
                         `(,op ,(car args) ,b)
                         `(,conj ,expr (,op ,(car args) ,b)))))))]
       [('not (list arg))
        (visit/ctx vtor arg (not neg))]
       [((or 'and 'or) (list arg))
        (visit/ctx vtor arg neg)]
       [('and (list args ... arg))
       `(,(if neg 'or 'and) ,(visit/ctx vtor (cons 'and args) neg)
                            ,(visit/ctx vtor arg neg))]
       [('or (list args ... arg))
       `(,(if neg 'and 'or) ,(visit/ctx vtor (cons 'or args) neg)
                            ,(visit/ctx vtor arg neg))]
       [(_ _)
       `(,op ,@(for/list ([arg args]) (visit/ctx vtor arg neg)))])]
    [(visit-symbol vtor x #:ctx [neg #f])
      (if neg `(not ,x) x)]
    [(visit-constant vtor x #:ctx [neg #f])
      (match x
       ['TRUE (if neg 'FALSE 'TRUE)]
       ['FALSE (if neg 'TRUE 'FALSE)]
       [_ x])])

  (->canon expr neg))

(define/contract (split-expr op expr)
  (-> symbol? expr? (listof expr?))
  (match expr
    [(list (? (symbols op)) args ...)
     (append-map (curry split-expr op) args)]
    [_ (list expr)]))

(define (all-combinations lists)
  (match lists
    [(? null?) '()]
    [(list h) (map (λ (x) (list x)) h)]
    [_ (define lists* (all-combinations (cdr lists)))
       (for*/list ([h (car lists)] [t lists*])
         (list* h t))]))

(define/contract (to-dnf expr)
  ; Converts the given logical expression into an equivalent DNF
  (-> expr? expr?)
  (match expr
    [(list (or 'let 'while) args ...)
     (error 'to-dnf "Unsupported operation ~a" expr)]
    [(or (? constant?) (? number?) (? symbol?)) expr]
    [`(if ,c ,t ,e)
     (to-dnf `(or (and (not ,c) (not ,e)) (and ,t ,c) (and ,t (not ,e))))]
    [`(not (not ,arg)) (to-dnf arg)]
    [`(not (or ,args ...)) (to-dnf `(and ,@(map (λ (e) (list 'not e)) args)))]
    [`(not (and ,args ...)) (to-dnf `(or ,@(map (λ (e) (list 'not e)) args)))]
    [`(or ,arg) (to-dnf arg)]
    [`(or ,args ...) `(or ,@(map to-dnf args))]
    [`(and ,arg) (to-dnf arg)]
; Alternative implementation without all-combinations:
;    [`(and ,a ,b ,args ...)
;     (define ts (for*/list ([x (split-expr 'or (to-dnf a))]
;                            [y (split-expr 'or (to-dnf `(and ,b ,@args)))])
;                  (list 'and x y)))
;     (if (= (length ts) 1)
;         (car ts)
;         (list* 'or ts))]
    [`(and ,args ...)
     (define lists (map (compose (curry split-expr 'or) to-dnf) args))
     (define ts (map (λ (t) (list* 'and t)) (all-combinations lists)))
     (if (= (length ts) 1)
         (car ts)
         (list* 'or ts))]
    [_ expr]))

(define/contract (all-subexprs expr #:no-vars [no-vars #f] #:no-consts [no-consts #f])
  ; Returns a list of subexpressions for the given expression
  (->* (expr?) (#:no-vars boolean? #:no-consts boolean?) (listof expr?))
  (remove-duplicates #:key remove-let
   (let loop ([expr expr])
     (match expr
       [(list (or '< '> '<= '>= '== '!= 'and 'or 'not 'while 'while* '!) args ...)
        (error 'all-subexprs "Unsupported operation: ~a" expr)]
       [(? symbol?) (if no-vars '() (list expr))]
       [(or (? number?) (? constant?)) (if no-consts '() (list expr))]
       [`(,(and (or 'let 'let*) let_) ([,vars ,vals] ...) ,body)
        (define val-subexprs (append-map loop vals))
        (define body-subexprs
          (map (λ (e)
                 (define free-vars (free-variables e))
                 (define bindings
                   (for/list ([var vars] [val vals] #:when (member var free-vars))
                     (list var val)))
                 (cond
                   [(null? bindings) e]
                   [else `(,let_ (,@bindings) ,e)]))
               (loop body)))
        (append body-subexprs val-subexprs)]
       [`(if ,cond ,t ,f)
        `(,expr ,@(loop t) ,@(loop f))]
       [(list op args ...)
        (cons expr (append-map loop args))]))))

(define/contract (fpcore-all-subexprs prog #:no-vars [no-vars #f] #:no-consts [no-consts #f])
  ; Returns FPCore programs for all subexpressions
  (->* (fpcore?) (#:no-vars boolean? #:no-consts boolean?) (listof fpcore?))
  (define-values (args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define-values (_ properties) (parse-properties props))
  (define name (dict-ref properties ':name "ex"))
  (define exprs (all-subexprs body #:no-vars no-vars #:no-consts no-consts))
  (for/list ([expr exprs] [k (in-naturals)])
    (define name* (if (>= k 1) (format "~a_expr~a" name k) name))
    (define props* (dict-set properties ':name name*))
    `(FPCore ,args ,@(unparse-properties props*) ,expr)))

; for loops?
(define (unroll-loops n expr)
  (define/transform-expr (unroll expr)
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
      (define let_ (match while_ ['while 'let] ['while* 'let*]))
      (define cond* (visit vtor cond))
      (define inits* (map (curry visit vtor) inits))
      (define updates* (map (curry visit vtor) updates))
      (define body* (visit vtor body))
      (let loop ([n n] [inits inits*] [updates updates*])
        (if (<= n 0)
           `(,while_ ,cond* ,(map list vars inits updates) ,body*)
           `(,let_ ,(map list vars inits)
              (if ,cond*
                  ,(loop (- n 1) updates updates)
                  ,body))))])
  (unroll expr))

(define/contract (fpcore-unroll-loops n prog)
  ; Unrolls loops in the given FPCore program
  (-> exact-nonnegative-integer? fpcore? fpcore?)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (if name
    `(FPCore ,name ,args ,@props ,(unroll-loops n body))
    `(FPCore ,args ,@props ,(unroll-loops n body))))

(define/contract (skip-loops expr)
  (-> expr? expr?)
  (match expr
    [`(,(and (or 'let 'let*) let_) ([,vars ,vals] ...) ,body)
     `(,let_ (,@(for/list ([var vars] [val vals]) (list var (skip-loops val))))
        ,(skip-loops body))]
    [`(if ,cond ,ift ,iff)
     `(if ,(skip-loops cond) ,(skip-loops ift) ,(skip-loops iff))]
    [`(,(and (or 'while 'while*) while_) ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (let ([let_ (match while_ ['while 'let] ['while* 'let*])])
       `(,let_ (,@(for/list ([var vars] [init inits]) (list var (skip-loops init))))
               ,retexpr))]
    [`(! ,props ... ,body)
     `(! ,@props ,(skip-loops body))]
    [`(,(? operator? op) ,args ...)
     (cons op (map skip-loops args))]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]))

(define/contract (fpcore-skip-loops prog)
  ; Skips loops in the given FPCore program
  (-> fpcore? fpcore?)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (if name
    `(FPCore ,name ,args ,@props ,(skip-loops body))
    `(FPCore ,args ,@props ,(skip-loops body))))

(define/contract (expand-let* expr)
  (-> expr? expr?)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     `(let (,@(for/list ([var vars] [val vals]) (list var (expand-let* val))))
        ,(expand-let* body))]
    [`(let* ([,vars ,vals] ...) ,body)
     (let loop ([vars vars] [vals vals])
       (match (list vars vals)
         [(list '() '()) (expand-let* body)]
         [(list (cons var rest-vars) (cons val rest-vals))
          `(let ((,var ,(expand-let* val)))
             ,(loop rest-vars rest-vals))]))]
    [`(if ,cond ,ift ,iff)
     `(if ,(expand-let* cond) ,(expand-let* ift) ,(expand-let* iff))]
    [`(,(and (or 'while 'while*) while_) ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     `(,while_ ,(expand-let* cond)
               (,@(for/list ([var vars] [init inits] [update updates])
                    (list var (expand-let* init) (expand-let* update))))
               ,(expand-let* retexpr))]
    [`(! ,props ... ,body)
     `(! ,@props ,(expand-let* body))]
    [`(,(? operator? op) ,args ...)
     (cons op (map expand-let* args))]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]))

(define/contract (fpcore-expand-let* prog)
  ; Expand let* in the body of the given FPCore program
  (-> fpcore? fpcore?)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (if name
    `(FPCore ,name ,args ,@props ,(expand-let* body))
    `(FPCore ,args ,@props ,(expand-let* body))))

(define/contract (expand-while* expr)
  (-> expr? expr?)
  (match expr
    [`(,(and (or 'let 'let*) let_) ([,vars ,vals] ...) ,body)
     `(,let_ (,@(for/list ([var vars] [val vals]) (list var (expand-while* val))))
        ,(expand-while* body))]
    [`(if ,cond ,ift ,iff)
     `(if ,(expand-while* cond) ,(expand-while* ift) ,(expand-while* iff))]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     `(while ,(expand-while* cond)
             (,@(for/list ([var vars] [init inits] [update updates])
                  (list var (expand-while* init) (expand-while* update))))
             ,(expand-while* retexpr))]
    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     `(while ,(expand-while* cond)
             (,@(for/list ([var vars])
                  (list var
                        `(let* (,@(for/list ([var* vars] [init* inits])
                                    (list var* (expand-while* init*))))
                               ,var)
                        `(let* (,@(for/list ([var* vars] [update* updates])
                                    (list var* (expand-while* update*))))
                               ,var))))
             ,(expand-while* retexpr))]
    [`(! ,props ... ,body)
     `(! ,@props ,(expand-while* body))]
    [`(,(? operator? op) ,args ...)
     (cons op (map expand-while* args))]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]))

(define/contract (fpcore-expand-while* prog)
  ; Expand let* in the body of the given FPCore program
  (-> fpcore? fpcore?)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (if name
    `(FPCore ,name ,args ,@props ,(expand-while* body))
    `(FPCore ,args ,@props ,(expand-while* body))))

(define/contract (precondition-ranges pre #:single-range [single-range #f])
  (->* (expr?) (#:single-range boolean?) expr?)
  (let* ([ranges ((compose condition->range-table canonicalize remove-let) pre)]
         [bounded-vars (for/list ([var-interval (in-dict-pairs ranges)]
                                  #:when (nonempty-bounded? (cdr var-interval)))
                         var-interval)])
    `(and
      ,@(for/list ([var-interval bounded-vars])
          (match-let ([(cons var intervals) var-interval])
            (if single-range
                (match-let ([(interval a _ a? _) (first intervals)]
                            [(interval _ b _ b?) (last intervals)])
                  (interval->condition (interval a b a? b?) var))
                `(or ,@(for/list ([int intervals])
                         (interval->condition int var)))))))))

(define/contract (fpcore-precondition-ranges prog #:single-range [single-range #f])
  ; Converts preconditions to ranges in the given FPCore program
  (->* (fpcore?) (#:single-range boolean?) fpcore?)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define-values (_ properties) (parse-properties props))
  (if (not (dict-has-key? properties ':pre))
      prog
      (let* ([new-pre (precondition-ranges (dict-ref properties ':pre) #:single-range single-range)]
             [new-properties (dict-set properties ':pre new-pre)])
        (if name
           `(FPCore ,name ,args ,@(unparse-properties new-properties) ,body)
           `(FPCore ,args ,@(unparse-properties new-properties) ,body)))))

(define/contract (fpcore-split-or prog)
  ; Transforms preconditions into DNF and returns FPCore programs for all conjunctions
  (-> fpcore? (listof fpcore?))
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define-values (_ properties) (parse-properties props))
  (if (not (dict-has-key? properties ':pre))
      (list prog)
      (let ([name (dict-ref properties ':name "ex")]
            [pre-list ((compose (curry split-expr 'or) to-dnf remove-let)
                       (dict-ref properties ':pre))])
        (define multiple-pre (> (length pre-list) 1))
        (for/list ([pre pre-list] [k (in-naturals)])
          (define name* (if multiple-pre (format "~a_case~a" name k) name))
          (define props* (dict-set* properties ':name name* ':pre pre))
          `(FPCore ,args ,@(unparse-properties props*) ,body)))))


(define/contract (fpcore-split-intervals n prog #:max [max 10000])
  ; Uniformly splits input intervals of all bounded variables into n parts.
  ; The total number of results is limited by the #:max parameter.
  (->* (exact-nonnegative-integer? fpcore?)
       (#:max exact-nonnegative-integer?)
       (listof fpcore?))
  (define-values (args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define-values (_ properties) (parse-properties props))
  (define name (dict-ref properties ':name "ex"))
  (if (not (dict-has-key? properties ':pre))
      (list prog)
      (let* ([pre (dict-ref properties ':pre)]
             [ranges ((compose condition->range-table canonicalize remove-let) pre)])
        (define bounded-vars
          (for/list ([var-interval (in-dict-pairs ranges)]
                     #:when (nonempty-bounded? (cdr var-interval)))
            var-interval))
        (define splits (let ([k (length bounded-vars)])
                         (if (> (expt n k) max) (exact-floor (expt max (/ k))) n)))
        (if (<= splits 1)
            (list prog)
            (let loop ([pre-list '()] [name* name] [vars bounded-vars])
              (cond
                [(null? vars)
                 (define pre* `(and ,@pre-list ,pre))
                 (define properties* (dict-set* properties ':pre pre* ':name name*))
                 `((FPCore ,args ,@(unparse-properties properties*) ,body))]
                [else
                 (match-define (list var (interval a b #t #t)) (car vars))
                 (define step (/ (- b a) splits))
                 (for/fold [(progs '())] [(i (in-range splits))]
                   (define new-pre `(<= ,(+ a (* i step)) ,var ,(+ a (* (+ i 1) step))))
                   (define new-name (format "~a_~a~a" name* var i))
                   (append progs
                           (loop (cons new-pre pre-list) new-name (cdr vars))))]))))))

(define/contract (fpcore-override-arg-precision prec prog)
  ; Adds (! :precision prec) to all arguments
  (-> symbol? fpcore? fpcore?)
  (define/match (transform-arg arg)
    [((list '! props ... name)) `(! ,@(append props `(:precision ,prec)) ,name)]
    [(name) `(! :precision ,prec ,name)])
  (match prog
    [(list 'FPCore (list args ...) props ... body)
      `(FPCore ,(map transform-arg args) ,@props ,body)]
    [(list 'FPCore name (list args ...) props ... body)
      `(FPCore ,name ,(map transform-arg args) ,@props ,body)]))

(define/contract (fpcore-override-props new-props prog)
  ; Adds given properties to the core
  (-> (listof symbol?) fpcore? fpcore?)
  (match prog
    [(list 'FPCore (? list? args) props ... body)
      `(FPCore ,args ,@props ,@new-props ,body)]
    [(list 'FPCore name (? list? args) props ... body)
      `(FPCore ,name ,args ,@props ,@new-props ,body)]))

(define/contract (fpcore-transform prog
                                   #:var-precision [var-precision #f]
                                   #:override-props [override-props #f]
                                   #:unroll [unroll #f]
                                   #:split [split #f]
                                   #:split-or [split-or #f]
                                   #:subexprs [subexprs #f])
  (->* (fpcore?)
       (#:var-precision (or/c #f symbol?)
        #:override-props (or/c #f (listof symbol?))
        #:unroll (or/c #f exact-nonnegative-integer?)
        #:split (or/c #f exact-nonnegative-integer?)
        #:split-or boolean?
        #:subexprs boolean?)
       (listof fpcore?))
  (define ((make-t cond f) progs)
    (if cond (append-map f progs) progs))
  (define transform
    (compose
     (make-t var-precision (compose list (curry fpcore-override-arg-precision var-precision)))
     (make-t override-props (compose list (curry fpcore-override-props override-props)))
     (make-t subexprs fpcore-all-subexprs)
     (make-t split (curry fpcore-split-intervals split))
     (make-t split-or fpcore-split-or)
     (make-t unroll (compose list (curry fpcore-unroll-loops unroll)))))
  (transform (list prog)))

(define/contract (fpcore-name prog [default-name #f])
  (->* (fpcore?)
       ((or/c #f string?))
       (or/c #f string?))
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define-values (_ properties) (parse-properties props))
  (cond
    [(dict-has-key? properties ':name) (dict-ref properties ':name)]
    [default-name]
    [else #f]))

(module+ test
  (require rackunit)

  (parameterize ([read-decimal-as-inexact #f])
    (for ([n '(1 10/3 -2.71 -3.1e-5 #e1e+100 1e+100 -7/2 0.1)])
      (check-equal?
       (string->number (string-trim (format-number n) #px"[()]"))
       (inexact->exact n))))

  (check-equal? 
    (free-variables '(let ([x 1] [y 2]) (+ 1 2)))
    '(y x))

  (check-equal? 
    (free-variables '(let ([x 1] [y 2]) (+ x 2)))
    '(y))

  (check-equal? 
    (free-variables '(let ([x 1] [y 2])
                      (let ([x y] [y x])
                       (+ x 1))))
    '(y))

  (check-equal? 
    (free-variables '(let* ([x 1] [y x]) 1))
    '(y))

  (check-equal? 
    (free-variables '(while TRUE ([i 0 (+ i 1)] [j 1 (+ i 1)]) 0))
    '(j))

  (check-equal? 
    (free-variables '(while* TRUE ([i 0 (+ i 1)] [j i (+ i 1)]) 0))
    '(j))

  (check-equal? 
    (free-variables '(for ([i 10]) ([m 0 0]) 1))
    '(m))

  (check-equal? 
    (free-variables '(for* ([i 10]) ([m 0 (+ i 1)]) 1))
    '(m))

  (check-equal? 
    (free-variables '(tensor ([i 3] [j 3]) (if (== i 1) 1 0)))
    '())

  (check-equal? 
    (free-variables '(tensor* ([i 10]) ([m 0 (+ i 1)]) 1))
    '(m))

  (check-equal?
   (remove-let '(let ([x (+ a b)]) (+ x a)))
   '(+ (+ a b) a))

  (check-equal?
   (remove-let '(let ([x (+ a b)] [y (* a 2)])
                  (let ([x (+ y x)] [a b])
                    (- x a))))
   '(- (+ (* a 2) (+ a b)) b))

  (check-equal?
   (remove-let '(let ([x (+ a b)])
                  (if (let ([y (- x 0)]) (== y x))
                      (- x (let ([z a]) z))
                      y)))
   '(if (== (- (+ a b) 0) (+ a b))
        (- (+ a b) a)
        y))

  (check-equal?
   (remove-let '(let* ([x (+ a b)] [y (+ a x)]) (* x y)))
   '(* (+ a b) (+ a (+ a b))))

  (check-equal?
   (canonicalize '(let ([x (+ a b)])
                    (if (<= 1 a x b)
                        (and (!= x a b) (== a 2 b) (<= a 4))
                        (> x a b 3))))
   '(let ([x (+ a b)])
      (if (and (and (<= 1 a) (<= a x)) (<= x b))
          (and (and (and (and (!= x a) (!= x b)) (!= a b)) (and (== a 2) (== 2 b))) (<= a 4))
          (and (and (> x a) (> a b)) (> b 3)))))

  (check-equal?
   (canonicalize '(not (and TRUE FALSE p)))
   '(or (or FALSE TRUE) (not p)))

  (check-equal?
   (canonicalize '(let ([p (<= x 2 y)])
                    (if (not p) 1 2)))
   '(let ([p (and (<= x 2) (<= 2 y))])
      (if (not p) 1 2)))

  (check-equal?
   (canonicalize '(not (let ([p (not (<= a b))])
                         (or p (!= a (+ b 1))))))
   '(let ([p (> a b)])
      (and (not p) (== a (+ b 1)))))

  (check-equal?
   (split-expr 'or '(or (and a b) (or (and c d) x)))
   '((and a b) (and c d) x))

)
