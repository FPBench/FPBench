#lang racket

(require "common.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt" "range-analysis.rkt")
(provide
  (contract-out
   [fix-file-name (-> string? string?)]
   [round-decimal (-> rational? exact-positive-integer? (or/c 'up 'down) rational?)]
   [format-number (->* (rational?)
                       (#:digits exact-positive-integer?
                        #:direction (or/c #f 'up 'down))
                       string?)]

   [free-variables (-> expr? (listof symbol?))]
   [unused-variables (-> expr? (listof symbol?))]
   [precondition-ranges (->* (expr?) (#:single-range boolean?) expr?)]
   [all-subexprs (->* (expr?)
                      (#:no-vars boolean? #:no-consts boolean?)
                      (listof expr?))]

   [split-expr (-> symbol? expr? (listof expr?))]
   [to-dnf (-> expr? expr?)]

   [remove-let (->* (expr?) ((dictof symbol? expr?)) expr?)]
   [canonicalize (->* (expr?) (#:neg boolean?) expr?)]
   [unroll-loops (-> exact-nonnegative-integer? expr? expr?)]
   [skip-loops (-> expr? expr?)]
   [expand-let* (-> expr? expr?)]
   [expand-while* (-> expr? expr?)]
   [expand-for (-> expr? expr?)]

   [fpcore-remove-let (-> fpcore? fpcore?)]
   [fpcore-unroll-loops (-> exact-nonnegative-integer? fpcore? fpcore?)]
   [fpcore-skip-loops (-> fpcore? fpcore?)]
   [fpcore-expand-let* (-> fpcore? fpcore?)]
   [fpcore-expand-while* (-> fpcore? fpcore?)]
   [fpcore-expand-for (-> fpcore? fpcore?)]
   [fpcore-override-arg-precision (-> symbol? fpcore? fpcore?)]
   [fpcore-override-props (-> (listof symbol?) fpcore? fpcore?)]
   [fpcore-name (->* (fpcore?) ((or/c #f string?)) (or/c #f string?))]
   
   [fpcore-all-subexprs (->* (fpcore?)
                             (#:no-vars boolean? #:no-consts boolean?)
                             (listof fpcore?))]
   [fpcore-precondition-ranges (->* (fpcore?) (#:single-range boolean?) fpcore?)]
   [fpcore-split-intervals (->* (exact-nonnegative-integer? fpcore?)
                                (#:max exact-nonnegative-integer?)
                                (listof fpcore?))]
   [fpcore-split-or (-> fpcore? (listof fpcore?))]
   [fpcore-transform (->* (fpcore?)
                          (#:var-precision (or/c #f symbol?)
                           #:override-props (or/c #f (listof symbol?))
                           #:unroll (or/c #f exact-nonnegative-integer?)
                           #:split (or/c #f exact-nonnegative-integer?)
                           #:split-or boolean?
                           #:subexprs boolean?)
                           (listof fpcore?))]

   [pretty-expr (-> expr? string?)]
   [pretty-fpcore (-> fpcore? string?)]))


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

(define (round-decimal n digits direction)
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
(define (format-number n #:digits [digits 16] #:direction [direction #f])
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
(define (variable-info expr)
  ; name -> unused? free?
  (define varhash (make-hash))

  ; push on a new variable entry
  (define (add-var var)
    (let ([cell (box (cons #t #f))])
      (hash-update! varhash var (curry cons cell) (list))
      cell))

  ; set variable as used, missing key means variable is also free
  (define (unmark-var! dict var)
    (dict-update dict var
                 (λ (x) (let ([v (unbox x)]) (set-box! x (cons #f (cdr v)))))
                 (lambda () (let ([b (add-var var)]) (begin0 b (set-box! b (cons #t #t)))))))

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
  varhash)

; only return names
(define (free-variables expr)
  (for/fold ([free '()] #:result (remove-duplicates free))
            ([(var cells) (in-hash (variable-info expr))])
    (append free
      (for/list ([b (in-list (reverse cells))] [i (in-naturals 1)]
                #:when (cdr (unbox b)))
        var))))

; only return names
(define (unused-variables expr)
  (define info (variable-info expr))
  (for/fold ([free '()] #:result (remove-duplicates free))
            ([(var cells) (in-hash (variable-info expr))])
    (append free
      (for/list ([b (in-list (reverse cells))] [i (in-naturals 1)]
                #:when (car (unbox b)))
        var))))

(define (remove-let expr [bindings '()])
  (define/transform-expr (rec expr ctx)
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
  (rec expr bindings))

(define (fpcore-remove-let prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(remove-let body))]
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(remove-let body))]))

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
     `(,let_ ,(for/list ([var vars] [val vals]) (list var (visit/ctx vtor val #f)))
             ,(visit/ctx vtor body neg))]
    [(visit-op vtor op args #:ctx [neg #f])
      (match* (op args)
       [((or '== '< '> '<= '>=) _)
        (define args* (for/list ([arg args]) (visit/ctx vtor arg #f)))
        (define op* (if neg (negate-cmp op) op))
        (define conj (if neg 'or 'and))
        (for/fold ([expr* (if neg 'FALSE 'TRUE)]) ([a args*] [b (cdr args*)])
          (if (constant? expr*) `(,op* ,a ,b) `(,conj ,expr* (,op* ,a ,b))))]
       [('!= _)
        (define args* (for/list ([arg args]) (visit/ctx vtor arg #f)))
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

(define (split-expr op expr)
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

; Converts the given logical expression into an equivalent DNF
(define (to-dnf expr)
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

; Returns a list of subexpressions for the given expression
(define (all-subexprs expr #:no-vars [no-vars #f] #:no-consts [no-consts #f])
  (define/visit-expr (rec expr)
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
     `(,(list cond ift iff) ,(visit vtor ift) ,(visit vtor iff))]
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
      (define val-subexprs (append-map (curry visit vtor) vals))
      (define body-subexprs
        (for/list ([e (visit vtor body)])
          (define free-vars (free-variables e))
          (define bindings
            (for/list ([var vars] [val vals] #:when (member var free-vars))
              (list var val)))
          (if (null? bindings) e `(,let_ ,bindings ,e))))
      (append body-subexprs val-subexprs)]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
      (error 'all-subexprs "Unsupported operation: ~a" while_)]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [ctx '()])
      (error 'all-subexprs "Unsupported operation: ~a" for_)]
    [(visit-tensor vtor vars vals body #:ctx [ctx '()])
      (error 'all-subexprs "Unsupported operation: tensor" )]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [ctx '()])
      (error 'all-subexprs "Unsupported operation: tensor*")]
    [(visit-op_ vtor op args #:ctx [ctx '()])
      (when (set-member? '(< > <= >= == != and or not !) op)
        (error 'all-subexprs "Unsupported operation: ~a" op))
      (cons (cons op args) (append-map (curry visit vtor) args))]
    [(visit-call vtor func args #:ctx [ctx '()])
      (cons (cons func args) (append-map (curry visit vtor) args))]
    [(visit-symbol vtor x #:ctx [ctx '()])
      (if no-vars '() (list x))]
    [(visit-terminal_ vtor x #:ctx [ctx '()])
      (if no-consts '() (list x))])

  (remove-duplicates (rec expr) #:key remove-let))

; Returns FPCore programs for all subexpressions
(define (fpcore-all-subexprs prog #:no-vars [no-vars #f] #:no-consts [no-consts #f])
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

; unrolls loops n times
; TODO: for loops?
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

  ; Unrolls loops in the given FPCore program
(define (fpcore-unroll-loops n prog)
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (if name
    `(FPCore ,name ,args ,@props ,(unroll-loops n body))
    `(FPCore ,args ,@props ,(unroll-loops n body))))

(define/transform-expr (skip-loops expr)
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
    (define let_ (match while_ ['while 'let] ['while* 'let*]))
   `(,let_ ,(for/list ([var vars] [init inits]) (list var (visit vtor init)))
           ,(visit vtor body))])

; Skips loops in the given FPCore program
(define (fpcore-skip-loops prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(skip-loops body))]   
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(skip-loops body))]))

(define/transform-expr (expand-let* expr)
  [(visit-let* vtor vars vals body #:ctx [ctx '()])
    (let loop ([vars vars] [vals vals])
      (cond
       [(null? vars) (visit vtor body)]
       [else `(let ((,(car vars) ,(visit vtor (car vals))))
                ,(loop (cdr vars) (cdr vals)))]))])

; Expand let* in the body of the given FPCore program
(define (fpcore-expand-let* prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(expand-let* body))]   
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(expand-let* body))]))

(define/transform-expr (expand-while* expr)
  [(visit-while* vtor cond vars inits updates body #:ctx [ctx '()])
   `(while ,(visit vtor cond)
           ,(for/list ([var vars])
              (list var
                   `(let* ,(for/list ([var* vars] [init inits])
                            (list var* (visit vtor init)))
                          ,var)
                   `(let* ,(for/list ([var* vars] [update updates])
                            (list var* (visit vtor update)))
                          ,var)))
           ,(visit vtor body))])

; Expand while* in the body of the given FPCore program
(define (fpcore-expand-while* prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(expand-while* body))]   
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(expand-while* body))]))

(define/transform-expr (expand-for expr)
  [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [ctx '()])
    (cond
     [(null? vars)
      (define let_ (match for_ ['for 'let] ['for* 'let*]))
      `(,let_ ,(for/list ([accum accums] [init inits])
                (list accum (visit/ctx vtor init ctx)))
              ,(visit/ctx vtor body ctx))]
     [(= (length vars) 1)
      (define while_ (match for_ ['for 'while] ['for* 'while*]))
      `(,while_ (< ,(car vars) ,(car vals))
                ,(for/list ([accum accums] [init inits] [update updates])
                  (list accum
                        (visit/ctx vtor init ctx)
                        (visit/ctx vtor update ctx)))
                ,(visit/ctx vtor body ctx))]
     [else  ; multi-indexed for loops can't be desugared
      `(,for_ ,(for/list ([var vars] [val vals]) (list var (visit/ctx vtor val ctx)))
              ,(for/list ([accum accums] [init inits] [update updates])
                (list accum
                      (visit/ctx vtor init ctx)
                      (visit/ctx vtor update ctx)))
              ,(visit/ctx vtor body ctx))])])

; Expand for/for* in the body of the given FPCore program
(define (fpcore-expand-for prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(expand-for body))]   
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(expand-for body))]))

(define (precondition-ranges pre #:single-range [single-range #f])
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

; Converts preconditions to ranges in the given FPCore program
(define (fpcore-precondition-ranges prog #:single-range [single-range #f])
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

; Uniformly splits input intervals of all bounded variables into n parts.
; The total number of results is limited by the #:max parameter.
(define (fpcore-split-intervals n prog #:max [max 10000])
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

; Transforms preconditions into DNF and returns FPCore programs for all conjunctions
(define (fpcore-split-or prog)
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

; Adds (! :precision prec) to all arguments
(define (fpcore-override-arg-precision prec prog)
  (define/match (transform-arg arg)
    [((list '! props ... name)) `(! ,@(append props `(:precision ,prec)) ,name)]
    [(name) `(! :precision ,prec ,name)])
  (match prog
    [(list 'FPCore (list args ...) props ... body)
      `(FPCore ,(map transform-arg args) ,@props ,body)]
    [(list 'FPCore name (list args ...) props ... body)
      `(FPCore ,name ,(map transform-arg args) ,@props ,body)]))

; Adds given properties to the core
(define (fpcore-override-props new-props prog)
  (match prog
   [(list 'FPCore (? list? args) props ... body)
    `(FPCore ,args ,@props ,@new-props ,body)]
   [(list 'FPCore name (? list? args) props ... body)
    `(FPCore ,name ,args ,@props ,@new-props ,body)]))

(define (fpcore-transform prog
                          #:var-precision [var-precision #f]
                          #:override-props [override-props #f]
                          #:unroll [unroll #f]
                          #:split [split #f]
                          #:split-or [split-or #f]
                          #:subexprs [subexprs #f])
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

(define (fpcore-name prog [default-name #f])
  (define-values (name args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define-values (_ properties) (parse-properties props))
  (cond
    [(dict-has-key? properties ':name) (dict-ref properties ':name)]
    [default-name]
    [else #f]))

;; Pretty formatter

(define (pretty-props props)
  (for/list ([(prop name) (in-dict (apply dict-set* '() props))])
    (format "~a ~s" prop name)))

(define/transform-expr (pretty-expr-helper expr) ; don't call pretty-format twice
  [(visit-! vtor props body #:ctx [ctx '()])
   `(! ,@(pretty-props props) ,(visit/ctx vtor body ctx))])

(define (pretty-expr expr)
  (pretty-format (pretty-expr-helper expr) #:mode 'display))

(define (pretty-fpcore core)
  (define-values (name args props* body)
    (match core
     [(list 'FPCore name (list args ...) props ... body)
      (values name args props body)]
     [(list 'FPCore (list args ...) props ... body)
      (values #f args props body)]))
  (pretty-format `(,(if name (format "FPCore ~a ~a" name args) (format "FPCore ~a" args))
                  ,@(pretty-props props*)
                   ,(pretty-expr-helper body))
                 #:mode 'display))


(module+ test
  (require rackunit)

  (parameterize ([read-decimal-as-inexact #f])
    (for ([n '(1 10/3 -2.71 -3.1e-5 #e1e+100 1e+100 -7/2 0.1)])
      (check-equal?
       (string->number (string-trim (format-number n) #px"[()]"))
       (inexact->exact n))))

  (check-equal?
   (free-variables '(let ([x 1] [y 2]) (+ (* x y) a)))
    '(a))

  (check-equal? 
    (unused-variables '(let ([x 1] [y 2]) (+ 1 2)))
    '(y x))

  (check-equal? 
    (unused-variables '(let ([x 1] [y 2]) (+ x 2)))
    '(y))

  (check-equal? 
    (unused-variables '(let ([x 1] [y 2])
                      (let ([x y] [y x])
                       (+ x 1))))
    '(y))

  (check-equal? 
    (unused-variables '(let* ([x 1] [y x]) 1))
    '(y))

  (check-equal? 
    (unused-variables '(while TRUE ([i 0 (+ i 1)] [j 1 (+ i 1)]) 0))
    '(j))

  (check-equal? 
    (unused-variables '(while* TRUE ([i 0 (+ i 1)] [j i (+ i 1)]) 0))
    '(j))

  (check-equal? 
    (unused-variables '(for ([i 10]) ([m 0 0]) 1))
    '(m))

  (check-equal? 
    (unused-variables '(for* ([i 10]) ([m 0 (+ i 1)]) 1))
    '(m))

  (check-equal? 
    (unused-variables '(tensor ([i 3] [j 3]) (if (== i 1) 1 0)))
    '())

  (check-equal? 
    (unused-variables '(tensor* ([i 10]) ([m 0 (+ i 1)]) 1))
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
    (expand-for '(for () ([x 0 (+ x 1)]) x))
    '(let ([x 0]) x))

  (check-equal?
    (expand-for '(for ([i n]) ([x 0 (+ x 1)]) x))
    '(while (< i n) ([x 0 (+ x 1)]) x))

  (check-equal? ; no expansion
    (expand-for '(for ([i n] [j n]) ([x 0 (+ x 1)]) x))
    '(for ([i n] [j n]) ([x 0 (+ x 1)]) x))

  (check-equal?
   (split-expr 'or '(or (and a b) (or (and c d) x)))
   '((and a b) (and c d) x))

)
