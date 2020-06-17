#lang racket

(require racket/extflonum)
(require "common.rkt" "tensor.rkt")
(provide *fpcores* *check-types* *ragged-check*
         fpcore? expr? argument? check-fpcore)

(define *fpcores* (make-parameter '()))  ; previously run fpcores, dictionary value is a list (core, in-types, rtype)
(define *check-types* (make-parameter #f))
(define *ragged-check* (make-parameter #f))
(define *dim-sizes* (make-parameter #f))

(define/contract (fpcore? thing)
  contract?
  (match thing
    [`(FPCore (,(? argument?) ...) ,props ... ,(? expr?)) (properties? props)]
    [`(FPCore ,(? symbol?) (,(? argument?) ...) ,props ... ,(? expr?)) (properties? props)]
    [_ false]))

(define (properties? props)
  (define-values (rest props*) (parse-properties props))
  (null? rest))

(define (argument? arg)
  (match arg
    [(? symbol? arg) true]
    [`(! ,props ... ,(? symbol?))
     (properties? props)]
    [`(,(? symbol?) ,(or (? number?) (? symbol?)) ...) true]
    [_ false]))

(define (expr? expr)
  (match expr
    [(? number?) true]
    [(? extflonum?) true]
    [(? constant?) true]
    [(? tensor?) true]
    [(? symbol?) true]
    [(list (? operator?) (? expr?) ...) true]
    [(list (? symbol?) (? expr?) ...) true]     ; fpcore calling
    [`(if ,(? expr?) ,(? expr?) ,(? expr?)) true]
    [`(,(or 'let 'let*) ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(,(or 'while 'while*) ,(? expr?) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(tensor ([,(? symbol?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(tensor* ([,(? symbol?) ,(? expr?)] ...) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(,(or 'for 'for*) ([,(? symbol?) ,(? expr?)] ...) ([,(? symbol?) ,(? expr?) ,(? expr?)] ...) ,(? expr?)) true]
    [`(cast ,(? expr?)) true]
    [`(! ,props ... ,(? expr?)) (properties? props)]
    [`(digits ,(? number?) ,(? number?) ,(? number?)) true]
    [_ false]))

;; Types

(define fpcore-types '(boolean real tensor))
(define typename? (apply symbols fpcore-types))

(define/contract (type? type)
  (-> list? boolean?)
  (typename? (first type)))

(define (typename-equal? type1 name)
  (-> type? typename? boolean?)
  (equal? (first type1) name))

(define (make-type name . data)
  (-> typename? list? type?)
  (list* name data))

;; Ragged tensor check

(define (subst key val)
  (define changed? #f)
  (for ([key2 (hash-keys (*dim-sizes*))])
    (let* ([val2 (hash-ref (*dim-sizes*) key2)]
           [idx-of-key (index-of val2 key)])
      (when idx-of-key 
        (let ([new-list (list-set val2 idx-of-key val)])
          (set! changed? #t)
          (hash-set*! (*dim-sizes*) key2 new-list)))))
  changed?)

(define (subst-elim)
  (let loop ()
    (define changed? #f)
    (for ([key (hash-keys (*dim-sizes*))])    ; substitute
      (let ([val (hash-ref (*dim-sizes*) key)])
        (when (= (length val) 1)
          (set! changed? (or changed? (subst key (first val)))))))
    (for ([key (hash-keys (*dim-sizes*))])    ; eliminate
      (let ([val (hash-ref (*dim-sizes*) key)])
        (hash-set*! (*dim-sizes*) key (remove-duplicates val))))
    (when changed? (loop))))

(define (add-equations eqs [var? symbol?] [val? number?])
  (define eqs*
    (filter
      (λ (x)
        (and
          (not (empty? x))
          (not (hash-has-key? (*dim-sizes*) (cdr x)))))
      (for/list ([eq eqs])
       (match eq
        [(list (? number? num1) (? number? num2))
          (unless (equal? num1 num2)
            (error 'add-equations "Dimension size mismatch. ~a != ~a" num1 num2 ))
          '()]
        [(list x x) '()]    
        [(list (? symbol? var) (? number? num)) (cons var num)]
        [(list (? number? num) (? symbol? var)) (cons var num)]
        [(list (? symbol? var1) (? symbol? var2)) (cons var1 var2)]))))
  (for ([eq eqs*])
    (cond
     [(hash-has-key? (*dim-sizes*) (car eq))
      (let ([val (hash-ref (*dim-sizes*) (car eq))])
        (hash-set*! (*dim-sizes*) (car eq) (cons (cdr eq) val)))]
     [else (hash-set*! (*dim-sizes*) (car eq) (list (cdr eq)))]))
  (when (not (empty? eqs*))
    (subst-elim)))

(define (check-sizes tensors)
  (define tensors* (map (λ (x) (drop (take x (sub1 (length x))) 2)) tensors))
  (define dim (length (first tensors*)))
  (add-equations
    (for/fold ([eqs '()]) ([i (in-range dim)])
      (append 
        eqs
        (combinations 
          (build-list (length tensors*) (λ (x) (list-ref (list-ref tensors* x) i))) 
          2)))))

(define (update-sizes types [dim-sizes (*dim-sizes*)])
  (for/list ([type types])
    (if (typename-equal? type 'tensor)
      (let* ([dim (second type)]
             [sizes (drop (take type (sub1 (length type))) 2)]
             [scalar (last type)]
             [sizes*
              (for/list ([size sizes])
                (if (and (hash-has-key? dim-sizes size) (= (length (hash-ref dim-sizes size)) 1))
                  (first (hash-ref dim-sizes size))
                  size))])
       `(tensor ,dim ,@sizes* ,scalar))       
      type)))

(define (reassign-sizes eqs)
  (for/fold ([eqs* '()]) ([key (dict-keys eqs)])
    (append eqs* (combinations (dict-ref eqs key) 2))))

;; Ragged check when function calling, returns a updated return type in terms of new dimension size variables
(define (check-rtype in-types args size-hash rtype)
  (define in-types* (filter (curryr typename-equal? 'tensor) in-types))
  (define args* (filter (curryr typename-equal? 'tensor) args))
  (define in-sizes (map (λ (x) (drop (take x (sub1 (length x))) 2)) in-types*))
  (define arg-sizes (map (λ (x) (drop (take x (sub1 (length x))) 2)) args*))

  (define eqs
    (for/fold ([eqs '()]) ([in-size in-sizes] [arg-size arg-sizes])
      (for/fold ([eqs* eqs]) ([in in-size] [arg arg-size])
        (cond
         [(number? in) eqs*]
         [(dict-has-key? eqs* in) (dict-set* eqs* in (cons arg (dict-ref eqs* in)))]
         [else (dict-set* eqs* in (list arg))]))))
  (define eqs* (reassign-sizes eqs))
  (add-equations eqs* (curry hash-has-key? (*dim-sizes*)) symbol?)

  (define update-hash
    (for/hash ([key (dict-keys eqs)])
      (values key (filter (λ (x) (not (dict-has-key? eqs* x))) (dict-ref eqs key)))))
  (first (update-sizes (list rtype) update-hash)))

(define (check-precond expr)    ; TODO: support '!='
 (match expr
  [(list 'and (? expr? subs) ...)
    (for/and ([i subs]) (check-precond i))]
  [(list '== (? expr? subs) ...)
    (when (for/or ([i subs]) (symbol? i))
      (add-equations (combinations subs 2)))
    #t]
  [_ #f]))

;; input types -> out type

(define/match (operator-type* op args)
  [((or '- 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt
        'cbrt 'sin 'cos 'tan 'asin 'acos 'atan 'sinh 'cosh 'tanh
        'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 'ceil 'floor
        'trunc 'round 'nearbyint 'cast)
    (list '(real)))
   '(real)]
  [((or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
    (list '(real) '(real)))
   '(real)]
  [('fma (list '(real) '(real) '(real))) '(real)]
  [((or '< '> '<= '>= '== '!=) (list '(real) ...)) '(boolean)]
  [((or 'isfinite 'isinf 'isnan 'isnormal 'signbit) (list '(real))) '(boolean)]
  [((or 'and 'or) (list '(boolean) ...)) '(boolean)]
  [('not (list '(boolean))) '(boolean)]
  [('array (list (? (curryr typename-equal? 'real) elems) ...)) `(tensor 1 ,(length elems) real)]
  [('array (list (? (curryr typename-equal? 'boolean) elems) ...)) `(tensor 1 ,(length elems) boolean)]
  [('array (list (? (curryr typename-equal? 'tensor) elems) ...))
    (cond
      [(for/or ([i (cdr elems)]) (not (equal? (second i) (second (car elems)))))
        (error 'operator-type* "Dimensionally inconsistent elements not allowed for 'array'")]
      [(for/and ([i (cdr elems)]) (equal? (last i) (last (car elems))))
        (when (*ragged-check*) (check-sizes elems))
       `(tensor ,(add1 (second (car elems))) ,(length elems) ,@(drop (car elems) 2))]
      [else #f])]
  [('dim (list (? (curryr typename-equal? 'tensor)))) '(real)]
  [('size (list (? (curryr typename-equal? 'tensor)) '(real) ...)) '(real)]
  [('ref (list (list tensor (? integer? n) (or (? integer? s) (? symbol? s)) ... (or (? typename? types) (? (listof typename?) types)))
               (? (curryr typename-equal? 'real) sizes) ...))  
    (let ([d (- n (length sizes))])
      (cond [(negative? d) (error 'operator-type* "Ref out of bounds")]
            [(zero? d) (if ((listof typename?) types) (map list types) (list types))]
            [else `(tensor ,d ,@(drop s (length sizes)) ,types)]))]
  [(_ _) #f])

;; optimistic type checker. Returns every valid return type
(define (operator-type op args)  
  (define types (for/list ([arg args])
                  (match arg
                    [(? type?) (list arg)]
                    [`(,(? type?) ...) arg])))
  (define arg-coords (apply cartesian-product types))
  (define rtypes
    (for/fold ([rtypes '()]) ([args* arg-coords])
      (let ([out (operator-type* op args*)])
        (if out
            (append rtypes (list out))
            rtypes))))
  (cond
    [(empty? rtypes) #f]
    [(= (length rtypes) 1) (first rtypes)]
    [else rtypes]))

(define (type-match? t1 t2)
  (define t1* (if (type? t1) (list t1) t1))
  (define t2* (if (type? t2) (list t1) t2))
  (for/or ([v t1*]) (set-member? t2* v)))

;; Returns true if the tensor types are equal. Scalar types must only share one type, not be completely equal.
;; Does not check dimension sizes
(define (tensor-type-equal? t1 t2)
  (cond
   [(equal? t2 '(tensor)) #t]   ; Unknown fpcores have a return type of '((real) (boolean) (tensor)), just assume it's valid
   [else 
    (define s1 (last t1))
    (define s2 (last t2))
    (and
      (= (length t1) (length t2))
      (cond
        [(and ((listof typename?) s1) ((listof typename?) s2)) (for/or ([i s1]) (set-member? s2 i))]
        [((listof typename?) s1) (for/or ([i s1]) (equal? i s2))]
        [((listof typename?) s2) (for/or ([i s2]) (equal? i s1))]
        [else (equal? s2 s2)]))]))

;; type checker when calling fpcores
(define (fpcore-as-operator-type* value args)
  (match-define (list core (list in-types ...) rtype sizes) value)
  (define match? 
    (for/and ([i in-types] [j args])  ;; j may be a type or a list of types (TODO: i can also be a list)
      (cond
      [(and (typename-equal? i 'tensor) (type? j))
        (tensor-type-equal? i j)]
      [(and (typename-equal? i 'tensor) ((listof type?) j))
        (for/or ([v j]) (tensor-type-equal? i v))]
      [(type? j) (tensor-type-equal? i j)]
      [else (for/or ([v j]) (type-match? i v))])))
  (cond
   [(and match? (*ragged-check*)) (check-rtype in-types args sizes rtype)]
   [match? rtype]
   [else #f]))

(define (fpcore-as-operator-type ident args)
  (define value (dict-ref (*fpcores*) ident #f))
  (if value
      (fpcore-as-operator-type* value args)
      #f))

(define/contract (check-types expr ctx)
  (-> expr? (dictof argument? type?) (or/c type? (listof type?)))
  (match expr
   [(? number? val)       '(real)]
   [(? extflonum? val)    '(real)]
   [(? hex? val)          '(real)]
   [(? constant? val)     (match val [(or 'TRUE 'FALSE) '(boolean)] [_ '(real)])]
   [(? symbol? var)       (dict-ref ctx var)]
   [`(digits ,m ,e ,b)    '(real)]
   [`(if ,test ,ift, iff)           ; if
     (unless (typename-equal? (check-types test ctx) 'boolean)
       (error 'check-types "Conditional test must return a boolean ~a" expr))
     (define ift-type
      (match (check-types ift ctx)
        [(? type? type) (list type)]
        [`(,(? type? types) ...) types]))
     (define iff-type
      (match (check-types iff ctx)
        [(? type? type) (list type)]
        [`(,(? type? types) ...) types]))
     (unless (for/or ([i ift-type]) (set-member? iff-type i))
       (error 'check-types "Conditional branches must have same type ~a" expr))
     (for/first ([i ift-type] #:when (set-member? iff-type i)) i)]
   [`(let ([,vars ,vals] ...) ,body)    ; let
     (define vals* (map (curryr check-types ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (check-types body ctx*)]
   [`(let* ([,vars ,vals] ...) ,body)    ; let*
     (define-values (ctx* vals*)
       (for/fold ([ctx ctx] [vals '()]) ([var vars] [val vals])
         (define val* (check-types val ctx))
         (define ctx* (dict-set ctx var val*))
         (values ctx* (cons val* vals))))
     (check-types body ctx*)]
   [`(while ,test ([,vars ,inits ,updates] ...) ,body)   ; while
     (define inits* (map (curryr check-types ctx) inits))
     (define ctx* (apply dict-set* ctx (append-map list vars inits*)))
     (unless (typename-equal? (check-types test ctx*) 'boolean)
       (error 'check-types "While loop conditions must return a boolean ~a" expr))
     (define updates* (map (curryr check-types ctx*) updates))
     (for ([var vars] [init inits*] [update updates*])
       (unless (equal? init update)
         (error 'check-types "Initialization and update must have the same type in while loop ~a" expr)))
     (check-types body ctx*)]
   [`(while* ,test ([,vars ,inits ,updates] ...) ,body)   ; while*
     (define-values (ctx* inits*)
       (for/fold ([ctx ctx] [inits* '()] #:result (values ctx (reverse inits*)))
                 ([var vars] [init inits])
         (define init* (check-types init ctx))
         (define ctx* (dict-set ctx var init*))
         (values ctx* (cons init* inits*))))
     (unless (typename-equal? (check-types test ctx*) 'boolean)
       (error 'check-types "While loop conditions must return a boolean ~a" expr))
     (define updates* (map (curryr check-types ctx*) updates))
     (for ([var vars] [init inits*] [update updates*])
       (unless (equal? init update)
         (error 'check-types "Initialization and update must have the same type in while loop ~a" expr)))
     (check-types body ctx*)]
   [`(for ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)  ; for
     (define vals* (map (curryr check-types ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (define inits* (map (curryr check-types ctx*) inits))
     (define ctx** (apply dict-set* ctx* (append-map list accums inits*)))
     (define updates* (map (curryr check-types ctx**) updates))
     (for ([accum accums] [init inits*] [update updates*])
       (unless 
         (if (typename-equal? init 'tensor) (tensor-type-equal? init update) (equal? init update))
         (error 'check-types "Initialization and update must have the same type in for loop ~a" expr)))
     (check-types body ctx**)]
   [`(for* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)   ; for*
     (define vals* (map (curryr check-types ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (define-values (ctx** inits*)
       (for/fold ([ctx* ctx*] [inits* '()] #:result (values ctx* (reverse inits*)))
                 ([var accums] [init inits])
         (define init* (check-types init ctx*))
         (define ctx** (dict-set ctx* var init*))
         (values ctx** (cons init* inits*))))
     (define updates* (map (curryr check-types ctx**) updates))
     (for ([accum accums] [init inits*] [update updates*])
       (unless 
         (if (typename-equal? init 'tensor) (tensor-type-equal? init update) (equal? init update))
         (error 'check-types "Initialization and update must have the same type in for loop ~a" expr)))
     (check-types body ctx**)]
   [`(tensor ([,vars ,vals] ...) ,body)     ; tensor*
     (define vals* (map (curryr check-types ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (define body* (check-types body ctx*))
    `(tensor ,(length vars) ,@vals ,@body*)]
   [`(tensor* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)    ; tensor**
     (define vals* (map (curryr check-types ctx) vals))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (define-values (ctx** inits*)
       (for/fold ([ctx* ctx*] [inits* '()] #:result (values ctx* (reverse inits*)))
                 ([var accums] [init inits])
         (define init* (check-types init ctx*))
         (define ctx** (dict-set ctx* var init*))
         (values ctx** (cons init* inits*))))
     (define updates* (map (curryr check-types ctx**) updates))
     (for ([accum accums] [init inits*] [update updates*])
       (unless 
         (if (typename-equal? init 'tensor) (tensor-type-equal? init update) (equal? init update))
         (error 'check-types "Initialization and update must have the same type in for loop ~a" expr)))
    `(tensor ,(length vars) ,@vals ,@(check-types body ctx**))]
   [`(! ,props* ... ,body)    ; !
     (check-types body ctx)]
   [`( ,op ,args ...)
     (define children (map (curryr check-types ctx) args))
     (define rtype
       (if (set-member? operators op)
           (operator-type op children)
           (fpcore-as-operator-type op children)))
     (unless rtype
       (error 'check-types "Invalid types for operator '~a': ~a" op children))
     rtype]))

(define (check-fpcore* name core vars properties body)
  (define-values (annotated-args args in-types ctx)
    (for/fold ([annot-args '()] [args '()] [in-types '()] [ctx (hash)])
              ([var vars])
      (unless (argument? var)
        (error 'check-fpcore* "FPCore parameters must be variables: ~a" var))
      (match var
        [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...) 
        (let* ([dim-sizes (for/list ([i (filter symbol? sizes)]) (list i '(real)))]
               [type `(tensor ,(length sizes) ,@sizes (real boolean))])
          (values (append annot-args (list var)) 
                  (append args (list name) (filter symbol? sizes))
                  (append in-types (list type)) 
                  (apply hash-set* (hash-set* ctx name type) (apply append dim-sizes))))]
        [(? list?) 
        (values (append annot-args (list var)) (append args (list (last var))) 
                (append in-types (list '(real))) (hash-set* ctx (list (last var)) '(real)))]
        [_ (values (append annot-args (list var)) (append args (list var)) 
                  (append in-types (list '(real))) (hash-set* ctx var '(real)))])))

  (define properties*
    (let loop ([properties properties])
      (match properties
        [(list) (list)]
        [(list (? property? prop) value rest ...)
          (cons (cons prop value) (loop rest))])))

  (when (dict-has-key? properties* ':pre)
    (define pre (dict-ref properties* ':pre))
    (define pre* (check-types pre ctx))
    (unless (typename-equal? pre* 'boolean)
      (error 'check-fpcore* "FPCore precondition must return a boolean: ~a" pre))
    (when (*ragged-check*)
      (check-precond pre))) ;; puts contraints on dimension sizes
  
  (cond   ; w/ type checking
    [(*check-types*)
      (define rtype (check-types body ctx))
      (define in-types* (update-sizes in-types))
      (define rtype* (first (update-sizes (list rtype))))
      (when name
        (*fpcores* (dict-set* (*fpcores*) name (list core in-types* rtype* (*dim-sizes*)))))
      #t]
    [else   ; no type checking
      (when name 
        (*fpcores* (dict-set* (*fpcores*) name (list core '() '() (make-hash)))))
      #t]))

(define/contract (check-fpcore core)
  (-> fpcore? boolean?)
  (parameterize ([*dim-sizes* (make-hash)])
    (match core
     [`(FPCore ,name (,vars ...) ,properties ... ,body)
      (check-fpcore* name core vars properties body)]
     [`(FPCore (,vars ...) ,properties ... ,body)
      (check-fpcore* #f core vars properties body)])))