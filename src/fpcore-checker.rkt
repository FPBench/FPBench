#lang racket

(require generic-flonum)
(require "common.rkt" "tensor.rkt")
(provide *fpcores* *check-types* *ragged-check*
         fpcore? expr? argument? check-fpcore check-argument)

(define *fpcores* (make-parameter '()))  ; previously run fpcores, dictionary value is a list (core, vars, in-types, rtype, size-equations)
(define *check-types* (make-parameter #f))
(define *ragged-check* (make-parameter #f))
(define *dim-sizes* (make-parameter #f))        ;; tensor dimension sizes equations
(define *tensor-scalars* (make-parameter #f))   ;; tensor scalar types

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
    [(? value?) true]
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
    (for ([key (hash-keys (*dim-sizes*))])    ; if x = a, substitute a for every x
      (let ([vals (hash-ref (*dim-sizes*) key)])
        (when (= (length vals) 1)
          (set! changed? (or changed? (subst key (first vals)))))))

    (for ([key (hash-keys (*dim-sizes*))])    ;; if x = a = b, where a, b, ... are independent variables: substitute a = x, b = x, ...
      (let ([vals (hash-ref (*dim-sizes*) key)])
        (when (and (> (length vals) 1)                              
                   (for/and ([val vals])  
                      (and (symbol? val) (not (hash-has-key? (*dim-sizes*) val)))))
          (hash-remove! (*dim-sizes*) key)
          (for ([val vals]) (hash-set*! (*dim-sizes*) val (list key)))
          (set! changed? #t))))

    (for ([key (hash-keys (*dim-sizes*))])    ; eliminate duplicates, check numbers for inequality
      (let* ([vals (hash-ref (*dim-sizes*) key)] 
             [nums (filter number? vals)])
        (when (> (length nums) 1)               ; if the key x is associated with a, b, ... and a, b, ... are numbers, check that a = b = ...
          (unless (for/and ([val (cdr nums)]) (equal? (car nums) val))
            (error 'subst-elim "Inconsistent tensor sizes. Deduced two or more different sizes for the same dimension")))
        (let ([vals* (remove-duplicates vals)])
          (if (and (= (length vals*) 1) (equal? key (first vals*)))  ; eliminate x = x
            (begin
              (hash-remove! (*dim-sizes*) key)
              (set! changed? #t))
            (hash-set*! (*dim-sizes*) key vals*)))))
    (when changed? (loop))))

;; Adds a list of length two lists that represent equations '(x a) : x = a
(define (add-equations eqs)
  (define eqs*
    (for/fold ([eqs* '()]) ([eq eqs])
      (match eq
       [(list (? number? num1) (? number? num2))
        (unless (equal? num1 num2)
          (error 'add-equations "Dimension size mismatch. ~a != ~a" num1 num2 ))
        eqs*]
       [(list x x) eqs*]  
       [(list (? symbol? var) (? number? num)) (cons (cons var num) eqs*)]
       [(list (? number? num) (? symbol? var)) (cons (cons var num) eqs*)]
       [(list (? symbol? var1) (? symbol? var2)) (cons (cons var1 var2) eqs*)])))
  (for ([eq eqs*])
    (cond
     [(hash-has-key? (*dim-sizes*) (car eq))
      (let ([val (hash-ref (*dim-sizes*) (car eq))])
        (hash-set*! (*dim-sizes*) (car eq) (cons (cdr eq) val)))]
     [else (hash-set*! (*dim-sizes*) (car eq) (list (cdr eq)))]))
  (when (not (empty? eqs*))
    (subst-elim)))

;; Adds equations to the dimension sizes hash based on 'array' elements
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

;; Updates types based on known dimension sizes
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
  (add-equations eqs*)      ;; check if equations are consistent, if not, sizes do not match

  (define new->old
    (for/fold ([new->old '()]) ([key (dict-keys eqs)])
      (let* ([vals 
              (remove-duplicates
                (for/list ([val (dict-ref eqs key)])
                  (if (and (dict-has-key? (*dim-sizes*) val) (= (length (dict-ref (*dim-sizes*) val)) 1))
                      (first (dict-ref (*dim-sizes*) val))
                      val)))]
             [nums (filter number? vals)])
        (if (empty? nums)
            (dict-set* new->old key vals)
            (dict-set* new->old key nums)))))

  (define update-hash                       ; hash of old size variables in terms of known sizes
    (for/hash ([key (dict-keys new->old)])
      (values key (filter symbol? (dict-ref new->old key)))))
  (first (update-sizes (list rtype) update-hash)))

;; Adds equations to the dimension sizes hash according to the precondition
(define (check-precond expr)
 (match expr
  [(list 'and (? expr? subs) ...)
    (for/and ([i subs]) (check-precond i))]
  [(list '== (? expr? subs) ...)
    (when (for/or ([i subs]) (symbol? i))
      (add-equations (combinations subs 2)))
    #t]
  [_ #f]))

;; Tensor scalar type inference

(define (ref-scalar? type)
  (match type
   [`((real ,(? symbol?)) (boolean ,(? symbol?))) #t]
   [_ #f]))

(define (set-scalar-types tensors rtype)
  (define rtype* (if (= (length rtype) 1) (first rtype) rtype))
  (for ([tensor tensors])
    (if (hash-has-key? (*tensor-scalars*) tensor)
      (unless (equal? (hash-ref (*tensor-scalars*) tensor) rtype*)
        (error 'set-scalar-types "Inconsistent tensor scalar types. Assumed ~a was of type ~a, but encountered ~a"
                                tensor (hash-ref (*tensor-scalars*) tensor) rtype*))
      (hash-set*! (*tensor-scalars*) tensor rtype*)))
  rtype)

(define (update-scalars in-types args)
  (define in-types*
    (for/list ([in-type in-types] [arg args])
      (if (hash-has-key? (*tensor-scalars*) arg)
        (list-set in-type (sub1 (length in-type)) (hash-ref (*tensor-scalars*) arg))
        in-type)))
  in-types*)

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
        (let ([first (car elems)])
          `(tensor ,(add1 (second first)) ,(length elems) ,@(drop (take first (sub1 (length first))) 2) ,(last first)))]
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

;; Type checker for operators. Returns every valid return type
(define (operator-type op args)  
  (define types (for/list ([arg args])
                  (match arg
                    [(? type?) (list arg)]
                    [`(,(? type?) ...) arg])))
  (define-values (types* tensors)
    (for/fold ([types* '()] [tensors '()]) ([type types])
      (if (ref-scalar? type)
        (let ([sym (cdar type)])
          (values (append types* (list `((real) (boolean)))) (append tensors sym)))
        (values (append types* (list type)) tensors))))

  (define arg-coords (apply cartesian-product types*))
  (define rtypes
    (for/fold ([rtypes '()]) ([args* arg-coords])
      (let ([out (operator-type* op args*)])
        (if out
            (append rtypes (list out))
            rtypes))))
  (cond
    [(empty? rtypes) #f]
    [(and (= (length rtypes) 1) (not (empty? tensors))) (set-scalar-types tensors (first rtypes))]
    [(= (length rtypes) 1) (first rtypes)]
    [else rtypes]))

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

;; Returns true if the type/type lists share a common element
(define (type-match? t1 t2)
  (define t1* (if (type? t1) (list t1) t1))
  (define t2* (if (type? t2) (list t1) t2))
  (for/or ([v t1*]) (set-member? t2* v)))

;; runs an fpcore again to see if it gets a more specific return type with known argument types
(define (fpcore-rtype core in-vars arg-types)
  (define-values (vars body)
    (match core
     [`(FPCore ,name (,vars ...) ,properties ... ,body) (values vars body)]
     [`(FPCore (,vars ...) ,properties ... ,body) (values vars body)]))
  (define-values (ctx size-hash)
    (for/fold ([ctx (hash)] [size-hash (hash)]) ([var vars] [type arg-types])
      (match var
       [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...) 
        (let ([dim-sizes (for/list ([i (filter symbol? sizes)]) (list i '(real)))]
              [known-sizes   ; for dim-sizes, get the known size
                (for/fold ([known-sizes '()])
                          ([size sizes] [val (drop (take type (sub1 (length type))) 2)]
                          #:when (symbol? size))
                  (append known-sizes (list size (list val))))])
          (values (apply hash-set* (hash-set* ctx name type) (apply append dim-sizes))
                  (apply hash-set* size-hash known-sizes)))]
       [(? list?) 
        (values (hash-set* ctx (list (last var)) '(real)) size-hash)]
       [_ 
        (values (hash-set* ctx var '(real)) size-hash)])))
  (parameterize ([*dim-sizes* (make-hash)]     ; runs with updated context
                 [*tensor-scalars* (make-hash)])
    (let ([rtype (check-types body ctx)])
      (first (update-sizes (list rtype) size-hash)))))

;; type checker when calling fpcores
(define (fpcore-as-operator-type* value arg-types vars)
  (match-define (list core in-vars (list in-types ...) rtype sizes) value)
  (define match? 
    (for/and ([i in-types] [j arg-types])  ;; j may be a type or a list of types (TODO: i can also be a list)
     (cond
      [(and (typename-equal? i 'tensor) (type? j))
        (tensor-type-equal? i j)]
      [(and (typename-equal? i 'tensor) ((listof type?) j))
        (for/or ([v j]) (tensor-type-equal? i v))]
      [(type? j) (tensor-type-equal? i j)]
      [else (for/or ([v j]) (type-match? i v))])))

  (when match?          ;; update scalar type of an input tensor when using it in a function call
    (for ([in in-types] [var vars]
          #:when (and (typename-equal? in 'tensor) (typename? (last in))))
      (set-scalar-types (list var) (list (last in)))))

  (define arg-types*    ;; update scalar type of an input tensor when using its scalar value in a function call
    (for/fold ([arg-types* '()]) ([in-type in-types] [type arg-types])
      (if (ref-scalar? type)
        (let ([sym (cdar type)])
          (when (or (typename-equal? in-type 'real) (typename-equal? in-type 'boolean))
            (hash-set*! (*tensor-scalars*) (first sym) (first in-type)))
          (append arg-types* (list in-type)))
        (append arg-types* (list type)))))
  (define rtype* (fpcore-rtype core in-vars arg-types*))
  (cond
   [(and match? (*ragged-check*)) (check-rtype in-types arg-types* sizes rtype*)]
   [match? rtype*]
   [else #f]))

(define (fpcore-as-operator-type ident arg-types vars)
  (define value (dict-ref (*fpcores*) ident #f))
  (when (not value)
    (error 'fpcore-as-operator-type "Unknown FPCore ~a. Use '--no-check' if your file contains FPCores that are recursive or used before they are declared" ident))
  (fpcore-as-operator-type* value arg-types vars))

;;
;; type checker
;;

;; type checker for each expression
(define/contract (check-types expr ctx)
  (-> expr? (dictof argument? type?) (or/c type? (listof type?)))
  (match expr
   [(? number? val)       '(real)]
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
     (match body*
      [(or '(real) '(boolean))  `(tensor ,(length vars) ,@vals ,(first body*))]
      [(? ref-scalar?)  `(tensor ,(length vars) ,@vals (real boolean))]
      [(? (curryr typename-equal? 'tensor))
        (let ([dim (second body*)]
              [sizes (drop (take body* (sub1 (length body*))) 2)]
              [stype (last body*)])
          `(tensor ,(add1 dim) ,@vals ,@sizes ,stype))])]
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
     (define body* (check-types body ctx**))
     (match body*
      [(or '(real) '(boolean))  `(tensor ,(length vars) ,@vals ,(first body*))]
      [(? ref-scalar?)  `(tensor ,(length vars) ,@vals (real boolean))]
      [(? (curryr typename-equal? 'tensor))
        (let ([dim (second body*)]
              [sizes (drop (take body* (sub1 (length body*))) 2)]
              [stype (last body*)])
          `(tensor ,(add1 dim) ,@vals ,@sizes ,stype))])]
   [`(! ,props* ... ,body)    ; !
     (check-types body ctx)]
   [`(ref ,ten ,args ...)
    (define ten* (check-types ten ctx))
    (define children (map (curryr check-types ctx) args))
    (define rtype (operator-type 'ref (cons ten* children)))
    (unless rtype
       (error 'check-types "Invalid types for operator 'ref': ~a ~a" ten* children))
    (define rtype*
     (match rtype
      [(? type?) rtype]
      [(? (listof type?)) (map (curryr append (list ten)) rtype)]))
    rtype*]
   [(list (? operator? op) args ...)
     (define children (map (curryr check-types ctx) args))
     (define rtype (operator-type op children))
     (unless rtype
       (error 'check-types "Invalid types for operator '~a': ~a" op children))
     rtype]
   [`(,fpcore ,args ...)
     (define children (map (curryr check-types ctx) args))
     (define rtype (fpcore-as-operator-type fpcore children args))
     (unless rtype
       (error 'check-types "Invalid types for FPCore '~a': ~a" fpcore children))
     rtype]))

;; checks core
(define (check-fpcore* name core vars properties body)
  (define-values (annotated-args args in-vars in-types ctx)
    (for/fold ([annot-args '()] [args '()] [in-vars '()] [in-types '()] [ctx (hash)])
              ([var vars])
      (unless (argument? var)
        (error 'check-fpcore* "FPCore parameters must be variables: ~a" var))
      (match var
       [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...) 
        (let* ([dim-sizes (for/list ([i (filter symbol? sizes)]) (list i '(real)))]
               [type `(tensor ,(length sizes) ,@sizes (real boolean))])
          (values (append annot-args (list var)) (append args (list name) (filter symbol? sizes))
                  (append in-vars (list name)) (append in-types (list type)) 
                  (apply hash-set* (hash-set* ctx name type) (apply append dim-sizes))))]
       [(? list?) 
        (values (append annot-args (list var)) (append args (list (last var))) 
                (append in-vars (list (last var))) (append in-types (list '(real))) 
                (hash-set* ctx (list (last var)) '(real)))]
       [_ (values (append annot-args (list var)) (append args (list var)) 
                  (append in-vars (list var)) (append in-types (list '(real)))
                  (hash-set* ctx var '(real)))])))

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
      (define rtype 
        (let ([rtype (check-types body ctx)])
          (if (ref-scalar? rtype)
            '((real) (boolean))
            rtype)))
      (define in-types* (update-scalars (update-sizes in-types) in-vars))
      (define rtype* (first (update-sizes (list rtype))))
      (when name
        (*fpcores* (dict-set* (*fpcores*) name (list core in-vars in-types* rtype* (*dim-sizes*)))))
      #t]
    [else   ; no type checking
      (when name 
        (*fpcores* (dict-set* (*fpcores*) name (list core in-vars '() '() (make-hash)))))
      #t]))

(define/contract (check-fpcore core)
  (-> fpcore? boolean?)
  (parameterize ([*dim-sizes* (make-hash)]
                 [*tensor-scalars* (make-hash)])
    (match core
     [`(FPCore ,name (,vars ...) ,properties ... ,body)
      (check-fpcore* name core vars properties body)]
     [`(FPCore (,vars ...) ,properties ... ,body)
      (check-fpcore* #f core vars properties body)])))

;;
;;  Argument checker
;;

(define (check-scalar-arg val typename)
  (match typename
   ['real (or (number? val) (gfl? val) (hex? val) (set-member? (remove* '(TRUE FALSE) constants) val))]
   ['boolean (or (boolean? val) (equal? val 'TRUE) (equal? val 'FALSE))]
   [(list real boolean) (or (check-scalar-arg val 'real) (check-scalar-arg val 'boolean))]))

(define (check-tensor-sizes tensor sizes stype)
  (let loop ([ten tensor] [sizes* sizes])
    (cond
     [(empty? sizes*) 
      (unless (check-scalar-arg ten stype)
        (error 'check-tensor-sizes "Expected a ~a tensor element. Received: ~a" stype ten))]
     [else
      (unless (= (length ten) (car sizes*))
        (error 'check-tensor-sizes "Expected a tensor dimension of size: ~a. Received: ~a" (car sizes*) (length ten)))
      (for ([elem ten]) (loop elem (cdr sizes*)))])))

(define (check-argument name ctx)
  (define core-info (dict-ref (*fpcores*) name))
  (define vars (second core-info))
  (define in-types (third core-info))
  (for ([var vars] [type in-types])
    (let ([val (dict-ref ctx var #f)])
      (unless val
        (error 'check-types "~a not provided as an argument" var))
      (cond
       [(typename-equal? type 'real) 
        (unless (check-scalar-arg val 'real)
          (error 'check-argument "Expected a real argument. Received: ~a" val))]
       [(typename-equal? type 'boolean)
        (unless (check-scalar-arg val 'boolean)
          (error 'check-argument "Expected a boolean argument. Received: ~a" val))]
       [(typename-equal? type 'tensor)
        (let ([dim (second type)]
              [sizes 
                (for/list ([elem (drop (take type (sub1 (length type))) 2)])
                  (if (symbol? elem)
                    (inexact->exact (dict-ref ctx elem))
                    elem))]
              [stype (last type)])
          (unless (= (tensor-dim val) dim)
            (error 'check-argument "Expected a tensor of dimension ~a. Got ~a" dim (tensor-dim val)))
          (check-tensor-sizes val sizes stype)
        )]
  ))))