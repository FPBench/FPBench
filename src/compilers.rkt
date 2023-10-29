#lang racket

(require "supported.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt")

(provide
  ; contractless
  define-compiler default-compiler-visitor
  ; inherits contract
  (rename-out
   [compiler-ctx-props ctx-props])
  ; new contracts
  (contract-out
    [*gensym-used-names* (parameter/c set-mutable?)]
    [*gensym-divider* (parameter/c (or/c char? string?))]
    [*gensym-collisions* (parameter/c natural?)]
    [*gensym-fix-name* (parameter/c (-> string? string?))]
    [struct compiler
     ([extensions (listof string?)]
      [header (-> string? string?)]
      [export (-> fpcore? string? string?)]
      [footer (-> string?)]
      [supported supported-list?])]
    [compilers (parameter/c (listof compiler?))]
    [make-compiler-ctx (->* ()
                            ((and/c hash? immutable?)
                             (and/c hash? immutable?)
                             (and/c hash? immutable?)
                             (and/c hash?))
                            compiler-ctx?)]
    [ctx-unique-name (->* (compiler-ctx? (or/c symbol? string?))
                          ((or/c boolean? symbol? (listof (or/c symbol? integer?))))
                          (values compiler-ctx? string?))]
    [ctx-random-name (->* (compiler-ctx?)
                          ((or/c boolean? symbol?))
                          (values compiler-ctx? string?))]
    [ctx-reserve-names (-> compiler-ctx? (listof (or/c symbol? string?)) compiler-ctx?)]
    [ctx-lookup-name (-> compiler-ctx? symbol? string?)]
    [ctx-lookup-prec (-> compiler-ctx? (or/c symbol? string?) any/c)]
    [ctx-update-props (-> compiler-ctx? (listof any/c) compiler-ctx?)]
    [ctx-lookup-prop (->* (compiler-ctx? symbol?) (any/c) any/c)]
    [ctx-set-extra (-> compiler-ctx? any/c any/c compiler-ctx?)]
    [ctx-update-extra (->* (compiler-ctx? any/c any/c) (any/c) compiler-ctx?)]
    [ctx-lookup-extra (-> compiler-ctx? any/c any/c)]
    [supported-by-lang? (-> fpcore? string? boolean?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct compiler (extensions header export footer supported))

(define compilers (make-parameter '()))

(define-syntax-rule (define-compiler arg ...)
  (compilers (cons (compiler arg ...) (compilers))))

(define (supported-by-lang? fpcore lang)
  (define compiler
    (for/first ([compiler (compilers)]
                #:when (set-member? (compiler-extensions compiler) lang))
        compiler))
  (if compiler
      (valid-core fpcore (compiler-supported compiler))
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;; name generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *gensym-used-names* (make-parameter (mutable-set)))
(define *gensym-divider* (make-parameter #\_))
(define *gensym-collisions* (make-parameter 1))
(define *gensym-fix-name* (make-parameter identity))

; Returns a unique, printable name based on the symbol and a fix-name procedure.
(define (gensym str)
  (define name
    (for/fold ([gen ((*gensym-fix-name*) str)])
              ([i (in-naturals (*gensym-collisions*))]                      
              #:break (not (set-member? (*gensym-used-names*) gen)))            
        (*gensym-collisions* (add1 i))                                  
        ((*gensym-fix-name*) (format "~a~a~a" str (*gensym-divider*) i))))
  (set-add! (*gensym-used-names*) name)
  name)

(define (->string s)
  (if (symbol? s) (symbol->string s) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler context ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct compiler-ctx (name-map props extra prec-map))

; Produces a new compiler context
(define (make-compiler-ctx [name-map (make-immutable-hash)]
                           [props (make-immutable-hash)]
                           [extra (make-immutable-hash)]
                           [prec-map (make-hash)])
  (compiler-ctx name-map props extra prec-map))

; Takes a given symbol or string and maps it to a unique, printable name,
; returning both the updated ctx struct and the name
(define (ctx-unique-name ctx name [prec #f])
  (define name* (->string name))
  (define unique (gensym name*))
  (define prec* (if prec prec (hash-ref (compiler-ctx-props ctx) ':precision 'binary64)))
  (hash-set! (compiler-ctx-prec-map ctx) unique prec*)
  (values (struct-copy compiler-ctx ctx
                       [name-map (hash-set (compiler-ctx-name-map ctx) name* unique)])
          unique))

; Produces a unique, printable name and returns the updated ctx struct and the name.
; The new name is not added to the name map
(define (ctx-random-name ctx [prec #f])
  (define name (gensym "tmp"))
  (define prec* (if prec prec (hash-ref (compiler-ctx-props ctx) ':precision 'binary64)))
  (hash-set! (compiler-ctx-prec-map ctx) name prec*)
  (values (struct-copy compiler-ctx ctx
                       [name-map (hash-set (compiler-ctx-name-map ctx) "tmp" name)])
          name))

; Reserves the list of names and returns the updated struct
(define (ctx-reserve-names ctx names)
  (for/fold ([ctx* ctx]) ([name names])
    (let-values ([(cx name*) (ctx-unique-name ctx* name)])
      cx)))

; Returns the unique, printable name currently mapped to by the given symbol.
; Returns the stringified version of the symbol otherwise.
(define (ctx-lookup-name ctx name)
  (define name* (->string name))
  (hash-ref (compiler-ctx-name-map ctx) name* name*))

; Returns the precision of the given variable name.
(define (ctx-lookup-prec ctx name)
  (hash-ref (compiler-ctx-prec-map ctx) (->string name)))

; Functionally extends a context struct's hash table of properties from the given properties.
(define (ctx-update-props ctx props)
  (struct-copy compiler-ctx ctx
               [props (apply hash-set* (compiler-ctx-props ctx) props)]))

; Returns the property value stored in the context struct. Returns the value at 
; failure otherwise.
(define (ctx-lookup-prop ctx prop [failure #f])
  (hash-ref (compiler-ctx-props ctx) prop failure))

; Add or set an extra entry
(define (ctx-set-extra ctx k v)
  (struct-copy compiler-ctx ctx
               [extra (hash-set (compiler-ctx-extra ctx) k v)]))

; Update an extra entry
(define (ctx-update-extra ctx k proc [fail #f])
  (struct-copy compiler-ctx ctx
               [extra (if fail
                          (hash-update (compiler-ctx-extra ctx) k proc fail)
                          (hash-update (compiler-ctx-extra ctx) k proc))]))

; Returns an extra entry
(define (ctx-lookup-extra ctx k [fail #f])
  (if fail
      (hash-ref (compiler-ctx-extra ctx) k fail)
      (hash-ref (compiler-ctx-extra ctx) k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; base compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-transform-visitor default-compiler-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: ~a" 'if)]
  [(visit-let_ vtor let_ vars vals body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: ~a" let_)]
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: ~a" while_)]
  [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: ~a" for_)]
  [(visit-tensor vtor vars vals body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: tensor")]
  [(visit-tensor* vtor vars vals accums inits updates body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: tensor*")]
  [(visit-! vtor props body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: !")]
  [(visit-op_ vtor op_ args body #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported operation: ~a" op_)]
  [(visit-terminal_ vtor x #:ctx ctx)
    (error 'default-compiler-visitor "Unsupported: ~a" x)])

