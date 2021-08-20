#lang racket

(require "supported.rkt" "fpcore-checker.rkt")

(provide
  define-compiler ; contractless (syntax)
  (rename-out     ; inherits contract
   [compiler-ctx-props ctx-props])
  (contract-out   ; new contracts
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
                             (and/c hash? immutable?))
                            compiler-ctx?)]
    [ctx-unique-name (->* (compiler-ctx? (or/c symbol? string?))
                          ((or/c boolean? symbol?))
                          (values compiler-ctx? string?))]
    [ctx-random-name (->* (compiler-ctx?)
                          ((or/c boolean? symbol?))
                          (values compiler-ctx? string?))]
    [ctx-reserve-names (-> compiler-ctx? (listof (or/c symbol? string?)) compiler-ctx?)]
    [ctx-update-props (-> compiler-ctx? (listof any/c) compiler-ctx?)]
    [ctx-lookup-name (-> compiler-ctx? symbol? string?)]
    [ctx-lookup-prec (-> compiler-ctx? symbol? any/c)]
    [ctx-lookup-prop (->* (compiler-ctx? symbol?) ((or/c boolean? symbol?)) any/c)]
    [supported-by-lang? (-> fpcore? string? boolean?)]))

;; Compiler struct

(struct compiler (extensions header export footer supported))

(define compilers (make-parameter '()))

(define-syntax-rule (define-compiler arg ...)
  (compilers (cons (compiler arg ...) (compilers))))

;;; Compiler contexts

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

(struct compiler-ctx (name-map prec-map props))

; Produces a new compiler context
(define (make-compiler-ctx [name-map (make-immutable-hash)]
                           [prec-map (make-immutable-hash)]
                           [props (make-immutable-hash)])
  (compiler-ctx name-map prec-map props))

; Takes a given symbol or string and maps it to a unique, printable name,
; returning both the updated ctx struct and the name
(define (ctx-unique-name ctx name [prec #f])
  (define name* (->string name))
  (define unique (gensym name*))
  (define prec* (if prec prec (dict-ref (compiler-ctx-props ctx) ':precision 'binary64)))
  (values (compiler-ctx (dict-set (compiler-ctx-name-map ctx) name* unique) 
                        (dict-set (compiler-ctx-prec-map ctx) name* prec*)
                        (compiler-ctx-props ctx))
          unique))

; Produces a unique, printable name and returns the updated ctx struct and the name.
; The new name is not added to the name map
(define (ctx-random-name ctx [prec #f])
  (define name (gensym "tmp"))
  (define prec* (if prec prec (dict-ref (compiler-ctx-props ctx) ':precision 'binary64)))
  (values (compiler-ctx (compiler-ctx-name-map ctx)
                        (dict-set (compiler-ctx-prec-map ctx) name prec*)
                        (compiler-ctx-props ctx))
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
  (dict-ref (compiler-ctx-name-map ctx) name* name*))

; Returns the precision of the given variable. Returns false if the symbol is unmapped.
(define (ctx-lookup-prec ctx name)
  (define name* (->string name))
  (dict-ref (compiler-ctx-prec-map ctx) name* #f))

; Functionally extends a context struct's hash table of properties from the given properties.
(define (ctx-update-props ctx props)
  (compiler-ctx (compiler-ctx-name-map ctx)
                (compiler-ctx-prec-map ctx)
                (apply hash-set* (compiler-ctx-props ctx) props)))

; Returns the property value stored in the context struct. Returns the value at 
; failure otherwise.
(define (ctx-lookup-prop ctx prop [failure #f])
  (dict-ref (compiler-ctx-props ctx) prop failure))

;;; Misc

(define (supported-by-lang? fpcore lang)
  (define compiler
    (for/first ([compiler (compilers)]
                #:when (set-member? (compiler-extensions compiler) lang))
        compiler))
  (if compiler
      (valid-core fpcore (compiler-supported compiler))
      #f))
