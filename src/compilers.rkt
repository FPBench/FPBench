#lang racket

(require "fpcore.rkt" "supported.rkt")
(provide *used-names* *gensym-divider* *gensym-collisions* *gensym-unique*
          make-compiler-ctx ctx-unique-name ctx-random-name ctx-lookup-name ctx-update-props
          ctx-lookup-prop ctx-props define-compiler)
(provide
  (contract-out
    [struct compiler
     ([extensions (listof string?)]
      [header (-> string? string?)]
      [export (-> fpcore? string? string?)]
      [footer (-> string?)]
      [supported supported-list?])]
    [compilers (parameter/c (listof compiler?))]))

;; Compiler struct

(struct compiler (extensions header export footer supported))

(define compilers (make-parameter '()))

(define-syntax-rule (define-compiler arg ...)
  (compilers (cons (compiler arg ...) (compilers))))

;;; Compiler contexts

(define *used-names* (make-parameter (mutable-set)))  ; todo - make immutable?
(define *gensym-divider* (make-parameter #\_))
(define *gensym-collisions* (make-parameter 1))
(define *gensym-unique* (make-parameter #t))

; Returns a unique, printable name based on the symbol and a fix-name procedure.
(define (gensym sym fix-name) 
  (define name
    (if (*gensym-unique*)
        (for/fold ([gen (fix-name (if (symbol? sym) (symbol->string sym) sym))])
                  ([i (in-naturals (*gensym-collisions*))]                      
                  #:break (not (set-member? (*used-names*) gen)))              
            (*gensym-collisions* (add1 i))                                  
            (fix-name (format "~a~a~a" sym (*gensym-divider*) i)))
        (symbol->string sym)))
  (set-add! (*used-names*) name)
  name)

(struct compiler-ctx (name-map props))

; Returns a new context struct with an empty name map and empty properties.
(define (make-compiler-ctx) 
  (compiler-ctx (make-immutable-hash) (make-immutable-hash)))

; Takes a given symbol and a fix-name procedure, maps it to a unique, printable name and returns both
; the updated ctx struct and the name
(define (ctx-unique-name ctx name fix-name)
  (define unique (gensym name fix-name))
  (values 
      (compiler-ctx (dict-set (compiler-ctx-name-map ctx) name unique) (compiler-ctx-props ctx))
      unique))

; Takes a fix-name procedure and returns a "random", unique, printable name that will be unmapped.
(define (ctx-random-name fix-name)
  (gensym 'VAR fix-name))

; Returns the unique, printable name currently mapped to by the given symbol.
; Returns the stringified version of the symbol otherwise.
(define (ctx-lookup-name ctx sym)
  (define name (dict-ref (compiler-ctx-name-map ctx) sym (symbol->string sym)))
  name)

; Functionally extends a context struct's hash table of properties from the given properties.
(define (ctx-update-props ctx props)
  (compiler-ctx (compiler-ctx-name-map ctx) (apply hash-set* (compiler-ctx-props ctx) props)))

; Returns the property value stored in the context struct. Returns the value at 
; failure otherwise.
(define (ctx-lookup-prop ctx prop failure)
  (dict-ref (compiler-ctx-props ctx) prop failure))

; Returns the context struct's properties.
(define (ctx-props ctx)
  (compiler-ctx-props ctx))