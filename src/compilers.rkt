#lang racket

(require "supported.rkt" "fpcore-checker.rkt")
(provide *used-names* *gensym-divider* *gensym-collisions* *gensym-fix-name*
          ctx-unique-name ctx-random-name ctx-lookup-name ctx-reserve-names ctx-names
          ctx-update-props ctx-lookup-prop ctx-props ctx-lookup-prec
          make-compiler-ctx define-compiler supported-by-lang?)
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

(define *used-names* (make-parameter (mutable-set)))
(define *gensym-divider* (make-parameter #\_))
(define *gensym-collisions* (make-parameter 1))
(define *gensym-fix-name* (make-parameter identity))

; Returns a unique, printable name based on the symbol and a fix-name procedure.
(define (gensym sym) 
  (define name
    (for/fold ([gen ((*gensym-fix-name*) (if (symbol? sym) (symbol->string sym) sym))])
              ([i (in-naturals (*gensym-collisions*))]                      
              #:break (not (set-member? (*used-names*) gen)))            
        (*gensym-collisions* (add1 i))                                  
        ((*gensym-fix-name*) (format "~a~a~a" sym (*gensym-divider*) i))))
  (set-add! (*used-names*) name)
  name)

(struct compiler-ctx (name-map prec-map props))

; Returns a new context struct with an empty name map and empty properties.
(define (make-compiler-ctx) 
  (compiler-ctx (make-immutable-hash) (make-immutable-hash) (make-immutable-hash)))

; Takes a given symbol maps it to a unique, printable name and returns both
; the updated ctx struct and the name
(define (ctx-unique-name ctx name [prec #f])
  (let ([unique (gensym name)]
        [prec (if prec prec (dict-ref (compiler-ctx-props ctx) ':precision 'binary64))])
    (values 
      (compiler-ctx (dict-set (compiler-ctx-name-map ctx) name unique) 
                    (dict-set (compiler-ctx-prec-map ctx) name prec)
                    (compiler-ctx-props ctx))
      unique)))

; Takes a fix-name procedure and returns the updated context struct and a "random", unique, printable name
; that will be unmapped, but it's precision is mapped.
(define (ctx-random-name ctx [prec #f])
  (let ([name (gensym 'tmp)]
        [prec (if prec prec (dict-ref (compiler-ctx-props ctx) ':precision 'binary64))])
    (values
      (compiler-ctx (compiler-ctx-name-map ctx) 
                    (dict-set (compiler-ctx-prec-map ctx) name prec)
                    (compiler-ctx-props ctx))
      name)))

; Reserves the list of names and returns the updated struct
(define (ctx-reserve-names ctx names)
  (for/fold ([ctx* ctx]) ([name names])
    (let-values ([(cx name*) (ctx-unique-name ctx* name)])
      cx)))

; Returns the unique, printable name currently mapped to by the given symbol.
; Returns the stringified version of the symbol otherwise.
(define (ctx-lookup-name ctx sym)
  (dict-ref (compiler-ctx-name-map ctx) sym (symbol->string sym)))

; Returns the precision of the given variable. Returns false if the symbol is unmapped.
(define (ctx-lookup-prec ctx name)
  (dict-ref (compiler-ctx-prec-map ctx) name #f))

; Functionally extends a context struct's hash table of properties from the given properties.
(define (ctx-update-props ctx props)
  (compiler-ctx (compiler-ctx-name-map ctx) (compiler-ctx-prec-map ctx) (apply hash-set* (compiler-ctx-props ctx) props)))

; Returns the property value stored in the context struct. Returns the value at 
; failure otherwise.
(define (ctx-lookup-prop ctx prop [failure #f])
  (dict-ref (compiler-ctx-props ctx) prop failure))

; Returns the context struct's properties.
(define (ctx-props ctx)
  (compiler-ctx-props ctx))

; Returns the context struct's hash names
(define (ctx-names ctx)
  (compiler-ctx-name-map ctx))

;;; Misc

(define (supported-by-lang? fpcore lang)
  (-> fpcore? string? boolean?)
  (define compiler
    (for/first ([compiler (compilers)]
                #:when (set-member? (compiler-extensions compiler) lang))
        compiler))
  (if compiler
      (valid-core fpcore (compiler-supported compiler))
      #f))