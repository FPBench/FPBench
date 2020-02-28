#lang racket

(require "fpcore.rkt" "supported.rkt")
(provide define-compiler gensym *names* gensym-dividing-char)
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

;; Unique name generation

(define *names* (make-parameter (mutable-set)))
(define gensym-dividing-char (make-parameter #\_))

(define (gensym name fix-name)
  (define fixed (fix-name name))
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a fixed))) (set->list (*names*))))
  (define options
    (cons fixed (for/list ([_ prefixed] [i (in-naturals)]) (format "~a~a~a" fixed (gensym-dividing-char) (+ i 1)))))
  (define name*
    (last (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)