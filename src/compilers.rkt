#lang racket

(require "fpcore.rkt" "supported.rkt")
(provide define-compiler
  (contract-out
    [struct compiler
     ([extensions (listof string?)]
      [header (-> string? string?)]
      [export (-> fpcore? string? string?)]
      [footer (-> string?)]
      [supported supported-list?])]
    [compilers (parameter/c (listof compiler?))]))

(struct compiler (extensions header export footer supported))

(define compilers (make-parameter '()))

(define-syntax-rule (define-compiler arg ...)
  (compilers (cons (compiler arg ...) (compilers))))
