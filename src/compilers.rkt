#lang racket

(require "fpcore.rkt")

(provide
 (contract-out
  [struct compiler
    ([extensions (listof string?)]
     [header (-> string? string?)]
     [export (-> fpcore? string? string?)]
     [footer (-> string?)]
     [unsupported (listof symbol?)])]
  [compilers (parameter/c (listof compiler?))])
 define-compiler)

(struct compiler (extensions header export footer unsupported))

(define compilers (make-parameter '()))

(define-syntax-rule (define-compiler arg ...)
  (compilers (cons (compiler arg ...) (compilers))))
