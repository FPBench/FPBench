#lang racket

(require "common.rkt" "fpcore-checker.rkt" "fpcore-interpreter.rkt")
(provide read-fpcore)

(define/contract (read-fpcore name p)
  (-> any/c input-port? (or/c fpcore? eof-object?))
  (parameterize ([read-decimal-as-inexact #f])
    ;(define p* (open-input-bytes (regexp-replace* #rx"#" (port->bytes p) "! :precision integer"))) ; expand '#' since this is special in Racket
    ;(define stx (read-syntax name p*))
    (define stx (read-syntax name p))
    (if (eof-object? stx) stx (check-fpcore stx))))