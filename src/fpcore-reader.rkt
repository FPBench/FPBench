#lang racket

(require "common.rkt" "fpcore-checker.rkt" "fpcore-interpreter.rkt")
(provide read-fpcore)

; Adapted from Racket source:
; github.com/racket/racket/blob/master/racket/src/cs/bootstrap.scheme-readtable.rkt
(define ((paren closer) c in src line col pos)
  (let loop ()
    (define c (peek-char in))
    (cond
      [(eqv? closer c)
       (read-char in)
       null]
      [(char-whitespace? c)
       (read-char in)
       (loop)]
      [(eqv? #\# c)   ;; Syntactic sugar expansion: '#' -> '! precision integer'
       (read-char in)
       `(! :precision integer ,(read/recursive in) . ,(loop))]
      [(and (eqv? #\. c)
            (char-whitespace? (peek-char in 1)))
       (read-char in)
       (begin0
         (read/recursive in)
         (let loop ()
           (define c (read-char in))
           (cond
             [(char-whitespace? c) (loop)]
             [(eqv? c closer) (void)]
             [else (error 'parens "unexpected: ~s" c)])))]
      [else
       (define v (read/recursive in))
       (if (special-comment? v)
           (loop)
           (cons v (loop)))])))

(define fpcore-readtable
  (make-readtable
   #f
   #\( 'terminating-macro (paren #\))))

(define/contract (read-fpcore name p)
  (-> any/c input-port? (or/c fpcore? eof-object?))
  (parameterize ([read-decimal-as-inexact #f] 
                 [current-readtable fpcore-readtable])
    ;(define p* (open-input-bytes (regexp-replace* #rx"#" (port->bytes p) "! :precision integer"))) ; expand '#' since this is special in Racket
    ;(define stx (read-syntax name p*))
    (define stx (read-syntax name p))
    (if (eof-object? stx) stx (check-fpcore stx))))