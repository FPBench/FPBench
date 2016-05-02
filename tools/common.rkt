#lang racket

(provide parse-properties unparse-properties constant?)

(define (property? symb)
  (and (symbol? symb) (string-prefix? (symbol->string symb) ":")))

(define (parse-properties lines)
  (let loop ([lines lines] [props '()])
    (match lines
      [(list (? property? prop) value rest ...)
       (loop rest (cons (cons prop value) props))]
      [(list _ ...)
       (values lines (reverse props))])))

(define/match (cons->list x)
  [((cons a b)) (list a b)])

(define (unparse-properties properties)
  (append-map cons->list properties))

(define constants
  '(E LOG2E LOG10E LN2 LN10
      PI PI_2 PI_4 1_PI 2_PI 2_SQRTPI
      SQRT2 SQRT1_2 MAXFLOAT HUGE_VAL
      TRUE FALSE))

(define (constant? x)
  (member x constants))

