#lang racket

(require "common.rkt" "fpcore.rkt")

(define/contract (operators-in expr)
  (-> expr? (listof symbol?))

  (match expr
    [`(while ,test ([,vars ,inits ,updates] ...) ,res)
     (cons 'while
           (append (operators-in test)
                   (append-map operators-in inits)
                   (append-map operators-in updates)
                   (operators-in res)))]
    [`(let ([,vars ,vals] ...) ,body)
     (cons 'let (append (append-map operators-in vals) (operators-in body)))]
    [`(if ,cond ,ift ,iff)
     (cons 'if (append (operators-in cond) (operators-in ift) (operators-in iff)))]
    [(list op args ...) (cons op (append-map operators-in args))]
    [(? symbol?) '()]
    [(? number?) '()]))

(define/contract ((filter type value) prog)
  (-> symbol? string? (-> fpcore? boolean?))
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))

  (match type
    ['cites
     (set-member? (dict-ref properties ':cite '()) (string->symbol value))]
    ['operator
     (set-member? (operators-in body) (string->symbol value))]
    ['type
     (equal? (dict-ref properties ':type #f) (string->symbol value))]))

(module+ main
  (require racket/cmdline)
  (define invert? #f)

  (command-line
   #:program "filter.rkt"
   #:once-each
   [("-v" "--invert") "Invert the meaning of the filter"
    (set! invert? #t)]
   #:args (type value)
   (define test
     (compose
      (if invert? not identity)
      (filter (string->symbol type) value)))
   (for ([expr (in-port read (current-input-port))])
     (when (test expr)
       (pretty-print expr)))))
