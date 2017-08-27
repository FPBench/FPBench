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

(define/contract ((filter type values) prog)
  (-> symbol? (listof string?) (-> fpcore? boolean?))
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))

  (match* (type values)
    [('cites (list value))
     (set-member? (dict-ref properties ':cite '()) (string->symbol value))]
    [((or 'operator 'operation) (list value))
     (set-member? (operators-in body) (string->symbol value))]
    [((or 'operators 'operations) (list ops ...))
     (subset? (operators-in body) (map string->symbol ops))]
    [((? symbol?) (list value))
     (define prop (string->symbol (format ":~a" type)))
     (and (dict-has-key? properties prop)
          (equal? (~a (dict-ref properties prop)) value))]
    [((? symbol?) (list))
     (define prop (string->symbol (format ":~a" type)))
     (dict-has-key? properties prop)]
    [(_ _)
     (raise-user-error 'filter "Unknown filter ~a with ~a arguments" type (length values))]))

(module+ main
  (require racket/cmdline)
  (define invert? #f)

  (command-line
   #:program "filter.rkt"
   #:once-each
   [("-v" "--invert") "Invert the meaning of the filter"
    (set! invert? #t)]
   #:args (type . values)
   (define test
     (compose
      (if invert? not identity)
      (filter (string->symbol type) values)))
   (port-count-lines! (current-input-port))
   (for ([expr (in-port read (current-input-port))])
     (when (test expr)
       (pretty-print expr (current-output-port) 1)
       (newline)))))
