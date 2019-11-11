#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt" "../src/supported.rkt")

(define/contract ((filter type values) prog)
  (-> symbol? (listof string?) (-> fpcore? boolean?))
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define prop-hash (property-values body))
  (property-hash-add! prop-hash props)

  (match* (type values)
    [((or 'operator 'operation) (list value))
     (set-member? (operators-in body) (string->symbol value))]
    [((or 'operators 'operations) (list ops ...))
     (subset? (operators-in body) (map string->symbol ops))]
    [((or 'not-operators 'not-operations) (list ops ...))
     (define body-ops (operators-in body))
     (for/and ([op (map string->symbol ops)])
       (not (set-member? body-ops op)))]

    [('constant (list value))
     (set-member? (constants-in body) (string->symbol value))]
    [('constants (list ops ...))
     (subset? (constants-in body) (map string->symbol ops))]
    [('not-constants (list ops ...))
     (define body-ops (constants-in body))
     (for/and ([op (map string->symbol ops)])
       (not (set-member? body-ops op)))]

    [((? symbol?) (list))
     (define prop (string->symbol (format ":~a" type)))
     (dict-has-key? prop-hash prop)]
    [((? symbol?) (list values ...))
     (define prop (string->symbol (format ":~a" type)))
     (subset? (set-map (dict-ref prop-hash prop '()) ~a) values)]
    [('cites (list values ...))
     (subset? (map string->symbol values) (append-map set->list (dict-ref prop-hash ':cite '())))]
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
