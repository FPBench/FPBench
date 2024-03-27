#lang racket

(require "../src/common.rkt" "../src/fpcore-checker.rkt" "../src/fpcore-reader.rkt" "../src/supported.rkt" "../src/multi-command-line.rkt")

(provide filter-body)

(define/contract ((filter type values) core)
  (-> symbol? (listof string?) (-> fpcore? boolean?))

  (match* (type values)
    [((or 'operator 'operation) (list value))
     (set-member? (operators-in core) (string->symbol value))]
    [((or 'operators 'operations) (list ops ...))
     (subset? (operators-in core) (map string->symbol ops))]
    [((or 'not-operators 'not-operations) (list ops ...))
     (define body-ops (operators-in core))
     (for/and ([op (map string->symbol ops)])
       (not (set-member? body-ops op)))]

    [('constant (list value))
     (set-member? (constants-in core) (string->symbol value))]
    [('constants (list ops ...))
     (subset? (constants-in core) (map string->symbol ops))]
    [('not-constants (list ops ...))
     (define body-ops (constants-in core))
     (for/and ([op (map string->symbol ops)])
       (not (set-member? body-ops op)))]

    [((? symbol?) (list))
     (define prop (string->symbol (format ":~a" type)))
     (dict-has-key? (property-values core) prop)]
    [((? symbol?) (list values ...))
     (define prop (string->symbol (format ":~a" type)))
     (subset? (set-map (dict-ref (property-values core) prop '()) ~a) values)]
    [('cites (list values ...))
     (subset? (map string->symbol values) (append-map set->list (dict-ref (property-values core) ':cite '())))]
    [(_ _)
     (raise-user-error 'filter "Unknown filter ~a with ~a arguments" type (length values))]))

(define (filter-body invert? type values in-file out-file stdin-port stdout-port)
   (define input-port
     (if (equal? in-file "-")
         stdin-port
         (open-input-file in-file #:mode 'text)))
   (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))
   (define test
     ((if invert? negate identity)
      (filter (string->symbol type) values)))
   (port-count-lines! (current-input-port))
   (for ([core (in-port (curry read-fpcore input-port) (current-input-port))])
     (when (test core)
       (pretty-print core (current-output-port) 1)
       (newline))))

(module+ main
  (require racket/cmdline)
  (define invert? #f)

  (command-line
   #:program "filter.rkt"
   #:once-each
   [("-v" "--invert") "Invert the meaning of the filter"
    (set! invert? #t)]
   #:args (type . values)
    (filter-body invert? type values (current-input-port) (current-output-port))))
