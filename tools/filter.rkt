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
    [`(! ,props ... ,body)
     (operators-in body)]
    [(list op args ...) (cons op (append-map operators-in args))]
    [(? symbol?) '()]
    [(? number?) '()]))

(define property-hash? (hash/c symbol? (set/c any/c)))
(define (property-hash-add! hash props)
  (define-values (_ properties) (parse-properties props))
  (for ([(k v) (in-dict properties)])
    (hash-update! hash k (curryr set-add v) (set))))

(define/contract (property-values expr)
  (-> expr? property-hash?)

  (define out (make-hash))

  (let loop ([expr expr])
    (match expr
      [`(while ,test ([,vars ,inits ,updates] ...) ,res)
       (loop test) (for-each loop inits) (for-each loop updates) (loop res)]
      [`(let ([,vars ,vals] ...) ,body)
       (for-each loop vals) (loop body)]
      [`(if ,cond ,ift ,iff)
       (loop cond) (loop ift) (loop iff)]
      [`(! ,props ... ,body)
       (property-hash-add! out props)
       (loop body)]
      [(list op args ...) (for-each loop args)]
      [(? symbol?) (void)]
      [(? number?) (void)]))

  out)

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
