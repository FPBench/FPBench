#lang racket

(require "../src/common.rkt" "../src/fpcore-checker.rkt" "../src/fpcore-reader.rkt" "../src/supported.rkt" "../src/multi-command-line.rkt")

(provide filter-body)

(define (get-args lst)
  (map (lambda (str)
         (if (string-contains? str ":")
             (string-split str ":")
             (error 'split-list-by-colon "Improper arguments")))
       lst))

(define (filter-apply lst core)
  (for-each (lambda (arg)
    (match arg 
    [(list op val) 
     (match op 
      ['operator
        (set-member? (operators-in core) (string->symbol val))]
      [_
        (error 'split-list-by-colon "Improper arguments")])])) lst))

(define/contract ((filter values) core)
  (-> symbol? (listof string?) (-> fpcore? boolean?))

  (match values
    [(list values ...)
      (match (get-args values)
        [(list (list operator value) ... ) (filter-apply (get-args values) core)]
        [_ (raise-user-error 'filter "Unknown filter with ~a arguments" (length values))])]
    [_ (raise-user-error 'filter "Unknown filter with ~a arguments" (length values))]))

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
