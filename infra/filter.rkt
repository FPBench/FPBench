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

(define (filter-body invert? values in-file out-file stdin-port stdout-port)
   (define test
     ((if invert? negate identity)
      (filter values)))
  (define input-port
     (if (equal? in-file "-")
         stdin-port
         (open-input-file in-file #:mode 'text)))
  (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))
   (port-count-lines! (current-input-port))
   (for ([core (in-port (curry read-fpcore "stdin") (current-input-port))])
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
    (filter-body invert? values in-file out-file (current-input-port) (current-output-port))))
