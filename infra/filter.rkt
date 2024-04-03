#lang racket

(require "../src/common.rkt" "../src/fpcore-checker.rkt" "../src/fpcore-reader.rkt" "../src/supported.rkt" "../src/multi-command-line.rkt")

(provide filter-body)

(define (get-args str)
  (if (string-contains? str ":")
             (string-split str ":")
             '()))

(define/contract ((filter query) core)
  (-> string? (-> fpcore? boolean?))
  (match (get-args query)
    [(list op val)
      (match (string->symbol op)
        ['operator 
          (set-member? (operators-in core) (string->symbol val))]
        [_ (raise-user-error 'filter "Unknown filter operation ~a" op)])]
    [_ (raise-user-error 'filter "Improperly formatted input string ~a" query)]))

(define (filter-body invert? queries in-file out-file stdin-port stdout-port)
  (define-values (input-port input-port-name)
    (if (equal? in-file "-")
        (values stdin-port "stdin")
        (values (open-input-file in-file #:mode 'text) in-file)))
  (define test
    ((if invert? negate identity)
      (andmap filter queries)))
  (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))
   (port-count-lines! (current-input-port))
   (for ([core (in-port (curry read-fpcore input-port-name) input-port)])
     (when (test core)
       (pretty-print core output-port 1)
       (newline))))

(module+ main
  (require racket/cmdline)
  (define invert? #f)
  (define *filter-in* (make-parameter "-"))
  (define *filter-out* (make-parameter "-"))

  (command-line
   #:program "filter.rkt"
   #:once-each
   [("-i" "--in-file") in_file_ "Input file to read FPCores from"
        (*filter-in* in_file_)]
   [("-o" "--out-file") out_file_ "Output file to write evaluated results to"
      (*filter-out* out_file_)]
   [("-v" "--invert") "Invert the meaning of the filter"
    (set! invert? #t)]
   #:args queries
    (filter-body invert? queries (*filter-in*) (*filter-out*) (current-input-port) (current-output-port))))
