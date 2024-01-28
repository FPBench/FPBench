#lang racket

(require "src/fpcore-checker.rkt" "src/fpcore-interpreter.rkt" "src/fpcore-reader.rkt" "src/multi-command-line.rkt")

(provide evaluate-main)

(define (evaluate-main argv stdin-port stdout-port)
  (define *in-file* (make-parameter "-"))
  (define *out-file* (make-parameter "-"))
  (define check-types? #t)
  (define ragged-check? #t)
  (multi-command-line
   #:program "evaluate.rkt"
   #:argv argv
   #:once-each
   [("-i" "--in-file") in_file_ "Input file to read FPCores from"
     (*in-file* in_file_)]
   [("-o" "--out-file") out_file_ "Output file to write evaluated results to"
     (*out-file* out_file_)]
   ["--no-check" "Disables type checking altogether (check level 1). Recursive, mutually recursive, and out-of-order FPCores can be evaluated in this mode"
     (set! check-types? #f)]
   ["--no-ragged-check" "Disables checking for ragged dimension sizes"
     (set! ragged-check? #f)]
   ;; maybe a way to provide a context?
   ;; context override information?
   #:args args
   
   (define-values (input-port input-port-name)
     (if (equal? (*in-file*) "-")
         (values stdin-port "stdin")
         (values (open-input-file (*in-file*) #:mode 'text) (*in-file*))))
   (define output-port
     (if (equal? (*out-file*) "-")
         stdout-port
         (open-output-file (*out-file*) #:mode 'text #:exists 'truncate)))

   (port-count-lines! input-port)
   (parameterize ([*fpcores* '()] [*check-types* check-types?] [*ragged-check* ragged-check?])
     (define last
       (for/last ([prog (in-port (curry read-fpcore input-port-name) input-port)] #:when #t)
          (check-fpcore prog)
          prog))
     (if (dict-has-key? (*fpcores*) 'main)
         (fprintf output-port "~a\n" (racket-run-fpcore (first (dict-ref (*fpcores*) 'main)) args))
         (fprintf output-port "~a\n" (racket-run-fpcore last args))))))
   
(module+ main
  (evaluate-main (current-command-line-arguments) (current-input-port) (current-output-port)))
