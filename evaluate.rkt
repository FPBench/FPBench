#lang racket

(require "src/fpcore-checker.rkt" "src/fpcore-interpreter.rkt" "src/fpcore-reader.rkt" "src/multi-command-line.rkt")

(provide evaluate-main evaluate-body make-evaluate-ctx)

; CLI args passed in through context
(struct evaluate-ctx (in-file out-file check-types? ragged-check? args) #:transparent)

(define (make-evaluate-ctx in-file out-file check-types? ragged-check? args)
  (evaluate-ctx in-file out-file check-types? ragged-check? args))

(define (evaluate-main argv stdin-port stdout-port)
  (define *in-file* (make-parameter "-"))
  (define *out-file* (make-parameter "-"))
  (define check-types? #t)
  (define ragged-check? #t)
  (command-line
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

   (evaluate-body (make-evaluate-ctx (*in-file*) (*out-file*) check-types? ragged-check? args) 
        stdin-port stdout-port)))

(define (evaluate-body ctx stdin-port stdout-port)
  (define check-types? #t)
  (define ragged-check? #t)
   
   (define-values (input-port input-port-name)
     (if (equal? (evaluate-ctx-in-file ctx) "-")
         (values stdin-port "stdin")
         (values (open-input-file (evaluate-ctx-in-file ctx) #:mode 'text) (evaluate-ctx-in-file ctx))))
   (define output-port
     (if (equal? (evaluate-ctx-out-file ctx) "-")
         stdout-port
         (open-output-file (evaluate-ctx-out-file ctx) #:mode 'text #:exists 'truncate)))

   (port-count-lines! input-port)
   (parameterize ([*fpcores* '()] [*check-types* (evaluate-ctx-check-types? ctx)] [*ragged-check* (evaluate-ctx-ragged-check? ctx)])
     (define last
       (for/last ([prog (in-port (curry read-fpcore input-port-name) input-port)] #:when #t)
          (check-fpcore prog)
          prog))
     (if (dict-has-key? (*fpcores*) 'main)
         (fprintf output-port "~a\n" (racket-run-fpcore (first (dict-ref (*fpcores*) 'main)) (evaluate-ctx-args ctx)))
         (fprintf output-port "~a\n" (racket-run-fpcore last (evaluate-ctx-args ctx))))))
   
(module+ main
  (evaluate-main (current-command-line-arguments) (current-input-port) (current-output-port)))
