#lang racket

(require "common.rkt" "fpcore.rkt")

(define (call-with-input-files files callback)
  (if (null? files)
      (callback)
      (call-with-input-file (car files)
        (λ (p) (call-with-input-files (cdr files) (λ args (apply callback (cons p args))))))))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "copy-properties.rkt"
   #:args (file1 file2)
   (call-with-input-files (list file1 file2)
     (λ (p1 p2)
       (for ([prog1 (in-port read p1)] [prog2 (in-port read p2)])
         (match-define (list 'FPCore (list _ ...) props ... _) prog1)
         (match-define (list 'FPCore (list args ...) _ ... body) prog2)
         (pretty-print `(FPCore ,args ,@props ,body) (current-output-port) 1))))))
