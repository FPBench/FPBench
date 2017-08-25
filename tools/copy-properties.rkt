#lang racket

(require "common.rkt" "fpcore.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "copy-properties.rkt"
   #:args ()
   (define prog1 (read))
   (match-define (list 'FPCore (list _ ...) props ... _) prog1)
   (define prog2 (read))
   (match-define (list 'FPCore (list args ...) _ ... body) prog2)
   (define prog2* `(FPCore ,args ,@props ,body))

   (pretty-print prog2* (current-output-port) 1)))
