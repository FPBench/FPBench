#lang racket

(require "export.rkt" "transform.rkt" "utils/byte-sequence.rkt")
(provide toolserver-main)

;; try to get a platform-independent newline
(define newline-port (open-output-string))
(newline newline-port)
(define newline-string (get-output-string newline-port))

(define (read-one-command batch-port)
  (let loop ([line (read-line batch-port)])
    (if (eof-object? line)
        '()
        (let ([argv (string-split (string-trim line))])
          (if (empty? argv)
              (loop (read-line batch-port))
              argv)))))

;; read one input without buffering
(define (read-one-input port)
  (byte-sequence->input-port
   (for*/stream ([line (in-lines port)]
                 #:break (equal? (string-trim line) ".")
                 [c (in-bytes (string->bytes/utf-8 (string-append line newline-string)))])
     c)
   #:name 'stdin))

(define (serve-batch batch-port default-output-port)
  (let loop ([command (read-one-command batch-port)])
    (unless (empty? command)
      (match command
        [(list "transform" argv ...)
         (transform-main argv (read-one-input batch-port) default-output-port)]
        [(list "export" argv ...)
         (export-main argv (read-one-input batch-port) default-output-port)]
        [_
         (fprintf (current-error-port)
                  "Invalid command sequence ~a: (should start with 'export' or 'transform')"
                  command)
         (newline (current-error-port))
         (fprintf (current-error-port) "Skipping input:")
         (newline (current-error-port))
         (newline (current-error-port))
         (fprintf (current-error-port) "~a" (port->string (read-one-input batch-port)))
         (newline (current-error-port))
         (newline (current-error-port))
         (flush-output (current-error-port))])
      (loop (read-one-command batch-port)))))

(define (toolserver-main argv stdin-port stdout-port)
  (define batches (box '()))
  (define (register-batch in out)
    (set-box! batches (cons (list in out) (unbox batches))))
  (define (server-batches)
    (reverse (unbox batches)))

  (command-line
   #:program "toolserver.rkt"
   #:argv argv
   #:multi
   ["--batch" batch_in_ batch_out_ "Process commands from a file"
              (register-batch batch_in_ batch_out_)]
   #:args ()

   (if (empty? (server-batches))
       (serve-batch stdin-port stdout-port)
       (for ([batch (server-batches)])
         (match-define (list in-file out-file) batch)

         (define input-port
           (if (equal? in-file "-")
               stdin-port
               (open-input-file in-file #:mode 'text)))
         (define output-port
           (if (equal? out-file "-")
               stdout-port
               (open-output-file out-file #:mode 'text #:exists 'truncate)))

         (serve-batch input-port output-port)
         (unless (equal? in-file "-") (close-input-port input-port))
         (unless (equal? out-file "-") (close-output-port output-port))))))

(module+ main
  (toolserver-main (current-command-line-arguments) (current-input-port) (current-output-port)))
