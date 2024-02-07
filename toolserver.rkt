#lang racket

(require racket/stream)
(require "export.rkt" "transform.rkt" "evaluate.rkt" "src/multi-command-line.rkt")
(provide toolserver-main toolserver-body)

;; Non-buffering byte sequences

;; byte-sequence->input-port :: sequence? (#:name any?) -> input-port?
;; converts a sequence of bytes into an input port
(define (byte-sequence->input-port seq #:name [name 'string])
  (define reader (byte-stream->input-port-reader (sequence->stream seq)))
  (make-input-port name reader #f void))

;; byte-stream->input-port-reader :: stream? -> bytes? -> (or/c eof-object? 1)
;; converts a stream of bytes into an input port reader
(define ((byte-stream->input-port-reader st) s)
  (cond
    [(stream-empty? st) eof]
    [else (bytes-set! s 0 (stream-first st))
          (set! st (stream-rest st))
          1]))

(module+ test
  (require rackunit)
  (test-case "finite byte sequence"
    (define port (byte-sequence->input-port '(1 2 3 10 10 4 10 7)))
    (check-equal? (bytes 1 2 3) (read-bytes-line port))
    (check-equal? (bytes) (read-bytes-line port))
    (check-equal? (bytes 4) (read-bytes-line port))
    (check-equal? (bytes 7) (read-bytes-line port))
    (check-pred eof-object? (read-bytes-line port)))

  (test-case "infinite byte sequence"
    (define seq (in-naturals 7))
    (define port (byte-sequence->input-port seq))
    (check-equal? (bytes 7 8 9) (read-bytes-line port))
    (check-equal? (bytes 11 12) (read-bytes 2 port)))

  (test-case "read lazily"
    (define port (open-input-bytes (bytes 0 1 2 3 4 5 6 7 8)))
    (define port* (byte-sequence->input-port (for/stream ([b (in-input-port-bytes port)])
                                               #:final (or (= b 2) (= b 5))
                                               (+ 100 b))))
    (check-equal? 0 (read-byte port))
    (check-equal? 101 (read-byte port*))
    (check-equal? 2 (read-byte port))
    (check-equal? 103 (read-byte port*))
    (check-equal? 4 (read-byte port))
    (check-equal? 105 (read-byte port*))
    (check-equal? 6 (read-byte port))
    (check-pred eof-object? (read-byte port*))
    (check-equal? 7 (read-byte port))
    (check-pred eof-object? (read-byte port*))
    (check-equal? 8 (read-byte port))
    (check-pred eof-object? (read-byte port*))
    (check-pred eof-object? (read-byte port))))

;; Tool server logic

;; try to get a platform-independent newline
(define newline-port (open-output-string))
(newline newline-port)
(define newline-string (get-output-string newline-port))
(define separator-string ".")

(define (read-one-command batch-port)
  (let loop ([line (read-line batch-port)])
    (if (eof-object? line)
        '()
        (let ([argv (string-split (string-trim line))])
          (if (or (empty? argv) (equal? argv (list separator-string)))
              (loop (read-line batch-port))
              argv)))))

;; read one input without buffering
(define (read-one-input port)
  (byte-sequence->input-port
   (for*/stream ([line (in-lines port)]
                 #:break (equal? (string-trim line) separator-string)
                 [c (in-bytes (string->bytes/utf-8 (string-append line newline-string)))])
     c)
   #:name 'stdin))

(define (serve-batch batch-port default-output-port)
  (let loop ([command (read-one-command batch-port)])
    (unless (empty? command)
      (match command
        [(list "transform" argv ...)
         (define port (read-one-input batch-port))
         (transform-main argv port default-output-port)
         ;; force an input to be read in case port is unused
         (sequence-for-each void port)]
        [(list "export" argv ...)
         (define port (read-one-input batch-port))
         (export-main argv port default-output-port)
         ;; force an input to be read in case port is unused
         (sequence-for-each void port)]
        [(list "evaluate" argv ...)
         (define port (read-one-input batch-port))
         (evaluate-main argv port default-output-port)
         ;; force an input to be read in case port is unused
         (sequence-for-each void port)]
        [_
         (fprintf (current-error-port)
                  "Invalid command sequence ~a: (should start with 'export' or 'transform' or 'evaluate')"
                  command)
         (newline (current-error-port))
         (fprintf (current-error-port) "Skipping input:")
         (newline (current-error-port))
         (fprintf (current-error-port) "~a" (port->string (read-one-input batch-port)))
         (newline (current-error-port))
         (flush-output (current-error-port))])
      (loop (read-one-command batch-port)))))

(define (toolserver-main argv stdin-port stdout-port)
  (define batches (box '()))
  (define (register-batch in out)
    (set-box! batches (cons (list in out) (unbox batches))))

  (command-line
   #:program "toolserver.rkt"
   #:argv argv
   #:multi
   ["--batch" batch_in_ batch_out_ "Process commands from a file"
              (register-batch batch_in_ batch_out_)]
   #:args ()
   
   (toolserver-body batches stdin-port stdout-port)))

(define (toolserver-body batches stdin-port stdout-port)
  (define (server-batches)
    (reverse (unbox batches)))

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
         (unless (equal? out-file "-") (close-output-port output-port)))))

(module+ main
  (toolserver-main (current-command-line-arguments) (current-input-port) (current-output-port)))
