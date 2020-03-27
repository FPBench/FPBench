#lang racket/base

(provide byte-sequence->input-port)
(require racket/stream)

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
