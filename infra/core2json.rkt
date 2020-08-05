#lang racket

(require json)
(require "../src/common.rkt" "../src/fpcore-checker.rkt" "../src/fpcore-reader.rkt" "../src/supported.rkt" "../src/core2fptaylor.rkt")

(define (~pp value)
  (let ([p (open-output-string)])
    (when (and (number? value) (number-round-trips? value))
      (set! value (exact->inexact value)))
    (pretty-print value p 1 #:newline? #f)
    (get-output-string p)))

(define (number-round-trips? n)
   (parameterize ([read-decimal-as-inexact #f])
     (= n (string->number (number->string (exact->inexact n))))))

(define/contract (core->json core)
  (-> fpcore? jsexpr?)
  (define-values (args props body)
   (match core
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define-values (_ prop-dict) (parse-properties props))
  (define prop-hash
    (for/hash ([(prop value) (in-dict prop-dict)])
      (values prop
              (match prop
                [':cite (map ~pp value)]
                [':example
                 (for/hash ([(k v) (in-dict value)])
                   (values k (~pp (car v))))]
                [_ (if (string? value) value (~pp value))]))))

  (define core-fptaylor
    (with-handlers ([(const true) (const false)])
      (core->fptaylor core 'foo)))

  (hash-set*
   prop-hash
   'arguments (map ~pp args)
   'body (~pp body)
   'operators (map ~pp (set->list (operators-in core)))
   'core (~pp core)
   'core_fptaylor core-fptaylor))

(module+ main
  (define padding-function #f)
  (command-line
   #:program "core2json.rkt"
   #:once-each
   ["--padding" padding "A function to wrap the resulting JSON with, for making JSONP"
    (set! padding-function padding)]
   #:args files
   (when padding-function
     (printf "~a(" padding-function))
   (write-json
    (apply append
           (for/list ([file files])
             (call-with-input-file file
               (λ (p)
                 (for/list ([core (in-port (curry read-fpcore file) p)])
                   (core->json core)))))))
   (when padding-function
     (printf ");"))))
