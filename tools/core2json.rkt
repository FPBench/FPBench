#lang racket

(require json)
(require "common.rkt" "fpcore.rkt" "filter.rkt")

(define/contract (core->json core)
  (-> fpcore? jsexpr?)

  (match-define (list 'FPCore (list args ...) props ... body) core)
  (define-values (_ prop-dict) (parse-properties props))
  (define prop-hash
    (for/hash ([(prop value) (in-dict prop-dict)])
      (values prop
              (match prop
                [':cite (map ~a value)]
                [_ (~a value)]))))
  (hash-set*
   prop-hash
   'arguments (map ~a args)
   'body (~a body)
   'operators (map ~a (operators-in body))))

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
