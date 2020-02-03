#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2js.rkt")

(define (compile->js prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
       (define N (length (second prog)))
       (fprintf p "~a\n\n" (core->js prog "f"))
       (fprintf p "console.log(f(~a));\n"
                (string-join (for/list ([i (range N)])
                               (format "parseFloat(process.argv[~a])" (+ i 2)))
                             ", "))))
  test-file)

(define (run<-js exec-name ctx type)
  (define out
    (with-output-to-string
      (λ ()
        (define file-command (format "node ~a" exec-name))
        (system (string-join (cons file-command (map (compose ~a real->double-flonum) (dict-values ctx))) " ")))))
  (define out*
    (match (string-trim out)
      ["NaN" "+nan.0"]
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      [(? string->number x) x]))
  (cons (real->double-flonum (string->number out*)) out*))

(define (js-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
      (or (= a b)
          (and (nan? a) (nan? b))
          (<= (abs (flonums-between a b)) ulps))]))

(define (js-format-args var val type)
  (format "~a = ~a" var val))

(define (js-format-output result)
  (format "~a" result))

(define js-tester (tester "js" compile->js run<-js js-equality js-format-args js-format-output js-supported))

;; TODO: Add types
(module+ main (parameterize ([*tester* js-tester])
  (test-imperative (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.js")))