#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2js.rkt")

(define (compile->js prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
       (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
       (fprintf p "~a~a\n\n" (js-header "Math") (core->js prog "f"))
       (fprintf p "console.log(f(~a));\n"
                (string-join (for/list ([i (range N)])
                               (format "parseFloat(process.argv[~a])" (+ i 2)))
                             ", "))))
  test-file)

(define (float->string x)
  (match x
   [(? gfl?)  (gfl->string x)]
   [(? real?) (~a x)]))

(define (run<-js exec-name ctx type number?)
  (define out
    (with-output-to-string
      (λ ()
        (define file-command (format "node ~a" exec-name))
        (system (string-join (cons file-command (map float->string (dict-values ctx))) " ")))))
  (define out*
    (match (string-trim out)
      ["NaN" "+nan.0"]
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      [(? string->number x) x]))
  (cons
    (parameterize ([gfl-exponent 11] [gfl-bits 64]) (gfl out*))
    out*))

(define (js-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else (<= (abs (gfls-between a b)) ulps)]))

(define (js-format-args var val type)
  (format "~a = ~a" var val))

(define (js-format-output result)
  (format "~a" result))

(define js-tester (tester "js" compile->js run<-js js-equality js-format-args js-format-output (const #t) js-supported #f))

;; TODO: Add types
(module+ main (parameterize ([*tester* js-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.js")])
    (exit state))))