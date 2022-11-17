#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2rust.rkt")

(define (compile->rust prog ctx type test-file)
  (define bit-length (match type ['binary64 "64"] ['binary32 "32"]))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p (rust-header ""))
      (fprintf p "#[allow(dead_code)]\nfn strtox(arg: String) -> f~a {\n    use std::str::FromStr;\n    f~a::from_str(&arg).unwrap_or_default()\n}\n\n" bit-length bit-length)
      (fprintf p "~a\n" (core->rust prog "f"))
      (fprintf p "fn main() {\n")
      (fprintf p "    print!(\"{:.20E}\", f(~a));\n}\n"
          (string-join (map (curry format "strtox(std::env::args().nth(~a).unwrap())") (map add1 (range N))) ", "))))
  (define bin-file (string-replace test-file ".rs" "-rust.bin"))
    (system (format "rustc -o ~a ~a" bin-file test-file))
    bin-file)

(define (run<-rust exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "NaN"]
       ["+inf.0" "inf"]
       ["-inf.0" "-inf"]
       [x x])))
  (define out
    (with-output-to-string
     (λ ()
      (system (string-join (cons exec-name in) " ")))))
  (define out*
    (match out
      ["NaN" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (rust-equality a b ulps type ignore?)
  (cond
    [(equal? a 'timeout) true]
    [else (<= (abs (gfls-between a b)) ulps)]))

(define (rust-format-args var val type)
  (format "~a = ~a" var val))

(define (rust-format-output result)
  (format "~a" result))

(define rust-tester (tester "rust" compile->rust run<-rust rust-equality rust-format-args rust-format-output (const #t) rust-supported #f))

; Command line
(module+ main (parameterize ([*tester* rust-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.rs")])
    (exit state))))
