#lang racket

(require math/flonum)
(require "test-common.rkt" "../src/core2cml.rkt")

(define (compile->cml prog ctx type test-file)
  (define s-file (string-replace test-file ".cml" ".S"))
  (define cake-file (string-replace test-file ".cml" ".cake"))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "~a\n" (core->cml prog "f"))
      (fprintf p "fun main () =\nlet\nval args = CommandLine.arguments()\n")
      (for ([i (range N)])
        (fprintf p "val arg~a = Double.fromString (List.nth args ~a)\n" i i))
      (fprintf p "val res = f ~a\nin\n"
        (if (zero? N) "()"
          (string-join (map (curry format "arg~a") (range N)) " ")))
      (fprintf p "print_int (Word64.toInt res)\nend;\n\nmain ();")))
  (system (format "cake <~a >~a --reg_alg=0" test-file s-file))
  (system (format "cc $CAKEML_BASE/basis_ffi.o ~a -o ~a" s-file cake-file))
  cake-file)

(define (run<-cml exec-name ctx types)
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name (map (compose ~a real->double-flonum) (dict-values ctx))) " ")))))
  (define out* (floating-point-bytes->real (integer->integer-bytes (string->number out) 8 #f)))
  (cons (real->double-flonum out*) (number->string out*)))

(define (cml-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
      (or (= a b)
          (and (nan? a) (nan? b)))]))

(define (cml-format-args var val type)
  (format "~a = ~a" var val))

(define (cml-format-output result)
  (format "~a" result))

(define cml-tester (tester "cml" compile->cml run<-cml cml-equality cml-format-args cml-format-output cml-supported))
(module+ main (parameterize ([*tester* cml-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.cml")])
    (exit state))))