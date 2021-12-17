#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2ocaml.rkt")

(define (compile->ocaml prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N
        (match prog
         [(list 'FPCore (list args ...) rest ...) (length args)]
         [(list 'FPCore name (list args ...) rest ...) (length args)]))
      (fprintf p "~a~a\n" (ocaml-header) (core->ocaml prog "f"))
      (fprintf p "let _ =\n  Printf.printf \"%.17g\" (f ~a)\n"
        (if (zero? N)
            "()"
            (string-join
              (for/list ([i (in-range 1 (+ 1 N))])
                (format "(float_of_string Sys.argv.(~a))" i))
              " ")))))
  (define bin-file (string-replace test-file ".ml" ""))
  (system (format "ocamlopt -o ~a -w -A ~a" bin-file test-file))
  bin-file)

(define (run<-ocaml exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "nan"]
       ["+inf.0" "infinity"]
       ["-inf.0" "neg_infinity"]
       [x x])))
  (define out
    (with-output-to-string
      (λ ()
        (system (format "~a ~a" exec-name (string-join in " "))))))
  (define out*
    (match out
     ["nan" "+nan.0"]
     ["infinity" "+inf.0"]
     ["neg_infinity" "-inf.0"]
     [x x]))
  (cons (->value out* type) out*))

(define (ocaml-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (ocaml-format-args var val type)
  (format "~a = ~a" var val))

(define (ocaml-format-output result)
  (format "~a" result))

(define ocaml-tester
  (tester "ocaml"
    compile->ocaml
    run<-ocaml
    ocaml-equality
    ocaml-format-args
    ocaml-format-output
    (const #t)
    ocaml-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* ocaml-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.ml")))
  (exit state))
