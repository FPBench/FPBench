#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2cakeml.rkt")

(define (compile->cml prog ctx type test-file)
  (define s-file (string-replace test-file ".cml" ".S"))
  (define cake-file (string-replace test-file ".cml" ".cake"))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "~a\n" (core->cml prog "f"))
      (fprintf p "fun main () =\nlet\nval args = CommandLine.arguments()\n")
      (for ([i (range N)])
        (define arg-index (* 2 i))
        (fprintf p
          "val arg~a = Double.fromWord (Word64.orb (Word64.<< (Word64.fromInt (Option.valOf (Int.fromNatString (List.nth args ~a)))) 32) (Word64.fromInt (Option.valOf (Int.fromNatString (List.nth args ~a)))))\n"
          i arg-index (add1 arg-index)))
      (fprintf p "val res = f ~a\nin\n"
        (if (zero? N) "()"
          (string-join (map (curry format "arg~a") (range N)) " ")))
      (fprintf p "print (Double.toString res)\nend;\n\nmain ();")))
  (system (format "cake <~a >~a --reg_alg=0" test-file s-file))
  (system (format "cc $CAKEML_BIN/basis_ffi.c ~a -lm -o ~a" s-file cake-file))
  cake-file)

(define (cml-arguments value)
  (define word
    (integer-bytes->integer
     (real->floating-point-bytes (value->real value) 8)
     #f))
  (define-values (high low) (quotient/remainder word (expt 2 32)))
  (list (~a high) (~a low)))

(define (run<-cml exec-name ctx types number?)
  (define command
    (string-join
     (cons exec-name
           (apply append (map cml-arguments (dict-values ctx))))
     " "))
  (define status #f)
  (define out
    (with-output-to-string
     (λ ()
       (set! status (system (format "~a 2>&1" command))))))
  (define out*
    (match (string-downcase (string-trim out))
      [(or "nan" "+nan" "-nan") +nan.0]
      [(or "inf" "+inf") +inf.0]
      ["-inf" -inf.0]
      [out (or (string->number out)
               (error 'run<-cml "CakeML produced invalid output (status ~a, command ~a): ~s" status command out))]))
  (cons (->value out* 'binary64) out*))

(define (cml-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else (<= (abs (gfls-between a b)) ulps)]))

(define (cml-format-args var val type)
  (format "~a = ~a" var val))

(define (cml-format-output result)
  (format "~a" result))

(define cml-tester (tester "cml" compile->cml run<-cml cml-equality cml-format-args cml-format-output (const #t) cml-supported #f))
(module+ main (parameterize ([*tester* cml-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.cml")])
    (exit state))))
