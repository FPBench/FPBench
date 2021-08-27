#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2java.rkt" "../src/evaluator.rkt")

(define (compile->java prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "~a~a\n" (java-header "Test") (core->java prog "f"))
      (fprintf p "public static void main(String[] args) {\n")
      (define strtox
        (match type
         ['binary64 "Double.parseDouble(args[~a])"] 
         ['binary32 "Float.parseFloat(args[~a])"]))
      (fprintf p "\tSystem.out.printf(\"%.~a\", f(~a));\n}\n~a"
               (match type ['binary64 "17g"] ['binary32 "10g"])
               (string-join (map (curry format strtox) (range N)) ", ")
               (java-footer))))
  (system (format "javac ~a" test-file))
  test-file) ; this gets ignored

(define (run<-java exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "NaN"]
       ["+inf.0" "Infinity"]
       ["-inf.0" "-Infinity"]
       [x x])))
  (define out
    (with-output-to-string
     (λ ()
       (system (format "java -cp /tmp Test ~a"
                       (string-join in " "))))))
  (define out*
    (match out
      ["NaN" "+nan.0"]
      ["Infinity" "+inf.0"]
      ["-Infinity" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (java-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (java-format-args var val type)
  (format "~a = ~a" var val))

(define (java-format-output result)
  (format "~a" result))

(define java-tester
  (tester "java"
    compile->java run<-java
    java-equality java-format-args java-format-output
    (const #t) java-supported #f))

; Command line
(module+ main (parameterize ([*tester* java-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/Test.java")])
    (exit state))))
