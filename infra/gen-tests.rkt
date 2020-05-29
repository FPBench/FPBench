#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt")
(provide number-suite->tests constant-suite->tests
         op-suite->tests bool-op-suite->tests)

(define (op->value op args)
  `(,op ,@args))

(define (entry->test val props name i n)
  (let* ([name-props (append (list ':name (format "Test ~a (~a/~a)" name i n)) props)]
         [test `(FPCore () ,@name-props ,val)]
         [spec (racket-run-fpcore test '())])
    `(FPCore () ,@name-props :spec ,spec ,val)))

(define (entry->arg-test inputs val props name)
   (let ([name-props (append (list ':name (format "Test ~a (with inputs)" name)) props)])
     `(FPCore ,inputs ,@name-props ,val)))
     
(define (sanity-suite->tests suite props test-file print-proc) ;; print-proc prints each test
  (call-with-output-file test-file #:mode 'text #:exists 'replace
    (lambda (port) (for ([test suite]) (print-proc test props port)))))

(define (constant->test test props port)
  (match-define (list constant lower upper) test)
  (pretty-write (entry->test `(if (and (< ,lower ,constant) (< ,constant ,upper)) 1 0) props constant 1 1) port)
  (fprintf port "\n"))

(define (op->test test props port expr-proc)  ;; expr-proc formats the expression
  (match-define (list op (list args ...)) test)
  (define n (length args))
  (for ([a args] [i (in-naturals)])
    (pretty-write (entry->test (expr-proc op a) props op (+ i 1) n) port)
    (fprintf port "\n")))

(define (constant-suite->tests suite props test-file)
  (sanity-suite->tests suite props test-file constant->test))

(define (number-suite->tests suite props test-file)
  (sanity-suite->tests suite props test-file (curryr op->test (λ (op args) args))))

(define (op-suite->tests suite props test-file)
  (sanity-suite->tests suite props test-file (curryr op->test (λ (op args) (op->value op args)))))

(define (bool-op-suite->tests suite props test-file)
    (sanity-suite->tests suite props test-file (curryr op->test (λ (op args) `(if ,(op->value op args) 1 0)))))
