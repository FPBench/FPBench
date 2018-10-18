#lang racket

(require "../tools/common.rkt" "../tools/fpcore.rkt")
(provide number-suite->tests constant-suite->tests
         op-suite->tests bool-op-suite->tests
         op-suite->arg-tests bool-op-suite->arg-tests)

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

;; there's certainly a better way to break this up...

(define (number-suite->tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list number (list args ...)) test)
        (define n (length args))
        (for ([a args] [i (in-naturals)])
          (pretty-write (entry->test a props number (+ i 1) n) port)
          (fprintf port "\n"))))))

(define (constant-suite->tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list constant lower upper) test)
          (pretty-write (entry->test `(if (and (< ,lower ,constant) (< ,constant ,upper)) 1 0) props constant 1 1) port)
          (fprintf port "\n")))))

(define (op-suite->tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list op (list args ...)) test)
        (define n (length args))
        (for ([a args] [i (in-naturals)])
          (pretty-write (entry->test (op->value op a) props op (+ i 1) n) port)
          (fprintf port "\n"))))))

(define (bool-op-suite->tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list op (list args ...)) test)
        (define n (length args))
        (for ([a args] [i (in-naturals)])
          (pretty-write (entry->test `(if ,(op->value op a) 1 0) props op (+ i 1) n) port)
          (fprintf port "\n"))))))

(define (op-suite->arg-tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list op (list args ...)) test)
        (define nargs (length (first args)))
        (define inputs (for/list ([i (in-range nargs)])
                         (string->symbol (format "arg~a" i))))
        (pretty-write (entry->arg-test inputs `(,op ,@inputs) props op) port)
        (fprintf port "\n")))))

(define (bool-op-suite->arg-tests suite props test-file)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port)
      (for ([test suite])
        (match-define (list op (list args ...)) test)
        (unless (or (eq? op 'and) (eq? op 'or) (eq? op 'not))
          (define nargs (length (first args)))
          (define inputs (for/list ([i (in-range nargs)])
                           (string->symbol (format "arg~a" i))))
          (pretty-write (entry->arg-test inputs `(if (,op ,@inputs) 1 0) props op) port)
          (fprintf port "\n"))))))
