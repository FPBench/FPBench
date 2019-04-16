#lang racket

(require "common.rkt" "fpcore.rkt")

(provide core-common-subexpr-elim)

(define (core-common-subexpr-elim prog)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define cs-hash (count-common-subexpr body))
  (define cse-body (common-subexpr-elim cs-hash body))
  `(FPCore ,args ,@props ,cse-body))

(define (combine-common-subexpr-hash h1 h2)
  (for ([(k v) (in-hash h2)])
    (if (hash-has-key? h1 k)
      (hash-set! h1 k #t)
      (hash-set! h1 k v)))
  h1)

(define (count-common-subexpr expr)
  (cond 
    [(list? expr)
     (match-define (list op args ...) expr)
     (if (eq? op '!)
       (count-common-subexpr (last args))
       (let ([cs-hash (make-hash (list (cons expr #f)))])
         (for ([e args])
           (combine-common-subexpr-hash cs-hash (count-common-subexpr e)))
         cs-hash))]
    [else (make-hash)]))

(define (common-subexpr-elim cs-hash expr)
  (define intermediates '())
  (define name-hash (make-hash))

  (define (common-subexpr-body expr)
    (cond
      [(list? expr)
       (match-define (list op args ...) expr)
       (if (eq? op '!)
         (list op (take args (sub1 (length args)))
                  (common-subexpr-body (last args)))
         ;; returns #f if subexpr doesn't appear more than once
         (let ([elimed-exprs (for/list ([arg args])
                               (common-subexpr-body arg))])
           (if (hash-ref cs-hash expr #f)
             (if (hash-has-key? name-hash expr)
               (hash-ref name-hash expr)
               (let* ([expr-name (string->symbol
                                   (format "i~a" (hash-count name-hash)))]
                      [fixed-expr (cons op elimed-exprs)])
                 (hash-set! name-hash expr expr-name)
                 (set! intermediates (cons (list expr-name fixed-expr)
                                           intermediates))
                 expr-name))
             `(,op ,@(for/list ([arg args])
                       (common-subexpr-body arg))))))]
      [else expr]))

  (define final-expr (common-subexpr-body expr))
  (if (empty? intermediates)
    final-expr
    `(let (,@(reverse intermediates)) ,final-expr)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "common-subexpr-elim.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (core-common-subexpr-elim expr)))))
