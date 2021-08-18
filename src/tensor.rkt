#lang racket

(require "common.rkt")
(provide
  (contract-out
   [tensor? (-> any/c boolean?)]
   [tensor-ref (-> tensor? integer? ... integer? any/c)]
   [tensor-dim (-> tensor? integer?)]
   [tensor-size (-> tensor? integer? integer?)]
   [tabulate->tensor (-> (listof integer?) (listof any/c) tensor?)]))


(define (tensor-val? x)
  (match x
   [(? value?) #t]
   [(? boolean?) #t]
   [(? constant?) #t]
   [(? tensor?) #t]
   [_ #f]))

(define (tensor? x)
  (and (list? x) (andmap tensor-val? x)))

(define (tensor-ref x n . rest)
  (if (empty? rest)
      (list-ref x n)
      (apply (curry tensor-ref (list-ref x n)) rest)))

(define (tensor-dim x)
  (let loop ([t x])
    (if (andmap tensor? t)
        (+ (apply min (map loop t)) 1)
        1)))

(define (tensor-size x n)
  (let loop ([x x] [n n])
    (if (zero? n)
        (length x)
        (loop (list-ref x 0) (- n 1)))))

(define (tabulate->tensor sizes vals)
  (unless (= (apply * sizes) (length vals))
    (error 'tabulate->tensor "Not enough values to create a 'rectangular' tensor"))
  (let loop ([sizes* sizes] [vals* vals])
    (if (= (length sizes*) 1)
        vals*
        (let ([div (/ (length vals*) (first sizes*))])
          (for/list ([i (in-range (first sizes*))])
            (loop (drop sizes* 1)
                  (take (drop vals* (* i div)) div)))))))
