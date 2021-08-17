#lang racket

(require "common.rkt")
(provide tensor? tensor-ref tensor-dim tensor-size tabulate->tensor)

(define (tensor-val? x)
  (match x
   [(? value?) #t]
   [(? boolean?) #t]
   [(? constant?) #t]
   [(? tensor?) #t]
   [_ #f]))

(define (tensor? x)
  (cond
    [(list? x) (andmap tensor-val? x)]
    [else #f]))

(define (tensor-ref x n . rest)
  (-> tensor? integer? ... integer? any/c)
  (cond [(empty? rest) (list-ref x n)]
        [else (apply (curry tensor-ref (list-ref x n)) rest)]))

(define (tensor-dim x)
  (-> tensor? integer? ... integer? any/c)
  (let loop ([t x])
    (cond [(for/and ([i t]) (tensor? i)) (add1 (apply min (map loop t)))]
          [else 1])))

(define (tensor-size x n)
  (-> tensor? integer? integer?)
  (length (list-ref x n)))

(define (tabulate->tensor sizes vals)
  (-> (listof integer?) (listof any/c) tensor?)
  (unless (= (apply * sizes) (length vals))
    (error 'tabulate->tensor "Not enough values to create a 'rectangular' tensor"))
  (let inner ([sizes* sizes] [vals* vals])
    (cond
      [(= (length sizes*) 1) vals*]
      [else 
        (let ([div (/ (length vals*) (first sizes*))])
          (for/list ([i (in-range (first sizes*))])
            (inner (drop sizes* 1) (take (drop vals* (* i div)) div))))])))
