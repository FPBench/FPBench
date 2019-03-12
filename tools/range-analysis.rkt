#lang racket

(require "common.rkt" "fpcore.rkt")
(provide (struct-out interval) make-interval nonempty-bounded?
         condition->range-table range-table-ref)

;; Range analysis is based on https://github.com/uwplse/herbie/blob/master/src/range-analysis.rkt

; Standard min and max can transform exact numbers into inexact numbers
(define (exact-max arg1 . args)
  (for/fold ([max arg1]) ([arg args])
    (if (< max arg) arg max)))

(define (exact-min arg1 . args)
  (for/fold ([min arg1]) ([arg args])
    (if (< min arg) min arg)))

;; NOTE: an interval can also be #f for an empty interval
(struct interval (l u) #:transparent)

(define (make-interval l u)
  (cond
    [(<= l u) (interval l u)]
    [else #f]))

(define (nonempty-bounded? intvl)
  (match intvl
    [(interval l u) (and (rational? l) (rational? u) (<= l u))]
    [else #f]))

(define (interval-intersect interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1) interval1)
    (match-define (interval l2 u2) interval2)
    (make-interval (exact-max l1 l2) (exact-min u1 u2))]
   [else #f]))

(define (interval-union interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1) interval1)
    (match-define (interval l2 u2) interval2)
    (make-interval (exact-min l1 l2) (exact-max u1 u2))]
   [interval1 interval1]
   [else interval2]))

(define (interval-invert intvl)
  (match intvl
    [(interval -inf.0 +inf.0) #f]
    [(interval -inf.0 u) (interval u +inf.0)]
    [(interval l +inf.0) (interval -inf.0 l)]
    [_ (interval -inf.0 +inf.0)]))

(define (make-range-table x intvl)
  (make-hash (list (cons x intvl))))

(define (make-empty-range-table)
  (make-hash))

;; NOTE: a range-table can also be #f for an invalid range-table 
(define (make-null-range-table)
  #f)

(define (range-table-ref rt x)
  (and rt (hash-ref rt x (interval -inf.0 +inf.0))))

(define (range-table-intersect table1 table2)
  (cond
    [(not table1) #f]
    [(not table2) #f]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)])
       (if (hash-has-key? table2 key1)
           (hash-set! new-range-table key1 (interval-intersect (hash-ref table1 key1) (hash-ref table2 key1)))
           (hash-set! new-range-table key1 (hash-ref table1 key1))))
     (for ([key2 (hash-keys table2)] #:unless (hash-has-key? new-range-table key2))
       (hash-set! new-range-table key2 (hash-ref table2 key2)))
     new-range-table]))

(define (range-table-union table1 table2)
  (cond
    [(not table1) table2]
    [(not table2) table1]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)] #:when (hash-has-key? table2 key1))
       (hash-set! new-range-table key1 (interval-union (hash-ref table1 key1) (hash-ref table2 key1))))
     new-range-table]))

(define (range-table-invert table)
  (cond
    [(and table (= (count identity (hash-values table)) 1))
     (match-define (list (cons var itvl)) (filter cdr (hash->list table)))
     (make-range-table var (interval-invert itvl))]
    [else
     (make-empty-range-table)]))

(define (flip-cmp cmp)
  (match cmp
    ['< '>]
    ['> '<]
    ['<= '>=]
    ['>= '<=]
    ['== '==]))

(define (parse-cmp cmp)
  (match cmp ['< <] ['> >] ['<= <=] ['>= >=] ['== =]))

(define (condition->range-table condition)
  (match condition
    [(list (and (or '< '> '<= '>= '==) cmp) (? number? a) (? number? b))
     (if ((parse-cmp cmp) a b)
         (make-empty-range-table)
         (make-null-range-table))]
    ['TRUE (make-empty-range-table)]
    ['FALSE (make-null-range-table)]
    [`(== ,(? symbol? var) ,(? number? num))
     (make-range-table var (make-interval num num))]
    [(list (or '< '<=) (? symbol? var) (? number? num))
     (make-range-table var (make-interval -inf.0 num))]
    [`(,(or '< '<= '==) (fabs ,(? symbol? var)) ,(? number? num))
     (make-range-table var (make-interval (- num) num))]
    [`(,(or '> '>=) ,(? symbol? var) ,(? number? num))
     (make-range-table var (make-interval num +inf.0))]
    [`(,(or '> '>=) (fabs ,(? symbol? var)) ,(? number? num))
     (make-empty-range-table)]
    [(list (and (or '< '<= '== '>= '>) cmp) (? number? num) var) ; don't check for variable? here b/c fabs
     (condition->range-table (list (flip-cmp cmp) var num))]
    [(list (and (or '< '<= '> '>=) cmp) exprs ...)
     (if (not (equal? (filter number? exprs) (sort (filter number? exprs) (parse-cmp cmp))))
       #f
       (let
         ([from-left (last-number exprs)]
          [from-right (reverse (last-number (reverse exprs)))])
         (foldl range-table-intersect
                (make-empty-range-table)
                (for/list ([left from-left] [expr exprs] [right from-right]
                          #:when (symbol? expr) #:unless (number? expr))
                  (range-table-intersect
                   (if left
                       (condition->range-table (list cmp left expr))
                       (make-empty-range-table))
                   (if right
                       (condition->range-table (list cmp expr right))
                       (make-empty-range-table)))))))]
    [(list '== exprs ...)
     (define num (get-all-equal-value exprs))
     (if num
         (foldl range-table-intersect
                (make-empty-range-table)
                (map (lambda (x) (make-range-table x (make-interval num num)))
                     (filter symbol? exprs)))
         (make-null-range-table))]

    [`(and ,conds ...)
     (foldl range-table-intersect (make-empty-range-table) (map condition->range-table conds))]
    [`(or ,conds ...)
     (foldl range-table-union (make-null-range-table) (map condition->range-table conds))]
    [`(not ,cond1) (range-table-invert (condition->range-table cond1))]
    [_
     (make-empty-range-table)]))
 
(define (range-table-ref rt x)
  (if rt
      (hash-ref rt x (list (interval -inf.0 +inf.0 #f #f)))
      '()))

(define (get-all-equal-value lst)
  (let ([nums (filter number? lst)])
    (if (foldl (lambda (x y) (and x y)) #t (map (lambda (x) (= x (car nums))) nums))
        (car nums)
        #f)))

(define (last-number lst)
  (let loop ([lst lst] [last #f])
    (match lst
      ['() '()]
      [(cons (? number? x) rest)
       (cons x (loop rest x))]
      [(cons _ rest)
       (cons last (loop rest last))])))
