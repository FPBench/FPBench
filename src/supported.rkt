#lang racket
(require "common.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt")
(provide valid-core unsupported-features 
         invert-op-proc invert-const-proc invert-rnd-mode-proc
         ieee754-ops ieee754-rounding-modes fpcore-ops fpcore-consts
         operators-in constants-in property-values variables-in-expr)

(provide
  (contract-out
    [struct supported-list
     ([ops (-> symbol? boolean?)]
      [consts (-> symbol? boolean?)]
      [precisions (-> symbol? boolean?)]
      [round-modes (-> symbol? boolean?)])]))

(module+ test
  (require rackunit))

;;; Predefined supported procs

(define ieee754-ops (curry set-member? '(+ - * / < > <= >= == != fabs fma sqrt)))
(define ieee754-rounding-modes 
  (curry set-member? '(nearestEven nearestAway toPositive toNegative toZero)))

(define fpcore-ops (curry set-member? (append operators '(! if let let* while while* digits))))
(define fpcore-consts (curry set-member? constants)) ; same as "constants" in common.rkt

;;; Blacklist <==> Whitelist

(define (invert-op-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) fpcore-ops))

(define (invert-const-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) fpcore-consts))

(define (invert-rnd-mode-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) ieee754-rounding-modes))

;;; Core checking

(struct supported-list (ops consts precisions round-modes))

(define (valid-core core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (and (andmap (supported-list-ops supp) (operators-in core))
       (andmap (supported-list-consts supp) (constants-in core))
       (andmap (supported-list-round-modes supp) (round-modes-in core))
       (or (not core-prec)
           (andmap (supported-list-precisions supp) (set->list core-prec)))))

(define (unsupported-features core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (set-union
    (filter-not (supported-list-ops supp) (operators-in core))
    (filter-not (supported-list-consts supp) (constants-in core))
    (filter-not (supported-list-round-modes supp) (round-modes-in core))
    (if core-prec
        (filter-not (supported-list-precisions supp) (set->list core-prec))
        '())))

(define/contract (operators-in-expr expr)
  (-> expr? (listof symbol?))
  (define vtor
    (struct-copy visitor default-reduce-visitor ; default behavior is counting terminals
      [visit-terminal_ (λ (vtor a #:ctx ctx) '())]
      [visit-op (λ (vtor op args #:ctx ctx) (list op))]
      [reduce (curry apply append)]))
  (remove-duplicates (visit vtor expr)))

(define/contract (operators-in core)
  (-> fpcore? (listof symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (operators-in-expr body))

(define/contract (constants-in-expr expr)
  (-> expr? (listof symbol?))
  (define vtor
    (struct-copy visitor default-reduce-visitor ; default behavior is counting terminals
      [visit-terminal_ (λ (vtor a #:ctx ctx) '())]
      [visit-constant (λ (vtor a #:ctx ctx) (list a))]
      [reduce (curry apply append)]))
  (remove-duplicates (visit vtor expr)))

(define/contract (constants-in core)
  (-> fpcore? (listof symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (constants-in-expr body))  

(define property-hash? (hash/c symbol? (set/c any/c)))
(define (property-hash-add! hash props)
  (define-values (_ properties) (parse-properties props))
  (for ([(k v) (in-dict properties)])
    (hash-update! hash k (curryr set-add v) (set))))

(define/contract (property-values-expr expr)
  (-> expr? property-hash?)
  (define out (make-hash))
  (define vtor
    (struct-copy visitor default-transform-visitor
      [visit-! (λ (vtor props body #:ctx ctx)
                  (property-hash-add! out props)
                  (visit-!/transform vtor props body #:ctx ctx))]))
  (visit vtor expr)
  out)

(define/contract (property-values core)
  (-> fpcore? property-hash?)
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define prop-hash (property-values-expr body))
  (property-hash-add! prop-hash props)
  prop-hash) 

(define/contract (variables-in-expr expr)
  (-> expr? (listof symbol?))
  (define vtor
    (struct-copy visitor default-reduce-visitor ; default behavior is counting terminals
      [visit-terminal_ (λ (vtor a #:ctx ctx) '())]
      [visit-symbol (λ (vtor a #:ctx ctx) (list a))]
      [reduce (curry apply append)]))
  (remove-duplicates (visit vtor expr)))

(module+ test
  (define exprs (list
    `(+ a b)
    `(* (+ a b) E)
    `(let ([a LOG2E] [b 2]) (sqrt (/ a b)))
    `(let* ([a 1] [b PI]) (cbrt (/ a b)))
    `(while (< a b) ([a 0 (+ a 2)] [b 100 (- b 1)]) (* (exp a) (sin b)))
    `(while* (< a b) ([a 0 (+ a 2)] [b 100 (- b 1)]) (* (log a) (cos b)))
    `(! :precision binary64 (- (! :round toPositive (+ x y)) (! :round toNegative (+ x y))))))

  (check-equal?
    (map operators-in-expr exprs)
   `((+) (*) (sqrt) (cbrt) (< + - *) (< + - *) (-)))

  (check-equal?
    (map constants-in-expr exprs)
   `(() (E) (LOG2E) (PI) () () ()))

  (check-equal?
    (map property-values-expr exprs)
    (list (make-hash) (make-hash) (make-hash) (make-hash) (make-hash) (make-hash) 
        (make-hash `((:precision . ,(set 'binary64)) (:round . ,(set 'toNegative 'toPositive)))))))
