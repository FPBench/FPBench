#lang racket
(require racket/hash)
(require "common.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt")
(provide valid-core unsupported-features
         invert-op-proc invert-const-proc invert-rnd-mode-proc
         ieee754-ops ieee754-rounding-modes fpcore-ops fpcore-consts
         operators-in constants-in property-values)

(provide
  (contract-out
    [struct supported-list
     ([ops (-> symbol? boolean?)]
      [consts (-> symbol? boolean?)]
      [precisions (-> any/c boolean?)]
      [round-modes (-> symbol? boolean?)]
      [tensor-args? boolean?])]))

(module+ test
  (require rackunit))

;;; Predefined supported procs

(define ieee754-ops (curry set-member? '(+ - * / < > <= >= == != fabs fma sqrt)))
(define ieee754-rounding-modes
  (curry set-member? '(nearestEven nearestAway toPositive toNegative toZero)))

(define fpcore-ops (curry set-member? (append operators '(! if let let* while while* digits))))
(define fpcore-consts (curry set-member? constants)) ; same as "constants" in common.rkt

;;; Blacklist <==> Whitelist

(define/contract (invert-op-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) fpcore-ops))

(define/contract (invert-const-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) fpcore-consts))

(define/contract (invert-rnd-mode-proc proc)
  (-> (-> symbol? boolean?) (-> symbol? boolean?))
  (conjoin (negate proc) ieee754-rounding-modes))

;;; Core checking

(struct supported-list (ops consts precisions round-modes tensor-args?))

(define (valid-core core supp)
  (define core-precs (dict-ref (property-values core) ':precision #f))
  (define core-rnd-modes (dict-ref (property-values core) ':round #f))
  (and (andmap (supported-list-ops supp) (set->list (operators-in core)))
       (andmap (supported-list-consts supp) (set->list (constants-in core)))
       (or (not core-precs)
           (andmap (supported-list-precisions supp) (set->list core-precs)))
       (or (not core-rnd-modes)
           (andmap (supported-list-round-modes supp) (set->list core-rnd-modes)))
       (or (supported-list-tensor-args? supp)
           (andmap (negate list?) (arguments-in core)))))

(define (unsupported-features core supp)
  (define core-precs (dict-ref (property-values core) ':precision #f))
  (define core-rnd-modes (dict-ref (property-values core) ':round #f))
  (set-union
    (filter-not (supported-list-ops supp) (set->list (operators-in core)))
    (filter-not (supported-list-consts supp) (set->list (constants-in core)))
    (if core-precs
        (filter-not (supported-list-precisions supp) (set->list core-precs))
        '())
    (if core-rnd-modes
        (filter-not (supported-list-round-modes supp) (set->list core-rnd-modes))
        '())))

(define/reduce-expr (operators-in-expr expr)
; (-> expr? (set/c symbol?))
  [(visit-if vtor cond ift iff)
   (set-add (visit-if/reduce vtor cond ift iff) 'if)]
  [(visit-let_ vtor let_ vars vals body)
   (set-add (visit-let_/reduce vtor let_ vars vals body) let_)]
  [(visit-while_ vtor while_ cond vars inits updates body)
   (set-add (visit-while_/reduce vtor while_ cond vars inits updates body) while_)]
  [(visit-for_ vtor for_ vars vals accums inits updates body)
   (set-add (visit-for_/reduce vtor for_ vars vals accums inits updates body) for_)]
  [(visit-tensor vtor vars vals body)
   (set-add (visit-tensor/reduce vtor vars vals body) 'tensor)]
  [(visit-tensor* vtor vars vals accums inits updates body)
   (set-add (visit-tensor*/reduce vtor vars vals accums inits updates body) 'tensor*)]
  [(visit-! vtor props body)
   (set-add (visit vtor body) '!)]
  [(visit-terminal_ vtor x)
   (set)]
  [(visit-op_ vtor op args)
   (set-add (visit-op_/reduce vtor op args) op)]
  [reduce
   (λ (sets) (apply set-union (set) sets))])

(define/contract (operators-in core)
  (-> fpcore? (set/c symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (operators-in-expr body))

(define/reduce-expr (constants-in-expr expr)
; (-> expr? (set/c symbol?)
  [(visit-terminal_ vtor x) (set)]
  [(visit-constant vtor x) (set x)]
  [reduce (λ (sets) (apply set-union (set) sets))])

(define/contract (constants-in core)
  (-> fpcore? (set/c symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (constants-in-expr body))

(define (arguments-in core)
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (for/list ([arg (in-list args)])
    (match arg
      [(list '! props ... name) name]
      [_ arg])))

(define property-hash? (hash/c symbol? (set/c any/c)))
(define (property-hash-add prop-hash props)
  (define-values (_ properties) (parse-properties props))
  (for/fold ([prop-hash* prop-hash])
            ([(k v) (in-dict properties)])
    (hash-update prop-hash* k (curryr set-add v) (set))))

(define/reduce-expr (property-values-expr expr)
  [(visit-! vtor props body)
   (property-hash-add (visit vtor body) props)]
  [(visit-terminal_ vtor x)
   (hash)]
  [(reduce args)
   (if (empty? args) (hash)
       (apply (curryr hash-union #:combine/key
                      (lambda (k v1 v2) (set-union v1 v2))) args))])

(define (property-values-variables prop-hash vars)
  (for/fold ([prop-hash* prop-hash]) ([var vars])
    (match var
      [(list '! props ... name)
       (property-hash-add prop-hash* props)]
      [_
       prop-hash*])))

(define/contract (property-values core)
  (-> fpcore? property-hash?)
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define prop-hash (property-values-variables (property-values-expr body) args))
  (property-hash-add prop-hash props))

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
   (map list->set (map operators-in-expr exprs))
   (map list->set `((+) (* +) (let sqrt /) 
                    (let* cbrt /) (while < + - * exp sin)
                    (while* < + - * log cos) (! - +))))

  (check-equal?
    (map list->set (map constants-in-expr exprs))
    (map list->set `(() (E) (LOG2E) (PI) () () ())))

  (check-equal?
    (map property-values-expr exprs)
    (list (hash) (hash) (hash) (hash) (hash) (hash)
          (hash ':precision (set 'binary64) ':round (set 'toNegative 'toPositive)))))
