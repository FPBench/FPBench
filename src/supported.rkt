#lang racket
(require racket/hash racket/set)
(require "common.rkt" "fpcore-checker.rkt" "fpcore-visitor.rkt")
(provide valid-core make-program-features unsupported-features
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
(struct program-features (operators constants properties arguments))

(define/contract (make-program-features core)
  (-> any/c (or/c #f program-features?))
  (and (fpcore? core) (collect-program-features core)))

(define (collect-program-features core)
  (define-values (args props body)
    (match core
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define operators (mutable-set))
  (define constants (mutable-set))
  (define properties (make-hash))
  (define (add-properties! props)
    (define-values (_ property-dict) (parse-properties props))
    (for ([(property value) (in-dict property-dict)])
      (hash-update! properties property
                    (lambda (property-values)
                      (set-add! property-values value)
                      property-values)
                    (mutable-set))))
  (define-expr-visitor feature-visitor
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
     (set-add! operators 'if)
     (visit/ctx vtor cond ctx)
     (visit/ctx vtor ift ctx)
     (visit/ctx vtor iff ctx)]
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
     (set-add! operators let_)
     (for ([val (in-list vals)]) (visit/ctx vtor val ctx))
     (visit/ctx vtor body ctx)]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
     (set-add! operators while_)
     (visit/ctx vtor cond ctx)
     (for ([init (in-list inits)] [update (in-list updates)])
       (visit/ctx vtor init ctx)
       (visit/ctx vtor update ctx))
     (visit/ctx vtor body ctx)]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [ctx '()])
     (set-add! operators for_)
     (for ([val (in-list vals)]) (visit/ctx vtor val ctx))
     (for ([init (in-list inits)] [update (in-list updates)])
       (visit/ctx vtor init ctx)
       (visit/ctx vtor update ctx))
     (visit/ctx vtor body ctx)]
    [(visit-tensor vtor vars vals body #:ctx [ctx '()])
     (set-add! operators 'tensor)
     (for ([val (in-list vals)]) (visit/ctx vtor val ctx))
     (visit/ctx vtor body ctx)]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [ctx '()])
     (set-add! operators 'tensor*)
     (for ([val (in-list vals)]) (visit/ctx vtor val ctx))
     (for ([init (in-list inits)] [update (in-list updates)])
       (visit/ctx vtor init ctx)
       (visit/ctx vtor update ctx))
     (visit/ctx vtor body ctx)]
    [(visit-! vtor props body #:ctx [ctx '()])
     (set-add! operators '!)
     (add-properties! props)
     (visit/ctx vtor body ctx)]
    [(visit-constant vtor constant #:ctx [ctx '()])
     (set-add! constants constant)]
    [(visit-op_ vtor operator args #:ctx [ctx '()])
     (set-add! operators operator)
     (for ([arg (in-list args)]) (visit/ctx vtor arg ctx))])
  (add-properties! props)
  (for ([arg (in-list args)])
    (match arg
      [(list '! arg-props ... name) (add-properties! arg-props)]
      [_ (void)]))
  (visit feature-visitor body)
  (program-features
   (for/set ([operator (in-set operators)]) operator)
   (for/set ([constant (in-set constants)]) constant)
   (for/hash ([(property property-values) (in-hash properties)])
     (values property (for/set ([value (in-set property-values)]) value)))
   (for/list ([arg (in-list args)])
     (match arg
       [(list '! arg-props ... name) name]
       [_ arg]))))

(define (core->features core)
  (if (program-features? core) core (make-program-features core)))

(define (valid-core core supp)
  (define features (core->features core))
  (and features
       (let* ([properties (program-features-properties features)]
              [core-precs (dict-ref properties ':precision #f)]
              [core-rnd-modes (dict-ref properties ':round #f)])
         (and (for/and ([operator (in-set (program-features-operators features))])
                ((supported-list-ops supp) operator))
              (for/and ([constant (in-set (program-features-constants features))])
                ((supported-list-consts supp) constant))
              (or (not core-precs)
                  (for/and ([prec (in-set core-precs)])
                    ((supported-list-precisions supp) prec)))
              (or (not core-rnd-modes)
                  (for/and ([mode (in-set core-rnd-modes)])
                    ((supported-list-round-modes supp) mode)))
              (or (supported-list-tensor-args? supp)
                  (for/and ([arg (in-list (program-features-arguments features))])
                    (not (list? arg))))))))

(define (unsupported-features core supp)
  (define features (core->features core))
  (define core-properties (program-features-properties features))
  (define core-precs (dict-ref core-properties ':precision #f))
  (define core-rnd-modes (dict-ref core-properties ':round #f))
  (set-union
   (filter-not (supported-list-ops supp)
               (set->list (program-features-operators features)))
   (filter-not (supported-list-consts supp)
               (set->list (program-features-constants features)))
   (if core-precs
       (filter-not (supported-list-precisions supp) (set->list core-precs))
       '())
   (if core-rnd-modes
       (filter-not (supported-list-round-modes supp) (set->list core-rnd-modes))
       '())))

(define (operators-in core)
  (program-features-operators (core->features core)))

(define (constants-in core)
  (program-features-constants (core->features core)))

(define (property-values core)
  (program-features-properties (core->features core)))

(module+ test
  (define exprs (list
    `(+ a b)
    `(* (+ a b) E)
    `(let ([a LOG2E] [b 2]) (sqrt (/ a b)))
    `(let* ([a 1] [b PI]) (cbrt (/ a b)))
    `(while (< a b) ([a 0 (+ a 2)] [b 100 (- b 1)]) (* (exp a) (sin b)))
    `(while* (< a b) ([a 0 (+ a 2)] [b 100 (- b 1)]) (* (log a) (cos b)))
    `(! :precision binary64 (- (! :round toPositive (+ x y)) (! :round toNegative (+ x y))))))

  (define features
    (for/list ([expr (in-list exprs)])
      (make-program-features `(FPCore () ,expr))))

  (check-equal?
   (map program-features-operators features)
   (map list->set `((+) (* +) (let sqrt /) 
                    (let* cbrt /) (while < + - * exp sin)
                    (while* < + - * log cos) (! - +))))

  (check-equal?
    (map program-features-constants features)
    (map list->set `(() (E) (LOG2E) (PI) () () ())))

  (check-equal?
    (map program-features-properties features)
    (list (hash) (hash) (hash) (hash) (hash) (hash)
          (hash ':precision (set 'binary64) ':round (set 'toNegative 'toPositive))))

  (define combined-features
    (make-program-features
     '(FPCore ((! :precision binary32 x) y)
        :precision binary64
        :round nearestEven
        (! :precision binary16
           (! :round toPositive
              (let* ([a (+ x E)] [b (* a PI)])
                (if (< a b) b a)))))))
  (check-equal? (program-features-operators combined-features)
                (set '! 'let* '+ '* 'if '<))
  (check-equal? (program-features-constants combined-features)
                (set 'E 'PI))
  (check-equal? (program-features-properties combined-features)
                (hash ':precision (set 'binary32 'binary64 'binary16)
                      ':round (set 'nearestEven 'toPositive)))
  (check-equal? (program-features-arguments combined-features) '(x y))

  (define named-core
    '(FPCore named ((! :precision binary32 x)) :precision binary64 :spec x x))
  (define named-features (make-program-features named-core))
  (check-equal? (program-features-operators named-features) (set))
  (check-equal? (program-features-constants named-features) (set))
  (check-equal? (program-features-properties named-features)
                (hash ':precision (set 'binary32 'binary64) ':spec (set 'x)))
  (check-equal? (program-features-arguments named-features) '(x))
  (check-equal? (operators-in named-features) (program-features-operators named-features))
  (check-equal? (constants-in named-features) (program-features-constants named-features))
  (check-equal? (property-values named-features) (program-features-properties named-features))

  (define check-core
    '(FPCore (x) :precision binary64 :round nearestEven
             (! :precision binary32 (! :round toPositive (+ x E)))))
  (define check-features (make-program-features check-core))
  (define supported-all
    (supported-list (lambda (op) (and (memq op '(+ !)) #t))
                    (lambda (const) (eq? const 'E))
                    (lambda (prec) (and (memq prec '(binary32 binary64)) #t))
                    (lambda (mode) (and (memq mode '(nearestEven toPositive)) #t))
                    #t))
  (define unsupported-round
    (struct-copy supported-list supported-all
                 [round-modes (lambda (mode) (eq? mode 'nearestEven))]))
  (check-true (valid-core check-core supported-all))
  (check-true (valid-core check-features supported-all))
  (check-false (valid-core check-features unsupported-round))
  (check-false (make-program-features 'not-a-core))
  (check-false (valid-core 'not-a-core supported-all))
  (check-false (set-mutable? (program-features-operators check-features)))
  (check-false (set-mutable? (program-features-constants check-features)))
  (check-false
   (set-mutable?
    (dict-ref (program-features-properties check-features) ':precision))))
