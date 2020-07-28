#lang racket
(require "common.rkt" "fpcore-checker.rkt")
(provide valid-core unsupported-features 
         invert-op-proc invert-const-proc invert-rnd-mode-proc
         ieee754-ops ieee754-rounding-modes fpcore-ops fpcore-consts
         operators-in constants-in property-values round-modes-in variables-in-expr)

(provide
  (contract-out
    [struct supported-list
     ([ops (-> symbol? boolean?)]
      [consts (-> symbol? boolean?)]
      [precisions (-> symbol? boolean?)]
      [round-modes (-> symbol? boolean?)])]))

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
  (remove-duplicates
   (match expr
     [`(while ,test ([,vars ,inits ,updates] ...) ,res)
      (cons 'while
            (append (operators-in-expr test)
                    (append-map operators-in-expr inits)
                    (append-map operators-in-expr updates)
                    (operators-in-expr res)))]
     [`(while* ,test ([,vars ,inits ,updates] ...) ,res)
      (cons 'while*
            (append (operators-in-expr test)
                    (append-map operators-in-expr inits)
                    (append-map operators-in-expr updates)
                    (operators-in-expr res)))]
     [`(let ([,vars ,vals] ...) ,body)
      (cons 'let (append (append-map operators-in-expr vals) (operators-in-expr body)))]
     [`(let* ([,vars ,vals] ...) ,body)
      (cons 'let* (append (append-map operators-in-expr vals) (operators-in-expr body)))]
     [`(if ,cond ,ift ,iff)
      (cons 'if (append (operators-in-expr cond) (operators-in-expr ift) (operators-in-expr iff)))]
     [`(! ,props ... ,body)
      (cons '! (operators-in-expr body))]
     [(list op args ...) (cons op (append-map operators-in-expr args))]
     [(? hex?) '()]
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (operators-in core)
  (-> fpcore? (listof symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (operators-in-expr body))

(define/contract (constants-in-expr expr)
  (-> expr? (listof symbol?))
  (remove-duplicates
   (match expr
     [`(,(or 'while 'while*) ,test ([,vars ,inits ,updates] ...) ,res)
            (append (constants-in-expr test)
                    (append-map constants-in-expr inits)
                    (append-map constants-in-expr updates)
                    (constants-in-expr res))]
     [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body)
      (append (append-map constants-in-expr vals) (constants-in-expr body))]
     [`(if ,cond ,ift ,iff)
      (append (constants-in-expr cond) (constants-in-expr ift) (constants-in-expr iff))]
     [`(! ,props ... ,body)
      (constants-in-expr body)]
     [(list op args ...) (append-map constants-in-expr args)]
     [(? constant?) (list expr)]
     [(? hex?) '()]
     [(? symbol?) '()]
     [(? number?) '()])))

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
  (let loop ([expr expr])
    (match expr
      [`(,(or 'while 'while*) ,test ([,vars ,inits ,updates] ...) ,res)
       (loop test) (for-each loop inits) (for-each loop updates) (loop res)]
      [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body)
       (for-each loop vals) (loop body)]
      [`(if ,cond ,ift ,iff)
       (loop cond) (loop ift) (loop iff)]
      [`(! ,props ... ,body)
       (property-hash-add! out props)
       (loop body)]
      [(list op args ...) (for-each loop args)]
      [(? hex?) (void)]
      [(? symbol?) (void)]
      [(? number?) (void)]))
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

(define/contract (round-modes-in-expr expr)
  (-> expr? (listof symbol?))
  (remove-duplicates
   (match expr
     [`(,(or 'while 'while*) ,test ([,vars ,inits ,updates] ...) ,res)
            (append (round-modes-in-expr test)
                    (append-map round-modes-in-expr inits)
                    (append-map round-modes-in-expr updates)
                    (round-modes-in-expr res))]
     [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body)
      (append (append-map round-modes-in-expr vals) (round-modes-in-expr body))]
     [`(if ,cond ,ift ,iff)
      (append (round-modes-in-expr cond) (round-modes-in-expr ift) (round-modes-in-expr iff))]
     [`(! ,props ... ,body)
      (let ([rnd-mode (dict-ref (apply hash-set* #hash() props) ':round #f)])
        (append (round-modes-in-expr body)
                (if (equal? rnd-mode #f) '() (list rnd-mode))))]
     [(list op args ...) (append-map round-modes-in-expr args)]
     [(? constant?) '()]
     [(? hex?) '()]
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (round-modes-in core)
  (-> fpcore? (listof symbol?))
  (define-values (args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (remove-duplicates
    (let ([rnd-mode (dict-ref (apply hash-set* #hash() props) ':round #f)]
          [in-body (round-modes-in-expr body)])
        (if (equal? rnd-mode #f) 
            (append in-body '(nearestEven)) 
            (append (list rnd-mode) in-body))))) 

(define/contract (variables-in-expr expr)
  (-> expr? (listof symbol?))
  (remove-duplicates
   (match expr
     [`(,(or 'while 'while*) ,test ([,vars ,inits ,updates] ...) ,res)
            (append (variables-in-expr test)
                    (append-map variables-in-expr inits)
                    (append-map variables-in-expr updates)
                    (variables-in-expr res))]
     [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body)
      (append (append-map variables-in-expr vals) (variables-in-expr body))]
     [`(if ,cond ,ift ,iff)
      (append (variables-in-expr cond) (variables-in-expr ift) (variables-in-expr iff))]
     [`(! ,props ... ,body)
      (variables-in-expr body)]
     [(list op args ...) (append-map variables-in-expr args)]
     [(? constant?) '()]
     [(? hex?) '()]
     [(? symbol?) '(list expr)]
     [(? number?) '()])))