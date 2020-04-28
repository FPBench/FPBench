#lang racket
(require "common.rkt" "fpcore.rkt")
(provide valid-core unsupported-features 
         invert-op-list invert-const-list invert-round-modes-list
         ieee754-ops ieee754-rounding-modes fpcore-ops fpcore-consts
         operators-in constants-in property-values round-modes-in variables-in-expr)

(provide
  (contract-out
    [struct supported-list
     ([ops (listof symbol?)]
      [consts (listof symbol?)]
      [precisions (listof symbol?)]
      [round-modes (listof symbol?)])]))

;;; Predefined supported lists

(define ieee754-ops '(+ - * / < > <= >= == != fabs fma sqrt))
(define ieee754-rounding-modes '(nearestEven nearestAway toPositive toNegative toZero))

(define fpcore-ops (append operators '(! if let let* while while*))) ; annotation, control constructs, and "operators" in common.rkt
(define fpcore-consts constants) ; same as "constants" in common.rkt

;;; Blacklist <==> Whitelist

(define (invert-op-list list)
  (-> (listof symbol?) (listof symbol?))
  (set-subtract fpcore-ops list))

(define (invert-const-list list)
  (-> (listof symbol?) (listof symbol?))
  (set-subtract constants list))

(define (invert-round-modes-list list)
  (-> (listof symbol?) (listof symbol?))
  (set-subtract ieee754-rounding-modes list))

;;; Core checking

(struct supported-list (ops consts precisions round-modes))

(define (valid-core core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (define supp-prec (supported-list-precisions supp))
  (and  (subset? (operators-in core) (supported-list-ops supp))
        (subset? (constants-in core) (supported-list-consts supp))
        (subset? (round-modes-in core) (supported-list-round-modes supp))  
        (or 
          (equal? core-prec #f)
          (andmap (lambda (e) (set-member? supp-prec e)) (set->list core-prec)))))

(define (unsupported-features core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (set-union
    (set-subtract (operators-in core) (supported-list-ops supp))
    (set-subtract (constants-in core) (supported-list-consts supp))
    (set-subtract (round-modes-in core) (supported-list-round-modes supp))
    (if (equal? core-prec #f) 
        '()
        (set-subtract (set->list core-prec) (supported-list-precisions supp)))))

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
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (operators-in core)
  (-> fpcore? (listof symbol?))
  (match-define (list 'FPCore (list args ...) props ... body) core)
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
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (constants-in core)
  (-> fpcore? (listof symbol?))
  (match-define (list 'FPCore (list args ...) props ... body) core)
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
      [(? symbol?) (void)]
      [(? number?) (void)]))
  out)

(define/contract (property-values core)
  (-> fpcore? property-hash?)
  (match-define (list 'FPCore (list args ...) props ... body) core)
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
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (round-modes-in core)
  (-> fpcore? (listof symbol?))
  (match-define (list 'FPCore (list args ...) props ... body) core)
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
     [(? symbol?) '(list expr)]
     [(? number?) '()])))