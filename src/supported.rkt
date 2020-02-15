#lang racket
(require "common.rkt" "fpcore.rkt")
(provide valid-core unsupported-features operators-in constants-in property-values
         invert-op-list invert-const-list) 

(provide
  (contract-out
    [struct supported-list
     ([ops (listof symbol?)]
      [consts (listof symbol?)]
      [precisions (listof symbol?)])]))

(struct supported-list (ops consts precisions))

; Blacklist <==> Whitelist

(define (invert-op-list list)
  (-> (listof symbol?) (listof symbol?))
  (set-subtract (append operators '(if let let* while while* !)) list))

(define (invert-const-list list)
  (-> (listof symbol?) (listof symbol?))
  (set-subtract constants list))

; Core checking

(define (valid-core core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (define supp-prec (supported-list-precisions supp))
  (and (subset? (operators-in core) (supported-list-ops supp))
        (subset? (constants-in core) (supported-list-consts supp))
        (or 
          (equal? core-prec #f)
          (andmap (lambda (e) (set-member? supp-prec e)) (set->list core-prec)))))

(define (unsupported-features core supp)
  (define core-prec (dict-ref (property-values core) ':precision #f))
  (set-union
    (set-subtract (operators-in core) (supported-list-ops supp))
    (set-subtract (constants-in core) (supported-list-consts supp))
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
