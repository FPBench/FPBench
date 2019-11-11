#lang racket
(require "common.rkt" "fpcore.rkt")
(provide operators-in constants-in property-values)

(define/contract (operators-in expr)
  (-> expr? (listof symbol?))

  (remove-duplicates
   (match expr
     [`(while ,test ([,vars ,inits ,updates] ...) ,res)
      (cons 'while
            (append (operators-in test)
                    (append-map operators-in inits)
                    (append-map operators-in updates)
                    (operators-in res)))]
     [`(while* ,test ([,vars ,inits ,updates] ...) ,res)
      (cons 'while*
            (append (operators-in test)
                    (append-map operators-in inits)
                    (append-map operators-in updates)
                    (operators-in res)))]
     [`(let ([,vars ,vals] ...) ,body)
      (cons 'let (append (append-map operators-in vals) (operators-in body)))]
     [`(let* ([,vars ,vals] ...) ,body)
      (cons 'let* (append (append-map operators-in vals) (operators-in body)))]
     [`(if ,cond ,ift ,iff)
      (cons 'if (append (operators-in cond) (operators-in ift) (operators-in iff)))]
     [`(! ,props ... ,body)
      (cons '! (operators-in body))]
     [(list op args ...) (cons op (append-map operators-in args))]
     [(? symbol?) '()]
     [(? number?) '()])))

(define/contract (constants-in expr)
  (-> expr? (listof symbol?))

  (remove-duplicates
   (match expr
     [`(,(or 'while 'while*) ,test ([,vars ,inits ,updates] ...) ,res)
            (append (constants-in test)
                    (append-map constants-in inits)
                    (append-map constants-in updates)
                    (constants-in res))]
     [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body)
      (append (append-map constants-in vals) (constants-in body))]
     [`(if ,cond ,ift ,iff)
      (append (constants-in cond) (constants-in ift) (constants-in iff))]
     [`(! ,props ... ,body)
      (constants-in body)]
     [(list op args ...) (append-map constants-in args)]
     [(? constant?) (list expr)]
     [(? symbol?) '()]
     [(? number?) '()])))

(define property-hash? (hash/c symbol? (set/c any/c)))
(define (property-hash-add! hash props)
  (define-values (_ properties) (parse-properties props))
  (for ([(k v) (in-dict properties)])
    (hash-update! hash k (curryr set-add v) (set))))

(define/contract (property-values expr)
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
