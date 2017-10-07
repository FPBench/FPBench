#lang racket

(require "common.rkt" "fpcore.rkt")

(define/contract (remove-let expr [bindings '()])
  (->* (expr?) ((dictof symbol? expr?)) expr?)
  (match expr
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) (dict-ref bindings expr expr)]
    [`(let ([,vars ,vals] ...) ,body)
     (define vals* (map (curryr remove-let bindings) vals))
     (remove-let body (apply dict-set* bindings (append-map list vars vals*)))]
    [`(,op ,args ...) (cons op (map (curryr remove-let bindings) args))]))

(define/contract (extract-let gensym expr [bindings '()])
  (->* ((-> symbol? symbol?) expr?) ((dictof symbol? symbol?))
       (cons/c (listof (cons/c symbol? expr?)) expr?))
  (match expr
    [(? constant?) (cons '() expr)]
    [(? number?) (cons '() expr)]
    [(? symbol?) (cons '() (dict-ref bindings expr expr))]
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (match-define (list (cons val-defs vals*) ...)
       (map (curryr (curry extract-let gensym) bindings) vals))
     (define defs (map cons vars* vals*))
     (define bindings*
       (for/fold ([bindings* bindings]) ([var* vars*] [var vars])
         (dict-set bindings* var var*)))
     (match-define (cons body-defs res)
       (extract-let gensym body bindings*))
     (cons (append (apply append val-defs) defs body-defs) res)]
    [`(while ,args ...)
     (error 'extract-let "while is not supported")]
    [`(,op ,args ...)
     (match-define (list (cons defs args*) ...)
       (map (curryr (curry extract-let gensym) bindings) args))
     (cons (apply append defs) `(,op ,@args*))]))

(define/contract (canonicalize expr)
  (-> expr? expr?)
  (match expr
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]
    [(list (and (or '== '< '> '<= '>=) op) args ...)
     (define args* (map canonicalize args))
     (for/fold ([expr* 'TRUE]) ([a args*] [b (cdr args*)])
       (if (constant? expr*) `(,op ,a ,b) `(and ,expr* (,op ,a ,b))))]
    [(list '!= args ...)
     (define args* (map canonicalize args))
     (let loop ([args args*] [expr 'TRUE])
       (if (<= (length args) 1)
           expr
           (loop (cdr args)
                 (for/fold ([expr expr]) ([b (cdr args)])
                   (if (constant? expr) `(!= ,(car args) ,b) `(and ,expr (!= ,(car args) ,b)))))))]
    [`(,op ,args ...) `(,op ,@(map canonicalize args))]))

;; from core2scala.rkt
(define/contract (unroll-loops expr n)
  (-> expr? exact-nonnegative-integer? expr?)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     `(let (,@(for/list ([var vars] [val vals]) (list var (unroll-loops val n))))
        ,(unroll-loops body n))]
    [`(if ,cond ,ift ,iff)
     `(if ,(unroll-loops cond n) ,(unroll-loops ift n) ,(unroll-loops iff n))]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     `(let (,@(map list vars inits))
        ,(if (= n 0)
             retexpr
             (unroll-loops `(while ,cond (,@(map list vars updates updates)) ,retexpr)
                           (- n 1))))]
    [`(,(? operator? op) ,args ...)
     (cons op (map (curryr unroll-loops n) args))]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) expr]))


(module+ test
  (define *names* (make-parameter (mutable-set)))

  (define (gensym name)
    (define prefixed
      (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
    (define options
      (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a~a" name (+ i 1))))))
    (define name*
      (car (set-subtract options prefixed)))
    (set-add! (*names*) name*)
    name*)

  (parameterize ([read-decimal-as-inexact #f])
    (for ([prog (in-port (curry read-fpcore "test")
                         (open-input-file "../benchmarks/test.fpcore"))])
      (match-define (list 'FPCore (list args ...) props ... body) prog)
      (define-values (_ properties) (parse-properties props))
      (define pre (dict-ref properties ':pre 'TRUE))
      (printf "body: ~a\n\n" body)
      (define body* (unroll-loops body 3))
      (pretty-print body*)
      (printf "canonicalized pre: ~a\n\n" (canonicalize pre))
      (match-define (cons a b) (extract-let gensym body*))
      (printf "extract: defs = ~a\n~a\n\n" a b)
      (printf "remove:  ~a\n" (remove-let body))
      (printf "remove*: ~a\n\n\n" (remove-let body*))))
  
)