#lang racket

(require "common.rkt" "fpcore.rkt")
(provide format-number remove-let canonicalize unroll-loops)

(define (factor n k)
  (if (or (= n 0) (< k 2))
      (values n 0)
      (let loop ([n (inexact->exact n)] [e 0])
        (define-values (q r) (quotient/remainder n k))
        (if (= r 0)
            (loop q (+ e 1))
            (values n e)))))

; TODO: use (order-of-magnitude n) from racket/math to get normalized results
(define/contract (format-number n)
  (-> rational? string?)
  (define t (inexact->exact n))
  (let*-values ([(d e10) (factor (denominator t) 10)]
                [(d e5) (factor d 5)]
                [(d e2) (factor d 2)])
    (cond
      [(= t 0) (~a t)]
      [(> d 1) (format "(~a)" t)]
      [else
       (let*-values ([(m) (if (> e2 e5) (expt 5 e2) (expt 2 e5))]
                     [(n en10) (factor (* (numerator t) m) 10)]
                     [(e) (- en10 (+ e2 e5 e10))])
         (cond
           [(<= 0 e 3)
            (format "~a~a" n (make-string e #\0))]
           [else (format "~ae~a" n e)]))])))

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

(define/match (negate-cmp cmp)
  [('==) '!=]
  [('!=) '==]
  [('<) '>=]
  [('>) '<=]
  [('<=) '>]
  [('>=) '<]
  [(_) (error 'negate-cmp "Unsupported operation ~a" cmp)])

(define/contract (canonicalize expr #:neg [neg #f])
  ;; Transforms multiple argument operations (comparisons, and, or) into binary operations.
  ;; Removes logical negations.
  ; TODO: negation of 'if and 'while
  (-> expr? expr?)
  (match expr
    ['TRUE (if neg 'FALSE 'TRUE)]
    ['FALSE (if neg 'TRUE 'FALSE)]
    [(? constant?) expr]
    [(? number?) expr]
    [(? symbol?) (if neg `(not ,expr) expr)]
    [`(let ([,vars ,vals] ...) ,body)
     `(let (,@(for/list ([var vars] [val vals]) (list var (canonicalize val))))
        ,(canonicalize body #:neg neg))]
    [(list (and (or '== '< '> '<= '>=) op) args ...)
     (define args* (map canonicalize args))
     (define op* (if neg (negate-cmp op) op))
     (define conj (if neg 'or 'and))
     (for/fold ([expr* (if neg 'FALSE 'TRUE)]) ([a args*] [b (cdr args*)])
       (if (constant? expr*) `(,op* ,a ,b) `(,conj ,expr* (,op* ,a ,b))))]
    [(list '!= args ...)
     (define args* (map canonicalize args))
     (define op (if neg '== '!=))
     (define conj (if neg 'or 'and))
     (let loop ([args args*] [expr (if neg 'FALSE 'TRUE)])
       (if (<= (length args) 1)
           expr
           (loop (cdr args)
                 (for/fold ([expr expr]) ([b (cdr args)])
                   (if (constant? expr)
                       `(,op ,(car args) ,b)
                       `(,conj ,expr (,op ,(car args) ,b)))))))]
    [(list 'not arg) (canonicalize arg #:neg (not neg))]
    [(list (or 'and 'or) arg) (canonicalize arg #:neg neg)]
    [(list 'and args ... arg)
     `(,(if neg 'or 'and) ,(canonicalize (cons 'and args) #:neg neg) ,(canonicalize arg #:neg neg))]
    [(list 'or args ... arg)
     `(,(if neg 'and 'or) ,(canonicalize (cons 'or args) #:neg neg) ,(canonicalize arg #:neg neg))]
    [`(,op ,args ...) `(,op ,@(map (curry canonicalize #:neg neg) args))]))

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
  (require rackunit)

  (parameterize ([read-decimal-as-inexact #f])
    (for ([n '(1 10/3 -2.71 -3.1e-5 #e1e+100 1e+100 -7/2 0.1)])
      (check-equal?
       (string->number (string-trim (format-number n) #px"[()]"))
       (inexact->exact n))))
        
  (check-equal?
   (remove-let '(let ([x (+ a b)]) (+ x a)))
   '(+ (+ a b) a))
  
  (check-equal?
   (remove-let '(let ([x (+ a b)] [y (* a 2)])
                  (let ([x (+ y x)] [a b])
                    (- x a))))
   '(- (+ (* a 2) (+ a b)) b))

  (check-equal?
   (remove-let '(let ([x (+ a b)])
                  (if (let ([y (- x 0)]) (== y x))
                      (- x (let ([z a]) z))
                      y)))
   '(if (== (- (+ a b) 0) (+ a b))
        (- (+ a b) a)
        y))

  (check-equal?
   (canonicalize '(let ([x (+ a b)])
                    (if (<= 1 a x b)
                        (and (!= x a b) (== a 2 b) (<= a 4))
                        (> x a b 3))))
   '(let ([x (+ a b)])
      (if (and (and (<= 1 a) (<= a x)) (<= x b))
          (and (and (and (and (!= x a) (!= x b)) (!= a b)) (and (== a 2) (== 2 b))) (<= a 4))
          (and (and (> x a) (> a b)) (> b 3)))))

  (check-equal?
   (canonicalize '(not (and TRUE FALSE p)))
   '(or (or FALSE TRUE) (not p)))

  (check-equal?
   (canonicalize '(let ([p (<= x 2 y)])
                    (if (not p) 1 2)))
   '(let ([p (and (<= x 2) (<= 2 y))])
      (if (not p) 1 2)))

  (check-equal?
   (canonicalize '(not (let ([p (not (<= a b))])
                         (or p (!= a (+ b 1))))))
   '(let ([p (> a b)])
      (and (not p) (== a (+ b 1)))))

)