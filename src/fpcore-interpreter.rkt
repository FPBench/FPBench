#lang racket

(require "common.rkt" "tensor.rkt" "fpcore-checker.rkt" "evaluator.rkt")
(require math/bigfloat math/special-functions math/base generic-flonum)

(provide
  (contract-out
   [eval-expr (-> evaluator? (-> expr? context/c any/c))]
   [eval-expr* (-> evaluator?
                   (-> expr? context/c any/c)
                   (-> expr? context/c any/c))]
   [racket-run-fpcore (-> fpcore?
                          (listof string?)
                          (or/c real? tensor? boolean?))]))

(define/contract context/c contract? (dictof symbol? any/c))

(define ((eval-expr* evaltor rec) expr ctx)
  (match expr
    [(? number?) ((evaluator-real evaltor) expr)]
    [(? gfl?) expr]
    [(? hex?) ((evaluator-real evaltor) (hex->racket expr))]
    [`(digits ,m ,e ,b) ((evaluator-real evaltor) (digits->number m e b))]
    [(? constant?) ((evaluator-constant evaltor) expr)]
    [(? tensor?) expr]
    [(? symbol?) (dict-ref ctx expr)]
    [`(if ,test ,ift ,iff)
     (if (rec test ctx) (rec ift ctx) (rec iff ctx))]
    [`(let ([,vars ,vals] ...) ,body)
     (define vals* (for/list ([val vals]) (rec val ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (rec body ctx*)]
    [`(let* () ,body)
     (rec body ctx)]
    [`(let* ([,var ,val] ,rest ...) ,body)
     (rec `(let* ,rest ,body) (dict-set ctx var (rec val ctx)))]
    [`(while ,test ([,vars ,inits ,updates] ...) ,res)
     (define vals* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
     (if (rec test ctx*)
         (rec
          `(while ,test
             ,(for/list ([var vars] [init inits] [update updates])
                (list var (rec update ctx*) update))
             ,res)
          ctx)
         (rec res ctx*))]
    [`(while* ,test ([,vars ,inits ,updates] ...) ,res)
     (define-values (vals* ctx*)
       (for/fold ([vals '()] [ctx ctx]) ([var vars] [init inits])
         (define val (rec init ctx))
         (values (cons val vals) (dict-set ctx var val))))
     (if (rec test ctx*)
         (rec `(let* ,(for/list ([var vars] [val (reverse vals*)]) (list var val))
                 (while* ,test ,(for/list ([var vars] [init inits] [update updates])
                                  (list var update update)) ,res))
              ctx*)
         (rec res ctx*))]
    [`(for ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (compose repr->real (curryr rec ctx)) vals))
     (define ranges (map (compose stream->list in-range) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*)))
     (for/fold ([cx ctx*] #:result (rec body cx)) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*]) ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx*)))))]
    [`(for* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (compose repr->real (curryr rec ctx)) vals))
     (define ranges (map (compose stream->list in-range) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*)))
     (for/fold ([cx ctx*] #:result (rec body cx)) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*]) ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx**)))))]
    [`(tensor ([,vars ,vals] ...) ,body)
     (define sizes (map (compose repr->integer (curryr rec ctx)) vals))
     (define ranges (map (λ (x) (build-list x identity)) sizes))
     (define coords (apply cartesian-product ranges))
     (define vals* 
      (for/list ([coord coords])
        (let ([ctx* (apply dict-set* ctx (append-map list vars coord))])
          (rec body ctx*))))
     (tabulate->tensor (map repr->integer sizes) vals*)]
    [`(tensor* ([,vars ,vals] ...) ([,accums ,inits ,updates] ...) ,body)
     (define sizes (map (compose repr->integer (curryr rec ctx)) vals))
     (define ranges (map (λ (x) (build-list x identity)) sizes))
     (define coords (apply cartesian-product ranges))
     (define inits* (for/list ([init inits]) (rec init ctx)))
     (define ctx* (apply dict-set* ctx (append-map list accums inits*))) ; should these be added before or after?
     (define vals* 
      (for/fold ([cx ctx*] [vals '()] #:result vals) ([coord coords])
       (let ([cx* (apply dict-set* cx (append-map list vars coord))])
         (for/fold ([cx** cx*] #:result (values cx** (append vals (list (rec body cx**))))) 
                   ([accum accums] [update updates])
           (dict-set cx** accum (rec update cx**))))))
     (tabulate->tensor (map repr->integer sizes) vals*)]
    [`(! ,props* ... ,body)
     (define-values (_ props) (parse-properties props*))
     (cond
      [(or (dict-has-key? props ':precision) (dict-has-key? props ':round))
       (define-values (prec rnd) (get-evaluator-params evaltor))
       (define nprec (expand-prec (dict-ref props ':precision prec)))
       (define nrnd (dict-ref props ':round rnd))
       (define evaltor* (get-evaluator nprec nrnd))
       (set-evaluator-params! evaltor*)
       (define ret ((eval-expr evaltor*) body ctx))
       (set-evaluator-params! evaltor)
       ret]
      [else
       ((eval-expr* evaltor rec) body ctx)])]
    [`(cast ,expr) ((evaluator-real evaltor) (repr->real ((eval-expr* evaltor rec) expr ctx)))]
    [`(array ,vals ...) (for/list ([i vals]) ((eval-expr* evaltor rec) i ctx))]
    [`(dim ,val) (tensor-dim (rec val ctx))]
    [`(size ,val ,dim) (tensor-size (rec val ctx) (repr->integer dim))]
    [`(ref ,val ,elems ...) (apply (curry tensor-ref (rec val ctx)) (map (compose repr->integer (curryr rec ctx)) elems))]
    [(list (? (curry dict-has-key? (*fpcores*)) ident) args ...)
      (define args* (map (compose repr->real (curryr rec ctx)) args))
      (define core* (first (dict-ref (*fpcores*) ident)))
      (racket-run-fpcore core* (map ~a args*))]
    [(list (? operator? op) args ...)
      (apply ((evaluator-function evaltor) op) (map (curryr rec ctx) args))]))

(define ((eval-expr evaltor) expr ctx)
  (let eval ([expr expr] [ctx ctx])
    ((eval-expr* evaltor eval) expr ctx)))

; Main interpreter

(define (tensor-layer->size arr size ctx)
  (match size
   [(? symbol?) 
    (let ([size* (dict-ref ctx size #f)])
      (cond
       [(equal? size* #f) (list (cons size (exact->inexact (length arr))))]
       [else (unless (= (length arr) size*)
              (error 'tensor-layer->size
                     (format "Dimension of tensor argument has incorrect size. Expected: ~a=~a, Actual: ~a"
                     size (repr->integer size*) (length arr))))
             '()]))]      
   [(? number?)
    (unless (= (length arr) size)
      (error 'tensor-layer->size
             (format "Dimension of tensor argument has incorrect size. Expected: ~a, Actual: ~a"
             (repr->integer size) (length arr))))
    '()]
   [_  (error 'tensor-layer->size (format "Size of array must be a variable or number. Given ~a") size)]))

(define (arg->tensor name sizes arg evaltor ctx)
  (define p (open-input-string arg))
  (define syn (read-syntax 'str p))
  (when (eof-object? syn)
    (error 'arg->tensor "Couldn't read tensor. Check input expression."))
  (define ten (syntax-e-rec syn))
  (define ten* ((eval-expr evaltor) ten (hash)))
  (unless (tensor? ten*)
    (error 'arg->tensor "Expected a tensor"))
  (unless (= (tensor-dim ten*) (length sizes))
    (error 'arg->tensor "Tensor argument has incorrect dimension. Expected: ~a. Actual: ~a"
                        (length sizes) (tensor-dim ten*)))
  (append
    (let loop ([ten** ten*] [sizes* sizes])
     (cond 
      [(= (length sizes*) 1) (tensor-layer->size ten** (first sizes*) ctx)]
      [else
       (append
        (tensor-layer->size ten** (first sizes*) ctx)
        (let ([ctx* (loop (first ten**) (drop sizes* 1))])
          (unless (for/and ([i (drop ten** 1)]) (equal? ctx* (loop i (drop sizes* 1))))
            (error 'arg->tensor "Ragged tensors not supported"))
          ctx*))]))
    (list (cons name ten*))))

(define (arg->expr arg evaltor)
  (define syn (read-syntax 'str (open-input-string arg)))
  (when (eof-object? syn)
    (error 'arg->expr "Couldn't read expression"))
  (define expr (syntax-e-rec syn))
  ((eval-expr evaltor) expr (hash)))

(define (result->inexact x prec)
  (match* (x prec)
   [((? list?) _) (map (curryr result->inexact prec) x)]
   [((? boolean?) _) x]
   [(_ '(float 11 64)) (real->double-flonum x)]
   [(_ '(float 8 32)) (real->double-flonum x)]
   [(_ _) x]))

(define (racket-run-fpcore* name vars props* body args)
  (define-values (_ props) (parse-properties props*))
  (define base-precision (expand-prec (dict-ref props ':precision 'binary64)))
  (define base-rounding (dict-ref props ':round 'nearestEven))
  (define evaltor (get-evaluator base-precision base-rounding))
  (define ctx
    (for/fold ([ctx '()]) ([var vars] [arg args])
     (append ctx
      (match var
        [`(! ,var-props* ... ,(? symbol? var*))
         (define-values (_ var-props) (parse-properties var-props*))
         (define var-precision (expand-prec (dict-ref var-props ':precision base-precision)))
         (define var-rounding (dict-ref var-props ':round base-rounding))
         (define var-evaltor (get-evaluator var-precision var-rounding))
         (set-evaluator-params! var-evaltor)
         (list (cons var* (arg->expr arg var-evaltor)))]
        [`(,(? symbol? name) ,(or (? number? sizes) (? symbol? sizes)) ...)
         (set-evaluator-params! evaltor)
         (arg->tensor name sizes arg evaltor ctx)]
        [(? symbol?)
         (set-evaluator-params! evaltor)
         (list (cons var (arg->expr arg evaltor)))]))))

  (when name (check-argument name ctx))
  (when (dict-has-key? props ':pre)
    (set-evaluator-params! evaltor)
    (define pre (dict-ref props ':pre))
    (unless ((eval-expr evaltor) pre ctx)
      (error 'racket-run-fpcore* "Precondtition not met: ~a" pre)))

  (set-evaluator-params! evaltor)
  (result->inexact (repr->real ((eval-expr evaltor) body ctx)) base-precision))
  
(define (racket-run-fpcore prog args)
  (match prog
   [`(FPCore ,name (,vars ...) ,properties ... ,body)
    (racket-run-fpcore* name vars properties body args)]
   [`(FPCore (,vars ...) ,properties ... ,body)
    (racket-run-fpcore* #f vars properties body args)]))
