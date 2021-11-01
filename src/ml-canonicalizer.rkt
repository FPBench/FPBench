#lang racket

(require racket/hash)
(require "common-subexpr-elim.rkt" "fpcore-visitor.rkt")

(provide canonicalize-ml)

(define gensym-name "t")
(define gensym-names (make-parameter (mutable-set)))
(define gensym-collisions (make-parameter 1))

(define (sym-append . parts)
  (string->symbol (apply string-append (map ~a parts))))

(define (gensym ctx)
  (define name
    (for/fold ([gen (sym-append gensym-name)])
              ([i (in-naturals (gensym-collisions))]                      
              #:break (and (not (set-member? (gensym-names) gen))
                           (not (set-member? ctx gen))))
      (gensym-collisions (+ i 1))
      (sym-append gensym-name i)))
  (begin0 name (set-add! (gensym-names) name)))


(define (canonicalize/expr expr ctx)
  (define/transform-expr (->canon/expr expr ctx)
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
      (define name (gensym ctx))
      (define expr* `(if ,cond ,ift ,iff))
      (values name (hash name expr*))]
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
      (define name (gensym ctx))
      (define expr* `(,let_ ,(map list vars vals) ,body))
      (values name (hash name expr*))]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
      (define name (gensym ctx))
      (define expr* `(,while_ ,cond ,(map list vars inits updates) ,body))
      (values name (hash name expr*))]
    [(visit-! vtor props body #:ctx [ctx '()])
      (define name (gensym ctx))
      (values name (hash name `(! ,@props ,body)))]
    [(visit-op_ vtor op args #:ctx [ctx '()])
      (define-values (binds args*)
        (for/fold ([binds (hash)] [args* '()] #:result (values binds (reverse args*)))
                  ([arg (in-list args)])
          (define-values (arg* bind*) (visit/ctx vtor arg ctx))
          (values (hash-union binds bind*) (cons arg* args*))))
      (values (cons op args*) binds)]
    [(visit-terminal_ vtor x #:ctx [ctx '()])
      (values x (hash))])
  (define-values (expr* bindings) (->canon/expr expr ctx))
  (for/fold ([below expr*] #:result (fuse-let below))
            ([(v b) (in-hash bindings)])
    `(let ((,v ,(canonicalize/clause b ctx))) ,below)))

(define (canonicalize/clause expr ctx)
  (define/transform-expr (->canon/clause expr ctx)
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
      (define cond* (visit/ctx vtor cond ctx))
      (define ift* (visit/ctx vtor ift ctx))
      (define iff* (visit/ctx vtor iff ctx))
      `(if ,cond* ,ift* ,iff*)]
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
      (define-values (ctx* vals*)
        (for/fold ([ctx* ctx] [vals* '()] #:result (values ctx* (reverse vals*)))
                  ([var (in-list vars)] [val (in-list vals)])
          (define val-ctx (match let_ ['let ctx] ['let* ctx*]))
          (values (set-add ctx* var) (cons (visit/ctx vtor val val-ctx) vals*))))
      `(,let_ ,(map list vars vals*) ,(visit/ctx vtor body ctx*))]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
      (define cond* (visit/ctx vtor cond ctx))
      (define-values (ctx* inits*)
        (for/fold ([ctx* ctx] [inits* '()] #:result (values ctx* (reverse inits*)))
                  ([var (in-list vars)] [val (in-list inits)])
          (define val-ctx (match while_ ['while ctx] ['while* ctx*]))
          (values (set-add ctx* var) (cons (visit/ctx vtor val val-ctx) inits*))))
      (define updates*
        (for/fold ([ctx** ctx*] [updates* '()] #:result (reverse updates*))
                  ([var (in-list vars)] [val (in-list updates)])
          (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
          (values (set-add ctx** var) (cons (visit/ctx vtor val val-ctx) updates*))))
      `(,while_ ,cond ,(map list vars inits* updates*) ,(visit/ctx vtor body ctx*))]
    [(visit-! vtor props body #:ctx [ctx '()])
      `(! ,@props ,(visit/ctx vtor body ctx))]
    [(visit-op_ vtor op args #:ctx [ctx '()])
      (canonicalize/expr (cons op args) ctx)]
    [(visit-terminal_ vtor x #:ctx [ctx '()])
      (canonicalize/expr x ctx)])
  (->canon/clause expr ctx))

; For the ML compilers, the allowed FPCore format is more restricted.
; ML languages are very particular about the formatting
; of let and if blocks. In particular, Haskell requires
; that expressions be aligned for let and if blocks.
; For complex FPCore's with nested let and if expressions,
; tracking identation becomes difficult.
; This procedure converts a more general FPCore into the format specified
; below by pulling up all nested let and if expressions.
;
;  <prog> := <clause> | <expr>
;  <clause> := (if <clause> <clause> <clause>) |
;              (let ([<symbol> <clause>] ...+) <clause>) |
;              (let* ([<symbol> <clause>] ...+) <clause>) |
;              (while <clause> ([<symbol> <clause> <clause>] ...+) <clause>) |
;              (while* <clause> ([<symbol> <clause> <clause>] ...+) <clause>) |
;              (! <props> ... <clause>) |
;              <expr>
;  <expr> := (<func> <expr> ...+) | <terminal>
;
; Ideally, the ML compilers can be overhauled to support more
; general FPCores, but until then, this fix seems to work fairly well.
;
(define (canonicalize-ml expr names)
  (define/transform-expr (->canon/top expr ctx)
    [(visit-if vtor cond ift iff #:ctx [ctx '()])
      (canonicalize/clause expr ctx)]
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
      (canonicalize/clause expr ctx)]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [ctx '()])
      (canonicalize/clause expr ctx)]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [ctx '()])
      (error 'canonicalize-ml "unimplemented: ~a" for_)]
    [(visit-tensor vtor vars vals body #:ctx [ctx '()])
      (error 'canonicalize-ml "unimplemented: tensor")]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [ctx '()])
      (error 'canonicalize-ml "unimplemented: tensor*")]
    [(visit-! vtor props body #:ctx [ctx '()])
      (canonicalize/clause expr ctx)]
    [(visit-op_ vtor op args #:ctx [ctx '()])
      (canonicalize/expr expr ctx)]
    [(visit-terminal_ vtor x #:ctx [ctx '()])
      (canonicalize/expr expr ctx)])
  (parameterize ([gensym-collisions 1]
                 [gensym-names (mutable-set)])
    (->canon/top expr names)))

(module+ test
  (require rackunit)
  (define (equal-canon? e1 e2)
    (check-equal? (canonicalize-ml e1 '()) e2))

  (equal-canon? '(+ x 1) '(+ x 1))
  (equal-canon? '(+ 1 (let ([x 1]) x)) '(let* ([t (let* ([x 1]) x)]) (+ 1 t)))
  (equal-canon? '(+ (let ([x 1]) x) (let ([y 2]) y)) '(let* ([t (let* ([x 1]) x)] [t1 (let* ([y 2]) y)]) (+ t t1)))

  (equal-canon? '(if (and (let* ([t TRUE]) (not t)) FALSE) 1 0)
                '(if (let* ([t (let* ([t TRUE]) (not t))]) (and t FALSE)) 1 0))
  ;;; (equal-canon? '(! :precision binary32 (let ([x 1]) x))
  ;;;               '(let* ([t (let* ([x 1]) x)]) (! :precision binary32 t)))
  (equal-canon? '(! :precision binary32 (if (< x 0) x y))
                '(! :precision binary32 (if (< x 0) x y)))
)
    
