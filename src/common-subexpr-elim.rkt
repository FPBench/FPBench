#lang racket

;; TODO: Once the let unroller is implemented we can eliminate more subexpressions.
;; Right now we don't look inside any let or while statements for eliminating, but once
;; the unroller is implemented, we can just expand everything for MAXIMUM elimination.
(require racket/hash)
(require "common.rkt" "fpcore-checker.rkt" "fpcore-reader.rkt" "fpcore-visitor.rkt")

(provide
  (contract-out
   [core-common-subexpr-elim (-> fpcore? fpcore?)]
   [fuse-let (-> expr? expr?)]  ; ml-canonicalizer.rkt
   [fpcore-fuse-let (-> fpcore? fpcore?)]
   [*expr-cse-able?* (parameter/c (-> expr? boolean?))]))

(module+ test (require rackunit))

(define *names* (make-parameter (mutable-set)))
(define *expr-cse-able?* (make-parameter list?))

;;;;;; name generation

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (for/list ([_ (cons name prefixed)] [i (in-naturals)])
      (string->symbol (format "~a_~a" name i))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (add-name! name)
  (set-add! (*names*) name))

(define (clear-names!)
  (set-clear! (*names*)))

;; fuse let bindings together in 3 passes
;;  (1) expand single-binding let exprs to let* exprs
;;  (2) fuse let* exprs together
;;  (3) eliminate useless let bindings [<var> <var2>]
(define (fuse-let expr)
  ; pass 1
  (define/transform-expr (expand-single-let-exprs expr)
    [(visit-let vtor vars vals body #:ctx [ctx '()])
      (match vars
       [(list) (visit/ctx vtor body ctx)]
       [(list a) `(let* ((,a ,(visit/ctx vtor (car vals) ctx)))
                    ,(visit/ctx vtor body ctx))]
       [_ `(let (,@(for/list ([var vars] [val vals]) (list var (visit/ctx vtor val ctx))))
            ,(visit/ctx vtor body ctx))])])
  ; pass 2
  (define/transform-expr (fuse-let*-exprs expr)
    [(visit-let* vtor vars vals body #:ctx [ctx '()])
      (match body
       [`(let* ([,vars2 ,vals2] ...) ,body2)
        (define vars* (append vars vars2))
        (define vals* (append vals vals2))
        (visit/ctx vtor
                  `(let* (,@(for/list ([var vars*] [val vals*]) (list var (visit/ctx vtor val ctx))))
                    ,(visit/ctx vtor body2 ctx))
                   ctx)]
       [_ `(let* (,@(for/list ([var vars] [val vals]) (list var (visit/ctx vtor val ctx))))
            ,(visit/ctx vtor body ctx))])])
  ; pass 3
  (define/transform-expr (elim-useless-binding expr ctx)
    [(visit-let_ vtor let_ vars vals body #:ctx [ctx '()])
      (define-values (vars* vals* ctx*)
        (for/fold ([vars* '()] [vals* '()] [ctx* ctx]
                  #:result (values (reverse vars*) (reverse vals*) ctx*))
                  ([var vars] [val vals])
          (if (variable? val)   ; useless
              (values vars* vals* (dict-set ctx* var val))
              (values (cons var vars*) (cons val vals*) ctx*))))
      (if (null? vars*)
          (visit/ctx vtor body ctx*)
         `(,let_ (,@(for/list ([var vars*] [val vals*])
                      (list var (visit/ctx vtor val ctx*))))
                ,(visit/ctx vtor body ctx*)))]
    [(visit-symbol vtor x #:ctx [ctx '()])
      (dict-ref ctx x x)])
  ; apply passes
  (elim-useless-binding (fuse-let*-exprs (expand-single-let-exprs expr)) '()))


; kahn's algorithm
(define (topo-sort deps)
  (define (filter-no-edges deps)
    (map car (filter (compose null? cdr) deps)))
  (let loop ([sorted '()] [deps deps] [no-edges (filter-no-edges deps)])
    (cond
     [(null? no-edges) (append (reverse sorted) (map car deps))]
     [else 
      (define chosen (car no-edges))
      (define deps*
        (for/list ([v (in-list deps)] #:unless (= (car v) chosen))
          (cons (car v) (filter-not (curry = chosen) (cdr v)))))
      (define edges* (filter-no-edges deps*))
      (loop (cons chosen sorted) deps* edges*)])))

(define (dict-set** dict keys vals)
  (for/fold ([dict dict]) ([k keys] [v vals])
    (dict-set dict k v)))

(define (hash-set**! h keys vals)
  (for ([k keys] [v vals])
    (hash-set! h k v)))

; This function is slightly more complicated than it sounds.
; It assigns unique names to variables for the time that they
; store a particular value.
; The caveat ensures, for example, that an expression in the value of
; a while binding is not merged with same expression in the body of the
; same while binding. While the two may refer to the same location,
; there is no guarantee that it contains the same value, so we cannot
; compute it before entering the loop.
(define (unique-vars/cse expr vartable)
  (define/transform-expr (uniquify expr ctx)
    [(visit-let vtor vars vals body #:ctx [ctx '()])
      (define vars* (map gensym vars))
      (define ctx* (dict-set** ctx vars vars*))
      (hash-set**! vartable vars* vars)
      `(let ,(for/list ([var vars*] [val vals]) (list var (visit/ctx vtor val ctx)))
            ,(visit/ctx vtor body ctx*))]
    [(visit-let* vtor vars vals body #:ctx [ctx '()])
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx ctx] [vars* '()] [vals* '()]
                  #:result (values ctx (reverse vars*) (reverse vals*)))
                  ([var vars] [val vals])
          (let ([name (gensym var)])
            (hash-set! vartable name var)
            (values (dict-set ctx var name)
                    (cons name vars*)
                    (cons (visit/ctx vtor val ctx) vals*)))))
      `(let* ,(map list vars* vals*) ,(visit/ctx vtor body ctx*))]
    [(visit-while vtor cond vars inits updates body #:ctx [ctx '()])
      (define vars* (map gensym vars))  ; first for updates
      (define ctx* (dict-set** ctx vars vars*))
      (define vars** (map gensym vars)) ; second for body (hack to avoid over combining)
      (define ctx** (dict-set** ctx vars vars**))
      (hash-set**! vartable (append vars* vars**) (append vars vars))
      `(while ,(visit/ctx vtor cond ctx)
              ,(for/list ([var vars*] [init inits] [update updates])
                (list var (visit/ctx vtor init ctx) (visit/ctx vtor update ctx*)))
              ,(visit/ctx vtor body ctx**))]
    [(visit-while* vtor cond vars inits updates body #:ctx [ctx '()])
      (define-values (ctx* vars* inits* updates*)
        (for/fold ([ctx ctx] [vars* '()] [inits* '()] [updates* '()]
                  #:result (values ctx (reverse vars*) (reverse inits*) (reverse updates*)))
                  ([var vars] [init inits] [update updates])
          (define name (gensym var))
          (define ctx* (dict-set* ctx var name))
          (hash-set! vartable name var)
          (values ctx*
                  (cons name vars*)
                  (cons (visit/ctx vtor init ctx) inits*)
                  (cons (visit/ctx vtor update ctx*) updates*))))
      (define vars** (map gensym vars))
      (define ctx** (dict-set** ctx* vars vars**))
      (hash-set**! vartable vars** vars)
      `(while* ,(visit/ctx vtor cond ctx)
                ,(map list vars* inits* updates*)
                ,(visit/ctx vtor body ctx**))]
    [(visit-for vtor vars vals accums inits updates body #:ctx [ctx '()])
      (define vars* (map gensym vars))      ; first for updates
      (define accums* (map gensym accums))
      (define ctx* (dict-set** ctx (append vars accums) (append vars* accums*)))
      (define vars** (map gensym vars))     ; second for body (hack to avoid over combining)
      (define accums** (map gensym accums))
      (define ctx** (dict-set** ctx (append vars accums) (append vars** accums**)))
      (hash-set**! vartable (append vars* vars** accums* accums**)
                            (append vars vars accums accums))
      `(for ,(for/list ([var vars*] [val vals]) (list var (visit/ctx vtor val ctx)))
            ,(for/list ([accum accums*] [init inits] [update updates])
              (list accum (visit/ctx vtor init ctx) (visit/ctx vtor update ctx*)))
            ,(visit/ctx vtor body ctx**))]
    [(visit-for* vtor vars vals accums inits updates body #:ctx [ctx '()])
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx ctx] [vars* '()] [vals* '()]
                  #:result (values ctx (reverse vars*) (reverse vals*)))
                  ([var vars] [val vals])
          (define name (gensym var))
          (hash-set! vartable name var)
          (values (dict-set* ctx var name)
                  (cons name vars*)
                  (cons (visit/ctx vtor val ctx) vals*))))
      (define-values (ctx** accums* inits* updates*)
        (for/fold ([ctx ctx*] [accums* '()] [inits* '()] [updates* '()]
                    #:result (values ctx (reverse accums*) (reverse inits*) (reverse updates*)))
                  ([accum accums] [init inits] [update updates])
          (define name (gensym accum))
          (define ctx* (dict-set* ctx accum name))
          (hash-set! vartable name accum)
          (values ctx*
                  (cons name accums*)
                  (cons (visit/ctx vtor init ctx) inits*)
                  (cons (visit/ctx vtor update ctx*) updates*))))
      (define vars** (map gensym vars))
      (define accums** (map gensym accums))
      (define ctx*** (dict-set** ctx** (append vars accums) (append vars** accums**)))
      (hash-set**! vartable (append vars** accums**) (append vars accums))
      `(for* ,(map list vars* vals*)
              ,(map list accums* inits* updates*)
              ,(visit/ctx vtor body ctx***))]
    [(visit-tensor vtor vars vals body #:ctx [ctx '()])
      (define vars* (map gensym vars))
      (define ctx* (dict-set** ctx vars vars*))
      (hash-set**! vartable vars* vars)
      `(tensor ,(for/list ([var vars*] [val vals]) (list var (visit/ctx vtor val ctx)))
            ,(visit/ctx vtor body ctx*))]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [ctx '()])
      (define-values (ctx* vars* vals*)
        (for/fold ([ctx ctx] [vars* '()] [vals* '()]
                  #:result (values ctx (reverse vars*) (reverse vals*)))
                  ([var vars] [val vals])
          (define name (gensym var))
          (hash-set! vartable name var)
          (values (dict-set* ctx var name)
                  (cons name vars*)
                  (cons (visit/ctx vtor val ctx) vals*))))
      (define-values (ctx** accums* inits* updates*)
        (for/fold ([ctx ctx*] [accums* '()] [inits* '()] [updates* '()]
                    #:result (values ctx (reverse accums*) (reverse inits*) (reverse updates*)))
                  ([accum accums] [init inits] [update updates])
          (define name (gensym accum))
          (define ctx* (dict-set* ctx accum name))
          (hash-set! vartable name accum)
          (values ctx*
                  (cons name accums*)
                  (cons (visit/ctx vtor init ctx) inits*)
                  (cons (visit/ctx vtor update ctx*) updates*))))
      (define vars** (map gensym vars))
      (define accums** (map gensym accums))
      (define ctx*** (dict-set** ctx** (append vars accums) (append vars** accums**)))
      (hash-set**! vartable (append vars** accums**) (append vars accums))
      `(tensor* ,(map list vars* vals*)
                ,(map list accums* inits* updates*)
                ,(visit/ctx vtor body ctx***))]
    [(visit-symbol vtor x #:ctx [ctx '()])
      (dict-ref ctx x x)])
  (uniquify expr '()))

;; main cse procedure
(define (common-subexpr-elim vars props expr)
  (define exprhash  ; expr -> idx
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons var i))))
  (define exprs ; idx -> munged expr
    (make-hash
      (for/list ([var vars] [i (in-naturals)])
        (cons i (list var)))))
  (define common (mutable-set))
  (define vartable (make-hash))
  (define exprc (length vars))

  ; reset parameters
  (clear-names!)
  (for ([var vars]) (add-name! var))

  ; more convienient shorthand
  (define (get-name name)
    (hash-ref vartable name name))
        
  ; visitor for munge (`rec` is munge)
  (define/visit-expr (munge/visit expr rec)
    [(visit-if vtor cond ift iff #:ctx [rec '()])
      (list 'if (rec cond) (rec ift) (rec iff))]
    [(visit-let_ vtor let_ vars vals body #:ctx [rec '()]) ; special-cased below
      (for ([var vars]) (add-name! var)) ; add variables names
      (list let_ (map get-name vars) (map rec vals) (rec body))]
    [(visit-while_ vtor while_ cond vars inits updates body #:ctx [rec '()])
      (for ([var vars]) (add-name! var)) ; add variables names
      (list while_ (rec cond) (map get-name vars) (map rec inits) (map rec updates) (rec body))]
    [(visit-for_ vtor for_ vars vals accums inits updates body #:ctx [rec '()])
      (for ([var (append vars accums)]) (add-name! var))
      (list for_ (map get-name vars) (map rec vals) (map get-name accums)
                 (map rec inits) (map rec updates) (rec body))]
    [(visit-tensor vtor vars vals body #:ctx [rec '()])
      (for ([var vars]) (add-name! var)) ; add variables names
      (list 'tensor (map get-name vars) (map rec vals) (rec body))]
    [(visit-tensor* vtor vars vals accums inits updates body #:ctx [rec '()])
      (for ([var (append vars accums)]) (add-name! var)) ; add variables names
      (list 'tensor* (map get-name vars) (map rec vals) (map get-name accums)
                     (map rec inits) (map rec updates) (rec body))]
    [(visit-! vtor props* body #:ctx [rec '()])
      (list '! props* (rec body (apply dict-set* props props*)))]
    [(visit-op_ vtor op args #:ctx [rec '()])
      (cons op (map rec args))]
    [(visit-call vtor func args #:ctx [rec '()])
      (cons func (map rec args))]
    [(visit-terminal_ vtor x #:ctx [rec '()])
      (box (hash-ref vartable x x))]) ; put constants into box so its not confused with indices

  ; convert to indices to process
  (define (munge expr)
    (let loop ([expr expr] [props props])
      (define key (cons expr props))
      (cond
       [(hash-has-key? exprhash key) ; already seen
        (define idx (hash-ref exprhash key))
        (when ((*expr-cse-able?*) expr)
          (set-add! common idx))
        idx]
       [else    ; new expresion
        (define old-exprc exprc)
        (hash-set! exprhash key exprc)
        (define rec (λ (e [p props]) (loop e p)))
        (begin0 exprc
          (set! exprc (+ 1 exprc))
          (hash-set! exprs old-exprc (munge/visit expr rec)))])))

  ; fill `exprhash` and `exprs` and store top index
  (define root
    (let ([expr* (unique-vars/cse expr vartable)])
      (clear-names!)
      (for ([var vars]) (add-name! var))
      (munge expr*)))

  ; helper: merges dependency tables
  (define (merge-deps idx hs)
    (define h ; add current index if it is common
      (if (set-member? common idx)
          (hash idx (cons idx (remove-duplicates (append-map hash-keys hs))))
          (hash)))
    (apply hash-union h hs ; combine child tables with this one
           #:combine (λ (x y) (cons idx (remove-duplicates (append (cdr x) (cdr y)))))))

  ; dependent idxs
  ; idx -> (loc, deps ...)
  ; order of let bindings must respect dependencies
  (define common-deps
    (let loop ([idx root])
      (match (hash-ref exprs idx)
       [(list (or 'let 'let* 'tensor) vars vals body) ; don't propogate
        (merge-deps idx (map loop (cons body vals)))]
       [(list (or 'while 'while*) cond vars inits updates body) ; don't propogate
        (merge-deps idx (map loop (append (list cond body) inits updates)))]
       [(list (or 'for 'for* 'tensor*) vars vals accums inits updates body) ; don't propogate
        (merge-deps idx (map loop (append vals inits updates (list body))))]
       [(list '! props body) ; don't propogate
        (merge-deps idx (list (loop body)))]
       [(list op args ...)
        (define deps (merge-deps idx (map loop args)))
        (define prop-up  ; pull up all common idxs bound one level below
          (for/hash ([(k v) (in-hash deps)] #:when (set-member? args (car v)))
            (values k (cons idx (cdr v)))))
        (hash-union deps prop-up #:combine (λ (x y) y))]
       [(box x) (hash)]))) ; ignore constants
        

  ; let locations
  (define common-locs (mutable-set))
  (for ([vals (hash-values common-deps)])
    (set-add! common-locs (car vals)))

  ; create let bindings: ([var val] ...)
  (define (gen-let-bindings loc)
    (define deps
      (for/list ([(k v) (in-hash common-deps)]
                #:when (= (car v) loc))
        (cons k (cdr v))))
    (set-remove! common-locs loc)
    (for/list ([idx (topo-sort deps)]) ; break ties with topo sort (dependency graph)
      (define name (gensym 't))
      (begin0 (list name (reconstruct idx))
        (hash-set! exprs idx (box name)))))

  ; reconstruct expression
  (define (reconstruct idx)
    (let loop ([idx idx])
      (match* ((hash-ref exprs idx) idx)
       [(_ (? (curry set-member? common-locs))) ; location for let bindings
        (list 'let* (gen-let-bindings idx) (loop idx))]
       [((list (and (or 'let 'let*) let_) vars vals body) _)
        `(,let_ ,(for/list ([var vars] [val vals]) (list var (loop val))) ,(loop body))]
       [((list (and (or 'while 'while*) while_) cond vars inits updates body) _)
        `(,while_ ,(loop cond)
                  ,(for/list ([var vars] [init inits] [update updates])
                    (list var (loop init) (loop update)))
                  ,(loop body))]
       [((list (and (or 'for 'for*) for_) vars vals accums inits updates body) _)
        `(,for_ ,(for/list ([var vars] [val vals]) (list var (loop val)))
                ,(for/list ([accum accums] [init inits] [update updates])
                  (list accum (loop init) (loop update)))
                ,(loop body))]
       [((list 'tensor vars vals body) _)
        `(tensor ,(for/list ([var vars] [val vals]) (list var (loop val))) ,(loop body))]
       [((list 'tensor* vars vals accums inits updates body) _)
        `(tensor* ,(for/list ([var vars] [val vals]) (list var (loop val)))
                  ,(for/list ([accum accums] [init inits] [update updates])
                    (list accum (loop init) (loop update)))
                  ,(loop body))]
       [((list '! props body) _)
        `(! ,@props ,(loop body))]
       [((list op args ...) _)
        (cons op (map loop args))]
       [((box term) _) term])))

  ; reconstruct expression top-down
  (fuse-let (reconstruct root)))

;;;;;;; top-level

; common subexpression eliminator
(define (core-common-subexpr-elim core)
  (*names* (mutable-set))
  (define-values (name args props body)
    (match core
     [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (define default-props (apply dict-set* '() '(:precision binary64 round nearestEven)))
  (define props* (apply dict-set* default-props props))
  (define cse-body (common-subexpr-elim args props* body))
  (if name
    `(FPCore ,name ,args ,@props ,cse-body)
    `(FPCore ,args ,@props ,cse-body)))

; Fuse let expressions together
(define (fpcore-fuse-let prog)
  (match prog
   [(list 'FPCore (list args ...) props ... body)
    `(FPCore ,args ,@props ,(fuse-let body))]   
   [(list 'FPCore name (list args ...) props ... body)
    `(FPCore ,name ,args ,@props ,(fuse-let body))]))

;;;; command line
(module+ main
  (require racket/cmdline)
  (command-line
   #:program "common-subexpr-elim.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (core-common-subexpr-elim expr)))))

(module+ test
  (define-syntax-rule (check-cse in out)
    (check-equal? (core-common-subexpr-elim in) out))
  (define-syntax-rule (check-fuse-let in out)
    (check-equal? (fuse-let in) out))

  ; cse

  (check-cse '(FPCore (a) a)
             '(FPCore (a) a))

  (check-cse '(FPCore (a) (+ (+ a a) (+ a a)))
             '(FPCore (a) (let* ((t_0 (+ a a))) (+ t_0 t_0))))

  (check-cse '(FPCore (a x) (+ (- (+ a x) a)
                               (- (+ a x) a)))
             '(FPCore (a x) (let* ((t_0 (- (+ a x) a))) (+ t_0 t_0))))
  
  (check-cse '(FPCore (a) (let ((j0 (+ a a))) j0))
             '(FPCore (a) (let* ((j0 (+ a a))) j0)))

  (check-cse '(FPCore (a) (let ((j0 (+ a a))) (+ (+ a a) j0)))
             '(FPCore (a) (let* ((t_0 (+ a a))) (+ t_0 t_0))))

  (check-cse '(FPCore (a) (let ((i0 (- a a))) (- (+ (+ a a) (+ a a)) i0)))
             '(FPCore (a) (let* ((i0 (- a a)) (t_0 (+ a a))) (- (+ t_0 t_0) i0))))

  (check-cse '(FPCore (a) (let ((x (- a a))) (let ((x (+ a a))) x)))
             '(FPCore (a) (let* ((x (- a a)) (x (+ a a))) x)))

  (check-cse '(FPCore (a) (let ((x (+ a 1)) (y (- a 1))) (+ (* a x) y)))
             '(FPCore (a) (let ((x (+ a 1)) (y (- a 1))) (+ (* a x) y))))

  (check-cse '(FPCore (a) (while (< i 100) ((i (+ a a) (+ i 1))) (* i (+ a a))))
             '(FPCore (a) (let* ((t_0 (+ a a))) (while (< i 100) ((i t_0 (+ i 1))) (* i t_0)))))

  (check-cse '(FPCore (a) (while* (< i 100) ((i (+ a a) (+ i 1))) (* i (+ a a))))
             '(FPCore (a) (let* ((t_0 (+ a a))) (while* (< i 100) ((i t_0 (+ i 1))) (* i t_0)))))

  (check-cse '(FPCore (a n) (for ((i n)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a n) (let* ((t_0 (+ a a))) (for ((i n)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-cse '(FPCore (a n) (for* ((i n)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a n) (let* ((t_0 (+ a a))) (for* ((i n)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-cse '(FPCore (a n) (tensor ((i n)) (* (+ n a) (+ n a))))
             '(FPCore (a n) (tensor ((i n)) (let* ((t_0 (+ n a))) (* t_0 t_0)))))

  (check-cse '(FPCore (a) (tensor* ((i 3)) ((m (+ a a) (+ m i))) (* m (+ a a))))
             '(FPCore (a) (let* ((t_0 (+ a a))) (tensor* ((i 3)) ((m t_0 (+ m i))) (* m t_0)))))

  (check-cse '(FPCore () (+ (foo) (foo)))
             '(FPCore () (let* ((t_0 (foo))) (+ t_0 t_0))))

  ; cse (no combine)

  (check-cse '(FPCore (a) (* (+ a a) (let ((a (* a 2))) (+ a a))))
             '(FPCore (a) (* (+ a a) (let* ((a (* a 2))) (+ a a)))))
  
  (check-cse '(FPCore (a) (* (+ a a) (let* ((a (* a 2))) (+ a a))))
             '(FPCore (a) (* (+ a a) (let* ((a (* a 2))) (+ a a)))))

  (check-cse '(FPCore (a) (* (+ a a) (while (< a 100) ((a 0 (+ a a))) (+ a a))))
             '(FPCore (a) (* (+ a a) (while (< a 100) ((a 0 (+ a a))) (+ a a)))))
  
  (check-cse '(FPCore (a) (* (+ a a) (while* (< i a) ((a 0 (+ a a))) (+ a a))))
             '(FPCore (a) (* (+ a a) (while* (< i a) ((a 0 (+ a a))) (+ a a)))))

  (check-cse '(FPCore (a n) (* (+ a a) (for ([i n]) ((a i (+ a a))) (+ a a))))
             '(FPCore (a n) (* (+ a a) (for ([i n]) ((a i (+ a a))) (+ a a)))))

  (check-cse '(FPCore (a n) (* (+ a a) (for* ([i n]) ((a i (+ a a))) (+ a a))))
             '(FPCore (a n) (* (+ a a) (for* ([i n]) ((a i (+ a a))) (+ a a)))))

  (check-cse '(FPCore (a n) (* (+ a a) (tensor ([a n]) (+ a a))))
             '(FPCore (a n) (* (+ a a) (tensor ([a n]) (+ a a)))))

  (check-cse '(FPCore (a n) (* (+ a a) (tensor* ([i n]) ((a i (+ a a))) (+ a a))))
             '(FPCore (a n) (* (+ a a) (tensor* ([i n]) ((a i (+ a a))) (+ a a)))))

  (check-cse '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a))))
             '(FPCore (a) (+ (* a a) (! :precision binary32 (* a a)))))
             
  (check-cse '(FPCore (a) (+ (* a a) (! :round nearestEven (* a a))))
             '(FPCore (a) (+ (* a a) (! :round nearestEven (* a a)))))

  ; fuse-let

  (check-fuse-let '(let ([x t]) x)
                  't)

  (check-fuse-let '(let ([a 1]) (let* ([a 2] [b 3]) (+ a b)))
                  '(let* ([a 1] [a 2] [b 3]) (+ a b)))

  (check-fuse-let '(let ([a 1]) (let ([b 2]) (let ([c 3]) (+ (* a b) c))))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let ([c 3]) (+ (* a b) c)))
                  '(let* ([a 1] [b 2] [c 3]) (+ (* a b) c)))

  (check-fuse-let '(let* ([a 1] [b 2]) (let* ([c 3] [d 4]) (+ (* a b) (* c d))))
                  '(let* ([a 1] [b 2] [c 3] [d 4]) (+ (* a b) (* c d))))
)
