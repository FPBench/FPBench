#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt" "../src/sampler.rkt")

(define var-odds (make-parameter 0.5))
(define math-const (make-parameter 0.5))
(define unique-vars (make-parameter #f))
(define exhaustive (make-parameter #f))
(define gensym-count (make-parameter 1))

;; Move this somewhere better
(define bool-ops '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

(define (member? v list)
  (for/or ([i list]) (equal? i v)))

(define (clamp x low high)
  (max low (min x high)))

(define (gensyms count [name 'x] [start (gensym-count)])
  (gensym-count (+ (gensym-count) count))
  (for/list ([i (in-range count)])
    (string->symbol (format "~a~a" name (+ i start)))))

; Returns the max number of unbound variables in an expression
(define (max-unbound-in-expr expr)
 (match expr
  [`(,(or 'while 'while*) ,cond ([,vars ,inits ,updates] ...) ,body) 
    (for/sum ([arg (flatten (list cond body inits updates))]) (max-unbound-in-expr arg))]
  [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body) (for/sum ([arg (list* body vals)]) (max-unbound-in-expr arg))]
  [`(if ,cond ,ift ,iff) (for/sum ([arg (list cond ift iff)]) (max-unbound-in-expr arg))]
  [(list op args ...) (for/sum ([arg args]) (max-unbound-in-expr arg))]
  ['term  1]
  [_      0]))

; Returns the min number of unbound variables in an expression
(define (min-unbound-in-expr expr)
 (match expr
  [`(,(or 'while 'while*) ,cond ([,vars ,inits ,updates] ...) ,body) 
   (clamp (for/sum ([arg (flatten (list cond body inits updates))]) (max-unbound-in-expr arg)) 0 1)]
  [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body) 
   (clamp (for/sum ([arg (list* body vals)]) (min-unbound-in-expr arg)) 0 1)]
  [`(if ,cond ,ift ,iff) 
   (clamp (for/sum ([arg (list cond ift iff)]) (min-unbound-in-expr arg)) 0 1)]
  [(list op args ...) (clamp (for/sum ([arg args]) (min-unbound-in-expr arg)) 0 1)]
  ['term 0]
  [_  0]))

(define (random-vars free count)
  (define unused free)
  (define bound 0)
  (for/list ([i (in-range count)] 
            #:unless (or (= count 0) (empty? free)))
    (let ([name (if (> (- count bound) (length unused))
                    (rand-from-list free)
                    (rand-from-list unused))])
      (set! bound (add1 bound))
      (set! unused (remove name unused))
      name)))

(define (distribute-vars vars distr total) ; assigns n 'vars' based on a distribution of m total 'vars' in subexpression
  (define l (shuffle (append vars (build-list (- total (length vars)) (const 'term)))))
  (for/fold ([svars '()] [used 0] #:result (reverse svars))
            ([c distr])
    (let ([v (take (drop l used) c)])
      (values (list* (filter-not (curry equal? 'term) v) svars) (+ used c)))))

(define (assign-vars expr free [let-assign? #f])
  (define varc (length free))
  (let inner ([subexpr expr] [free free])
   (match subexpr
    [`(while ([,vars ,vals] ...) ,body) subexpr]
    [`(let ([,vars ,vals] ...) ,body)
     (if let-assign?
       (begin                                                                             ; first pass
         (set! let-assign? #f)
        `(let (,@(map (λ (x y) (list x (inner y '()))) vars vals)) ,(inner body free)))     
       (let* ([args (list* body vals)]                                                    ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(let (,@(map (λ (x y vs) (list x (inner y vs))) vars vals (drop svars 1)))
              ,(inner body (first svars)))))]
    [`(let* ([,vars ,vals] ...) ,body)
     (if let-assign?
       (begin                                                                             ; first pass
         (set! let-assign? #f)
        `(let* ,(for/list ([i (in-naturals)] [var vars] [val vals])
                    (list var (inner val (random-vars (take free i) (random 0 (add1 (max-unbound-in-expr val)))))))
               ,(inner body (random-vars free (random 0 (add1 (length free)))))))
       (let* ([args (list* body vals)]                                                    ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(let* (,@(map (λ (x y vs) (list x (inner y vs))) vars vals (drop svars 1)))
               ,(inner body (first svars)))))]
    [`(if ,cond ,ift, iff)
     (let* ([args (list cond ift iff)]
            [distr (for/list ([arg args]) (max-unbound-in-expr arg))]
            [total (foldl + 0 distr)])
      `(if ,@(map (curryr inner) args (distribute-vars free distr total))))]    
    [`(,(? operator? op) ,args ...)
     (let* ([distr (for/list ([arg args]) (max-unbound-in-expr arg))]
            [total (foldl + 0 distr)])
      `(,op ,@(map (curryr inner) args (distribute-vars free distr total))))]      
    ['term (if (empty? free) subexpr (first free))]
    [_  subexpr])))

(define (assign-consts expr consts prec)
  (let inner ([subexpr expr])
   (match subexpr
    [`(while ([,vars ,vals] ...) ,body) `(while (,@(map (λ (x y) (list x (inner y))) vars vals)) ,(inner body))]
    [`(while* ([,vars ,vals] ...) ,body) `(while* (,@(map (λ (x y) (list x (inner y))) vars vals)) ,(inner body))]
    [`(let ([,vars ,vals] ...) ,body) `(let (,@(map (λ (x y) (list x (inner y))) vars vals)) ,(inner body))]
    [`(let* ([,vars ,vals] ...) ,body) `(let* (,@(map (λ (x y) (list x (inner y))) vars vals)) ,(inner body))]
    [`(if ,cond ,ift, iff) `(if ,(inner cond) ,(inner ift) ,(inner iff))]
    [`(,(? operator? op) ,args ...) `(,op ,@(map inner args))] 
    ['term (if (exhaustive)
               (rand-from-list '(1.0 0.0 -1.0))
               (if (> (random) (math-const)) (rand-from-list consts) (sample-random prec)))]
    [_  subexpr])))

; Returns a list of integers of size count such that all values are less than depth
; and at least one is one less than depth.
(define (next-child-depth depth count)
  (if (= depth 0) '()
    (let ([li (for/list ([i (in-range count)]) (random 0 depth))])
      (if (for/or ([v li]) (= v (sub1 depth)))
          li
          (next-child-depth depth count)))))

;;; Set generator

(define (gen-set-cond ops gen-proc)
  (for/fold ([exprs '()]) ([cond ops])
   (match cond
    [(or '< '> '<= '>= '== '!=)
     (append exprs
      (for*/list ([subexpr1 (gen-proc)]
                  [subexpr2 (gen-proc)])
        `(,cond ,subexpr1 ,subexpr2)))]
    [(or 'isfinite 'isinf 'isnan 'isnormal 'signbit)
     (append exprs
      (for*/list ([subexpr (gen-proc)])
        `(,cond ,subexpr)))])))

(define (gen-set-layer ops depth)
  (cond
    [(> depth 0)
     (for/fold ([exprs '()]) ([op (remove* bool-ops ops)])
      (match op
        [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
              'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
              'ceil 'floor 'trunc 'round 'nearbyint)
         (append exprs
           (for/list ([subexpr (gen-set-layer ops (sub1 depth))])
            `(,op ,subexpr)))]
        [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
         (append exprs
           (for*/list ([subexpr1 (gen-set-layer ops (sub1 depth))]
                       [subexpr2 (gen-set-layer ops (sub1 depth))])
            `(,op ,subexpr1 ,subexpr2)))]
        ['fma
         (append exprs
           (for*/list ([subexpr1 (gen-set-layer ops (sub1 depth))]
                       [subexpr2 (gen-set-layer ops (sub1 depth))]
                       [subexpr3 (gen-set-layer ops (sub1 depth))])
            `(,op ,subexpr1 ,subexpr2 ,subexpr3)))]
        ['if
          (append exprs
            (for*/list ([cond (gen-set-cond (filter (curryr member? bool-ops) ops)
                                            (thunk (gen-set-layer ops (sub1 depth))))]
                        [subexpr1 (gen-set-layer ops (sub1 depth))]
                        [subexpr2 (gen-set-layer ops (sub1 depth))])
            `(if ,cond ,subexpr1 ,subexpr2)))]
      ))]
    [(equal? (var-odds) 1.0) (list 'var)]
    [else (list 'const 'var)]))

(define (gen-expr-set ops precs rnd-modes consts depth)
  (for* ([expr (gen-set-layer ops depth)] [prec precs] [rnd rnd-modes])
   (pretty-display
    (let* ([max-vars (max-unbound-in-expr expr)]
           [min-vars (min-unbound-in-expr expr)]
           [free-count (if (unique-vars) max-vars (random min-vars (add1 max-vars)))]
           [args (gensyms free-count)])
      `(FPCore ,args :precision ,prec :round ,rnd ,(assign-vars expr args)))
    (current-output-port))
    (newline)))

;; Random generator

(define (gen-body-min-vars gen-proc [min 1])
  (let ([body (gen-proc)])
    (if (< (min-unbound-in-expr body) min)
        (gen-body-min-vars gen-proc)
        body)))

(define (gen-rand-cond ops gen-proc)
  (let ([cond (rand-from-list ops)])
   (match cond
    [(or '< '> '<= '>= '== '!=)
      `(,cond ,(gen-proc) ,(gen-proc))]
    [(or 'isfinite 'isinf 'isnan 'isnormal 'signbit)
      `(,cond ,(gen-proc))])))

(define (gen-rand-layer ops depth)
  (cond
    [(> depth 0)
      (let ([op (rand-from-list (remove* bool-ops ops))])
       (match op
        [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
              'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
              'ceil 'floor 'trunc 'round 'nearbyint)
          `(,op ,(gen-rand-layer ops (sub1 depth)))]
        [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
          (let ([depths (next-child-depth depth 2)])
            `(,op ,(gen-rand-layer ops (first depths)) 
                  ,(gen-rand-layer ops (second depths))))]
        ['fma
          (let ([depths (next-child-depth depth 3)])
            `(,op ,(gen-rand-layer ops (first depths))
                  ,(gen-rand-layer ops (second depths))
                  ,(gen-rand-layer ops (third depths))))]
        ['if
          (let ([depths (next-child-depth depth 2)]
                [cond (gen-rand-cond (filter (curryr member? bool-ops) ops)
                                     (thunk (gen-rand-layer ops (random 0 depth))))])
          `(if ,cond ,(gen-rand-layer ops (first depths)) ,(gen-rand-layer ops (first depths))))]
        [(or 'let 'let*)
          (let* ([body (gen-rand-layer ops (sub1 depth))]
                 [free-count (add1 (random 0 (max-unbound-in-expr body)))] ; number terminals to be assigned as variables
                 [vars (gensyms (if (zero? free-count) free-count (add1 (random 0 free-count))))])  ; number of unique variables
            (assign-vars `(,op ,(for/list ([var vars]) (list var (gen-rand-layer ops (random 0 depth)))) ,body) (random-vars vars free-count) #t))]
        [(or 'while 'while*)
          (let* ([body (gen-rand-layer ops (sub1 depth))]
                 [free-count (add1 (random 0 (max-unbound-in-expr body)))] ; number terminals to be assigned as variables
                 [vars (gensyms (if (zero? free-count) free-count (add1 (random 0 free-count))))]  ; number of unique variables
                 [cond (gen-rand-cond (filter (curryr member? bool-ops) ops) (thunk (gen-rand-layer ops 0)))])
            (assign-vars
              `(,op ,cond ,(for/list ([var vars]) (list var (gen-rand-layer ops 1) (gen-rand-layer ops (random 0 depth)))) ,body)
              (random-vars vars free-count) #t))]
      ))]
    [else 'term]))

(define (gen-random-expr ops precs rnd-modes consts depth number)
  (for ([i (in-range number)])
   (parameterize ([gensym-count 1])
    (pretty-display
     (let* ([expr (gen-rand-layer ops depth)]
            [prec (rand-from-list precs)]
            [rnd (rand-from-list rnd-modes)]
            [max-vars (max-unbound-in-expr expr)]
            [min-vars (min-unbound-in-expr expr)]
            [free-count (if (unique-vars) max-vars (random min-vars (add1 max-vars)))]    ; number terminals to be assigned as variables
            [args (gensyms (if (or (unique-vars) (zero? free-count)) free-count (add1 (random 0 free-count))) 'arg 1)] ; number of unique variables
            [props `(,(format ":prec ~a" prec) ,(format ":round ~a" rnd))]
            [name-props (if (> depth 3) (list* (format ":name \"Random ~a\"" (add1 i)) props) props)])
        `(,(format "FPCore ~a" args) ,@name-props ,(assign-consts (assign-vars expr (random-vars args free-count)) consts prec)))
     (current-output-port))
    (newline))))

;;; Command line

(module+ main
 (define depth 1)
 (define number 1)
 (command-line
  #:program "gen-random.rkt"
  #:once-each
  [("-o" "--output") _file "Name of output file"
    (current-output-port (open-output-file _file #:mode 'text #:exists 'truncate))]
  [("-d" "--depth") _depth "expr depth. Default 1"
    (set! depth (string->number _depth))
    (when (not (exact-nonnegative-integer? depth)) (error 'main "Invalid depth: ~a" depth))]
  [("-n" "--number") _number "Number of exprs to produce. This is overriden by --exhaustive. Default 1"
    (set! number (string->number _number))]
  ["--var-odds" _chance "Odds of a terminal producing a variable [0, 1]"
    (if (<= 0.0 (string->number _chance) 1.0)
      (var-odds (string->number _chance))
      (error 'main "--var-odds must be between 0 and 1, inclusive: ~a" _chance))]
  ["--unique-vars" "If this flag is set to #t, every variable will be unique"
    (unique-vars #t)]
  ["--exhaustive" "If this flag is set to #t, this program will output a test for every operator, precision, and rounding mode combination"
    (exhaustive #t)]
  #:args ()
  (parameterize ([pretty-print-columns 160])
    ;(define ops (append (remove* '(cast and or not) operators) '(if)))
    (define ops '(while + - * / < >))
    (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
    ; (define precs '(binary80 binary64 binary32))
    ; (define rnd-modes '(nearestEven toPositive toNegative toZero))
    (define precs '(binary64))
    (define rnd-modes '(nearestEven))

    (when (not (terminal-port? (current-output-port)))
      (fprintf (current-output-port) ";; -*- mode: scheme -*-\n")
      (fprintf (current-output-port) ";; Count: ~a\n\n" number))
    (if (exhaustive)
        (gen-expr-set ops precs rnd-modes consts depth)
        (gen-random-expr ops precs rnd-modes consts depth number)))))
    