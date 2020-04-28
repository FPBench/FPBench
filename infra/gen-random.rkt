#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt" "../src/sampler.rkt")

(define var-odds (make-parameter 0.5))
(define math-const (make-parameter 0.5))
(define unique-vars (make-parameter #f))
(define full-set (make-parameter #f))

;; Move this somewhere better
(define bool-ops '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

(define (member? v list)
  (for/or ([i list]) (equal? i v)))

(define (clamp x low high)
  (max low (min x high)))

(define (gensyms count [start 1]) 
  (for/list ([i (in-range start (+ start count))])
    (string->symbol (format "x~a" i))))

; Returns the number of variable terminals in an expression
(define (variable-count expr)
 (match expr
  [`(let ([,vars ,vals] ...) ,body) (+ (for/sum ([val vals]) (variable-count val)) (variable-count body))]
  [`(let* ([,vars ,vals] ...) ,body) (+ (for/sum ([val vals]) (variable-count val)) (variable-count body))]
  [`(if ,cond ,ift ,iff) (for/sum ([arg (list cond ift iff)]) (variable-count arg))]
  [(list op args ...) (for/sum ([arg args]) (variable-count arg))]
  ['var  1]    ; variable placeholder
  [_     0]))

; Returns the max number of unbound variables in an expression
(define (max-unbound-in-expr expr)
 (match expr
  [`(let ([,vars ,vals] ...) ,body) (- (+ (max-unbound-in-expr body) (for/sum ([val vals]) (max-unbound-in-expr val))) 
                                       (length vars))]
  [`(let* ([,vars ,vals] ...) ,body) (- (+ (max-unbound-in-expr body) (for/sum ([val vals]) (max-unbound-in-expr val)))
                                        (length vars))]
  [`(if ,cond ,ift ,iff) (for/sum ([arg (list cond ift iff)]) (max-unbound-in-expr arg))]
  [(list op args ...) (for/sum ([arg args]) (max-unbound-in-expr arg))]
  ['var  1]    ; variable placeholder
  [_     0]))

; Returns the min number of unbound variables in an expression
(define (min-unbound-in-expr expr)
 (match expr
  [`(let ([,vars ,vals] ...) ,body) (clamp (for/sum ([val vals]) (min-unbound-in-expr val)) 0 1)]
  [`(let* ([,vars ,vals] ...) ,body) (min-unbound-in-expr (first vals))]
  [`(if ,cond ,ift ,iff) (clamp (for/sum ([arg (list cond ift iff)]) (min-unbound-in-expr arg)) 0 1)]
  [(list op args ...) (clamp (for/sum ([arg args]) (min-unbound-in-expr arg)) 0 1)]
  ['var  1]    ; variable placeholder
  [_     0]))

; Returns a random list of size count that countains every element of free at least once.
(define (random-vars free count)
  (define unused free)
  (define bound 0)
  (if (= count 0) '()
    (for/list ([i (in-range count)])
      (let ([name (if (> (- count bound) (length unused))
                      (rand-from-list free)
                      (rand-from-list unused))])
        (set! bound (add1 bound))
        (set! unused (remove name unused))
        name))))

(define (subexpr-vars vars distr)
  (define used 0)
  (if (empty? distr) '()
    (for/list ([c distr])
      (let ([v (take (drop vars used) c)])
        (set! used (+ used c))
        v))))

(define (assign-terminals expr consts prec free)
  (define varc (length free))
  (let inner ([subexpr expr] [free free])
   (match subexpr
    [`(let ([,vars ,vals] ...) ,body)
      (let* ([letfree-count (for/sum ([val vals]) (max-unbound-in-expr val))]
             [letfree (if (= (length free) 0) '() (random-vars free letfree-count))]
             [distr (for/list ([val vals]) (min (length free) (max-unbound-in-expr val)))]
             [vars* (gensyms (length vars) (add1 varc))]
             [unbound (remove* (remove-duplicates letfree) free)] ; must be in body
             [extra (- (max-unbound-in-expr body) (length vars) (length unbound))])
        (set! varc (+ varc (length vars)))
       `(let (,@(map (λ (x y vs) (list x (inner y vs))) vars* vals (subexpr-vars letfree distr))) 
            ,(inner body (append vars* unbound (if (= (length free) 0) '() (random-vars free extra))))
      ))]
    [`(let* ([,vars ,vals] ...) ,body)
      (let* ([letfree-count (for/sum ([val vals]) (max-unbound-in-expr val))]
             [letfree (if (= (length free) 0) '() (random-vars free letfree-count))]
             [distr (for/list ([val vals]) (min (length free) (max-unbound-in-expr val)))]
             [vars* (gensyms (length vars) (add1 varc))]
             [unbound (remove* (remove-duplicates letfree) free)] ; must be in body
             [extra (- (max-unbound-in-expr body) (length vars) (length unbound))])
        (set! varc (+ varc (length vars)))
       `(let* (,@(for/list ([var vars*] [val vals] [vs (subexpr-vars letfree distr)])
                    (let ([vs* (if (= (length vs) (min-unbound-in-expr vs)) vs 
                                   (random-vars (append (remove* (member var vars*) vars*) vs) 
                                                (- (length vs) (min-unbound-in-expr vs))))])
                      (list var (inner val vs*)))))
            ,(inner body (append vars* unbound (if (= (length free) 0) '() (random-vars free extra))))
      ))]
    [`(if ,cond ,ift ,iff)
      (let* ([var-count (max-unbound-in-expr subexpr)]
             [args (list cond ift iff)]
             [vars (if (= (length free) 0) '() (random-vars free var-count))]
             [distr (for/list ([arg args]) (min (length free) (max-unbound-in-expr arg)))])
        `(if ,@(map (λ (x vs) (inner x vs)) args (subexpr-vars vars distr))))]     
    [(list op args ...)
      (let* ([var-count (max-unbound-in-expr subexpr)]
             [vars (if (= (length free) 0) '() (random-vars free var-count))]
             [distr (for/list ([arg args]) (min (length free) (max-unbound-in-expr arg)))])
        `(,op ,@(map (λ (x vs) (inner x vs)) args (subexpr-vars vars distr))))]
    ['var (first free)]
    ['const (if (full-set)
                (rand-from-list '(1.0 0.0 -1.0))
                (if (> (random) (math-const)) (rand-from-list consts) (sample-random prec)))]
    [_ expr])))

; Returns a list of integers of size count such that all values are less than depth
; and at least one is one less than depth.
(define (next-child-depth depth count)
  (if (= depth 0) '()
    (let ([li (for/list ([i (in-range count)]) (random 0 depth))])
      (if (for/or ([v li]) (= v (sub1 depth)))
          li
          (next-child-depth depth count)))))

;;; Set generator

(define (gen-set-cond ops child-proc)
  (for/fold ([exprs '()]) ([cond ops])
   (match cond
    [(or '< '> '<= '>= '== '!=)
     (append exprs
      (for*/list ([subexpr1 (child-proc)]
                  [subexpr2 (child-proc)])
        `(,cond ,subexpr1 ,subexpr2)))]
    [(or 'isfinite 'isinf 'isnan 'isnormal 'signbit)
     (append exprs
      (for*/list ([subexpr (child-proc)])
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
  (for*/list ([expr (gen-set-layer ops depth)] [prec precs] [rnd rnd-modes])
    (let* ([max-vars (max-unbound-in-expr expr)]
           [min-vars (min-unbound-in-expr expr)]
           [free-count (if (unique-vars) max-vars (random min-vars (add1 max-vars)))]
           [args (gensyms free-count)])
  `(FPCore ,args :precision ,prec :round ,rnd ,(assign-terminals expr consts prec args)))))

;; Random generator

(define (gen-body-min-vars gen-proc [min 1])
  (let ([body (gen-proc)])
    (if (< (min-unbound-in-expr body) min)
        (gen-body-min-vars gen-proc)
        body)))

(define (gen-rand-cond ops child-proc)
  (let ([cond (rand-from-list ops)])
   (match cond
    [(or '< '> '<= '>= '== '!=)
      `(,cond ,(child-proc) ,(child-proc))]
    [(or 'isfinite 'isinf 'isnan 'isnormal 'signbit)
      `(,cond ,(child-proc))])))

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
          (let* ([body (gen-body-min-vars (thunk (gen-rand-layer ops (sub1 depth))))]
                 [var-count (add1 (random 0 (max-unbound-in-expr body)))]
                 [vars (for/list ([i (in-range var-count)]) 'var)]
                 [vals (for/list ([i (in-range var-count)]) 
                          (gen-rand-layer ops (random 0 depth)))])
            `(,op (,@(map list vars vals)) ,body))]
      ))]
    [else (if (> (random) (var-odds)) 'const 'var)]))

(define (gen-random-expr ops precs rnd-modes consts depth number)
  (for/list ([i (in-range number)])
    (let* ([expr (gen-rand-layer ops depth)]
           [prec (rand-from-list precs)]
           [rnd (rand-from-list rnd-modes)]
           [max-vars (max-unbound-in-expr expr)]
           [min-vars (min-unbound-in-expr expr)]
           [free-count (if (unique-vars) max-vars (random min-vars (add1 max-vars)))]
           [args (gensyms free-count)])
      `(,(string->symbol (format "FPCore ~a" args)) 
          ,(if (> depth 3) (string->symbol (format ":name \"Random ~a\"" (add1 i))) "")
          ,(string->symbol (format ":prec ~a" prec))
          ,(string->symbol (format ":round ~a" rnd))
          ,(assign-terminals expr consts prec args)))))

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
  [("-n" "--number") _number "Number of exprs to produce. This is overriden by --full-set. Default 1"
    (set! number (string->number _number))]
  ["--var-odds" _chance "Odds of a terminal producing a variable [0, 1]"
    (if (<= 0.0 (string->number _chance) 1.0)
      (var-odds (string->number _chance))
      (error 'main "--var-odds must be between 0 and 1, inclusive: ~a" _chance))]
  ["--unique-vars" "If this flag is set to #t, every variable will be unique"
    (unique-vars #t)]
  ["--full-set" "If this flag is set to #t, this program will output a test for every operator, precision, and rounding mode combination"
    (full-set #t)]
  #:args ()
  (parameterize ([pretty-print-columns 160])
    ;(define ops (append (remove* '(cast and or not) operators) '(if)))
    (define ops '(if <))
    (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
    ; (define precs '(binary80 binary64 binary32))
    ; (define rnd-modes '(nearestEven toPositive toNegative toZero))
    (define precs '(binary64))
    (define rnd-modes '(nearestEven))
    (define cores
      (if (full-set)
          (gen-expr-set ops precs rnd-modes consts depth)
          (gen-random-expr ops precs rnd-modes consts depth number)))

    (when (full-set) (set! number (length cores)))
    (when (not (terminal-port? (current-output-port)))
      (fprintf (current-output-port) ";; -*- mode: scheme -*-\n")
      (fprintf (current-output-port) ";; Count: ~a\n\n" number))
    (for ([core cores]) 
      (pretty-display core (current-output-port))
      (newline)))))
    