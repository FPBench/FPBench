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

; Combinations with repetition
(define (combinationsr li k)
  (cond [(= k 0) '(())]
        [(empty? li) '()]
        [(append (combinationsr (rest li) k)
                 (map (lambda (x) (cons (first li) x))
                      (combinationsr li (sub1 k))))]))

; Returns a random element of li if exhaustive? is true. Else returns the list
(define (from-list li exhaustive?)
  (if exhaustive? li (list (rand-from-list li))))

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

(define (assign-vars expr free [first-pass? #f])
  (define varc (length free))
  (let inner ([subexpr expr] [free free])
   (match subexpr
    [`(while ,cond ([,vars ,vals ,updates] ...) ,body)
     (if first-pass?
       (begin                                                                             ; first pass
         (set! first-pass? #f)
        `(while ,(inner cond (remove-duplicates (random-vars vars (add1 (random 0 (max-unbound-in-expr cond))))))
                ,(for/list ([var vars] [val vals] [update updates]) 
                    (list var val (inner update (random-vars vars (add1 (random 0 (max-unbound-in-expr update)))))))
                ,(inner body free)))     
       (let* ([args (list* body cond (append vals updates))]                              ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(while ,(inner cond (second svars)) 
                 ,(for/list ([var vars] [val vals] [update updates] [i (in-naturals)])
                      (list var (inner val (list-ref svars (+ 2 i))) (inner update (list-ref svars (+ 2 (length vals) i)))))      
                 ,(inner body (first svars)))))]
    [`(while* ,cond ([,vars ,vals ,updates] ...) ,body)
     (if first-pass?
       (begin                                                                             ; first pass
         (set! first-pass? #f)
        `(while* ,(inner cond (remove-duplicates (random-vars vars (add1 (random 0 (max-unbound-in-expr cond))))))
                 ,(for/list ([var vars] [val vals] [update updates] [i (in-naturals)])
                    (list var (inner val (random-vars (take vars i) (random 0 (add1 (max-unbound-in-expr val)))))
                          (inner update (random-vars vars (add1 (random 0 (max-unbound-in-expr update)))))))
                 ,(inner body free)))        
       (let* ([args (list* body cond (append vals updates))]                              ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(while* ,(inner cond (second svars)) 
                  ,(for/list ([var vars] [val vals] [update updates] [i (in-naturals)])
                      (list var (inner val (list-ref svars (+ 2 i))) (inner update (list-ref svars (+ 2 (length vals) i)))))      
                  ,(inner body (first svars)))))]
    [`(let ([,vars ,vals] ...) ,body)
     (if first-pass?
       (begin                                                                             ; first pass
         (set! first-pass? #f)
        `(let (,@(map list vars vals)) ,(inner body free)))     
       (let* ([args (list* body vals)]                                                    ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(let (,@(map (λ (x y v) (list x (inner y v))) vars vals (drop svars 1)))
              ,(inner body (first svars)))))]
    [`(let* ([,vars ,vals] ...) ,body)
     (if first-pass?
       (begin                                                                             ; first pass
         (set! first-pass? #f)
        `(let* ,(for/list ([i (in-naturals)] [var vars] [val vals])
                    (list var (inner val (random-vars (take vars i) (random 0 (add1 (max-unbound-in-expr val)))))))
               ,(inner body (random-vars free (random 0 (add1 (length free)))))))
       (let* ([args (list* body vals)]                                                    ; later pass
              [distr (for/list ([arg args]) (max-unbound-in-expr arg))]               
              [total (foldl + 0 distr)]
              [svars (distribute-vars free distr total)])
         `(let* (,@(map (λ (x y v) (list x (inner y v))) vars vals (drop svars 1)))
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
    [`(while ,cond ([,vars ,vals ,updates] ...) ,body) 
      `(while ,(inner cond) (,@(map (λ (x y z) (list x (inner y) (inner z))) vars vals updates)) ,(inner body))]
    [`(while* ,cond ([,vars ,vals ,updates] ...) ,body) 
      `(while* ,(inner cond) (,@(map (λ (x y z) (list x (inner y) (inner z))) vars vals updates)) ,(inner body))]
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
  (if (zero? count) '() 
      (let ([pivot (random 0 (add1 count))]) 
        (for/list ([i (in-range count)]) 
          (if (= i pivot) (sub1 depth) (random 0 depth))))))    

;;; Generator

(define (gen-cond ops gen-proc exhaustive?)
  (for/fold ([exprs '()]) ([cond (from-list ops exhaustive?)])
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

(define (gen-layer ops depth exhaustive?)
  (cond
    [(> depth 0)
      (for/fold ([exprs '()]) ([op (from-list (remove* bool-ops ops) exhaustive?)])
       (match op
        [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
             'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
             'ceil 'floor 'trunc 'round 'nearbyint 'cast)
         (append exprs
          (for/list ([subexpr (gen-layer ops (sub1 depth) exhaustive?)])
          `(,op ,subexpr)))]
        [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
         (append exprs
          (for*/list ([subexpr1 (gen-layer ops (sub1 depth) exhaustive?)]
                      [subexpr2 (gen-layer ops (sub1 depth) exhaustive?)])
          `(,op ,subexpr1 ,subexpr2)))]
        ['fma
         (append exprs
          (for*/list ([subexpr1 (gen-layer ops (sub1 depth) exhaustive?)]
                      [subexpr2 (gen-layer ops (sub1 depth) exhaustive?)]
                      [subexpr3 (gen-layer ops (sub1 depth) exhaustive?)])
          `(,op ,subexpr1 ,subexpr2 ,subexpr3)))]
        ['if
         (append exprs
          (for*/list ([cond (gen-cond (filter (curryr member? bool-ops) ops) (thunk (gen-layer ops (sub1 depth) exhaustive?)) exhaustive?)]
                      [subexpr1 (gen-layer ops (sub1 depth) exhaustive?)]
                      [subexpr2 (gen-layer ops (sub1 depth) exhaustive?)])
          `(if ,cond ,subexpr1 ,subexpr2)))]
        [(or 'let 'let*)
         (append exprs
          (for*/list ([body (gen-layer ops (sub1 depth) exhaustive?)]                                        ; for all possibe body exprs
                      [free-count (from-list (for/list ([i (in-range (max-unbound-in-expr body))]) (add1 i)) ; number terminals to be assigned as variables   
                                             exhaustive?)]                                            
                      [var-count (from-list (for/list ([i (in-range free-count)]) (add1 i))                  ; number of unique variables
                                             exhaustive?)]  
                      [vals (combinationsr (gen-layer ops (random 0 depth) exhaustive?) var-count)])         ; for all possible vals combinations
            (let ([vars (gensyms var-count)])
              (assign-vars `(,op ,(map list vars vals) ,body) (random-vars vars free-count) #t))))]
        [(or 'while 'while*)
         (append exprs 
          (for*/list ([body (gen-layer ops (sub1 depth) exhaustive?)]                                     ; for all possible body exprs
                      [cond (gen-cond (filter (curryr member? bool-ops) ops)                              ; for all possible conds
                                      (thunk (gen-layer ops (sub1 depth) exhaustive?)) exhaustive?)]
                      [free-count (if (unique-vars) (list (max-unbound-in-expr body))                     ; for [0, max] variable terminals   
                                      (from-list (build-list (add1 (max-unbound-in-expr body)) identity) exhaustive?))]                                           
                      [var-count (if (unique-vars) (list free-count)                                      ; for 0 or [1, free-count] unique variables
                                     (from-list (if (zero? free-count) (list 0) (build-list free-count (λ (x) (add1 x)))) exhaustive?))]
                      [vals (combinationsr (gen-layer ops (random 0 depth) exhaustive?) var-count)]       ; for all possible val expr combinations
                      [updates (combinationsr (gen-layer ops (random 0 depth) exhaustive?) var-count)])   ; for all possible update expr combinations
            (let ([vars (gensyms var-count)])
              (assign-vars
                `(,op ,cond ,(map list vars vals updates) ,body) (random-vars vars free-count) #t))))]
      ))]
    [else (list 'term)]))

(define (gen-expr ops precs rnd-modes consts depth number exhaustive?)
  (define i 1)
  (for* ([c (in-range number)]
         [expr (gen-layer ops depth exhaustive?)] [prec (from-list precs exhaustive?)] [rnd (from-list rnd-modes exhaustive?)])
   (parameterize ([gensym-count 1])
    (let* ([max-vars (max-unbound-in-expr expr)])
      (for* ([free-count (if (unique-vars) (list max-vars)       
                             (from-list (build-list (add1 max-vars) identity) exhaustive?))]
             [var-count (if (unique-vars) (list free-count)
                            (from-list (if (zero? free-count) (list 0) (build-list free-count (λ (x) (add1 x)))) exhaustive?))])
        (let* ([args (gensyms var-count 'arg 1)]
               [props `(,(format ":prec ~a" prec) ,(format ":round ~a" (rand-from-list rnd-modes)))]
               [name-props (if (> depth 3) (list* (format ":name \"Random ~a\"" i) props) props)]) 
          (set! i (add1 i))
          (pretty-display
           `(,(format "FPCore ~a" args) ,@name-props ,(assign-consts (assign-vars expr (random-vars args free-count)) consts prec))
            (current-output-port))
          (newline)))))))

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
  ["--unique-vars" "If this flag is set to #t, every variable will be unique"
    (unique-vars #t)]
  ["--exhaustive" "If this flag is set to #t, this program will output a test for every operator, precision, and rounding mode combination"
    (exhaustive #t)]
  #:args ()
  (parameterize ([pretty-print-columns 200])
    (define ops (append (remove* '(and or not) operators) '(if let let* while while*)))
    (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
    (define precs '(binary80 binary64 binary32))
    (define rnd-modes '(nearestEven toPositive toNegative toZero))

    (when (exhaustive) (set! number 1)) ;; override number if exhaustive generation
    (when (not (terminal-port? (current-output-port)))
      (fprintf (current-output-port) ";; -*- mode: scheme -*-\n")
      (fprintf (current-output-port) (if (exhaustive) (format ";; Exhaustive at depth ~a\n\n" depth) (format ";; Count: ~a\n\n" number))))

    (gen-expr ops precs rnd-modes consts depth number (exhaustive)))))
    