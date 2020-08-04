#lang racket

(require "../src/common.rkt" "../src/sampler.rkt" "../src/fpcore-visitor.rkt")
(provide gen-expr pretty-fpcore pretty-expr)

(define math-const (make-parameter 0.5)) ; odds of generating a math constant
(define random-const? (make-parameter #f))
(define unique-vars (make-parameter #f))
(define gensym-count (make-parameter 1))
(define exhaustive? (make-parameter #f))

;; Move this somewhere better
(define bool-ops 
        '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

; Combinations with repetition
(define (combinationsr li k)
  (cond [(= k 0) '(())]
        [(empty? li) '()]
        [(append (combinationsr (rest li) k)
                 (map (lambda (x) (cons (first li) x))
                      (combinationsr li (sub1 k))))]))

;; Returns a random list of variables
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

; Returns every combination of terminals given a list of unique variables, the number of variable terminals, and the 
; number of total terminals
(define (terminal-combinations vars var-terms total-terms)
  (define vars* (flatten (for/list ([i vars]) (make-list (add1 (- var-terms (length vars))) i))))
  (define consts (make-list (- total-terms var-terms) 'term))
  (if (empty? vars*) (list (make-list total-terms 'term))
      (let inner ([li (combinations vars* var-terms)])
        (cond [(empty? li) '()]
              [(append (permutations (append (first li) consts))
                      (inner (rest li)))]))))

;; Returns a random list of vars and unassigned terminals
(define (random-terminals vars total-terms)
  (if (> (length vars) total-terms)
      (random-vars vars total-terms)
      (shuffle (append vars (make-list (- total-terms (length vars)) 'term)))))

; Returns a random element of li if exhaustive? is true. Else returns the list
(define (from-list li)
  (if (exhaustive?) li (list (rand-from-list li))))

;; Generates a list of "unique" names
(define (gensyms count [name 'x] [start (gensym-count)])
  (gensym-count (if (= start (gensym-count)) (+ (gensym-count) count) (gensym-count)))
  (for/list ([i (in-range count)])
    (string->symbol (format "~a~a" name (+ i start)))))
    
;; Generates a list of random constants. Will generate random floats in random mode
(define (random-consts count consts prec)
  (for/list ([i (in-range count)])
    (if (random-const?)
        (rand-from-list consts) 
        (if (> (random) (math-const)) (rand-from-list consts) (sample-random prec)))))

; Returns the max number of unbound variables in an expression
(define (unbound-in-expr expr)
 (match expr
  [`(,(or 'while 'while*) ,cond ([,vars ,inits ,updates] ...) ,body) 
    (for/sum ([arg (flatten (list cond body inits updates))]) (unbound-in-expr arg))]
  [`(,(or 'let 'let*) ([,vars ,vals] ...) ,body) (for/sum ([arg (list* body vals)]) (unbound-in-expr arg))]
  [`(if ,cond ,ift ,iff) (for/sum ([arg (list cond ift iff)]) (unbound-in-expr arg))]
  [(list op args ...) (for/sum ([arg args]) (unbound-in-expr arg))]
  ['term  1]
  [_      0]))

; assigns n 'vars' based on a distribution of n total variable terminals in subexpressions
(define (distribute-vars vars distr total) 
  (for/fold ([svars '()] [used 0] #:result (reverse svars))
            ([c distr])
    (let ([v (take (drop vars used) c)])
      (values (list* v svars) (+ used c)))))

(define (assign-vars expr free [first-pass? #f])
  (let inner ([subexpr expr] [free free])
   (match subexpr
    [`(,op ,cond ([,vars ,vals ,updates] ...) ,body)      ; while and while*
     (let* ([args (list* body cond (append vals updates))]        
            [distr (for/list ([arg args]) (unbound-in-expr arg))]               
            [total (foldl + 0 distr)]
            [svars (distribute-vars free distr total)])
       `(,op ,(inner cond (second svars)) 
               ,(for/list ([var vars] [val vals] [update updates] [i (in-naturals)])
                  (list var (inner val (list-ref svars (+ 2 i))) (inner update (list-ref svars (+ 2 (length vals) i)))))      
               ,(inner body (first svars))))]
    [`(,op ([,vars ,vals] ...) ,body) ; let and let*
     (let* ([args (list* body vals)]                                             
            [distr (for/list ([arg args]) (unbound-in-expr arg))]               
            [total (foldl + 0 distr)]
            [svars (distribute-vars free distr total)])
       `(,op (,@(map (λ (x y v) (list x (inner y v))) vars vals (drop svars 1))) ,(inner body (first svars))))]
    [`(if ,cond ,ift, iff)
     (let* ([args (list cond ift iff)]
            [distr (for/list ([arg args]) (unbound-in-expr arg))]
            [total (foldl + 0 distr)])
      `(if ,@(map (curryr inner) args (distribute-vars free distr total))))]    
    [`(,(? operator? op) ,args ...)
     (let* ([distr (for/list ([arg args]) (unbound-in-expr arg))] 
            [total (foldl + 0 distr)])
      `(,op ,@(map (curryr inner) args (distribute-vars free distr total))))]      
    ['term (if (empty? free) subexpr (first free))]
    [_  subexpr])))

;; All possible expression with assigned terminals
(define (assign-terminals expr gen-proc [allow-zero? #t]) ; gen-proc takes number of terminal 'names' to produce
  (define max-vars (unbound-in-expr expr))
  (define vars* (gen-proc max-vars))    
  (for*/lists (exprs vars)
     ([free-count 
        (cond           ; for [0, max] or [1, max] variable terminals 
          [(unique-vars) (list max-vars)]   
          [allow-zero? (from-list (build-list (add1 max-vars) identity))]
          [else (from-list (build-list max-vars add1))])]                               
      [var-count 
        (cond           ; for 0 or [1, free-count] unique variables
          [(unique-vars) (list free-count)]
          [(zero? free-count) (list 0)]
          [else (from-list (build-list free-count add1))])]
      [terminals        ; generate terminal combinations
        (if (exhaustive?) 
            (remove-duplicates (terminal-combinations (take vars* var-count) free-count max-vars))
            (list (random-terminals (take vars* var-count) max-vars)))])
        (values (assign-vars expr terminals) (take vars* var-count))))

;; All possible expressions with assigned terminals given a set of known variables
(define (assign-known expr vars [all? #f]) ; all? ensures that no terminals are left unbound
  (define max-vars (unbound-in-expr expr))
  (cond
    [(zero? max-vars) (list expr)]
    [all?
      (for/list 
        ([terminals (if (exhaustive?) 
                        (combinationsr vars max-vars) 
                        (list (random-terminals (random-vars vars max-vars) max-vars)))])
          (assign-vars expr terminals))]
    [else
      (for*/list 
         ([free-count (if (unique-vars) 
                          (list max-vars) 
                          (from-list (build-list max-vars add1)))]
          [vars* (if (> (length vars) free-count)
                        (combinations vars free-count)
                        (list vars))]
          [terminals (if (exhaustive?) 
                         (remove-duplicates (terminal-combinations vars* free-count max-vars))
                         (list (random-terminals vars* max-vars)))])
            (assign-vars expr terminals))]))
        
; Returns a list of integers of size count such that all values are less than depth
; and at least one is one less than depth.
(define (next-child-depth depth count)
  (if (zero? count) '() 
      (let ([pivot (random 0 (add1 count))]) 
        (for/list ([i (in-range count)]) 
          (if (= i pivot) (sub1 depth) (random 0 depth))))))    

; let value generator for let/let* and while/while*
(define (gen-let-vals op vars gen-proc)
; if 'let' or 'while' and exhaustive:
;   generate combination of expressions, all terminals unassigned
; else if 'let*' or 'while*' and exhaustive:
;   generate val expressions for var 1,2,... (these will be different for let*)
;   and return the cartesian product of these sets
; else, not exhaustive, return random val expression
  (cond
    [(and (exhaustive?) (or (equal? op 'let) (equal? op 'while)))  
      (combinationsr (gen-proc) (length vars))]                   
    [(and (exhaustive?) (or (equal? op 'let*) (equal? op 'while*))) 
      (let ([lsts (for/list ([i (in-range (length vars))])        
                    (for/fold ([comb '()]) ([val (gen-proc)])  
                      (append comb (assign-known val (take vars i)))))])
          (apply cartesian-product lsts))]                        
    [(list (for/list ([var vars] [i (in-naturals)])                                 
              (first (assign-known (first (gen-proc)) 
                                   (if (or (equal? op 'let) (equal? op 'while)) 
                                       '() 
                                       (take vars i))))))]))

; condition generator
(define (gen-cond ops gen-proc)
  (for/fold ([exprs '()]) ([cond (from-list ops)])
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

;; Layer generator
(define (gen-layer ops depth [allow-cond? #f])
  (cond
    [(> depth 0)
      (for/fold ([exprs '()]) 
                ([op (from-list (if allow-cond? (filter (curry set-member? bool-ops) ops) 
                                                (remove* bool-ops ops)))])
       (match op
        [(or '< '> '<= '>= '== '!= 'isfinite 'isinf 'isnan 'isnormal 'signbit) ; conditionals
         (append exprs
          (gen-cond (list op) (curry gen-layer ops (sub1 depth))))]
        ['-*    ; nice solution to unary minus
         (append exprs
          (for/list ([subexpr (gen-layer ops (sub1 depth))])
          `(- ,subexpr)))]
        [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
             'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
             'ceil 'floor 'trunc 'round 'nearbyint 'cast)
         (append exprs
          (for/list ([subexpr (gen-layer ops (sub1 depth))])
          `(,op ,subexpr)))]
        [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
         (append exprs
          (for*/list ([subexpr1 (gen-layer ops (sub1 depth))]
                      [subexpr2 (gen-layer ops (sub1 depth))])
          `(,op ,subexpr1 ,subexpr2)))]
        ['fma
         (append exprs
          (for*/list ([subexpr1 (gen-layer ops (sub1 depth))]
                      [subexpr2 (gen-layer ops (sub1 depth))]
                      [subexpr3 (gen-layer ops (sub1 depth))])
          `(,op ,subexpr1 ,subexpr2 ,subexpr3)))]
        ['if
         (append exprs
          (for*/list ([cond (gen-cond (filter (curry set-member? bool-ops) ops) 
                                      (curry gen-layer ops (random 0 depth)))]
                      [subexpr1 (gen-layer ops (sub1 depth))]
                      [subexpr2 (gen-layer ops (sub1 depth))])
          `(if ,cond ,subexpr1 ,subexpr2)))]
        [(or 'let 'let*)
         (append exprs
          (for/fold ([exprs* '()])
                    ([body (gen-layer ops (sub1 depth))]) ; for all possibe body exprs
            (let-values ([(bodies varss) (assign-terminals 
                                            body 
                                            (curryr gensyms 'x (gensym-count))
                                            #f)])
              (append exprs*       ; for all possible vals combinations, see 'gen-let-vals'                  
                (for/list ([body* bodies] [vars varss] #:when #t 
                           [vals (gen-let-vals op vars (curry gen-layer ops (random 0 depth)))])                  
                  `(,op ,(map list vars vals) ,body*))))))]
        [(or 'while 'while*)
         (append exprs
          (for*/fold ([exprs* '()])   ; for all possibe body exprs, conds
                     ([body (gen-layer ops (sub1 depth))]
                      [cond (gen-cond (filter (curry set-member? bool-ops) ops)
                                      (curry gen-layer ops (sub1 depth)))])
            (let*-values ([(bodies varss) (assign-terminals 
                                            body 
                                            (curryr gensyms 'x (gensym-count))
                                            #f)])
              (append exprs*    ; for all possible vals combinations
                (for/list ([body* bodies] [vars varss] #:when #t 
                           [cond* (assign-known cond vars)] #:when #t
                           [vals (gen-let-vals op vars (curry gen-layer ops (random 0 depth)))]
                                 #:when #t                                                                        
                           [updates 
                            (if (exhaustive?)   ; for all possible update expr combinations
                                (combinationsr 
                                  (for/fold ([comb '()]) ([val (gen-layer ops (random 0 depth))])  
                                        (append comb (assign-known val vars)))
                                  (length vars))
                                (list ; else, get random val expressions
                                  (for/list ([var vars]) 
                                    (first (assign-known 
                                              (first (gen-layer ops (random 0 depth)))
                                              vars)))))])
                      `(,op ,cond* ,(map list vars vals updates) ,body*))))))]
      ))]
    [else (list 'term)]))

;; Top-level generator
(define (gen-expr ops precs rnd-modes consts depth number
                  prec? round? allow-cond? [out-proc expr->bare])
  (define i 1)
  (for* ([c (in-range number)]
         [expr (gen-layer ops depth allow-cond?)] 
         [prec (from-list precs)] 
         [rnd (from-list rnd-modes)])
    (let-values ([(exprs* args*)  ; full expr list
                    (let-values ([(exprs argss) 
                                    (assign-terminals expr (curryr gensyms 'arg 1))])
                      (for/fold  ; iterate through constant combinations
                        ([full-exprs '()] [arg-list '()] #:result (values full-exprs arg-list)) 
                        ([expr* exprs] [args argss] #:when #t 
                          [final 
                            (assign-known expr* 
                              (if (exhaustive?)  
                                 consts 
                                 (random-consts (unbound-in-expr expr*) consts prec)) #t)])
                        (values (append full-exprs (list final)) 
                                (append arg-list (list args)))))])
      (for ([expr* exprs*] [args args*])
        (let ([props (append (if prec? (list ':precision prec) '())
                             (if round? (list ':round (rand-from-list rnd-modes)) '())
                             (if (> depth 3) (list ':name (format "\"Random ~a\"" i)) '()))])
          (set! i (add1 i))
          (out-proc expr* args props)
          (gensym-count 1))))))

;; Pretty formatter

(define (pretty-props props)
  (for/list ([(prop name) (in-dict (apply dict-set* '() props))])
    (format "~a ~a" prop name)))

(define (pretty-expr-helper expr) ; don't call pretty-format twice
  (define/transform-expr (pretty-ify expr ctx)
    [visit-! (λ (vtor props body #:ctx ctx)
                `(! ,@(pretty-props props) ,(visit/ctx vtor body ctx)))])
  (pretty-ify expr '()))

(define (pretty-expr expr)
  (pretty-format (pretty-expr-helper expr) #:mode 'display))

(define (pretty-fpcore core)
  (define-values (name args props* body)
     (match core
      [(list 'FPCore (list args ...) props ... body) (values #f args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values name args props body)]))
  (pretty-format `(,(if body (format "FPCore ~a" args) (format "FPCore ~a ~a" name args))
                    ,@(pretty-props props*)
                    ,(pretty-expr-helper body))
                 #:mode 'display))

;; Expression wrappers

; No wrapper
(define (expr->bare expr args props)
  (displayln (pretty-expr expr))
  (newline))

; Normal test
(define (expr->test expr args props)
  (displayln (pretty-fpcore `(FPCore ,args ,@props ,expr)))
  (newline))

; Test with 'if' statement, returning 1 on success, 0 on failure
(define (expr->bool-test expr args name props)
  (displayln (pretty-fpcore `(FPCore ,args ,@props (if ,expr 1 0))))
  (newline))

;;; Command line
(module+ main
  (define depth 1)
  (define number 1)
  (define ops (remove* '(and or not array dim size ref) 
                        (append operators '(-* if let let* while while*))))
  (define consts (append (remove* '(MAXFLOAT HUGE_VAL INFINITY NAN TRUE FALSE) constants)))
  (define precs '(binary80 binary64 binary32))
  (define rnd-modes '(nearestEven toPositive toNegative toZero))
  (define prec? #t)
  (define round? #t)
  (define allow-cond? #f)   ; allow conditionals at the top level
  (define out-proc expr->test)

  (command-line
    #:program "gen-random.rkt"
    #:once-each
    [("-o" "--output") _file "Name of output file"
      (current-output-port (open-output-file _file #:mode 'text #:exists 'truncate))]
    [("-d" "--depth") _depth "Expression depth. Default 1"
      (set! depth (string->number _depth))
      (when (not (exact-nonnegative-integer? depth)) (error 'main "Invalid depth: ~a" depth))]
    [("-n" "--number") _number "Number of expressions to produce. This is overriden by --exhaustive. Default 1"
      (set! number (string->number _number))]
    ["--unique-vars" "If this flag is set to #t, every variable will be unique"
      (unique-vars #t)]
    [("-e" "--exhaustive") "If this flag is set to #t, this program will output a test for every operator, precision, and rounding mode combination"
      (exhaustive? #t)
      (set! consts '(1.0))] ; simple constant list, very, very simple
    [("-t" "--type") _type "Specifies what to wrap the expressions in. Default is 'test'. Options: 'bare', 'test' 'bool'"
      (cond
       [(equal? _type "bare")   (set! out-proc expr->bare)] ; no wrapper
       [(equal? _type "test")   (set! out-proc expr->test)] ; normal FPCore
       [(equal? _type "bool")   (set! out-proc expr->bool-test) ; if statement, generator only produces conditionals
                                (set! allow-cond? #t)]
       [else  (error 'gen-random "Invalid wrapper type")])]

    [("--operator") _ops "Generates expressions with the given operators. Must be a single string"
      (let ([ops* (map string->symbol (string-split _ops " "))])
        (if (set-member? ops* '- )                ; unary minus is '-*' internally
            (set! ops (append '(-*) ops*))
            (set! ops ops*)))]
    [("--not-operator") _not-ops "Removes the given operators from the default list. Must be a single string"
      (let ([not-ops (map string->symbol (string-split _not-ops " "))])
        (if (set-member? not-ops '-)              ; unary minus is '-*' internally
            (set! ops (remove* (append '(-*) not-ops) ops))
            (set! ops (remove* not-ops ops))))]

    [("--const") _const "Generates expressions with the given constants. Must be a single string"
      (set! consts (map string->symbol (string-split _const " ")))]
    [("--not-const") _not-consts "Removes the given constants from the default list. Must be a single string"
      (set! consts (remove* (map string->symbol (string-split _not-consts " ")) consts))]
    [("--random-const") "50% of constants are also generated from a 'random' sampler rather than from a list or the default list" ; 50% is abitrary
      (random-const? #t)]

    [("--prec") _precs "Generates expressions with the given precisions. Must be a single string"
      (set! precs (map string->symbol (string-split _precs " ")))]
    [("--not-prec") _not-precs "Removes the given precisions from the default list. Must be a single string"
      (set! precs (remove* (map string->symbol (string-split _not-precs " ")) precs))]
    [("--no-prec") "All generated expressions have no top-level rounding annotations. Not to be confused with '--not-prec'"
      (set! prec? #f)
      (set! precs '(binary64))] ; placeholder for loop

    [("--round") _round "Generates expressions with the given round modes. Must be a single string"
      (set! rnd-modes (map string->symbol (string-split _round " ")))]
    [("--not-round") _not-round "Removes the given round modes from the default list. Must be a single string"
      (set! rnd-modes (remove* (map string->symbol (string-split _not-round " ")) rnd-modes))]
    [("--no-round") "All generated expressions have no top-level rounding annotations. Not to be confused with '--not-round'"
      (set! round? #f)
      (set! rnd-modes '(nearestEven))] ; placeholder for loop

    #:args ()
    (parameterize ([pretty-print-columns 100])
      (when (exhaustive?) (set! number 1)) ;; override number if exhaustive generation
      (when (equal? out-proc expr->bare) ;; avoid redudancy when generating bare expressions
        (set! prec? #f)
        (set! precs '(binary64))
        (set! round? #f)
        (set! rnd-modes '(nearestEven)))  
      (when (not (terminal-port? (current-output-port)))
        (fprintf (current-output-port) ";; -*- mode: scheme -*-\n")
        (fprintf (current-output-port) (if (exhaustive?)
                                           (format ";; Exhaustive at depth ~a\n\n" depth) 
                                           (format ";; Count: ~a\n\n" number))))
      (gen-expr ops precs rnd-modes consts depth number prec? round? allow-cond? out-proc))))
    