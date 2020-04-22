#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt" "../src/sampler.rkt")

(define var-odds (make-parameter 0.5))
(define math-const (make-parameter 0.5))
(define unique-vars (make-parameter #f))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

(define (gensyms count) 
  (for/list ([i count])
    (string->symbol (format "x~a" (add1 i)))))

(define (variable-count expr)
 (match expr
  [(list op args ...) (for/sum ([arg args]) (variable-count arg))]
  ['var       1]    ; variable placeholder
  [_          0]))

(define (assign-terminals expr consts prec vars var-count [simple-const #f])
  (define unused vars)
  (define bound 0)
  (let inner ([subexpr expr])  
    (match subexpr
      [(list op args ...) (append `(,op) (map inner args))]
      ['var
       (cond
        [(unique-vars)
          (let ([name (list-ref vars bound)])
            (set! bound (add1 bound))
            name)]
        [(> (- var-count bound) (length unused))
          (let ([name (rand-from-list vars)])
            (set! bound (add1 bound))
            (set! unused (remove name unused))
            name)]
        [else
          (let ([name (rand-from-list unused)])
            (set! bound (add1 bound))
            (set! unused (remove name unused))
            name)])]
      ['const  (if simple-const 
                   (rand-from-list '(1.0 0.0 -1.0))
                   (if (> (random) (math-const)) (rand-from-list consts) (sample-random prec)))]
      [_       subexpr])))

; Returns a list of integers of size count such that all values are less than depth
; and at least one is one less than depth.
(define (next-child-depth depth count)
  (if (= depth 0) '()
    (let ([li (for/list ([i (in-range count)]) (random 0 depth))])
      (if (for/or ([v li]) (= v (sub1 depth)))
          li
          (next-child-depth depth count)))))

;;; Set generator

(define (gen-set-layer ops consts depth)
  (cond
    [(> depth 0)
     (for/fold ([exprs '()]) ([op ops])
      (match op
        [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
              'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
              'ceil 'floor 'trunc 'round 'nearbyint)
         (append exprs
           (for/list ([subexpr (gen-set-layer ops consts (sub1 depth))])
            `(,op ,subexpr)))]
        [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
         (append exprs
           (for*/list ([subexpr1 (gen-set-layer ops consts (sub1 depth))]
                       [subexpr2 (gen-set-layer ops consts (sub1 depth))])
            `(,op ,subexpr1 ,subexpr2)))]
        ['fma
         (append exprs
           (for*/list ([subexpr1 (gen-set-layer ops consts (sub1 depth))]
                       [subexpr2 (gen-set-layer ops consts (sub1 depth))]
                       [subexpr3 (gen-set-layer ops consts (sub1 depth))])
            `(,op ,subexpr1 ,subexpr2 ,subexpr3)))]))]
    [(equal? (var-odds) 1.0) (list 'var)]
    [else (list 'const 'var)]))

(define (gen-expr-set ops precs rnd-modes consts depth)
  (let ([exprs (gen-set-layer ops consts depth)])
    (for*/list ([expr exprs] [prec precs] [rnd rnd-modes])
      (let* ([var-count (variable-count expr)]
             [free-count 
              (cond
               [(unique-vars) var-count]
               [(= var-count 0) 0]
               [else (add1 (random 0 var-count))])]
             [args (gensyms free-count)])
    `(FPCore ,args :precision ,prec :round ,rnd ,(assign-terminals expr consts prec args var-count #t))))))

;; Random generator

(define (gen-rand-layer ops consts depth)
  (cond
    [(> depth 0)
      (let ([op (rand-from-list ops)])
        (match op
          [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
               'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
               'ceil 'floor 'trunc 'round 'nearbyint)
            `(,op ,(gen-rand-layer ops consts (sub1 depth)))]
          [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
            (let ([depths (next-child-depth depth 2)])
              `(,op ,(gen-rand-layer ops consts (first depths)) 
                    ,(gen-rand-layer ops consts (second depths))))]
          ['fma
            (let ([depths (next-child-depth depth 3)])
              `(,op ,(gen-rand-layer ops consts (first depths))
                    ,(gen-rand-layer ops consts (second depths))
                    ,(gen-rand-layer ops consts (third depths))))]
      ))]
    [else (if (> (random) (var-odds)) 'const 'var)]))

(define (gen-random-expr ops precs rnd-modes consts depth number)
  (for/list ([i (in-range number)])
    (let* ([expr (gen-rand-layer ops consts depth)]
           [prec (rand-from-list precs)]
           [rnd (rand-from-list rnd-modes)]
           [var-count (variable-count expr)]
           [free-count 
            (cond
              [(unique-vars) var-count]
              [(= var-count 0) 0]
              [else (add1 (random 0 var-count))])]
           [args (gensyms free-count)])
      `(FPCore ,args :precision ,prec :round ,rnd ,(assign-terminals expr consts prec args var-count)))))

;;; Command line

(module+ main
 (define full-set #f)
 (define depth 1)
 (define newline "\n")
 (define number 1)
 (command-line
  #:program "gen-exprs.rkt"
  #:once-each
  [("-o" "--output") _file "Name of output file"
    (current-output-port (open-output-file _file #:mode 'text #:exists 'truncate))
    (set! newline "\n\n")]
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
    (set! full-set #t)]
  #:args ()

  (define ops 
    '(fabs exp exp2 expm1 log log10 log2 log1p sqrt cbrt sin cos tan asin acos atan
      sinh cosh tanh asinh acosh atanh erf erfc tgamma lgamma ceil floor trunc
      round nearbyint + - * / pow hypot atan2 fmod remainder fmax fmin fdim copysign fma))
  (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
  (define precs '(binary80 binary64 binary32))
  (define rnd-modes '(nearestEven toPositive toNegative toZero))
  (define cores
    (if full-set (gen-expr-set ops precs rnd-modes consts depth)
                 (gen-random-expr ops precs rnd-modes consts depth number)))

  (when (not (terminal-port? (current-output-port)))
    (fprintf (current-output-port) ";; -*- mode: scheme -*-\n")
    (fprintf (current-output-port) ";; Count: ~a\n\n" (length cores)))
  (for ([core cores]) (fprintf (current-output-port) "~a~a" core newline))))
    