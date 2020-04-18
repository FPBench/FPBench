#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt" "../src/sampler.rkt")

(define var-probability (make-parameter 0.5))
(define unique-vars (make-parameter #f))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

(define (gensyms count) 
  (for/list ([i count])
    (string->symbol (format "x~a" (add1 i)))))

(define (variable-count expr)
 (match expr
  [(list op args ...) (for/sum ([arg args]) (variable-count arg))]
  [(? constant?) 0]
  [(? symbol?) 1]
  [(? number?) 0]))

(define (assign-variables expr vars var-count)
  (define unused vars)
  (define bound 0)
  (let inner ([subexpr expr])
    (match subexpr
      [(list op args ...) (append `(,op) (map inner args))]
      ['x
        (if (> (- var-count bound) (length unused))
          (let ([name (rand-from-list vars)])
              (set! bound (add1 bound))
              (set! unused (remove name unused))
              name)
          (let ([name (rand-from-list unused)])
              (set! bound (add1 bound))
              (set! unused (remove name unused))
              name))]
      [_    subexpr])))

; Returns a list of integers of size count such that all values are less than depth
; and at least one is one less than depth.
(define (next-children-depth depth count)
  (if (= depth 0) '()
    (let ([li (for/list ([i (in-range count)])
                  (random 0 depth))])
      (if (for/or ([v li]) (= v (sub1 depth)))
          li
          (next-children-depth depth count)))))

;;; Generator

(define (gen-expression-layer ops prec consts depth [_op #f])
  (cond
    [(> depth 0)
      (let ([op (if (equal? _op #f) (rand-from-list ops) _op)])
        (match op
          [(or 'fabs 'exp 'exp2 'expm1 'log 'log10 'log2 'log1p 'sqrt 'cbrt 'sin 'cos 'tan 
               'asin 'acos 'atan 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh 'erf 'erfc 'tgamma 'lgamma 
               'ceil 'floor 'trunc 'round 'nearbyint)
            `(,op ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops)))]
          [(or '+ '- '* '/ 'pow 'hypot 'atan2 'fmod 'remainder 'fmax 'fmin 'fdim 'copysign)
            (let ([depths (next-children-depth depth 2)])
              `(,op ,(gen-expression-layer ops prec consts (first depths) (rand-from-list ops))
                    ,(gen-expression-layer ops prec consts (second depths) (rand-from-list ops))))]
          ['fma
            (let ([depths (next-children-depth depth 3)])
              `(,op ,(gen-expression-layer ops prec consts (first depths) (rand-from-list ops))
                    ,(gen-expression-layer ops prec consts (second depths) (rand-from-list ops))
                    ,(gen-expression-layer ops prec consts (third depths) (rand-from-list ops))))]
      ))]
    [else 
      (if (> (random) (var-probability))
          (if (> (random) 0.5) (rand-from-list consts) (sample-random prec))
          'x)]))

(define (gen-expr-set ops precs consts depth)
  (for*/list ([op ops] [prec precs])
    (let* ([expr (gen-expression-layer ops prec consts depth op)]
           [var-count (variable-count expr)]
           [free-count 
            (cond
              [(unique-vars) var-count]
              [(= var-count 0) 0]
              [else (add1 (random 0 var-count))])]
           [args (gensyms free-count)])
      `(FPCore ,args :precision ,prec ,(assign-variables expr args var-count)))))

(define (gen-random-expr ops precs consts depth number)
  (for/list ([i (in-range number)])
    (let* ([op (rand-from-list ops)]
           [prec (rand-from-list precs)]
           [expr (gen-expression-layer ops prec consts depth op)]
           [var-count (variable-count expr)]
           [free-count 
            (cond
              [(unique-vars) var-count]
              [(= var-count 0) 0]
              [else (add1 (random 0 var-count))])]
            [args (gensyms free-count)])
      `(FPCore ,args :precision ,prec ,(assign-variables expr args var-count)))))

;;; Command line

(module+ main
 (define full-set #f)
 (define depth 1)
 (define outport #f)
 (define number 1)

 (command-line
  #:program "gen-expressions.rkt"
  #:once-each
  [("-o" "--output") _file "Name of output file"
    (set! outport (open-output-file _file #:mode 'text #:exists 'truncate))]
  [("-d" "--depth") _depth "Expression depth. Default 1"
    (set! depth (string->number _depth))
    (when (not (exact-nonnegative-integer? depth)) (error 'main "Invalid depth: ~a" depth))]
  [("-n" "--number") _number "Number of expressions to produce. This is overriden by --full-set. Default 1"
    (set! number (string->number _number))]
  ["--var-odds" _chance "Odds of a terminal producing a variable [0, 1]"
    (if (<= 0.0 (string->number _chance) 1.0)
      (var-probability (string->number _chance))
      (error 'main "--var-odds must be between 0 and 1, inclusive: ~a" _chance))]
  ["--unique-vars" "If this flag is set to #t, every variable will be unique"
    (unique-vars #t)]
  ["--full-set" "If this flag is set to #t, this program will output a test for every op/precision combination"
    (set! full-set #t)]
  #:args ()
  (define ops 
    '(fabs exp exp2 expm1 log log10 log2 log1p sqrt cbrt sin cos tan asin acos atan
      sinh cosh tanh asinh acosh atanh erf erfc tgamma lgamma ceil floor trunc
      round nearbyint + - * / pow hypot atan2 fmod remainder fmax fmin fdim copysign fma))
  (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
  (define precs '(binary64 binary32))

  (define cores
    (if full-set (gen-expr-set ops precs consts depth)
                 (gen-random-expr ops precs consts depth number)))

  (for ([core cores]) 
    (if (equal? outport #f)
      (pretty-print core (current-output-port) 1)
      (fprintf outport "~a\n\n" core)))))
    