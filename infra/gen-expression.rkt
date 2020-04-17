#lang racket

(require "../src/common.rkt" "../src/fpcore.rkt")

(define var-probability (make-parameter 0.5))
(define unique-vars (make-parameter #f))
(define full-set (make-parameter #f))
(define out-file (make-parameter #f))

(define (rand-from-list list)
  (list-ref list (random 0 (length list))))

(define (variable-count expr)
 (match expr
  [(list op args ...) (for/sum ([arg args]) (variable-count arg))]
  [(? constant?) 0]
  [(? symbol?) 1]
  [(? number?) 0]))

(define (gensyms count) 
  (for/list ([i count])
    (string->symbol (format "x~a" (add1 i)))))

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
            `(,op ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops))
                  ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops)))]
          ['fma
            `(,op ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops))
                  ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops))
                  ,(gen-expression-layer ops prec consts (sub1 depth) (rand-from-list ops)))]
      ))]
    [else 
      (if (> (random) (var-probability))
          (rand-from-list consts)
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

(define (gen-random-expr ops precs consts depth)
  (define op (rand-from-list ops))
  (define prec (rand-from-list precs))
  (let* ([expr (gen-expression-layer ops prec consts depth op)]
         [var-count (variable-count expr)]
         [free-count 
           (cond
            [(unique-vars) var-count]
            [(= var-count 0) 0]
            [else (add1 (random 0 var-count))])]
          [args (gensyms free-count)])
    `(FPCore ,args :precision ,prec ,(assign-variables expr args var-count))))

;;; Command line

(module+ main
 (command-line
  #:program "gen-expressions.rkt"
  #:once-each
  ["--var-odds" _chance "Odds of a terminal producing a variable [0, 1]"
    (if (<= 0.0 (string->number _chance) 1.0)
      (var-probability (string->number _chance))
      (error 'main "--var-odds must be between 0 and 1, inclusive: ~a" _chance))]
  ["--unique-vars" "If this flag is set to #t, every variable will be unique"
    (unique-vars #t)]
  ["--full-set" "If this flag is set to #t, a list of tests will with every precision, operator combination"
    (full-set #t)]
  [("-o" "--output") _file "Name of output file"
    (out-file _file)]
  #:args (_depth)
  (define ops 
    '(fabs exp exp2 expm1 log log10 log2 log1p sqrt cbrt sin cos tan asin acos atan
      sinh cosh tanh asinh acosh atanh erf erfc tgamma lgamma ceil floor trunc
      round nearbyint + - * / pow hypot atan2 fmod remainder fmax fmin fdim copysign fma))
  (define consts '(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI M_2_SQRTPI SQRT2 SQRT1_2))
  (define precs '(binary64 binary32))
  (define depth (string->number _depth))
  (when (< depth 0) (error 'main "Invalid depth: ~a" depth))

  (define cores
    (if (full-set)
        (gen-expr-set ops precs consts depth)
        (list (gen-random-expr ops precs consts depth))))
  
  (define outport (if (equal? (out-file) #f) #f ((open-output-file (out-file) #:mode 'text #:exists 'truncate))))
  (for ([core cores]) 
    (if (equal? (out-file) #f)
        (printf "~a\n" core)
        (fprintf outport "~a\n\n" core)))))
    