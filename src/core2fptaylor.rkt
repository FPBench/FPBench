#lang racket

(require "fpcore-reader.rkt" "fpcore-extra.rkt" "imperative.rkt" "range-analysis.rkt")

(provide core->fptaylor fptaylor-supported *fptaylor-inexact-scale*)

(define *fptaylor-inexact-scale* (make-parameter 1))

(define fptaylor-supported 
  (supported-list
    (invert-op-proc 
      (curry set-member?
             '(atan2 cbrt ceil copysign erf erfc exp2 expm1 fdim floor fmod hypot if 
              lgamma log10 log1p log2 nearbyint pow remainder round tgamma trunc while while*
              array dim size ref for for* tensor tensor*)))
    (invert-const-proc (curry set-member? '(NAN INFINITY)))
    (curry set-member? '(binary16 binary32 binary64 binary128 real))
    ; Note: nearestEven and nearestAway behave identically in FPTaylor
    ieee754-rounding-modes
    #f))

; Language-specific reserved names (avoid name collisions)
(define fptaylor-reserved
  '(Variables variables Definitions definitions Expressions expressions Constraints constraints
    IN in int real float16 float32 float64 float128
    rnd no_rnd rnd16_ne rnd16 rnd16_0 rnd16_down rnd16_up
    rnd32_ne rnd32 rnd32_0 rnd32_down rnd32_up
    rnd64_ne rnd64 rnd64_0 rnd64_down rnd64_up
    rnd128_ne rnd128 rnd128_0 rnd128_down rnd128_up
    inv abs fma sqrt min max exp log cos sin tan cosh sinh tanh
    acos asin atan atan2 arccos arcsin arctan acosh asinh atanh
    arsinh arcosh artanh arcsinh arccosh arctanh argsinh argcosh argtanh
    sub2 floor_power2 interval))

(define (inexact-operator? op)
  (set-member? '(exp log sin cos tan asin acos atan
                sinh cosh tanh asinh acosh atanh) op))

(define/match (prec->fptaylor prec)
  [('undefined) ""]
  [('real) "real"]
  [('binary16) "float16"]
  [('binary32) "float32"]
  [('binary64) "float64"]
  [('binary128) "float128"]
  [(_) (error 'prec->fptaylor "Unsupported precision ~a" prec)])

(define/match (rm->fptaylor rm)
  [('nearestEven) "ne"]
  ; The same as 'nearestEven
  [('nearestAway) "ne"]
  [('toPositive) "up"]
  [('toNegative) "down"]
  [('toZero) "zero"]
  [(_) (error 'rm->fptaylor "Unsupported rounding mode ~a" rm)])

(define (round->fptaylor expr ctx #:scale [scale 1])
  (define prec (ctx-lookup-prop ctx ':precision))
  (define rm (rm->fptaylor (ctx-lookup-prop ctx ':round)))
  (define bits
    (match prec
     ['undefined "undefined"]
     ['real ""]
     ['binary16 "16"]
     ['binary32 "32"]
     ['binary64 "64"]
     ['binary128 "128"]
     [_ (error 'round->fptaylor "Unsupported precision ~a" prec)]))
  (cond
   [(equal? bits "undefined") (format "rnd(~a)" (trim-infix-parens expr))]
   [(equal? bits "") expr]
   [(and (equal? rm "ne") (= scale 1)) (format "rnd~a(~a)" bits (trim-infix-parens expr))]
   [else (format "rnd[~a,~a,~a](~a)" bits rm scale (trim-infix-parens expr))]))

(define (operator->fptaylor op args ctx)
  (match (cons op args)
   [(list 'fabs a) (round->fptaylor (format "abs(~a)" a) ctx)]
   [(list 'fmax a b) (round->fptaylor (format "max(~a, ~a)" a b) ctx)]
   [(list 'fmin a b) (round->fptaylor (format "min(~a, ~a)" a b) ctx)]
   [(list 'fma a b c) (round->fptaylor (format "((~a * ~a) + ~a)" a b c) ctx)]
   [(list (? inexact-operator? f) args ...)
    (round->fptaylor (format "~a(~a)" f (string-join args ", ")) ctx #:scale (*fptaylor-inexact-scale*))]
   [(list (? operator? f) args ...)
    (round->fptaylor (format "~a(~a)" f (string-join args ", ")) ctx)]))

(define constant->expr
  (match-lambda
    ['E "exp(1)"]
    ['LN2 "log(2)"]
    ['LN10 "log(10)"]
    ['PI "4 * atan(1)"]
    ['PI_2 "2 * atan(1)"]
    ['PI_4 "atan(1)"]
    ['M_1_PI "1 / (4 * atan(1))"]
    ['M_2_PI "1 / (2 * atan(1))"]
    ['M_2_SQRTPI "1 / sqrt(atan(1))"]
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "1 / sqrt(2)"]
    [(? hex? expr) (format "~a" expr)]
    [(? number? expr) (format-number expr)]
    [c (error 'constant->expr "Unsupported constant ~a" c)]))

(define (constant->fptaylor expr ctx)
  (define cexpr (constant->expr expr))
  (round->fptaylor cexpr ctx))

(define (program->fptaylor name args arg-ctxs body return ctx vars)
  (define expr-name
    (let ([name* (ctx-lookup-prop ctx ':name #f)])
      (if name* (let-values ([(_ name) (ctx-unique-name ctx name*)]) name) name)))
  (define pre ((compose canonicalize remove-let)
                (ctx-lookup-prop ctx ':pre 'TRUE)))
  (define var-ranges 
    (make-immutable-hash 
      (dict-map (condition->range-table pre) 
                (lambda (var range) (cons (ctx-lookup-name ctx var) range)))))
  (define arg-strings
    (for/list ([arg args] [ctx arg-ctxs])
      (define range (dict-ref var-ranges arg (make-interval -inf.0 +inf.0)))
      (unless (nonempty-bounded? range)
        (error 'fptaylor->function "Bad range for ~a in ~a (~a)" arg name range))
      (unless (= (length range) 1)
        (print range)
        (error 'fptaylor->function "FPTaylor only accepts one sampling range"))
      (match-define (interval l u l? u?) (car range))
      (define prec (ctx-lookup-prop ctx ':precision))
      (format "\t~a ~a in [~a, ~a];" (prec->fptaylor prec) arg
        (format-number l) (format-number u))))

  (define var-string
    (if (null? arg-strings) 
        ""
        (format "Variables\n~a\n\n" (string-join arg-strings "\n"))))

  (define def-string
    (if (non-empty-string? body)
        (format "Definitions\n~a\n" body)
        ""))

  ; TODO: constraints
  (format "{\n~a~aExpressions\n\t~a = ~a;\n}"
    var-string def-string expr-name return))

; Override visitor behavior
(define-expr-visitor imperative-visitor fptaylor-visitor
  [(visit-! vtor props body #:ctx ctx)
    (visit/ctx vtor body (ctx-update-props ctx props))])

; ignore 'declaration' and type' since they are never used
(define core->fptaylor
  (make-imperative-compiler "fptaylor"
    #:operator operator->fptaylor
    #:constant constant->fptaylor
    #:round round->fptaylor
    #:program program->fptaylor
    #:flags '(round-after-operation
              never-declare)
    #:reserved fptaylor-reserved
    #:visitor fptaylor-visitor))

(define-compiler '("fptaylor" "fpt") (const "")  core->fptaylor (const "") fptaylor-supported)

;;; Legacy command line

(module+ main
  (require racket/cmdline)
  (define files (make-parameter #f))
  (define files-all (make-parameter #f))
  (define auto-file-names (make-parameter #f))
  (define out-path (make-parameter "."))
  (define precision (make-parameter #f))
  (define var-precision (make-parameter #f))
  (define split-or (make-parameter #f))
  (define subexprs (make-parameter #f))
  (define split (make-parameter #f))
  (define unroll (make-parameter #f))

  (command-line
   #:program "core2fptaylor.rkt"
   #:once-each
   ["--files" "Save FPTaylor tasks corresponding to different FPBench expressions in separate files"
              (files #t)]
   ["--files-all" "Save all FPTaylor tasks in separate files"
                  (files-all #t)]
   ["--auto-file-names" "Generate special names for all files"
                        (auto-file-names #t)]
   ["--out-path" path "All files are saved in the given path"
                 (out-path path)]
   ["--precision" prec "The precision of all operations (overrides the :precision property)"
                  (precision (string->symbol prec))]
   ["--var-precision" prec "The precision of input variables (overrides the :var-precision property)"
                      (var-precision (string->symbol prec))]
   ["--scale" scale "The scale factor for operations which are not correctly rounded"
              (*fptaylor-inexact-scale* (string->number scale))]
   ["--split-or" "Convert preconditions to DNF and create separate FPTaylor tasks for all conjunctions"
                 (split-or #t)]
   ["--subexprs" "Create FPTaylor tasks for all subexpressions"
                 (subexprs #t)]
   ["--split" n "Split intervals of bounded variables into the given number of parts"
              (split (string->number n))]
   ["--unroll" n "How many iterations to unroll any loops to"
               (unroll (string->number n))]
   #:args ([input-file #f])
   ((if input-file
        (curry call-with-input-file input-file)
        (λ (proc) (proc (current-input-port))))
    (λ (port)
      (port-count-lines! port)
      (for ([prog (in-port (curry read-fpcore "input") port)] [n (in-naturals)])
        ;;; (with-handlers ([exn:fail? (λ (exn) (eprintf "[ERROR]: ~a\n\n" exn))])
          (define def-name (format "ex~a" n))
          (define prog-name (if (auto-file-names) def-name (fpcore-name prog def-name)))
          (define override-props (if (precision) `(:precision ,(precision)) null))
          (define progs (fpcore-transform prog
                                          #:var-precision (var-precision)
                                          #:override-props override-props
                                          #:unroll (unroll)
                                          #:split (split)
                                          #:subexprs (subexprs)
                                          #:split-or (split-or)))
          (define results (map (curryr core->fptaylor def-name) progs))
          (define multiple-results (> (length results) 1))
          (cond
            [(files-all)
             (for ([r results] [k (in-naturals)])
               (define fname (fix-file-name
                              (string-append prog-name
                                             (if multiple-results (format "_case~a" k) "")
                                             ".txt")))
               (call-with-output-file (build-path (out-path) fname) #:exists 'replace
                 (λ (p) (fprintf p "~a" r))))]
            [(files)
             (define fname (fix-file-name (format "~a.txt" prog-name)))
             (call-with-output-file (build-path (out-path) fname) #:exists 'replace
               (λ (p) (for ([r results])
                        (if multiple-results (fprintf p "~a\n" r) (fprintf p "~a" r)))))]
            [else (for ([r results]) (printf "~a\n" r))])
          )))
  ))
