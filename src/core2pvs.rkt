#lang racket

(require "imperative.rkt"
         "fpcore-extra.rkt"
         "range-analysis.rkt")

(provide core->pvs
         pvs-supported)

(define let-bind-format "~a = ~a")
(define let-format "LET ~a IN\n~a")
(define let-bind-separator ",\n")
(define if-format "IF ~a THEN ~a ELSE ~a ENDIF")

(define pvs-supported
  (supported-list (invert-op-proc (curry set-member?
                                         '(remainder atan2
                                                     fdim
                                                     copysign
                                                     erf
                                                     erfc
                                                     tgamma
                                                     lgamma
                                                     while
                                                     while*
                                                     digits
                                                     floor
                                                     trunc
                                                     round
                                                     nearbyint
                                                     cast
                                                     ceil
                                                     isfinite
                                                     isinf
                                                     isnan
                                                     isnormal
                                                     signbit
                                                     dim
                                                     size
                                                     ref
                                                     array
                                                     !)))
                  (invert-const-proc (curry set-member? '(INFINITY NAN)))
                  (curry set-member? '(binary64))
                  (curry equal? 'nearestEven)
                  #f))

;; Language-specific reserved names (avoid name collisions)
(define pvs-reserved
  '(f THEORY
      BEGIN
      END
      real
      OR
      AND
      =
      NOT
      +
      -
      *
      /
      <
      >
      <=
      >=
      ^
      %
      exp
      sqrt
      sin
      cos
      tan
      acos
      asin
      atan
      abs
      ln
      TRUE
      FALSE
      LET
      IN
      IF
      THEN
      ELSE
      ENDIF
      IMPORTING
      LEMMA
      IMPLIES
      FORALL))

;; Rewrites due to operations not being implemented in ReFLOW
(define (rewrite2pvs op args)
  (match (cons op args)
    [(list '!= arg1 arg2) `(not (== ,arg1 ,arg2))]
    [(list 'fma arg1 arg2 arg3) `(+ (* ,arg1 ,arg2) ,arg3)]
    [(list 'exp2 arg) `(pow 2 ,arg)]
    [(list 'expm1 arg) `(- (exp ,arg) 1)]
    [(list 'cbrt arg) `(pow ,arg (/ 1 3))]
    [(list 'hypot arg1 arg2) `(sqrt (+ (pow ,arg1 2) (pow ,arg2 2)))]
    [(list 'log10 arg) `(/ (log ,arg) (log 10))]
    [(list 'log2 arg) `(/ (log ,arg) (log 2))]
    [(list 'log1p arg) `(log (+ ,arg 1))]
    [(list 'sinh arg) `(* (/ 1 2) (+ (exp ,arg) (/ (- 1) (exp ,arg))))]
    [(list 'cosh arg) `(* (/ 1 2) (+ (exp ,arg) (/ 1 (exp ,arg))))]
    [(list 'tanh arg) `(/ (+ (exp ,arg) (- (/ 1 (exp ,arg)))) (+ (exp ,arg) (/ 1 (exp ,arg))))]
    [(list 'asinh arg) `(log (+ ,arg (sqrt (+ (pow ,arg 2) 1))))]
    [(list 'acosh arg) `(log (+ ,arg (sqrt (- (pow ,arg 2) 1))))]
    [(list 'atanh arg) `(* (/ 1 2) (log (/ (+ 1 ,arg) (+ 1 (- ,arg)))))]
    [(list 'fmin arg1 arg2) `(if (< ,arg1 ,arg2) ,arg1 ,arg2)]
    [(list 'fmax arg1 arg2) `(if (> ,arg1 ,arg2) ,arg1 ,arg2)]
    [_ #f]))

;; Basic operations implemented in ReFLOW
(define (operator->pvs op args ctx)
  (match op
    ['or (format "(~a OR ~a)" (car args) (cadr args))]
    ['and (format "(~a AND ~a)" (car args) (cadr args))]
    ['== (format "(~a = ~a)" (car args) (cadr args))]
    ['!= (format "(NOT(~a = ~a))" (car args) (cadr args))]
    ['-
     #:when (equal? 1 (length args))
     (format "(- ~a)" (car args))]
    [(or '+ '- '* '/ '< '> '<= '>=) (format "(~a ~a ~a)" (car args) op (cadr args))]
    ['pow (format "(~a ^ ~a)" (car args) (cadr args))]
    ['fmod (format "(~a % ~a)" (car args) (cadr args))]
    [(or 'exp 'sqrt 'sin 'cos 'tan 'acos 'asin 'atan) (format "(~a(~a))" op (car args))]
    ['not (format "(NOT(~a))" (car args))]
    ['fabs (format "(abs(~a))" (car args))]
    ['log (format "(ln(~a))" (car args))]
    [_ (error 'operator->pvs "parsing for ~a operator is not implemented in core2pvs" op)]))

(define (constant->pvs x ctx)
  (match x
    [(or 'TRUE 'FALSE) (~a x)]
    ['E "exp(1)"]
    ['LOG2E "(ln(exp(1)) / ln(2))"]
    ['LOG10E "(ln(exp(1)) / ln(10))"]
    ['LN2 "(ln(2))"]
    ['LN10 "(ln(10))"]
    ['PI "(4 * atan(1))"]
    ['PI_2 "(2 * atan(1))"]
    ['PI_4 "(atan(1))"]
    ['M_1_PI "(1 / (4 * atan(1)))"]
    ['M_2_PI "(1 / (2 * atan(1)))"]
    ['M_2_SQRTPI "(1 / sqrt(atan(1)))"]
    ['SQRT2 "(sqrt(2))"]
    ['SQRT1_2 "(1 / sqrt(2))"]
    [(? number?) (format-number x)]
    [_ (error 'constant->pvs "parsing for ~a constant is not implemented in core2pvs" x)]))

; Override visitor behavior
(define-expr-visitor
 imperative-visitor
 pvs-visitor
 ;; Currying anything outside of if-statement into let-body
 ;; to allow nesting over if-statement
 [(visit-if vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (ctx* tmpvar) ; allocate a variable
    (parameterize ([current-output-port (open-output-nowhere)])
      (define-values (_ ift-ctx) (visit/ctx vtor ift ctx))
      (define prec (ctx-lookup-prop ift-ctx ':precision))
      (ctx-random-name (ctx-update-props ctx `(:precision ,prec)))))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx))
  ; tabulations
  (printf "~a~a"
          indent
          (format let-format (format let-bind-format tmpvar (format if-format cond* ift* iff*)) ""))
  (define ctx** (ctx-set-extra ctx 'indent (format "~a~a" indent "\t")))
  (values tmpvar ctx**)]
 ;; Currying anything outside of let-statement into let-body
 ;; to allow nesting over let-statement
 [(visit-let vtor vars vals body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (ctx* vars* vals*)
    (for/fold ([ctx* ctx]
               [vars* '()]
               [vals* '()]
               #:result (values ctx* (reverse vars*) (reverse vals*)))
              ([var (in-list vars)]
               [val (in-list vals)])
      (define-values (val* val-ctx) (visit/ctx vtor val ctx))
      (define prec (ctx-lookup-prop val-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (values name-ctx (cons name vars*) (cons val* vals*))))
  (printf "~a~a"
          indent
          (format let-format
                  (string-join (map (curry format let-bind-format) vars* vals*)
                               (string-append let-bind-separator indent "    "))
                  ""))
  (define ctx** (ctx-set-extra ctx* 'indent (format "~a~a" indent "\t")))
  (define-values (body* body-ctx) (visit/ctx vtor body ctx**))
  (values body* body-ctx)]
 [(visit-let* vtor vars vals body #:ctx ctx)
  (visit/ctx vtor
             (let loop ([vars vars]
                        [vals vals])
               (cond
                 [(null? vars) body]
                 [else `(let (,(list (car vars) (car vals))) ,(loop (cdr vars) (cdr vals)))]))
             ctx)]
 [(visit-op vtor op args #:ctx ctx)
  ;; Try to find rewrites for operators that are not supported in ReFLOW
  (match (rewrite2pvs op args)
    [(? list? rewrite) (visit/ctx vtor rewrite ctx)]
    [#f
     (define args*
       (for/list ([arg args])
         (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
         arg*))
     (values (operator->pvs op args* ctx)
             (if (set-member? bool-ops op)
                 (ctx-update-props ctx (list ':precision 'boolean))
                 ctx))])])

(define (pre->pvs-input name args arg-ctxs ctx)
  (define pre ((compose canonicalize remove-let) (ctx-lookup-prop ctx ':pre 'TRUE)))
  (define var-ranges
    (make-immutable-hash (dict-map (condition->range-table pre)
                                   (lambda (var range) (cons (ctx-lookup-name ctx var) range)))))
  (define arg-strings
    (for/list ([arg args]
               [ctx arg-ctxs])
      ;; not sure what to do here for now
      (define range (dict-ref var-ranges arg (list (make-interval -123.0 123.0))))
      (unless (nonempty-bounded? range)
        (set! range (list (make-interval -123.0 123.0))) ;; purely for debugging
        #;(error 'pre->pvs-input "Bad range for ~a in ~a (~a)" arg name range))
      (unless (= (length range) 1)
        (set! range (list (make-interval -123.0 123.0))) ;; purely for debugging
        #;(error 'pre->pvs-input "ReFLOW only accepts one sampling range, not ~a" (length range)))

      (match-define (interval l u l? u?) (car range))
      (format "\t~a in [~a, ~a]" arg (format-number l) (format-number u))))
  (format "f(~a):\n~a" (string-join args ", ") (string-join arg-strings ",\n")))

(define (params->pvs args)
  (format "~a: real" (string-join args ", ")))

(define (program->pvs name args arg-ctxs body ret ctx used-vars)
  (define pvs-input (pre->pvs-input name args arg-ctxs ctx))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define pvs-program
    (format "~a: THEORY\nBEGIN\nf(~a): real =\n~a~a~a\nEND ~a"
            name
            (params->pvs args)
            body
            indent
            ret
            name))
  (values pvs-input pvs-program))

(define core->pvs
  (make-imperative-compiler "pvs"
                            #:operator operator->pvs
                            #:constant constant->pvs
                            #:type (const "") ;; not supported in ReFLOW input
                            #:round ~a ;; not supported in ReFLOW input
                            #:implicit-round ~a ;; not supported in ReFLOW input
                            #:round-mode ~a ;; not supported in ReFLOW input
                            #:program program->pvs
                            #:flags '()
                            #:reserved pvs-reserved
                            #:visitor pvs-visitor))

(define-compiler '(PVS) (const "") core->pvs (const "") pvs-supported)

(module+ test
  (require rackunit)
  (define compile0 core->pvs)
  (define (compile* . exprs)
    (for ([expr exprs]
          [i (in-naturals 1)])
      (define-values (input prog) (compile0 expr (format "fn~a" i)))
      (printf "~a\n~a\n\n" input prog)))
  #;'(FPCore (x)
             (let ([x 1]
                   [y x])
               (+ x y))) ; let check
  #;'(FPCore (x)
             (let* ([x 1]
                    [y x])
               (+ x y))) ; let* check
  #;'(FPCore (x)
             (hypot 1
                    (let ([x 1]
                          [y x])
                      (+ x y)))) ; nested let check
  #;'(FPCore (x)
             (+ 1
                (let* ([x 1]
                       [y x])
                  (+ x y)))) ; nested let* check
  #;'(FPCore (x) (fmin x 1)) ; fmin implemented as if statement
  #;'(FPCore (x) (* (fmax x 1) 1)) ; nested fmax check
  #;'(FPCore (x eps)
             :name
             "2sin (example 3.3)"
             :pre
             (and (<= -1e4 x) (<= x 1e4) (< (* 1e-16 (fabs x)) eps) (< eps (fabs x)))
             (- (sin (+ x eps)) (sin x)))
  #;'(FPCore (x) (while* (< x 4) ([x 0.0 (+ x 1.0)]) x))
  (compile* '(FPCore (radius theta)
                     :name
                     "polarToCarthesian, x"
                     :pre
                     (and (<= 1 radius 10) (<= 0 theta 360))
                     :spec
                     (* radius (cos (* theta (/ 180 PI))))
                     (let* ([pi 3.14159265359]
                            [radiant (* theta (/ pi 180.0))])
                       (* radius (cos radiant))))
            #;'(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))
            #;'(FPCore (a b) (+ (* a b) (- a b)))))
