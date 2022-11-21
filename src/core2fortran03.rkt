#lang racket

(require generic-flonum)
(require "imperative.rkt")

(provide core->fortran type->fortran fortran-supported)

(define fortran-supported 
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(acosh asinh atanh cbrt ceil copysign erf erfc exp2 expm1 floor fma
          hypot isfinite isinf isnormal lgamma log1p log2 nearbyint remainder
          signbit tgamma trunc
          array dim size ref for for* tensor tensor*)))
    (curry set-member? '(TRUE FALSE))
    (curry set-member? '(binary32 binary64 integer))
    (curry equal? 'nearestEven)
    #f))

(define fortran-reserved    ; Language-specific reserved names (avoid name collisions)
  '(abstract allocatable allocate assign associate asynchronous backspace bind
    block call case class close common contains continue cycle data deallocate
    deferred dimension do else elemental elewhere end endfile endif entry enum
    enumerator equivalence exit extends external final flush forall fromat function
    generic goto if implicit import include inquire intent interface intrisic module
    namelist non_overridable nopass nullify only open operator optional parameter pass
    pause pointer print private procedure program protected public pure read recursive
    result return rewing rewrite save select sequence stop subroutine target then use
    value volatile wait while where write))

(define (fortran-fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string (char-downcase char))
         (format "_~a" (char->integer char))))
   ""))

(define/match (type->fortran type)
  [('binary64) "real(8)"]
  [('binary32) "real(4)"]
  [('boolean) "logical"]
  [('integer) "integer(8)"])

(define (number->fortran x type)
  (define (append-exponent x)
    (if (string-contains? x "e")
        (match type
         ['binary64 (string-replace x "e" "d")]
         [_ x])
        (match type
         ['binary64  (string-append x "d0")]
         [_ (string-append x "e0")])))
  (if (negative? x)
      (format "(-~a)" (append-exponent (~a (abs (real->double-flonum x)))))
      (append-exponent (~a (real->double-flonum x)))))

(define (constant->fortran x ctx)
  (match x
   ['TRUE ".true."]
   ['FALSE ".false."]
   [(? hex?) (number->fortran (hex->racket x) (ctx-lookup-prop ctx ':precision))]
   [(? number?) (number->fortran x (ctx-lookup-prop ctx ':precision))]
   [(? symbol?) (~a x)]))

(define (operator->fortran op args ctx)
  (define type (type->fortran (ctx-lookup-prop ctx ':precision)))
  (match (cons op args)
   [(list 'not a) (format "(.not. ~a)" a)]
   [(list (or '== '!= '< '> '<= '>=)) ".true."]
   [(list (or '== '< '> '<= '>=) arg args ...)
    (format "(~a)"
            (string-join
              (for/list ([a (cons arg args)] [b args])
                (format "~a ~a ~a" a op b))
              " .and. "))]
   [(list '!= args ...)
    (format "(~a)"
            (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append (for/list ([b (cdr args)])
                              (format "~a /= ~a" (car args) b))
                            (loop (cdr args)))))
              " .and. "))]
   [(list 'and a ...) (string-join args " .and. ")]
   [(list 'or a ...) (string-join args " .or. ")]
   [(list 'isnan a) (format "(~a /= ~a)" a a)]
   [(list 'fabs a) (format "abs(~a)" a)]
   [(list 'fdim a b) (format "dim(~a, ~a)" a b)]
   [(list 'fmax a b) (format "merge(~a, merge(~a, max(~a, ~a), ~a /= ~a), ~a /= ~a)"
                             b a a b b b a a)]
   [(list 'fmin a b) (format "merge(~a, merge(~a, min(~a, ~a), ~a /= ~a), ~a /= ~a)"
                              b a a b b b a a)]
   [(list 'fmod a b) (format "mod(~a, ~a)" a b)]
   [(list 'pow a b) (format "(~a ** ~a)" a b)]
   [(list 'round a) (format "anint(~a)" a)]
   [_ (format "~a(~a)" op (string-join args ", "))]))

(define (assignment->fortran var val ctx)
  (format "~a = ~a" var val))

(define (round->fortran x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (format "real(~a, ~a)" x (match prec ['binary64 8] ['binary32 4])))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary80) 4]
    [('binary64) 3]
    [('binary32) 2]
    [('integer)  1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->fortran op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->fortran arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (program->fortran name args arg-ctxs body ret ctx used-vars)
  (define type (type->fortran (ctx-lookup-prop ctx ':precision)))
  (define declared-in (sort (remove* args used-vars) string<?))
  (format "~a function ~a(~a)\n~a~a~a    ~a = ~a\nend function\n" type name
          (string-join args ", ")
          (apply string-append
            (for/list ([arg args] [ctx arg-ctxs])
              (let ([type (type->fortran (ctx-lookup-prop ctx ':precision))])
                (format "    ~a, intent (in) :: ~a\n" type arg))))
          (apply string-append
            (for/list ([name (in-list declared-in)])
              (let ([type (type->fortran (ctx-lookup-prec ctx name))])
                (format "    ~a :: ~a\n" type name))))
          body name ret))

(define core->fortran
  (make-imperative-compiler "fortran03"
    #:infix-ops (remove* '(== != < > <= >= not and or) default-infix-ops)
    #:type type->fortran
    #:constant constant->fortran
    #:operator operator->fortran
    #:assign assignment->fortran
    #:round round->fortran
    #:implicit-round implicit-round->fortran
    #:program program->fortran
    #:flags '(spaces-for-tabs
              end-block-with-name
              never-declare
              if-then
              do-while)
    #:reserved fortran-reserved
    #:fix-name fortran-fix-name
    #:indent "    "))

(define-compiler '("f03") (const "") core->fortran (const "") fortran-supported)
