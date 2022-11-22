#lang racket

(require "imperative.rkt" "core2c.rkt")

(provide core->matlab matlab-supported)

(define matlab-supported 
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(cbrt exp2 expm1 fdim fma fmod isnormal log1p
          nearbyint remainder signbit
          array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64))
    (curry equal? 'nearestEven)
    #f))

(define cpp-reserved
  '(catch class const_cast delete dynamic_cast explicit export friend
    inline mutable namespace new operator private protected public
    reinterpret_cast static_cast template this throw try typeid typename
    using virtual wchar_t))

(define codegen-reserved
  '(abs asm bool boolean_T byte_T char_T cint8_T cint16_T cint32_T creal_T
    creal32_T creal64_T cuint8_T cuint16_T cuint32_T ERT false fortran HAVESTDIO
    id_t int_T int8_T int16_T int32_T INTEGER_CODE LINK_DATA_BUFFER_SIZE
    LINK_DATA_STREAM localB localC localDWork localP localX localXdis localXdot
    localZCE loalZCSV matrix MAX_int8_T* MAX_int16_T* MAX_int32_T* MAX_int64_T*
    MAX_uint8_T* MAX_uin16_T* MAX_uint32_T* MAX_uint64_T* MIN_int8_T*
    MIN_int16_T* MIN_int32_T* MIN_int64_T* MODEL MT NCSTATES NULL NUMST pointer_T
    PROFILING_ENABLED PROFILING_NUM_SPACES real_T real32_T real64_T RT RT_MALLOC
    rtInf rtMinusInf rtNaN SeedFileBuffer SeedFileBufferLen single TID01EQ time_T
    true uint_T uint8_T uint16_T uint32_T uint64_T UNUSED_PARAMETER USE_RT_MODEL
    VCAST_FLUSH_DATA vector))

(define matlab-reserved ; Language-specific reserved names (avoid name collisions)
  (append c-reserved cpp-reserved codegen-reserved))

(define (operator->matlab op args ctx)
  (match (cons op args)
   [(list 'not a) (format "~~(~a)" a)]
   [(list '!= args ...)
    (format "(~a)"
            (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                      (for/list ([b (cdr args)])
                        (format "~a ~~= ~a" (car args) b))
                      (loop (cdr args)))))
              "&&"))]
   [(list 'copysign a b) (format "(sign(~a) * abs(~a))" b a)]
   [(list 'fabs a) (format "abs(~a)" a)]
   [(list 'fmax a b) (format "max(~a, ~a)" a b)]
   [(list 'fmin a b) (format "min(~a, ~a)" a b)]
   [(list 'fmod a b) (format "rem(~a, ~a)" a b)]
   [(list 'lgamma a) (format "gammaln(~a)" a)]
   [(list 'pow a b) (format "(~a ^ ~a)" a b)]
   [(list 'remainder a b) (format "rem(~a, ~a)" a b)]
   [(list 'tgamma a) (format "gamma(~a)" a)]
   [(list 'trunc a) (format "fix(~a)" a)]
   [_ (format "~a(~a)" op (string-join args ", "))]))

(define (number->matlab x ctx)
  (match x
   ['E "2.71828182845904523536"]
   ['LOG2E "1.44269504088896340736"]
   ['LOG10E "0.434294481903251827651"]
   ['LN2	"0.693147180559945309417"]
   ['LN10	"2.30258509299404568402"]
   ['PI	"pi"]
   ['PI_2	"1.57079632679489661923"]
   ['PI_4	"0.785398163397448309616"]
   ['M_1_PI	"0.318309886183790671538"]
   ['M_2_PI "0.636619772367581343076"]
   ['M_2_SQRTPI "1.12837916709551257390"]
   ['SQRT2 "1.41421356237309504880"]  
   ['SQRT1_2 "0.707106781186547524401"]
   ['INFINITY "Inf"]
   ['NAN "NaN"]
   [(? hex?) (~a (real->double-flonum (hex->racket x)))]
   [(? number?) (~a (real->double-flonum x))]
   [(? symbol?) (~a x)]))

(define (constant->matlab x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define ->num
    (match prec
     ['binary32 (curry format "single(~a)")]
     [_ identity]))
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   [_ (->num (number->matlab x ctx))]))

(define declaration->matlab
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a;" var (constant->matlab (match prec ['boolean 'TRUE] [_ 0]) ctx))]
   [(var val ctx)
    (format "~a = ~a;" var val)]))

(define (round->matlab x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define cast
    (match prec
     ['binary64 (curry format "double(~a)")]
     ['binary32 (curry format "single(~a)")]
     [_ identity]))
  (cast x))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 2]
    [('binary32) 1]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

; handles matlab's weird casting behavior for binary operators
(define (operator->matlab* op args arg-prec ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define ->single (curry format "single(~a)"))
  (define ->double (curry format "double(~a)"))
  (define-values (fa fb fo) ; formatters
    (match (list (first arg-prec) (second arg-prec) prec)
     [(list 'binary32 'binary32 'binary32) ; f32 f32 -> f32
      (values identity identity identity)]
     [(list 'binary32 'binary32 'binary64) ; f32 f32 -> f64
      (values ->double ->double identity)]
     [(list 'binary64 'binary32 'binary32) ; f64 f32 -> f32
      (values identity ->double ->single)]
     [(list 'binary64 'binary32 'binary64) ; f64 f32 -> f64
      (values identity ->double identity)]
     [(list 'binary32 'binary64 'binary32) ; f32 f64 -> f32
      (values ->double identity ->single)]
     [(list 'binary32 'binary64 'binary64) ; f32 f64 -> f64
      (values ->double identity identity)]
     [(list 'binary64 'binary64 'binary32) ; f64 f64 -> f32
      (values identity identity ->single)]
     [(list 'binary64 'binary64 'binary64) ; f64 f64 -> f64
      (values identity identity identity)]))
  (fo (compile-operator op (list (fa (first args)) (fb (second args))) ctx)))

(define (program->matlab name args arg-ctxs body ret ctx used-vars)
  (define-values (_ ret-var) (ctx-random-name ctx))
  (format "function ~a = ~a(~a)\n~a\t~a = ~a;\nend\n"
          ret-var name (string-join args ", ")
          body ret-var ret))

; override operators since Matlab will cast down to lowest precision
(define-expr-visitor imperative-visitor matlab-visitor
  [(visit-op vtor op args #:ctx ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (define-values (args* arg-precs)
      (for/lists (l1 l2) ([arg args])
        (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
        (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
        (values arg* arg-prec)))
    (define bool? (set-member? bool-ops op))
    (values (if (and (= (length args) 2) (not bool?))
                (operator->matlab* op args* arg-precs ctx)
                (compile-operator op args* ctx))
            (if bool?
                (ctx-update-props ctx (list ':precision 'boolean))
                ctx))])

(define core->matlab
  (make-imperative-compiler "matlab"
    #:infix-ops (remove* '(not !=) default-infix-ops)
    #:operator operator->matlab
    #:constant constant->matlab
    #:declare declaration->matlab
    #:round round->matlab
    #:program program->matlab
    #:flags '(end-block-with-end
              use-elseif)
    #:visitor matlab-visitor
    #:reserved matlab-reserved))

(define-compiler '("m" "mat") (const "") core->matlab (const "") matlab-supported)
          
