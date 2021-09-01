#lang racket

(require "imperative.rkt" "core2c.rkt")

(provide core->matlab matlab-supported)

(define matlab-supported 
  (supported-list
    (invert-op-proc
      (curry set-member? '(fma expm1 log1p)))
    fpcore-consts
    (curry set-member? '(binary32 binary64))
    (curry equal? 'nearestEven)))

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
   [(list '!= args)
    (format "(~a)"
            (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                      (for/list ([b (cdr args)])
                        (format "~a != ~a" (car args) b))
                      (loop (cdr args)))))
              "&&"))]
   [(list '- a) (format "(uminus ~a)" a)]
   [(list '- a b) (format "(~a - ~a)" a b)]
   [(list 'fmod a b) (format "mod(~a, ~a)" a b)]
   [(list 'remainder a b) (format "rem(~a, ~a)" a b)]
   [_ (format "~a(~a)" op (string-join args ", "))]))

(define (constant->matlab x ctx)
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   ['E "2.71828182845904523536"]
   ['LOG2E "1.44269504088896340736"]
   ['LOG10E "0.434294481903251827651"]
   ['LN2	"0.693147180559945309417"]
   ['LN10	"2.30258509299404568402"]
   ['PI	"3.14159265358979323846"]
   ['PI_2	"1.57079632679489661923"]
   ['PI_4	"0.785398163397448309616"]
   ['M_1_PI	"0.318309886183790671538"]
   ['M_2_PI "0.636619772367581343076"]
   ['M_2_SQRTPI "1.12837916709551257390"]
   ['M_SQRT2 "1.41421356237309504880"]  
   ['M_SQRT1_2 "0.707106781186547524401"]
   ['INFINITY "Inf"]
   ['NAN "NaN"]
   [(? hex?) (~a (real->double-flonum (hex->racket x)))]
   [(? number?) (~a (real->double-flonum x))]
   [(? symbol?) (~a x)]))

(define declaration->matlab
  (case-lambda
   [(var ctx)
    (define prec (ctx-lookup-prop ctx ':precision))
    (format "~a = ~a;" var (constant->matlab (match prec ['boolean 'TRUE] [_ 0]) ctx))]
   [(var val ctx)
    (format "~a = ~a;" var val)]))

(define (program->matlab name args arg-ctxs body ret ctx used-vars)
  (define-values (_ ret-var) (ctx-random-name ctx))
  (format "function ~a = ~a(~a)\n~a\t~a = ~a;\nend\n"
          ret-var name (string-join args ", ")
          body ret-var ret))

(define core->matlab
  (make-imperative-compiler "matlab"
    #:infix-ops (remove* '(!= -) default-infix-ops)
    #:operator operator->matlab
    #:constant constant->matlab
    #:declare declaration->matlab
    #:flags '(end-block-with-end
              use-elseif)
    #:program program->matlab
    #:reserved matlab-reserved))

(define-compiler '("m" "mat") (const "") core->matlab (const "") matlab-supported)
          
