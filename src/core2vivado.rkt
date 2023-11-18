#lang racket

(require "imperative.rkt")

(provide vivado-header core->vivado vivado-supported)

(define vivado-header
  (const "#include <ap_fixed.h>\n#include <hls_half.h>\n#include <hls_math.h>\n\n"))

(define vivado-supported
  (supported-list
    (invert-op-proc
      (curry set-member? '(log2 tgamma lgamma array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (match-lambda
      [(or 'binary16 'binary32 'binary64 (list 'fixed _ _)) #t]
      [_ #f])
    (curry equal? 'toNegative)
    #f))

(define vivado-reserved
  '(alignas alignof and and_eq asm atomic_cancel atomic_commit atomic_noexcept auto bitand bitor bool
    break case catch char char8_t char16_t char32_t class compl concept const consteval constexpr
    constinit const_cast continue co_await co_return co_yield decltype default delete do double
    dynamic_cast else enum explicit export extern false float for friend goto if inline int long
    mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public
    reflexpr register reinterpret_cast requires return short signed sizeof static static_assert
    static_cast struct switch synchronized template this thread_local throw true try typedef typeid
    typename union unsigned using virtual void volatile wchar_t while xor xor_eq))

(define/match (type->vivado type)
  [('binary16) "half"]
  [('binary32) "float"]
  [('binary64) "double"]
  [('boolean) "bool"]
  [((list 'fixed scale nbits)) (format "ap_fixed<~a, ~a>" nbits (+ nbits scale))])

(define (operator->vivado op args ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define expr (format "hls::~a(~a)" op (string-join (map ~a args) ", ")))
  (match prec
    [(list 'fixed _ _) (round->vivado expr ctx)]
    [_ expr]))

(define/match (vivado-type->suffix type)
  [("half") "_h"]
  [("float") "f"]
  [("double") ""])

(define (constant->vivado x ctx)
  (define type (type->vivado (ctx-lookup-prop ctx ':precision)))
  (match x
    ['TRUE "true"]
    ['FALSE "false"]
    [(or (? number?) (? hex?))
      (define val (real->double-flonum (if (hex? x) (hex->racket x) x)))
      (match type
        [(or "float" "double") (format "~a~a" val (vivado-type->suffix type))]
        [_ (format "~a{~a}" type val)])]
    [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN) (format "~a{~a}" type x)]
    [(? symbol?) (format "~a{M_~a}" type x)]))

(define (round->vivado x ctx)
  (define type (type->vivado (ctx-lookup-prop ctx ':precision)))
  (format "static_cast<~a>(~a)" type (trim-infix-parens x)))

(define (cmp-float-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 3]
    [('binary32) 2]
    [('binary16) 1]
    [(_)         0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->vivado op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(binary16 binary32 binary64) arg-prec)
      (if (> (cmp-float-prec prec arg-prec) 0)
          (round->vivado arg ctx)
          arg)
      arg))

(define (params->vivado args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->vivado (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg)))
    ", "))

(define (program->vivado name args arg-ctxs body ret ctx used-vars)
  (define type (type->vivado (ctx-lookup-prop ctx ':precision)))
  (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
          type name (params->vivado args arg-ctxs) body (trim-infix-parens ret)))

(define core->vivado
  (make-imperative-compiler "vivado"
    #:operator operator->vivado
    #:constant constant->vivado
    #:type type->vivado
    #:round round->vivado
    #:implicit-round implicit-round->vivado
    #:program program->vivado
    #:reserved vivado-reserved
    #:flags '(round-after-operation)))

(define-compiler '("vivado") vivado-header core->vivado (const "") vivado-supported)

(define vivado-hpp-header (const "#include <ap_fixed.h>\n#include <hls_half.h>\n\n"))

(define (program->vivado-hpp name args arg-ctxs body ret ctx used-vars)
  (define type (type->vivado (ctx-lookup-prop ctx ':precision)))
  (format "~a ~a(~a);\n" type name (params->vivado args arg-ctxs)))

(define core->vivado-hpp
  (make-imperative-compiler "vivado-hpp"
    #:program program->vivado-hpp
    #:reserved vivado-reserved
    #:flags '(no-body)))

(define-compiler '("vivado-hpp") vivado-hpp-header core->vivado-hpp (const "") vivado-supported)
