#lang racket

(require generic-flonum)
(require "imperative.rkt")

(provide core->fortran fortran-supported)

(define fortran-supported 
  (supported-list
    fpcore-ops
    fpcore-consts
    (curry set-member? '(binary32 binary64 integer))
    (curry equal? 'nearestEven)))

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

(define/match (type->fortran type)
  [('binary64) "real(8)"]
  [('binary32) "real(4)"]
  [('boolean) "integer"]
  [('integer) "integer(8)"])

(define (assignment->fortran var val ctx)
  (format "~a = ~a" var val))

(define (program->fortran name args arg-ctxs body ret ctx used-vars)
  (define type (type->fortran (ctx-lookup-prop ctx ':precision)))
  (format "~a function ~a(~a)\n~a~a~a    ~a = ~a\nend function\n" type name
          (string-join args ", ")
          (apply string-append
            (for/list ([arg args] [ctx arg-ctxs])
              (let ([type (type->fortran (ctx-lookup-prop ctx ':precision))])
                (format "    ~a, intent (in) :: ~a\n" type arg))))
          (apply string-append
            (for/list ([(name prec) (in-dict used-vars)])
              (format "    ~a :: ~a\n" type name)))
          body name ret))

(define core->fortran
  (make-imperative-compiler "fortran03"
    #:type type->fortran
    #:assign assignment->fortran
    #:program program->fortran
    #:flags '(spaces-for-tabs
              end-block-with-name
              never-declare
              if-then
              do-while)
  ;  #:visitor fortran-visitor
    #:reserved fortran-reserved
    #:indent "    "))

(define-compiler '("f03") (const "") core->fortran (const "") fortran-supported)
