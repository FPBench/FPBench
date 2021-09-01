;
;   Common compiler for Lisp-like or Lisp-adjacent languages
;     S-expression: SMT
;     M-expression: Mathematica
;

#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "fpcore-extra.rkt"
         "supported.rkt")

(provide (all-from-out "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")
         make-lisp-compiler lisp-visitor)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *lisp-lang* (make-parameter #f))

(struct lisp
  (name               ; string representation of language
   operator           ; procedure to format any non-infix operator
   constant           ; procedure to format constants
   if-format          ; format string for if expressions
   let-format         ; pair containing format string for let expressions and string seperator
   let-bind-format    ; format string for a single binding
   round              ; procedure to format (explicit) casts
   program            ; procedure to format the entire program
   flags))            ; list of optional flags to change minor behavior

;;;;;;;;;;;;;;;;;;;;;;;;;;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-flags 
  '(round-after-operation))   ; ensure rounding after any operation (SMT)

(define (valid-flag? maybe-flag)
  (set-member? valid-flags maybe-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-flag-raised? flag)
  (set-member? (lisp-flags (*lisp-lang*)) flag))

(define (compile-after-op x ctx)
  (if (compile-flag-raised? 'round-after-operation)
      (compile-round x ctx)
      x))

(define (compile-operator op args ctx)
  ((lisp-operator (*lisp-lang*)) op args ctx))

(define (compile-constant x ctx)
  ((lisp-constant (*lisp-lang*)) x ctx))

(define (compile-program name args arg-ctxs body ctx)
  ((lisp-program (*lisp-lang*)) name args arg-ctxs body ctx))

(define (compile-round expr ctx)
  ((lisp-round (*lisp-lang*)) expr ctx))

(define (if-format)
  (lisp-if-format (*lisp-lang*)))

(define (let-format)
  (lisp-let-format (*lisp-lang*)))

(define (let-bind-format)
  (car (lisp-let-bind-format (*lisp-lang*))))

(define (let-bind-seperator)
  (cdr (lisp-let-bind-format (*lisp-lang*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-compile-operator op args ctx)
  (format "(~a)" (string-join (map ~a (cons op args)) " ")))

(define (default-compile-constant x ctx)
  (~a x))

(define (default-compile-program name args arg-ctxs body ctx)
  (format "(defun ~a (~a)\n ~a)\n" name
         (string-join args " ") body))

(define (default-compile-round expr ctx)
  (~a expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-ctx
  (ctx-update-props
    (make-compiler-ctx)
    '(:precision binary64 :round nearestEven)))

(define bool-ops '(< > <= >= == != and or not
                   isfinite isinf isnan isnormal signbit))

;;;;;;;;;;;;;;;;;;;;;;;;;;; visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-expr-visitor default-compiler-visitor lisp-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
    (define-values (cond* _) (visit/ctx vtor cond ctx))
    (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
    (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx))
    (values (format (if-format) cond* ift* iff*) ift-ctx)]

  [(visit-let vtor vars vals body #:ctx ctx)
    (define-values (ctx* vars* vals*)
      (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                #:result (values ctx* (reverse vars*) (reverse vals*)))
                ([var (in-list vars)] [val (in-list vals)])
        (define-values (val* val-ctx) (visit/ctx vtor val ctx))
        (define prec (ctx-lookup-prop val-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (values (format (let-format)
                    (string-join (map (curry format (let-bind-format)) vars* vals*)
                                 (let-bind-seperator))
                    body*)
            body-ctx)]

  [(visit-let* vtor vars vals body #:ctx ctx)
    (visit/ctx
      vtor
      (let loop ([vars vars] [vals vals])
        (cond [(null? vars) body]
              [else `(let (,(list (car vars) (car vals)))
                      ,(loop (cdr vars) (cdr vals)))]))
      ctx)]

  [(visit-cast vtor body #:ctx ctx)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx))
    (values (compile-round body* ctx) body-ctx)]

  [(visit-! vtor props body #:ctx ctx)
    (define ctx* (ctx-update-props ctx props))
    (visit/ctx vtor body ctx*)]

  [(visit-op_ vtor op args #:ctx ctx)
    (define args*
      (for/list ([arg args])
        (let-values ([(arg* arg-ctx) (visit/ctx vtor arg ctx)])
          arg*)))
    (values (compile-operator op args* ctx)
            (if (set-member? bool-ops op)
                (ctx-update-props ctx '(:precision boolean))
                ctx))]

  [(visit-digits vtor m e b #:ctx ctx)
    (visit/ctx vtor (digits->number m e b) ctx)]

  [(visit-number vtor x #:ctx ctx)
    (values (compile-constant x ctx) ctx)]

  [(visit-constant vtor x #:ctx ctx)
    (values (compile-constant x ctx)
            (if (set-member? '(TRUE FALSE) x)
                (ctx-update-props ctx '(:precision boolean))
                ctx))]

  [(visit-symbol vtor x #:ctx ctx)
    (define name (ctx-lookup-name ctx x))
    (define var-prec (ctx-lookup-prec ctx name))
    (values name (ctx-update-props ctx `(:precision ,var-prec)))])


;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-lisp-compiler name
                            ; language behavior
                            #:operator [operator default-compile-operator]
                            #:constant [constant default-compile-constant]
                            #:if-format [if-format "(~a ~a ~a)"]
                            #:let-format [let-format "(let (~a) ~a)"]
                            #:let-bind-format [let-bind-format (cons "(~a ~a)" " ")]
                            #:round [round default-compile-round]
                            #:program [program default-compile-program]
                            #:flags [flags '()]
                            ; visitor behvaior
                            #:visitor [vtor lisp-visitor]
                            #:reserved [reserved '()]
                            #:fix-name [fix-name identity])
  (unless (andmap valid-flag? flags)
    (error 'make-lisp-compiler "Undefined compiler flags: ~a" flags))
  (define language
    (lisp name operator constant if-format let-format let-bind-format
          round program flags))
  (lambda (prog name)
    (parameterize ([*gensym-used-names* (mutable-set)] 
                   [*gensym-collisions* 1]
                   [*gensym-fix-name* fix-name]
                   [*lisp-lang* language])
      (define-values (args props body)
        (match prog
         [(list 'FPCore (list args ...) props ... body) (values args props body)]
         [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
      (define ctx
        (let ([ctx0 (ctx-update-props default-ctx props)])
          (ctx-reserve-names ctx0 reserved)))

      ; compiled function name
      (define fname
        (let-values ([(cx fname) (ctx-unique-name ctx name)])
          (begin0 fname (set! ctx cx))))

      ; compiled argument names
      (define-values (arg-names arg-ctxs)
        (for/lists (ns ps) ([arg (in-list args)])
          (match arg
           [(list '! props ... name)
            (define arg-ctx (ctx-update-props ctx props))
            (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
            (define-values (cx aname) (ctx-unique-name ctx name arg-prec))
            (begin0 (values aname arg-ctx) (set! ctx cx))]
           [name
            (define-values (cx aname) (ctx-unique-name ctx name))
            (begin0 (values aname ctx) (set! ctx cx))])))

      (define-values (out _) (visit/ctx vtor body ctx))
      (compile-program fname arg-names arg-ctxs out ctx))))
