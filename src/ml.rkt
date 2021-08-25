;
;   Common compiler for ML languages
;     CakeML
;

#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")

(provide (all-from-out "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")
         make-ml-compiler ml-visitor)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *ml-lang* (make-parameter #f))

(struct ml
  (name               ; string representation of language
   infix              ; list of ops that use default infix formatter
   operator           ; procedure to format any non-infix operator
   constant           ; procedure to format constants
   round              ; procedure to format (explicit) casts
   program            ; procedure to format the entire program
   flags))            ; list of optional flags to change minor behavior

;;;;;;;;;;;;;;;;;;;;;;;;;;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-flags '())

(define (valid-flag? maybe-flag)
  (set-member? valid-flags maybe-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-infix-operator op args ctx)
  (match (cons op args)
   [(list '- a)
    (format (if (string-prefix? a "-") "-(~a)" "-~a") a)]
   [(list 'not a)
    (format "!~a" a)]
   [(list (or '== '!= '< '> '<= '>=))
    (compile-constant 'TRUE ctx)]
   [(list (or '+ '- '* '/) a b) ; binary arithmetic 
    (format "(~a ~a ~a)" a op b)]
   [(list (or '== '< '> '<= '>=) arg args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons arg args)] [b args])
                (format "~a ~a ~a" a op b))
              " && "))]
   [(list '!= args ...)
     (format "(~a)"
             (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                     (for/list ([b (cdr args)])
                       (format "~a != ~a" (car args) b))
                     (loop (cdr args)))))
              " && "))]
   [(list 'and a ...)
    (format "(~a)" (string-join (map ~a a) " && "))]
   [(list 'or a ...)
    (format "(~a)" (string-join (map ~a a) " || "))]))

(define (compile-operator op args ctx)
  (if (set-member? (ml-infix (*ml-lang*)) op)
      (compile-infix-operator op args ctx)  ; imported from ml.rkt
      ((ml-operator (*ml-lang*)) op args ctx)))

(define (compile-function op args ctx)
  ((ml-operator (*ml-lang*)) op args ctx))

(define (compile-constant x ctx)
  ((ml-constant (*ml-lang*)) x ctx))

(define (compile-round expr ctx)
  ((ml-round (*ml-lang*)) expr ctx))

(define (compile-program name args arg-ctxs body ctx)
  ((ml-program (*ml-lang*)) name args arg-ctxs body ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-infix-ops '(+ - * / == != < > <= >= not and or))

(define (default-compile-operator op args ctx)
  (string-join (map ~a (cons op args)) " "))

(define (default-compile-constant x ctx)
  (~a x))

(define (default-compile-round expr ctx)
  (~a expr))

(define (default-compile-program name args arg-ctxs body ctx)
  (format "fun ~a ~a = ~a\n" name (string-join args " ") body))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-ctx
  (ctx-update-props
    (make-compiler-ctx)
    '(:precision binary64 :round nearestEven)))

(define single-indent "  ")
(define double-indent (format "~a~a" single-indent single-indent))

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define bool-ops '(< > <= >= == != and or not
                   isfinite isinf isnan isnormal signbit))

;;;;;;;;;;;;;;;;;;;;;;;;;;; visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-expr-visitor default-compiler-visitor ml-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define-values (cond* _) (visit/ctx vtor cond ctx))
    (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
    (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx))
    (values (format "~aif ~a then\n~a~a~a\n~aelse\n~a~a~a\n"
                    indent cond*
                    indent single-indent ift* indent
                    indent single-indent iff*)
            ift-ctx)]

  [(visit-let_ vtor let_ vars vals body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define-values (ctx* vars* vals*)
      (for/fold ([ctx* ctx] [vars* '()] [vals* '()]
                #:result (values (ctx-set-extra ctx* 'indent (format "~a~a" indent double-indent))
                                 (reverse vars*)
                                 (reverse vals*)))
                ([var (in-list vars)] [val (in-list vals)])
        (define val-ctx
          (ctx-set-extra (match let_ ['let ctx] ['let* ctx*])
                         'indent (format "~a~a" indent single-indent)))
        (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
        (define prec (ctx-lookup-prop val*-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (values (format "~alet\n~ain~a~a~a\nend"
                    indent (apply string-append
                            (for/list ([var vars*] [val vals*])
                              (format "~a~aval ~a = ~a\n" indent
                                      single-indent var (trim-infix-parens val))))
                    indent single-indent (trim-infix-parens body*))
            body-ctx)]

  ; Convert to local function
  ;   fun loop vars ... =
  ;     if cond then
  ;       updates
  ;       ...
  ;     else
  ;       body
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define-values (fn-ctx fn-name) (ctx-unique-name ctx 'loop))
    (define-values (ctx* vars* vals*)
      (for/fold ([ctx* fn-ctx] [vars* '()] [vals* '()]
                #:result (values (ctx-set-extra ctx* 'indent (format "~a~a" indent double-indent))
                                 (reverse vars*)
                                 (reverse vals*)))
                ([var (in-list vars)] [val (in-list inits)])
        (define val-ctx (match while_ ['while fn-ctx] ['while* ctx*]))
        (define-values (val* val*-ctx)
          (visit/ctx vtor val
            (ctx-set-extra val-ctx 'indent
              (format "~a~a" indent single-indent))))
        (define prec (ctx-lookup-prop val*-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
        (values name-ctx (cons name vars*) (cons val* vals*))))
    (define-values (cond* cond*-ctx) (visit/ctx vtor cond ctx*))
    (define updates*
      (for/fold ([ctx** ctx*] [vals* '()] #:result (reverse vals*))
                ([var (in-list vars)] [val (in-list updates)])
        (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
        (define-values (val* val*-ctx)
          (visit/ctx vtor val
            (ctx-set-extra val-ctx 'indent
              (format "~a~a~a" indent double-indent single-indent))))
        (define prec (ctx-lookup-prop val*-ctx ':precision))
        (define-values (name-ctx name) (ctx-unique-name ctx** var prec))
        (values name-ctx (cons val* vals*))))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (values (format "~alet\n~a~afun ~a ~a =\n~a~aif ~a then\n~a~a~a~a\n~a~aelse\n~a~a~a~a\n~a"
                    indent indent single-indent fn-name
                    (string-join vars* " ")
                    indent double-indent (trim-infix-parens cond*)
                    indent double-indent single-indent
                    (string-join (cons fn-name updates*) " ")
                    indent double-indent
                    indent double-indent single-indent body*
                    (format "~ain\n~a~a~a\n~aend"
                            indent indent single-indent
                            (string-join (cons fn-name vals*) " ")
                            indent))
            body-ctx)]

  [(visit-cast vtor body #:ctx ctx)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx))
    (values (compile-round body* ctx) body-ctx)]

  [(visit-! vtor props body #:ctx ctx)
    (define ctx* (ctx-update-props ctx props))
    (visit/ctx vtor body ctx*)]

  [(visit-op vtor op args #:ctx ctx)
    (define args*
      (for/list ([arg args])
        (let-values ([(arg* arg-ctx) (visit/ctx vtor arg ctx)])
          arg*)))
    (values (compile-operator op args* ctx)
            (if (set-member? bool-ops op)
                (ctx-update-props ctx (list ':precision 'boolean))
                ctx))]
  
  [(visit-call vtor fn args #:ctx ctx)
    (define args*
      (for/list ([arg args])
        (define-values (arg* _) (visit/ctx vtor arg ctx))
          arg*))
    (values (compile-function fn args ctx) ctx)]

  [(visit-digits vtor m e b #:ctx ctx)
    (visit/ctx vtor (digits->number m e b) ctx)]

  [(visit-number vtor x #:ctx ctx)
    (values (compile-constant x ctx) ctx)]

  [(visit-constant vtor x #:ctx ctx)
    (values (compile-constant x ctx)
            (if (set-member? '(TRUE FALSE) x)
                (ctx-update-props ctx (list ':precision 'boolean))
                ctx))]
  
  [(visit-symbol vtor x #:ctx ctx)
    (define var-prec (ctx-lookup-prec ctx x))
    (values (ctx-lookup-name ctx x) (ctx-update-props ctx `(:precision ,var-prec)))])


;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-ml-compiler name
                          ; language behavior
                          #:infix-ops [infix default-infix-ops]
                          #:operator [operator default-compile-operator]
                          #:constant [constant default-compile-constant]
                          #:round [round default-compile-round]
                          #:program [program default-compile-program]
                          #:flags [flags '()]
                          ; visitor behvaior
                          #:visitor [vtor ml-visitor]
                          #:reserved [reserved '()]
                          #:fix-name [fix-name identity])
  (unless (andmap valid-flag? flags)
    (error 'make-ml-compiler "Undefined ml flags: ~a" flags))
  (define language (ml name infix operator constant round program flags))
  (lambda (prog name)
    (parameterize ([*gensym-used-names* (mutable-set)] 
                   [*gensym-collisions* 1]
                   [*gensym-fix-name* fix-name]
                   [*ml-lang* language])
      (define-values (args props body)
        (match prog
         [(list 'FPCore (list args ...) props ... body) (values args props body)]
         [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
      (define ctx
        (let ([ctx0 (ctx-update-props default-ctx props)])
          (let ([ctx1 (ctx-reserve-names ctx0 reserved)])
            (ctx-set-extra ctx1 'indent "  "))))

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

      (define p (open-output-string))
      (parameterize ([current-output-port p])
        (define-values (o _) (visit/ctx vtor body ctx))
        (compile-program fname arg-names arg-ctxs
                         (trim-infix-parens o) ctx)))))

(module+ test
  (require rackunit)
  (define compile0 (make-ml-compiler "default"))
  (define (compile* . exprs)
    (apply values (for/list ([expr exprs] [i (in-naturals 1)])
                    (compile0 expr (format "fn~a" i)))))
  
  (compile*
    '(FPCore (x) (if (< x 0) (+ x 1) (- x 1)))
    '(FPCore (x) (let ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (let* ([x 1] [y x]) (+ x y)))
    '(FPCore (x) (while (< x 4) ([x 0.0 (+ x 1.0)]) x))
    '(FPCore (x) (while* (< x 4) ([x 0.0 (+ x 1.0)]) x))
    '(FPCore (x) (+ (foo x) 1))
    '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))
    '(FPCore (a b) (+ (* a b) (- a b))))
)
