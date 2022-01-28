;
;   Common compiler for ML languages
;     CakeML, OCaml, Haskell
;

#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt"
         "ml-canonicalizer.rkt" "supported.rkt")

(provide (all-from-out "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")
         make-ml-compiler ml-visitor default-infix-ops
         half-indent single-indent double-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *ml-lang* (make-parameter #f))

(struct ml
  (name               ; string representation of language
   infix              ; list of ops that use default infix formatter
   operator           ; procedure to format any non-infix operator
   constant           ; procedure to format constants
   round              ; procedure to format (explicit) casts
   implicit-round     ; procedure to handle (implicit) casts
   program            ; procedure to format the entire program
   flags))            ; list of optional flags to change minor behavior

;;;;;;;;;;;;;;;;;;;;;;;;;;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-flags
  '(round-output))        ; calls 'implicit-round' on return expression

(define (valid-flag? maybe-flag)
  (set-member? valid-flags maybe-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-flag-raised? flag)
  (set-member? (ml-flags (*ml-lang*)) flag))

(define (compile-infix-operator op args ctx)
  (match (cons op args)
   [(list '- a)
    (format "(- ~a)" a)]
   [(list 'not a)
    (format "(not ~a)" a)]
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

(define (compile-round expr ictx octx)
  ((ml-round (*ml-lang*)) expr ictx octx))

(define (compile-implicit-round arg-ctxs ctx)
  ((ml-implicit-round (*ml-lang*)) arg-ctxs ctx))

(define (compile-program name args arg-ctxs body ctx)
  ((ml-program (*ml-lang*)) name args arg-ctxs body ctx))

(define (compile-body expr ictx octx)
  (if (compile-flag-raised? 'round-output)
      (compile-round expr ictx octx)
      expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-infix-ops '(+ - * / == != < > <= >= not and or))

(define (default-compile-operator op args ctx)
  (string-join (map ~a (cons op args)) " "))

(define (default-compile-constant x ctx)
  (~a x))

(define (default-compile-round expr ictx octx)
  (~a expr))

(define (default-implicit-round arg-ctxs ctx)
  (values (make-list (length arg-ctxs) identity) identity))

(define (default-compile-program name args arg-ctxs body ctx)
  (format "fun ~a ~a = ~a\n" name (string-join args " ") body))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-ctx
  (ctx-update-props
    (make-compiler-ctx)
    '(:precision binary64 :round nearestEven)))

(define half-indent " ")
(define single-indent (string-append half-indent half-indent))
(define double-indent (string-append single-indent single-indent))

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
    (define ctx* (ctx-set-extra ctx 'indent (format "~a~a" indent single-indent)))
    (printf "if ")
    (define-values (cond* _) (visit/ctx vtor cond ctx*))
    (printf "~a then\n~a~a" cond* indent single-indent)
    (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx*))
    (printf "~a\n~aelse\n~a~a" ift* indent indent single-indent)
    (define-values (iff* iff-ctx) (visit/ctx vtor iff ctx*))
    (printf "~a" iff*)
    (values "" ift-ctx)]

  [(visit-let_ vtor let_ vars vals body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define let-ctx (ctx-set-extra ctx 'indent (format "~a~a" indent single-indent)))
    (printf "let\n")
    (define ctx*
      (for/fold ([ctx* let-ctx] #:result ctx*)
                ([var (in-list vars)] [val (in-list vals)])
        (define val-ctx (match let_ ['let let-ctx] ['let* ctx*]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop var-ctx ':precision)])
                (ctx-unique-name ctx* var prec)))))
        (printf "~a~aval ~a = " indent single-indent name)
        (define val-ctx*
          (ctx-update-extra val-ctx 'indent
                            (curry format "~a~a~a" double-indent single-indent)))
        (printf "~a\n" (let-values ([(val* _) (visit/ctx vtor val val-ctx*)])
                        (trim-infix-parens val*)))
        name-ctx))
    (printf "~ain\n~a~a" indent indent single-indent)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (printf "~a\n~aend" (trim-infix-parens body*) indent)
    (values "" body-ctx)]

  ; let
  ;   vars = vals
  ;   ...
  ;   fun loop vars ... =
  ;     if cond then
  ;       let
  ;         vars = updates
  ;         ...
  ;       in
  ;         loop vars ...
  ;       end
  ;     else
  ;       body
  ; in
  ;   loop inits ...
  ; end
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define inner-cond-indent (string-append double-indent single-indent half-indent))
    (define-values (while-ctx fn-name)
      (let ([ctx0 (ctx-set-extra ctx 'indent (format "~a~a" indent single-indent))])
        (ctx-unique-name ctx0 'loop)))
    (printf "let\n")
    (define-values (ctx* vars*)                             ; loop variables
      (for/fold ([ctx* while-ctx] [vars* '()]
                #:result (values ctx* (reverse vars*)))
                ([var (in-list vars)] [val (in-list inits)])
        (define val-ctx (match while_ ['while while-ctx] ['while* ctx*]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx* var prec)))))
        (printf "~a~aval ~a = " indent single-indent name)
        (printf "~a\n" (let-values ([(val* _) (visit/ctx vtor val val-ctx)])
                          (trim-infix-parens val*)))
        (values name-ctx (cons name vars*))))
    (printf "~a~afun ~a ~a =\n" indent single-indent
            fn-name (string-join vars* " "))
    (printf "~a~aif " indent double-indent)
    (define-values (cond* _)
      (let ([ctx0 (ctx-set-extra ctx* 'indent (format "~a~a" indent inner-cond-indent))])
        (visit/ctx vtor cond ctx0)))    ; condition
    (printf "~a then\n~a~a~alet\n" cond* indent double-indent single-indent)
    (define-values (ctx** vars**)                           ; loop update
      (for/fold ([ctx** ctx*] [vars* '()] #:result (values ctx** (reverse vars*)))
                ([var (in-list vars)] [val (in-list updates)])
        (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx** var prec)))))
        (printf "~a~aval ~a = " indent (string-append double-indent double-indent) name)
        (define val-ctx*
          (ctx-update-extra val-ctx 'indent
                            (curry format "~a~a~a" double-indent single-indent)))
        (printf " ~a\n" (let-values ([(val* _) (visit/ctx vtor val val-ctx*)])
                          (trim-infix-parens val*)))
        (values name-ctx (cons name vars*))))
    (printf "~a~a~ain\n~a~a~a"
            indent double-indent single-indent
            indent double-indent double-indent)
    (printf "~a ~a\n" fn-name (string-join vars** " "))   ; call the loop
    (printf "~a~a~aend\n~a~aelse\n~a~a~a"
            indent double-indent single-indent
            indent double-indent
            indent double-indent single-indent)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (printf "~a\n~ain\n~a~a~a ~a\n~aend" (trim-infix-parens body*) indent
            indent single-indent fn-name (string-join vars* " ") indent)
    (values "" body-ctx)]

  [(visit-cast vtor body #:ctx ctx)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx))
    (values (compile-round body* body-ctx ctx) ctx)]

  [(visit-! vtor props body #:ctx ctx)
    (define ctx* (ctx-update-props ctx props))
    (visit/ctx vtor body ctx*)]

  [(visit-op vtor op args #:ctx ctx)
    (define-values (args* arg-ctxs)
      (for/lists (l1 l2) ([arg args])
        (visit/ctx vtor arg ctx)))
    (define-values (arg-casts out-cast) (compile-implicit-round arg-ctxs ctx))
    (define expr* (compile-operator op (map (λ (f x) (f x)) arg-casts args*) ctx))
    (if (set-member? bool-ops op)
        (values expr* (ctx-update-props ctx (list ':precision 'boolean)))
        (values (out-cast expr*) ctx))]
  
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
    (define name (ctx-lookup-name ctx x))
    (define var-prec (ctx-lookup-prec ctx name))
    (values name (ctx-update-props ctx `(:precision ,var-prec)))])


;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-ml-compiler name
                          ; language behavior
                          #:infix-ops [infix default-infix-ops]
                          #:operator [operator default-compile-operator]
                          #:constant [constant default-compile-constant]
                          #:round [round default-compile-round]
                          #:implicit-round [implicit-round default-implicit-round]
                          #:program [program default-compile-program]
                          #:flags [flags '()]
                          ; visitor behvaior
                          #:visitor [vtor ml-visitor]
                          #:reserved [reserved '()]
                          #:fix-name [fix-name identity])
  (unless (andmap valid-flag? flags)
    (error 'make-ml-compiler "Undefined ml flags: ~a" flags))
  (define language (ml name infix operator constant round implicit-round program flags))
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

      (define body* (canonicalize-ml body (apply set (set->list (*gensym-used-names*)))))
      (define p (open-output-string))
        (parameterize ([current-output-port p])
          (define-values (o c) (visit/ctx vtor body* ctx))
          (define o* 
            (let ([o (string-append (get-output-string p) o)])
              (trim-infix-parens (compile-round o c ctx))))
          (compile-program fname arg-names arg-ctxs o* ctx)))))

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
