;
;   Common compiler for all imperative languages
;     C, JS, Go, Rust, Sollya, Scala, Fortran
;     FPTaylor, MATLAB
;

#lang racket

(require "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")

(provide (all-from-out "common.rkt" "compilers.rkt" "fpcore-visitor.rkt" "supported.rkt")
         make-imperative-compiler
         default-infix-ops
         bool-ops
         imperative-visitor
         compile-operator)  ; exported for MATLAB

;;;;;;;;;;;;;;;;;;;;;;;;;;; language-specific abstractions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *imperative-lang* (make-parameter #f))

(struct imperative
  (name             ; string representation of language
   infix            ; list of ops that use default infix formatter
   operator         ; procedure to format any non-infix operator
   constant         ; procedure to format constants
   type             ; procedure that returns language name of an FPCore precision
   declare          ; procedure to format declarations
   assign           ; procedure to format assignments
   round            ; procedure to format (explicit) casts
   implicit-round   ; procedure to format implicit casts
   round-mode       ; procedure to format changes to rounding mode
   use-vars         ; procedure to format post-processing on new variables
   program          ; procedure to format the entire program
   flags))          ; list of optional flags to change minor behavior

;;;;;;;;;;;;;;;;;;;;;;;;;;; flags ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-flags
  '(no-parens-around-condition        ; removes parenthesis from 'if' and 'while' conditions (Go, Python, Rust)
    for-instead-of-while              ; changes 'while' to 'for' (Go)
    never-declare                     ; declarations are assignments (Sollya, FPTaylor, Fortran)
    semicolon-after-enclosing-brace   ; end 'if' or 'while' blocks with "};" (Sollya)
    if-then                           ; "if (cond) then { ... }" (Sollya, Fortran)
    while-do                          ; "while (cond) do { ... }" (Sollya)
    round-after-operation             ; ensure rounding after any operation (Sollya, FPTaylor, Julia)
    colon-instead-of-brace            ; use a colon rather than braces for code blocks (Python)
    use-elif                          ; use 'elif' instead of 'else if' (Python)
    use-elseif                        ; use 'elseif' instead of 'else if' (MATLAB, Julia)
    boolean-ops-use-name              ; boolean operators use alphabetic name rather than symbol (Python)
    spaces-for-tabs                   ; replace tabs with 4 spaces (Fortran, Rust)
    do-while                          ; changes 'while' to 'do while' (Fortran)
    end-block-with-name               ; blocks enclosed by "<x> ... end <x>, implicitly no braces" (Fortran)
    end-block-with-end                ; blocks ended by "end", implictly no braces" (MATLAB, Julia)
    no-body))                         ; do not compile the body (C header)

(define (valid-flag? maybe-flag)
  (set-member? valid-flags maybe-flag))

(define (more-than-one-of? specific flags)
  (> (count (curry set-member? specific) flags) 1))

(define (flag-conflict? flags)
  (or (more-than-one-of? '(colon-instead-of-brace     ; brace vs. colon vs. end <name> vs. end
                           end-block-with-name
                           end-block-with-end)
                         flags)
      (more-than-one-of? '(for-instead-of-while       ; while vs. for vs. do while
                           do-while)
                         flags)))

(define (format-condition cond)
  (if (compile-flag-raised? 'no-parens-around-condition)
      (format "~a" cond)
      (format "(~a)" cond)))

(define (while-name)
  (cond
   [(compile-flag-raised? 'for-instead-of-while) "for"]
   [(compile-flag-raised? 'do-while) "do while"]
   [else "while"]))

(define (else-if-name)
  (cond
   [(compile-flag-raised? 'use-elif) "elif"]
   [(compile-flag-raised? 'use-elseif) "elseif"]
   [else "else if"]))

(define (after-if)
  (if (compile-flag-raised? 'if-then)
      " then"
      ""))

(define (after-while)
  (if (compile-flag-raised? 'while-do)
      " do"
      ""))

(define (if-declare decl indent)
  (if (compile-flag-raised? 'never-declare)
      ""
      (format "~a~a\n" indent decl)))

(define (if-format)
  (cond
   [(compile-flag-raised? 'colon-instead-of-brace) "~aif ~a~a:\n"]
   [(compile-flag-raised? 'end-block-with-name) "~aif ~a~a\n"]
   [(compile-flag-raised? 'end-block-with-end) "~aif ~a~a\n"]
   [else "~aif ~a~a {\n"]))

(define (else-if-format)
  (cond
   [(compile-flag-raised? 'colon-instead-of-brace) "~a~a ~a~a:\n"]
   [(compile-flag-raised? 'end-block-with-name) "~a~a ~a~a\n"]
   [(compile-flag-raised? 'end-block-with-end) "~a~a ~a~a\n"]
   [else "~a} ~a ~a~a {\n"]))

(define (else-format)
  (cond
   [(compile-flag-raised? 'colon-instead-of-brace) "~aelse:\n"]
   [(compile-flag-raised? 'end-block-with-name) "~aelse\n"]
   [(compile-flag-raised? 'end-block-with-end) "~aelse\n"]
   [else "~a} else {\n"]))

(define (while-format)
  (cond
   [(compile-flag-raised? 'colon-instead-of-brace) "~a~a ~a~a:\n"]
   [(compile-flag-raised? 'end-block-with-name) "~a~a ~a~a\n"]
   [(compile-flag-raised? 'end-block-with-end) "~a~a ~a~a\n"]
   [else "~a~a ~a~a {\n"]))

(define (end-of-block indent type)
  (cond
   [(compile-flag-raised? 'colon-instead-of-brace) ""]
   [(compile-flag-raised? 'end-block-with-end)
    (format "~aend\n" indent)]
   [(compile-flag-raised? 'end-block-with-name)
    (define name
      (cond
       [(equal? type 'if) "if"]
       [(compile-flag-raised? 'do-while) "do"]
       [else (while-name)]))
    (format "~aend ~a\n" indent name)]
   [(compile-flag-raised? 'semicolon-after-enclosing-brace) (format "~a};\n" indent)]
   [else (format "~a}\n" indent)]))

(define (visit-body vtor body ctx)
  (if (compile-flag-raised? 'no-body)
      (values "" ctx)
      (visit/ctx vtor body ctx)))

(define (single-tab)
  (if (compile-flag-raised? 'spaces-for-tabs)
      "    "
      "\t"))

;;;;;;;;;;;;;;;;;;;;;;;;;;; shorthands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-flag-raised? flag)
  (set-member? (imperative-flags (*imperative-lang*)) flag))

(define (compile-after-op x ctx)
  (if (compile-flag-raised? 'round-after-operation)
      (compile-round x ctx)
      x))

(define (compile-infix-operator op args ctx)
  (match (cons op args)
   [(list '- a)
    (compile-after-op (format (if (string-prefix? a "-") "-(~a)" "-~a") a) ctx)]
   [(list 'not a)
    (if (compile-flag-raised? 'boolean-ops-use-name)
        (format "not ~a" a)
        (format "!~a" a))]
   [(list (or '== '!= '< '> '<= '>=))
    (compile-constant 'TRUE ctx)]
   [(list (or '+ '- '* '/) a b) ; binary arithmetic 
    (compile-after-op (format "(~a ~a ~a)" a op b) ctx)]
   [(list (or '== '< '> '<= '>=) arg args ...)
    (format "(~a)"
            (string-join
              (for/list ([a (cons arg args)] [b args])
                (format "~a ~a ~a" a op b))
              (if (compile-flag-raised? 'boolean-ops-use-name)
                  " and "
                  " && ")))]
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
              (if (compile-flag-raised? 'boolean-ops-use-name)
                  " and "
                  " && ")))]
   [(list 'and a ...)
    (define and-str (if (compile-flag-raised? 'boolean-ops-use-name) " and " " && "))
    (format "(~a)" (string-join (map ~a a) and-str))]
   [(list 'or a ...)
    (define or-str (if (compile-flag-raised? 'boolean-ops-use-name) " or " " || "))
    (format "(~a)" (string-join (map ~a a) or-str))]))

(define (compile-operator op args ctx)
  (if (set-member? (imperative-infix (*imperative-lang*)) op)
      (compile-infix-operator op args ctx)
      ((imperative-operator (*imperative-lang*)) op args ctx)))

(define (compile-function fn args ctx)
  ((imperative-operator (*imperative-lang*)) fn args ctx))

(define (compile-constant x ctx)
  ((imperative-constant (*imperative-lang*)) x ctx))

(define (compile-type x)
  ((imperative-type (*imperative-lang*)) x))

(define compile-declaration
  (case-lambda
   [(var ctx) ((imperative-declare (*imperative-lang*)) var ctx)]
   [(var val ctx)
    (if (compile-flag-raised? 'never-declare)
        ((imperative-assign (*imperative-lang*)) var (trim-infix-parens val) ctx)
        ((imperative-declare (*imperative-lang*)) var (trim-infix-parens val) ctx))]))

(define (compile-assignment var val ctx)
  ((imperative-assign (*imperative-lang*)) var (trim-infix-parens val) ctx))

(define (compile-round expr ctx)
  ((imperative-round (*imperative-lang*)) expr ctx))

(define (compile-implicit-round op arg ctx arg-ctx)
  ((imperative-implicit-round (*imperative-lang*)) op arg ctx arg-ctx))

(define (compile-round-mode mode ctx)
  ((imperative-round-mode (*imperative-lang*)) mode ctx))

(define (compile-use-vars vars ctx)
  (define vars* (map (curry ctx-lookup-name ctx) vars))
  ((imperative-use-vars (*imperative-lang*)) vars* ctx))

(define (compile-program name args arg-ctxs body ret ctx used-vars)
  ((imperative-program (*imperative-lang*)) name args arg-ctxs body ret ctx used-vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;; defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-infix-ops '(+ - * / == != < > <= >= not and or))

(define (default-compile-operator fn args ctx)
  (format "~a(~a)" fn (string-join (map ~a args) ", ")))

(define (default-compile-constant x ctx)
  (~a x))

(define (default-compile-type type)
  "var")

(define default-compile-declaration
  (case-lambda
   [(var ctx) (format "~a ~a;" (compile-type (ctx-lookup-prop ctx ':precision)) var)]
   [(var val ctx) (format "~a ~a = ~a;" (compile-type (ctx-lookup-prop ctx ':precision)) var val)]))

(define (default-compile-assignment var val ctx)
  (format "~a = ~a;" var val))

(define (default-compile-round expr ctx)
  expr)

(define (default-compile-implicit-round op arg ctx arg-ctx)
  arg)

(define (default-compile-round-mode expr ctx)
  expr)

(define (default-use-vars vars ctx)
  "")

(define (default-compile-program name args arg-ctxs body ret ctx used-vars)
  (if (non-empty-string? body)
      (format "function ~a(~a) {\n~a\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              body ret)
      (format "function ~a(~a) {\n\treturn ~a;\n}\n"
              name (string-join (map ~a args) ", ")
              ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-ctx
  (ctx-update-props
    (make-compiler-ctx)
    '(:precision binary64 :round nearestEven)))

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

(define (visit-if/imperative vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define branches 
    (let loop ([expr (list 'if cond ift iff)])
      (match expr
       [(list 'if cond ift iff)
        (define-values (cond* _) (visit/ctx vtor cond ctx))
        (cons (list cond* ift) (loop iff))]
       [_ (list (list #t expr))])))
  (let loop ([branches branches] [first? #t] [ctx ctx] [ret #f])
    (match* (first? (car branches))
     [(#t (list cond ift))
      (define-values (ctx* tmpvar)   ; messy workaround to get ift context
        (parameterize ([current-output-port (open-output-nowhere)])
          (define-values (_ ift-ctx) (visit/ctx vtor ift ctx))
          (define prec (ctx-lookup-prop ift-ctx ':precision))
          (ctx-random-name (ctx-update-props ctx `(:precision ,prec)))))
      (printf (if-declare (compile-declaration tmpvar ctx*) indent))
      (printf (if-format) indent (format-condition (trim-infix-parens cond)) (after-if))
      (define-values (ift* ift-ctx) 
        (let ([ctx0 (ctx-set-extra ctx 'indent (format "~a~a" indent (single-tab)))])
          (visit/ctx vtor ift ctx0)))
      (printf "~a~a~a\n" indent (single-tab) (compile-assignment tmpvar ift* ctx))
      (loop (cdr branches) #f ctx* tmpvar)]
     [(_ (list #t last))
      (printf (else-format) indent)
      (define ctx* (ctx-set-extra ctx 'indent (format "~a~a" indent (single-tab))))
      (define-values (last* else-ctx) (visit/ctx vtor last ctx*))
      (printf "~a~a~a\n" indent (single-tab) (compile-assignment ret last* ctx))
      (printf (end-of-block indent 'if))
      (values ret else-ctx)]
     [(_ (list cond elif))
      (printf (else-if-format) indent (else-if-name)
              (format-condition (trim-infix-parens cond))
              (after-if))
      (define ctx* (ctx-set-extra ctx 'indent (format "~a~a" indent (single-tab))))
      (define-values (elif* elif-ctx) (visit/ctx vtor elif ctx*))
      (printf "~a~a~a\n" indent (single-tab) (compile-assignment ret elif* ctx))
      (loop (cdr branches) #f ctx ret)])))

(define (visit-let_/imperative vtor let_ vars vals body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define ctx*
    (for/fold ([ctx* ctx]) ([var (in-list vars)] [val (in-list vals)])
      (define-values (val* val-ctx) (visit/ctx vtor val (match let_ ['let ctx] ['let* ctx*])))
      (define prec (ctx-lookup-prop val-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (define decl-ctx (ctx-update-props ctx* `(:precision ,prec)))
      (printf "~a~a\n" indent (compile-declaration name val* decl-ctx))
      name-ctx))
  (printf "~a" (compile-use-vars vars ctx*))
  (visit/ctx vtor body ctx*))

(define (visit-while_/imperative vtor while_ cond vars inits updates body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (ctx* vars*)
    (for/fold ([ctx* ctx] [vars* '()] #:result (values ctx* (reverse vars*)))
              ([var (in-list vars)] [val (in-list inits)])
      (define val-ctx (match while_ ['while ctx] ['while* ctx*]))
      (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
      (define prec (ctx-lookup-prop val*-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (define decl-ctx (ctx-update-props ctx* `(:precision ,prec)))
      (printf "~a~a\n" indent (compile-declaration name val* decl-ctx))
      (values name-ctx (cons name vars*))))
  (define tmpvar
    (let-values ([(cx name) (ctx-random-name ctx* 'boolean)])
      name))
  (printf "~a" (compile-use-vars vars ctx*))
  (define-values (cond* cond*-ctx) (visit/ctx vtor cond ctx*))
  (printf "~a~a\n" indent (compile-declaration tmpvar cond* cond*-ctx))
  (printf (while-format) indent (while-name)
          (format-condition tmpvar) (after-while))
  (define ctx**
    (match while_
     ['while
      (define val-ctx (ctx-set-extra ctx* 'indent (format "~a~a" indent (single-tab))))
      (define-values (ctx** vars**)
        (for/fold ([ctx** ctx*] [vars* '()]
                  #:result (values (ctx-set-extra ctx* 'indent (format "~a~a" indent (single-tab)))
                                   (reverse vars*)))
                  ([var (in-list vars)] [val (in-list updates)])
          (define-values (val* val*-ctx) (visit/ctx vtor val val-ctx))
          (define prec (ctx-lookup-prop val*-ctx ':precision))
          (define-values (name-ctx name) (ctx-unique-name ctx** var prec))
          (define decl-ctx (ctx-update-props ctx** `(:precision ,prec)))
          (printf "~a~a~a\n" indent (single-tab) (compile-declaration name val* decl-ctx))
          (values name-ctx (cons name vars*))))
      (printf "~a" (compile-use-vars vars ctx**))
      (for ([var* (in-list vars*)] [var** (in-list vars**)])
        (printf "~a~a~a\n" indent (single-tab) (compile-assignment var* var** ctx**)))
      ctx**]
     ['while*
      (define ctx** (ctx-set-extra ctx* 'indent (format "~a~a" indent (single-tab))))
      (for ([var* (in-list vars*)] [val (in-list updates)])
        (let-values ([(val* _) (visit/ctx vtor val ctx**)])
          (printf "~a~a~a\n" indent (single-tab) (compile-assignment var* val* ctx**))))
      ctx**]))
  (define-values (cond** cond**-ctx) (visit/ctx vtor cond ctx**))
  (printf "~a~a~a\n" indent (single-tab) (compile-assignment tmpvar cond** cond**-ctx))
  (printf (end-of-block indent 'while))
  (visit/ctx vtor body ctx*))

(define (visit-cast/imperative vtor x #:ctx ctx)
  (define-values (body* body-ctx) (visit/ctx vtor x ctx))
  (values (compile-round body* ctx) body-ctx))

(define (visit-!/imperative vtor props body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define curr-prec (ctx-lookup-prop ctx ':precision))
  (define curr-round (ctx-lookup-prop ctx ':round))
  (define ctx* (ctx-update-props ctx props))
  (define new-prec (ctx-lookup-prop ctx* ':precision))
  (define new-round (ctx-lookup-prop ctx* ':round))
  (define body-ctx
    (parameterize ([current-output-port (open-output-nowhere)])
      (let-values ([(_ body-ctx) (visit/ctx vtor body ctx*)])
        body-ctx)))
  (define body-prec (ctx-lookup-prop body-ctx ':precision))
  (define-values (ctx** tmpvar)
    (let ([ctx** (ctx-update-props ctx* `(:precision ,body-prec))])
      (ctx-random-name ctx**)))
  (unless (equal? curr-round new-round)
    (printf "~a" (compile-round-mode new-round ctx)))
  (define-values (body* _) (visit/ctx vtor body ctx*))
  (printf "~a~a\n" indent (compile-declaration tmpvar body* ctx**))
  (unless (equal? curr-round new-round)
    (printf "~a" (compile-round-mode curr-round ctx)))
  (values tmpvar body-ctx))

(define (visit-op_/imperative vtor op args #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define args*
    (for/list ([arg args])
      (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
      (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
      (if (equal? prec arg-prec)
          arg*
          (compile-implicit-round op arg* arg-ctx ctx))))
  (values (compile-operator op args* ctx)
          (if (set-member? bool-ops op)
              (ctx-update-props ctx (list ':precision 'boolean))
              ctx)))

(define (visit-call/imperative vtor fn args #:ctx ctx)
  (define args*
    (for/list ([arg args])
      (define-values (arg* _) (visit/ctx vtor arg ctx))
      arg*))
  (values (compile-function fn args ctx) ctx))

(define (visit-digits/imperative vtor m e b #:ctx ctx)
  (visit/ctx vtor (digits->number m e b) ctx))

(define (visit-number/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx) ctx))

(define (visit-constant/imperative vtor x #:ctx ctx)
  (values (compile-constant x ctx)
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props ctx (list ':precision 'boolean))
              ctx)))

(define (visit-symbol/imperative vtor x #:ctx ctx)
  (define name (ctx-lookup-name ctx x))
  (define var-prec (ctx-lookup-prec ctx name))
  (values name (ctx-update-props ctx `(:precision ,var-prec))))

(define-expr-visitor default-compiler-visitor imperative-visitor
  [visit-if visit-if/imperative]
  [visit-let_ visit-let_/imperative]
  [visit-while_ visit-while_/imperative]
  [visit-cast visit-cast/imperative]
  [visit-! visit-!/imperative]
  [visit-call visit-call/imperative]
  [visit-op_ visit-op_/imperative]
  [visit-digits visit-digits/imperative]
  [visit-number visit-number/imperative]
  [visit-constant visit-constant/imperative]
  [visit-symbol visit-symbol/imperative])

;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-imperative-compiler name
                                  ; language behavior
                                  #:infix-ops [infix default-infix-ops]
                                  #:operator [operator default-compile-operator]
                                  #:constant [constant default-compile-constant]
                                  #:type [type default-compile-type]
                                  #:declare [declare default-compile-declaration]
                                  #:assign [assign default-compile-assignment]
                                  #:round [round default-compile-round]
                                  #:implicit-round [implicit-round default-compile-implicit-round]
                                  #:round-mode [round-mode default-compile-round-mode]
                                  #:use-vars [use-vars default-use-vars]
                                  #:program [program default-compile-program]
                                  #:flags [flags '()]
                                  ; visitor behavior
                                  #:visitor [vtor imperative-visitor]
                                  #:reserved [reserved '()]
                                  #:fix-name [fix-name-proc fix-name]
                                  #:indent [indent "\t"])
  (unless (andmap valid-flag? flags)
    (error 'make-imperative-compiler "undefined imperative flags: ~a" flags))
  (when (flag-conflict? flags)
    (error 'make-imperative-compiler "conflicting flags: ~a" flags))
  (define language
    (imperative name infix operator constant type
                declare assign round implicit-round round-mode
                use-vars program flags))
  (lambda (prog name)
    (parameterize ([*gensym-used-names* (mutable-set)] 
                   [*gensym-collisions* 1]
                   [*gensym-fix-name* fix-name-proc]
                   [*imperative-lang* language])
      (define-values (args props body)
        (match prog
         [(list 'FPCore (list args ...) props ... body) (values args props body)]
         [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
      (define ctx
        (let ([ctx0 (ctx-update-props default-ctx props)])
          (let ([ctx1 (ctx-reserve-names ctx0 reserved)])
            (ctx-set-extra ctx1 'indent indent))))

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

      (define non-varnames (cons fname (map (curry ctx-lookup-name ctx) reserved)))
      (define p (open-output-string))
      (define-values (body* ret used-vars)
        (parameterize ([current-output-port p])
          (define-values (o cx) (visit-body vtor body ctx))
          (values (get-output-string p)
                  (trim-infix-parens o)
                  (remove* non-varnames (set->list (*gensym-used-names*))))))
      (compile-program fname arg-names arg-ctxs body* ret ctx used-vars))))

(module+ test
  (require rackunit)
  (define compile0 (make-imperative-compiler "default"))
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
