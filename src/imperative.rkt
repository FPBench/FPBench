#lang racket

(require "common.rkt" "compilers.rkt")
(provide convert-core convert-expr *lang* language *reserved-names* *fix-name-format*)

;;; Abstraction for different languages

(struct language (name operator constant declaration assignment round round-mode function))
(define *lang* (make-parameter #f))

(define (convert-operator ctx operator args)
  (define args*
    (if (= (length args) 1)
        (list (trim-infix-parens (first args)))
        args))
  ((language-operator (*lang*)) (ctx-props ctx) operator args*))

(define (convert-constant ctx expr)
  ((language-constant (*lang*)) (ctx-props ctx) expr))

(define (convert-declaration ctx var [val #f])
  (define val* (if (string? val) (trim-infix-parens val) val))
  (if (equal? (language-name (*lang*)) "sollya")
      ((language-assignment (*lang*)) var val*)
      ((language-declaration (*lang*)) (ctx-props ctx) var val*)))
    
(define (convert-assignment var val)
  (define val* (if (string? val) (trim-infix-parens val) val))
  ((language-assignment (*lang*)) var val*))

(define (round-expr expr ctx [all? #f]) ; Sollya will never ignore but C can
  (if (or all? (equal? (language-name (*lang*)) "sollya"))
      ((language-round (*lang*)) expr (ctx-props ctx))
      expr))

(define (change-round-mode mode indent) ; C only
  ((language-round-mode (*lang*)) mode indent))

(define (convert-function name args arg-props body return ctx vars)
  ((language-function (*lang*)) name args arg-props body return ctx vars))

(define (while-name) ; Go only
  (if (equal? (language-name (*lang*)) "go") "for" "while"))

(define (format-cond str) ; Go only, if and while conditions
  (format (if (equal? (language-name (*lang*)) "go") "~a" "(~a)") str))

(define (use-vars vars indent) ; Go doesn't like unused variables
  (if (equal? (language-name (*lang*)) "go")
    (format "~aUse(~a)\n" indent (string-join vars ", "))
    ""))

;;; Compiler for imperative languages

(define *reserved-names* (make-parameter '()))
(define *fix-name-format* (make-parameter "_~a_"))

(define (fix-name name) ;; Common fix-name
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format (*fix-name-format*) (char->integer char))))
   ""))

(define (convert-application ctx operator args)
  (match (cons operator args)
    [(list '- a) (round-expr (format (if (string-prefix? a "-") "-(~a)" "-~a") a) ctx)]
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) "TRUE"]
    [(list (or '+ '- '*) a b) (round-expr (format "(~a ~a ~a)" a operator b) ctx)]  ; Division not included!! (Sollya issues)
    [(list (or '== '< '> '<= '>=) head args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "~a ~a ~a" a operator b))
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
     (format "(~a)" (string-join (map ~a a) " || "))]
    [(list (? operator? f) args ...)
     (convert-operator ctx operator args)]))

(define/match (collect-branches expr)
  [((list 'if cond ift iff))
   (cons (list cond ift) (collect-branches iff))]
  [(_) (list (list #t expr))])

;; Used by C (implicit casting issues)
(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary80)  4]
    [('binary64)  3]
    [('binary32)  2]
    [('integer)   1]
    [('boolean)   0])
  (- (prec->num prec1) (prec->num prec2)))

(define (boolean-op? op) ; correct return type
  (set-member? '(< > <= >= == != and or not isfinite isinf isnan isnormal signbit) op))

(define (convert-expr expr #:ctx [ctx (make-compiler-ctx)] #:indent [indent "\t"])
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let*-values ([(val* prec) (convert-expr val #:ctx ctx #:indent indent)]
                        [(cx name) (ctx-unique-name ctx* var prec)]
                        [(cx*) (ctx-update-props cx `(:precision ,prec))])
            (printf "~a~a\n" indent (convert-declaration cx* name val*))
            cx)))
      (printf "~a" (use-vars (for/list ([var vars]) (ctx-lookup-name ctx* var)) indent))
      (convert-expr body #:ctx ctx* #:indent indent)]

    [`(let* ([,vars ,vals] ...) ,body)
      (define ctx*
        (for/fold ([ctx* ctx]) ([var vars] [val vals])
          (let*-values ([(val* prec) (convert-expr val #:ctx ctx* #:indent indent)]
                        [(cx name) (ctx-unique-name ctx* var prec)]
                        [(cx*) (ctx-update-props cx `(:precision ,prec))])
            (printf "~a~a\n" indent (convert-declaration cx* name val*))
            cx)))
      (printf "~a" (use-vars (for/list ([var vars]) (ctx-lookup-name ctx* var)) indent))
      (convert-expr body #:ctx ctx* #:indent indent)]

    ; TODO: what if the values in each branch store values rounded at different precisions ???
    [`(if ,cond ,ift ,iff)
      (if (equal? (language-name (*lang*)) "scala")   ; Scala/Daisy has a very different if
        (let*-values ([(cond* cond-prec) (convert-expr cond #:ctx ctx #:indent indent)]
                      [(ift* ift-prec) (convert-expr ift #:ctx ctx #:indent (format "\t\t~a" indent))]
                      [(iff* iff-prec) (convert-expr iff #:ctx ctx #:indent (format "\t\t~a" indent))]
                      [(ctx* outvar) (ctx-random-name (ctx-update-props ctx `(:precision ,ift-prec)))])
          (printf "~a~a\n" indent
            (convert-declaration ctx* outvar
              (format "\n~a\tif (~a) {\n~a\t\t~a\n~a\t} else {\n~a\t\t~a\n~a\t}"
                      indent cond* indent ift* indent indent iff* indent)))
          (values outvar ift-prec))

        ; collect branches and translate conditions above the if/elif/else
        (let ([branches
                (let loop ([expr expr])
                  (match expr
                   [(list 'if cond ift iff)
                    (define-values (cond* _) (convert-expr cond #:ctx ctx #:indent indent))
                    (cons (list cond* ift) (loop iff))]
                   [_ (list (list #t expr))]))])
          ; translate branches here
          (let loop ([branches branches] [first? #t] [out #f])
            (match (cons first? (car branches))
             [(list #t cond ift) ; first branch => if branch
              (define-values (ctx* outvar) ; messy workaround, convert ift twice
                (parameterize ([current-output-port (open-output-nowhere)])
                  (let-values ([(_ ift-prec) (convert-expr ift #:ctx ctx #:indent (format "~a\t" indent))])
                    (ctx-random-name (ctx-update-props ctx `(:precision ,ift-prec))))))
              (if (equal? (language-name (*lang*)) "sollya") ; Sollya has slightly different if
                  (printf "~aif ~a then {\n" indent (format-cond (trim-infix-parens cond)))
                  (printf "~a~a\n~aif ~a {\n" indent (convert-declaration ctx* outvar)
                          indent (format-cond (trim-infix-parens cond))))
              (define-values (ift* ift-prec) (convert-expr ift #:ctx ctx #:indent (format "~a\t" indent)))
              (printf "~a\t~a\n" indent (convert-assignment outvar ift*))
              (loop (cdr branches) #f (cons outvar ift-prec))]
             [(list _ #t else) ; branch condition #t => else branch
              (printf "~a} else {\n" indent)
              (define-values (else* else-prec) (convert-expr else #:ctx ctx #:indent (format "~a\t" indent)))
              (printf "~a\t~a\n" indent (convert-assignment (car out) else*))
              (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))
              (values (car out) (cdr out))]
             [(list _ cond elif) ; otherwise => else if branch
              (printf
                (if (equal? (language-name (*lang*)) "sollya") ; Sollya has slightly different else if
                    "~a} else if ~a then {\n"
                    "~a} else if ~a {\n")
                indent (format-cond (trim-infix-parens cond)))
              (define-values (elif* elif-prec) (convert-expr elif #:ctx ctx #:indent (format "~a\t" indent)))
              (printf "~a\t~a\n" indent (convert-assignment (car out) elif*))
              (loop (cdr branches) first? out)]))))]

    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let*-values ([(val* prec) (convert-expr val #:ctx ctx #:indent indent)]
                        [(cx name) (ctx-unique-name ctx* var prec)]
                        [(cx*) (ctx-update-props cx `(:precision ,prec))])
            (printf "~a~a\n" indent (convert-declaration cx name val*))
            (values cx (flatten (cons vars* name))))))
      (define test-var
        (let-values ([(cx name) (ctx-random-name ctx)])
            (set! ctx cx)
            name))
      (printf "~a" (use-vars vars* indent))

      (define-values (cond* cond*-prec) (convert-expr cond #:ctx ctx* #:indent indent))
      (printf "~a~a\n" indent
              (convert-declaration (ctx-update-props ctx '(:precision boolean))
                                   test-var cond*))
      ; Sollya has slightly different while
      (if (equal? (language-name (*lang*)) "sollya")
          (printf "~awhile ~a do {\n" indent (format-cond test-var))
          (printf "~a~a ~a {\n" indent (while-name) (format-cond test-var)))
      (define-values (ctx** vars**)
        (for/fold ([ctx** ctx*] [vars** '()]) ([var vars] [update updates])
          (let*-values ([(val* prec) (convert-expr update #:ctx ctx* #:indent indent*)]
                        [(cx name) (ctx-unique-name ctx* var prec)]
                        [(cx*) (ctx-update-props cx `(:precision ,prec))])
            (printf "~a\t~a\n" indent (convert-declaration cx* name val*))
            (values cx (flatten (cons vars** name))))))
      (printf "~a" (use-vars vars** indent*))
      (for ([var* vars*] [var** vars**])
          (printf "~a\t~a\n" indent (convert-assignment var* var**)))

      (define-values (cond** cond**-prec) (convert-expr cond #:ctx ctx** #:indent indent*))
      (printf "~a\t~a\n" indent (convert-assignment test-var cond**))
      (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    [`(while* ,cond ([,vars ,inits ,updates] ...) ,retexpr)
      (define indent* (format "~a\t" indent))
      (define-values (ctx* vars*)
        (for/fold ([ctx* ctx] [vars* '()]) ([var vars] [val inits])
          (let*-values ([(val* prec) (convert-expr val #:ctx ctx* #:indent indent)]
                        [(cx name) (ctx-unique-name ctx* var prec)]
                        [(cx*) (ctx-update-props cx `(:precision ,prec))])
            (printf "~a~a\n" indent (convert-declaration cx* name val*))
            (values cx (flatten (cons vars* name))))))
      (define test-var
        (let-values ([(cx name) (ctx-random-name ctx)])
            (set! ctx cx)
            name))
      (printf "~a" (use-vars vars* indent))

      (define-values (cond* cond*-prec) (convert-expr cond #:ctx ctx* #:indent indent))
      (printf "~a~a\n" indent
          (convert-declaration (ctx-update-props ctx '(:precision boolean))
                               test-var cond*))
      ; Sollya has slightly different while*
      (if (equal? (language-name (*lang*)) "sollya")
          (printf "~awhile ~a do {\n" indent (format-cond test-var))
          (printf "~a~a ~a {\n" indent (while-name) (format-cond test-var)))
      (for ([var* vars*] [update updates])
        (let-values ([(val* _) (convert-expr update #:ctx ctx* #:indent indent*)])
          (printf "~a\t~a\n" indent (convert-assignment var* val*))))

      (define-values (cond** cond**-prec) (convert-expr cond #:ctx ctx* #:indent indent*))
      (printf "~a\t~a\n" indent (convert-assignment test-var cond**))
      (printf "~a}~a\n" indent (if (equal? (language-name (*lang*)) "sollya") ";" ""))
      (convert-expr retexpr #:ctx ctx* #:indent indent)]

    ; Ignore all casts
    [`(cast ,body) 
      (define-values (body* _) (convert-expr body #:ctx ctx #:indent indent))
      (values (round-expr body* ctx #t) (ctx-lookup-prop ctx ':precision))]

    [`(! ,props ... ,body)
      (define curr-prec (ctx-lookup-prop ctx ':precision 'nearestEven))
      (define curr-round (ctx-lookup-prop ctx ':round 'nearestEven))
      (define new-prec (dict-ref (apply hash-set* #hash() props) ':precision curr-prec))
      (define new-round (dict-ref (apply hash-set* #hash() props) ':round curr-round))
      (cond
       [(and (or (equal? (language-name (*lang*)) "c")  ; Only C and Scala need to emit a temporary variable
                 (equal? (language-name (*lang*)) "scala"))
             (or (not (equal? curr-prec new-prec))
                 (not (equal? curr-round new-round))))
        (define ctx* (ctx-update-props ctx props))
        (define-values (body* body-prec) (convert-expr body #:ctx ctx* #:indent indent))
        (if (and (equal? curr-round new-round) (equal? curr-prec body-prec))
            (values body* body-prec)
            (let-values ([(ctx** var) (ctx-random-name (ctx-update-props ctx* `(:precision ,body-prec)))])
              (unless (equal? curr-round new-round) 
                (printf "~a~a" indent (change-round-mode new-round indent)))
              (printf "~a~a\n" indent (convert-declaration ctx** var body*))
              (unless (equal? curr-round new-round)
                (printf "~a~a" indent (change-round-mode curr-round indent)))
              (if (equal? new-prec body-prec)
                  (values var body-prec)
                  (values (round-expr var ctx #t) body-prec))))]
      [else
       (convert-expr body #:ctx (ctx-update-props ctx props) #:indent indent)])]

    [(list (? operator? operator) args ...)
      (define-values (args* precs*)
        (for/lists (args* precs*) ([arg args])
          (convert-expr arg #:ctx ctx #:indent indent)))

      ; C implicit casting: check if arguments need to be explicitly casted
      ; #f if no casting, -1 if cast result, 1 if cast args
      (define cast
        (and (equal? (language-name (*lang*)) "c")
             (set-member? `(+ - * /) operator)
             (andmap (negate (curry equal? (ctx-lookup-prop ctx ':precision))) precs*)
             (cmp-prec (ctx-lookup-prop ctx ':precision) (car precs*))))
      ;; TODO: Warn when higher precision arguments passed to lower precision operators
      ;;      (C only, exclude +,-,*,/,==,!=,>,<,>=,<=)
      (define args**
        (for/list ([arg* args*] [prec* precs*])
          (if (and cast (positive? cast) ; Cast if needed
                   (not (equal? (ctx-lookup-prop ctx ':precision) prec*)))
              (round-expr arg* ctx #t)
              arg*)))
      (values
        (if (and cast (negative? cast))
            (round-expr (convert-application ctx operator args**) ctx #t)
            (convert-application ctx operator args**))
        (if (boolean-op? operator) 'boolean (ctx-lookup-prop ctx ':precision)))]

    [(list digits m e b) 
      (values (convert-constant ctx (digits->number m e b)) (ctx-lookup-prop ctx ':precision))]
    [(? constant?)
      (values 
        (convert-constant ctx expr)
        (if (set-member? '(TRUE FALSE) expr)
            'boolean
            (ctx-lookup-prop ctx ':precision)))]
    [(or (? constant?) (? number?) (? hex?))
      (values (convert-constant ctx expr) (ctx-lookup-prop ctx ':precision))]
    [(? symbol?)
      (values (ctx-lookup-name ctx expr) (ctx-lookup-prec ctx expr))]))

(define (convert-core prog name)
  (parameterize ([*used-names* (mutable-set)] 
                 [*gensym-collisions* 1] 
                 [*gensym-fix-name* fix-name])
    (define-values (args props body)
     (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define default-ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
    (define ctx (ctx-reserve-names default-ctx (*reserved-names*)))

    (define func-name 
      (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
        (set! ctx cx)
        fname)) 

    (define-values (arg-names arg-props)
      (for/lists (n p) ([var args])
        (match var
          [(list '! props ... name) 
            (values 
                (let-values ([(cx name) (ctx-unique-name ctx name)])
                            (set! ctx cx)
                            name)
                (apply hash-set* (ctx-props ctx) props))]
          [name 
            (values 
                (let-values ([(cx name) (ctx-unique-name ctx name)])
                            (set! ctx cx)
                            name)
                (ctx-props ctx))])))

    (define non-varnames (map (curry ctx-lookup-name ctx) (*reserved-names*)))
    (define indent (if (equal? (language-name (*lang*)) "scala") "\t\t" "\t"))

    (define-values (body-out return-out) 
      (let ([p (open-output-string)])
        (parameterize ([current-output-port p])
          (define-values (out _) (convert-expr body #:ctx ctx #:indent indent))
          (values (get-output-string p) out))))
    (convert-function func-name arg-names arg-props body-out return-out ctx (remove* non-varnames (set->list (*used-names*))))))