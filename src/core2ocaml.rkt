#lang racket

(require "ml.rkt")
(provide core->ocaml ocaml-header ocaml-supported)

; 'cast' is a no-op since only one precision is supported
(define ocaml-supported
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(acosh asinh atanh cbrt erf erfc exp2 fdim
          lgamma log2 nearbyint remainder tgamma
          array dim size ref for for* tensor tensor*)))
    (curry set-member? '(TRUE FALSE INFINITY NAN MAX_VALUE PI E))
    (curry equal? 'binary64)
    (curry equal? 'nearestEven)
    #f))

(define ocaml-reserved          ; Language-specific reserved names (avoid name collision)
  '(and as asssert asr begin class constraint do done down to else
    end exception external false for fun function funtor if in
    include inherit initializer land lazy let lor lsl lsr lxor
    match method mod module mutable new nonrec object of open
    or private rec sig struct then to true try type val virtual
    when while with))

(define ocaml-header
  (const
    (string-append
      "let c_fmax x y = if (Float.is_nan x) then y else if (Float.is_nan y) then x else (Float.max x y)\n\n"
      "let c_fmin x y = if (Float.is_nan x) then y else if (Float.is_nan y) then x else (Float.min x y)\n\n")))

(define (fix-name name)
  (apply string-append
    (for/list ([char (~a name)])
      (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
          (string (char-downcase char))
          (format "_~a_" (char->integer char))))))

(define (equality->ocaml x xs)
  (format "(~a)"
          (string-join
            (for/list ([a (cons x xs)] [b xs])
              (format "(~a = ~a)" a b))
            " && ")))

(define (inequality->ocaml x xs)
  (format "(not (~a))"
          (string-join
            (for/list ([a (cons x xs)] [b xs])
              (format "(~a = ~a)" a b))
            " || ")))

(define (operator->ocaml op args ctx)
  (match (cons op args)
   [(list '- a) (format "(Float.neg ~a)" a)]
   [(list '+ a b) (format "(Float.add ~a ~a)" a b)]
   [(list '- a b) (format "(Float.sub ~a ~a)" a b)]
   [(list '* a b) (format "(Float.mul ~a ~a)" a b)]
   [(list '/ a b) (format "(Float.div ~a ~a)" a b)]
   [(list '== arg args ...) (equality->ocaml arg args)]
   [(list '!= arg args ...) (inequality->ocaml arg args)]
   [_
    (define args* (string-join args " "))
    (match op
     ['isfinite (format "(Float.is_finite ~a)" args*)]
     ['isinf (format "(Float.is_infinite ~a)" args*)]
     ['isnan (format "(Float.is_nan ~a)" args*)]
     ['isnormal (format "((Float.classify_float ~a) == Float.FP_normal)" args*)]
     ['signbit (format "(Float.sign_bit ~a)" args*)]
     ['fabs (format "(Float.abs ~a)" args*)]
     ['fmax (format "(c_fmax ~a)" args*)]
     ['fmin (format "(c_fmin ~a)" args*)]
     ['fmod (format "(Float.rem ~a)" args*)]
     ['copysign (format "(Float.copy_sign ~a)" args*)]
     [_ (format "(Float.~a ~a)" op args*)])]))

(define (number->ocaml x)
  (if (negative? x)
      (format "(Float.neg ~a)" (abs (real->double-flonum x)))
      (~a (real->double-flonum x))))

(define (constant->ocaml x ctx)
  (match x
   ['TRUE "true"]
   ['FALSE "false"]
   ['INFINITY "Float.infinity"]
   ['NAN "Float.nan"]
   ['E "(Float.exp 1.0)"]
   ['PI "Float.pi"]
   ['MAXFLOAT "Float.max_float"]
   [(? hex?) (number->ocaml (hex->racket x))]
   [(? number?) (number->ocaml x)]
   [_ (~a x)]))

(define (params->ocaml args)
  (if (null? args)
      "()"
      (string-join args " ")))

(define (body-is-multi-lined? body)
  (or (string-contains? body "if")
      (string-contains? body "let")))

(define (program->ocaml name args arg-ctxs body ctx)
  (if (body-is-multi-lined? body)
      (format "let ~a ~a =\n  ~a\n" name (params->ocaml args) body)
      (format "let ~a ~a = ~a\n" name (params->ocaml args) body)))

(define-expr-visitor ml-visitor ocaml-visitor
  [(visit-let_ vtor let_ vars vals body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define ctx*
      (for/fold ([ctx* ctx] #:result ctx*)
                ([var (in-list vars)] [val (in-list vals)])
        (define val-ctx (match let_ ['let ctx] ['let* ctx*]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx* var prec)))))
        (printf "let ~a = " name)
        (define val-ctx* (ctx-update-extra val-ctx 'indent
                                           (curry format "~a~a" double-indent)))
        (printf "~a in\n~a" (let-values ([(val* _) (visit/ctx vtor val val-ctx*)])
                              (trim-infix-parens val*))
                            indent)
        name-ctx))
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (values body* body-ctx)]

  ; let var = val in
  ; ...
  ; let rec loop vars ... =
  ;   if cond then
  ;     let var = update in
  ;     ...
  ;     loop vars ...
  ;   else
  ;     body
  ; loop inits ...
  [(visit-while_ vtor while_ cond vars inits updates body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define-values (while-ctx fn-name) (ctx-unique-name ctx 'loop))
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
        (printf "let ~a = " name)
        (printf "~a in\n~a" (let-values ([(val* _) (visit/ctx vtor val val-ctx)])
                              (trim-infix-parens val*))
                            indent)
        (values name-ctx (cons name vars*))))
    (printf "let rec ~a ~a =\n" fn-name (string-join vars* " "))
    (printf "~a~aif " indent single-indent)
    (define-values (cond* _)          ; condition
      (let ([ctx0 (ctx-update-extra ctx* 'indent (curry format "~a~a" double-indent))])
        (visit/ctx vtor cond ctx0)))
    (printf "~a then\n~a" cond* indent)
    (define-values (ctx** vars**)                           ; loop update
      (for/fold ([ctx** ctx*] [vars* '()] #:result (values ctx** (reverse vars*)))
                ([var (in-list vars)] [val (in-list updates)])
        (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx** var prec)))))
        (printf "~alet ~a = " double-indent name)
        (define val-ctx*
          (ctx-update-extra val-ctx 'indent
                            (curry format "~a~a~a" double-indent single-indent)))
        (printf "~a in\n~a"
                (let-values ([(val* _) (visit/ctx vtor val val-ctx*)])
                  (trim-infix-parens val*))
                indent)
        (values name-ctx (cons name vars*))))
    (printf "~a~a~a ~a\n" double-indent single-indent fn-name (string-join vars** " "))   ; call the loop
    (printf "~a~aelse\n~a~a" indent single-indent indent double-indent)
    (define-values (body* body-ctx) (visit/ctx vtor body ctx*))
    (printf "~a\n~ain\n" (trim-infix-parens body*) indent)
    (values (format "~a~a ~a" indent fn-name (string-join vars* " "))
            body-ctx)])

(define core->ocaml
  (make-ml-compiler "ocaml"
    #:infix-ops (remove* '(+ - * / == !=) default-infix-ops)
    #:operator operator->ocaml
    #:constant constant->ocaml
    #:program program->ocaml
    #:visitor ocaml-visitor
    #:fix-name fix-name))

(define-compiler '("ocaml" "ml") ocaml-header core->ocaml (const "") ocaml-supported)
