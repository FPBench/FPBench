#lang racket

(require generic-flonum)
(require "ml.rkt")

(provide haskell-header core->haskell type->haskell haskell-supported)

(define haskell-header (const "import Numeric\n\n"))

(define haskell-supported
  (supported-list
    (invert-op-proc
      (curry set-member?
        '(cbrt ceil erf erfc exp2 fdim floor fma fmod hypot isnormal lgamma
          log2 log10 nearbyint remainder round signbit tgamma trunc)))
    (curry set-member? '(TRUE FALSE INFINITY NAN PI E))
    (curry set-member? '(binary64 binary32))
    (curry equal? 'nearestEven)))

(define haskell-reserved    ; Language-specific reserved names (avoid name collision)
  '(as case class data default deriving do else family forall foreign
    hiding if import in infix infixl infixr instance let mdo module
    newtype of proc qualified rec type where))

(define (fix-name name)
  (apply string-append
    (for/list ([char (~a name)])
      (if (regexp-match #rx"[a-zA-Z0-9]" (string char))
          (string (char-downcase char))
          (format "~a" (char->integer char))))))

(define/match (type->haskell type)
  [('binary64) "Double"]
  [('binary32) "Float"]
  [('boolean) "Bool"])

(define (operator->haskell op args ctx)
  (match (cons op args)
   [(list '!= args ...)
    (format "(~a)"
            (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                      (for/list ([b (cdr args)])
                        (format "~a /= ~a" (car args) b))
                      (loop (cdr args)))))
              " && "))]
   [(list '- a) (format "(negate ~a)" a)]
   [(list '- a b) (format "(~a - ~a)" a b)]
   [(list 'isfinite a) (format "((not (isNaN ~a)) && (not (isInfinite ~a)))" a a)]
   [(list 'isinf a) (format "(isInfinite ~a)" a)]
   [(list 'isnan a) (format "(isNaN ~a)" a)]
   [(list 'copysign a b) (format "((signum ~a) * (abs ~a))" b a)]
   [(list 'fabs a) (format "(abs ~a)" a)]
   [(list 'fmax a b) (format "(max ~a ~a)" a b)]
   [(list 'fmin a b) (format "(min ~a ~a)" a b)]
   [(list 'pow a b) (format "(~a ** ~a)" a b)]
   [_ (format "(~a ~a)" op (string-join args " "))]))

(define (constant->haskell x ctx)
  (define type (type->haskell (ctx-lookup-prop ctx ':precision)))
  (match x
   ['TRUE "True"]
   ['FALSE "False"]
   ['INFINITY "(1.0 / 0.0)"]
   ['NAN "(0.0 / 0.0)"]
   ['PI "pi"]
   ['E "exp 1.0"]
   [(? hex?) (format "(~a :: ~a)" (real->double-flonum (hex->racket x)) type)]
   [(? number?) (format "(~a :: ~a)" (real->double-flonum x) type)]
   [(? symbol?) (~a x)]))

(define (params->haskell args)
  (if (null? args) "" (string-join args " ")))

(define (contract->haskell itypes otype)
  (if (null? itypes)
      (~a otype)
      (format "~a -> ~a" (string-join itypes " -> ") otype)))

(define (body-is-multi-lined? body)
  (or (string-contains? body "if")
      (string-contains? body "let")))

(define (program->haskell name args arg-ctxs body ctx)
  (define type (type->haskell (ctx-lookup-prop ctx ':precision)))
  (define arg-types (map (compose type->haskell (curryr ctx-lookup-prop ':precision)) arg-ctxs))
  (define contract (contract->haskell arg-types type))
  (define args* (params->haskell args))
  (if (body-is-multi-lined? body)
      (format "~a :: ~a\n~a ~a =\n  ~a\n" name contract name args* body)
      (format "~a :: ~a\n~a ~a = ~a\n" name contract name args* body)))

(define-expr-visitor ml-visitor haskell-visitor
  [(visit-if vtor cond ift iff #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define cond-indent (string-append single-indent half-indent))
    (define if-indent (string-append double-indent single-indent half-indent))
    (printf "if ")
    (define-values (cond* _)
      (let ([ctx0 (ctx-set-extra ctx 'indent (format "~a~a" indent cond-indent))])
        (visit/ctx vtor cond ctx0)))
    (printf "~a\n~a~athen " (trim-infix-parens cond*) indent single-indent)
    (define-values (ift* ift-ctx)
      (let ([ctx0 (ctx-set-extra ctx 'indent (format "~a~a" indent if-indent))])
        (visit/ctx vtor ift ctx0)))
    (printf "~a\n~a~aelse " ift* indent single-indent)
    (define-values (iff* iff-ctx)
      (let ([ctx0 (ctx-set-extra ctx 'indent (format "~a~a" indent if-indent))])
        (visit/ctx vtor iff ctx0)))
    (printf "~a" iff*)
    (values "" ift-ctx)]

  [(visit-let_ vtor let_ vars vals body #:ctx ctx)
    (define indent (ctx-lookup-extra ctx 'indent))
    (define inner-indent (string-append double-indent single-indent half-indent))
    (printf "let ")
    (define ctx*
      (for/fold ([ctx* ctx] #:result ctx*)
                ([var (in-list vars)] [val (in-list vals)] [i (in-naturals)])
        (define val-ctx (match let_ ['let ctx] ['let* ctx*]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx* var prec)))))
        (printf (if (zero? i) "~a = " (format "~a~a~~a = " indent double-indent)) name)
        (define nindent (apply string-append (make-list (string-length name) half-indent)))
        (define val-ctx*
          (ctx-update-extra val-ctx 'indent
                            (curry format "~a~a~a" inner-indent nindent)))
        (printf "~a\n" (let-values ([(val* _) (visit/ctx vtor val val-ctx*)])
                         (trim-infix-parens val*)))
        name-ctx))
    (printf "~ain  " indent)
    (define-values (body* body-ctx)
      (let ([ctx0 (ctx-set-extra ctx* 'indent (format "~a~a" indent double-indent))])
        (visit/ctx vtor body ctx0)))
    (printf "~a" (trim-infix-parens body*))
    (values "" body-ctx)]

  ; let
  ;   vars = vals
  ;   ...
  ;   loop vars ... =
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
    (define inner-indent (string-append double-indent single-indent half-indent))
    (define quad-indent (string-append double-indent double-indent))
    (define inner-cond-indent (string-append quad-indent half-indent))
    (define inner-if-indent (string-append quad-indent double-indent single-indent))
    (define inner-let-indent (string-append quad-indent quad-indent half-indent))
    (define-values (while-ctx fn-name) (ctx-unique-name ctx 'loop))
    (printf "let ")
    (define-values (ctx* vars*)                             ; loop variables
      (for/fold ([ctx* while-ctx] [vars* '()]
                #:result (values ctx* (reverse vars*)))
                ([var (in-list vars)] [val (in-list inits)] [i (in-naturals)])
        (define val-ctx (match while_ ['while while-ctx] ['while* ctx*]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx* var prec)))))
        (printf (if (zero? i) "~a = " (format "~a~a~~a = " indent double-indent)) name)
        (define nindent (apply string-append (make-list (string-length name) half-indent)))
        (printf "~a\n"
                (let ([ctx0 (ctx-update-extra val-ctx 'indent
                                              (curry format "~a~a~a" inner-indent nindent))])
                  (let-values ([(val* _) (visit/ctx vtor val ctx0)])
                    (trim-infix-parens val*))))
        (values name-ctx (cons name vars*))))
    (printf "~a~a~a ~a =\n" indent double-indent fn-name (string-join vars* " "))
    (printf "~a~a~aif " indent double-indent single-indent)
    (define-values (cond* _)
      (let ([ctx0 (ctx-set-extra ctx* 'indent (format "~a~a" indent inner-cond-indent))])
        (visit/ctx vtor cond ctx0)))    ; condition
    (printf "~a\n~a~a~athen let " (trim-infix-parens cond*)
            indent double-indent double-indent)
    (define-values (ctx** vars**)                           ; loop update
      (for/fold ([ctx** ctx*] [vars* '()] #:result (values ctx** (reverse vars*)))
                ([var (in-list vars)] [val (in-list updates)] [i (in-naturals)])
        (define val-ctx (match while_ ['while ctx*] ['while* ctx**]))
        (define-values (name-ctx name)    ; messy workaround to get val context
          (parameterize ([current-output-port (open-output-nowhere)])
            (let-values ([(_ var-ctx) (visit/ctx vtor val val-ctx)])
              (let ([prec (ctx-lookup-prop val-ctx ':precision)])
                (ctx-unique-name ctx** var prec)))))
        (printf (if (zero? i) "~a = " (format "~a~a~~a = " indent inner-let-indent)) name)
        (define nindent (apply string-append (make-list (string-length name) half-indent)))
        (printf "~a\n" 
                (let ([ctx0 (ctx-update-extra val-ctx 'indent
                                             (curry format "~a~a~a~a~a" inner-let-indent single-indent
                                                                        half-indent nindent))])
                  (let-values ([(val* _) (visit/ctx vtor val ctx0)])
                    (trim-infix-parens val*))))
        (values name-ctx (cons name vars*))))
    (printf "~a~a~a~ain  " indent quad-indent double-indent half-indent)
    (printf "~a ~a\n" fn-name (string-join vars** " "))   ; call the loop
    (printf "~a~aelse " indent quad-indent)
    (define-values (body* body-ctx)
      (let ([ctx0 (ctx-set-extra ctx* 'indent (format "~a~a" indent inner-if-indent))])
        (visit/ctx vtor body ctx0)))
    (printf "~a\n~ain  " (trim-infix-parens body*) indent)
    (printf "~a ~a"  fn-name (string-join vars* " "))
    (values "" body-ctx)])

(define core->haskell
  (make-ml-compiler "haskell"
    #:infix-ops (remove* '(!= not -) default-infix-ops)
    #:operator operator->haskell
    #:constant constant->haskell
    #:program program->haskell
    #:visitor haskell-visitor
    #:fix-name fix-name
    #:reserved haskell-reserved))

(define-compiler '("hs") haskell-header core->haskell (const "") haskell-supported)
