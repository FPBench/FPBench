#lang racket

(require "common.rkt" "compilers.rkt" "supported.rkt")
(provide expr->tex core->tex tex-supported)

(define tex-supported 
  (supported-list
    (negate (curry set-member? '(let let* while while*)))
    (curry set-member? '(PI E INFINITY NAN TRUE FALSE))
    (const #t)
    ieee754-rounding-modes))

;;
;;  This compiler is adapted from Herbie
;;

; self-paren-level : #t --> paren me
;                    #f --> do not paren me
;
; args-paren-level : #t --> do not paren args
;                    #f --> paren args
(define precedence-ordering '(#t + * fn #f))

(define (precedence< a b)
  (< (index-of precedence-ordering a)
     (index-of precedence-ordering b)))

(define (precedence-levels op)
  (match op
    [(or '+ '- 'neg 'or) (values '+ '+)]
    [(or '* 'and) (values '* '*)]
    ['/ (values #f #t)]
    ['pow (values #f #f)]
    ['atan2 (values 'fn #t)]
    ['if (values #t #t)]
    [(or 'remainder 'fmod) (values #t #f)]
    [(or 'cbrt 'ceil 'copysign 'exp2 'floor 'fmax 'exp 'sqrt 'fmin 'fabs 'fdim  'expm1 'fma 'log1p 'hypot 'j0 'j1 'y0 'y1 'lgamma 'tgamma 'trunc)
     (values #f #t)]
    [(or '== '< '> '<= '>= '!=)
     (values #f #t)]
    [_ (values 'fn #f)]))

;; Compiler

(define (fix-name name) ;; Imperative fix-name
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "_~a" (char->integer char))))
   ""))

; fix name for replacing special characters in TeX
(define (fix-op-name name)
  (string-join
    (for/list ([char (~a name)])
     (if (regexp-match #rx"[-\\_&%\\$\\{\\}]" (string char))
          "\\_"
         (string char)))
   ""))

(define/match (variable->tex expr)
  [('l)       "\\ell"]
  [('eps)     "\\varepsilon"]
  [('epsilon) "\\varepsilon"]
  [('alpha)   "\\alpha"]
  [('beta)    "\\beta"]
  [('gamma)   "\\gamma"]
  [('phi)     "\\phi"]
  [('phi1)    "\\phi_1"]
  [('phi2)    "\\phi_2"]
  [('lambda)  "\\lambda"]
  [('lambda1) "\\lambda_1"]
  [('lambda2) "\\lambda_2"]
  [(_) (symbol->string expr)])

(define/match (constant->tex expr)
  [('PI)            "\\pi"]
  [('E)             "e"]
  [('INFINITY)      "\\infty"]
  [('NAN)           "\\mathsf{NaN}"]
  [('TRUE)          "\\top"]
  [('FALSE)         "\\perp"])

(define/match (round-mode->tex expr)
  [('nearestEven)   "RNE"]
  [('toPositive)    "RTP"]
  [('toNegative)    "RTN"]
  [('toZero)        "RTZ"])

(define (operator->tex op args)
  (match op
   ['==         (format "~a = ~a" (first args) (second args))]
   ['>          (format "~a > ~a" (first args) (second args))]
   ['<          (format "~a < ~a" (first args) (second args))]
   ['>=         (format "~a \\geq ~a" (first args) (second args))]
   ['<=         (format "~a \\leq ~a" (first args) (second args))]
   ['*          (format "~a \\cdot ~a" (first args) (second args))]
   ['/          (format "\\frac{~a}{~a}" (first args) (second args))]
   ['atan2      (format "\\tan^{-1}_* \\frac{~a}{~a}" (first args) (second args))]
   ['cbrt       (format "\\sqrt[3]{~a}" (first args))]
   ['ceil       (format "\\left\\lceil~a\\right\\rceil" (first args))]
   ['exp        (format "e^{~a}" (first args))]
   ['exp2       (format "2^{~a}" (first args))]
   ['fabs       (format "\\left|~a\\right|" (first args))]
   ['floor      (format "\\left\\lfloor~a\\right\\rfloor" (first args))]
   ['fma        (format "\\mathsf{fma}\\left(~a\\right)" (string-join args ", "))]
   ['fmax       (format "\\mathsf{max}\\left(~a, ~a\\right)" (first args) (second args))]
   ['fmin       (format "\\mathsf{min}\\left(~a, ~a\\right)" (first args) (second args))]
   ['fmod       (format "~a \\bmod ~a" (first args) (second args))]
   ['j0         (format "j_0\\left(~a\\right)" (first args))]
   ['j1         (format "j_1\\left(~a\\right)" (first args))]
   ['log10      (format "\\log_{10} ~a" (first args))]
   ['log2       (format "\\log_{2} ~a" (first args))]
   ['logb       (format "\\log_{b} ~a" (first args))]
   ['pow        (format "{~a}^{~a}" (first args) (second args))]
   ['remainder  (format "~a \\mathsf{rem} ~a" (first args) (second args))]
   ['rint       (format "\\mathsf{rint} ~a" (first args))]
   ['round      (format "\\mathsf{round} ~a" (first args))]
   ['sqrt       (format "\\sqrt{~a}" (first args))]
   ['tgamma     (format "\\Gamma\\left(~a\\right)" (first args))]
   ['y0         (format "y_0\\left(~a\\right)" (first args))]
   ['y1         (format "y_1\\left(~a\\right)" (first args))]
   ['complex    (format "~a + ~a i" (first args) (second args))]
   ['re         (format "\\Re(~a)" (first args))]
   ['im         (format "\\Im(~a)" (first args))]
   ['conj       (format "\\overline{~a}")]
   [(or 'sin 'cos 'tan 'sinh 'cosh 'tanh 'log)
    (format "\\~a ~a" op (first args))]
   [(or 'acos 'acosh 'asin 'asinh 'atan 'atanh)
    (format "\\~a^{-1} ~a" (substring (~a op) 1) (first args))]
   [(or 'erf 'erfc 'expm1 'lgamma 'log1p 'trunc)
    (format "\\mathsf{~a}\\left(~a\\right)" op (first args))]
   [(or 'copysign 'fdim 'hypot)
    (format "\\mathsf{~a}\\left(~a, ~a\\right)" op (first args) (second args))]
   [_ (format "\\mathsf{~a}\\left(~a\\right)" (fix-op-name op) (string-join args ", "))]))
   
(define (application->tex op args)
  (match (cons op args)
   [(list '- a) (format "-~a" a)]
   [(list (or '+ '- ) a b) (format "~a ~a ~a" a op b)]
   [(list (or '== '< '> '<= '>=) head args ...)
    (string-join
      (for/list ([a (cons head args)] [b args])
        (operator->tex op (list a b)))
      " \\land ")]
   [(list '!= args ...)
    (string-join
      (let loop ([args args])
        (if (null? args)
            '()
            (append
              (for/list ([b (cdr args)])
                (format "~a \\ne ~a" (car args) b))
              (loop (cdr args)))))
      " \\land ")]
   [(list 'not a) (format "\\neg ~a" a)]
   [(list 'and a ...) (string-join (map ~a a) " \\land ")]
   [(list 'or a ...) (string-join (map ~a a) " \\lor ")]
   [(list op args ...) (operator->tex op args)]))

(define (collect-branches expr loc)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift loc)
           (collect-branches iff (cons 3 loc)))]
    [else
     (list (list #t expr loc))]))

(define (expr->tex* expr ctx color-loc color)
    "Compile an expression to math mode TeX."
  (let texify ([expr expr] [ctx ctx] [parens #t] [loc '(2)])
    (format
      (if (and color-loc (equal? (reverse color-loc) loc))
        (format "\\color{~a}{~~a}" color)
        "~a")
      (match expr
        [`(if ,cond ,ift ,iff)
         (define NL "\\\\\n")
         (define IND "\\;\\;\\;\\;")
         (with-output-to-string
           (λ ()
             (printf "\\begin{array}{l}\n")
             (for ([branch (collect-branches expr loc)] [n (in-naturals)])
               (match branch
                 [(list #t bexpr bloc)
                  (printf "\\mathbf{else}:~a~a~a~a\n"
                          NL IND (texify bexpr ctx #t (cons 2 bloc)) NL)]
                 [(list bcond bexpr bloc)
                  (printf "\\mathbf{~a}\\;~a:~a~a~a~a\n"
                          (if (= n 0) "if" "elif")
                          (texify bcond ctx #t (cons 1 bloc))
                          NL IND (texify bexpr ctx #t (cons 2 bloc)) NL)]))
             (printf "\\end{array}")))]

        [`(cast ,body)
          (format "\\langle ~a \\rangle_{\\text{~a}}"
                  (texify body ctx parens loc) (ctx-lookup-prop ctx ':precision))]

        [`(! ,props ... ,body) 
          (define curr-prec (ctx-lookup-prop ctx ':precision))
          (define curr-rnd (ctx-lookup-prop ctx ':round))
          (define ctx* (ctx-update-props ctx props))
          (define body* (texify body ctx* parens loc))
          (define new-prec (ctx-lookup-prop ctx* ':precision curr-prec))
          (define new-rnd (ctx-lookup-prop ctx* ':round curr-rnd))
          (cond
            [(and (not (equal? curr-prec new-prec)) (not (equal? curr-rnd new-rnd)))
             (format "\\left( ~a \\right)_{\\text{~a}, \\text{~a}}" body* (round-mode->tex new-rnd) new-prec)]
            [(not (equal? curr-prec new-prec))
             (format "\\left( ~a \\right)_{\\text{~a}}" body* new-prec)]
            [(not (equal? curr-rnd new-rnd))
             (format "\\left( ~a \\right)_{\\text{~a}}" body* (round-mode->tex new-rnd))]
            [else body*])]

        [(? exact-integer?)
         (number->string expr)]
        [(and (? rational?) (? exact?))
         (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
        [(? (conjoin complex? (negate real?))) ;; Herbie stuff
         (format "~a ~a ~a i"
                 (texify (real-part expr) ctx '+ loc)
                 (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
                 (texify (abs (imag-part expr)) ctx '+ loc))]
        [(or (list 'digits (? number?) (? number?) (? number?)) (? hex?) (? number?))
         (define expr*
           (number->string
            (match expr
              [(list digits m e b) (digits->number m e b)]
              [(? hex?) (hex->racket expr)]
              [_ expr])))
         (match expr*
          [(or "+inf.0" "+inf.f") "+\\infty"]
          [(or "-inf.0" "-inf.f") "-\\infty"]
          [(or "+nan.0" "+nan.f") "\\mathsf{NaN}"]
          [_  
            (match (string-split (string-trim expr* ".0") #rx"e|f")
              [(list num) num]
              [(list significand exp)
                (define num
                  (if (equal? significand "1")
                      (format "10^{~a}" exp)
                      (format "~a \\cdot 10^{~a}" significand exp)))
                (if (precedence< parens #f) num (format "\\left( ~a \\right)" num))])])]
        [(? constant?) (constant->tex expr)]
        [(? symbol?) (variable->tex expr)]
        
        [`(<= ,x ,(or -inf.0 -inf.f))
         (texify `(== ,x -inf.0) ctx parens loc)]
        [(list op args ...)
         (define-values (self-paren-level arg-paren-level) (precedence-levels op))
         (define texed-args
           (for/list ([arg args] [id (in-naturals 1)])
             (texify arg ctx arg-paren-level (cons id loc))))
         (format ; omit parens if parent contex has lower precedence
           (if (or (precedence< parens self-paren-level) (equal? self-paren-level 'fn)) 
               "~a" 
               "\\left(~a\\right)")
           (application->tex op texed-args))]))))

;; Exports

(define (expr->tex expr #:prec [prec 'binary64] #:loc [color-loc #f] #:color [color "red"])
  (define ctx (ctx-update-props (make-compiler-ctx) (list ':precision prec)))
  (expr->tex* expr ctx color-loc color))

; Names are optional in TeX programs
(define (core->tex prog [name ""] #:loc [color-loc #f] #:color [color "red"])
  (parameterize ([*used-names* (mutable-set)] 
                 [*gensym-collisions* 1] 
                 [*gensym-fix-name* fix-name])
    (define-values (args props body)
      (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))

    (define func-name 
      (if (non-empty-string? name)
          (let-values ([(cx fname) (ctx-unique-name ctx (string->symbol name))])
            (set! ctx cx)
            fname)
          ""))

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

    (define body* (expr->tex* body ctx color-loc color))
    (if (non-empty-string? func-name)
        (format "\\mathsf{~a}\\left(~a\\right) = ~a\n"
                func-name
                (string-join arg-names ", ")
                body*)
        body*)))

(define-compiler '("tex") (const "") core->tex (const "") tex-supported)