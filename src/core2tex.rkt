#lang racket

(require "common.rkt" "compilers.rkt" "supported.rkt")
(provide tex-supported core->tex (rename-out [expr*->tex expr->tex]))

(define tex-supported 
  (supported-list
    (negate (curry set-member? '(while while* dim size for for* tensor tensor*)))
    (curry set-member? '(PI E INFINITY NAN TRUE FALSE))
    (const #t)
    ieee754-rounding-modes
    #t)) ; allow tensor-valued arguments

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
    ['array (values 'fn #t)]
    ['ref (values 'fn #t)]
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
  [(_) (string-replace (symbol->string expr) "_" "\\_")])

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
   ['array      (format "\\left[~a\\right]" (string-join args ", "))]
   ['ref        (format "~a\\left[~a\\right]" (first args) (second args))]
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
   ['floor      (format "\\left\\lfloor ~a\\right\\rfloor " (first args))]
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

(define (collect-branches expr)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift)
           (collect-branches iff))]
    [else
     (list (list #t expr))]))

;; Main texifier

(define (stmt->tex expr ctx [parens #t])
  (match expr
     [`(let ([,vars ,vals] ...) ,body)
      (define vals*
        (for/list ([val (in-list vals)])
          (expr->tex val ctx parens)))
      (string-append
        (format "~a := ~a\\\\\n~a"
                (string-join (map ~a vars) ", ")
                (string-join vals* ", ")
                (stmt->tex body ctx parens)))]
     [`(let* ([,vars ,vals] ...) ,body)
      (define vals*
        (for/list ([val (in-list vals)])
          (expr->tex val ctx parens)))
      (format "~a\n~a"
              (string-join
                (for/list ([var (in-list vars)] [val (in-list vals*)])
                  (format "~a := ~a\\\\" var val))
                "\n")
              (stmt->tex body ctx parens))]
     [`(if ,cond ,ift ,iff)
      (define NL "\\\\\n")
      (define IND "\\;\\;\\;\\;")
      (with-output-to-string
        (λ ()
          (for ([branch (in-list (collect-branches expr))] [n (in-naturals)])
            (match branch
              [(list #t bexpr)
              (printf "\\mathbf{else}:~a~a~a~a\n" NL IND
                      (expr->tex bexpr ctx #t)
                      NL)]
              [(list bcond bexpr)
              (printf "\\mathbf{~a}\\;~a:~a~a~a~a\n"
                      (if (= n 0) "if" "elif")
                      (expr->tex bcond ctx #t)
                      NL IND
                      (expr->tex bexpr ctx #t)
                      NL)]))))]
     [_ (expr->tex expr ctx parens)]))

(define (expr->tex expr ctx [parens #t])
  (match expr
     [`(let ([,vars ,vals] ...) ,body)
      (format "\\begin{array}{l}\n~a\n\\end{array}"
        (stmt->tex expr ctx parens))]
     [`(let* ([,vars ,vals] ...) ,body)
      (format "\\begin{array}{l}\n~a\n\\end{array}"
        (stmt->tex expr ctx parens))]
     [`(if ,cond ,ift ,iff)
      (format "\\begin{array}{l}\n~a\n\\end{array}"
        (stmt->tex expr ctx parens))]

     [`(cast ,body)
      (format "\\langle ~a \\rangle_{\\text{~a}}"
              (expr->tex body ctx parens) (ctx-lookup-prop ctx ':precision))]
     [`(! ,props ... ,body) 
      (define curr-prec (ctx-lookup-prop ctx ':precision))
      (define curr-rnd (ctx-lookup-prop ctx ':round))
      (define ctx* (ctx-update-props ctx props))
      (define body* (expr->tex body ctx* parens))
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
      
     [(list 'digits (? number? m) (? number? e) (? number? b))
      (expr->tex (digits->number m e b) ctx parens)]
     [`(<= ,x ,(or -inf.0 -inf.f))
       ( `(== ,x -inf.0) ctx parens)]
     [(list op args ...)
      (define-values (self-paren-level arg-paren-level) (precedence-levels op))
      (define texed-args
        (for/list ([arg (in-list args)])
          (expr->tex arg ctx arg-paren-level)))
      (format ; omit parens if parent contex has lower precedence
        (if (or (precedence< parens self-paren-level) (equal? self-paren-level 'fn)) 
            "~a" 
            "\\left(~a\\right)")
        (application->tex op texed-args))]
        
     [(? hex?)
      (expr->tex (hex->racket expr) ctx parens)]
     [(? exact-integer?)
      (number->string expr)]
     [(and (? rational?) (? exact?))
      (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
     [(? (conjoin complex? (negate real?))) ;; Herbie stuff
      (format "~a ~a ~a i"
              (expr->tex (real-part expr) ctx '+)
              (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
              (expr->tex (abs (imag-part expr)) ctx '+))]
     [(or (list 'digits (? number?) (? number?) (? number?)) (? hex?) (? number?))
      (define expr* (number->string expr))
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
     [(? symbol?) (variable->tex expr)]))

;; Exports

(define (expr*->tex expr #:prec [prec 'binary64])
  (define ctx (ctx-update-props (make-compiler-ctx) (list ':precision prec)))
  (expr->tex expr ctx))

; Names are optional in TeX programs
(define (core->tex prog [name ""])
  (parameterize ([*gensym-used-names* (mutable-set)] 
                 [*gensym-collisions* 1] 
                 [*gensym-fix-name* fix-name])
    (define-values (args props body)
      (match prog
      [(list 'FPCore (list args ...) props ... body) (values args props body)]
      [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
    (define ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))

    ; handle variable annotations
    (define arg-names
      (for/list ([var (in-list args)])
        (match var
         [(list '! props ... name) (fix-name (~a name))]
         [_ (fix-name (~a var))])))

    (define body* (expr->tex body ctx))
    (if (non-empty-string? name)
        (format "\\mathsf{~a}\\left(~a\\right) = ~a\n"
                name
                (string-join arg-names ", ")
                body*)
        (format "~a\n" body*))))
    
(define-compiler '("tex") (const "") core->tex (const "") tex-supported)

(module+ test
  (require rackunit)
  (check-equal? (core->tex '(FPCore (x y) :precision binary64 (array (+ x y) (* x y))))
                "\\left[x + y, x \\cdot y\\right]\n")
  (check-equal? (core->tex '(FPCore (v) :precision binary64 (ref v 0))) "v\\left[0\\right]\n")
  (check-equal? (core->tex '(FPCore (x y) :precision binary64 (ref (array x y) 1)))
                "\\left[x, y\\right]\\left[1\\right]\n")
  (check-true
   (string-contains?
    (core->tex '(FPCore (x y)
                       :precision binary64
                       (let* ([sum (+ x y)]) (array sum (* sum y)))))
    "\\left[sum, sum \\cdot y\\right]"))
  (check-true
   (string-contains?
    (core->tex '(FPCore (x) (if (> x 0) 1 (if (< x 0) -1 0))))
    "\\mathbf{elif}"))
  (check-exn exn:fail:contract?
             (λ () (keyword-apply core->tex '(#:loc) '(#f) (list '(FPCore () 0)))))
  (check-exn exn:fail:contract?
             (λ () (keyword-apply core->tex '(#:color) '("red") (list '(FPCore () 0)))))
  (check-exn exn:fail:contract?
             (λ () (keyword-apply expr*->tex '(#:loc) (list '(2)) (list 'x))))
  (check-exn exn:fail:contract?
             (λ () (keyword-apply expr*->tex '(#:color) '("red") (list 'x))))
  (check-true (supported-by-lang? (make-program-features '(FPCore (x y) (array x y))) "tex"))
  (check-true (supported-by-lang? (make-program-features '(FPCore (v) (ref v 0))) "tex"))
  (check-true (supported-by-lang? (make-program-features '(FPCore ((v 3)) (ref v 0))) "tex")))
