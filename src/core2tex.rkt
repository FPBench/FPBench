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
(define (precedence< a b)
  (case a
    [(#t) (not (eq? b #t))]
    [(+) (or (eq? b '*) (eq? b 'fn) (eq? b #f))]
    [(*) (or (eq? b 'fn) (eq? b #f))]
    [(fn) (eq? b #f)]
    [else #f]))

(define (precedence-levels op)
  (case op
    [(+ - neg or) (values '+ '+)]
    [(* and) (values '* '*)]
    [(array ref) (values 'fn #t)]
    [(/) (values #f #t)]
    [(pow) (values #f #f)]
    [(atan2) (values 'fn #t)]
    [(if) (values #t #t)]
    [(remainder fmod) (values #t #f)]
    [(cbrt ceil copysign exp2 floor fmax exp sqrt fmin fabs fdim
           expm1 fma log1p hypot j0 j1 y0 y1 lgamma tgamma trunc)
     (values #f #t)]
    [(== < > <= >= !=) (values #f #t)]
    [else (values 'fn #f)]))

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
  [(_) (let ([name (symbol->string expr)])
         (if (string-contains? name "_")
             (string-replace name "_" "\\_")
             name))])

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
   ['array (string-append "\\left[" (string-join args ", ") "\\right]")]
   ['ref (string-append (car args) "\\left[" (cadr args) "\\right]")]
   ['== (string-append (car args) " = " (cadr args))]
   ['> (string-append (car args) " > " (cadr args))]
   ['< (string-append (car args) " < " (cadr args))]
   ['>= (string-append (car args) " \\geq " (cadr args))]
   ['<= (string-append (car args) " \\leq " (cadr args))]
   ['* (string-append (car args) " \\cdot " (cadr args))]
   ['/ (string-append "\\frac{" (car args) "}{" (cadr args) "}")]
   ['atan2 (string-append "\\tan^{-1}_* \\frac{" (car args) "}{" (cadr args) "}")]
   ['cbrt (string-append "\\sqrt[3]{" (car args) "}")]
   ['ceil (string-append "\\left\\lceil" (car args) "\\right\\rceil")]
   ['exp (string-append "e^{" (car args) "}")]
   ['exp2 (string-append "2^{" (car args) "}")]
   ['fabs (string-append "\\left|" (car args) "\\right|")]
   ['floor (string-append "\\left\\lfloor " (car args) "\\right\\rfloor ")]
   ['fma (string-append "\\mathsf{fma}\\left(" (string-join args ", ") "\\right)")]
   ['fmax (string-append "\\mathsf{max}\\left(" (car args) ", " (cadr args) "\\right)")]
   ['fmin (string-append "\\mathsf{min}\\left(" (car args) ", " (cadr args) "\\right)")]
   ['fmod (string-append (car args) " \\bmod " (cadr args))]
   ['j0 (string-append "j_0\\left(" (car args) "\\right)")]
   ['j1 (string-append "j_1\\left(" (car args) "\\right)")]
   ['log10 (string-append "\\log_{10} " (car args))]
   ['log2 (string-append "\\log_{2} " (car args))]
   ['logb (string-append "\\log_{b} " (car args))]
   ['pow (string-append "{" (car args) "}^{" (cadr args) "}")]
   ['remainder (string-append (car args) " \\mathsf{rem} " (cadr args))]
   ['rint (string-append "\\mathsf{rint} " (car args))]
   ['round (string-append "\\mathsf{round} " (car args))]
   ['sqrt (string-append "\\sqrt{" (car args) "}")]
   ['tgamma (string-append "\\Gamma\\left(" (car args) "\\right)")]
   ['y0 (string-append "y_0\\left(" (car args) "\\right)")]
   ['y1 (string-append "y_1\\left(" (car args) "\\right)")]
   ['complex (string-append (car args) " + " (cadr args) " i")]
   ['re (string-append "\\Re(" (car args) ")")]
   ['im (string-append "\\Im(" (car args) ")")]
   ['conj (format "\\overline{~a}")]
   [(or 'sin 'cos 'tan 'sinh 'cosh 'tanh 'log)
    (string-append "\\" (symbol->string op) " " (car args))]
   [(or 'acos 'acosh 'asin 'asinh 'atan 'atanh)
    (string-append "\\" (substring (symbol->string op) 1) "^{-1} " (car args))]
   [(or 'erf 'erfc 'expm1 'lgamma 'log1p 'trunc)
    (string-append "\\mathsf{" (symbol->string op) "}\\left(" (car args) "\\right)")]
   [(or 'copysign 'fdim 'hypot)
    (string-append "\\mathsf{" (symbol->string op) "}\\left(" (car args) ", " (cadr args) "\\right)")]
   [_ (string-append "\\mathsf{" (fix-op-name op) "}\\left(" (string-join args ", ") "\\right)")]))
   
(define (application->tex op args)
  (case op
    [(-)
     (cond
       [(and (pair? args) (null? (cdr args))) (string-append "-" (car args))]
       [(and (pair? args) (pair? (cdr args)) (null? (cddr args)))
        (string-append (car args) " - " (cadr args))]
       [else (operator->tex op args)])]
    [(+)
     (if (and (pair? args) (pair? (cdr args)) (null? (cddr args)))
         (string-append (car args) " + " (cadr args))
         (operator->tex op args))]
    [(== < > <= >=)
     (if (pair? args)
         (string-join
           (for/list ([a (in-list args)] [b (in-list (cdr args))])
             (operator->tex op (list a b)))
           " \\land ")
         (operator->tex op args))]
    [(!=)
     (string-join
       (let loop ([args args])
         (if (null? args)
             '()
             (append
               (for/list ([b (in-list (cdr args))])
                 (string-append (car args) " \\ne " b))
               (loop (cdr args)))))
       " \\land ")]
    [(not)
     (if (and (pair? args) (null? (cdr args)))
         (string-append "\\neg " (car args))
         (operator->tex op args))]
    [(and) (string-join args " \\land ")]
    [(or) (string-join args " \\lor ")]
    [else (operator->tex op args)]))

(define (collect-branches expr)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift)
           (collect-branches iff))]
    [else
     (list (list #t expr))]))

(define (number-string->tex number-string parens)
  (cond
    [(or (string=? number-string "+inf.0") (string=? number-string "+inf.f")) "+\\infty"]
    [(or (string=? number-string "-inf.0") (string=? number-string "-inf.f")) "-\\infty"]
    [(or (string=? number-string "+nan.0") (string=? number-string "+nan.f")) "\\mathsf{NaN}"]
    [else
     (define exponent-index
       (for/first ([char (in-string number-string)]
                   [index (in-naturals)]
                   #:when (or (char=? char #\e) (char=? char #\f)))
         index))
     (if exponent-index
         (let* ([significand (substring number-string 0 exponent-index)]
                [exponent (substring number-string (add1 exponent-index))]
                [num (if (string=? significand "1")
                         (string-append "10^{" exponent "}")
                         (string-append significand " \\cdot 10^{" exponent "}"))])
           (if (precedence< parens #f)
               num
               (string-append "\\left( " num " \\right)")))
         (let* ([start (if (string-prefix? number-string ".0") 2 0)]
                [end (if (string-suffix? number-string ".0")
                         (- (string-length number-string) 2)
                         (string-length number-string))])
           ;; Match string-trim's literal separator without regex work.
           (substring number-string start (if (> start end) start end))))]))

(define (number->tex expr parens)
  (number-string->tex (number->string expr) parens))

(define (hex-literal? expr)
  (define name (symbol->string expr))
  (define length (string-length name))
  (define start
    (if (and (> length 0)
             (or (char=? (string-ref name 0) #\+)
                 (char=? (string-ref name 0) #\-)))
        1
        0))
  (and (< (+ start 2) length)
       (char=? (string-ref name start) #\0)
       (char=? (string-ref name (add1 start)) #\x)
       (hex? expr)))

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

(define (compound-expr->tex expr ctx parens)
  (define op (car expr))
  (define args (cdr expr))
  (define-values (self-paren-level arg-paren-level) (precedence-levels op))
  (define texed-args
    (for/list ([arg (in-list args)])
      (expr->tex arg ctx arg-paren-level)))
  (define application (application->tex op texed-args))
  (if (or (precedence< parens self-paren-level) (equal? self-paren-level 'fn))
      application
      (string-append "\\left(" application "\\right)")))

(define (expr->tex expr ctx [parens #t])
  (if (and (pair? expr)
             (case (car expr)
               [(let let* if cast ! digits <=) #f]
               [else #t]))
      (compound-expr->tex expr ctx parens)
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
      (compound-expr->tex expr ctx parens)]
     [(? exact-integer?)
      (number->string expr)]
     [(and (? rational?) (? exact?))
      (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
     [(? (conjoin complex? (negate real?))) ;; Herbie stuff
      (format "~a ~a ~a i"
              (expr->tex (real-part expr) ctx '+)
              (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
              (expr->tex (abs (imag-part expr)) ctx '+))]
     [(? number?) (number->tex expr parens)]
     [(? symbol?)
      (cond
        [(case expr
           [(E LOG2E LOG10E LN2 LN10 PI PI_2 PI_4 M_1_PI M_2_PI
               M_2_SQRTPI SQRT2 SQRT1_2 MAXFLOAT HUGE_VAL
               INFINITY NAN TRUE FALSE)
            #t]
           [else #f])
         (constant->tex expr)]
        [(hex-literal? expr)
         (expr->tex (hex->racket expr) ctx parens)]
        [else (variable->tex expr)])])))

;; Exports

;; Retain the color keywords for compatibility; TeX output is always uncolored.
(define (expr*->tex expr #:prec [prec 'binary64] #:loc [_loc #f] #:color [_color "red"])
  (define ctx (ctx-update-props (make-compiler-ctx) (list ':precision prec)))
  (expr->tex expr ctx))

; Names are optional in TeX programs
(define (core->tex prog [name ""] #:loc [_loc #f] #:color [_color "red"])
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
      (for/list ([var args])
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
  (check-true (supported-by-lang? (make-program-features '(FPCore (x y) (array x y))) "tex"))
  (check-true (supported-by-lang? (make-program-features '(FPCore (v) (ref v 0))) "tex"))
  (check-true (supported-by-lang? (make-program-features '(FPCore ((v 3)) (ref v 0))) "tex"))
  (check-equal? (number->tex 0.0 #t) "0")
  (check-equal? (number->tex -0.0 #t) "-0")
  (check-equal? (number->tex 0.5 #t) "0.5")
  (check-equal? (number->tex 1e-6 #t) "10^{-6}")
  (check-equal? (number->tex 1.25e-6 #t) "1.25 \\cdot 10^{-6}")
  (check-equal? (number->tex +inf.0 #t) "+\\infty")
  (check-equal? (number->tex -inf.0 #f) "-\\infty")
  (check-equal? (number->tex +nan.0 #t) "\\mathsf{NaN}")
  (check-equal? (number-string->tex "1e-6" #t) "10^{-6}")
  (check-equal? (number-string->tex "1.25e-6" #t) "1.25 \\cdot 10^{-6}")
  (check-equal? (number-string->tex "1f+20" #t) "10^{+20}")
  (check-equal? (number-string->tex "1e-6" #f) "\\left( 10^{-6} \\right)")
  (check-equal? (expr*->tex '(+ x (* y z))) "x + y \\cdot z")
  (check-equal? (expr*->tex '(- x)) "-x")
  (check-equal? (expr*->tex '(- x y)) "x - y")
  (check-equal? (expr*->tex '(fmax (+ x y) z))
                "\\mathsf{max}\\left(x + y, z\\right)")
  (check-equal? (expr*->tex '(pow x y)) "{x}^{y}")
  (check-equal? (expr*->tex '(== x y z)) "x = y \\land y = z")
  (check-equal? (expr*->tex '(!= x y z))
                "x \\ne y \\land x \\ne z \\land y \\ne z")
  (check-equal? (expr*->tex '(+ x y) #:loc '(2 1) #:color "blue")
                "x + y")
  (check-equal? (core->tex '(FPCore (x) (+ x 1)) #:loc '(2 1) #:color "blue")
                "x + 1\n")
  (check-equal? (expr*->tex 'x_y) "x\\_y")
  (check-equal? (expr*->tex (string->symbol "0x1p+2")) "4"))
