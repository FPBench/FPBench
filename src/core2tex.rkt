#lang racket

(require "common.rkt" "compilers.rkt" "supported.rkt")
(provide core->tex tex-supported)

(define tex-supported 
  (supported-list
    (append 
     '(j0 j1 y0 y1 rint)
      (invert-op-list '(let let* while while* for for* tensor tensor*
                       array dim ref size)))
    '(PI E INFINITY NAN TRUE FALSE)
    '(binary32 binary64)       ;; TODO: 'any precision' 
    (invert-round-modes-list '(nearestAway))))

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
    [(or '+ '- 'neg 'or 'complex) (values '+ '+)]
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

(define (collect-branches expr)
  (match expr
    [`(if ,cond ,ift ,iff)
     (cons (list cond ift)
           (collect-branches iff))]
    [else
     (list (list #t expr))]))

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

(define (operator->tex op)
  (match op
   ['==         "~a = ~a"]
   ['>          "~a \\gt ~a"]
   ['<          "~a \\lt ~a"]
   ['>=         "~a \\ge ~a"]
   ['<=         "~a \\le ~a"]
   ['*          "~a \\cdot ~a"]
   ['/          "\\frac{~a}{~a}"]
   ['atan2      "\\tan^{-1}_* \\frac{~a}{~a}"]
   ['cbrt       "\\sqrt[3]{~a}"]
   ['ceil       "\\left\\lceil~a\\right\\rceil"]
   ['exp        "e^{~a}"]
   ['exp2       "2^{~a}"]
   ['fabs       "\\left|~a\\right|"]
   ['floor      "\\left\\lfloor~a\\right\\rfloor"]
   ['fma        "\\mathsf{fma}\\left(~a, ~a, ~a\\right)"]
   ['fmax       "\\mathsf{max}\\left(~a, ~a\\right)"]
   ['fmin       "\\mathsf{min}\\left(~a, ~a\\right)"]
   ['fmod       "~a \\bmod ~a"]
   ['j0         "j_0\\left(~a\\right)"]
   ['j1         "j_1\\left(~a\\right)"]
   ['log10      "\\log_{10} ~a"]
   ['log2       "\\log_{2} ~a"]
   ['logb       "\\log_{b} ~a"]
   ['pow        "{~a}^{~a}"]
   ['remainder  "~a \\mathsf{rem} ~a"]
   ['rint       "\\mathsf{rint} ~a"]
   ['round      "\\mathsf{round} ~a"]
   ['sqrt       "\\sqrt{~a}"]
   ['tgamma     "\\Gamma\\left(~a\\right)"]
   ['y0         "y_0\\left(~a\\right)"]
   ['y1         "y_1\\left(~a\\right)"]
   [(or 'acos 'acosh 'asin 'asinh 'atan 'atanh)
    (format "\\~a^{-1} ~~a" (substring (~a op) 1))]
   [(or 'erf 'erfc 'expm1 'lgamma 'log1p 'trunc)
    (format "\\mathsf{~a}\\left(~~a\\right)" op)]
   [(or 'copysign 'fdim 'hypot)
    (format "\\mathsf{~a}\\left(~~a, ~~a\\right)" op)]
   [_  (format "\\~a ~~a" op)]))

(define (application->tex op args)
  (match (cons op args)
   [(list '- a) (format "-~a" a)]
   [(list (or '+ '- ) a b) (format "~a ~a ~a" a op b)]
   [(list (or '== '< '> '<= '>=) head args ...)
    (string-join
      (for/list ([a (cons head args)] [b args])
        (apply format (operator->tex op) (list a b)))
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
   [(list 'and a ...)
    (string-join (map ~a a) " \\land ")]
   [(list 'or a ...)
    (string-join (map ~a a) " \\lor ")]
   [(list (? operator? f) args ...)
     (apply format (operator->tex op) args)]))

(define (expr->tex expr ctx [parens #t])
  (match expr
    [`(if ,cond ,ift ,iff)
      (define NL "\\\\\n")
      (define IND "\\;\\;\\;\\;")
      (with-output-to-string
        (λ ()
          (printf "\\begin{array}{l}\n")
          (for ([branch (collect-branches expr)] [n (in-naturals)])
            (match branch
              [(list #t bexpr)
              (printf "\\mathbf{else}:~a~a~a~a\n"
                      NL IND (expr->tex bexpr ctx #t) NL)]
              [(list bcond bexpr)
              (printf "\\mathbf{~a}\\;~a:~a~a~a~a\n"
                      (if (= n 0) "if" "elif")
                      (expr->tex cond ctx)
                      NL IND (expr->tex bexpr ctx #t) NL)]))
          (printf "\\end{array}")))]
    [`(cast ,body) (expr->tex body ctx)]
    [`(! ,props ... ,body)
      (expr->tex body (ctx-update-props ctx props))] 
    [`(<= ,x -inf.0) ; weird correction
      (expr->tex `(== ,x -inf.0) ctx parens)]
    [(list (? operator? op) args ...)
      (define-values (self-paren-level arg-paren-level) (precedence-levels op))
      (define args_c (map (λ (arg) (expr->tex arg ctx arg-paren-level)) args))
      (format ; omit parens if parent contex has lower precedence
        (if (precedence< parens self-paren-level) "~a" "\\left(~a\\right)")
        (application->tex op args_c))]

    [(? exact-integer?) (number->string expr)]
    [(and (? rational?) (? exact?))
      (format "\\frac{~a}{~a}" (numerator expr) (denominator expr))]
    [(? (conjoin complex? (negate real?)))
      (format "~a ~a ~a i"
              (expr->tex (real-part expr) ctx '+)
              (if (or (< (imag-part expr) 0) (equal? (imag-part expr) -0.0)) '- '+)
              (expr->tex (abs (imag-part expr)) ctx '+))]
    [(? number?)
      (match (string-split (string-trim (~a expr) ".0") "e")
      [(list "-inf.bf") "-\\infty"]
      [(list "+inf.bf") "+\\infty"]
      [(list num) num]
      [(list significand exp)
        (define num
          (if (equal? significand "1")
              (format "10^{~a}" exp)
              (format "~a \\cdot 10^{~a}" significand exp)))
        (if (precedence< parens #f) num (format "\\left( ~a \\right)" num))])]
    [(list digits m e b) (constant->tex (digits->number m e b))]
    [(? constant?) (constant->tex expr)]
    [(? hex?) (constant->tex (hex->racket expr))]
    [(? number?) (constant->tex expr)]
    [(? symbol?) (variable->tex expr)]))

;; Exports

(define (core->tex prog name)
  (define-values (args props body)
    (match prog
     [(list 'FPCore (list args ...) props ... body) (values args props body)]
     [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
  (define ctx (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))
  (expr->tex body ctx))

(define-compiler '("tex") (const "") core->tex (const "") tex-supported)