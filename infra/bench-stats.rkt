#lang racket
(require xml)
(require "../tools/common.rkt" "../tools/fpcore.rkt")

(define/contract operator-groups (dictof string? (listof symbol?))
  #hash(("Arithmetic" . (+ - * / fabs fma sqrt hypot fmin fmax fdim))
        ("Comparison" . (< > == <= >= != and or not))
        ("FP-specific" . (isfinite isinf isnan isnormal signbit copysign))
        ("Exponents" . (exp expm1 exp2 expm1 pow log log10 log2 log1p))
        ("Trigonometry" . (sin cos tan cotan asin acos atan atan2))
        ("Special functions" . (erf erfc tgamma lgamma cbrt))
        ("Rounding" . (ceil floor trunc round nearbyint cast))
        ("Remainder" . (fmod remainder))
        ("Hyperbolic" . (sinh cosh tanh asinh acosh atanh))
        ("Conditionals" . (if))
        ("Loops" . (while))
        ("Temporaries" . (let))))

(module+ test
  (require rackunit)

  (define all-operators-in-groups
    (apply append (dict-values operator-groups)))

  (with-check-info
   (['missing (set-subtract operators all-operators-in-groups)])
   (check subset? operators all-operators-in-groups)))

(define/match (operators-in expr)
  [(`(while ,test ([,vars ,inits ,updates] ...) ,res))
   (cons 'while
         (append (operators-in test)
                 (append-map operators-in inits)
                 (append-map operators-in updates)
                 (operators-in res)))]
  [(`(let ([,vars ,vals] ...) ,body))
   (cons 'let (append (append-map operators-in vals) (operators-in body)))]
  [(`(if ,cond ,ift ,iff))
   (cons 'if (append (operators-in cond) (operators-in ift) (operators-in iff)))]
  [((list op args ...)) (cons op (append-map operators-in args))]
  [((? symbol?)) '()]
  [((? number?)) '()])

(define/contract (operator->group op)
  (-> symbol? string?)
  (or
   (for/first ([(name ops) (in-dict operator-groups)]
               #:when (set-member? ops op))
     name)
   (error "Unknown operator\n" op)))

(define/contract (expr-groups expr)
  (-> expr? (listof string?))
  (remove-duplicates (map operator->group (operators-in expr))))

(define/contract (operator-table progs)
  (-> (listof fpcore?) (listof (list/c string? number?)))
  (define groups
    (apply append
           (for/list ([prog progs])
             (match-define `(FPCore (,vars ...) ,properties ... ,body) prog)
             (expr-groups body))))
  (sort 
   (filter (λ (x) (> (second x) 0))
           (for/list ([(group _) (in-dict operator-groups)])
             (list group (count (curry equal? group) groups))))
   > #:key second))

(define/contract (domain-table progs)
  (-> (listof fpcore?) (listof (list/c string? number?)))
  (sort
   (map (λ (x) (list (car x) (length x)))
        (group-by identity
                  (for/list ([prog progs])
                    (match-define `(FPCore (,vars ...) ,properties ... ,body) prog)
                    (define-values (_ props) (parse-properties properties))
                    (~a (dict-ref props ':fpbench-domain "(unknown)")))))
   > #:key second))

(define/contract (source-table progs)
  (-> (listof fpcore?) (listof (list/c string? number?)))
  (sort
   (map (λ (x) (list (car x) (length x)))
        (group-by identity
                  (apply append
                         (for/list ([prog progs])
                           (match-define `(FPCore (,vars ...) ,properties ... ,body) prog)
                           (define-values (_ props) (parse-properties properties))
                           (map ~a (dict-ref props ':cite '()))))))
   > #:key second))

(define/contract (table->string table #:title [title #f])
  (-> (listof (list/c string? number?)) #:title (or/c #f string?) string?)
  (with-output-to-string
    (λ ()
      (define longest-key
        (apply max (map (compose string-length first) table)))
      (define longest-value
        (apply max (map (compose string-length ~a second) table)))
      (define width (+ 2 longest-key longest-value))
      (when title
        (printf "~a\n" title)
        (printf "~a\n" (build-string width (const #\-))))
      (for ([row table])
        (match-define (list key value) row)
        (printf "~a~a~a\n" key
                (build-string (- width (string-length key)
                                 (string-length (~a value)))
                              (const #\space))
                value))
      (newline))))

(define/contract (table->xexpr table #:title [title #f])
  (-> (listof (list/c string? number?)) #:title (or/c #f string?) xexpr/c)
  `(table
    (thead
     (tr (th ((colspan "2")) ,title)))
    (tbody
     ,@(for/list ([row table])
         `(tr (td ,(first row)) (td ,(~a (second row))))))))

(define/contract (table->html table #:title [title #f])
  (-> (listof (list/c string? number?)) #:title (or/c #f string?) xexpr/c)
  `(html
    (head
     (meta ((charset "utf-8")))
     (title "FPBench benchmark statistics"))
    (body
     ,(table->xexpr table #:title title))))

(module+ main
  (require racket/cmdline)
  
  (define output (compose display table->string))

  (command-line
   #:program "bench-stats.rkt"
   #:once-each
   ["--format" format "Format of table (text|html|full-html)"
    (set! output
          (match format
            ["text" (compose display table->string)]
            ["html" (compose write-xexpr table->xexpr)]
            ["full-html" (compose write-xexpr table->html)]))]
   #:args ()
   (let* ([progs (sequence->list (in-port read))])
     (printf "~a Benchmarks\n\n" (length progs))
     (output (operator-table progs) #:title "Features used")
     (output (domain-table progs) #:title "Domains")
     (output (source-table progs) #:title "Sources"))))
