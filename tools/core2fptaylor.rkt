#lang racket

(require "common.rkt" "fpcore.rkt")
(provide compile-program)

;; Range analysis is based on herbie/src/range-analysis.rkt

; Standard min and max can transform exact numbers into inexact numbers
(define (exact-max arg1 . args)
  (for/fold ([max arg1]) ([arg args])
    (if (< max arg) arg max)))

(define (exact-min arg1 . args)
  (for/fold ([min arg1]) ([arg args])
    (if (< min arg) min arg)))

;; NOTE: an interval can also be #f for an empty interval
(struct interval (l u) #:transparent)

(define (make-interval l u)
  (cond
    [(<= l u) (interval l u)]
    [else #f]))

(define (is-non-empty-bounded intvl)
  (match intvl
    [(interval l u) (and (rational? l) (rational? u) (<= l u))]
    [else #f]))

(define (interval-intersect interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1) interval1)
    (match-define (interval l2 u2) interval2)
    (make-interval (exact-max l1 l2) (exact-min u1 u2))]
   [else #f]))

(define (interval-union interval1 interval2)
  (cond
   [(and interval1 interval2)
    (match-define (interval l1 u1) interval1)
    (match-define (interval l2 u2) interval2)
    (make-interval (exact-min l1 l2) (exact-max u1 u2))]
   [interval1 interval1]
   [else interval2]))

(define (interval-invert intvl)
  (match intvl
    [(interval -inf.0 +inf.0) #f]
    [(interval -inf.0 u) (interval u +inf.0)]
    [(interval l +inf.0) (interval -inf.0 l)]
    [_ (interval -inf.0 +inf.0)]))

(define (make-range-table x intvl)
  (make-hash (list (cons x intvl))))

(define (make-empty-range-table)
  (make-hash))

;; NOTE: a range-table can also be #f for an invalid range-table 
(define (make-null-range-table)
  #f)

(define (range-table-ref rt x)
  (and rt (hash-ref rt x (interval -inf.0 +inf.0))))

(define (range-table-intersect table1 table2)
  (cond
    [(not table1) #f]
    [(not table2) #f]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)])
       (if (hash-has-key? table2 key1)
           (hash-set! new-range-table key1 (interval-intersect (hash-ref table1 key1) (hash-ref table2 key1)))
           (hash-set! new-range-table key1 (hash-ref table1 key1))))
     (for ([key2 (hash-keys table2)] #:unless (hash-has-key? new-range-table key2))
       (hash-set! new-range-table key2 (hash-ref table2 key2)))
     new-range-table]))

(define (range-table-union table1 table2)
  (cond
    [(not table1) table2]
    [(not table2) table1]
    [else
     (define new-range-table (make-hash))
     (for ([key1 (hash-keys table1)] #:when (hash-has-key? table2 key1))
       (hash-set! new-range-table key1 (interval-union (hash-ref table1 key1) (hash-ref table2 key1))))
     new-range-table]))

(define (range-table-invert table)
  (cond
    [(and table (= (count identity (hash-values table)) 1))
     (match-define (list (cons var itvl)) (filter cdr (hash->list table)))
     (make-range-table var (interval-invert itvl))]
    [else
     (make-empty-range-table)]))

(define (flip-cmp cmp)
  (match cmp
    ['< '>]
    ['> '<]
    ['<= '>=]
    ['>= '<=]
    ['== '==]))

(define (parse-cmp cmp)
  (match cmp ['< <] ['> >] ['<= <=] ['>= >=] ['== =]))

(define (condition->range-table condition)
  (match condition
    [(list (and (or '< '> '<= '>= '==) cmp) (? number? a) (? number? b))
     (if ((parse-cmp cmp) a b)
         (make-empty-range-table)
         (make-null-range-table))]
    ['TRUE (make-empty-range-table)]
    ['FALSE (make-null-range-table)]
    [`(== ,(? symbol? var) ,(? number? num))
     (make-range-table var (make-interval num num))]
    [(list (or '< '<=) (? symbol? var) (? number? num))
     (make-range-table var (make-interval -inf.0 num))]
    [`(,(or '< '<= '==) (fabs ,(? symbol? var)) ,(? number? num))
     (make-range-table var (make-interval (- num) num))]
    [`(,(or '> '>=) ,(? symbol? var) ,(? number? num))
     (make-range-table var (make-interval num +inf.0))]
    [`(,(or '> '>=) (fabs ,(? symbol? var)) ,(? number? num))
     (make-empty-range-table)]
    [(list (and (or '< '<= '== '>= '>) cmp) (? number? num) var) ; don't check for variable? here b/c fabs
     (condition->range-table (list (flip-cmp cmp) var num))]
    [(list (and (or '< '<= '> '>=) cmp) exprs ...)
     (if (not (equal? (filter number? exprs) (sort (filter number? exprs) (parse-cmp cmp))))
       #f
       (let
         ([from-left (last-number exprs)]
          [from-right (reverse (last-number (reverse exprs)))])
         (foldl range-table-intersect
                (make-empty-range-table)
                (for/list ([left from-left] [expr exprs] [right from-right]
                          #:when (symbol? expr) #:unless (number? expr))
                  (range-table-intersect
                   (if left
                       (condition->range-table (list cmp left expr))
                       (make-empty-range-table))
                   (if right
                       (condition->range-table (list cmp expr right))
                       (make-empty-range-table)))))))]
    [(list '== exprs ...)
     (define num (get-all-equal-value exprs))
     (if num
         (foldl range-table-intersect
                (make-empty-range-table)
                (map (lambda (x) (make-range-table x (make-interval num num)))
                     (filter symbol? exprs)))
         (make-null-range-table))]

    [`(and ,conds ...)
     (foldl range-table-intersect (make-empty-range-table) (map condition->range-table conds))]
    [`(or ,conds ...)
     (foldl range-table-union (make-null-range-table) (map condition->range-table conds))]
    [`(not ,cond1) (range-table-invert (condition->range-table cond1))]
    [_
     (make-empty-range-table)]))

(define (get-all-equal-value lst)
  (let ([nums (filter number? lst)])
    (if (foldl (lambda (x y) (and x y)) #t (map (lambda (x) (= x (car nums))) nums))
        (car nums)
        #f)))

(define (last-number lst)
  (let loop ([lst lst] [last #f])
    (match lst
      ['() '()]
      [(cons (? number? x) rest)
       (cons x (loop rest x))]
      [(cons _ rest)
       (cons last (loop rest last))])))

;; End: Range analysis

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define (inexact-operator? op)
  (set-member? '(exp log sin cos tan asin acos atan
                sinh cosh tanh asinh acosh atanh) op))

(define constant->fptaylor
  (match-lambda
    ['E "exp(1)"]
    ['LN2 "log(2)"]
    ['LN10 "log(10)"]
    ['PI "4 * atan(1)"]
    ['PI_2 "2 * atan(1)"]
    ['PI_4 "atan(1)"]
    ['1_PI "1 / (4 * atan(1))"]
    ['2_PI "1 / (2 * atan(1))"]
    ['2_SQRTPI "1 / sqrt(atan(1))"]
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "1 / sqrt(2)"]
    [c (error 'constant->fptaylor "Unsupported constant ~a" c)]))

(define/match (operator->fptaylor op)
  [((or '+ '- '* '/ 'sqrt)) op]
  [((or 'exp 'log 'sin 'cos 'tan 'asin 'acos 'atan)) op]
  [((or 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh)) op]
  [('fmax) 'max] [('fmin) 'min] [('fabs) 'abs]
  [(_) (error 'operator->fptaylor "Unsupported operation ~a" op)])

(define (application->fptaylor type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list (or '== '< '> '<= '>= '!= 'not 'and 'or) args ...)
     error 'application->fptaylor "Unsupported operation ~a" operator]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a (operator->fptaylor operator) b)]
    [(list (? operator? f) args ...)
     (format "~a(~a)" (operator->fptaylor f) (string-join args ", "))]))

(define/match (type->fptaylor type)
  [('real) "real"]
  [('binary16) "float16"]
  [('binary32) "float32"]
  [('binary64) "float64"]
  [('binary128) "float128"]
  [(_) (error 'type->fptaylor "Unsupported type ~a" type)])

(define (type->rnd type #:direction [dir 'ne] #:scale [scale 1])
  (define bits
    (match type
      ['real ""]
      ['binary16 "16"]
      ['binary32 "32"]
      ['binary64 "64"]
      ['binary128 "128"]
      [_ (error 'type->rnd "Unsupported type ~a" type)]))
  (cond
    [(equal? bits "") ""]
    [(and (eq? dir 'ne) (= scale 1)) (format "rnd~a" bits)]
    [else (format "rnd(~a,~a,~a)" bits dir scale)]))

(define/contract (format-number num)
  (-> number? string?)
  (define t (inexact->exact num))
  (if (= (denominator t) 1)
      (format "~a" t)
      (format "(~a)" t)))

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define *defs* (make-parameter (box '())))
                               
(define (add-def def)
  (set-box! (*defs*) (cons def (unbox (*defs*)))))

(define (expr->fptaylor expr #:names [names #hash()] #:inexact-scale [inexact-scale 1]
                        #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and local definitions (in *defs*).
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var* vars*] [val vals])
       (add-def (format "~a~a ~a= ~a;" indent (fix-name var*) (type->rnd type)
                        (expr->fptaylor val #:names names #:inexact-scale inexact-scale #:type type #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->fptaylor body #:names names* #:inexact-scale inexact-scale #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_fptaylor
       (map (λ (arg) (expr->fptaylor arg #:names names #:inexact-scale inexact-scale #:type type #:indent indent)) args))
     ; Move to application->fptaylor?
     (if (and (inexact-operator? operator) (not (= inexact-scale 1)))
         (let ([args_fptaylor*
                (for/list [(arg args_fptaylor)]
                  (define tmp (gensym 'tmp))
                  (add-def (format "~a~a ~a= ~a;" indent (fix-name tmp) (type->rnd type) arg))
                  (fix-name tmp))])
           (format "~a(~a(~a))" (type->rnd type #:scale inexact-scale)
                   (operator->fptaylor operator) (string-join args_fptaylor* ", ")))
         (application->fptaylor type operator args_fptaylor))]
    [(? constant?)
     (format "~a(~a)" (type->rnd type) (constant->fptaylor expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (define t (inexact->exact expr))
     (if (= (denominator t) 1)
         (format "~a" t)
         (format "~a(~a)" (type->rnd type) t))]))

(define (compile-program prog #:var-type [var-type 'real] #:name name
                         #:inexact-scale [inexact-scale 1] #:indent [indent "\t"])
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))
  ; A special property :var-precision
  (define var-type* (dict-ref properties ':var-precision var-type))
  ; TODO: Add a property for :inexact-scale?
  (define name* (dict-ref properties ':name name))
  (define var-ranges
    (condition->range-table (dict-ref properties ':pre '())))

  (define arg-strings
    (for/list ([var args])
      (define range
        (cond
          [(and var-ranges (hash-has-key? var-ranges var)) (dict-ref var-ranges var)]
          [else (make-interval -inf.0 +inf.0)]))
      (unless (is-non-empty-bounded range)
        (error 'compile-program "Bad range for ~a in ~a (~a)" var name* range))
      (match-define (interval l u) range)
      (format "~a~a ~a in [~a, ~a];" indent (type->fptaylor var-type*) (fix-name var)
              (format-number l) (format-number u))))

  (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)]
                       [*defs* (box '())])
          (define expr-name (gensym name*))
          (define expr-body (expr->fptaylor body #:type type #:inexact-scale inexact-scale #:indent indent))
          (unless (empty? arg-strings)
            (printf "Variables\n~a\n\n" (string-join arg-strings "\n")))
          (unless (empty? (unbox (*defs*)))
            (printf "Definitions\n~a\n\n" (string-join (reverse (unbox (*defs*))) "\n")))
          (printf "Expressions\n~a~a ~a= ~a;\n"
                  indent (fix-name expr-name) (type->rnd type) expr-body)))))

(module+ test
  ; My opinion: all decimal numbers in fpcore files are real numbers
  (parameterize ([read-decimal-as-inexact #f])
    (for ([expr (in-port (curry read-fpcore "test")
                         (open-input-file "../benchmarks/test.fpcore"))])
      (printf "~a\n\n" (compile-program expr #:name "test" #:indent "  "))))
)

; TODO: save results in separate files?
(module+ main
  (require racket/cmdline)
  (define var-type 'real)
  (define inexact-scale 1)
  
  (command-line
   #:program "core2fptaylor.rkt"
   #:once-each
   ["--var-type" type "The default type of input variables"
                 (set! var-type (string->symbol type))]
   ["--scale" scale "The scale factor for inexactly rounded operations"
              (set! inexact-scale (string->number scale))]
   #:args ()
   (port-count-lines! (current-input-port))
   (parameterize ([read-decimal-as-inexact #f])
     (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
       (printf "~a\n\n" (compile-program expr
                                         #:name (format "ex~a" n)
                                         #:var-type var-type
                                         #:inexact-scale inexact-scale
                                         #:indent "  "))))))
