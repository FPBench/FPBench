#lang racket

(require "common.rkt" "fpcore.rkt" "fpcore-extra.rkt" "range-analysis.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
         (string char)
         (format "$~a$" (char->integer char))))
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
  [('==) '=]
  [((or '+ '- '* '/ 'sqrt '< '> '<= '>=)) op]
  [((or 'exp 'log 'sin 'cos 'tan 'asin 'acos 'atan)) op]
  [((or 'sinh 'cosh 'tanh 'asinh 'acosh 'atanh)) op]
  [('fmax) 'max] [('fmin) 'min] [('fabs) 'abs]
  [(_) (error 'operator->fptaylor "Unsupported operation ~a" op)])

(define (application->fptaylor type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list (or '!= 'not 'and 'or) args ...)
     (error 'application->fptaylor "Unsupported operation ~a" operator)]
    [(list (or '+ '- '* '/ '== '< '> '<= '>=) a b)
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

(define (expr->fptaylor expr
                        #:names [names #hash()]
                        #:inexact-scale [inexact-scale 1]
                        #:type [type 'binary64])
  ;; Takes in an expression. Returns an expression and local definitions (in *defs*).
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var* vars*] [val vals])
       (add-def (format "~a ~a= ~a" (fix-name var*) (type->rnd type)
                        (expr->fptaylor val #:names names #:inexact-scale inexact-scale #:type type))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->fptaylor body #:names names* #:inexact-scale inexact-scale #:type type)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_fptaylor
       (map (λ (arg) (expr->fptaylor arg #:names names #:inexact-scale inexact-scale #:type type)) args))
     (if (and (inexact-operator? operator) (not (= inexact-scale 1)))
         (let ([args_fptaylor*
                (for/list [(arg args_fptaylor)]
                  (define tmp (gensym 'tmp))
                  (add-def (format "~a ~a= ~a" (fix-name tmp) (type->rnd type) arg))
                  (fix-name tmp))])
           (format "~a(~a(~a))" (type->rnd type #:scale inexact-scale)
                   (operator->fptaylor operator) (string-join args_fptaylor* ", ")))
         (application->fptaylor type operator args_fptaylor))]
    [(? constant?)
     (format "~a(~a)" (type->rnd type) (constant->fptaylor expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (define n-str (format-number expr))
     (if (string-contains? n-str "/")
         (format "~a~a" (type->rnd type) n-str)
         n-str)]))

; This function should be called after remove-let and canonicalize
; (negations should be removed)
(define (conjuncts expr)
  (match expr
    [`(and ,args ...) (append-map conjuncts args)]
    [`(or ,args ...)
     (error 'conjuncts "Logical disjunction is not supported")]
    [`(not ,arg)
     (error 'conjuncts "Logical negation is not supported")]
    [`(,op ,args ...) (list expr)]))

; Removes inequalities in the form (cmp var number) and (cmp number var)
(define (select-constraints expr)
  (define conjs (conjuncts expr))
  (filter (match-lambda
            ['TRUE #f]
            ['FALSE (error 'select-constraints "FALSE precondition")]
            [(list (or '!= '== '< '> '<= '>=) (? symbol?) (? number?)) #f]
            [(list (or '!= '== '< '> '<= '>=) (? number?) (? symbol?)) #f]
            [_ #t]) conjs))

(define (compile-program prog
                         #:name [name "expr"]
                         #:precision [precision #f]
                         #:var-precision [var-precision #f] 
                         #:inexact-scale [inexact-scale 1]
                         #:indent [indent "\t"])
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type
    (if precision precision (dict-ref properties ':precision 'binary64)))
  ; A special property :var-precision
  (define var-type
    (if var-precision var-precision (dict-ref properties ':var-precision 'real)))
  (define name* (dict-ref properties ':name name))
  (define pre ((compose canonicalize remove-let)
               (dict-ref properties ':pre 'TRUE)))
  (define body* (canonicalize body))

  (parameterize ([*names* (apply mutable-set args)]
                 [*defs* (box '())])
    ; Main expression
    (define expr-body (expr->fptaylor body* #:type type #:inexact-scale inexact-scale))
    (define expr-name (~a (gensym name*)))
    ; Ranges of variables
    (define var-ranges (condition->range-table pre))
    (define arg-strings
      (for/list ([var args])
        (define range
          (cond
            [(and var-ranges (hash-has-key? var-ranges var)) (dict-ref var-ranges var)]
            [else (make-interval -inf.0 +inf.0)]))
        (unless (nonempty-bounded? range)
          (error 'compile-program "Bad range for ~a in ~a (~a)" var name* range))
        (match-define (interval l u) range)
        (format "~a~a ~a in [~a, ~a];" indent (type->fptaylor var-type) (fix-name var)
                (format-number l) (format-number u))))
    ; Other constraints
    (define constraints
      (map (curry expr->fptaylor #:type 'real) (select-constraints pre)))
    (with-output-to-string
        (λ ()
          (unless (empty? arg-strings)
            (printf "Variables\n~a\n\n" (string-join arg-strings "\n")))
          (unless (empty? constraints)
            (printf "Constraints\n")
            (for ([c constraints] [n (in-naturals)])
              (define c-name (fix-name (gensym (format "constraint~a" n))))
              (printf "~a~a: ~a;\n" indent c-name c))
            (printf "\n"))
          (unless (empty? (unbox (*defs*)))
            (printf "Definitions\n")
            (for ([def (reverse (unbox (*defs*)))])
              (printf "~a~a;\n" indent def))
            (printf "\n"))
          (printf "Expressions\n~a~a ~a= ~a;\n"
                  indent (fix-name expr-name) (type->rnd type) expr-body)))))

(module+ test
  (for ([prog (in-port (curry read-fpcore "test")
                       (open-input-file "../benchmarks/fptaylor-tests.fpcore"))])
    (define progs (fpcore-transform prog #:split-or #t))
    (define results (map (curry compile-program #:name "test") progs))
    (for ([r results])
      (printf "{\n~a}\n\n" r)))
)

(module+ main
  (require racket/cmdline)
  (define files #f)
  (define files-all #f)
  (define auto-file-names #f)
  (define precision #f)
  (define var-precision #f)
  (define split-or #f)
  (define subexprs #f)
  (define split #f)
  (define unroll #f)
  (define inexact-scale 1)
  
  (command-line
   #:program "core2fptaylor.rkt"
   #:once-each
   ["--files" "Save FPTaylor tasks corresponding to different FPBench expression in separate files"
              (set! files #t)]
   ["--files-all" "Save all FPTaylor tasks in separate files"
                  (set! files-all #t)]
   ["--auto-file-names" "Generate special names for all files"
                        (set! auto-file-names #t)]
   ["--precision" prec "The precision of all operations (overrides the :precision property)"
             (set! precision (string->symbol prec))]
   ["--var-precision" prec "The precision of input variables (overrides the :var-precision property)"
                      (set! var-precision (string->symbol prec))]
   ["--scale" scale "The scale factor for operations which are not correctly rounded"
              (set! inexact-scale (string->number scale))]
   ["--split-or" "Convert preconditions to DNF and create separate FPTaylor tasks for all conjunctions"
                 (set! split-or #t)]
   ["--subexprs" "Create FPTaylor tasks for all subexpressions"
                 (set! subexprs #t)]
   ["--split" n "Split intervals of bounded variables into the given number of parts"
              (set! split (string->number n))]
   ["--unroll" n "How many iterations to unroll any loops to"
               (set! unroll (string->number n))]
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([prog (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (with-handlers ([exn:fail? (λ (exn) (eprintf "[ERROR]: ~a\n\n" exn))])
       (define def-name (format "ex~a" n))
       (define prog-name (if auto-file-names def-name (fpcore-name prog def-name)))
       (define progs (fpcore-transform prog
                                       #:unroll unroll
                                       #:split split
                                       #:subexprs subexprs
                                       #:split-or split-or))
       (define results (map (curry compile-program
                                   #:name def-name
                                   #:precision precision
                                   #:var-precision var-precision
                                   #:inexact-scale inexact-scale
                                   #:indent "  ")
                            progs))
       (define multiple-results (> (length results) 1))
       (cond
         [files-all (for ([r results] [k (in-naturals)])
                      (define fname (fix-file-name (string-append prog-name
                                                                  (if multiple-results (format "_case~a" k) "")
                                                                  ".txt")))
                      (call-with-output-file fname #:exists 'replace
                        (λ (p) (fprintf p "~a" r))))]
         [files (call-with-output-file (fix-file-name (format "~a.txt" prog-name)) #:exists 'replace
                  (λ (p) (for ([r results])
                           (if multiple-results (fprintf p "{\n~a}\n\n" r) (fprintf p "~a" r)))))]
         [else (for ([r results]) (printf "{\n~a}\n\n" r))])
       )))
  )
