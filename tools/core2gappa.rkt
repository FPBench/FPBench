#lang racket

(require "common.rkt" "fpcore.rkt" "fpcore-extra.rkt" "range-analysis.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define constant->gappa
  (match-lambda
    ['SQRT2 "sqrt(2)"]
    ['SQRT1_2 "1 / sqrt(2)"]
    ['TRUE "0 <= 1"]
    ['FALSE "0 => 1"]
    [c (error 'constant->gappa "Unsupported constant ~a" c)]))

(define/match (operator->gappa op)
  [((or '+ '- '* '/ 'sqrt 'fma)) op]
  [((or '<= '>=)) op]
  ; Gappa does not support strict inequalities
  [('<) '<=]
  [('>) '>=]
  [('not) 'not]
  [('and) '/\\]
  [('or) '\\/]
  [(_) (error 'operator->gappa "Unsupported operation ~a" op)])

(define (application->gappa operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list '== a b)
     (format "(~a = ~a)" a b)]
    [(list '!= a b)
     (format "(~a <> ~a)" a b)]
    [(list (or '+ '- '* '/ '<= '>= '< '> 'and 'or) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list 'fabs arg)
     (format "|~a|" arg)]
    [(list (or 'and 'or) a b)
     (format "(~a ~a ~a)" a (operator->gappa operator) b)]
    [(list (or 'sqrt 'fma 'not) args ...)
     (format "~a(~a)" (operator->gappa operator) (string-join args ", "))]
    [_ (error 'application->gappa "Unsupported operation ~a" operator)]))

(define/match (type->rnd type)
  [('real) ""]
  [('binary32) "float<ieee_32,ne>"]
  [('binary64) "float<ieee_64,ne>"]
  [('binary128) "float<ieee_128,ne>"]
  [('binary80) "float<x86_80,ne>"])

(define (format-rounded type expr)
  (define rnd (type->rnd type))
  (if (equal? rnd "") (format "~a" expr) (format "~a(~a)" rnd expr)))

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

(define (expr->gappa expr #:names [names #hash()] #:type [type 'binary64])
  ;; Takes in an expression. Prints out all local definitions and returns an expression.
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define prefix (if (eq? type 'real) "M" ""))
     (define vars* (map (λ (v) (gensym (string-append prefix (~a v)))) vars))
     (for ([var* vars*] [val vals])
       (printf "~a ~a= ~a;\n" (fix-name var*) (type->rnd type)
               (expr->gappa val #:names names #:type type)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->gappa body #:names names* #:type type)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->gappa "Unsupported operation ~a" expr)]
    ; Gappa does not support strict inequalities directly.
    ; It is possible to replace (< a b) with (not (<= b a)) but this seems to be not very useful
    ; and complicates the approximation of rational numbers in the right hand sides of inequalities.
    ; Note: expressions must be canonicalized.
    [`(< ,a ,b) (expr->gappa `(<= ,a ,b) #:names names #:type type)]
    [`(> ,a ,b) (expr->gappa `(<= ,b ,a) #:names names #:type type)]
    [`(>= ,a ,b) (expr->gappa `(<= ,b ,a) #:names names #:type type)]
    [(list '<= (? number? a) b)
     ; TODO: approximate (round down) a if necessary (Gappa does not accept general rational numbers)
     (application->gappa '>= (list (expr->gappa b #:names names #:type type)
                                   (format-number a)))]
    [(list '<= a (? number? b))
     ; TODO: approximate (round up) b if necessary
     (application->gappa '<= (list (expr->gappa a #:names names #:type type)
                                   (format-number b)))]
    [`(<= ,a ,b)
     (error 'expr->gappa "Cannot translate the inequality: ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_gappa
       (map (λ (arg) (expr->gappa arg #:names names #:type type)) args))
     (application->gappa operator args_gappa)]
    [(? constant?)
     (format "~a(~a)" (type->rnd type) (constant->gappa expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr))]
    [(? number?)
     (define n-str (format-number expr))
     (if (string-contains? n-str "/")
         (format "~a~a" (type->rnd type) n-str)
         (format-rounded type n-str))]))

(define (remove-unsupported-inequalities expr)
  ; Should be called after remove-let and canonicalize.
  ; Note: returns #f if all inequalities are not supported.
  (match expr
    [(? constant?) expr]
    [(list (and (or 'and 'or) op) a b)
     (define a* (remove-unsupported-inequalities a))
     (define b* (remove-unsupported-inequalities b))
     (if (not a*)
         (if (not b*) #f b*)
         (if (not b*) a* `(,op ,a* ,b*)))]
    [(list (or '< '> '<= '>=) a b)
     (if (or (number? a) (number? b))
         expr
         (begin (eprintf "[WARNING] Unsupported inequality: ~a\n" expr)
                #f))]
    [(list (or '== '!=) args ...) expr]
    [_ (error 'remove-unsupported-inequalities "Unsupported operation ~a" expr)]))

(define (compile-program prog
                         #:name name
                         #:precision [precision #f]
                         #:var-precision [var-precision #f]
                         #:rel-error [rel-error #f]
                         #:unroll [unroll #f])
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
  (define var-ranges (condition->range-table pre))
  (define body*
    ((compose canonicalize (if unroll (curryr unroll-loops unroll) identity)) body))

  (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (define real-expr-name (gensym "Mexpr"))
          (define expr-name (gensym "expr"))

          ; Generate variables corresponding to input arguments
          (define real-vars
            (for/hash ([arg args])
              (define name (gensym (string-append "M" (~a arg))))
              (unless (eq? var-type 'real)
                (printf "~a = ~a(~a);\n" (fix-name name) (type->rnd var-type)
                        (fix-name (gensym (string-append "T" (~a arg))))))
              (values arg name)))

          ; Generate rounded versions of input arguments
          (define vars
            (for/hash ([arg args])
              (define real-name (dict-ref real-vars arg))
              (if (eq? type var-type)
                  (printf "~a = ~a;\n" (fix-name arg) (fix-name real-name))
                  (printf "~a = ~a(~a);\n" (fix-name arg) (type->rnd type) (fix-name real-name)))
              (values arg arg)))

          ; Generate real-valued expressions
          (printf "\n")
          (define real-expr-body (expr->gappa body* #:names real-vars #:type 'real))
          (printf "~a = ~a;\n\n" real-expr-name real-expr-body)

          ; Generate rounded expressions
          (define expr-body (expr->gappa body* #:names vars #:type type))
          (printf "~a ~a= ~a;\n\n" expr-name (type->rnd type) expr-body)

          ; Generate preconditions (some inequalities may be skipped)
          (define pre*
            (let ([expr (remove-unsupported-inequalities pre)])
              (if expr
                  (expr->gappa expr #:names real-vars #:type 'real)
                  "")))

          ; Generate ranges of variables
          (define ranges
            (for/list ([var args])
              (define range
                (cond
                  [(and var-ranges (hash-has-key? var-ranges var)) (dict-ref var-ranges var)]
                  [else (make-interval -inf.0 +inf.0)]))
              (if (is-non-empty-bounded range)
                  (format "~a in [~a, ~a]" (fix-name (dict-ref real-vars var))
                          ; TODO: round down and up if necessary
                          ; It is also required to round non-decimal rational numbers when
                          ; translating preconditions. But it is necessary to be careful
                          ; and take into account the context:
                          ; (not (<= a 1/3)) is different from (<= a 1/3).
                          ; For preconditions P we need to generate weaker preconditions Q
                          ; such that P ==> Q.
                          (format-number (interval-l range))
                          (format-number (interval-u range)))
                  "")))

          ; Combine preconditions and ranges
          (define cond-body
            (let* ([sep (format " ~a " (operator->gappa 'and))]
                   [ranges (string-join (filter non-empty-string? ranges) sep)])
              (if (non-empty-string? ranges)
                  ; For some strange reason we must add parentheses around the combined
                  ; range expression. Otherwise Gappa complains that expressions are not bounded.
                  (format "~a~a(~a)" pre* sep ranges)
                  pre)))
          
          (when (equal? cond-body "")
            (set! cond-body "0 <= 1"))
          (define goal
            (if rel-error
                (format "~a -/ ~a in ?" expr-name real-expr-name)
                (format "|~a - ~a| in ?" expr-name real-expr-name)))
          (printf "{ ~a\n  -> ~a }\n" cond-body goal)

          ; TODO: subdivision hints
          
          ))))

(module+ test
  (for ([expr (in-port (curry read-fpcore "test")
                       (open-input-file "../benchmarks/fptaylor-tests.fpcore"))])
    (printf "~a\n\n" (compile-program expr #:name "test")))
)

(module+ main
  (require racket/cmdline)
  (define stdout #f)
  (define rel-error #f)
  (define precision #f)
  (define var-precision #f)
  (define unroll #f)
  
  (command-line
   #:program "core2gappa.rkt"
   #:once-each
   ["--stdout" "Print Gappa expressions to the standard output"
               (set! stdout #t)]
   ["--rel-error" "Produce Gappa expressions for relative errors"
                  (set! rel-error #t)]
   ["--precision" prec "The precision of all operations (overrides the :precision property)"
                  (set! precision (string->symbol prec))]
   ["--var-precision" prec "The precision of input variables (overrides the :var-precision property)"
                      (set! var-precision (string->symbol prec))]
   ["--unroll" n "How many iterations to unroll any loops to"
               (set! unroll (string->number n))]
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (with-handlers ([exn:fail? (λ (exn) (eprintf "[ERROR]: ~a\n\n" exn))])
       (define result (compile-program expr
                                       #:name (format "ex~a" n)
                                       #:precision precision
                                       #:var-precision var-precision
                                       #:rel-error rel-error
                                       #:unroll unroll))
       (if stdout
           (printf "~a\n\n" result)
           ; TODO: generate names from the :name properties
           (call-with-output-file (format "ex~a.g" n) #:exists 'replace
             (λ (p) (fprintf p "~a" result)))))))
  )

