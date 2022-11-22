#lang racket

(require "common.rkt" "fpcore-reader.rkt" "fpcore-extra.rkt" "range-analysis.rkt" "supported.rkt")
(provide core->gappa gappa-supported)

(define gappa-supported
  (supported-list 
    (disjoin ieee754-ops
             (curry set-member?
                    '(let not and or array dim size ref
                      for for* tensor tensor*)))
    (curry set-member? '(SQRT2 SQRT1_2 TRUE FALSE))
    (curry set-member? '(binary32 binary64 binary80 binary128))
    (curry equal? 'nearestEven)
    #f))

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
    ['TRUE "0 in [0, 0]"]
    ['FALSE "1 in [0, 0]"]
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
     ; Approximate (round down) a if necessary (Gappa does not accept general rational numbers in inequalities)
     (application->gappa '>= (list (expr->gappa b #:names names #:type type)
                                   (format-number a #:direction 'down)))]
    [(list '<= a (? number? b))
     ; Approximate (round up) b if necessary
     (application->gappa '<= (list (expr->gappa a #:names names #:type type)
                                   (format-number b #:direction 'up)))]
    [`(<= ,a ,b)
     (error 'expr->gappa "Cannot translate the inequality: ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args-gappa
       (map (λ (arg) (expr->gappa arg #:names names #:type type)) args))
     (application->gappa operator args-gappa)]
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

(define (core->gappa prog name
                         #:precision [precision #f]
                         #:var-precision [var-precision #f]
                         #:rel-error [rel-error #f])
  (define-values (args props body)
   (match prog
    [(list 'FPCore (list args ...) props ... body) (values args props body)]
    [(list 'FPCore name (list args ...) props ... body) (values args props body)]))
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
  (define body* (canonicalize body))

  (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (define real-expr-name (gensym (string-append "M" name)))
          (define expr-name (gensym name))

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
                  [else (list (make-interval -inf.0 +inf.0))]))
              (unless (= (length range) 1)
                (error 'core->gappa "Gappa only accepts one sampling range"))
              (if (nonempty-bounded? range)
                  (format "~a in [~a, ~a]" (fix-name (dict-ref real-vars var))
                          ; Round down and up if necessary
                          (format-number (interval-l (car range)) #:direction 'down)
                          (format-number (interval-u (car range)) #:direction 'up))
                  "")))

          ; Combine preconditions and ranges
          (define cond-body
            (let* ([sep (format " ~a " (operator->gappa 'and))]
                   [ranges (string-join (filter non-empty-string? ranges) sep)])
              (if (non-empty-string? ranges)
                  ; For some strange reason we must add parentheses around the combined
                  ; range expression. Otherwise Gappa complains that expressions are not bounded.
                  (format "~a~a(~a)" pre* sep ranges)
                  pre*)))

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
  (for ([prog (in-port (curry read-fpcore "test")
                       (open-input-file "../benchmarks/fptaylor-tests.fpcore"))])
    (define progs (fpcore-transform prog #:split-or #t))
    (map (curryr core->gappa "test") progs)))

(module+ main
  (require racket/cmdline)
  (define stdout (make-parameter #f))
  (define auto-file-names (make-parameter #f))
  (define out-path (make-parameter "."))
  (define rel-error (make-parameter #f))
  (define precision (make-parameter #f))
  (define var-precision (make-parameter #f))
  (define split-or (make-parameter #f))
  (define subexprs (make-parameter #f))
  (define split (make-parameter #f))
  (define unroll (make-parameter #f))

  (command-line
   #:program "core2gappa.rkt"
   #:once-each
   ["--stdout" "Print Gappa expressions to the standard output"
               (stdout #t)]
   ["--auto-file-names" "Generate special names for all files"
                        (auto-file-names #t)]
   ["--out-path" path "Save all results in the given path"
                 (out-path path)]
   ["--rel-error" "Produce Gappa expressions for relative errors"
                  (rel-error #t)]
   ["--precision" prec "The precision of all operations (overrides the :precision property)"
                  (precision (string->symbol prec))]
   ["--var-precision" prec "The precision of input variables (overrides the :var-precision property)"
                      (var-precision (string->symbol prec))]
   ["--split-or" "Convert preconditions to DNF and create separate Gappa expressions for all conjunctions"
                 (split-or #t)]
   ["--subexprs" "Create Gappa expressions for all subexpressions"
                 (subexprs #t)]
   ["--split" n "Split intervals of bounded variables into the given number of parts"
              (split (string->number n))]
   ["--unroll" n "How many iterations to unroll any loops to"
               (unroll (string->number n))]
   #:args ([input-file #f])
   ((if input-file
        (curry call-with-input-file input-file)
        (λ (proc) (proc (current-input-port))))
    (lambda (port)
      (port-count-lines! port)
      (for ([prog (in-port (curry read-fpcore "input") port)] [n (in-naturals)])
        (with-handlers ([exn:fail? (λ (exn) (eprintf "[ERROR]: ~a\n\n" exn))])
          (define def-name (format "ex~a" n))
          (define prog-name (if (auto-file-names) def-name (fpcore-name prog def-name)))
          (define progs (fpcore-transform prog
                                          #:unroll (unroll)
                                          #:split (split)
                                          #:subexprs (subexprs)
                                          #:split-or (split-or)))
          (define results (map (curry core->gappa def-name
                                      #:precision (precision)
                                      #:var-precision (var-precision)
                                      #:rel-error (rel-error))
                               progs))
          (define multiple-results (> (length results) 1))
          (for ([r results] [k (in-naturals)])
            (if (stdout)
                (printf "~a\n\n" r)
                (let ([fname (fix-file-name
                              (string-append prog-name (if multiple-results (format "_case~a" k) "") ".g"))])
                  (call-with-output-file (build-path (out-path) fname) #:exists 'replace
                    (λ (p) (fprintf p "~a" r))))))))))
   ))
