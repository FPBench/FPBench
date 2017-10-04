#lang racket

(require "common.rkt" "fpcore.rkt")
(provide compile-program)

(define (fix-name name)
  (string-join
   (for/list ([char (~a name)])
     (if (regexp-match #rx"[a-zA-Z0-9]" (string char))
         (string char)
         (format "_~a_" (char->integer char))))
   ""))

(define (application->c type operator args)
  (match (cons operator args)
    [(list '- a)
     (format "-~a" a)]
    [(list 'not a)
     (format "!~a" a)]
    [(list (or '+ '- '* '/) a b)
     (format "(~a ~a ~a)" a operator b)]
    [(list (or '== '!= '< '> '<= '>=))
     "TRUE"]
    [(list (or '== '< '> '<= '>=) head args ...)
     (format "(~a)"
             (string-join
              (for/list ([a (cons head args)] [b args])
                (format "~a ~a ~a" a operator b))
              " && "))]
    [(list '!= args ...)
     (format "(~a)"
             (string-join
              (let loop ([args args])
                (if (null? args)
                    '()
                    (append
                     (for/list ([b (cdr args)])
                       (format "~a != ~a" (car args) b))
                     (loop (cdr args)))))
              " && "))]
    [(list 'and a b)
     (format "(~a && ~a)" a b)]
    [(list 'or a b)
     (format "(~a || ~a)" a b)]
    [(list (? operator? f) args ...)
     (format "~a(~a)" f (string-join args ", "))]))

(define/match (type->c type)
  [('real) "real"]
  [('binary16) "float16"]
  [('binary32) "float32"]
  [('binary64) "float64"]
  [('binary128) "float128"])

(define/match (type->rnd type)
  [('real) ""]
  [('binary16) "rnd16"]
  [('binary32) "rnd32"]
  [('binary64) "rnd64"]
  [('binary128) "rnd128"])

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

(define (expr->fptaylor expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and local definitions (in *defs*).
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var* vars*] [val vals])
       (add-def (format "~a~a ~a= ~a;" indent (fix-name var*) (type->rnd type)
                        (expr->fptaylor val #:names names #:type type #:indent indent))))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->fptaylor body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (error 'expr->fptaylor "Unsupported operation ~a" expr)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->fptaylor arg #:names names #:type type #:indent indent)) args))
     (application->c type operator args_c)]
    [(? constant?)
     (format "((~a) ~a)" (type->c type) expr)]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (define t (inexact->exact expr))
     (if (= (denominator t) 1)
               (format "~a" t)
               (format "~a(~a)" (type->rnd type) t))]))

(define (compile-program prog #:name name #:indent [indent "\t"])
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a ~a" (type->c type) (fix-name (if (list? var) (car var) var)))))
  (define output
    (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)]
                       [*defs* (box '())])
          (define expr-name (gensym name))
          (define expr-body (expr->fptaylor body #:type type #:indent indent))
          (unless (empty? (unbox (*defs*)))
            (printf "Definitions\n~a\n\n" (string-join (reverse (unbox (*defs*))) "\n")))
          (printf "Expressions\n~a~a ~a= ~a;\n"
                  indent (fix-name expr-name) (type->rnd type) expr-body)))))
  (format "~a" output))
;~a ~a(~a) {\n~a}\n" (type->c type) (fix-name name) (string-join arg-strings ", ") c-body))

; My opinion: all decimal numbers in fpcore files are real numbers
(parameterize ([read-decimal-as-inexact #f])
             (for ([expr (in-port (curry read-fpcore "test")
                                  (open-input-file "../benchmarks/test.fpcore"))])
               (printf "~a\n\n" (compile-program expr #:name "test" #:indent "  "))))

#|
(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (printf "#include <math.h>\n\n")
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "ex~a" n))))))
|#