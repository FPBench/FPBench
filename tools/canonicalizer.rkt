#lang racket

(require "common.rkt" "fpcore.rkt")
(require racket/hash)
(provide compile-program expr->canon)

(define (flatten-context ctx)
  (apply append (map (match-lambda [(cons prop val) (list prop val)]) (hash->list ctx))))

(define (update-ctx ctx updates)
  (define new-hash (make-immutable-hash updates))
  (hash-union ctx new-hash #:combine/key (λ (k a b) b)))

(define (expr->canon expr ctx)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define bindings (for/list ([var vars] [val vals])
                        (list var (expr->canon val ctx))))
     (define new-body (expr->canon body ctx))
     `(let ,bindings ,new-body)]
    [`(if ,condition ,then-branch ,else-branch)
      (define new-cond (expr->canon condition ctx))
      (define new-then (expr->canon then-branch ctx))
      (define new-else (expr->canon else-branch ctx))
      `(if ,new-cond ,new-then ,new-else)]
    [`(while ,condition ([,vars ,inits ,updates] ...) ,return)
     (define new-cond (expr->canon condition ctx))
     (define new-loop (for/list ([var vars] [init inits] [update updates])
       (list var (expr->canon init ctx) (expr->canon update ctx))))
     (define new-ret (expr->canon return ctx))
     `(while ,new-cond ,new-loop ,new-ret)]
    [(? symbol?)
     expr]
    [(or (? constant?) (? number?))
     (if (= (length (hash-keys ctx)) 0)
       (if (= (inexact->exact (exact->inexact expr)) expr)
         (exact->inexact expr)
         expr)
     `(! ,@(flatten-context ctx) ,expr))]
    [(list '! props ... body)
     (define-values (_ properties) (parse-properties props))
     (expr->canon body (update-ctx ctx properties))]
    [(list (? operator? operator) args ...)
     (define new-args (for/list ([arg args])
                        (expr->canon arg ctx)))
     (if (= (length (hash-keys ctx)) 0)
       (cons operator new-args)
       `(! ,@(flatten-context ctx) ,(cons operator new-args)))]
    [_ (error 'expr->smt "Unsupported expr ~a" expr)]))

(define (canonicalize-args args props)
  (for/list ([arg args])
    (if (list? arg)
      (car (canonicalize-args (list (last arg))
                              (append props (reverse (cdr (reverse (cdr arg)))))))
      (append (list '!) props (list arg)))))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define ctx (make-immutable-hash properties))
  `(FPCore ,(canonicalize-args args props) ,(expr->canon body ctx)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "canonicalizer.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (pretty-write (compile-program expr #:name (format "ex~a" n))))))
