#lang racket

(require "common.rkt" "fpcore.rkt")
(require racket/hash)
(provide canonicalize-core expr->canon)

(define *to-canonicalize* (make-parameter '(:pre :spec)))
(define *to-propagate* (make-parameter '(:precision :round :math-library)))

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
      (if (null? props)
        arg
        (append (list '!) props (list arg))))))

(define (canonicalize-core prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define propagate-properties (for/list ([prop properties]
                                          #:when (set-member? (*to-propagate*) (car prop)))
                                 prop))
  (define to-canonicalize-properties (for/list ([prop properties]
                                             #:when (set-member? (*to-canonicalize*) (car prop)))
                                       prop))
  (define canonicalized-properties (for/list ([prop to-canonicalize-properties])
                                     (if (list? prop)
                                       (cons (car prop) (expr->canon (cadr prop) (make-immutable-hash propagate-properties)))
                                       (cons (car prop) (cdr prop)))))
  (define top-level-properties (append canonicalized-properties (set-subtract
                                                                  properties
                                                                  propagate-properties
                                                                  to-canonicalize-properties)))
  (define propagate-ctx (make-immutable-hash propagate-properties))
  `(FPCore
     ,(canonicalize-args args (flatten-context propagate-ctx))
     ,@(flatten-context (make-immutable-hash top-level-properties))
     ,(expr->canon body propagate-ctx)))

(module+ main
  (require racket/cmdline)

  (define (custom-command-line command-line-args)
    (parse-command-line "canonicalizer" command-line-args
      `((once-each
         [("-p" "--propagate")
          ,(lambda (lf . args) (begin
                                 (define other-arg-index (index-where args (λ (x) (eq? (string-ref x 0) #\-))))
                                 (if other-arg-index
                                   (let ([these-args (map (λ (x) (string->symbol (string-append ":" x))) (take args other-arg-index))]
                                         [other-args (list-tail args other-arg-index)])
                                     (*to-propagate* these-args)
                                     (custom-command-line other-args))
                                   (*to-propagate* (map (λ (x) (string->symbol (string-append ":" x))) args)))))
          ("Properties for the canonicalizer to propagate (defaults to precision, round, and math-library)"
           "arg ...")]
         [("-c" "--canonicalize")
          ,(lambda (lf . args) (begin
                                 (define other-arg-index (index-where args (λ (x) (eq? (string-ref x 0) #\-))))
                                 (if other-arg-index
                                   (let ([these-args (map (λ (x) (string->symbol (string-append ":" x))) (take args other-arg-index))]
                                         [other-args (list-tail args other-arg-index)])
                                     (*to-canonicalize* these-args)
                                     (custom-command-line other-args))
                                   (*to-canonicalize* (map (λ (x) (string->symbol (string-append ":" x))) args)))))
          ("Properties for the canonicalizer to canonicalize (defaults to pre and spec)"
           "arg ...")]))
      (lambda (flag-accum) null)
        null))

  ;; Assign the output to _ so it doesn't print
  (define _ (custom-command-line (current-command-line-arguments)))
  (unless (empty? (set-intersect (*to-propagate*) (*to-canonicalize*)))
    (println "Error: a property cannot be both propagated and canonicalized")
    (custom-command-line #("-h")))

  (port-count-lines! (current-input-port))
  (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
    (pretty-write (canonicalize-core expr #:name (format "ex~a" n)))))
