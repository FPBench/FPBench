#lang racket

(require "common.rkt" "fpcore-reader.rkt" "fpcore-visitor.rkt")
(require racket/hash)
(provide fpcore->canon expr->canon fpcore->condensed expr->condensed)

(define (update-ctx ctx props)
  (let-values ([(_ properties) (parse-properties props)])
    (hash-union ctx (make-immutable-hash properties)
                #:combine/key (lambda (k a b) b))))

(define (annotate expr ctx)
  (if (hash-empty? ctx)
      expr
      `(! ,@(apply append (hash-map ctx list)) ,expr)))

(define (visit-op_/canon visitor operator args #:ctx ctx)
  (annotate (visit-op_/transform visitor operator args #:ctx ctx) ctx))

(define (visit-literal/canon visitor x #:ctx ctx)
  (annotate (visit-terminal_/transform visitor x #:ctx ctx) ctx))

(define/transform-expr (expr->canon expr ctx)
  [(visit-! visitor props body #:ctx ctx)
   (visit/ctx visitor body (update-ctx ctx props))]
  [visit-op_ visit-op_/canon]
  [visit-number visit-literal/canon]
  [visit-constant visit-literal/canon])

(define (arg->canon arg ctx)
  (match arg
    [`(! ,props ... ,s)
     (annotate s (update-ctx ctx props))]
    [s (annotate s ctx)]))

(define (keyify sym)
  (let ([s (symbol->string sym)])
    (if (equal? (string-ref s 0) #\:)
        sym
        (string->symbol (string-append ":" s)))))

(define (fpcore->canon prog
                       #:to-propagate [to-propagate '(precision round math-library)]
                       #:to-canonicalize [to-canonicalize '(pre spec)])
  (match-define `(FPCore (,args ...) ,props ... ,body) prog)
  (define propagate-keys (map keyify to-propagate))
  (define canonicalize-keys (map keyify to-canonicalize))

  (define-values (_ properties) (parse-properties props))
  (define ctx (make-immutable-hash
               (filter (lambda (pr) (set-member? propagate-keys (car pr))) properties)))
  (define other-properties (filter (lambda (pr) (not (set-member? propagate-keys (car pr)))) properties))

  `(FPCore
    ,(for/list ([arg args]) (arg->canon arg ctx))
    ,@(apply append (for/list ([pr other-properties])
                      (if (set-member? canonicalize-keys (car pr))
                          (list (car pr) (expr->canon (cdr pr) ctx))
                          (list (car pr) (cdr pr)))))
    ,(expr->canon body ctx)))

(define (new-prop? pr ctx)
  (let ([k (car pr)]
        [v (cdr pr)])
    (if (hash-has-key? ctx k)
        (not (equal? (hash-ref ctx k) v))
        #t)))

(define (update-ctx/condense ctx props)
  (let*-values ([(_ properties) (parse-properties props)]
                [(properties*) (filter (curryr new-prop? ctx) properties)]
                [(ctx*) (hash-union ctx (make-immutable-hash properties*)
                                    #:combine/key (lambda (k a b) b))])
    (values ctx* properties*)))

(define (annotate/condense expr properties*)
  (if (empty? properties*)
      expr
      `(! ,@(apply append (map (lambda (pr) (list (car pr) (cdr pr))) properties*)) ,expr)))

(define (arg->condensed arg ctx)
  (match arg
    [`(! ,props ... ,s)
     (let-values ([(_ properties*) (update-ctx/condense ctx props)])
       (annotate/condense s properties*))]
    [s s]))

(define/transform-expr (expr->condensed expr ctx)
  [(visit-! visitor props body #:ctx ctx)
   (let-values ([(ctx* properties*) (update-ctx/condense ctx props)])
     (annotate/condense (visit/ctx visitor body ctx*) properties*))])

(define (fpcore->condensed prog
                           #:to-condense [to-condense '(pre spec)])
  (match-define `(FPCore (,args ...) ,props ... ,body) prog)
  (define condense-keys (map keyify to-condense))

  (define-values (_ properties) (parse-properties props))
  (define ctx (make-immutable-hash properties))

  `(FPCore
    ,(for/list ([arg args]) (arg->condensed arg ctx))
    ,@(apply append (for/list ([pr properties])
                      (if (set-member? condense-keys (car pr))
                          (list (car pr) (expr->condensed (cdr pr) ctx))
                          (list (car pr) (cdr pr)))))
    ,(expr->condensed body ctx)))

;; legacy command line interface, only handles canonicalizer
(module+ main
  (require racket/cmdline)

  (define *to-canonicalize* (make-parameter '(:pre :spec)))
  (define *to-propagate* (make-parameter '(:precision :round :math-library)))

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
    (pretty-write (fpcore->canon expr #:to-propagate (*to-propagate*) #:to-canonicalize (*to-canonicalize*)))))
