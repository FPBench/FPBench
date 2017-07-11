#lang racket

(require "common.rkt" "fpcore.rkt")

(provide fpimp? statement? eval-stmts* eval-stmts racket-run-fpimp)

(define (fpimp? thing)
  (match thing
    [`(FPImp (,(? symbol?) ...) ,props&body ...)
     (define-values (body props) (parse-properties props&body))
     (andmap statement? body)]
    [_ false]))

(define-by-match statement?
  `(output ,(? expr?) ...)
  `[= ,(? symbol?) ,(? expr?)]
  (cons 'if (? if-branches?))
  `(while ,(? expr?) ,(? statement?) ...))

(define-by-match if-branches?
  (cons `[,(? expr?) ,(? statement?) ...] (? if-branches?))
  (list `[else ,(? statement?) ...])
  (list))

(define/contract ((eval-stmts* eval-expr rec) stmts ctx)
  (-> (-> expr? context/c any/c)
      (-> (listof statement?) context/c any/c)
      (-> (listof statement?) context/c any/c))
  (match-define (cons stmt rest) stmts)
  (match stmt
    [`(output ,exprs ...)
     (map (curryr eval-expr ctx) exprs)]
    [`[= ,var ,expr]
     (rec rest (dict-set ctx var (eval-expr expr ctx)))]
    [`(if)
     (rec rest ctx)]
    [`(if [else ,body ...])
     (rec (append body rest) ctx)]
    [`(if [,test ,body ...] ,tests ...)
     (if (eval-expr test ctx)
         (rec (append body rest) ctx)
         (rec (cons `(if ,@tests) rest) ctx))]
    [`(while ,test ,body ...)
     (if (eval-expr test ctx)
         (rec (append body stmts) ctx)
         (rec rest ctx))]))

(define/contract ((eval-stmts eval-expr) stmts ctx)
  (-> (-> expr? context/c any/c) (-> (listof statement?) context/c any/c))
  (let loop ([stmts stmts] [ctx ctx])
    ((eval-stmts* eval-expr loop) stmts ctx)))

(define/contract (racket-run-fpimp prog vals)
  (-> fpimp? (listof real?) (listof real?))
  (match-define `(FPImp (,vars ...) ,props&body ...) prog)
  (define-values (body props) (parse-properties props&body))
  (define evaltor
    (match (dict-ref props ':type 'binary64)
      ['binary64 racket-double-evaluator]
      ['binary32 racket-single-evaluator]))
  ((eval-stmts (eval-expr racket-double-evaluator)) body (map cons vars vals)))

(module+ main
  (command-line
   #:program "fpimp.rkt"
   #:args args
   (let ([vals (map (compose real->double-flonum string->number) args)])
     (for ([prog (in-port read)])
       (printf "~a\n" (racket-run-fpimp prog vals))))))

