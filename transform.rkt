#lang racket

(require "src/fpcore.rkt"
         "src/fpcore-extra.rkt"
         "src/common-subexpr-elim.rkt")

(provide transform-main)

(define (transform-main argv stdin-port stdout-port)
  (define passes (box '()))
  (define (register-pass pass shape)
    (set-box! passes (cons (list pass shape) (unbox passes))))
  (define (transform-passes)
    (reverse (unbox passes)))

  (command-line
   #:program "transform.rkt"
   #:argv argv
   #:multi
   ["--unroll" unroll_ "Unroll the first N iterations of each loop"
               (register-pass (curry fpcore-unroll-loops (string->number unroll_)) 'one-to-one)]
   ["--skip-loops" "Replace loops with their bodies, as if the were executed zerotimes"
                   (register-pass fpcore-skip-loops 'one-to-one)]
   ["--precondition-ranges" "Weaken preconditions to a conjunction (one per argument) of a disjunction of ranges"
                            (register-pass (curry fpcore-precondition-ranges #:single-range #f) 'one-to-one)]
   ["--precondition-range" "Weaken preconditions to a conjunction of single ranges for each variable"
                           (register-pass (curry fpcore-precondition-ranges #:single-range #t) 'one-to-one)]
   ["--expand-let*" "Expand each let* to a series of nested let expressions"
                    (register-pass fpcore-expand-let* 'one-to-one)]
   ["--expand-while*" "Expand each while* to a while loop with nested let* expressions"
                      (register-pass fpcore-expand-while* 'one-to-one)]
   ["--rational-constants" "Convert every number to a rational"
                           (register-pass 'rational-constants 'one-to-one)]
   ["--cse" "Lift each common subexpression to an intermediate variable bound by a let* expression"
            (register-pass core-common-subexpr-elim 'one-to-one)]
   ["--subexprs" "Break an FPCore down into separate cores for each subexpression"
                 (register-pass fpcore-all-subexprs 'one-to-many)]
   #:args (in-file out-file)

   (define input-port
     (if (equal? in-file "-")
         stdin-port
         (open-input-file in-file #:mode 'text)))
   (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))

   (port-count-lines! input-port)
   (for ([expr (in-port (curry read-fpcore (if (equal? in-file "-") "stdin" in-file)) input-port)] [n (in-naturals)])

     (define working-exprs (box (list expr)))
     (define (apply-pass pass shape)
       (case shape
         ['one-to-one (set-box! working-exprs
                                (for/list ([expr (unbox working-exprs)])
                                  (pass expr)))]
         ['one-to-many (set-box! working-exprs
                                 (apply append (for/list ([expr (unbox working-exprs)])
                                  (pass expr))))]))

     (for ([pass-shape (transform-passes)])
       (apply apply-pass pass-shape))

     (for ([expr (unbox working-exprs)])
       (pretty-print expr output-port 1)))))

(module+ main
  (transform-main (current-command-line-arguments) (current-input-port) (current-output-port)))
