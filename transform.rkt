#lang racket

(module+ main
  (require racket/cmdline)

  (define passes (box '()))
  (define (register-pass p)
    (set-box! passes (cons p (unbox passes))))
  (define (transform-passes)
    (reverse (unbox passes)))

  (command-line
   #:program "transform.rkt"
   #:multi
   ["--unroll" unroll_ "Unroll the first N iterations of each loop"
               (register-pass (list 'unroll (string->number unroll_)))]
   ["--skip-loops" "Replace loops with their bodies, as if the were executed zerotimes"
                   (register-pass 'skip-loops)]
   ["--precondition-ranges" "Weaken preconditions to a conjunction (one per argument) of a disjunction of ranges"
                            (register-pass 'precondition-ranges)]
   ["--precondition-range" "Weaken preconditions to a conjunction of single ranges for each variable"
                           (register-pass 'precondition-range)]
   ["--expand-let*" "Expand each let* to a series of nested let expressions"
                    (register-pass 'expand-let*)]
   ["--expand-while*" "Expand each while* to a while loop with nested let* expressions"
                      (register-pass 'expand-while*)]
   ["--cse" "Lift each common subexpression to an intermediate variable bound by a let* expression"
            (register-pass 'cse)]
   ["--rational-constants" "Convert every number to a rational"
                           (register-pass 'rational-constants)]
   #:args (in-file out-file)

   ;; placeholder
   (printf "in file: ~a, out file: ~a\n\n" in-file out-file)
   (for ([pass (transform-passes)] [n (in-naturals)])
     (printf "  pass ~a: ~a\n" (+ n 1) pass))

   ))
