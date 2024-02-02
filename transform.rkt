#lang racket

(require "src/fpcore-reader.rkt"
         "src/fpcore-extra.rkt"
         "src/common-subexpr-elim.rkt"
         "src/canonicalizer.rkt"
         "src/multi-command-line.rkt")

(provide transform-main transform-body make-transform-ctx)

(struct transform-ctx (passes default-to-propagate default-to-canonicalize canon-to-propagate canon-to-canonicalize in-file out-file) #:transparent)

(define (make-transform-ctx passes default-to-propagate default-to-canonicalize canon-to-propagate canon-to-canonicalize in-file out-file)
  (transform-ctx passes default-to-propagate default-to-canonicalize canon-to-propagate canon-to-canonicalize in-file out-file))

(define (transform-main argv stdin-port stdout-port)
  (define passes (box '()))
  (define (register-pass pass shape)
    (set-box! passes (cons (list pass shape) (unbox passes))))

  (define default-to-propagate '(precision round math-library))
  (define default-to-canonicalize '(pre spec))

  (define canon-to-propagate (box default-to-propagate))
  (define canon-to-canonicalize (box default-to-canonicalize))
  (define (+p prop)
    (set-box! canon-to-propagate (set-add (unbox canon-to-propagate) (string->symbol prop))))
  (define (-p prop)
    (set-box! canon-to-propagate (set-remove (unbox canon-to-propagate) (string->symbol prop))))
  (define (+c prop)
    (set-box! canon-to-canonicalize (set-add (unbox canon-to-canonicalize) (string->symbol prop))))
  (define (-c prop)
    (set-box! canon-to-canonicalize (set-remove (unbox canon-to-canonicalize) (string->symbol prop))))

  (multi-command-line
   #:program "transform"
   #:argv argv
   #:multi
   ["--unroll" unroll_ "Unroll the first N iterations of each loop"
               (register-pass (curry fpcore-unroll-loops (string->number unroll_)) 'one-to-one)]
   ["--skip-loops" "Replace loops with their bodies, as if the were executed zero times"
                   (register-pass fpcore-skip-loops 'one-to-one)]
   ["--precondition-ranges" "Weaken preconditions to a conjunction (one per argument) of a disjunction of ranges"
                            (register-pass (curry fpcore-precondition-ranges #:single-range #f) 'one-to-one)]
   ["--precondition-range" "Weaken preconditions to a conjunction of single ranges for each variable"
                           (register-pass (curry fpcore-precondition-ranges #:single-range #t) 'one-to-one)]
   ["--remove-let" "Remove let bindings by substituting into the body expressions"
                    (register-pass fpcore-remove-let 'one-to-one)]
   ["--expand-let*" "Expand each let* to a series of nested let expressions"
                    (register-pass fpcore-expand-let* 'one-to-one)]
   ["--expand-while*" "Expand each while* to a while loop with nested let* expressions"
                      (register-pass fpcore-expand-while* 'one-to-one)]
   ["--expand-for" "Expand for and for* loops (if possible)"
                    (register-pass fpcore-expand-for 'one-to-one)]
   ["--fuse-let" "Fuses nested let/let* bindings into a single let* binding"
                    (register-pass fpcore-fuse-let 'one-to-one)]
   ["--rational-constants" "Convert every number to a rational"
                           (register-pass 'rational-constants 'one-to-one)]
   ["--cse" "Lift each common subexpression to an intermediate variable bound by a let* expression"
            (register-pass core-common-subexpr-elim 'one-to-one)]
   ["--subexprs" "Break an FPCore down into separate cores for each subexpression"
                 (register-pass fpcore-all-subexprs 'one-to-many)]
   ;; all the crazy canonicalizer controls
   [("+p" "++propagate-prop") prop_ "Propagate this property during canonicalization"
                             (+p prop_)]
   [("-p" "--propagate-prop") prop_ "Don't propagate this property during canonicalization"
                             (-p prop_)]
   [("+c" "++canonicalize-prop") prop_ "Recursively canonicalize this property during canonicalization"
                                (+c prop_)]
   [("-c" "--canonicalize-prop") prop_ "Don't recursively canonicalize this property during canonicalization"
                                (-c prop_)]
   ["--propagate-clear" "Clear the list of properties to propagate during canonicalization"
                       (set-box! canon-to-propagate '())]
   ["--propagate-default" "Restore the list of propertie to propagate to default: (precision round math-library)"
                         (set-box! canon-to-propagate default-to-propagate)]
   ["--canonicalize-clear" "Clear the list of properties to recursively canonicalize during canonicalization"
                       (set-box! canon-to-canonicalize '())]
   ["--canonicalize-default" "Restore the list of propertie to recursively canonicalize to default: (pre spec)"
                            (set-box! canon-to-canonicalize default-to-canonicalize)]
   ["--canonicalize" "Canonicalize rounding context annotations, according to previously supplied settings"
                     (let* ([to-propagate (unbox canon-to-propagate)]
                            [to-canonicalize (unbox canon-to-canonicalize)]
                            [canonicalizer-pass (lambda (expr) (fpcore->canon expr
                                                                              #:to-propagate to-propagate
                                                                              #:to-canonicalize to-canonicalize))])
                       (register-pass canonicalizer-pass 'one-to-one))]
   ["--condense" "Condense rounding context annotations"
                 (let* ([to-condense (unbox canon-to-canonicalize)]
                        [condenser-pass (lambda (expr) (fpcore->condensed expr
                                                                          #:to-condense to-condense))])
                        (register-pass condenser-pass 'one-to-one))]
   #:args (in-file out-file)
   (transform-body (make-transform-ctx passes default-to-propagate default-to-canonicalize canon-to-propagate canon-to-canonicalize in-file out-file) 
        stdin-port stdout-port)))

(define (transform-body ctx stdin-port stdout-port)
  (define (transform-passes)
    (reverse (unbox (transform-ctx-passes ctx))))
   (define input-port
     (if (equal? (transform-ctx-in-file ctx) "-")
         stdin-port
         (open-input-file (transform-ctx-in-file ctx) #:mode 'text)))
   (define output-port
     (if (equal? (transform-ctx-out-file ctx) "-")
         stdout-port
         (open-output-file (transform-ctx-out-file ctx) #:mode 'text #:exists 'truncate)))

   (port-count-lines! input-port)
   (for ([expr (in-port (curry read-fpcore (if (equal? (transform-ctx-in-file ctx) "-") "stdin" (transform-ctx-in-file ctx))) input-port)] [n (in-naturals)])

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
       (displayln (pretty-fpcore expr) output-port)
       (newline output-port))))

(module+ main
  (transform-main (current-command-line-arguments) (current-input-port) (current-output-port)))
