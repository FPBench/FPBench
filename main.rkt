#lang racket

(require 
  "transform.rkt"
  "evaluate.rkt"
  "export.rkt"
  "toolserver.rkt"
  "infra/filter.rkt")

(require
 "src/canonicalizer.rkt"
 "src/common.rkt"
 "src/common-subexpr-elim.rkt"
 "src/compilers.rkt"
 "src/core2c.rkt"
 "src/core2cakeml.rkt"
 "src/core2fptaylor.rkt"
 "src/core2fortran03.rkt"
 "src/core2gappa.rkt"
 "src/core2go.rkt"
 "src/core2haskell.rkt"
 "src/core2java.rkt"
 "src/core2js.rkt"
 "src/core2julia.rkt"
 "src/core2matlab.rkt"
 "src/core2ocaml.rkt"
 "src/core2python.rkt"
 "src/core2scala.rkt"
 "src/core2smtlib2.rkt"
 "src/core2sollya.rkt"
 "src/core2tex.rkt"
 "src/core2vivado.rkt"
 "src/core2wls.rkt"
 "src/fpcore-extra.rkt"
 "src/fpcore-checker.rkt"
 "src/fpcore-interpreter.rkt"
 "src/fpcore-reader.rkt"
 "src/imperative.rkt"
 "src/multi-command-line.rkt"
 "src/range-analysis.rkt"
 "src/supported.rkt"

 "infra/core2json.rkt"
 "infra/filter.rkt"
 "infra/gen-expr.rkt")

(provide
 (all-from-out
  "src/canonicalizer.rkt"
  "src/common.rkt"
  "src/common-subexpr-elim.rkt"
  "src/compilers.rkt"
  "src/core2c.rkt"
  "src/core2cakeml.rkt"
  "src/core2fptaylor.rkt"
  "src/core2fortran03.rkt"
  "src/core2gappa.rkt"
  "src/core2go.rkt"
  "src/core2haskell.rkt"
  "src/core2java.rkt"
  "src/core2js.rkt"
  "src/core2julia.rkt"
  "src/core2matlab.rkt"
  "src/core2ocaml.rkt"
  "src/core2python.rkt"
  "src/core2scala.rkt"
  "src/core2smtlib2.rkt"
  "src/core2sollya.rkt"
  "src/core2tex.rkt"
  "src/core2vivado.rkt"
  "src/core2wls.rkt"
  "src/fpcore-extra.rkt"
  "src/fpcore-checker.rkt"
  "src/fpcore-interpreter.rkt"
  "src/fpcore-reader.rkt"
  "src/imperative.rkt"
  "src/range-analysis.rkt"
  "src/supported.rkt"

  "infra/core2json.rkt"
  "infra/filter.rkt"
  "infra/gen-expr.rkt"
  ))



(module+ main
  (define *lang* (make-parameter #f))
  (define *runtime* (make-parameter #f))
  (define *bare* (make-parameter #f))
  (define *namespace* (make-parameter "main"))
  (define *in-file* (make-parameter "-"))
  (define *out-file* (make-parameter "-"))
  (define *rel-error* (make-parameter #f))
  (define *scale* (make-parameter 1))
  (define suppress-warnings #f)
  (define check-types? #t)
  (define ragged-check? #t)
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
  (define batches (box '()))
  (define (register-batch in out)
    (set-box! batches (cons (list in out) (unbox batches))))
  (define invert? #f)

  (multi-command-line
    #:program "fpbench"
    #:subcommands
    [export "Export FPCore to other Languages"
      #:once-each 
      ["--lang" lang_ "Output language to compile FPCore to"
        (*lang* lang_)]
      ["--bare" "Skip the file header and footer"
        (*bare* #t)]
      ["--namespace" namespace_ "Name of namespace or package to export benchmarks into"
        (*namespace* namespace_)]
      ["--runtime" runtime_ "Name of library to invoke mathematical operations on"
        (*runtime* runtime_)]
      ["--rel-error" "For Gappa export, produce expressions for relative instead of absolute error"
        (*rel-error* #t)]
      ["--scale" scale_ "For FPTaylor export, the scale factor for operations which are not correctly rounded"
        (*scale* (string->number scale_))]
      ["--suppress" "For Sollya, division by zero will not produce a warning"
        (set! suppress-warnings #t)]
      #:args (in-file out-file)
      (export-body (make-export-ctx 
        (*lang*) (*runtime*) (*bare*) (*namespace*) (*rel-error*) (*scale*) suppress-warnings in-file out-file) 
        (current-input-port) (current-output-port))]
    [compile "Compile FPCore to other Languages"
      #:once-each 
      ["--lang" lang_ "Output language to compile FPCore to"
        (*lang* lang_)]
      ["--bare" "Skip the file header and footer"
        (*bare* #t)]
      ["--namespace" namespace_ "Name of namespace or package to export benchmarks into"
        (*namespace* namespace_)]
      ["--runtime" runtime_ "Name of library to invoke mathematical operations on"
        (*runtime* runtime_)]
      ["--rel-error" "For Gappa export, produce expressions for relative instead of absolute error"
        (*rel-error* #t)]
      ["--scale" scale_ "For FPTaylor export, the scale factor for operations which are not correctly rounded"
        (*scale* (string->number scale_))]
      ["--suppress" "For Sollya, division by zero will not produce a warning"
        (set! suppress-warnings #t)]
      #:args (in-file out-file)
      (export-body (make-export-ctx 
        (*lang*) (*runtime*) (*bare*) (*namespace*) (*rel-error*) (*scale*) suppress-warnings in-file out-file) 
        (current-input-port) (current-output-port))]
    [transform "Transform FPCore expressions"
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
        (current-input-port) (current-output-port))]
    [evaluate "Evaluate an FPCore expression"
      #:once-each
      [("-i" "--in-file") in_file_ "Input file to read FPCores from"
        (*in-file* in_file_)]
      [("-o" "--out-file") out_file_ "Output file to write evaluated results to"
        (*out-file* out_file_)]
      ["--no-check" "Disables type checking altogether (check level 1). Recursive, mutually recursive, and out-of-order FPCores can be evaluated in this mode"
        (set! check-types? #f)]
      ["--no-ragged-check" "Disables checking for ragged dimension sizes"
        (set! ragged-check? #f)]
      #:args args
      (evaluate-body (make-evaluate-ctx (*in-file*) (*out-file*) check-types? ragged-check? args) 
        (current-input-port) (current-output-port))]
    [toolserver "FPBench toolserver"
      #:multi 
      ["--batch" batch_in_ batch_out_ "Process commands from a file"
              (register-batch batch_in_ batch_out_)]
      #:args ()
      (toolserver-body batches (current-input-port) (current-output-port))]
    [filter "Filter FPCores"
      #:once-each
      [("-v" "--invert") "Invert the meaning of the filter"
        (set! invert? #t)]
      #:args values
      (filter-body invert? values (current-input-port) (current-output-port))]
    
    #:args files 
    (match files
      ['()
        (eprintf "Please specify a FPBench tool, such as `fpbench evaluate`.\n")
        (eprintf "See <https://fpbench.org/tools.html> for more.\n")]
      [(cons tool _)
        (eprintf "Unknown FPBench tool `~a`.Run those tools with --help for more information.\n" tool)
        (eprintf "See <https://fpbench.org/tools.html> for more.\n")])))