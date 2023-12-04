#lang racket

(require 
  "transform.rkt"
  "evaluate.rkt"
  "export.rkt")

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
  (cond 
    [(= 0 (vector-length (current-command-line-arguments)))
      (eprintf "FPBench provides with two tools:\n")
      (eprintf "  export/compile - export FPCore to other languages\n")
      (eprintf "  transform - apply program transformations to FPCores\n")
      (eprintf "Run those tools with --help for more information.\n")]
    [(equal? (vector-ref (current-command-line-arguments) 0) "export")
      (export-main (vector-drop (current-command-line-arguments) 1) (current-input-port) (current-output-port))]
    [(equal? (vector-ref (current-command-line-arguments) 0) "compile")
      (export-main (vector-drop (current-command-line-arguments) 1) (current-input-port) (current-output-port))]
    [(equal? (vector-ref (current-command-line-arguments) 0) "transform")
      (transform-main (vector-drop (current-command-line-arguments) 1) (current-input-port) (current-output-port))]
    [(equal? (vector-ref (current-command-line-arguments) 0) "evaluate")
      (evaluate-main (vector-drop (current-command-line-arguments) 1) (current-input-port) (current-output-port))]
    ))
