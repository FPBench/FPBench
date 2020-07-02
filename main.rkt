#lang racket/base

(require
 "src/canonicalizer.rkt"
 "src/common.rkt"
 "src/common-subexpr-elim.rkt"
 "src/compilers.rkt"
 "src/core2c.rkt"
 "src/core2cml.rkt"
 "src/core2fptaylor.rkt"
 "src/core2gappa.rkt"
 "src/core2go.rkt"
 "src/core2js.rkt"
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
 "infra/filter.rkt")

(provide
 (all-from-out
  "src/canonicalizer.rkt"
  "src/common.rkt"
  "src/common-subexpr-elim.rkt"
  "src/compilers.rkt"
  "src/core2c.rkt"
  "src/core2cml.rkt"
  "src/core2fptaylor.rkt"
  "src/core2gappa.rkt"
  "src/core2go.rkt"
  "src/core2js.rkt"
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
  ))

(module+ main
  (eprintf "FPBench provides with two tools:\n")
  (eprintf "  export.rkt - export FPCore to other languages\n")
  (eprintf "  transform.rkt - apply program transformations to FPCores\n")
  (eprintf "Run those tools with --help for more information.\n"))
