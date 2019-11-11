#lang racket/base

(require
 "src/canonicalizer.rkt"
 "src/common.rkt"
 "src/compiler.rkt"
 "src/core2c.rkt"
 "src/core2fptaylor.rkt"
 "src/core2gappa.rkt"
 "src/core2go.rkt"
 "src/core2js.rkt"
 "src/core2scala.rkt"
 "src/core2smtlib2.rkt"
 "src/core2wls.rkt"
 "infra/core2json.rkt"
 "infra/filter.rkt"
 "src/range-analysis.rkt"
 "src/common-subexpr-elim.rkt")

(provide
 (all-from-out
  "src/canonicalizer.rkt"
  "src/common.rkt"
  "src/compiler.rkt"
  "src/core2c.rkt"
  "src/core2fptaylor.rkt"
  "src/core2gappa.rkt"
  "src/core2go.rkt"
  "src/core2js.rkt"
  "src/core2scala.rkt"
  "src/core2smtlib2.rkt"
  "src/core2wls.rkt"
  "infra/core2json.rkt"
  "infra/filter.rkt"
  "src/range-analysis.rkt"
  "src/common-subexpr-elim.rkt"))

(module+ main
  (eprintf "FPBench provides with two tools:\n")
  (eprintf "  export.rkt - export FPCore to other languages\n")
  (eprintf "  transform.rkt - apply program transformations to FPCores\n")
  (eprintf "Run those tools with --help for more information.\n"))
