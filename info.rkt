#lang info
(define name "fpbench")
(define version "2.0.3")

(define racket-launcher-names '("fpbench"))
(define racket-launcher-libraries '("main.rkt"))

(define deps '("base"
               "math-lib"
               "generic-flonum"))
(define build-deps '("rackunit-lib"))
