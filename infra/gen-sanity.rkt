#lang racket

(require "gen-tests.rkt")

(define sanity-numbers
  '(
    (decnum (0.0
             1.0
             -1.0
             1.5
             0.75))
    ;; (hexnum (0x0.0p+0
    ;;          0x1.0p+0
    ;;          -0x1.0p+0
    ;;          0x1.0p-1
    ;;          -0x1.0p+1
    ;;          0xf.fp-4))
    (rational (0/1
               1/1
               -1/1
               -1/2
               5/4))
    ;; (digits ((digits 0 0 2)
    ;;          (digits 1 0 2)
    ;;          (digits -1 1 2)
    ;;          (digits 3 -1 2)))
    ))

(define sanity-constants
  '(
    (E 2.0 3.0)
    (LOG2E 1.0 2.0)
    (LOG10E 0.25 0.5)
    (LN2 0.5 1.0)
    (PI 3.0 4.0)
    (PI_2 1.0 2.0)
    (PI_4 0.5 1.0)
    (M_1_PI 0.25 0.5)
    (M_2_PI 0.5 1.0)
    (M_2_SQRTPI 1.0 2.0)
    (SQRT2 1.0 2.0)
    (SQRT1_2 0.5 1.0)
    ))

(define sanity-ops
  '(
    (+ ((0.0 1.0) (1.0 1.0) (-1.0 1.0)))
    (- ((0.0 1.0) (1.0 1.0) (-1.0 1.0)))
    (* ((0.0 1.0) (2.0 -1.0) (2.0 2.0)))
    (/ ((1.0 1.0) (-1.0 2.0) (2.0 -2.0)))
    (fabs ((0.0) (-1.0) (1.0)))
    (fma ((0.0 1.0 0.0) (1.0 1.0 0.0) (1.0 -1.0 1.0) (1.0 1.0 1.0) (-1.0 2.0 0.0)))
    (exp ((0.0)))
    (exp2 ((-1.0) (0.0) (1.0) (2.0)))
    (expm1 ((0.0)))
    (log ((1.0)))
    (log10 ((1.0) (10.0)))
    (log2 ((1.0) (2.0) (4.0)))
    (log1p ((0.0)))
    (pow ((0.0 1.0) (1.0 0.0) (1.0 1.0) (1.0 2.0) (2.0 2.0)))
    (sqrt ((0.0) (1.0) (4.0)))
    (cbrt ((0.0) (1.0) (8.0)))
    (hypot ((0.0 0.0) (3.0 4.0)))
    (sin ((0.0)))
    (cos ((0.0)))
    (tan ((0.0)))
    (asin ((0.0)))
    (acos ((1.0)))
    (atan ((0.0)))
    (atan2 ((0.0 1.0)))
    (sinh ((0.0)))
    (cosh ((0.0)))
    (tanh ((0.0)))
    (asinh ((0.0)))
    (acosh ((1.0)))
    (atanh ((0.0)))
    (erf ((0.0)))
    (erfc ((0.0)))
    (tgamma ((1.0) (2.0) (3.0)))
    (lgamma ((1.0) (2.0)))
    (ceil ((0.0) (0.25) (0.75) (1.0)))
    (floor ((0.0) (0.25) (0.75) (1.0)))
    (fmod ((1.0 1.0) (1.25 1.0) (3.0 2.0)))
    (remainder ((1 1) (1.25 1) (3 2)))
    (fmax ((1.0 0.0) (-1.0 0.0) (1.0 -1.0)))
    (fmin ((1.0 0.0) (-1.0 0.0) (1.0 -1.0)))
    (fdim ((2.0 1.0) (1.0 1.0) (1.0 2.0)))
    (copysign ((1.0 1.0) (-1.0 1.0) (1.0 -1.0) (-1.0 -1.0)))
    (trunc ((0.0) (0.25) (-0.75) (1.0)))
    (round ((0.0) (0.25) (0.75) (1.0)))
    (nearbyint ((0.0) (0.25) (0.75) (1.0)))
    ;; (cast ((-1.0) (0.0) (1.0)))
    ))

(define sanity-bool-ops
  '(
    (< ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (> ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (<= ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (>= ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (== ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (!= ((1.0 0.0) (0.0 1.0) (-1.0 0.0) (0.0 -1.0) (1.0 -1.0) (-1.0 1.0) (0.0 0.0)))
    (and ((TRUE TRUE) (TRUE FALSE) (FALSE TRUE) (FALSE FALSE)))
    (or ((TRUE TRUE) (TRUE FALSE) (FALSE TRUE) (FALSE FALSE)))
    (not ((TRUE) (FALSE)))
    (isinf ((0.0) (1.0) (INFINITY) (NAN)))
    (isnan ((0.0) (1.0) (INFINITY) (NAN)))
    (isfinite ((0.0) (1.0) (INFINITY) (NAN)))
    (isnormal ((0.0) (1.0) (INFINITY) (NAN)))
    (signbit ((0.0) (1.0) (-1.0)))
    ))


(number-suite->tests sanity-numbers '() "../tests/sanity-numbers.fpcore")
(constant-suite->tests sanity-constants '() "../tests/sanity-constants.fpcore")
(op-suite->tests sanity-ops '() "../tests/sanity-ops.fpcore")
(bool-op-suite->tests sanity-bool-ops '() "../tests/sanity-bool-ops.fpcore")

(op-suite->arg-tests sanity-ops '() "../tests/test-ops.fpcore")
(bool-op-suite->arg-tests sanity-bool-ops '() "../tests/test-bool-ops.fpcore")
