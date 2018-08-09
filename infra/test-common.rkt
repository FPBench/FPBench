#lang racket

(require  "../tools/fpcore.rkt" "../tools/fpimp.rkt")

(provide eval-fuel-expr eval-fuel-stmt sample-double sample-single)

(define ((eval-fuel-expr evaltor fuel [default #f]) expr ctx)
  (let/ec k
    (let eval ([expr expr] [ctx ctx] [fuel fuel])
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (λ (expr ctx) (eval expr ctx (- fuel 1)))) expr ctx)))))

(define ((eval-fuel-stmt evaltor fuel [default #f]) stmts ctx)
  (let/ec k
    (define (eval expr ctx fuel)
      (if (<= fuel 0)
          (k default)
          ((eval-expr* evaltor (curryr eval (- fuel 1))) expr ctx)))
    
    (let loop ([stmts stmts] [ctx ctx] [fuel fuel])
      ((eval-stmts* (curryr eval fuel) (curryr loop (- fuel 1))) stmts ctx))))

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define (sample-single)
  (real->single-flonum (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f))))
