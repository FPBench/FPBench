#lang racket
(require "common.rkt")

(define operator-groups
  '((basic + - * / fabs fma sqrt hypot fmin fmax fdim)
    (cmp < > == <= >= != and or not)
    (fpinternal isfinite isinf isnan isnormal signbit copysign)
    (exp exp expm1 exp2 expm1 pow log log10 log2 log1p)
    (trig sin cos tan cotan asin acos atan atan2)
    (special erf erfc tgamma lgamma cbrt)
    (rounding ceil floor trunc round nearbyint)
    (modulo fmod remainder)
    (hyperbolic sinh cosh tanh asinh acosh atanh)
    (if if)
    (while while)
    (let let)))

(module+ test
  (require rackunit)

  (define all-operators-in-groups (apply append (dict-values operator-groups)))

  (with-check-info (['missing (set-subtract operators all-operators-in-groups)])
                   (check subset? operators all-operators-in-groups)))

(define/match (operators-in expr)
  [(`(while ,test ([,vars ,inits ,updates] ...) ,res))
   (cons 'while
         (append (operators-in test)
                 (append-map operators-in inits)
                 (append-map operators-in updates)
                 (operators-in res)))]
  [(`(let ([,vars ,vals] ...) ,body))
   (cons 'let (append (append-map operators-in vals) (operators-in body)))]
  [(`(if ,cond ,ift ,iff))
   (cons 'if (append (operators-in cond) (operators-in ift) (operators-in iff)))]
  [((list op args ...)) (cons op (append-map operators-in args))]
  [((? symbol?)) '()]
  [((? number?)) '()])

(define (operator->group op)
  (or
   (for/first ([(name ops) (in-dict operator-groups)]
               #:when (set-member? ops op))
     name)
   (eprintf "WARNING: Unknown operator ~a\n" op)))

(define (expr-groups expr)
  (remove-duplicates (map operator->group (operators-in expr))))

(define (make-table progs)
  (printf "<!doctype html>\n<html>\n<head>\n")
  (printf "  <meta charset='utf8' />\n")
  (printf "  <link rel='stylesheet' href='table.css' />\n")
  (printf "  <title>Operators used in FPBench suite</title>\n</head>\n")
  (printf "<body>\n  <table>\n")

  (printf "  <tr><th></th><th>~a</th></tr>\n"
          (string-join (map (compose ~a car) operator-groups) "</th><th>"))
  (for ([prog progs] [n (in-naturals)])
    (match-define `(FPCore (,vars ...) ,properties ... ,body) prog)
    (printf "  <tr><td>~a</td>"
            (if (member ':name prog)
                (cadr (member ':name prog))
                (format "Problem ~a" n)))
    (define groups (expr-groups body))
    (for ([group (map car operator-groups)])
      (printf "<td>~a</td>" (if (member group groups) "✓" "")))
    (printf "</tr>\n"))
  (printf "  </table>\n</body>\n</html>\n"))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "core2table.rkt"
   #:args ()
   (make-table (in-port read (current-input-port)))))
