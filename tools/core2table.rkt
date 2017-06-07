#lang racket

(define (attribute? symb)
  (and (symbol? symb) (string-prefix? (symbol->string symb) ":")))

(define (program-body prog)
  (match-define (list 'fpcore (list args ...) props&body ...) prog)
  (let loop ([props&body props&body])
    (if (attribute? (car props&body))
        (loop (cddr props&body))
        (car props&body))))

(define operator-groups
  '((basic + - * / fabs sqrt hypot)
    (exp exp expm1 pow log log1p sinh cosh tanh)
    (trig sin cos tan cotan asin acos atan atan2)
    (cmp < > == <= >= and or)
    (if if)
    (while while)
    (let let)))

(define/match (operators expr)
  [(`(while ,test ([,vars ,inits ,updates] ...) ,res))
   (cons 'while
         (append (operators test)
                 (append-map operators inits)
                 (append-map operators updates)
                 (operators res)))]
  [(`(let ([,vars ,vals] ...) ,body))
   (cons 'let (append (append-map operators vals) (operators body)))]
  [(`(if ,cond ,ift ,iff))
   (cons 'if (append (operators cond) (operators ift) (operators iff)))]
  [((list op args ...)) (cons op (append-map operators args))]
  [((? symbol?)) '()]
  [((? number?)) '()])

(define (operator->group op)
  (or
   (for/first ([name (map car operator-groups)] [ops (map cdr operator-groups)]
               #:when (member op ops))
     name)
   (eprintf "WARNING: Unknown operator ~a\n" op)))

(define (expr-groups expr)
  (remove-duplicates (map operator->group (operators expr))))

(define (make-table progs)
  (printf "<!doctype html>\n<html>\n<head>\n")
  (printf "  <meta charset='utf8' />\n")
  (printf "  <link rel='stylesheet' href='table.css' />\n")
  (printf "  <title>Operators used in FPBench suite</title>\n</head>\n")
  (printf "<body>\n  <table>\n")

  (printf "  <tr><th></th><th>~a</th></tr>\n"
          (string-join (map (compose ~a car) operator-groups) "</th><th>"))
  (for ([prog progs] [n (in-naturals)])
    (printf "  <tr><td>~a</td>"
            (if (member ':name prog)
                (cadr (member ':name prog))
                (format "Problem ~a" n)))
    (define groups (expr-groups (program-body prog)))
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
