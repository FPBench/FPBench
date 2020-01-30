#lang racket

(require math/flonum)
(require "test-common.rkt" "test-imperative.rkt" "../src/core2go.rkt")

(define (compile->go prog type test-file)
  (define bit-length (match type ['binary64 "64"] ['binary32 "32"]))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (length (second prog)))
      (fprintf p "package main\n\nimport (\n\"math\"\n\"fmt\"\n\"os\"\n\"strconv\"\n)\n\nvar _ = math.E\nvar _ = os.Stdin\n\n")
      (fprintf p "func Use(vals ...interface{}) {\n\tfor _, val := range vals {\n\t\t_ = val\n\t}\n}\n\n")
      (fprintf p "func strtox(arg string) float~a {\n\ts, _ := strconv.ParseFloat(arg, ~a)\n\treturn s\n}\n\n" bit-length bit-length)
      (fprintf p "~a\n" (core->go prog "f"))
      (fprintf p "func main(){")
      (fprintf p "fmt.Printf(\"%.20g\", f(~a))}\n" 
          (string-join (map (curry format "strtox(os.Args[~a])") (map add1 (range N))) ", "))))
  (define bin-file (string-replace test-file ".go" ".bin"))
  (system (format "go build -o ~a ~a" bin-file test-file))
  bin-file)

(define (run<-go exec-name ctx type)
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name (map (compose ~a real->double-flonum) (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["NaN" "+nan.0"]
      ["+Inf" "+inf.0"]
      ["-Inf" "-inf.0"]
      [x x]))
  (cons
    ((match type
      ['binary64 real->double-flonum]
      ['binary32 real->single-flonum])
    (string->number out*)) out*))

(define (go-equality a b ulps)
  (match (list a b)
    ['(timeout timeout) true]
    [else
      (or (= a b)
          (and (nan? a) (nan? b))
          (<= (abs (flonums-between a b)) ulps))]))

(define go-tester (tester compile->go run<-go go-supported go-equality))

; Command line
(module+ main (parameterize ([*tester* go-tester])
  (test-imperative (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.go")))
