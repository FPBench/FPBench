#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2go.rkt")

(define (compile->go prog ctx type test-file)
  (define bit-length (match type ['binary64 "64"] ['binary32 "32"]))
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "package main\n\nimport (\n\"math\"\n\"fmt\"\n\"os\"\n\"strconv\"\n)\n\nvar _ = math.E\nvar _ = os.Stdin\n\n")
      (fprintf p "func Use(vals ...interface{}) {\n\tfor _, val := range vals {\n\t\t_ = val\n\t}\n}\n\n")
      (fprintf p go-func-header)
      (fprintf p "func strtox(arg string) float~a {\n\ts, _ := strconv.ParseFloat(arg, ~a)\n\treturn s\n}\n\n" bit-length bit-length)
      (fprintf p "~a\n" (core->go prog "f"))
      (fprintf p "func main(){")
      (fprintf p "fmt.Printf(\"%.20g\", f(~a))}\n" 
          (string-join (map (curry format "strtox(os.Args[~a])") (map add1 (range N))) ", "))))
  (define bin-file (string-replace test-file ".go" "-go.bin"))
  (system (format "go build -o ~a ~a" bin-file test-file))
  bin-file)

(define (float->string x)
  (match x
   [(? gfl?)  (gfl->string x)]
   [(? real?) (~a x)]))

(define (run<-go exec-name ctx type number)
  (define out
    (with-output-to-string
     (λ ()
      (system 
        (string-join 
          (cons exec-name 
            (map (λ (x)
                  (match x
                   [+nan.0 "NaN"] [+inf.0 "+Inf"] [-inf.0 "-Inf"]
                   [x (float->string x)]))
                 (dict-values ctx))) 
          " ")))))
  (define out*
    (match out
      ["NaN" "+nan.0"]
      ["+Inf" "+inf.0"]
      ["-Inf" "-inf.0"]
      [x x]))
  (cons
    (parameterize ([gfl-exponent 11] [gfl-bits 64]) (gfl out*))
    out*))

(define (go-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else (<= (abs (gfls-between a b)) ulps)]))

(define (go-format-args var val type)
  (format "~a = ~a" var val))

(define (go-format-output result)
  (format "~a" result))

(define go-tester (tester "go" compile->go run<-go go-equality go-format-args go-format-output (const #t) go-supported #f))

; Command line
(module+ main (parameterize ([*tester* go-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.go")])
    (exit state))))
