#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2haskell.rkt")

(define (arg-info prog type)
  (define args
    (match prog
     [(list 'FPCore (list args ...) rest ...) args]
     [(list 'FPCore name (list args ...) rest ...) args]))
  (values (length args)
          (for/list ([arg args])
            (match arg
             [(list '! props ... name) (dict-ref (apply dict-set* '() props) ':precision type)]
             [_ type]))))

(define (compile->haskell prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define-values (N arg-types) (arg-info prog type))
      (fprintf p "import System.Environment\nimport Text.Printf\n")
      (fprintf p "~a~a\n\n" (haskell-header) (core->haskell prog "f"))
      (fprintf p "main = do\n  args <- getArgs\n")
      (fprintf p "  printf \"%.17g\" ")
      (if (zero? N)
          (fprintf p "f")
          (fprintf p "(f ~a)"
                     (string-join
                       (for/list ([i (in-range N)] [type arg-types])
                         (format "(read (args !! ~a) :: ~a)" i (type->haskell type)))
                       " ")))))
  (define bin-file (string-replace test-file ".hs" ""))
  (system (format "ghc -o ~a ~a >> /dev/null" bin-file test-file))
  bin-file)

(define (run<-haskell exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "NaN"]
       ["+inf.0" "Infinity"]
       ["-inf.0" "-Infinity"]
       [x x])))
  (define out
    (with-output-to-string
      (λ ()
        (system (format "~a ~a" exec-name (string-join in " "))))))
  (define out*
    (match out
     ["NaN" "+nan.0"]
     ["Infinity" "+inf.0"]
     ["-Infinity" "-inf.0"]
     [x x]))
  (cons (->value out* type) out*))

(define (haskell-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (haskell-format-args var val type)
  (format "~a = ~a" var val))

(define (haskell-format-output result)
  (format "~a" result))

(define haskell-tester
  (tester "haskell"
    compile->haskell
    run<-haskell
    haskell-equality
    haskell-format-args
    haskell-format-output
    (const #t)
    haskell-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* haskell-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.hs")))
  (exit state))
