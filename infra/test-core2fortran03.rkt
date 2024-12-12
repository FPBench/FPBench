#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2fortran03.rkt" "../src/evaluator.rkt")

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

(define (compile->fortran prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define-values (N arg-types) (arg-info prog type))
      (define args* (for/list ([i (in-range 1 (+ N 1))]) (format "a~a" i)))
      (define vals* (for/list ([i (in-range 1 (+ N 1))]) (format "r~a" i)))
      (fprintf p "~a\n~a\n\n" (fortran-header) (core->fortran prog "f"))
      (fprintf p "program main\n    integer :: i\n")
      (when (> N 0) (fprintf p "    character (len=65) :: ~a\n" (string-join args* ", ")))
      (for ([val vals*] [type arg-types])
        (fprintf p "    ~a :: ~a\n" (type->fortran type) val))
      (fprintf p "    ~a :: ~a\n\n" (type->fortran type) "f")
      (for ([i (in-range 1 (+ N 1))])
        (fprintf p "    call get_command_argument(~a, a~a)\n" i i))
      (for ([i (in-range 1 (+ N 1))])
        (fprintf p "    read (unit=a~a, fmt=*) r~a\n" i i))
      (fprintf p "    print *, f(~a)\n" (string-join vals* ", "))
      (fprintf p "end program main")))
  (define exe-file (string-replace test-file ".f03" ".bin"))
  (system (format "cc -std=f2003 -ffree-line-length-512 -o ~a ~a -lgfortran -lm" exe-file test-file))
  exe-file)

(define (run<-fortran exec-name ctx type number)
  (define in
    (for/list ([val (dict-values ctx)])
      (match (value->string val)
       ["+nan.0" "nan"]
       ["+inf.0" "inf"]
       ["-inf.0" "-inf"]
       [x x])))
  (define out
    (with-output-to-string
      (λ () (system (string-join (cons exec-name in) " ")))))
  (define out*
    (match (string-trim out)
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (fortran-equality a b ulps type ignore?)
  (cond
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    (<= (abs (gfls-between a* b*)) ulps)]))
          
(define (fortran-format-args var val type)
  (format "~a = ~a" var val))

(define (fortran-format-output result)
  (format "~a" result))

(define fortran-tester
  (tester "fortran03"
    compile->fortran
    run<-fortran
    fortran-equality
    fortran-format-args
    fortran-format-output
    (const #t)
    fortran-supported
    #f))

; Command line
(module+ main 
  (define state
    (parameterize ([*tester* fortran-tester])
      (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.f03")))
  (exit state))
