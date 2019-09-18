#lang racket

(require math/flonum)
(require "test-common.rkt"
         "../src/common.rkt"
         "../src/core2fptaylor.rkt"
         "../src/fpcore.rkt")

(define tests-to-run (make-parameter 10))
(define test-file (make-parameter "/tmp/test.txt"))
(define config-file (make-parameter "/tmp/config.cfg"))
(define fuel (make-parameter 100))

(define ulps (make-parameter 0))


(define (string->double string)
  (cond
    [(regexp-match #rx"[-]inf"    string) -inf.0]
    [(regexp-match #rx"inf"     string) +inf.0]
    [(regexp-match #rx"[-]?nan" string) +nan.0]
    [else (real->double-flonum (string->number string))]))


; compile->X
; This invokes the core2x compiler being tested to produce an executable test
; file. Typically, additional boilerplate is needed to call the function on
; arguments. compile->X takes a file location, usually in /tmp by default, and
; puts the result into that file. The tests are not threadsafe and can’t be run
; in parallel.
(define (compile->fptaylor prog test-file #:type type)
  (call-with-output-file test-file #:exists 'replace
    (lambda (port) (fprintf port "~a\n" (core->fptaylor prog "f"))))
  test-file)




; run<-X
; This executes the test in the specified file, captures its output, and
; translates it back into a value that can be compared to the result from the
; reference interpreter. Arguments to the core can be provided here, or baked in
; during the compilation step.
(define (run<-fptaylor exec-name #:type type)
  (begin
    (define out
      (with-output-to-string
        (lambda ()
          (parameterize ([current-error-port(current-output-port)])
            (system (format "fptaylor ~a" exec-name))))))
    (cond [(regexp-match #rx"ERROR" out)
           (begin
             ;(printf "~a\n" out) ;; Dump raw output
             (define conservative_bounds_line (regexp-match
                                               #rx"Bounds [(]without rounding[)]: [^\n]*"
                                               out))
             (define conservative_bounds (regexp-match*
                                          #rx"([+-]?[0-9]+[.][0-9]+[eE][+-]?[0-9]+)|([-]?inf)|([-]?nan)"
                                          (car conservative_bounds_line)))
             (define conservative_lower (string->double (car conservative_bounds)))
             (define conservative_upper (string->double (cadr conservative_bounds)))
             (cons conservative_lower conservative_upper))]
          [else
           (begin
             (define bounds_line (regexp-match
                                  #rx"Bounds [(]floating-point[)]: [^\n]*"
                                  out))
             (define bounds (regexp-match*
                             #rx"[+-]?[0-9]+[.][0-9]+[eE][+-]?[0-9]+"
                             (car bounds_line)))
             (define lower (string->double (car bounds)))
             (define upper (string->double (cadr bounds)))
             (cons lower upper))])))




; =*
; Equality is hard for computer number systems; we may need to define a custom
; way to compare numbers to determine if the test has passed.
(define (=* a bound)
  (if (nan? a)
      (and (nan? (car bound)) (nan? (cdr bound)))
      (<= (car bound) a (cdr bound))))




(define (run-tests p file)
  (define err 0)
  (port-count-lines! p)
  (for ([prog (in-port (curry read-fpcore file) p)])
    (match-define (list 'FPCore (list vars ...) props* ... body) prog)
    (define-values (_ props) (parse-properties props*))
    (define type (dict-ref props ':precision 'binary64))
    (define timeout 0)
    (define results
      (for/list ([i (in-range (tests-to-run))])
        (define ctx (for/list ([var vars])
                      (cons var (match type
                                  ['binary64 (sample-double)]
                                  ['binary32 (sample-single)]))))
        (define exec-file (compile->fptaylor prog (test-file) #:type type))
        (define evaltor (match type ['binary64 racket-double-evaluator] ['binary32 racket-single-evaluator]))
        (define out
          (match ((eval-fuel-expr evaltor (fuel) 'timeout) body ctx)
            [(? real? result)
             ((match type
                ['binary64 real->double-flonum] ['binary32 real->single-flonum])
              result)]
            [(? complex? result)
             (match type
               ['binary64 +nan.0] ['binary32 +nan.f])]
            ['timeout 'timeout]
            [(? boolean? result) result]))
        (when (equal? out 'timeout)
          (set! timeout (+ timeout 1)))
        (define out* (if (equal? out 'timeout) 'timeout (run<-fptaylor exec-file #:type type)))
        (list ctx out out*)))
    (unless (null? results)
      (printf "~a/~a: ~a~a\n" (count (lambda (x) (=* (second x) (third x))) results) (length results)
              (dict-ref props ':name body) (match timeout
                                             [0 ""]
                                             [1 " (1 timeout)"]
                                             [_ (format " (~a timeouts)" timeout)]))
      (set! err (+ err (count (lambda (x) (not (=* (second x) (third x)))) results)))
      (for ([x (in-list results)] #:unless (=* (second x) (third x)))
        (printf "\t~a ∉ [~a, ~a]\n" (second x) (car (third x)) (cdr (third x))))))
  err)



; main
; The main function of the test driver needs to do a bunch of things:
;  - Wait for a bunch of cores on stdin
;  - Send them to the compile and run functions, rerunning a specified number of
;    times
;  - Run the reference interpreter
;  - Check the answers, and report errors
;  - Return the number of errors, and possibly a nonzero return code to explode
;    the make process
; The following features / flags need to be supported:
;  - fuel: how many steps to run the reference interpreter, before timing out.
;          The reference interpreter is incredibly slow
;  - repeat: how many times to rerun the test with different inputs
;  - error: how many ulps of error to tolerate before failing. This needs to be
;    small (but nonzero!) for C, 3 ulps usually works, and larger for js. The
;    problem is that the system C libraries are not one ulp correct for all math
;    functions, while the reference interpreter uses MPFR and is supposed to be.
;  - Output file name
(module+ main
  (command-line
   #:program "test/compiler.rkt"
   #:once-each
   ["--fuel" fuel_ "Number of computation steps to allow"
             (fuel (string->number fuel_))]
   ["--repeat" repeat_ "Number of times to test each program"
               (tests-to-run (string->number repeat_))]
   ["--error" ulps_ "Error, in ULPs, allowed for libc inaccuracies (probably use a value around 3)"
              (ulps (string->number ulps_))]
   ["-o" name_ "Name for generated fptaylor file"
         (test-file name_)]
   #:args ()

   (let ([error (run-tests (current-input-port) "stdin")])
     (exit error))))
