#lang racket

(require math/bigfloat racket/extflonum)
(require "test-common.rkt" "../src/core2sollya.rkt")

(define (translate->sollya prog ctx type test-file)
  (*prog* (core->sollya prog "f"))
  test-file)

(define (dyadic->exact s)
  (let* ([matches (regexp-match #rx"([+-]?[0-9]+)((?i:b)([+-]?[0-9]+))?" s)]
         [m (string->number (second matches))]
         [e (if (fourth matches)
                (string->number (fourth matches))
                0)])
    (if (>= e 0)
        (* m (expt 2 e))
        (/ m (expt 2 (- e))))))

(define (extfl->real x)
  (cond
    [(equal? x +inf.t)  +inf.0] 
    [(equal? x -inf.t)  -inf.0]
    [(equal? x +nan.t)  +nan.0] 
    [(equal? x -nan.t)  -nan.0]
    [else (extfl->exact x)]))  

(define (run<-sollya exec-name ctx type number)
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (fprintf port "~a~a\n\n" (sollya-header) (*prog*))
      (fprintf port "prec=4096!;\ndisplay=dyadic!;\n\n")
      (fprintf port "f(~a);\n"
               (string-join
                (for/list ([arg ctx])
                  (match-define (cons var value) arg)
                  (if (extflonum? value)
                      (cond
                        [(equal? value +nan.t) "nan"]
                        [(or (equal? value +inf.t) (equal? value -inf.t)) 
                          (if (extfl>= value 0.0t0) "infty" "-infty")]
                        [else (~a (extfl->exact value))])
                      (cond
                        [(nan? value) "nan"]
                        [(infinite? value) (if (>= value 0) "infty" "-infty")]
                        [else (~a (inexact->exact value))])))
                ", "))))
  (define res
    (with-output-to-string
      (lambda ()
        (system (format "sollya ~a" exec-name)))))
  (define out
    (last
      (string-split
        (string-trim res)
        "\n")))
  (define out*
    (cond
      [(regexp-match #rx"(?i:error)" out)
       (begin
         (printf "~a\n" out)
         (error 'run<-sollya "Sollya evaluation failed."))]
      [(regexp-match #rx"(?i:nan)" out)
       (match type
         ['binary32 +nan.f]
         ['binary64 +nan.0]
         ['binary80 +nan.0])]
      [(regexp-match #rx"[-](?i:infty)" out)
       (match type
         ['binary32 -inf.f]
         ['binary64 -inf.0]
         ['binary80 -inf.0])]
      [(regexp-match #rx"[+]?(?i:infty)" out)
       (match type
         ['binary32 +inf.f]
         ['binary64 +inf.0]
         ['binary80 +inf.0])]
      [else
       (dyadic->exact out)]))
  (when (regexp-match #rx"WARNING" res) ;; checks if division by zero was encountered
    (*ignore-by-run* (list-set (*ignore-by-run*) number #t)))
  (cons 
    (match type
      ['binary32 (real->single-flonum out*)]
      ['binary64 (real->double-flonum out*)]
      ['binary80 (parameterize ([bf-precision 64]) (real->extfl (bigfloat->real (bf out*))))])
    (format "~a" out*)))

;; In some cases, such as division by zero, sollya does not distinguish
;; between inf and nan. So for infinite results from the reference interpreter,
;; or for zero, as seen in the probabilities in a clustering algorithm benchmark,
;; allow a NaN answer from sollya.
(define (sollya-equality a b ulps ignore?)
  (cond 
   [ignore? #t]    ;; If division by zero occurs, ignore the result and pass the test  
   [(extflonum? b)
      (let ([a* (real->extfl a)])
        (or (equal? a* b) 
            (extfl= a* b) 
            (and (or (extfl= a* 0.0t0) (or (equal? a* +inf.t) (equal? a* -inf.t)) (equal? a* +nan.t)) (equal? b +nan.t))))]
   [else (or (equal? a b)
             (= a b) 
             (and (or (= a 0) (infinite? a) (nan? a)) (nan? b)))]))

(define (sollya-format-args var val type)
  (format "~a = ~a\n\t~a = ~a" var val var 
          (if (extflonum? val) 
              (extfl->real val)
              (if (or (nan? val) (infinite? val))
                  val (inexact->exact val)))))

(define (sollya-format-output result)
  (format "~a" result))

(define sollya-tester (tester "sollya" translate->sollya run<-sollya sollya-equality sollya-format-args
                              sollya-format-output (const #t) sollya-supported #f))

; Command line
(module+ main (parameterize ([*tester* sollya-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.sollya")])
    (exit state))))