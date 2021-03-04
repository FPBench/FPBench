#lang racket

(require generic-flonum)
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

(define (run<-sollya exec-name ctx type number)
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (fprintf port "~a~a\n\n" (sollya-header) (*prog*))
      (fprintf port "prec=4096!;\ndisplay=dyadic!;\n\n")
      (fprintf port "f(~a);\n"
               (string-join
                (for/list ([arg ctx])
                  (match-define (cons var value) arg)
                  (cond
                    [(real? value) (~a (exact->inexact value))]
                    [(gflnan? value) "nan"]
                    [(gflinfinite? value) (if (gfl>= value 0.gfl) "infty" "-infty")]
                    [else (~a (gfl->real value))]))
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
      [(regexp-match #rx"(?i:nan)" out) +nan.0]
      [(regexp-match #rx"[-](?i:infty)" out) -inf.0]
      [(regexp-match #rx"[+]?(?i:infty)" out) +inf.0]
      [else (dyadic->exact out)]))
  (when (regexp-match #rx"WARNING" res) ;; checks if division by zero was encountered
    (*ignore-by-run* (list-set (*ignore-by-run*) number #t)))
  (cons (->value out* type) (~a out*)))

;; In some cases, such as division by zero, sollya does not distinguish
;; between inf and nan. So for infinite results from the reference interpreter,
;; or for zero, as seen in the probabilities in a clustering algorithm benchmark,
;; allow a NaN answer from sollya.
(define (sollya-equality a b ulps type ignore?)
  (cond 
   [ignore? #t]    ;; If division by zero occurs, ignore the result and pass the test
   [(equal? a 'timeout) true]
   [else
    (define a* (->value a type))
    (define b* (->value b type))
    ; Sollya is not 1/2 ULP for binary80, add 3 to be safe?
    (define ulps* (if (equal? type 'binary80) (+ ulps 3) ulps))
    (<= (abs (gfls-between a* b*)) ulps*)]))

(define (sollya-format-args var val type)
  (format "~a = ~a\n\t~a = ~a" var val var (gfl->real val)))

(define (sollya-format-output result)
  (format "~a" result))

(define sollya-tester (tester "sollya" translate->sollya run<-sollya sollya-equality sollya-format-args
                              sollya-format-output (const #t) sollya-supported #f))

; Command line
(module+ main (parameterize ([*tester* sollya-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.sollya")])
    (exit state))))