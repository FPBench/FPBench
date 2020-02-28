#lang racket

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

(define (run<-sollya exec-name ctx type)
  (call-with-output-file exec-name #:exists 'replace
    (lambda (port)
      (fprintf port "~a~a\n\n" (sollya-header) (*prog*))
      (fprintf port "prec=4096!;\ndisplay=dyadic!;\n\n")
      (fprintf port "f(~a);\n"
               (string-join
                (for/list ([arg ctx])
                  (match-define (cons var value) arg)
                  (cond
                    [(nan? value) "nan"]
                    [(infinite? value) (if (>= value 0) "infty" "-infty")]
                    [else (format "~a" (inexact->exact value))]))
                ", "))))
  (define out 
    (last
      (string-split
      (string-trim
        (with-output-to-string
          (lambda ()
            (system (format "sollya ~a" exec-name)))))
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
         ['binary64 +nan.0])]
      [(regexp-match #rx"[-](?i:infty)" out)
       (match type
         ['binary32 -inf.f]
         ['binary64 -inf.0])]
      [(regexp-match #rx"[+]?(?i:infty)" out)
       (match type
         ['binary32 +inf.f]
         ['binary64 +inf.0])]
      [else
       (dyadic->exact out)]))
  (cons 
    (match type
      ['binary32 (real->single-flonum out*)]
      ['binary64 (real->double-flonum out*)])
    (format "~a" out*)))
    

;; In some cases, such as division by zero, sollya does not distinguish
;; between inf and nan. So for infinite results from the reference interpreter,
;; or for zero, as seen in the probabilities in a clustering algorithm benchmark,
;; allow a NaN answer from sollya.
(define (sollya-equality a b ulps)
  (or (equal? a b) (= a b) (and (or (= a 0) (infinite? a) (nan? a)) (or (nan? b)))))

(define (sollya-format-args var val type)
  (format "~a = ~a\n\t~a = ~a" var val var (if (or (nan? val) (infinite? val)) 
                                               val 
                                               (inexact->exact val))))

(define (sollya-format-output result)
  (format "~a" result))

(define sollya-tester (tester "sollya" translate->sollya run<-sollya sollya-equality sollya-format-args sollya-format-output sollya-supported))

; Command line
(module+ main (parameterize ([*tester* sollya-tester])
  (test-imperative (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.sollya")))