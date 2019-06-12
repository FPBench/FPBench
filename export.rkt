#lang racket

(require "src/core2c.rkt" "src/core2fptaylor.rkt" "src/core2gappa.rkt"
         "src/core2go.rkt" "src/core2js.rkt" "src/core2scala.rkt"
         "src/core2smtlib2.rkt" "src/core2sollya.rkt" "src/core2wls.rkt")

(define (determine-lang preset file-name)
  (string-downcase
   (or preset
       (match (string-split file-name "." #:trim? #f)
         [(list) ""]
         [(list _) ""]
         [(list _ ... extension) extension]))))

(module+ main
  (define *lang* (make-parameter #f))

  ;; only used by js, but could be used for other converters
  (define *runtime* (make-parameter #f))

  ;; used for pkg in core2go
  (define *namespace* (make-parameter "main"))

  (define *rel-error* (make-parameter #f))
  (define *scale* (make-parameter 1))

  (command-line
   #:program "export.rkt"
   #:once-each
   ["--lang" lang_ "Output language to compile FPCore to"
    (*lang* lang_)]
   ["--runtime" runtime_ "Name of library to invoke mathematical operations on"
    (*runtime* runtime_)]
   ["--namespace" namespace_ "Name of namespace or package to export benchmarks into"
    (*namespace* namespace_)]
   ["--rel-error" "For Gappa export, produce expressions for relative instead of absolute error"
    (*rel-error* #t)]
   ["--scale" scale_ "For FPTaylor export, the scale factor for operations which are not correctly rounded"
    (*scale* (string->number scale_))]
   #:args (in-file out-file)

   (define input-port
     (if (equal? in-file "-")
         (current-input-port)
         (open-input-file in-file #:mode 'text)))
   (define output-port
     (if (equal? out-file "-")
         (current-output-port)
         (open-output-file out-file #:mode 'text #:exists 'truncate)))

   (define-values (header export footer)
     (match (determine-lang (*lang*) out-file)
       ["c" (values c-header export-c "")]
       ["fptaylor" (values "" (curry export-fptaylor #:scale (*scale*)) "")]
       [(or "gappa" "g") (values "" (curry export-gappa #:rel-error rel-error))]
       ["go" (values (format go-header (*namespace*)) export-go "")]
       ["js" (values "" (curry export-js #:runtime (*runtime*)) "")]
       ["scala" (values (format scala-header (*namespace*)) export-scala scala-footer)]
       [(or "smt" "smt2" "smtlib" "smtlib2") (values "" export-smtlib2 "")]
       ["sollya" (values "" export-sollya "")]
       ["wls" (values "" export-wls "")]
       [_ (raise-user-error "Unsupported output language" (*lang*))]))

   (port-count-lines! input-port)
   (fprintf output-port header)
   (for ([expr (in-port (curry read-fpcore (if (equal? in-file "-") "stdin" in-file)) input-port)] [n (in-naturals)])
     (fprintf output-port "~a\n" (export expr (format "ex~a" n))))
   (fprintf output-port footer)))
