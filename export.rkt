#lang racket

(require "src/core2c.rkt"
         "src/core2fptaylor.rkt"
         "src/core2gappa.rkt"
         "src/core2go.rkt"
         "src/core2json.rkt"
         "src/core2js.rkt"
         "src/core2scala.rkt"
         "src/core2smtlib2.rkt"
         "src/core2sollya.rkt"
         "src/core2wls.rkt")


(define (determine-lang preset file-name)
  (string-downcase
   (or preset
       (match (string-split file-name "." #:trim #f)
         [(list) ""]
         [(list _) ""]
         [(list _ ... extension) extension]))))

(module+ main
  (require racket/cmdline)

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

   (define fname (if (equal? in-file "-") "stdin" in-file))
   (define input-port (if (equal? in-file "-")
                          (current-input-port)
                          (open-input-file in-file #:mode 'text)))
   (define output-port (if (equal? out-file "-")
                           (current-output-port)
                           (open-output-file out-file #:mode 'text #:exists 'truncate)))
   (port-count-lines! input-port)


   (match (determine-lang (*lang*) out-file)
     ["c"
      (export-c input-port output-port #:fname fname)]
     ["fptaylor"
      (export-fptaylor input-port output-port #:fname fname #:scale (*scale*))]
     [(or "gappa" "g")
      (export-gappa input-port output-port #:fname fname #:rel-error (*rel-error*))]
     ["go"
      (export-go input-port output-port #:fname fname #:pkg (*namespace*))]
     ["js"
      (export-js input-port output-port #:fname fname #:runtime (*runtime*))]
     ["scala"
      (export-scala input-port output-port #:fname fname)]
     [(or "smt" "smt2" "smtlib" "smtlib2")
      (export-smtlib2 input-port output-port #:fname fname)]
     ["sollya"
      (export-sollya input-port output-port #:fname fname)]
     ["wls"
      (export-wls input-port output-port #:fname fname)]
     [_ (raise-user-error "Unsupported output language" (*lang*))])))
