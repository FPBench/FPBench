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

(module+ main
  (require racket/cmdline)

  (define *lang* (make-parameter #f))

  ;; only used by js, but could be used for other converters
  (define *runtime* (make-parameter "Math"))

  ;; used for pkg in core2go
  (define *namespace* (make-parameter "main"))

  (define *rel-error* (make-parameter #f))
  (define *scale* (make-parameter 1))

  (define (determine-lang out-file)
    (if (false? (*lang*))
        (let* ([fname (string-downcase out-file)]
               [suffix-match (regexp-match #rx"[.]([^.]*)$" fname)])
          (when suffix-match
            (*lang* (last suffix-match))))
        (*lang* (string-downcase (*lang*)))))

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

   (determine-lang out-file)

   (define fname (if (equal? in-file "-") "stdin" in-file))
   (define input-port (if (equal? in-file "-")
                          (current-input-port)
                          (open-input-file in-file #:mode 'text)))
   (define output-port (if (equal? out-file "-")
                           (current-output-port)
                           (open-output-file out-file #:mode 'text #:exists 'truncate)))


   (fprintf (current-error-port) "in-file: ~a, out-file: ~a\noptions:\n  lang: ~a\n  runtime: ~a\n  namespace:~a\n  rel-error: ~a\n  scale: ~a\n"
            in-file out-file (*lang*) (*runtime*) (*namespace*) (*rel-error*) (*scale*))

   (match (*lang*)
     ["c" (export-c input-port output-port
                    #:fname fname)]
     ["fptaylor" (export-fptaylor input-port output-port
                                  #:fname fname
                                  #:scale (*scale*))]
     [(or "gappa" "g") (export-gappa input-port output-port
                                  #:fname fname
                                  #:rel-error (*rel-error*))]
     ["go" (export-go input-port output-port
                      #:fname fname
                      #:pkg (*namespace*))]
     ["js" (export-js input-port output-port
                      #:fname fname
                      #:runtime (*runtime*))]
     ["scala" (export-scala input-port output-port
                            #:fname fname)]
     [(or "smt" "smt2" "smtlib" "smtlib2") (export-smtlib2 input-port output-port
                                                           #:fname fname)]
     ["sollya" (export-sollya input-port output-port
                              #:fname fname)]
     ["wls" (export-wls input-port output-port
                        #:fname fname)]
     [_ (error 'export.rkt "Unsupported output language ~a" (*lang*))])

   (fprintf (current-error-port) "Done.\n")

   ))
