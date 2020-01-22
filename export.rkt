#lang racket

(require "src/fpcore.rkt" "src/compilers.rkt" "src/supported.rkt" "src/imperative.rkt")
(require "src/core2c.rkt" "src/core2fptaylor.rkt" "src/core2gappa.rkt"
         "src/core2go.rkt" "src/core2js.rkt" "src/core2scala.rkt"
         "src/core2smtlib2.rkt" "src/core2sollya.rkt" "src/core2wls.rkt")

(provide export-main)

(define (file-extension file-name)
  (define ext (path-get-extension file-name))
  (and ext (last (string-split (bytes->string/locale ext) "."))))

(define (determine-lang preset file-name)
  (define lang (or preset (file-extension file-name)))
  (and lang (string-downcase lang)))

(define (export-main argv stdin-port stdout-port)
  (define *lang* (make-parameter #f))

  (define *runtime* (make-parameter #f))
  (define *bare* (make-parameter #f))
  (define *namespace* (make-parameter "main"))

  (define *rel-error* (make-parameter #f))
  (define *scale* (make-parameter 1))

  (command-line
   #:program "export.rkt"
   #:argv argv
   #:once-each
   ["--lang" lang_ "Output language to compile FPCore to"
    (*lang* lang_)]
   ["--bare" "Skip the file header and footer"
    (*bare* #t)]
   ["--namespace" namespace_ "Name of namespace or package to export benchmarks into"
    (*namespace* namespace_)]
   ["--runtime" runtime_ "Name of library to invoke mathematical operations on"
    (*runtime* runtime_)]
   ["--rel-error" "For Gappa export, produce expressions for relative instead of absolute error"
    (*rel-error* #t)]
   ["--scale" scale_ "For FPTaylor export, the scale factor for operations which are not correctly rounded"
    (*scale* (string->number scale_))]
   #:args (in-file out-file)

   (define input-port
     (if (equal? in-file "-")
         stdin-port
         (open-input-file in-file #:mode 'text)))
   (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))

   (define extension (determine-lang (*lang*) out-file))

   (define-values (header export footer supported)
     (match extension
       ["fptaylor" (values "" (curry core->fptaylor #:inexact-scale (*scale*)) "" '())]
       [(or "gappa" "g") (values "" (curry core->gappa #:rel-error (*rel-error*)) "" '())]
       ["scala" (values (format scala-header (*namespace*)) core->scala scala-footer '())]
       [(or "smt" "smt2" "smtlib" "smtlib2") (values "" core->smtlib2 "" '())]
       ["sollya" (values "" core->sollya "" '())]
       ["wls" (values "" core->wls "" '())]
       [#f (raise-user-error "Please specify an output language (using the --lang flag)")]
       [_
        (apply values
          (or
           (for/first ([compiler (compilers)]
                       #:when (set-member? (compiler-extensions compiler) extension))
             (list (compiler-header compiler)
                   (compiler-export compiler)
                   (compiler-footer compiler)
                   (compiler-supported compiler)))
           (raise-user-error "Unsupported output language" (*lang*))))]))

   (if (and (equal? extension "js") (*runtime*)) (js-runtime (*runtime*)) (void))
   (port-count-lines! input-port)
   (unless (*bare*) (fprintf output-port (header (*namespace*))))
   (for ([core (in-port (curry read-fpcore (if (equal? in-file "-") "stdin" in-file)) input-port)] [n (in-naturals)])
     (unless (valid-core core supported)
       (raise-user-error (format "Sorry, the *.~a exporter does not support ~a" extension
                                 (string-join (map ~a (set-intersect 
                                      (operators-in core) 
                                      (supported-ops->unsupported(supported-list-ops supported))))
                                       ", "))))
     (fprintf output-port "~a\n" (export core (format "ex~a" n))))
   (unless (*bare*) (fprintf output-port (footer)))))

(module+ main
  (export-main (current-command-line-arguments) (current-input-port) (current-output-port)))
