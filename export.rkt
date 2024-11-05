#lang racket

; utility
(require "src/fpcore-reader.rkt"
         "src/compilers.rkt"
         "src/supported.rkt"
         "src/multi-command-line.rkt")

; compilers
(require "src/core2c.rkt"
         "src/core2cakeml.rkt"
         "src/core2cuda.rkt"
         "src/core2fortran03.rkt"
         "src/core2fptaylor.rkt"
         "src/core2gappa.rkt" 
         "src/core2go.rkt"
         "src/core2h.rkt"
         "src/core2haskell.rkt"
         "src/core2java.rkt"
         "src/core2js.rkt"
         "src/core2julia.rkt"
         "src/core2matlab.rkt"
         "src/core2ocaml.rkt"
         "src/core2python.rkt"
         "src/core2rust.rkt"
         "src/core2scala.rkt"
         "src/core2smtlib2.rkt"
         "src/core2sollya.rkt"
         "src/core2tex.rkt"
         "src/core2vivado.rkt"
         "src/core2wls.rkt")

(provide export-body export-main make-export-ctx)

; CLI args passed in through context
(struct export-ctx (lang runtime bare namespace rel-error scale suppress-warnings in-file out-file) #:transparent)

(define (make-export-ctx lang runtime bare namespace rel-error scale suppress-warnings in-file out-file)
  (export-ctx lang runtime bare namespace rel-error scale suppress-warnings in-file out-file))

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

  (define suppress-warnings #f)

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
   ["--suppress" "For Sollya, division by zero will not produce a warning"
    (set! suppress-warnings #t)]
   #:args (in-file out-file)

   (export-body (make-export-ctx 
        (*lang*) (*runtime*) (*bare*) (*namespace*) (*rel-error*) (*scale*) suppress-warnings in-file out-file) 
        stdin-port stdout-port)))

; #t source means from main entry, #f for tool specific
(define (export-body ctx stdin-port stdout-port)
   (define input-port
     (if (equal? (export-ctx-in-file ctx) "-")
         stdin-port
         (open-input-file (export-ctx-in-file ctx) #:mode 'text)))
   (define output-port
     (if (equal? (export-ctx-out-file ctx) "-")
         stdout-port
         (open-output-file (export-ctx-out-file ctx) #:mode 'text #:exists 'truncate)))

   (define extension (determine-lang (export-ctx-lang ctx) (export-ctx-out-file ctx)))
   
   (define-values (header export footer supported)
     (match extension
       [(or "gappa" "g") (values (const "") (curry core->gappa #:rel-error (export-ctx-rel-error ctx)) (const "") gappa-supported)]
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
           (raise-user-error "Unsupported output language" (export-ctx-lang ctx))))]))

   (when (and (equal? extension "js") (export-ctx-runtime ctx)) (js-runtime (export-ctx-runtime ctx)))
   (when (and (equal? extension "sollya") (export-ctx-suppress-warnings ctx)) (*sollya-warnings* #f))
   (when (and (set-member? '("fptaylor" "fpt") extension) (export-ctx-scale ctx)) (*fptaylor-inexact-scale* (export-ctx-scale ctx)))
   (when (equal? extension "scala") 
    (let ([out-name (if (equal? (export-ctx-out-file ctx) "-") 
                        "stdout" 
                        (string-trim (export-ctx-out-file ctx) ".scala"))])
      (*scala-prec-file* (open-output-file (string-append out-name ".prec.txt") #:mode 'text #:exists 'truncate))))                          

   (port-count-lines! input-port)
   (unless (export-ctx-bare ctx)
    (define namespace
      (match extension
       ["js" (js-runtime)]
       ["java" (java-namespace)]
       [_ (export-ctx-namespace ctx)]))
    (fprintf output-port (header namespace)))

   (for ([core (in-port (curry read-fpcore (if (equal? (export-ctx-in-file ctx) "-") "stdin" (export-ctx-in-file ctx))) input-port)] [n (in-naturals)])
     (let ([unsupported (unsupported-features core supported)])
      (unless (set-empty? unsupported)
        (raise-user-error (format "Sorry, the *.~a exporter does not support ~a" extension
            (string-join (map ~a unsupported) ", ")))))
     (fprintf output-port "~a\n" (export core (format "ex~a" n))))
   (unless (export-ctx-bare ctx) (fprintf output-port (footer))))

(module+ main
  (export-main (current-command-line-arguments) (current-input-port) (current-output-port)))
