#lang racket

; utility
(require "src/fpcore-reader.rkt"
         "src/compilers.rkt"
         "src/supported.rkt")

; compilers
(require "src/core2c.rkt"
         "src/core2cakeml.rkt"
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

(provide export-main make-export-ctx)

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

(define (export-main ctx stdin-port stdout-port)
   (define input-port
     (if (equal? in-file "-")
         stdin-port
         (open-input-file in-file #:mode 'text)))
   (define output-port
     (if (equal? out-file "-")
         stdout-port
         (open-output-file out-file #:mode 'text #:exists 'truncate)))

   (define extension (determine-lang (export-ctx-lang ctx) out-file))
   
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
   (when (and (equal? extension "sollya") suppress-warnings) (*sollya-warnings* #f))
   (when (and (set-member? '("fptaylor" "fpt") extension) (export-ctx-scale ctx)) (*fptaylor-inexact-scale* (export-ctx-scale ctx)))
   (when (equal? extension "scala") 
    (let ([out-name (if (equal? out-file "-") 
                        "stdout" 
                        (string-trim out-file ".scala"))])
      (*scala-prec-file* (open-output-file (string-append out-name ".prec.txt") #:mode 'text #:exists 'truncate))))                          

   (port-count-lines! input-port)
   (unless (export-ctx-bare ctx)
    (define namespace
      (match extension
       ["js" (js-runtime)]
       ["java" (java-namespace)]
       [_ (export-ctx-namespace ctx)]))
    (fprintf output-port (header namespace)))

   (for ([core (in-port (curry read-fpcore (if (equal? in-file "-") "stdin" in-file)) input-port)] [n (in-naturals)])
     (let ([unsupported (unsupported-features core supported)])
      (unless (set-empty? unsupported)
        (raise-user-error (format "Sorry, the *.~a exporter does not support ~a" extension
            (string-join (map ~a unsupported) ", ")))))
     (fprintf output-port "~a\n" (export core (format "ex~a" n))))
   (unless (export-ctx-bare ctx) (fprintf output-port (footer))))

(module+ main
  (export-main (current-command-line-arguments) (current-input-port) (current-output-port)))
