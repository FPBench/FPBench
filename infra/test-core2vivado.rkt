#lang racket

(require generic-flonum)
(require "test-common.rkt" "../src/core2vivado.rkt")

(define (compile->vivado prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
      (fprintf p "#include <cstdlib>\n#include <iostream>\n~a~a\n\n" (vivado-header) (core->vivado prog "f"))
      (fprintf p "int main(int argc, char *argv[]) {\n")
      (define strtox (match type ['binary32 "std::strtof(argv[~a], nullptr)"] [_ "std::strtod(argv[~a], nullptr)"]))
      (fprintf p "\tstd::cout << std::setprecision(17) << f(~a);\n}\n"
               (string-join (map (curry format strtox) (map add1 (range N))) ", "))))
  (define tcl-file (string-replace test-file ".cpp" ".tcl"))
  (system (format "printf 'open_project -reset project\nadd_files -tb -csimflags \"-std=c++11\" \"~a\"\nopen_solution -reset solution1\ncsim_design -setup\nexit\n' > ~a" test-file tcl-file))
  (system (format "vivado_hls -f ~a > /dev/null" tcl-file))
  "./project/solution1/csim/build/csim.exe")

(define (run<-vivado exec-name ctx type number)
  (define out
    (with-output-to-string
      (λ ()
        (system (string-join (cons exec-name (map value->string (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (vivado-equality a b ulps type ignore?)
  (cond
    [(equal? a 'timeout) true]
    [else
      (define a* (->value a type))
      (define b* (->value b type))
      (<= (abs (gfls-between a* b*)) ulps)]))

(define (vivado-format-args var val type)
  (format "~a = ~a" var val))

(define (vivado-format-output result)
  (format "~a" result))

(define vivado-tester
  (tester "vivado" compile->vivado run<-vivado vivado-equality vivado-format-args vivado-format-output (const #t) vivado-supported #f))

; Command line
(module+ main (parameterize ([*tester* vivado-tester])
  (let ([state (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.cpp")])
    (exit state))))
