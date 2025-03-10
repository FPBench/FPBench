#lang racket
(require generic-flonum)
(require "test-common.rkt" "../src/core2cuda.rkt" "../src/evaluator.rkt")

(define (compile->cuda prog ctx type test-file)
  (call-with-output-file test-file #:exists 'replace
    (λ (p)
      (define N (if (list? (second prog)) 
                   (length (second prog)) 
                   (length (third prog))))
      
      ; Define strtox BEFORE we try to use it
      (define strtox 
        (match type
          ['binary64 "strtod(argv[~a], NULL)"]
          ['binary32 "strtof(argv[~a], NULL)"]
          ['integer "strtoll(argv[~a], NULL, 10)"]))
      
      ; Keep function name consistent - use "f" everywhere
      (fprintf p "#include <stdio.h>\n~a~a\n\n"
               (cuda-header) 
               (core->cuda prog "f"))  ; Changed "foo" to "f"
      
      ; Add the global kernel function that calls the device function
      (fprintf p "__global__ void f(~a *result~a) {\n"
               (type->cuda type)
               (if (> N 0)
                   (format ", ~a" (string-join 
                                  (for/list ([i (range N)])
                                    (format "~a arg~a" (type->cuda type) i))
                                  ", "))
                   ""))
      
      ; Call device function and store result
      (fprintf p "    *result = f(~a);\n}\n\n"
               (if (> N 0)
                   (string-join 
                    (for/list ([i (range N)])
                      (format "arg~a" i))
                    ", ")
                   ""))
      
      ; Write main function
      (fprintf p "int main(int argc, char **argv) {
    // Allocate device memory
    ~a *d_result;
    cudaMalloc(&d_result, sizeof(~a));
    
    // Launch kernel
    dim3 grid(1);
    dim3 block(1);
    f<<<grid, block>>>(d_result"
               (type->cuda type)
               (type->cuda type))
      
      ; Add parameters if there are any
      (when (> N 0)
        (fprintf p ",~a"
                 (string-join 
                  (map (curry format strtox) (map add1 (range N)))
                  ", ")))
      
      (fprintf p ");
    
    // Copy result back and print
    ~a h_result;
    cudaMemcpy(&h_result, d_result, sizeof(~a), cudaMemcpyDeviceToHost);
    printf(\"%.~a\", h_result);
    
    // Cleanup
    cudaFree(d_result);
    return 0;
}\n"
               (type->cuda type)
               (type->cuda type)
               (match type
                 ['binary64 "17g"]
                 ['binary32 "17g"]
                 ['integer "li"]))))
  
  
  ; Compile
  (define cuda-file (string-replace test-file ".cu" ".bin"))
  (system (format "nvcc --expt-relaxed-constexpr -fmad=false ~a -o ~a" test-file cuda-file))
  cuda-file)

 
  ; nvcc --expt-relaxed-constexpr /tmp/test.cu -o /tmp/test

(define (cuda-equality a b ulps type ignore?)
  (cond
    [(equal? a 'timeout) true]
    [else
     (define a* (->value a type))
     (define b* (->value b type))
     (<= (abs (gfls-between a* b*)) ulps)]))

(define (cuda-format-args var val type)
  (format "~a = ~a" var val))

(define (cuda-format-output result)
  (format "~a" result))

(define (run<-cuda exec-name ctx type number)
  (define out
    (with-output-to-string
     (λ ()
       (system (string-join (cons exec-name 
                                (map value->string (dict-values ctx))) 
                          " ")))))
  ; Handle CUDA-specific output formatting
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define cuda-tester 
  (tester "cuda" 
          compile->cuda 
          run<-cuda 
          cuda-equality 
          cuda-format-args 
          cuda-format-output 
          (const #t) 
          cuda-supported 
          #f))

(module+ main 
  (parameterize ([*tester* cuda-tester])
    (let ([state (test-core (current-command-line-arguments) 
                           (current-input-port) 
                           "stdin" 
                           "/tmp/test.cu")])
      (exit state))))