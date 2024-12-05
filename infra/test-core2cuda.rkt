#lang racket
(require "test-common.rkt" "../src/core2cuda.rkt" "../src/evaluator.rkt")

(define (compile->cuda prog ctx type test-file)
 (call-with-output-file test-file #:exists 'replace
 (λ (p)
 (define N (if (list? (second prog)) (length (second prog)) (length (third prog))))
 ; Write CUDA-specific headers and includes
 (fprintf p "~a~a\n\n" (cuda-header) (core->cuda prog "f"))
 
 ; Write CUDA kernel launcher and main function
 (fprintf p "int main(int argc, char **argv) {
    // Allocate device memory
    ~a *d_result;
    cudaMalloc(&d_result, sizeof(~a));
    
    // Launch kernel
    dim3 grid(1);
    dim3 block(1);
    f<<<grid, block>>>(d_result" type type)
 
 ; Convert command line args based on type
 (define strtox (match type 
    ['binary64 "strtod(argv[~a], NULL)"]
    ['binary32 "strtof(argv[~a], NULL)"]
    ['integer "strtoll(argv[~a], NULL, 10)"]))
 
 ; Add parameters to kernel call
 (fprintf p ",~a" 
    (string-join (map (curry format strtox) (map add1 (range N))) ", "))
 
 ; Complete the main function with CUDA cleanup
 (fprintf p ");
    
    // Copy result back and print
    ~a h_result;
    cudaMemcpy(&h_result, d_result, sizeof(~a), cudaMemcpyDeviceToHost);
    printf(\"%.~a\", h_result);
    
    // Cleanup
    cudaFree(d_result);
    return 0;
}\n"
    type
    type
    (match type 
        ['binary64 "17g"]
        ['binary32 "17g"]
        ['integer "li"]))))
 
 ; Compile with nvcc instead of gcc
 (define cuda-file (string-replace test-file ".c" ".cu"))
 (system (format "nvcc ~a -o ~a" cuda-file test-file))
 test-file)

(define (run<-cuda exec-name ctx type number)
 (define out
 (with-output-to-string
 (λ ()
 (system (string-join (cons exec-name (map value->string (dict-values ctx))) " ")))))
 
 ; Handle CUDA-specific output formatting
 (define out*
 (match out
 ["nan" "+nan.0"]
 ["-nan" "+nan.0"]
 ["inf" "+inf.0"]
 ["-inf" "-inf.0"]
 [x x]))
 (cons (->value out* type) out*))

; Rest remains similar but adapted for CUDA specifics
(define cuda-tester 
    (tester "cuda" 
            compile->cuda 
            run<-cuda 
            c-equality  ; Can reuse C equality testing
            c-format-args 
            c-format-output 
            (const #t) 
            cuda-supported 
            #f))

(module+ main 
    (parameterize ([*tester* cuda-tester])
        (let ([state (test-core 
                (current-command-line-arguments)
                (current-input-port)
                "stdin" 
                "/tmp/test.cu")])
            (exit state))))