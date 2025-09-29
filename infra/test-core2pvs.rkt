#lang racket

(require generic-flonum)
(require "../src/core2pvs.rkt"
         "test-common.rkt")

(define (generate-wrapper N args test-file [type 'binary64])
  (call-with-output-file test-file
                         #:exists 'replace
                         (lambda (p)
                           (define args* (string-join (map (curry format "double ~a") args) ", "))
                           (define strtox
                             (match type
                               ['binary80 "strtold(argv[~a], NULL)"]
                               ['binary64 "strtod(argv[~a], NULL)"]
                               ['binary32 "strtof(argv[~a], NULL)"]
                               ['integer "strtoll(argv[~a], NULL, 10)"]))

                           (fprintf p "#include <stdio.h>\ndouble example_num(~a);\n\n" args*)
                           (fprintf p "int main(int argc, char **argv) {\n")
                           (fprintf p
                                    "printf(\"%.~a\", f_num(~a)); return 0; }\n"
                                    (match type
                                      ['binary80 "20Lg"]
                                      ['binary64 "17g"]
                                      ['binary32 "17g"]
                                      ['integer "li"])
                                    (string-join (map (curry format strtox) (map add1 (range N)))
                                                 ", ")))))

(define (generate-precisa-prelude [filename "precisa_prelude.c"])
  (call-with-output-file
   "/tmp/precisa_prelude.c"
   #:exists 'replace
   (λ (p)
     (fprintf
      p
      "#include<stdbool.h>

#define round(X) (double) (X)

/*@

axiomatic fp_funs {

  logic double Dadd(double X, double Y) = round(X+Y);

  logic double Dsub(double X, double Y) = round(X-Y);

  logic double Dmul(double X, double Y) = round(X*Y);

  logic double Dneg(double X) = round(0-X);

  logic double Dabs(double X) = round(X); //TODO define correctly

  logic double Ddiv(double X, double Y) =
  X;
//  (Y != 0.0 ? round(X/Y) : 0.0) ;

// }

// axiomatic int_funs {

  logic integer Iadd(integer X, integer Y) = X+Y;

  logic integer Isub(integer X, integer Y) = X-Y;

  logic integer Imul(integer X, integer Y) = X*Y;

  logic integer Ineg(integer X) = 0-X;

  // logic integer Idiv(integer X, integer Y) = (Y != 0 ? X/Y : 0) ;

// }

// axiomatic error_bounds {

  logic real ulp_dp(real X) = \\round_double(\\Up,X)-\\round_double(\\Down,X);

  logic real errAdd_dp(real X, real E_X, real Y, real E_Y)
  = E_X + E_Y + ulp_dp(\\abs(X + Y) + E_X + E_Y)/2;

  logic real errSub_dp(real X, real E_X, real Y, real E_Y)
  = E_X + E_Y + ulp_dp(\\abs(X - Y) + E_X + E_Y)/2;

  logic real errMul_dp(real X, real E_X, real Y, real E_Y)
  = \\abs(X)*E_Y+\\abs(Y)*E_X+E_X*E_Y + ulp_dp(\\abs(X)*\\abs(Y) + \\abs(X)*E_Y + E_X*\abs(Y) + E_X*E_Y)/2;

  logic real errDiv_dp(real X, real E_X, real Y, real E_Y)
  = ( ((Y*Y - E_Y*\abs(Y)) != 0 && (\abs(Y) - E_Y) !=0)?
      (\abs(Y)*E_X + \abs(X)*E_Y) / (Y*Y - E_Y*\abs(Y)) + ulp_dp((\abs(X) + E_X) / (\abs(Y) - E_Y)) / 2
      : 0 );

  logic real errNeg_dp(real X, real E_X)
  = E_X;

  logic real errAdd_i(integer X, real E_X, integer Y, real E_Y)
  = E_X + E_Y;

  logic real errSub_i(integer X, real E_X, integer Y, real E_Y)
  = E_X + E_Y;

}

*/

struct maybeInt {
bool isValid;
int value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeInt none () {
struct maybeInt result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeInt some (int val) {
struct maybeInt result = { true, val };
return result;
}

struct maybeFloat {
bool isValid;
float value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeFloat noneFloat () {
struct maybeFloat result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeFloat someFloat (float val) {
struct maybeFloat result = { true, val };
return result;
}

struct maybeDouble {
bool isValid;
double value;
};

/*@ assigns \nothing;
ensures ! \result.isValid;
*/
struct maybeDouble noneDouble () {
struct maybeDouble result = { false, 0 };
return result;
}

/*@ assigns \nothing;
ensures \result.isValid;
ensures \result.value == val;
*/
struct maybeDouble someDouble (double val) {
struct maybeDouble result = { true, val };
return result;
}

struct maybeBool {
    bool isValid;
    bool value;
};

/*@ assigns \nothing;
 ensures ! \result.isValid;
 */
struct maybeBool noneBool () {
    struct maybeBool result = { false, false };
    return result;
}

/*@ assigns \nothing;
 ensures \result.isValid;
 ensures \result.value == val;
 */
struct maybeBool someBool (bool val) {
    struct maybeBool result = { true, val };
    return result;
}"))))

(define (compile->pvs prog ctx type test-file)
  (define args (second prog))
  (generate-precisa-prelude)
  (define N
    (if (list? (second prog))
        (length (second prog))
        (length (third prog))))

  ;; Rewrite core to PVS
  (define-values (pvs-ranges pvs-prog) (core->pvs prog "example"))
  (call-with-output-file "/tmp/example.pvs" #:exists 'replace (lambda (p) (fprintf p pvs-prog)))
  (call-with-output-file "/tmp/example.input" #:exists 'replace (lambda (p) (fprintf p pvs-ranges)))

  ;; Compile PVS to C using Reflow - it will produce example.c
  (system "reflow /tmp/example.pvs /tmp/example.input --format=double")

  ;; Generate a wrapper for example.c to call directly double example_num
  (generate-wrapper N args test-file 'binary64)

  ;; Compile original example.c with renaming "main" to "example_main" (to avoid duplicative main functions)
  (system "cc -c /tmp/example.c -o /tmp/example.o -Dmain=example_main")

  ;; Compile final c-file that is to be called
  (define c-file (string-replace test-file ".c" ".bin"))
  (system (format "cc ~a /tmp/example.o -lm -frounding-math -o ~a" test-file c-file))
  c-file)

(define (run<-c exec-name ctx type number)
  (define out
    (with-output-to-string
     (λ () (system (string-join (cons exec-name (map value->string (dict-values ctx))) " ")))))
  (define out*
    (match out
      ["nan" "+nan.0"]
      ["-nan" "+nan.0"]
      ["inf" "+inf.0"]
      ["-inf" "-inf.0"]
      [x x]))
  (cons (->value out* type) out*))

(define (c-equality a b ulps type ignore?)
  (cond
    [(equal? a 'timeout) true]
    [else
     (define a* (->value a type))
     (define b* (->value b type))
     (<= (abs (gfls-between a* b*)) ulps)]))

(define (c-format-args var val type)
  (format "~a = ~a" var val))

(define (c-format-output result)
  (format "~a" result))

(define pvs-tester
  (tester "pvs"
          compile->pvs
          run<-c
          c-equality
          c-format-args
          c-format-output
          (const #t)
          pvs-supported
          #f))

; Command line
(module+ main
  (parameterize ([*tester* pvs-tester])
    (let ([state
           (test-core (current-command-line-arguments) (current-input-port) "stdin" "/tmp/test.c")])
      (exit state))))
