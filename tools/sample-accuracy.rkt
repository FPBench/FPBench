#lang racket

(require "common.rkt" "fpcore.rkt" math/flonum math/bigfloat)
(provide eval-on-points)

(define-syntax-rule (table-fn [var val] ...)
  (match-lambda [`var val] ...))

(define/contract bf-evaluator evaluator?
  (evaluator
   bf
   (table-fn
    [E (bfexp 1.bf)] [LOG2E (bf/ 1.bf log2.bf)]
    [LOG10E (bf/ 1.bf (bflog 10.bf))] [LN2 log2.bf] [LN10 (bflog 10.bf)]
    [PI pi.bf] [PI_2 (bf/ pi.bf 2.bf)] [PI_4 (bf/ pi.bf 4.bf)]
    [1_PI (bf/ 1.bf pi.bf)] [2_PI (bf/ 2.bf pi.bf)]
    [2_SQRTPI (bf/ 2.bf (bfsqrt pi.bf))] [SQRT2 (bfsqrt 2.bf)]
    [SQRT1_2 (bf/ 1.bf (bfsqrt 2.bf))] [NAN +nan.bf] [INFINITY +inf.bf]
    [TRUE #t] [FALSE #f])
   (table-fn
    [+ bf+] [- bf-] [* bf*] [/ bf/] [fabs bfabs] [exp bfexp]
    [exp2 bfexp2] [log bflog] [log10 bflog10] [log2 bflog2]
    [pow bfexpt] [sqrt bfsqrt] [hypot bfhypot] [sin bfsin]
    [cos bfcos] [tan bftan] [asin bfasin] [acos bfacos] [atan bfatan]
    [sinh bfsinh] [cosh bfcosh] [tanh bftanh] [asinh bfasinh]
    [acosh bfacosh] [atanh bfatanh] [erf bferf] [erfc bferfc]
    [tgamma bfgamma] [lgamma bflog-gamma] [ceil bfceiling]
    [floor bffloor] [trunc bftruncate] [round bfround] [fmax bfmax]
    [min bfmin] [fdim (λ (x y) (bfabs (bf- x y)))] [expm1 bfexpm1]
    [log1p bflog1p] [< bf<] [> bf>] [<= bf<=] [>= bf>=] [== bf=]
    [!= (compose not bf=)] [not not] [and (λ (x y) (and x y))]
    [or (λ (x y) (or x y))] [isfinite bfrational?]
    [isinf bfinfinite?] [isnan bfnan?] [isnormal bfrational?]
    [signbit (λ (x) (= (bigfloat-signbit x) 1))]
    ; TODO: currently unsupported
    [fma '?] [fmod '?] [remainder '?])))

(define-syntax-rule (define/table (name cols ...) [rows valss ...] ...)
  (define name
    (let ([table (make-hash (list (list 'rows valss ...) ...))])
      (λ (row col)
        (define row-vals (dict-ref table row))
        (for/first ([col-name '(cols ...)] [val row-vals]
                    #:when (equal? col col-name))
          val)))))

(define (bfmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define/table (constants binary32 binary64 mpfr)
  ;; TODO: Missing a lot of constants
  [E (exp 1.0f0) (flexp 1.0) (λ () (bfexp 1.bf))]
  [PI pi.f pi (λ () pi.bf)]
  [TRUE #t #t (λ () #t)]
  [FALSE #f #f (λ () #f)])

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define/table (operators ieee mpfr)
  [ +      fl+      bf+     ] [ -      fl-      bf-     ]
  [ *      fl*      bf*     ] [ /      fl/      bf/     ]
  [ abs    flabs    bfabs   ] [ sqrt   flsqrt   bfsqrt  ]
  [ hypot  flhypot  bfhypot ] [ exp    flexp    bfexp   ]
  [ expm1  flexpm1  bfexpm1 ] [ pow    flexpt   bfexpt  ]
  [ log    fllog    bflog   ] [ log1p  fllog1p  bflog1p ]
  [ sinh   flsinh   bfsinh  ] [ cosh   flcosh   bfcosh  ]
  [ tanh   fltanh   bftanh  ] [ sin    flsin    bfsin   ]
  [ cos    flcos    bfcos   ] [ tan    fltan    bftan   ]
  [ asin   flasin   bfasin  ] [ acos   flacos   bfacos  ]
  [ atan   flatan   bfatan  ] [ atan2  atan     bfatan2 ]
  [ <      <        bf<     ] [ >      >        bf>     ]
  [ ==     =        bf=     ] [ !=     (compose not =) (compose not bf=)]
  [ <=     <=       bf<=    ] [ >=     >=       bf>=    ]
  [ and    and-fn   and-fn  ] [ or     or-fn    or-fn   ]
  [ not    not      not     ])

(define (single-flonum->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f))

(define (single-flonum->ordinal x)
  (cond [(x . < . 0.0f0) (- (single-flonum->bit-field (- 0.0f0 x)))]
	[else            (single-flonum->bit-field (abs x))]))

(define (single-flonums-between x y)
  (- (single-flonum->ordinal y) (single-flonum->ordinal x)))

(define (ulp-difference x y #:type type)
  ((if (equal? type 'binary64) flonums-between single-flonums-between) x y))

(define (abs-error x y #:type type)
  (abs (- x y)))

(define (rel-error x y #:type type)
  (abs (/ (- x y) y)))

(define (ulp-error x y #:type type)
  (abs (exact->inexact (ulp-difference x y #:type type))))

(define (bits-error x y #:type type)
  (log (exact->inexact (+ 1 (abs (ulp-difference x y #:type type))))))

(define (random-exp k)
  "Like (random (expt 2 k)), but k is allowed to be arbitrarily large"
  (if (< k 31) ; Racket generates random numbers in the range [0, 2^32-2]; I think it's a bug
      (random (expt 2 k))
      (let ([head (* (expt 2 31) (random-exp (- k 31)))])
        (+ head (random (expt 2 31))))))

(define (=-or-nan? x y)
  (or (= x y) (and (nan? x) (nan? y))))

(define (sample-float)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f)))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))

(define precision-step 32)

(define (eval-on-points args body #:pre pre #:num num #:type type)
  (define (sample _) (if (equal? type 'binary32) (sample-float) (sample-double)))
  (define nargs (length args))

  (define samples
    (let loop ([samples '()] [n 0])
      (if (equal? n num)
          samples
          (let ([pt (build-list nargs sample)])
            (if ((eval-expr bf-evaluator) pre (map cons args (map bf pt)))
                (loop (cons pt samples) (+ n 1))
                (loop samples n))))))
  (define evaltor (match type ['binary32 racket-single-evaluator] ['binary64 racket-double-evaluator]))

  (values samples
          (for/list ([sample samples])
            ((evaluator-real evaltor)
             (bigfloat->real ((eval-expr bf-evaluator) body (map cons args (map bf sample))))))
          (for/list ([sample samples])
            (match
                ((eval-expr evaltor) body (map cons args (map (evaluator-real evaltor) sample)))
              [(? real? out) out]
              [_ +nan.0]))))

(define (average-error expr #:measure [measurefn bits-error] #:points [N 8000])
  (match-define (list 'FPCore (list args ...) props ... body) expr)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))
  (define pre (dict-ref properties ':pre 'TRUE))

  (define-values (points exacts approxs)
    (eval-on-points args body #:pre pre #:num N #:type type))

  (/
   (flsum ; Avoid rounding error when summing errors
    (for/list ([pt points] [exact exacts] [approx approxs])
      (measurefn approx exact #:type type)))
   (length points)))

(module+ main
  (require racket/cmdline)

  (define measure bits-error)
  (define precision 2048)

  (command-line
   #:program "core2avgerr.rkt"
   #:once-each
   [("--measure") measurename "Which error measure to use (abs|rel|ulp|bit)"
    (set! measure (match measurename ["bit" bits-error] ["ulp" ulp-error] ["abs" abs-error] ["rel" rel-error]))]
   [("--precision") bits "How many bits to use for precise evaluation (default: 2048)"
    (set! precision (string->number bits))]
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (displayln (parameterize ([bf-precision precision]) (average-error expr #:measure measure))))))
