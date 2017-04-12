#lang racket

(require "common.rkt" math/flonum math/bigfloat)
(provide eval-on-points)

(define (eval-expr expr ctx real->number op->function constant->number)
  (let eval ([expr expr] [ctx ctx])
    (match expr
      [(? number?) (real->number expr)]
      [(? constant?) ((constant->number expr))]
      [(? symbol?) (dict-ref ctx expr)]
      [`(if ,test ,ift ,iff)
       (if (eval test ctx)
           (eval ift ctx)
           (eval iff ctx))]
      [`(let ([,vars ,vals] ...) ,body)
       (define vals* (for/list ([val vals]) (eval val ctx)))
       (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
       (eval body ctx*)]
      [`(while ,test ([,vars ,inits ,updates] ...) ,res)
       (define vals* (for/list ([init inits]) (eval init ctx)))
       (define ctx* (apply dict-set* ctx (append-map list vars vals*)))
       (if (eval test ctx*)
           (let ([inits* (for/list ([update updates]) (eval update ctx*))])
             (eval
              `(while ,test
                 (,(for/list ([var vars] [init inits] [update updates])
                     (list var (eval update ctx*) update)))
                 ,res)
              ctx))
           (eval res ctx*))]
      [(list op args ...)
       (apply (op->function op) (map (curryr eval ctx) args))])))

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
  [ not    not      not     ] [ sqr    sqr      bfsqr   ])

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

(define (make-exacts* args prog pts #:type type #:start-prec prec0)
  (let loop ([prec prec0] [prev #f])
    (let ([curr (eval-bf args prog pts #:precision prec #:type type)])
      (if (and prev (andmap =-or-nan? prev curr))
          (values (- prec precision-step) curr)
          (loop (+ prec precision-step) curr)))))

(define (make-exacts args prog pts #:type type)
  (define n (length pts))
  (let loop ([n* 1] [prec 128])
    (cond
     [(>= n* n)
      (define-values (_ points)
        (make-exacts* args prog pts #:type type #:start-prec prec))
      points]
     [else
      (define-values (prec* _)
        (make-exacts* args prog (take pts n*) #:type type #:start-prec prec))
      (loop (* n* 2) prec*)])))

(define (eval-on-points args body #:pre pre #:num num #:type type)
  (define (sample _) (if (equal? type 'binary32) (sample-float) (sample-double)))
  (define nargs (length args))

  (define samples
    (let loop ([samples '()] [n 0])
      (if (equal? n num)
          samples
          (let ([pt (build-list nargs sample)])
            (if (car (eval-bf args pre (list pt) #:precision 256 #:type type))
                (loop (cons pt samples) (+ n 1))
                (loop samples n))))))

  (values samples (make-exacts args body samples #:type type) (eval-fl args body samples #:type type)))

(define (eval-fl args expr points #:type type)
  (define real->
    (if (equal? type 'binary32) real->single-flonum real->double-flonum))
  (define (op-> op) (operators op 'ieee))
  (define (constant-> constant) (constants constant type))
  (for/list ([point points])
    (eval-expr expr (map (λ (v x) (cons v (real-> x))) args point) real-> op-> constant->)))

(define (eval-bf args expr points #:type type #:precision prec)
  (define real->
    (if (equal? type 'binary32) real->single-flonum real->double-flonum))
  (parameterize ([bf-precision prec])
    (for/list ([point points])
      (define out
       (eval-expr expr (map (λ (v x) (cons v (bf x))) args point) bf (curryr operators 'mpfr) (curryr constants 'mpfr)))
      (match out
       [(? bigfloat?) (real-> (bigfloat->real out))]
       [(? boolean?) out]))))

(define (average-error expr #:measure [measurefn bits-error] #:points [N 8000])
  (match-define (list 'fpcore (list args ...) props ... body) expr)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':type 'binary64))
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

  (command-line
   #:program "core2avgerr.rkt"
   #:once-each
   [("--measure") measurename "Which error measure to use (abs|rel|ulp|bit)"
    (set! measure (match measurename ["bit" bits-error] ["ulp" ulp-error] ["abs" abs-error] ["rel" rel-error]))]
   #:args ()
   (for ([expr (in-port read (current-input-port))])
     (displayln (average-error expr #:measure measure)))))
