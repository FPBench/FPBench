# Herbie Test Format

### Overview

Herbie tests are written as Racket `lambda`s in s-expression syntax.  The
arguments to the lambda are the variables to the expression that we would like
to improve.  For example, a Herbie test of the identify function could just be:

    (lambda (x)
      x)

Herbie recognizes several optional named arguments.  A test for cancellation
could be:

    (lambda (x y)
      #:name "Cancellation test"
      (+ x (- y x))
      #:target
      y)

In this case, we are giving a name to the test ("Cancellation test") and also
"target" expression whose accuracy Herbie should match.  A target is only used
for comparing the accuracy of Herbie's output. Herbie does not use the target
during its search and the evaluation does not try to syntactically compare
Herbie's output to the target.

Herbie also allows users to specify the input ranges where they'd like to
improve the expression.  This is useful for partial expressions or when a user
knows their code will be run on a certain class of inputs.  For example, to
only consider positive values of `x` and values of `y` between -1 and 1, a user
could write a test like:

    (lambda ([x (< 0 default)] [y (< -1 default 1)])
      (exp (* (sqrt x) (sin y))))

In such tests `default` is a synonym for `float` or `double`, depending on
whether Herbie is run in 32-bit or 64-bit mode.

Because Herbie uses sampling to determine the error of a floating point
expression, users can specify the input distribution of each variable. By
default, Herbie uses a uniform distribution over 64 bit <em>floats</em>,
essentially just flipping a coin to determine each bit.  To instead sample
uniformly over the <em>reals</em> that floating point can represent, a user
can annotate an input like so:

    (lambda ([y (uniform -1 1)])
      (sqrt (- 1 (sqr x))))

Herbie allows users to test and target expressions that contain `let*`s and
`if`s, as shown in these examples drawn from Herbie's standard benchmarks:

    # example with an if in the target
    (lambda ([x (< -.026 default .026)])
      #:name "NMSE example 3.9"
      (- (/ 1 x) (cotan x))
      #:target
      (if (< (abs x) .026)
          (* (/ x 3) (+ 1 (/ (sqr x) 15)))
          (- (/ 1 x) (cotan x))))

    # example with let in test as well as let and if in target
    (lambda (a b c)
      #:name "NMSE p42"
      (let* ((d (sqrt (- (sqr b) (* 4 (* a c))))))
        (/ (- (- b) d) (* 2 a)))
      #:target
      (let* ((d (sqrt (- (sqr b) (* 4 (* a c)))))
             (r1 (/ (+ (- b) d) (* 2 a)))
             (r2 (/ (- (- b) d) (* 2 a))))
        (if (< b 0)
            (/ c (* a r1))
            r2)))

### Expression Syntax

Herbie supports the following functions:

### Samplers and Distributions

### Fields

## Proposed Extensions

### Conditions and Rely/Guarantee

### Control: branches, variables, and loops

### Macros

