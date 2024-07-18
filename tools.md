# FPBench Tools

FPBench provides two tools for FPCore: an [exporter](#export) and a
[transformation tool](#transform).

## Exporting FPCores to other languages {#export}

The FPBench exporter compiles FPCore programs to another language. The
exporter allows FPBench's benchmarks to be used by tools with a custom
input format. The exporter is invoked like so:

``` shell
racket -y export.rkt input.fpcore output.lang
```

For example, to compile `benchmarks/rump.fpcore` to C, run:

``` shell
racket -y export.rkt benchmarks/rump.fpcore rump.c
```

The exporter infers the output language from the file extension, and
will signal an error if the extension is unknown. The `--lang` flag
can be used to override the file extension.

Supported languages include:

| Extension  | Language                                           |
|------------|----------------------------------------------------|
| `c`        | C 99 using `math.h` functions                      |
| `cakeml`   | CakeML version v1217                               |
| `f03`      | Fortran 2003 using `gfortran`                      |
| `go`       | Go version 1.12                                    |
| `hs`       | GHC Haskell version 9.8.1                          |
| `js`       | JavaScript, specifically ECMAScript Harmony        |
| `jl`       | Julia version 1.8                                  |
| `java`     | Java version 11                                    |
| `mat`      | Matlab version R2023a                              |
| `ocaml`    | OCaml version 4.04.2                               |
| `py`       | Python version 3.10                                |
| `rs`       | Rust version 1.71.0                                |
| `fptaylor` | The input format for FPTaylor                      |
| `gappa`    | The input format for Gappa                         |
| `scala`    | The input format for Daisy                         |
| `sollya`   | The input format for Sollya                        |
| `smt2`     | SMT-LIB2 using the standard floating-point theory  |
| `tex`      | LaTeX using the `amsmath` library                  |
| `wls`      | The Wolfram language as implemented in Mathematica |

The exporter also supports additional, language-dependent flags,
including:

`--bare`
:   For `c`, `go`, and `scala` export, skip the file header and
    footer.

`--namespace`
:   For `go` and `scala` export, the name of the top-level object or
    package name.

`--runtime`
:   For `js` export, a library to invoke mathematical operations on
    instead of `Math`.

`--rel-error`
:   For `gappa` export, produce expressions for relative instead of
    absolute error.

`--scale`
:   For `fptaylor` export, the scale factor for operations which
    are not correctly rounded.

The argument `-` can be used in place of the input or output file
names to use standard input and output. When outputting to standard
out, the `--lang` flag is necessary to specify the output language.

## Applying transformations to FPCores {#transform}

The FPBench transformation tool applies a variety of transformations to
FPCore programs, such as common subexpression elimination, precondition
simplification, and de- and resugaring. The transformation tool is
invoked like so:

``` shell
racket -y transform.rkt args ... input.fpcore output.fpcore
```

The list `args` of arguments names a list of transformations, such as:

`--unroll N`
:   Unroll the first `N` iterations of each loop. Each iteration
    consists of a `let` or `let*` to bind initial values and an `if`
    to check the conditions. This sound transformation is frequently
    combined with the unsound `--skip-loops` to simulate loops by
    their first few iterations.

`--skip-loops`
:   Replaces a `while` loop with a simple `let` which binds the initial
    values and executes the body (as if the loop executed zero time).
    `while*` loops are likewise transformed into `let*`.

`--precondition-ranges`
:   Weakens the precondition to a conjunction (one conjunct per
    argument) of a disjunction of ranges. The precondition is guaranteed
    to be weaker. This transformation is useful for exporting to a
    language that only allows ranges as preconditions.

`--precondition-range`
:   Like `--precondition-ranges`, but further weakens to precondition to
    a conjunction of single ranges for each variable.

`--expand-let*`
:   Expands each `let*` to a series of nested `let` expressions.

`--expand-while*`
:   Expands each `while*` to a `while` loop with nested `let*`
    expressions.

`--cse`
:   Lifts each common subexpression to an intermediate variable bound by
    a `let*` expression.

`--subexprs`
:   Converts each FPCore into multiple FPCores, one for each
    subexpression in the original.

The transformations which are applied in order (left to right) to each
FPCore expression in the input file. The ordering is especially
important for pairs of operations such as `--unroll` and
`--skip-loops`.

Like for the exporter, the argument `-` can be used in place of the
input or output file names to use standard input and output.
