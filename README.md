![FPBench](www/img/logo.png)

FPBench makes it easier to compare and combine tools from the
floating-point research community by developing common standards and
benchmark suites.

[![Build Status](https://travis-ci.org/FPBench/FPBench.svg?branch=master)](https://travis-ci.org/FPBench/FPBench)

Standards
---------

The FPBench standards are documented in `www/spec/`. FPBench currently
contains three standards:

 - *FPCore* describes floating-point computations. FPCore is a simple
   S-expression functional programming language and can represent
   arithmetic, transcendental operators, loops, and conditionals.
 - *Metadata* describe the provenance and interpretation of FPCore
   computations.
 - *Measures* describe accuracy measurements for FPCore computations.
   Several measures are standardized.

Each standard has achieved 1.0 status and can be used by implementations.

Benchmarks
----------

The FPBench benchmarks are located in `benchmarks/` in FPCore format.

FPBench contains 72 benchmarks from four sources (FPTaylor, Herbie,
Salsa, and Rosa) covering a variety of application domains and the
full complement of FPCore features.

Tools
-----

The FPBench tools are located in `tools/`, and make it easier to
write, test, and use FPCore computations. These tools include:

 - `fpcore.rkt` runs FPCore computations and can be used as a
   reference for FPCore implementation work.
 - `core2c.rkt` exports FPCore computations to C
 
These tools are documented in `www/tools.html`.

FPBench also ships with a set of tools based on the FPImp intermediate
language. FPImp is an imperative extension to FPCore which makes
translating C, Fortran, Matlab, or other imperative languages to
FPCore simpler. FPImp is documented in `www/fpimp.html`, and FPBench
ships with tools for manipulating FPImp programs:

 - `fpimp.rkt` runs FPImp programs
 - `imp2core.rkt` compiles FPImp to FPCore

We recommend using FPImp only for writing FPCore computations; FPImp
is not a standard and we do not recommend using it in other tools.

Helping Out
-----------

FPBench is organized on our
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/fpbench)
where we discuss work in progress, edit proposed standards, and
announce major
improvements. [Email us](mailto:fpbench.cs.washington.edu) if you have
questions or would like to get involved!
