FILTER = racket infra/filter.rkt

### Install / Compile

setup:
	raco pkg install --skip-installed --auto generic-flonum
	raco make main.rkt export.rkt transform.rkt toolserver.rkt evaluate.rkt 

testsetup:
	raco make infra/*.rkt

##### Testing

c-sanity:
ifneq (, $(shell which $(CC)))
	cat tests/sanity/*.fpcore | racket infra/test-core2c.rkt --repeat 1
else
	$(warning skipping C sanity tests; unable to find C compiler $(CC))
endif

java-sanity:
ifneq (, $(shell which java))
	cat tests/sanity/*.fpcore | racket infra/test-core2java.rkt --repeat 1
else
	$(warning skipping Java sanity tests; unable to find Java compiler)
endif

fptaylor-sanity:
ifneq (, $(shell which fptaylor))
	cat tests/sanity/*.fpcore | racket infra/test-core2fptaylor.rkt --repeat 1
	$(RM) -r log tmp
else
	$(warning skipping fptaylor sanity tests; unable to find fptaylor)
endif

js-sanity:
ifneq (, $(shell which node))
	cat tests/sanity/*.fpcore | racket infra/test-core2js.rkt --repeat 1
else
	$(warning skipping javascript sanity tests; unable to find node)
endif

smtlib2-sanity:
ifneq (, $(shell which z3))
	cat tests/sanity/*.fpcore | racket infra/test-core2smtlib2.rkt --repeat 1
else
	$(warning skipping smtlib2 sanity tests; unable to find z3)
endif

sollya-sanity:
ifneq (, $(shell which sollya))
	cat tests/sanity/*.fpcore | racket infra/test-core2sollya.rkt --repeat 1
else
	$(warning skipping sollya sanity tests; unable to sollya interpreter)
endif

cakeml-sanity:
ifneq (, $(shell which cake))
	cat tests/sanity/*.fpcore | racket infra/test-core2cakeml.rkt --repeat 1
else
	$(warning skipping CakeML sanity tests; unable to find CakeML compiler)
endif

wls-sanity:
ifneq (, $(shell which wolframscript))
	cat tests/sanity/*.fpcore | racket infra/test-core2wls.rkt --repeat 1
else
	$(warning skipping wolframscript sanity tests; unable to find wolframscript interpreter)
endif

go-sanity:
ifneq (, $(shell which go))
	cat tests/sanity/*.fpcore | racket infra/test-core2go.rkt --repeat 1
else
	$(warning skipping Go sanity tests; unable to find go)
endif

rust-sanity:
ifneq (, $(shell which cargo))
	cat tests/sanity/*.fpcore | racket infra/test-core2rust.rkt --repeat 1
else
	$(warning skipping Rust sanity tests; unable to find cargo)
endif

scala-sanity:
ifneq (, $(shell which daisy))
	cp -r $(DAISY_BASE)/library .
	cat tests/sanity/*.fpcore | racket infra/test-core2scala.rkt --repeat 1
	rm -r library
else
	$(warning skipping Scala sanity tests; unable to find Scala compiler)
endif

ocaml-sanity:
ifneq (, $(shell which ocamlopt))
	cat tests/sanity/*.fpcore | racket infra/test-core2ocaml.rkt --repeat 1
else
	$(warning skipping OCaml sanity tests; unable to find OCaml compiler)
endif

python-sanity:
ifneq (, $(shell which python3))
	cat tests/sanity/*.fpcore | racket infra/test-core2python.rkt --repeat 1
else
	$(warning skipping Python sanity tests; unable to find Python interpreter)
endif

fortran-sanity:
ifneq (, $(shell which $(CC)))
	cat tests/sanity/*.fpcore | racket infra/test-core2fortran03.rkt --repeat 1
else
	$(warning skipping Fortran sanity tests; unable to find Fortran compiler)
endif

matlab-sanity:
ifneq (, $(shell which matlab))
	cat tests/sanity/*.fpcore | racket infra/test-core2matlab.rkt --repeat 1
else
	$(warning skipping MATLAB sanity tests; unable to find MATLAB tool)
endif

haskell-sanity:
ifneq (, $(shell which ghc))
	cat tests/sanity/*.fpcore | racket infra/test-core2haskell.rkt --repeat 1
else
	$(warning skipping Haskell sanity tests; unable to find Haskell compiler)
endif

julia-sanity:
ifneq (, $(shell which julia))
	cat tests/sanity/*.fpcore | racket infra/test-core2julia.rkt --repeat 1
else
	$(warning skipping Julia sanity tests; unable to find Julia interpreter)
endif

sanity: c-sanity java-sanity js-sanity go-sanity smtlib2-sanity sollya-sanity \
		wls-sanity cml-sanity fptaylor-sanity scala-sanity ocaml-sanity \
		python-sanity fortran-sanity matlab-sanity haskell-sanity julia-sanity \
		rust-sanity

raco-test:
	raco test .

c-test:
ifneq (, $(shell which $(CC)))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2c.rkt --error 3
else
	$(warning skipping C tests; unable to find C compiler $(CC))
endif

java-test:
ifneq (, $(shell which java))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2java.rkt --error 3
else
	$(warning skipping Java tests; unable to find Java compiler)
endif

fptaylor-test:
ifneq (, $(shell which fptaylor))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2fptaylor.rkt --error 3
	$(RM) -r log tmp
else
	$(warning skipping fptaylor tests; unable to find fptaylor)
endif

js-test:
ifneq (, $(shell which node))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2js.rkt --error 150
else
	$(warning skipping javascript tests; unable to find node)
endif

smtlib2-test:
ifneq (, $(shell which z3))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2smtlib2.rkt
else
	$(warning skipping smtlib2 tests; unable to find z3)
endif

sollya-test:
ifneq (, $(shell which sollya))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2sollya.rkt
else
	$(warning skipping sollya tests; unable to find sollya interpreter)
endif

cakeml-test:
ifneq (, $(shell which cake))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2cakeml.rkt
else
	$(warning skipping CakeML tests; unable to find CakeML compiler)
endif

wls-test:
ifneq (, $(shell which wolframscript))
	cat benchmarks/*.fpcore tests/*.fpcore  | racket infra/test-core2wls.rkt -s --error 150

else
	$(warning skipping wolframscript tests; unable to find wolframscript interpreter)
endif

go-test:
ifneq (, $(shell which go))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2go.rkt -s --error 150
else
	$(warning skipping Go tests; unable to find Go compiler)
endif

rust-test:
ifneq (, $(shell which cargo))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2rust.rkt -s --error 150
else
	$(warning skipping Rust tests; unable to find Rust compiler)
endif

scala-test:
ifneq (, $(shell which daisy))
	cp -r $(DAISY_BASE)/library .
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2scala.rkt
	rm -r library
else
	$(warning skipping Scala tests; unable to find Scala compiler)
endif

ocaml-test:
ifneq (, $(shell which ocamlopt))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2ocaml.rkt --error 3
else
	$(warning skipping OCaml tests; unable to find OCaml compiler)
endif

python-test:
ifneq (, $(shell which python3))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2python.rkt --error 3
else
	$(warning skipping Python tests; unable to find Python interpreter)
endif

fortran-test:
ifneq (, $(shell which $(CC)))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2fortran03.rkt --error 3
else
	$(warning skipping Fortran tests; unable to find Fortran compiler)
endif

matlab-test:
ifneq (, $(shell which matlab))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2matlab.rkt --error 3
else
	$(warning skipping MATLAB sanity tests; unable to find MATLAB tool)
endif

haskell-test:
ifneq (, $(shell which ghc))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2haskell.rkt --error 3
else
	$(warning skipping Haskell tests; unable to find Haskell compiler)
endif

julia-test:
ifneq (, $(shell which julia))
	cat benchmarks/*.fpcore tests/*.fpcore | racket infra/test-core2julia.rkt --error 20
else
	$(warning skipping Julia tests; unable to find Julia interpreter)
endif

# For CI, Julia takes to long in a single run
julia-benchmarks:
ifneq (, $(shell which julia))
	cat benchmarks/*.fpcore | racket infra/test-core2julia.rkt --error 20
else
	$(warning skipping Julia tests; unable to find Julia interpreter)
endif

# For CI, Julia takes to long in a single run
julia-binary64:
ifneq (, $(shell which julia))
	cat tests/*default.fpcore tests/letstar.fpcore tests/metadata.fpcore |	\
		racket infra/test-core2julia.rkt --error 20
else
	$(warning skipping Julia tests; unable to find Julia interpreter)
endif

# For CI, Julia takes to long in a single run
julia-binary32:
ifneq (, $(shell which julia))
	cat tests/*binary32.fpcore | racket infra/test-core2julia.rkt --error 20
else
	$(warning skipping Julia tests; unable to find Julia interpreter)
endif


update-tool-tests:
	tests/scripts/test-export.sh generate	
	tests/scripts/test-transform.sh generate
	tests/scripts/test-evaluate.sh generate

export-test:
	tests/scripts/test-export.sh

transform-test:
	tests/scripts/test-transform.sh

toolserver-test:
	tests/scripts/test-toolserver.sh

evaluate-test:
	tests/scripts/test-evaluate.sh

tensor-test:
	cat tests/tensor/batch.txt | racket toolserver.rkt

tools-test: export-test transform-test toolserver-test evaluate-test tensor-test

test: c-test java-test js-test go-test smtlib2-test sollya-test wls-test \
	  cml-test fptaylor-test daisy-test ocaml-test python-test matlab-test \
	  haskell-test rust-test export-test transform-test toolserver-test \
	  evaluate-test raco-test

clean:
	$(RM) -r library tmp log *.zo *.dep

www/benchmarks.jsonp: $(wildcard benchmarks/*.fpcore)
	racket infra/core2json.rkt --padding load_benchmarks $^ > "$@"

.PHONY: c-sanity c-test fptaylor-sanity fptaylor-test js-sanity js-test \
		julia-sanity julia-test julia-benchmarks julia-binary64 julia-binary32 \
		smtlib2-sanity smtlib2-test sollya-sanity sollya-test wls-sanity wls-test \
		cml-sanity cml-test go-sanity go-test scala-sanity scala-test \
		raco-test export-test transform-test toolserver-test evaluate-test \
		tools-test sanity test testsetup setup update-tool-tests clean
