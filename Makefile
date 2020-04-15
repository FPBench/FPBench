all: testsetup sanity test setup

FILTER = racket infra/filter.rkt
known_inaccurate = "round" "isnormal" "fmod" "remainder"

c-sanity:
ifneq (, $(shell which $(CC)))
	cat tests/sanity/*.fpcore | racket infra/test-core2c.rkt --repeat 1
else
	$(warning skipping C sanity tests; unable to find C compiler $(CC))
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

cml-sanity:
ifneq (, $(shell which cake))
	cat tests/sanity/*.fpcore | racket infra/test-core2cml.rkt --repeat 1
else
	$(warning skipping CakeML sanity tests; unable to find CakeML compiler)
endif

wls-sanity:
ifneq (, $(shell which wolframscript))
	cat tests/sanity/*.fpcore | racket infra/test-core2wls.rkt --repeat 1
else
	$(warning skipping wolframscript sanity tests; unable to find wolframscript interpreter)
endif

sanity: c-sanity fptaylor-sanity js-sanity smtlib2-sanity sollya-sanity wls-sanity

raco-test:
	raco test .

c-test:
ifneq (, $(shell which $(CC)))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2c.rkt --error 3
else
	$(warning skipping C tests; unable to find C compiler $(CC))
endif

# Core to C???
fptaylor-test:
ifneq (, $(shell which fptaylor))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2c.rkt --error 3
	$(RM) -r log tmp
else
	$(warning skipping fptaylor tests; unable to find fptaylor)
endif

js-test:
ifneq (, $(shell which node))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2js.rkt --error 150
else
	$(warning skipping javascript tests; unable to find node)
endif

smtlib2-test:
ifneq (, $(shell which z3))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2smtlib2.rkt
else
	$(warning skipping smtlib2 tests; unable to find z3)
endif

sollya-test:
ifneq (, $(shell which sollya))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2sollya.rkt
else
	$(warning skipping sollya tests; unable to find sollya interpreter)
endif

cml-test:
ifneq (, $(shell which cake))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2cml.rkt
else
	$(warning skipping CakeML tests; unable to find CakeML compiler)
endif

wls-test:
ifneq (, $(shell which wolframscript))
	cat benchmarks/*.fpcore tests/*.fpcore  | $(FILTER) not-operators $(known_inaccurate) \
 	| racket infra/test-core2wls.rkt --error 3
else
	$(warning skipping wolframscript tests; unable to find wolframscript interpreter)
endif

test: c-test fptaylor-test js-test smtlib2-test sollya-test wls-test raco-test

testsetup:
	raco make infra/filter.rkt infra/test-core2c.rkt infra/test-core2fptaylor.rkt infra/test-core2js.rkt infra/test-core2smtlib2.rkt infra/test-core2sollya.rkt infra/test-core2wls.rkt

setup:
	raco make export.rkt transform.rkt

www/benchmarks.jsonp: $(wildcards benchmarks/*.fpcore)
	racket infra/core2json.rkt --padding load_benchmarks $^

.PHONY: c-sanity c-test fptaylor-sanity fptaylor-test js-sanity js-test smtlib2-sanity smtlib2-test sollya-sanity sollya-test wls-sanity wls-test raco-test sanity test testsetup setup
