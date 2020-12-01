FILTER = racket infra/filter.rkt
known_inaccurate = "round" "isnormal" "fmod" "remainder"

### Byte-compile

setup:
	raco make main.rkt export.rkt transform.rkt toolserver.rkt evaluate.rkt 

testsetup:
	raco make infra/filter.rkt \
		infra/test-core2c.rkt infra/test-core2fptaylor.rkt infra/test-core2js.rkt infra/test-core2go.rkt infra/test-core2smtlib2.rkt infra/test-core2sollya.rkt \
		infra/test-core2wls.rkt infra/test-core2cml.rkt infra/test-core2scala.rkt

##### Testing

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

go-sanity:
ifneq (, $(shell which go))
	cat tests/sanity/*.fpcore | racket infra/test-core2go.rkt --repeat 1
else
	$(warning skipping Go sanity tests; unable to find go)
endif

scala-sanity:
ifneq (, $(shell which daisy))
	cp -r $(DAISY_BASE)/library .
	cat tests/sanity/*.fpcore | racket infra/test-core2scala.rkt --repeat 1
	rm -r library
else
	$(warning skipping Scala tests; unable to find Scala compiler)
endif

sanity: c-sanity js-sanity go-sanity smtlib2-sanity sollya-sanity wls-sanity cml-sanity fptaylor-sanity scala-sanity

raco-test:
	raco test .

c-test:
ifneq (, $(shell which $(CC)))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2c.rkt --error 3
else
	$(warning skipping C tests; unable to find C compiler $(CC))
endif

fptaylor-test:
ifneq (, $(shell which fptaylor))
	cat benchmarks/*.fpcore tests/metadata.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2fptaylor.rkt --error 3
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
    | racket infra/test-core2wls.rkt -s --error 3

else
	$(warning skipping wolframscript tests; unable to find wolframscript interpreter)
endif

go-test:
ifneq (, $(shell which go))
	cat benchmarks/*.fpcore tests/*.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2go.rkt -s --error 150
else
	$(warning skipping Go tests; unable to find Go compiler)
endif

scala-test:
ifneq (, $(shell which daisy))
	cp -r $(DAISY_BASE)/library .
	cat benchmarks/*.fpcore tests/metadata.fpcore | $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2scala.rkt
	rm -r library
else
	$(warning skipping Scala tests; unable to find Scala compiler)
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

tools-test: export-test transform-test toolserver-test evaluate-test

test: c-test js-test go-test smtlib2-test sollya-test wls-test cml-test fptaylor-test daisy-test export-test transform-test toolserver-test evaluate-test raco-test 

clean:
	$(RM) -r library tmp log *.zo *.dep

www/benchmarks.jsonp: $(wildcard benchmarks/*.fpcore)
	racket infra/core2json.rkt --padding load_benchmarks $^ > "$@"

.PHONY: c-sanity c-test fptaylor-sanity fptaylor-test js-sanity js-test smtlib2-sanity smtlib2-test sollya-sanity sollya-test wls-sanity wls-test cml-sanity cml-test go-sanity go-test \
		scala-sanity scala-test raco-test export-test transform-test toolserver-test evaluate-test tools-test sanity test testsetup setup update-tool-tests clean
