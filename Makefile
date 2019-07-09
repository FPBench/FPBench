.PHONY: sanity test setup

FILTER = racket infra/filter.rkt

core2c_prec = binary32 binary64

core2js_prec = binary64
core2js_unsupported_ops = "fma" "!=" "isfinite" "isnormal" "signbit" "exp2" "erf" "erfc" "tgamma" "lgamma" "fmod" "remainder" "fdim" "copysign" "nearbyint"

core2smtlib2_prec = binary32 binary64
core2smtlib2_unsupported_ops = "while" "!=" "exp" "exp2" "expm1" "log" "log10" "log2" "log1p" "pow" "cbrt" "hypot" "sin" "cos" "tan" "asin" "acos" "atan" "atan2" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh" "erf" "erfc" "tgamma" "lgamma" "ceil" "floor" "fmod" "fdim" "copysign" "isfinite"
core2smtlib2_unsupported_consts = "LOG2E" "LOG10E" "M_1_PI" "M_2_PI" "M_2_SQRTPI"

core2sollya_prec = binary32 binary64
core2sollya_unsupported_ops = "isnormal" "tgamma" "lgamma" "remainder" "fmod" "round" "cbrt" "atan2"

core2wls_prec = binary32 binary64

known_inaccurate = "while*" "let*" "round" "isnormal" "fmod" "remainder"

sanity:
ifneq (, $(shell which $(CC)))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2c_prec) \
	| racket infra/test-core2c.rkt --repeat 1
else
	$(warning skipping C tests; unable to find C compiler $(CC))
endif

ifneq (, $(shell which node))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2js_prec) \
	| $(FILTER) not-operators $(core2js_unsupported_ops) \
	| racket infra/test-core2js.rkt --repeat 1
else
	$(warning skipping javascript tests; unable to find node)
endif

ifneq (, $(shell which z3))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2smtlib2_prec) \
	| $(FILTER) not-operators $(core2smtlib2_unsupported_ops) \
	| $(FILTER) not-constants $(core2smtlib2_unsupported_consts) \
	| racket infra/test-core2smtlib2.rkt --repeat 1
else
	$(warning skipping smtlib2 tests; unable to find z3)
endif

ifneq (, $(shell which sollya))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2sollya_prec) \
	| $(FILTER) not-operators $(core2sollya_unsupported_ops) \
	| racket infra/test-core2sollya.rkt --repeat 1
else
	$(warning skipping sollya tests; unable to sollya interpreter)
endif

ifneq (, $(shell which wolframscript))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2wls_prec) \
	| racket infra/test-core2wls.rkt --repeat 1
else
	$(warning skipping wolframscript tests; unable to find wolframscript interpreter)
endif


test:
	raco test .

ifneq (, $(shell which $(CC)))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2c_prec) \
	| $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2c.rkt --error 3
else
	$(warning skipping C tests; unable to find C compiler $(CC))
endif

ifneq (, $(shell which node))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2js_prec) \
	| $(FILTER) not-operators $(core2js_unsupported_ops) $(known_inaccurate) \
	| racket infra/test-core2js.rkt --error 150
else
	$(warning skipping javascript tests; unable to find node)
endif

ifneq (, $(shell which z3))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2smtlib2_prec) \
	| $(FILTER) not-operators $(core2smtlib2_unsupported_ops) $(known_inaccurate) \
	| $(FILTER) not-constants $(core2smtlib2_unsupported_consts) \
	| racket infra/test-core2smtlib2.rkt
else
	$(warning skipping smtlib2 tests; unable to find z3)
endif

ifneq (, $(shell which sollya))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2sollya_prec) \
	| $(FILTER) not-operators $(core2sollya_unsupported_ops) $(known_inaccurate) \
	| racket infra/test-core2sollya.rkt
else
	$(warning skipping sollya tests; unable to sollya interpreter)
endif

ifneq (, $(shell which wolframscript))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2wls_prec) \
	| racket infra/test-core2wls.rkt
else
	$(warning skipping wolframscript tests; unable to find wolframscript interpreter)
endif

setup:
	raco make export.rkt transform.rkt
