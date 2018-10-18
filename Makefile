.PHONY: sanity test

FILTER = racket tools/filter.rkt

core2c_prec = binary32 binary64

core2js_prec = binary64
core2js_unsupported = "fma" "!=" "isfinite" "isnormal" "signbit" "exp2" "erf" "erfc" "tgamma" "lgamma" "fmod" "remainder" "fdim" "copysign" "nearbyint"

core2smtlib2_prec = binary32 binary64
core2smtlib2_unsupported = "while" "!=" "exp" "exp2" "expm1" "log" "log10" "log2" "log1p" "pow" "cbrt" "hypot" "sin" "cos" "tan" "asin" "acos" "atan" "atan2" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh" "erf" "erfc" "tgamma" "lgamma" "ceil" "floor" "fmod" "fdim" "copysign" "isfinite" "LOG2E" "LOG10E" "M_1_PI" "M_2_PI" "M_2_SQRTPI"

core2wls_prec = binary32 binary64

known_inaccurate = "expm1" "log10" "log2" "log1p" "cbrt" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh" "erf" "erfc" "fmod" "remainder"


sanity:
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2c_prec) \
	| racket infra/test-core2c.rkt --repeat 1

	cat tests/sanity*.fpcore | $(FILTER) precision $(core2js_prec) | $(FILTER) not-operators $(core2js_unsupported) \
	| racket infra/test-core2js.rkt -o ./tmp.js --repeat 1

	cat tests/sanity*.fpcore | $(FILTER) precision $(core2smtlib2_prec) | $(FILTER) not-operators $(core2smtlib2_unsupported) \
	| racket infra/test-core2smtlib2.rkt --repeat 1

ifneq (, $(shell which wolframscript))
	cat tests/sanity*.fpcore | $(FILTER) precision $(core2wls_prec) \
	| racket infra/test-core2wls.rkt --repeat 1
endif


test:
	racket infra/test-imp2core.rkt benchmarks/*.fpimp

	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2c_prec) \
	| $(FILTER) not-operators $(known_inaccurate) \
	| racket infra/test-core2c.rkt

	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2js_prec) \
	| $(FILTER) not-operators $(core2js_unsupported) $(known_inaccurate) \
	| racket infra/test-core2js.rkt -o ./tmp.js --error 150

	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2smtlib2_prec) \
	| $(FILTER) not-operators $(core2smtlib2_unsupported) $(known_inaccurate) \
	| racket infra/test-core2smtlib2.rkt

ifneq (, $(shell which wolframscript))
	cat benchmarks/*.fpcore tests/test*.fpcore | $(FILTER) precision $(core2wls_prec) \
	| racket infra/test-core2wls.rkt
endif


%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt --canonicalize < $^ >> $@


c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@
