.PHONY: test

test:
	racket infra/test-imp2core.rkt benchmarks/*.fpimp
	cat benchmarks/*.fpcore | racket tools/filter.rkt precision binary32 binary64 \
	| racket infra/test-core2c.rkt
	cat benchmarks/*.fpcore | racket tools/filter.rkt precision binary64 \
	| racket infra/test-core2js.rkt -o ./tmp.js --error 150
	cat benchmarks/*.fpcore | racket tools/filter.rkt precision binary32 binary64 \
	| racket tools/filter.rkt operators "+" "-" "*" "/" fabs fma sqrt remainder fmax fmin trunc round nearbyint "<" ">" "<=" ">=" "==" "!=" and or not isfinite isinf isnan isnormal signbit \
	| racket infra/test-core2smtlib2.rkt

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt --canonicalize < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@
