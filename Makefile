.PHONY: test

test:
	racket infra/test-core2js.rkt -o ./tmp.js benchmarks/*.fpcore
	racket infra/test-core2c.rkt benchmarks/*.fpcore
	racket infra/test-imp2core.rkt benchmarks/*.fpimp

travis-test:
	racket infra/test-core2js.rkt -o ./tmp.js infra/triangleSorted.fpcore

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt --canonicalize < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@
