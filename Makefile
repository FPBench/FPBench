
all: table.html

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt --canonicalize < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@

table.html: $(wildcard benchmarks/*.fpcore)
	cat $^ | racket tools/bench-stats.rkt --format full-html > $@

test:
	racket test/core2c.rkt benchmarks/*.fpcore
	racket test/imp2core.rkt benchmarks/*.fpimp

.PHONY: all test
