
all: table.html

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@

table.html: $(wildcard benchmarks/*.fpcore)
	cat $^ | racket tools/core2table.rkt > $@

test:
	racket test/core2c.rkt benchmarks/*.fpcore
	racket test/imp2core.rkt benchmarks/*.fpimp

.PHONY: all test
