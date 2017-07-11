
all: table.html

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/surface2core.rkt < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@

table.html: $(wildcard benchmarks/*.fpcore)
	cat $^ | racket tools/core2table.rkt > $@

test:
	racket test/compiler.rkt benchmarks/*.fpcore

.PHONY: all test
