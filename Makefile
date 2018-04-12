.PHONY: test

test:
	racket infra/test-core2c.rkt benchmarks/*.fpcore
	racket infra/test-imp2core.rkt benchmarks/*.fpimp

RPATH=/var/www/fpbench/reports/$(shell date +%s)/

nightly:
	bash infra/test.sh
	rsync -r upload/ uwplse.org:$RPATH
	ssh uwplse.org chmod a+x $RPATH
	ssh uwplse.org chmod -R a+r $RPATH

%.fpcore: %.fpimp
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/imp2core.rkt --canonicalize < $^ >> $@

c/%.c: benchmarks/%.fpcore
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@

