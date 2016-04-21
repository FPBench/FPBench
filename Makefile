
core/%.core.fpbench: surface/%.surface.fpbench
	printf ";; -*- mode: scheme -*-\n\n" > $@
	racket tools/surface2core.rkt < $^ >> $@

c/%.c: core/%.core.fpbench
	printf "#include <tgmath.h>\n\n" > $@
	racket tools/core2c.rkt < $^ >> $@
