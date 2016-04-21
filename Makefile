
core/%.core.fpbench: surface/%.surface.fpbench
	racket tools/surface2core.rkt < $^ > $@

c/%.c: core/%.core.fpbench
	racket tools/core2c.rkt < $^ > $@
