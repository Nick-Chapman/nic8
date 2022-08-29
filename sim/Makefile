
top: reg diff

progs = $(patsubst prog/%.hex, %, $(wildcard prog/*.hex))
outs = $(patsubst %, reg/%.out, $(progs))

reg: $(outs) Makefile

diff:
	git diff reg


reg/%.out: prog/%.hex system.exe Makefile
	./system.exe +steps=150 +prog=$< > $@

dev: system.exe Makefile
	./$< +steps=20 +prog=prog/tight-count-loop.hex


system.exe: system.v Makefile
	iverilog -g2005-sv -s main $< -o $@
