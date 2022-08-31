
top: reg diff

progs = $(patsubst prog/%.hex, %, $(wildcard prog/*.hex))
outs = $(patsubst %, reg/%.out, $(progs))

reg: $(outs) Makefile

diff:
	git diff reg

reg/%.out: prog/%.hex top.exe Makefile
	vvp ./top.exe +steps=150 +prog=$< > $@

dev: top.exe Makefile
	vvp $< +steps=30 +prog=prog/tight-count-loop.hex

vs = $(wildcard *.v design/*.v)

top.exe: $(vs)
	iverilog $^ -Wall -g2005-sv -s main -o $@ 2>&1 | diff /dev/null - || rm $@
