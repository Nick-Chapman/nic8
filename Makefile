
top: regen-outs diff

progs = $(patsubst prog/%.hex, %, $(wildcard prog/*.hex))
outs = $(patsubst %, _gen/%.out, $(progs))

regen-outs: .regen-progs $(outs) Makefile

diff:
	git diff _gen

_gen/%.out: prog/%.hex simulation.exe Makefile
	vvp ./simulation.exe +steps=150 +prog=$< +verbose +change > $@

vs = $(wildcard verilog/*.v)

simulation.exe: $(vs)
	iverilog $^ -Wall -g2005-sv -s top -o $@ 2>&1 | diff /dev/null - || rm $@

.regen-progs: src/*.hs
	stack run
	touch .regen-progs
