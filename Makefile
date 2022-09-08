
top: regen-outs diff

progs = $(patsubst prog/%.hex, %, $(wildcard prog/*.hex))
traces = $(patsubst %, _gen/%.trace, $(progs))

regen-outs: .regen-progs $(traces) Makefile

diff:
	git diff _gen

_gen/%.trace: prog/%.hex simulation.exe Makefile
	vvp ./simulation.exe +steps=150 +prog=$< +verbose +change > $@

vs = $(wildcard verilog/*.v)

simulation.exe: $(vs)
	iverilog $^ -Wall -g2005-sv -s top -o $@ 2>&1 | diff /dev/null - || rm $@

.regen-progs: src/*.hs
	stack run
	touch .regen-progs
