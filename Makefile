
top: regen-outs diff

progs = $(patsubst prog/%.hex, %, $(wildcard prog/*.hex))
traces = $(patsubst %, _gen/%.trace, $(progs))
outputs = $(patsubst %, _gen/%.out, $(progs))

regen-outs: .regen-progs $(traces) $(outputs) Makefile

diff:
	git diff _gen/*.out

_gen/%.trace: prog/%.hex simulation.exe Makefile
	vvp -n ./simulation.exe +steps=150 +prog=$< +verbose > $@

_gen/%.out: prog/%.hex simulation.exe Makefile
	vvp -n ./simulation.exe +steps=1500 +prog=$< > $@

vs = $(wildcard verilog/*.v)

simulation.exe: $(vs)
	iverilog $^ -Wall -g2005-sv -s top -o $@ 2>&1 | diff /dev/null - || rm $@

.regen-progs: src/*.hs
	stack run
	touch .regen-progs
