
apps = $(patsubst app/%.s, %, $(wildcard app/*.s))

all: $(patsubst %, rom/%.bin, $(apps))

INC = -I lib -I data -I trials -I aoc-input -I cps
OPT = -c02 -esc -Fbin -wfail -maxerrors=0 -quiet -pic

rom/%.bin: app/%.s Makefile .list .deps
	@echo Building $<
	@vasm6502_oldstyle $< $(OPT) $(INC) -o $@ -L .list/$*.list -depend=make -depfile .deps/$*.dep || rm $@

.list: ; @mkdir -p $@
.deps: ; @mkdir -p $@

-include $(patsubst %, .deps/%.dep, $(apps))
