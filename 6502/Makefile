
apps = first hello messages timer hannah sound1 sound2 arcadians sonic computation pre-serial fib-trials forever-fib try-nmi
libs = decimal gc lcd macs panic print screen sleep sound ticks via

all: $(patsubst %, rom/%.bin, $(apps))

# TODO: auto compute the dep on shared source
shared = $(patsubst %, lib/%.s, $(libs))
trials = $(patsubst %, trials/%.s, $(fib1 fib2 fib3 fib4 fib5 fib6 fib7))

rom/arcadians.bin : data/arc-data.s
rom/fib-trials.bin : trials

rom/%.bin: app/%.s $(shared)
	vasm6502_oldstyle -c02 -esc -I lib -I data -I trials -Fbin $< -o $@
