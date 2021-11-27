
mains = first hello messages timer hannah sound1 sound2 arcadians sonic computation pre-serial fib-trials forever-fib
libs = decimal gc lcd macs panic print screen sleep sound ticks via
trials = fib1 fib2 fib3 fib4 fib5 fib6 fib7

bins = $(patsubst %, rom/%.bin, $(mains))
shared = $(patsubst %, lib/%.s, $(libs)) $(patsubst %, trials/%.s, $(trials))

all: $(bins)

rom/%.bin: %.s $(shared)
	vasm6502_oldstyle -c02 -esc -I lib -I trials -Fbin $< -o $@
