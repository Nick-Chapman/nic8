
mains = first hello messages timer hannah sound1 sound2 arcadians sonic computation pre-serial fib-trials forever-fib
shared_source = decimal fib1 fib2 fib3 fib4 fib5 fib6 fib7 gc lcd macs panic print screen sleep sound ticks via

bins = $(patsubst %, rom/%.bin, $(mains))
libs = $(patsubst %, lib/%.s, $(shared_source))

all: $(bins)

rom/%.bin: %.s $(libs)
	vasm6502_oldstyle -c02 -esc -I lib -Fbin $< -o $@
