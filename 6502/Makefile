
mains = first hello messages timer hannah sound1 sound2 arcadians sonic computation pre-serial fib
shared_source = via ticks lcd sound screen sleep decimal fib1 fib2

bins = $(patsubst %, rom/%.bin, $(mains))
libs = $(patsubst %, lib/%.s, $(shared_source))

all: $(bins)

rom/%.bin: %.s $(libs)
	vasm6502_oldstyle -I lib -dotdir -Fbin $< -o $@
