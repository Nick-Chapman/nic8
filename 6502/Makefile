
mains = first hello messages timer hannah sound1 sound2 arcadians sonic
shared_source = ticks lcd 76489

bins = $(patsubst %, rom/%.bin, $(mains))
libs = $(patsubst %, lib/%.s, $(shared_source))

all: $(bins)

rom/%.bin: %.s $(libs)
	vasm6502_oldstyle -I lib -dotdir -Fbin $< -o $@
