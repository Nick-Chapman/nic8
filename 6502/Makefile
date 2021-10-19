
all: rom/hello.bin rom/messages.bin rom/timer.bin rom/hannah.bin rom/sound1.bin rom/sound2.bin rom/arcadians.bin

rom/first.bin: first.s
	vasm6502_oldstyle -I lib -o rom/first.bin first.s -dotdir -Fbin

rom/hello.bin: hello.s lib/lcd.s
	vasm6502_oldstyle -I lib -o rom/hello.bin hello.s -dotdir -Fbin

rom/messages.bin: messages.s lib/lcd.s
	vasm6502_oldstyle -I lib -o rom/messages.bin messages.s -dotdir -Fbin

rom/timer.bin: timer.s lib/lcd.s lib/ticks.s
	vasm6502_oldstyle -I lib -o rom/timer.bin timer.s -dotdir -Fbin

rom/hannah.bin: hannah.s lib/lcd.s lib/ticks.s
	vasm6502_oldstyle -I lib -o rom/hannah.bin hannah.s -dotdir -Fbin

rom/sound1.bin: sound1.s lib/ticks.s
	vasm6502_oldstyle -I lib -o rom/sound1.bin sound1.s -dotdir -Fbin

rom/sound2.bin: sound2.s lib/ticks.s lib/lcd.s
	vasm6502_oldstyle -I lib -o rom/sound2.bin sound2.s -dotdir -Fbin

rom/arcadians.bin: arcadians.s lib/ticks.s arc-data.s
	vasm6502_oldstyle -I lib -o rom/arcadians.bin arcadians.s -dotdir -Fbin
