
all: rom/hello.bin rom/messages.bin rom/timer.bin

rom/first.bin: first.s
	vasm6502_oldstyle -I lib -o rom/first.bin first.s -dotdir -Fbin

rom/hello.bin: hello.s
	vasm6502_oldstyle -I lib -o rom/hello.bin hello.s -dotdir -Fbin

rom/messages.bin: messages.s
	vasm6502_oldstyle -I lib -o rom/messages.bin messages.s -dotdir -Fbin

rom/timer.bin: timer.s
	vasm6502_oldstyle -I lib -o rom/timer.bin timer.s -dotdir -Fbin
