
top: system.exe Makefile
	./$< +steps=20 +prog=prog/tight-count-loop.hex

system.exe: system.v Makefile
	iverilog -g2005-sv -s main $< -o $@
