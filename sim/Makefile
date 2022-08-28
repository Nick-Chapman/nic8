
top: system.exe
	./$^

system.exe: system.v
	iverilog -g2005-sv -s main $< -o $@
