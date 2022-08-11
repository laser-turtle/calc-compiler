.PHONY: all elf

elf:
	dune build
	./_build/default/bin/main.exe
	readelf -h -l my_exe
	chmod +x my_exe
