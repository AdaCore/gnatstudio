
# CFLAGS can be overriden on the command line if you need to pass special
# switches to the compiler, binder or linker
CFLAGS=-g

.PHONY: all force

all: force
	@make -C src CFLAGS="${CFLAGS}" all
	@make -C tests CFLAGS="${CFLAGS}" all

clean: force
	@make -C src clean
	@make -C tests clean

force:
