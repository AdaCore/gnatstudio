CC=gcc

all: libgprcustom.so libgpr_custom.so

gnatlib = $(strip $(shell gnatls -v | grep adalib))

libgprcustom.so: gprcustom.o
	$(CC) -shared -fPIC -o $@ $<

libgpr_custom.so: gpr_custom.o
	$(CC) -shared -fPIC -o $@ $< -L$(gnatlib) -lgnat

gprcustom.o: gprcustom.c
	$(CC) -c -fPIC $<

gpr_custom.o: gpr_custom.adb
	$(CC) -c -fPIC $<

clean:
	$(RM) *.o *.so *.ali
