CC=gcc

all: libgprcustom.so

libgprcustom.so: gprcustom.o
	$(CC) -shared -fPIC -o $@ $<

gprcustom.o: gprcustom.c
	$(CC) -c -fPIC gprcustom.c
