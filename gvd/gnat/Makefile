# Generated automatically from Makefile.in by configure.
CFLAGS = -g -O2
ALL_CFLAGS=$(CFLAGS)  -DHAVE_GETPT=1 -DHAVE_SETPGID=1 -DHAVE_SETSID=1  -DSYSTEM_INCLUDE='"s/gnu-linux.h"'

all: g-expect.o gexpect.o g-regpat.o gexpect_tty.o g-exptty.o

g-expect.o: g-expect.adb g-expect.ads
	gcc -c -gnatg $(CFLAGS) $<

gexpect.o: gexpect.c
	gcc -c $(ALL_CFLAGS) $<

g-regpat.o: g-regpat.adb g-regpat.ads
	gcc -c -gnatg $(CFLAGS) $<

g-exptty.o: g-exptty.adb g-exptty.ads g-expect.ads
	gcc -c -gnatg $(CFLAGS) g-exptty.adb

gexpect_tty.o: gexpect_tty.c
	gcc -c $(ALL_CFLAGS) $<

clean:
	rm -f *.o *.ali *~ b~*


