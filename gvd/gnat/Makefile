CFLAGS = -g -O2

all: g-expect.o gexpect.o g-regpat.o

g-expect.o: g-expect.adb g-expect.ads
	gcc -c -gnatg $(CFLAGS) $<

gexpect.o: gexpect.c
	gcc -c $(CFLAGS) $<

g-regpat.o: g-regpat.adb g-regpat.ads
	gcc -c -gnatg $(CFLAGS) $<

clean:
	rm -f *.o *.ali *~ b~*
