CFLAGS = -g -O2

all: pixmaps.o

pixmaps.o: pixmaps.c \
  breakat.xpm cont.xpm display.xpm down.xpm findfwd.xpm finish.xpm \
  interrupt.xpm kill.xpm lookup.xpm next.xpm nexti.xpm plot.xpm \
  print.xpm rotate.xpm run.xpm set.xpm show.xpm start.xpm step.xpm \
  stepi.xpm undisplay.xpm until.xpm up.xpm watch.xpm
	gcc -c $(CFLAGS) $<

clean:
	rm -f *.o *.ali *~ b~*
