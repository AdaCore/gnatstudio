GVD_SOURCES=../../gvd/gvd
GVD_PIXMAPS=../../gvd/pixmaps

all: find_utils gui

find_utils:
	gnatmake -O2 -g -gnaty -c -gnata -aI${GVD_SOURCES} find_utils `gtkada-config`

gui: pixmaps.o
	gnatmake -O2 -g -gnaty -aI${GVD_SOURCES} hypergrep_main `gtkada-config` -largs pixmaps.o

pixmaps.o: ${GVD_PIXMAPS}/pixmaps.c
	gcc -c ${GVD_PIXMAPS}/pixmaps.c
