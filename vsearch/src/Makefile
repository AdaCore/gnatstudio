GVD_SOURCES=../../gvd/gvd
GVD_PIXMAPS=../../gvd/pixmaps

all: find_utils gui

find_utils:
	gnatmake -g -c -gnaty -gnata -aI${GVD_SOURCES} find_utils `gtkada-config`

gui: pixmaps.o
	gnatmake -g -aI${GVD_SOURCES} hypergrep `gtkada-config` -largs pixmaps.o

pixmaps.o: ${GVD_PIXMAPS}/pixmaps.c
	gcc -c ${GVD_PIXMAPS}/pixmaps.c
