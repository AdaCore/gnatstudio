
# Location of GNAT sources
GNAT_SRC=../../../gnat/

# Including the gnat runtime. This must be found before the gnat sources on the
# path, or the binder will complain that sources are more recent than objects
RUNTIME_HEAD=${shell gnatls -v | grep "gcc-lib" | head -1}
RUNTIME_INC=${RUNTIME_HEAD:%=-aI%}

GNAT_INC=${RUNTIME_INC} -aI${GNAT_SRC}

CFLAGS=-g
GNATMAKE_FLAGS=-gnaty -gnatwu -gnata -m ${CFLAGS} `gtkada-config`


.PHONY: all check_gnat force

all: check_gnat ../obj/pixmaps.o
	cd ../obj; gnatmake -aI../src ${GNAT_INC} lib_make.adb \
	  ${GNATMAKE_FLAGS} -largs pixmaps.o

../obj/pixmaps.o: pixmaps.c logo.xpm
	cd ../obj; gcc -c ../src/pixmaps.c -I../src

check_gnat: force
	@[ -f ${GNAT_SRC}/prj.ads ] || \
	   (echo "GNAT sources not found in ${GNAT_SRC}"; false)

clean: force
	cd ../obj; ${RM} b~* *.o *.ali core wizard

force:
