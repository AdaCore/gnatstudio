
GNAT_SRC=../../gnat
RUNTIME_HEAD=${shell gnatls -v | grep "gcc-lib" | head -1}
RUNTIME_INC=${RUNTIME_HEAD:%=-aI%}
GNAT_INC=${RUNTIME_INC} -aI../../common -aI../src -aO../obj -aI${GNAT_SRC} 

CFLAGS=-g
GNATMAKE_FLAGS=-gnaty -gnatwu -m ${CFLAGS} ${GNAT_INC} 

all: test_api test_wizard test_directory

test_api: force
	gnatmake test_api.adb ${GNATMAKE_FLAGS}

test_wizard test_directory: force
	gnatmake $@.adb ${GNATMAKE_FLAGS} `gtkada-config` \
	  -largs ../obj/pixmaps.o -bargs -E

clean: force
	${RM} b~* *.o *.ali core test_api test_directory test_wizard

force:
