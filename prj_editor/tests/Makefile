
GNAT_SRC=../../../gnat/
RUNTIME_HEAD=${shell gnatls -v | grep "gcc-lib" | head -1}
RUNTIME_INC=${RUNTIME_HEAD:%=-aI%}
GNAT_INC=${RUNTIME_INC} -aI../src -aO../obj -aI${GNAT_SRC}

CFLAGS=-g
GNATMAKE_FLAGS=-gnaty -gnatwu -m ${CFLAGS} ${GNAT_INC}

all: test_api test_wizard

test_api: force
	gnatmake test_api.adb ${GNATMAKE_FLAGS}

test_wizard: force
	gnatmake test_wizard.adb ${GNATMAKE_FLAGS} `gtkada-config`

clean: force
	${RM} b~* *.o *.ali core test_api

force:
