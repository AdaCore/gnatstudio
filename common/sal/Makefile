#USAGE: make [CC=<compiler>] JAVA_SDK=<path>
CC=gcc
GNAT_RTS:=$(shell gnatls -v | grep adalib | sed -e "s/^[ ]*//" | head -1)
GNAT_BASE_PATH=$(shell which gnat | sed -e "s/bin\/gnat//")

ifeq ($(OS),Windows_NT)
	JAVA_OS_DIR=win32
	OUTPUT=gps_utils.dll
	GCCLIBS=-lgcc
	FPIC=
	NOSTDLIB=
	LIBGNAT="$(GNAT_RTS)/libgnat.a"
else
	MANAGE_SEGV_HANDLER=-D__MANAGE_SEGV_HANDLER__
	OUTPUT=libgps_utils.so
	GCCLIBS=-lgcc -lgcc_eh
	FPIC=-fPIC
	NOSTDLIB=-nostdlib
	LIBGNAT=adalib/libgnat.a

	ifeq (${shell uname}, SunOS)
		JAVA_OS_DIR=solaris
	else
		JAVA_OS_DIR=linux
	endif
endif
main:
	$(RM) obj/*gps_utils*
ifneq ($(OS),Windows_NT)
	# On linux / solaris, we have to recompile libgnat in order to ensure
	# that it is compiled with -fPIC. We don't do that on windows, since
	# -fPIC is irrelevant there.
	mkdir adalib
	cp $(GNAT_RTS)/Makefile.adalib adalib
	$(MAKE) -C adalib -f Makefile.adalib ROOT="$(GNAT_BASE_PATH)" CFLAGS="-O2 -fPIC"
endif
	$(CC) $(FPIC) -I$(JAVA_SDK)/include -I$(JAVA_SDK)/include/$(JAVA_OS_DIR) $(MANAGE_SEGV_HANDLER) -c jni_wrappers.c -o obj/jni_wrappers.o
	gnatmake -c $(FPIC) -Psal
	cd obj; gnatbind -o b~gps_utils.adb -Lgps_utils jni.ali gnatbench-jni_functions.ali gnatbench.ali
	gcc -c $(FPIC) -O -g obj/b~gps_utils.adb -o obj/b~gps_utils.o
	gcc -o obj/my_lib.a obj/*.o -Wl,-r -v -nostdlib $(LIBGNAT) $(GCCLIBS)
	gcc -v -shared $(NOSTDLIB) -o lib/$(OUTPUT) obj/my_lib.a
