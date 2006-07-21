#USAGE: make [CC=<compiler>] JAVA_SDK=<path>
CC=gcc
GNAT_RTS:=${shell gnatls -v | grep adalib | head -1}

ifeq ($(OS),Windows_NT)
	JAVA_OS_DIR=win32
        OUTPUT=gps_utils.dll
else
	MANAGE_SEGV_HANDLER=-D__MANAGE_SEGV_HANDLER__
        OUTPUT=libgps_utils.so

	ifeq (${shell uname}, SunOS)
		JAVA_OS_DIR=solaris
	else
		JAVA_OS_DIR=linux
	endif
endif

main:
	cd obj; $(CC) -fPIC -I$(JAVA_SDK)/include -I$(JAVA_SDK)/include/$(JAVA_OS_DIR) $(MANAGE_SEGV_HANDLER) -c ../jni_wrappers.c
	$(RM) obj/*gps_utils*
	gnatmake -Psal
	cd obj; \
	gnatbind -Lgps_utils jni.ali gnatbench-jni_functions.ali gnatbench.ali -o b~gps_utils.adb; \
	gcc -c b~gps_utils.adb -fPIC -O1 -Wall -g; \
	gcc -o my_lib.o *.o -Wl,-r -v -nostdlib; \
	gcc -shared my_lib.o -o $(OUTPUT) ${GNAT_RTS}/libgnat.a -lgcc -lgcc_eh
