#USAGE: make [CC=<compiler>] JAVA_SDK=<path>
CC=gcc

ifeq ($(OS),Windows_NT)
	JAVA_OS_DIR=win32
else
	MANAGE_SEGV_HANDLER=-D__MANAGE_SEGV_HANDLER__

	ifeq (${shell uname}, SunOS)
		JAVA_OS_DIR=solaris
	else
		JAVA_OS_DIR=linux
	endif
endif

main:
	cd obj; $(CC) -fPIC -I$(JAVA_SDK)/include -I$(JAVA_SDK)/include/$(JAVA_OS_DIR) $(MANAGE_SEGV_HANDLER) -c ../jni_wrappers.c 
	$(RM) lib/*gps_utils*
	gnatmake -Psal 
