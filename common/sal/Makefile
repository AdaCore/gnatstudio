#USAGE: make [CC=<compiler>] JAVA_SDK=<path>
CC=gcc

ifeq ($(OS),Windows_NT)
	JAVA_OS_DIR=win32
else
	JAVA_OS_DIR=linux
endif

main:
	cd obj; $(CC) -fPIC -I$(JAVA_SDK)/include -I$(JAVA_SDK)/include/$(JAVA_OS_DIR) -c ../jni_wrappers.c 
	$(RM) lib/*gps_utils*
	gnatmake -Psal 
