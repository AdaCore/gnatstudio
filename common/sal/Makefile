#USAGE: CC=<compiler> JAVA_SDK=<path> TARGET=(win32|linux) make

main:
	cd obj; $(CC) -fPIC -I$(JAVA_SDK)/include -I$(JAVA_SDK)/include/$(TARGET) -c ../jni_wrappers.c 
	$(RM) lib/*gps_utils*
	gnatmake -Psal
