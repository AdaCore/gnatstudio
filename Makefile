all build compile link ada c c++ clean :
	$(MAKE) -s -C glide -f Makefile.glide $@

gvd:
	$(MAKE) -s -C gvd -f Makefile.gvd EXEC=gvd ADA_SOURCES=gvd_main.adb

.PHONY: gvd
