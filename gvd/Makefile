CFLAGS = -g -O2

all:
	$(MAKE) CFLAGS="$(CFLAGS)" -C pixmaps
	$(MAKE) CFLAGS="$(CFLAGS)" -C gnat
	$(MAKE) CFLAGS="$(CFLAGS)" -C gvd
	$(MAKE) CFLAGS="$(CFLAGS)" -C tests

clean:
	$(MAKE) -C pixmaps clean
	$(MAKE) -C gnat clean
	$(MAKE) -C gvd clean
	$(MAKE) -C tests clean
