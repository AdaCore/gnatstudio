CFLAGS = -g -O0

all:
	$(MAKE) CFLAGS="$(CFLAGS)" -C pixmaps
	$(MAKE) CFLAGS="$(CFLAGS)" -C gnat
	$(MAKE) CFLAGS="$(CFLAGS)" -C odd
	$(MAKE) CFLAGS="$(CFLAGS)" -C tests

clean:
	$(MAKE) -C pixmaps clean
	$(MAKE) -C gnat clean
	$(MAKE) -C odd clean
	$(MAKE) -C tests clean
