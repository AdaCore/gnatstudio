CFLAGS = -g -O2

all:
	$(MAKE) CFLAGS="$(CFLAGS)" -C gnat
	$(MAKE) CFLAGS="$(CFLAGS)" -C tests
	$(MAKE) CFLAGS="$(CFLAGS)" -C odd

clean:
	$(MAKE) -C gnat clean
	$(MAKE) -C tests clean
	$(MAKE) -C odd clean
