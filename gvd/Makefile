CFLAGS = -g -O2

all:
	$(MAKE) CFLAGS="$(CFLAGS)" -C pixmaps
	$(MAKE) CFLAGS="$(CFLAGS)" -C gnat
	$(MAKE) CFLAGS="$(CFLAGS)" -C gvd
	$(MAKE) CFLAGS="$(CFLAGS)" -C tests
	$(MAKE) CFLAGS="$(CFLAGS)" -C docs

install:
	$(MAKE) -C gvd install
	$(MAKE) -C po install
	$(MAKE) -C docs install

clean:
	$(MAKE) -C pixmaps clean
	$(MAKE) -C gnat clean
	$(MAKE) -C gvd clean
	$(MAKE) -C tests clean
	$(MAKE) -C po clean
	$(MAKE) -C docs clean
