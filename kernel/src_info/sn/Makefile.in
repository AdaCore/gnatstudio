all: snsrc src

snsrc:
	$(MAKE) -C snsrc

src:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean
	$(MAKE) -C snsrc clean

distclean:
	$(RM) Makefile config.cache include/config.h config.log config.status
	$(MAKE) -C src distclean
	$(MAKE) -C snsrc distclean

.PHONY: snsrc src distclean
