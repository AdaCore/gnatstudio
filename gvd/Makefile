CFLAGS = -g -O2

subdirs= pixmaps gnat gvd po tests docs


all:
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done

install: 
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done

clean-generic:
	-$(RM) core
	-$(RM) .\#*
clean: clean-generic
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done

mostlyclean-generic: clean-generic

mostlyclean: mostlyclean-generic
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done

distclean-generic: mostlyclean-generic
	-$(RM) config.cache config.log config.status

distclean: distclean-generic
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done
	
maintainer-clean: distclean-generic
	-$(RM) configure
	for subdir in $(subdirs); do \
	   $(MAKE) CFLAGS="$(CFLAGS)" -C $$subdir $@; \
	done
