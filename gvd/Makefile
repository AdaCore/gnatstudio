CFLAGS = -g -O2

subdirs= pixmaps gnat gvd po tests docs

install_subdirs=${subdirs:%=install_%}
clean_subdirs=${subdirs:%=clean_%}
mostly_clean_subdirs=${subdirs:%=mclean_%}
distclean_subdirs=${subdirs:%=distclean_%}
maintainer_clean_subdirs=${subdirs:%=maintainer_clean_%}

.PHONY: all install clean mostlyclean distclean maintainer_clean

all: ${subdirs}
${subdirs}: force
	$(MAKE) CFLAGS="$(CFLAGS)" -C $@

install: ${install_subdirs}
${install_subdirs}: force
	$(MAKE) -C ${@:install_%=%} install

clean-generic: force
	-$(RM) core
	-$(RM) .\#*
clean: ${clean_subdirs} force
${clean_subdirs}: force
	${MAKE} -C ${@:clean_%=%} clean

mostlyclean-generic: clean-generic
mostlyclean: mostlyclean-generic ${mostly_clean_subdirs}
${mostly_clean_subdirs}: force
	${MAKE} -C ${@:mclean_%=%} mostlyclean

distclean-generic: mostlyclean-generic force
	-$(RM) config.cache config.log config.status
distclean: distclean-generic ${distclean_subdirs}
${distclean_subdirs}: force
	${MAKE} -C ${@:distclean_%=%} distclean

maintainer-clean: distclean-generic ${maintainer_clean_subdirs} force
	-$(RM) configure
${maintainer_clean_subdirs}: force
	${MAKE} -C ${@:maintainer_clean_%=%} maintainer_clean

force:
