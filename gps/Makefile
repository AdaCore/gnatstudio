ifeq ($(OS),Windows_NT)
   LN = cp -p
else
   LN = ln -s
endif

GPRBUILD=gprbuild
GPRBUILD_FLAGS=

ifeq ($(Build),Production)
  GPRBUILD_BUILD_TYPE_FLAGS=-XBuild=Production -XGnatcoll_Build=Production
else
  GPRBUILD_BUILD_TYPE_FLAGS=-XBuild=Debug -XGnatcoll_Build=Debug
endif

.PHONY: default resources do_links all clean

default: resources do_links all

include ../Makefile.gnat

# NOTE: we need to build gnatcoll separately, since we cannot use
# gnat.adc (No_Tasking restriction) on all gnatcoll sources.
all:
	$(MAKE) -s LIBRARY_TYPE=static ENABLE_SHARED=no \
	  -C ../templates_parser setup
	$(MAKE) -s -C ../kernel/src_info/sn
	$(GPRBUILD) ${GPRBUILD_FLAGS} -p -ws -P../gnatlib/src/gnatcoll
	$(GPRBUILD) ${GPRBUILD_FLAGS} -p -ws -XTP_TASKING=No_Tasking \
	  ${GPRBUILD_BUILD_TYPE_FLAGS} -Pgps

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
	$(MAKE) -s -C ../common/expect
endif

do_links:
	-@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnat_src/$(f) ../gnat > /dev/null 2>&1 ;)
	@(cd ../gnat && gnatmake -q xsnamest && ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	$(MAKE) -s -C ../kernel/src_info/sn clean
	-gprclean -q -r -Pgps
