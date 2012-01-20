ifeq ($(OS),Windows_NT)
   LN = cp -p
else
   LN = ln -s
endif

GPRBUILD=gprbuild
GPRBUILD_FLAGS=

Build?=Debug

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
	$(MAKE) -s Gnatcoll_Build=${Build} -C ../gnatlib do_links
	$(MAKE) -C ../gnatlib
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -ws -P../spark/spark
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -p -ws -XTP_TASKING=No_Tasking \
	  $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps

scil: do_links
	$(MAKE) -s Gnatcoll_Build=${Build} -C ../gnatlib do_links
	-gnatmake -k -j0 --subdirs=codepeer -gnatcC -m -p -ws \
	  -XTP_TASKING=No_Tasking $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
	$(MAKE) -s -C ../common/expect
endif

do_links:
	-@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnatlib/gnat_src/$(f) ../gnat > /dev/null 2>&1 ;)

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	-gprclean -q -r -Pgps
