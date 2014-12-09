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

default: all

gps_and_cli: all
	$(MAKE) -C ../cli all

include ../Makefile.gnat

# NOTE: we need to build gnatcoll separately, since we cannot use
# gnat.adc (No_Tasking restriction) on all gnatcoll sources.
all: resources do_links
	$(MAKE) Gnatcoll_Build=${Build} -C ../gnatlib
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -ws -P../spark/spark
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -p -ws -XTP_TASKING=No_Tasking \
	  $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps -largs `pkg-config gmodule-2.0 --libs`

scil: do_links
	$(MAKE) -s Gnatcoll_Build=${Build} -C ../gnatlib do_links
	-gnatmake -q -k -j0 --subdirs=codepeer -gnatcC -m -p -ws \
	  -XTP_TASKING=No_Tasking $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
	$(MAKE) -s -C ../common/expect
endif

# If gnatlib/gnat_src exists, we use symbolic links to find the files we
# need, otherwise we assume that cron-src has put them in gnat/ already.
# Those few files are needed to generate support plug-ins for GPS.
do_links:
ifneq ($(wildcard ../gnatlib/gnat_src),)
	-@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnatlib/gnat_src/$(f) ../gnat > /dev/null 2>&1 ;)
endif

install:
	$(MAKE) -C .. install

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	-gprclean -q -r -Pgps
