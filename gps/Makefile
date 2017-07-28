SYS := $(shell gcc -dumpmachine)
ifeq ($(OS),Windows_NT)
	OS := Windows_NT
else
	ifneq (, $(findstring darwin, $(SYS)))
OS := osx
else
	OS := unix
endif
endif

ifeq ($(OS),Windows_NT)
   LN = cp -p
else
   LN = ln -s
endif

GPRBUILD=gprbuild
GPRBUILD_FLAGS=
PYTHON=python

Build?=Debug

ifeq ($(Build),Production)
  GPRBUILD_BUILD_TYPE_FLAGS=-XBuild=Production -XGnatcoll_Build=Production -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
else
  GPRBUILD_BUILD_TYPE_FLAGS=-XBuild=Debug -XGnatcoll_Build=Debug -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable
endif

.PHONY: default resources do_links all clean

default: all

gps_and_cli: all
	$(MAKE) -C ../cli all

include ../Makefile.gnat

# NOTE: we need to build gnatcoll separately, since we cannot use
# gnat.adc (No_Tasking restriction) on all gnatcoll sources.
all: resources do_links
	cd ../kernel/src; ${PYTHON} hooks.py
	cd ../kernel/generated; gnatcoll_db2ada -api=GPS.Kernel.Properties.Database -adacreate -dbmodel=../src/properties_schema.txt
ifeq ($(OS),Windows_NT)
	for f in ../kernel/generated/*; do cat $$f | tr -d '\015' > $$f-aux; mv -f $$f-aux $$f; done
endif
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -p -ws \
	  $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps -largs `pkg-config gmodule-2.0 --libs`

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
	$(MAKE) -s -C ../common/expect
endif

# If gnatlib/gnat_src exists, we use symbolic links to find the files we
# need, otherwise we assume that cron-src has put them in gnat/ already.
# Those few files are needed to generate support plug-ins for GPS.
do_links:
ifneq ($(wildcard ../gnat_src),)
	-@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnat_src/$(f) ../gnat > /dev/null 2>&1 ;)
endif

install:
	$(MAKE) -C .. install

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	-gprclean -q -r -Pgps
