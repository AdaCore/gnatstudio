# CPU limit
PROCESSORS ?= 0

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
ifeq ($(BUILD),Coverage)
  GPRBUILD_FLAGS=-j$(PROCESSORS) -m2 -p --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr
else
  GPRBUILD_FLAGS=-j$(PROCESSORS)
endif

PYTHON=python

BUILD?=Debug

ifeq ($(BUILD),Production)
  GPRBUILD_BUILD_TYPE_FLAGS=-XBUILD=Production -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable \
   -XBUILD_MODE=prod
else
  GPRBUILD_BUILD_TYPE_FLAGS=-XBUILD=${BUILD} -XOS=${OS} -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable \
   -XBUILD_MODE=dev
endif

.PHONY: default resources all clean

default: all

gps_and_cli: all
	$(MAKE) -C ../cli all

include ../Makefile.gnat

all: resources generate
	$(GPRBUILD) $(GPRBUILD_FLAGS) -m -p -ws \
	  $(GPRBUILD_BUILD_TYPE_FLAGS) -Pgps -largs `pkg-config gmodule-2.0 --libs`

# Generate some sources
generate:
	cd ../kernel/src; ${PYTHON} hooks.py
	cd ../kernel/generated; gnatcoll_db2ada \
          -api=GPS.Kernel.Properties.Database -adacreate -dbtype sqlite\
          -dbmodel=../src/properties_schema.txt
ifeq ($(OS),Windows_NT)
	for f in ../kernel/generated/*; do cat $$f | tr -d '\015' > $$f-aux; mv -f $$f-aux $$f; done
endif

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
endif

install:
	$(MAKE) -C .. install

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	-gprclean -q -r -Pgps
