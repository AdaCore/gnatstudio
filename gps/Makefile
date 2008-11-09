ifeq ($(OS),Windows_NT)
   LN = cp -p
else
   LN = ln -s
endif

GPRBUILD=gprbuild

.PHONY: default resources do_links all clean

default: resources do_links all

include ../Makefile.gnat

all:
	$(MAKE) -s LIBRARY_TYPE=static ENABLE_SHARED=no \
	  -C ../templates_parser setup
	$(MAKE) -s -C ../kernel/src_info/sn
	$(GPRBUILD) -p -ws -XTP_TASKING=No_Tasking -Pgps

resources:
ifeq ($(OS),Windows_NT)
	@cd src; windres gps.rc -O coff -o ../obj/gps.res
	$(MAKE) -s -C ../common/expect
endif

do_links:
	-@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnat_src/$(f) ../gnat > /dev/null 2>&1 ;)
	@sed -e 's/GNAT.Command_Line/GComLin/g' ../gnat_src/g-comlin.ads > ../gnat/gcomlin.ads
	@sed -e 's/GNAT.Command_Line/GComLin/g' ../gnat_src/g-comlin.adb > ../gnat/gcomlin.adb
	@(cd ../gnat && gnatmake -q xsnamest && ./xsnamest && mv snames.ns snames.ads && mv snames.nb snames.adb)

clean:
ifeq ($(OS),Windows_NT)
	$(MAKE) -s -C ../common/expect clean
endif
	$(MAKE) -s -C ../kernel/src_info/sn clean
	-gprclean -q -r -Pgps

