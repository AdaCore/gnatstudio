include Makefile.common
include Makefile.gnat

PROJECT_FILE = glide/glide.gpr
EXEC         = glide2
ADA_SOURCES  = glide2.adb

LN = ln -s -f

default: do_links all

do_links: force
	@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnat_src/$(f) gnat > /dev/null ;)

include $(ROOT)/builder/src/Makefile.glide

