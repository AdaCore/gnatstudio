include Makefile.gnat

LN = ln -s -f

# List of the modules and the order in which they should be recompiled

EXTERNAL_DEPENDENCIES=\
gtkada \
gvd

DEPENDENCIES= \
kbody \
common \
syntax \
vdiff \
aunit \
prj_editor \
src_editor \
hypergrep \
glide \
widgets

all: do_links ${EXTERNAL_DEPENDENCIES} ${DEPENDENCIES}

clean: ${DEPENDENCIES:%=%_clean}

do_links: force
	@$(foreach f,$(GNAT_SOURCES), \
	  $(LN) ../gnat_src/$(f) gnat > /dev/null ;)

${DEPENDENCIES:%=%_clean}: force
	make -C ${@:%_clean=%}/src clean

${EXTERNAL_DEPENDENCIES}: force

gvd: force
	@echo "----------------------------------"
	@echo "--------- Compiling $@"
	@echo "----------------------------------"
	make -C gvd/pixmaps
	make -C gvd/gnat
	make -C gvd/gvd

${DEPENDENCIES}: force
	@echo "----------------------------------"
	@echo "--------- Compiling $@"
	@echo "----------------------------------"
	make -C ${@:%=%/src}

force:

