
# Dependencies:
#   prj_editor: gnat gtkada common gvd
#   hypergrep : gvd gtkada glide prj_editor gnat common vdiff aunit src_editor
#   vdiff     : gtkada
#   src_editor: common gtkada gvd
#   glide     : gtkada gvd prj_editor gnat common hypergrep vdiff aunit
#               src_editor
#   kernel    : 
#
# but glide depends on hypergrep and hypergrep depends on glide


# List of the modules and the order in which they should be recompiled

EXTERNAL_DEPENDENCIES=\
gtkada \
gvd \
common \
aunit

DEPENDENCIES= \
kernel \
src_editor \
vdiff \
prj_editor \
hypergrep \
glide



all: ${EXTERNAL_DEPENDENCIES} ${DEPENDENCIES}

clean: ${DEPENDENCIES:%=%_clean}


${DEPENDENCIES:%=%_clean}: force
	make -C ${@:%_clean=%}/src clean

${EXTERNAL_DEPENDENCIES}: force

${DEPENDENCIES}: force
	@echo "----------------------------------"
	@echo "--------- Compiling $@"
	@echo "----------------------------------"
	make -C ${@:%=%/src}

force:




