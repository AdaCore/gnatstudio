
MAIN=kernel
MODULES=gnat gtkada common gvd src_editor vsearch prj_editor browsers
MODULE_INCLUDE+=-aI../../kernel/src
# This forces compilation of all files separately, without compiling the
# dependencies, which is just what we want.
NO_STYLE_FILES=glide_*.adb ../../src_editor/src/src_editor*.adb \
  ../../src_editor/src/src_highlighting.adb \
  ../../src_editor/src/src_editor_defaults.ads

include ../../Makefile.common
