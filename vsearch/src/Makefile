GVD_SOURCES=../../gvd/gvd
all: find_utils gui

find_utils:
	gnatmake -c -gnaty -gnata find_utils -aI${GVD_SOURCES} `gtkada-config`

gui:
	gnatmake hypergrep -aI${GVD_SOURCES} `gtkada-config`
