build:
	gnatmake -gnatg -Icommon -Igtkada -Igvd -Ignat *.adb

distrib:
	@-mkdir gvd gnat > /dev/null 2>&1
	cp ../../common/src/sdefault.adb common
	cp ../../common/src/htables.ad? common
	cp ../../common/src/traces.ad? common
	cp ../../gtkada/src/glib-graphs.ad? gtkada
	cp ../../gvd/common/basic_types.ad? gvd
	cp ../../gvd/common/os_utils.ad? gvd
	cp ../../gvd/common/string_utils.ad? gvd
	cp ../../gnat/*.ad? gnat
