build:
	gnatmake -Pstandalone read_ali
	gnatmake -Pstandalone read_scope_tree
	gnatmake -Pstandalone find_ref
	gnatmake -Pstandalone find_dep

distrib:
	@-mkdir gvd gnat > /dev/null 2>&1
	cp ../../common/src/sdefault.adb common
	cp ../../common/src/htables.ad? common
	cp ../../common/src/string_hash.ad? common
	cp ../../common/src/traces.ad? common
	cp ../../gtkada/src/glib-graphs.ad? gtkada
	cp ../../gvd/common/basic_types.ad? gvd
	cp ../../gvd/common/os_utils.ad? gvd
	cp ../../gvd/common/string_utils.ad? gvd
	cp ../../gvd/common/language_handlers.ad? gvd
	cp ../../gvd/common/language.ad? gvd
	cp ../../gnat/*.ad? gnat

clean:
	rm -f standalone_obj/*
