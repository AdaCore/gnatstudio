LARGS = -largs sn/lib/db_capi.o -Lsn/snsrc/db/PORT -ldb

build:
	gnatmake -Pstandalone read_ali $(LARGS)
	gnatmake -Pstandalone read_many_ali $(LARGS)
	gnatmake -Pstandalone read_ali_twice $(LARGS)
	gnatmake -Pstandalone read_scope_tree $(LARGS)
	gnatmake -Pstandalone find_ref $(LARGS)
	gnatmake -Pstandalone find_dep $(LARGS)
	gnatmake -Pstandalone generate_db $(LARGS)

distrib:
	@-mkdir gvd gnat > /dev/null 2>&1
	cp ../../common/src/sdefault.adb common
	cp ../../common/src/file_utils.ad? common
	cp ../../gvd/common/htables.ad? common
	cp ../../gvd/common/string_hash.ad? common
	cp ../../common/src/traces.ad? common
	cp ../../gtkada/src/glib-graphs.ad? gtkada
	cp ../../gvd/common/basic_types.ad? gvd
	cp ../../gvd/common/os_utils.ad? gvd
	cp ../../gvd/common/string_utils.ad? gvd
	cp ../../gvd/common/language_handlers.ad? gvd
	cp ../../gvd/common/language-unknown.ad? gvd
	cp ../../gvd/common/language.ad? gvd
	cp ../../gnat/*.ad? gnat

clean:
	@for i in standalone_obj/* ; do [ -f $$i ] && rm -f $$i || true; done
