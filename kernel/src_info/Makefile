GNATMAKE_FLAGS=
LARGS = -aO../../gvd/gnat -largs sn/lib/db_capi.o -Lsn/snsrc/db/PORT -ldb ../../gvd/common/obj/gexpect_tty.o

build:
	cd standalone_obj; gcc -c ../common/a-file_utils.c
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone read_ali $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone read_many_ali $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone read_ali_twice $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone read_scope_tree $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone find_ref $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone find_dep $(LARGS)
	gnatmake ${GNATMAKE_FLAGS} -Pstandalone generate_db $(LARGS)

distrib:
	@-mkdir gvd gnat > /dev/null 2>&1
	cp ../../common/src/sdefault.adb common
	cp ../../common/src/file_utils.ad? common
	cp ../../common/src/a-file_utils.c common
	cp ../../gvd/common/htables.ad? common
	cp ../../gvd/common/string_hash.ad? common
	cp ../../gvd/common/traces.ad? common
	cp ../../gtkada/src/glib-graphs.ad? gtkada
	cp ../../gtkada/src/gtkada.ads gtkada
	cp ../../gtkada/src/gtkada-intl.ad? gtkada
	cp ../../gvd/common/basic_types.ad? gvd
	cp ../../gvd/common/os_utils.ad? gvd
	cp ../../gvd/common/string_utils.ad? gvd
	cp ../../gvd/common/language_handlers.ad? gvd
	cp ../../gvd/common/language-unknown.ad? gvd
	cp ../../gvd/common/language.ad? gvd
	cp ../../gvd/common/odd_intl.ad? gvd
	cp ../../gvd/gnat/g-exptty.ad? gvd
	cp ../../gvd/gnat/gexpect_tty.c gvd
	cp ../../gnat/*.ad? gnat

clean:
	@for i in standalone_obj/* ; do [ -f $$i ] && rm -f $$i || true; done
