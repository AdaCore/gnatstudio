GNATMAKE_FLAGS=
LARGS = -aO../../gvd/gnat -largs sn/lib/db_capi.o -Lsn/snsrc/db/PORT -ldb ../../gvd/common/obj/gexpect_tty.o

CP=cp -p

build:
	cd standalone_obj; gcc -c ../common/file-utils.c
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone read_ali $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone read_many_ali $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone read_ali_twice $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone read_scope_tree $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone find_ref $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone find_local_ref $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone find_dep $(LARGS)
	gnatmake -m -a ${GNATMAKE_FLAGS} -Pstandalone generate_db $(LARGS)

distrib:
	@-mkdir -p gvd gnat > /dev/null 2>&1
	$(CP) ../../common/src/sdefault.adb common
	$(CP) ../../gvd/common/file_utils.ad? common
	$(CP) ../../gvd/common/file-utils.c common
	$(CP) ../../gvd/common/htables.ad? common
	$(CP) ../../gvd/common/string_hash.ad? common
	$(CP) ../../gvd/common/traces.ad? common
	$(CP) ../../gtkada/src/glib-graphs.ad? gtkada
	$(CP) ../../gtkada/src/gtkada.ads gtkada
	$(CP) ../../gtkada/src/gtkada-intl.ad? gtkada
	$(CP) ../../gvd/common/basic_types.ad? gvd
	$(CP) ../../gvd/common/os_utils.ad? gvd
	$(CP) ../../gvd/common/vfs.ad? gvd
	$(CP) ../../gvd/common/string_utils.ad? gvd
	$(CP) ../../gvd/common/language_handlers.ad? gvd
	$(CP) ../../gvd/common/language-unknown.ad? gvd
	$(CP) ../../gvd/common/language.ad? gvd
	$(CP) ../../gvd/common/odd_intl.ad? gvd
	$(CP) ../../ada_module/src/src_info-* common
	$(CP) ../../cpp_module/src/src_info-* common
	$(CP) ../../gvd/gnat/g-exptty.ad? gvd
	$(CP) ../../gvd/gnat/g-tty.ad? gvd
	$(CP) ../../gvd/gnat/gexpect_tty.c gvd
	$(CP) ../../gnat/*.ad? gnat

clean:
	@for i in standalone_obj/* ; do [ -f $$i ] && rm -f $$i || true; done
force:

