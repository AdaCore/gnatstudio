## Process this file with automake to produce Makefile.in

## TMPDIR can be used to control where temp files are created during a
## "make check".  Be careful to not initialize it to "" (/) by
## default.  Some build tools (namely BSD ar) also use TMPDIR for
## scratch files.
#TMPDIR =

INCLUDES = -I$(srcdir)/../include -I$(srcdir)/include -D__DBINTERFACE_PRIVATE \
$(WINFLAGS)

# ################################################################
#
# libdb
#

lib_LIBRARIES = libdb.a

H_OBJS = hash.$(OBJEXT) hash_bigkey.$(OBJEXT) hash_buf.$(OBJEXT) \
hash_func.$(OBJEXT) hash_log2.$(OBJEXT) hash_page.$(OBJEXT) \
hsearch.$(OBJEXT) ndbm.$(OBJEXT)

B_OBJS = bt_close.$(OBJEXT) bt_conv.$(OBJEXT) bt_debug.$(OBJEXT) \
bt_delete.$(OBJEXT) bt_get.$(OBJEXT) bt_open.$(OBJEXT) \
bt_overflow.$(OBJEXT) bt_page.$(OBJEXT) bt_put.$(OBJEXT) \
bt_search.$(OBJEXT) bt_seq.$(OBJEXT) bt_split.$(OBJEXT) \
bt_utils.$(OBJEXT)

D_OBJS = db.$(OBJEXT)

M_OBJS = mpool.$(OBJEXT)

R_OBJS = rec_close.$(OBJEXT) rec_delete.$(OBJEXT) rec_get.$(OBJEXT) \
rec_open.$(OBJEXT) rec_put.$(OBJEXT) rec_search.$(OBJEXT) \
rec_seq.$(OBJEXT) rec_utils.$(OBJEXT)

libdb_a_SOURCES = @HACK@
EXTRA_libdb_a_SOURCES = hack.c
libdb_a_LIBADD = @LIBOBJS@ $(H_OBJS) $(B_OBJS) $(D_OBJS) $(M_OBJS) $(R_OBJS)
libdb_a_DEPENDENCIES = $(libdb_a_LIBADD)

# ################################################################
#
# The test program.
#

check_PROGRAMS = dbtest

dbtest_SOURCES = @HACK@
dbtest_LDADD = -L. $(dbtest_DEPENDENCIES)
dbtest_DEPENDENCIES = dbtest.$(OBJEXT) @LIBOBJS@ -ldb

check-local:
	srcdir=$(srcdir); export srcdir; \
	if test -z "$(TMPDIR)"; then TMPDIR=`pwd`; \
	else TMPDIR="$(TMPDIR)"; fi; \
	$(FCTSH) $(top_srcdir)/test/run.test

# ################################################################
#
# All the compilation rules.  Normally you don't have to do this with
# automake.
#

hash.$(OBJEXT): ../hash/hash.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash.c
hash_bigkey.$(OBJEXT): ../hash/hash_bigkey.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash_bigkey.c
hash_buf.$(OBJEXT): ../hash/hash_buf.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash_buf.c
hash_func.$(OBJEXT): ../hash/hash_func.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash_func.c
hash_log2.$(OBJEXT): ../hash/hash_log2.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash_log2.c
hash_page.$(OBJEXT): ../hash/hash_page.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hash_page.c
hsearch.$(OBJEXT): ../hash/hsearch.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/hsearch.c
ndbm.$(OBJEXT): ../hash/ndbm.c
	$(COMPILE) -c -I$(srcdir)/../hash $(srcdir)/../hash/ndbm.c

bt_close.$(OBJEXT): ../btree/bt_close.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_close.c
bt_conv.$(OBJEXT): ../btree/bt_conv.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_conv.c
bt_debug.$(OBJEXT): ../btree/bt_debug.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_debug.c
bt_delete.$(OBJEXT): ../btree/bt_delete.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_delete.c
bt_get.$(OBJEXT): ../btree/bt_get.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_get.c
bt_open.$(OBJEXT): ../btree/bt_open.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_open.c
bt_overflow.$(OBJEXT): ../btree/bt_overflow.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_overflow.c
bt_page.$(OBJEXT): ../btree/bt_page.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_page.c
bt_put.$(OBJEXT): ../btree/bt_put.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_put.c
bt_search.$(OBJEXT): ../btree/bt_search.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_search.c
bt_seq.$(OBJEXT): ../btree/bt_seq.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_seq.c
bt_split.$(OBJEXT): ../btree/bt_split.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_split.c
bt_stack.$(OBJEXT): ../btree/bt_stack.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_stack.c
bt_utils.$(OBJEXT): ../btree/bt_utils.c
	$(COMPILE) -c -I$(srcdir)/../btree $(srcdir)/../btree/bt_utils.c

db.$(OBJEXT): ../db/db.c
	$(COMPILE) -c $(srcdir)/../db/db.c

mpool.$(OBJEXT): ../mpool/mpool.c
	$(COMPILE) -c -I$(srcdir)/../mpool $(srcdir)/../mpool/mpool.c

rec_close.$(OBJEXT): ../recno/rec_close.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_close.c
rec_delete.$(OBJEXT): ../recno/rec_delete.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_delete.c
rec_get.$(OBJEXT): ../recno/rec_get.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_get.c
rec_open.$(OBJEXT): ../recno/rec_open.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_open.c
rec_put.$(OBJEXT): ../recno/rec_put.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_put.c
rec_search.$(OBJEXT): ../recno/rec_search.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_search.c
rec_seq.$(OBJEXT): ../recno/rec_seq.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_seq.c
rec_utils.$(OBJEXT): ../recno/rec_utils.c
	$(COMPILE) -c -I$(srcdir)/../recno $(srcdir)/../recno/rec_utils.c

dbtest.$(OBJEXT): ../test/dbtest.c
	$(COMPILE) -c $(srcdir)/../test/dbtest.c

memmove.$(OBJEXT): memmove.c
	$(COMPILE) -DMEMMOVE -c $(srcdir)/memmove.c
mktemp.$(OBJEXT): mktemp.c
	$(COMPILE) -c $(srcdir)/mktemp.c
snprintf.$(OBJEXT): snprintf.c
	$(COMPILE) -c $(srcdir)/snprintf.c

