all: cscmarshal.h cscmarshal.c

cscmarshal.h: cschtml.list
	glib-genmarshal --prefix=cschtml --header $< > $@

cscmarshal.c: cschtml.list
	glib-genmarshal --prefix=cschtml --body $< > $@

