SDK=/opt/cygnus/snavigator/sdk

CFLAGS= -I.
LIBS=	-L$(SDK)/lib -lpafdb

dbqry:	dbqry.c
	$(CC) $(CFLAGS) $< -o $@ $(LIBS)
