###############################################################################
#
# Copyright (C) 1997-1999 Cygnus Solutions, Inc.
#
# Description:
# Makefile for the ELF parser.
#
###############################################################################

SDK     = ../../..
LIBS	= -lm -ldl -L$(SDK)/lib -lsnptools -ldbutils -lpafdb -lutils -ltcl8.1
INCLUDES= -I$(SDK)/include

CC      = gcc
LEX	= flex

DBDIR	= .snprj
DB	= $(DBDIR)/test
BIN 	= $(SDK)/../../bin

all:	ebrowser

OBJECTS = ebrowser.o

ebrowser:	${OBJECTS}
		${CC} -o $@ ${OBJECTS} ${LIBS}

%.c:		%.l
		${LEX} $< && mv lex.yy.c $@

%.o:		%.c
		${CC} -c ${CFLAGS} ${DEFINES} ${INCLUDES} $<

clean:
		rm -rf ebrowser *.o .snprj sources

test:		ebrowser
		ls -1 *.elf > sources
		-mkdir $(DBDIR)
		./ebrowser -n $(DB) -y sources -p $(BIN)/dbimp
		@echo "Functions:"
		@$(BIN)/dbdump $(DB).fu
