###############################################################################
#
#
# Copyright (C) 1997-1999 Cygnus Solutions, Inc.
#
# Description:
# Makefile for the post-processed version of the lex specification for S-N's
# assembly parser.
#
###############################################################################

CAT=cat
CC=gcc
CHMOD=chmod
CP=cp
LEX=flex
M4=m4
MV=mv

SDK=../../../../sdk
BIN=../../../../../bin
INCLUDES=-I${SDK}/include
LIBS=-L${SDK}/lib -lm -ldl -lfl -lsnptools -ldbutils -lpafdb -ltcl8.1 -lutils

ASMS = a.s b.s
OBJS = abrowser.o
PROJDIR=example
PFLAGS=-r -c 300 -y sourcelist -n ${PROJDIR}/asm -p ${BIN}/dbimp

OUT = abrowser

all:	${OUT}

${OUT}:	${OBJS}
	${CC} -o $@ ${OBJS} ${LIBS}

macros.m4:
	@if [ "${TARGET}" != "" ] ;\
         then \
	   echo ${TARGET}.m4 "=>" $@ ;\
	   ${CP} ${TARGET}.m4 $@ ;\
	   ${CHMOD} +w $@ ;\
	 else \
	   echo "Run \`make TARGET=<target>' to build \`$@'." ;\
	   echo "Run \`make guess' to create \`guess.m4' if you need hints." ;\
	   exit 1 ;\
	 fi

guess:	${ASMS}	
	${SHELL} build-macros $@

abrowser.l:	abrowser.l.in macros.m4 toolbox.m4
	${M4} < abrowser.l.in > $@

%.s:	%.c
	${CC} -S $<

%.o:	%.c
	${CC} ${CFLAGS} ${INCLUDES} -o $@ -c $<

%.c:	%.l
	${LEX} $< 
	${MV} lex.yy.c $@

test:	abrowser sourcelist
	-mkdir ${PROJDIR}
	$< ${PFLAGS}

sourcelist:
	ls -1 *.s > $@

clean:
	rm -rf ${OUT}.l macros.m4 guess.m4 sourcelist ${PROJDIR} ${ASMS} ${OBJS} ${OUT}
