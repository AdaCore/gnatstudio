# Copyright (c) 2000, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

YFLAGS=-d

toolbox = $(top_builddir)/sdk/parsers/toolbox
common = $(top_builddir)/parsers/common

noinst_PROGRAMS = chbrowser

INCLUDES = -I$(top_srcdir)/hyper -I$(top_srcdir)/sdk/parsers/toolbox \
		-I$(top_srcdir)/parsers/common

chbrowser_SOURCES = parser.y lexer.l common.h common.c emit.h emit.c error.c \
			main.c symtab.h symtab.c stack.h stack.c

chbrowser_LDADD = -L$(toolbox) -lsnptools \
		$(top_builddir)/db/libdbutils.a \
		 -L$(DB_LIBDIR) -ldb $(common)/libcommon.a @LEXLIB@ \
		$(top_builddir)/misc/libutils/libutils.a \
		$(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)

chbrowser_DEPENDENCIES = $(top_builddir)/db/libdbutils.a $(DB_LIBDIR)/libdb.a \
			$(top_builddir)/misc/libutils/libutils.a \
			$(toolbox)/libsnptools.a $(common)/libcommon.a

