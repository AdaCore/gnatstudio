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

SUBDIRS =

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/misc/libutils -I$(top_srcdir)/hyper \
		-I$(DB_INCDIR) $(TCLHDIR)

if MSVC
LINK = link -DEBUG $(CFLAGS) $(LDFLAGS) -OUT:$@
else
CFLAGS += -Wall
LINK = $(PURIFY) $(CC) $(CFLAGS) $(LDFLAGS) -o $@
endif

sdkdir = $(exec_prefix)/lib

sdk_LIBRARIES = libdbutils.a

install-data-local:
	$(INSTALL_DATA) $(top_builddir)/../db/PORT/libdb.a $(sdkdir)/libpafdb.a
	$(RANLIB) $(sdkdir)/libpafdb.a

libdbutils_a_SOURCES = dbutils.c dbutils.h longstr.c srchtbl.c

