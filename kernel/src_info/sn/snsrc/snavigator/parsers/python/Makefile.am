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

## Process this file with automake to create .in.

toolbox = $(top_builddir)/parsers/toolbox
common = $(top_builddir)/parsers/common

bin_PROGRAMS = pybrowser

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/hyper \
	-I$(top_srcdir)/parsers/toolbox \
	-I$(top_srcdir)/parsers/common $(TCLHDIR)

pybrowser_LDADD = $(toolbox)/libsnptools.a $(top_builddir)/dbutils/libdbutils.a \
		 $(common)/libcommon.a $(DB_LIBDIR)/libdb.a @LEXLIB@ \
		 $(top_builddir)/misc/libutils/libutils.a \
		 $(TCL_BUILD_LIB_SPEC) $(TCL_LIBS)

pybrowser_SOURCES = pybrowser.l

pybrowser_DEPENDENCIES = $(top_builddir)/dbutils/libdbutils.a $(DB_LIBDIR)/libdb.a \
			$(toolbox)/libsnptools.a $(common)/libcommon.a \
			$(top_builddir)/misc/libutils/libutils.a

