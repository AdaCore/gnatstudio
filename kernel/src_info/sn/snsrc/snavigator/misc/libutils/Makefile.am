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

sdkdir = @SN_SDK_EXEC_PREFIX@/lib
sdk_LIBRARIES = libutils.a

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/db $(TCLHDIR)

if HAVE_REALPATH
REALPATH =
else
REALPATH = realpath.c
endif

if HAVE_GETOPT
GETOPT =
else
GETOPT = getopt.c
endif


libutils_a_SOURCES = homedir.c mxdefine.h mxfuncs.h \
fileutils.h fileutils.c mxlogger.h strs.c $(GETOPT) $(REALPATH)

