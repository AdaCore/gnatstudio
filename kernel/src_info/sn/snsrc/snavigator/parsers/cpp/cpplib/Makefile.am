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

INCLUDES = $(WINFLAGS) -I$(top_srcdir)/hyper -I$(top_srcdir)/dbutils \
	$(TCLHDIR)
	
noinst_LIBRARIES = libcpp2.a

libcpp2_a_SOURCES = cp_argument.c cp_class.c cp_crossref.c \
cp_declaration.c cp_declarator.c cp_enum.c cp_error.c cp_expression.c \
cp_init.c cp_list.c cp_literal.c \
cp_malloc.c cp_name.c cp_oper.c cp_process.c cp_stack.c cp_statement.c \
cp_step.c cp_symbol.c cp_type.c cp_typename.c cpdefines.h cplex.c \
cpkeyw.c cpkeyw.h crossref.h crossrefP.h operator.h symbol.h \
macread.c macro.c token.c expr.c process.c

