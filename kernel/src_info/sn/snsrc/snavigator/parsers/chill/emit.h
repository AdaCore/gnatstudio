/*

Copyright (c) 2000, Red Hat, Inc.

This file is part of Source-Navigator.

Source-Navigator is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2, or (at your option)
any later version.

Source-Navigator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along
with Source-Navigator; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.



*/

/*
 * emit.h
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Functions for emitting interesting constructs to the project database.
 */

#ifndef EMIT_H
#define EMIT_H

#ifdef DEBUG
#define emit(msg) printf("chill: %s\n", msg)
#define emitarg(msg, arg) printf("chill: %s (%s)\n", msg, arg)
#else
#define emit(msg)
#define emitarg(msg, arg)
#endif

#include "symtab.h"

extern int local;

int emit_module(struct identifier * id, struct blockregion block);

int emit_spec_module(struct identifier * id, struct blockregion block);

int emit_procedure(struct identifier * id, struct sig signature);

int emit_process(struct identifier * id, struct sig signature);

int emit_synonyms(struct identifierlist * idlist, char * type);

int emit_declarations(struct identifierlist * idlist, char * type);

int emit_type_synonyms(struct identifierlist * idlist, char * type);

int emit_structure(struct identifierlist * idlist, char * type);

int emit_enumeration();

int emit_enumeration_values(struct identifierlist * idlist);

int emit_include(char * filename, unsigned startline, unsigned startcol,
		 unsigned endline, unsigned endcol);

int emit_xref_assignment(struct identifierlist * idlist);

int emit_xref_procedure(char * procname, unsigned lineno);

int emit_xref_variable(char * varname, unsigned lineno);

#endif /* EMIT_H */

