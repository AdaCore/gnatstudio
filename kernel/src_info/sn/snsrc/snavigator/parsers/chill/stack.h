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
 * stack.h
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Routines for implementing a simple stack symbol table.
 */

#ifndef STACK_H
#define STACK_H

enum symtypes {
	dontcare
};

struct symbol {
	char * name;
	enum symtypes type;
};

/* Push data onto the stack.
   Returns: 0 if okay. */
int push_symbol(struct symbol sym);

/* Is a particular name on the stack?
   Returns: 0 if present, -1 otherwise. */
int find_symbol(char * name);

/* Destroy the entire stack in one swoop.
   Returns: 0 if okay. */
int destroy_stack();

#endif /* STACK_H */ 

