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
 * stack.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Implementation of routines for implementing a simple stack symbol table.
 */

#include <stddef.h>
#include <stdlib.h>

#include <tcl.h>

#include "stack.h"

struct stacknode {
	struct symbol sym;
	struct stacknode * next;
};

static struct stacknode * base = NULL;
static struct stacknode * current = NULL; 

int
push_symbol(struct symbol sym)
{
	int first_run = 0;
	struct stacknode ** ptr;

	if (base == NULL)
	{
		ptr = &base;
		first_run = 1;
	}
	else
	{
		ptr = &current->next;
	}

	*ptr = ckalloc(sizeof(struct stacknode));
	if (*ptr == NULL)
	{
		return -1;
	}
	(*ptr)->sym.name = ckalloc(strlen(sym.name) + 1);
	(void) strcpy((*ptr)->sym.name, sym.name);
	(*ptr)->sym.type = dontcare;
	(*ptr)->next = NULL;

	/* Cater for the initial condition. */
	current = (first_run) ? base : current->next;

	return 0;
}

int
find_symbol(char * name)
{
	struct stacknode * ptr = base;
	
	while (ptr != NULL)
	{
		if (strcmp(ptr->sym.name, name) == 0)
		{
			return 1;
		}
		ptr = ptr->next;
	}
	return 0;
}	

static int
recursively_destroy_stack(struct stacknode * ptr)
{
	struct stacknode * next;

	if (ptr == NULL)
	{
		return 0;
	}
	else
	{
		next = ptr->next;
		ckfree(ptr->sym.name);
		ckfree(ptr);
		return recursively_destroy_stack(next);
	}
}

int
destroy_stack()
{
  return recursively_destroy_stack(base);
}

