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

#include <stdlib.h>
#include <stdio.h>

#include "stack.h"
#include "symtab.h"

static struct symbol s;

static int with = 0;
static char with_name[512];

static char module_name[512];
static char proc_name[512];

void
set_proc_name(char * name)
{
    if (name == NULL)
    {
        proc_name[0] = 0;
    }
    else
    {
        strcpy(proc_name, name);
    }
}

void
set_process_name(char * name)
{
    set_proc_name(name);
}

char *
get_proc_name()
{
    if (proc_name[0] == 0)
    {
	return get_module_name();
    }
    else
    {
	return proc_name;
    }
}

char *
get_process_name()
{
    return get_proc_name();
}

void
set_module_name(char * name)
{
    if (name == NULL)
    {
	module_name[0] = 0;
    }
    else
    {
        strcpy(module_name, name);
    }

    /* Actions in module scope will be treated like C++ constructors. */
    set_proc_name(name);
}

void
set_spec_module_name(char * name)
{
    set_module_name(name);
}

char *
get_module_name()
{
    if (module_name[0] == 0)
	return "anonymous";
    else
        return module_name;
}

char *
get_spec_module_name()
{
    return get_module_name();
}

int
remember_locals(struct identifierlist * ids)
{
    while (ids != NULL)
    {
	    s.name = ids->name;
	    s.type = dontcare;

	    if (push_symbol(s) != 0) { return -1; }
	    ids = ids->next;
    }
    return 0;
}

void
forget_locals()
{
    destroy_stack();
}

void
activate_with()
{
  with_name[0] = 0;
  with = 1;
}

int
with_active()
{
  return with;
}

void
set_with_scope(char * scope)
{
  strcpy(with_name, scope);
}

char *
get_with_scope()
{
  return (with_name[0] == 0) ? NULL : with_name;
}

void
leave_with()
{
  with_name[0] = 0;
  with = 0;
}

