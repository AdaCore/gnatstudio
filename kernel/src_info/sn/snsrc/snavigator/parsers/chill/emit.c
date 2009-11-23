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
 * emit.c
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * Handler functions for treating interesting things we encounter in CHILL source code.
 */

#include <stdlib.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "common.h"
#include "stack.h"
#include "symtab.h"
#include "snptools.h"

/* Are we in local declaration space? */
int local = 0;

int
emit_module(struct identifier * id, struct blockregion block)
{
#ifdef DEBUG
	printf("emit_module: %s (%d,%d) to (%d,%d)\n", (id == NULL) ? "anonymous" : id->name, block.startline, block.startcol,
		block.endline, block.endcol);
#endif

	if (id == NULL)
		return sn_insert_symbol(SN_CLASS_DEF, NULL, "anonymous", sn_current_file(), block.startline, 
				   block.startcol, block.endline, block.endcol, 0L, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
	else
	   	return sn_insert_symbol(SN_CLASS_DEF, NULL, id->name, sn_current_file(), id->line, id->startcol,
	   		   block.endline, block.endcol, 0L, NULL, NULL, NULL, NULL, id->line, id->startcol, id->line, id->endcol);
}

int
emit_spec_module(struct identifier * id, struct blockregion block)
{
	return emit_module(id, block);
}

int
emit_procedure(struct identifier * id, struct sig signature)
{
#ifdef DEBUG
	printf("emit_procedure: %s (%d,%d) to (%ld,%ld)\n", id->name, 
	       id->line, id->startcol, sn_line(), sn_column());
#endif

	sn_insert_symbol(SN_MBR_FUNC_DCL, get_module_name(), id->name,
				sn_current_file(), id->line, id->startcol,
				sn_line(), sn_column(),
				SN_PRIVATE, signature.rettype, signature.args,
				NULL, NULL,
				id->line, id->startcol, id->line, id->endcol);
 
	return sn_insert_symbol(SN_MBR_FUNC_DEF, get_module_name(), id->name,
				sn_current_file(), id->line, id->startcol,
				sn_line(), sn_column(),
				SN_PRIVATE, signature.rettype, signature.args,
				NULL, NULL,
				id->line, id->startcol, id->line, id->endcol);
}

int
emit_process(struct identifier * id, struct sig signature)
{
	return emit_procedure(id, signature);
}

int
emit_synonyms(struct identifierlist * idlist, char * type)
{
	/* Treat these as constants. */
	while (idlist != NULL)
	{
#ifdef DEBUG
		printf("emit_synonyms: %s %s\n", idlist->name, type);
#endif
		sn_insert_symbol(SN_CONS_DEF, get_module_name(), idlist->name, sn_current_file(), idlist->line,
			idlist->startcol, idlist->line, idlist->endcol, 0L, type, NULL, NULL, NULL, idlist->line,
			idlist->startcol, idlist->line, idlist->endcol);
		
		idlist = idlist->next;
	}
	return 0;
}

int
emit_type_synonyms(struct identifierlist * idlist, char * type)
{
	/* Treat these as typedefs. */

	while (idlist != NULL)
	{
#ifdef DEBUG
		printf("emit_mode_synonyms: %s %s\n", idlist->name, type);
#endif
		sn_insert_symbol(SN_TYPE_DEF, get_module_name(), idlist->name, sn_current_file(), idlist->line,
			idlist->startcol, idlist->line, idlist->endcol, 0L, type, NULL, NULL, NULL,
			idlist->line, idlist->startcol, idlist->line, idlist->endcol);

		idlist = idlist->next;
	}
	return 0;
}

int
emit_structure(struct identifierlist * idlist, char * type)
{
	/* Treat these as classes (as we have to with C and other languages). */
	while (idlist != NULL)
	{
#ifdef DEBUG
	   printf("emit_structure: %s %s\n", idlist->name, type);
#endif
   	   /* Treat these as class member variables. */
	   sn_insert_symbol(SN_CLASS_DEF, get_module_name(), idlist->name, sn_current_file(), sn_line(), sn_column(),
		sn_line(), sn_column(), 0L, type, NULL, NULL, NULL, sn_line(), sn_column(), sn_line(), sn_column());

	   idlist = idlist->next;
	}
	return 0;
}
	
int
emit_declarations(struct identifierlist * idlist, char * type)
{
	/* Treat these as class member variables. */
	while (idlist != NULL)
	{
#ifdef DEBUG
	    printf("emit_decl: %s %s\n", idlist->name, type);
#endif
	   sn_insert_symbol(SN_MBR_VAR_DEF, get_module_name(), idlist->name, sn_current_file(), 
			    idlist->line, idlist->startcol, idlist->line, idlist->endcol, SN_PRIVATE | SN_STATIC,
			    type, NULL, NULL, NULL, idlist->line, idlist->startcol, idlist->line, idlist->endcol);
	
	   idlist = idlist->next;
	}
	return 0;
}

int
emit_enumeration_values(struct identifierlist * idlist)
{
	while (idlist != NULL)
	{
#ifdef DEBUG
		printf("emit_enum_val: %s\n", idlist->name);
#endif
	    sn_insert_symbol(SN_ENUM_CONST_DEF, get_module_name(), idlist->name, sn_current_file(),
			idlist->line, idlist->startcol, idlist->line, idlist->endcol, 0L,
			NULL, NULL, NULL, NULL, idlist->line, idlist->startcol, idlist->line, idlist->endcol);
	    idlist = idlist->next;
	}
	return 0;
}

/* Sets in CHILL are tagless (unlike C enums). */

int
emit_enumeration()
{
	return sn_insert_symbol(SN_ENUM_DEF, get_module_name(), "anonymous", sn_current_file(), sn_line(), sn_column(),
				sn_line(), sn_column(), 0L, NULL, NULL, NULL, NULL, sn_line(), sn_column(), sn_line(), sn_column());
}

int
emit_include(char * filename, unsigned startline, unsigned startcolumn,
	     unsigned endline, unsigned endcolumn)
{
	char absfilename[MAXPATHLEN];
#ifdef DEBUG
	printf("emit_include: %s\n", filename);
#endif
	if (sn_find_file(filename, absfilename) == 0)
	{
	  return sn_insert_symbol(SN_INCLUDE_DEF, NULL, absfilename,
				  sn_current_file(), sn_line(),
				  sn_column(), sn_line(), sn_column(),
				  0, NULL, NULL, NULL, NULL,
				  sn_line(), sn_column(), sn_line(),
				  sn_column());
	}
	return 1;
}

int
emit_xref_assignment(struct identifierlist * idlist)
{
	char * varname;
	int scope;

	while (idlist != NULL)
	{
	  scope = SN_REF_SCOPE_GLOBAL;
	  varname = idlist->name;

	  if (with_active())
	  {
	    varname = get_with_scope();
      	  }

#ifdef DEBUG 
	printf("emit_xref_assignment, argv=[%s,%d]\n", varname, idlist->line);
#endif

	  if (find_symbol(varname))
	  {
	    if ((int) sn_getopt(SN_OPT_LOCAL_VARS) == 0)
	    {
  	      /* No cross-referencing of this local variable required. */
#ifdef DEBUG
	      printf("emit_xref_assignment: no xref of last mentioned var!\n");
#endif
	      return 0;
	    }
	    else
	    {
	      scope = SN_REF_SCOPE_LOCAL;
	    }
	  }

	  sn_insert_xref(SN_REF_TO_MBR_VAR, SN_MBR_FUNC_DEF, scope,
	  	        get_module_name(), get_proc_name(), NULL,
	  	        get_module_name(), varname, NULL, sn_current_file(),
		        idlist->line, SN_REF_WRITE);

	  idlist = idlist->next;
	}
	return 0;
}

int
emit_xref_procedure(char * procname, unsigned lineno)
{
	assert(procname != NULL);

#ifdef DEBUG
	printf("emit_xref_procedure from %s::%s to %s::%s\n",
		get_module_name(), get_proc_name(), get_module_name(), procname);
#endif

	return sn_insert_xref(SN_REF_TO_MBR_FUNC, SN_MBR_FUNC_DEF,
	    SN_REF_SCOPE_GLOBAL, get_module_name(), get_proc_name(), NULL,
	    get_module_name(), procname, NULL, sn_current_file(), lineno,
	    SN_REF_PASS);
}

int
emit_xref_variable(char * varname, unsigned lineno)
{
	int scope = SN_REF_SCOPE_GLOBAL;

	assert(varname != NULL);

#ifdef DEBUG
	printf("emit_xref_variable from %s::%s to %s\n",
		get_module_name(), get_proc_name(), varname);
#endif

	if (find_symbol(varname))
	{
	  if ((int) sn_getopt(SN_OPT_LOCAL_VARS) == 0)
	  {
	    /* No cross-referencing of this local variable is required. */
#ifdef DEBUG
	    printf("emit_xref_variable: no xref of last mentioned var!\n");
#endif
	    return 0;
	  }
	  else
	  {
	    scope = SN_REF_SCOPE_LOCAL;
	  }
	}
	
	return sn_insert_xref(SN_REF_TO_MBR_VAR, SN_MBR_FUNC_DEF,
			      scope, get_module_name(), get_proc_name(),
			      NULL, get_module_name(), varname, NULL,
			      sn_current_file(), lineno, SN_REF_READ);
}

