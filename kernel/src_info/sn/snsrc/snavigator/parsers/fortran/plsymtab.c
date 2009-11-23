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

/* plsymtab.c:

		Routines associated with printing of local symbol table info

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.

	Shared functions defined:

		debug_symtabs()	Prints debugging info about symbol tables.
		print_loc_symbols(curmodhash) Prints local symtab info.

	Private functions defined:
		has_nonalnum()	  True if string has non-alphanumeric char
		sort_symbols()	  Sorts the list of names of a given category.
		swap_symptrs()	  Swaps a pair of pointers.
		check_flags()     Outputs messages about used-before-set etc.
		check_mixed_common() checks common for nonportable mixed type
		print_symbols(sym_list,n,do_types) Prints symbol lists.
		print_variables(sym_list,n)  Prints variable symbol table
		find_sixclashes() Finds variables with the same first 6 chars.
		identify_module(mod_name) Prints module name and file name.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <tcl.h>

#include "ftnchek.h"
#define PLSYMTAB
#include "symtab.h"
#include "sn.h"


int get_size(Lsymtab *symt,int type);

#if 0
PRIVATE int
has_nonalnum();

PRIVATE unsigned
find_sixclashes(), print_variables(), print_symbols();
#endif

PRIVATE void
identify_module(),
swap_symptrs(), sort_symbols(), check_flags();
#if 0
PRIVATE void
check_mixed_common();
#endif

static void * SN_calloc (int size1, int size2)
{
	void * p;
	p = (void*)ckalloc (size1*size2);
	memset (p, 0, size1*size2);
	return p;
}

PRIVATE void
sort_symbols(sp,n)      /* sorts a given list */
	Lsymtab *sp[];
	unsigned n;
{
	int i,j,swaps;
	for(i=0;i<n;i++) {
	    swaps = 0;
	    for(j=n-1;j>=i+1;j--) {
		if((strcmp(sp[j-1]->name, sp[j]->name)) > 0) {
		   swap_symptrs(&sp[j-1], &sp[j]);
		   swaps ++;
		}
	    }
	    if(swaps == 0) break;
	}
}


PRIVATE void			/* swaps two pointers */
swap_symptrs(x_ptr,y_ptr)
	Lsymtab **x_ptr,**y_ptr;
{
	Lsymtab *temp = *x_ptr;
	*x_ptr = *y_ptr;
	*y_ptr = temp;
}

/* Routine to print module name and file name just once in standard
   format is shared by print_loc_symbols, check_mixed_common and check_flags*/
PRIVATE int any_warnings;

PRIVATE void
identify_module(mod_name)
     char *mod_name;
{
#ifdef ERROR_MESS
  if(do_symtab) {
    fprintf(list_fd,"\nWarning: ");
  }
  else {
    if(any_warnings++ == 0) { /* 1st message of this module? */
      if(novice_help) {		/* Old-style format */
	fprintf(list_fd,
		"\nWarning in module %s file %s:",
		mod_name,current_filename);
      }
      else {			/* Lint-style format */
	fprintf(list_fd,
		"\n\"%s\" module %s: Warning:",
		current_filename,mod_name);
      }
    }
    fprintf(list_fd,"\n   ");	/* Details go indented on next line */
  }
  ++warning_count;		/* Count these warnings too */
#endif
}


void
print_loc_symbols(curmodhash)
     int curmodhash;		/* hash entry of current module */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
    static Lsymtab **sym_list=(Lsymtab **)NULL;
#else
    Lsymtab *sym_list[LOCSYMTABSZ]; /* temp. list of symtab entries to print */
#endif
    int	mod_type,		/* datatype of this module */
	this_is_a_function;	/* flag for treating funcs specially */
    Lsymtab *module;	 	/* entry of current module in symtab */
    char *mod_name;		/* module name */

#ifdef DYNAMIC_TABLES
    if(sym_list == (Lsymtab **)NULL) { /* Initialize if not done before */
      if( (sym_list=(Lsymtab **)SN_calloc(LOCSYMTABSZ,sizeof(Lsymtab *)))
	 == (Lsymtab **)NULL) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for local symbol list");
      }
    }
#endif

    any_warnings=0;		/* for identify_module(mod_name); */

			/* Keep track of symbol table and string usage */
    if(loc_symtab_top > max_loc_symtab) {
	max_loc_symtab = loc_symtab_top;
    }
    if(loc_str_top > max_loc_strings) {
	max_loc_strings = loc_str_top;
    }
    if(token_head_space_top > max_tokenlists) {
      max_tokenlists=token_head_space_top;
    }
    if(token_space_top > max_token_space) {
        max_token_space = token_space_top;
    }

			/* Global symbols only increase in number */
    max_glob_symtab = glob_symtab_top;
    max_glob_strings = STRSPACESZ - glob_str_bot;



    		/* Set up name & type, and see what kind of module it is */

	      module = hashtab[curmodhash].loc_symtab;

	      mod_name = module->name;
	      mod_type = get_type(module);

	      if(  mod_type != type_PROGRAM
		&& mod_type != type_SUBROUTINE
		&& mod_type != type_COMMON_BLOCK
		&& mod_type != type_BLOCK_DATA )
			this_is_a_function = TRUE;
	      else
			this_is_a_function = FALSE;

#ifdef rigo
	  			/* Print name & type of the module */
    if(do_symtab) {
      unsigned i;
      for(i=0,numentries=0;i<loc_symtab_top;i++) {
	if(loc_symtab[i].entry_point)
	  sym_list[numentries++] = &loc_symtab[i];
      }

	   if(numentries > 1) {
	      sort_symbols(sym_list,numentries);
	   }

	  fprintf(list_fd,"\n\nModule %s:",mod_name);
	  if( this_is_a_function ) fprintf(list_fd," func:");
	  fprintf(list_fd," %4s",type_name[mod_type]);
			/* Print a * next to non-declared function name */
	  if(datatype_of(module->type) == type_UNDECL ) {
			fprintf(list_fd,"*");
			imps++;
	  }
	  fprintf(list_fd,"\n");


				/* Print Entry Points (skip if only one,
				   since it is same as module name) */
      if(do_symtab && numentries > 1) {
	      fprintf(list_fd,"\nEntry Points\n");
	      (void) print_symbols(list_fd,sym_list,numentries,FALSE);
      }

			/* End of printing module name and entry points */
    }/*if(do_symtab)*/
#endif


#ifdef rigo

				/* Print the externals */

    if(do_symtab) {
        unsigned i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM) {
	      	  sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	      sort_symbols(sym_list,n);


	      fprintf(list_fd,"\nExternal subprograms referenced:\n");
	      imps += print_symbols(list_fd,sym_list,n,TRUE);
	}

      }/*if(do_symtab)*/
#endif


#ifdef rigo
				/* Print list of statement functions */
    if(do_symtab) {
           unsigned i,n;

	   for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_STMT_FUNCTION){
	      	  sym_list[n++] = &loc_symtab[i];
	       }
	   }
	   if(n != 0) {
	      sort_symbols(sym_list,n);
	      fprintf(list_fd,"\nStatement functions defined:\n");
	      imps += print_symbols(list_fd,sym_list,n,TRUE);
	    }
    }/*if(do_symtab)*/
#endif


#ifdef rigo
				/* Print the common blocks */
    if(do_symtab || port_check || f77_standard) {
           unsigned i,numblocks;

	   for(i=0,numblocks=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_COMMON_BLOCK) {
	      	  sym_list[numblocks++] = &loc_symtab[i];
	      }
	   }

	   if(numblocks != 0) {
	      sort_symbols(sym_list,numblocks);
	      if(do_symtab) {
		  fprintf(list_fd,"\nCommon blocks referenced:\n");
		  (void) print_symbols(list_fd,sym_list,numblocks,FALSE);
	      }
	      if(port_check || f77_standard) {
		    check_mixed_common(list_fd,sym_list,numblocks);
	      }
	   }
     }/*if(do_symtab||port_check)*/
#endif

#ifdef rigo
				/* Print the namelists */
    if(do_symtab) {
           unsigned i,numlists;

	   for(i=0,numlists=0;i<loc_symtab_top;i++) {
	      if(storage_class_of(loc_symtab[i].type) == class_NAMELIST) {
	      	  sym_list[numlists++] = &loc_symtab[i];
	      }
	   }

	   if(numlists != 0) {
	      sort_symbols(sym_list,numlists);
	      if(do_symtab) {
		  fprintf(list_fd,"\nNamelists defined:\n");
		  (void) print_symbols(list_fd,sym_list,numlists,FALSE);
	      }
	    }

    }/* End printing the namelists */
#endif
				/* Process the variables */

/*     if(do_symtab || usage_check) { */
    {
        unsigned i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && (!loc_symtab[i].entry_point || this_is_a_function)) {
		  sym_list[n++] = &loc_symtab[i];
#ifdef rigo
		  if(loc_symtab[i].argument && loc_symtab[i].set_flag) {
		    if(++args_modified <= 3)
			if(this_is_a_function && pure_functions) {
			    identify_module(mod_name);
			    fprintf(list_fd,
				  "Function %s %s argument %s",
				  mod_name,
				  loc_symtab[i].assigned_flag?
					"modifies":"may modify",
				  loc_symtab[i].name);
			}
		  }
		  if(loc_symtab[i].common_var && loc_symtab[i].set_flag) {
		    if(++com_vars_modified <= 3)
			if(this_is_a_function && pure_functions) {
			    identify_module(mod_name);
			    fprintf(list_fd,
				  "Function %s %s common variable %s",
				  mod_name,
				  loc_symtab[i].assigned_flag?
					"modifies":"may modify",
				  loc_symtab[i].name);
			}
		  }
#endif
	       }
	}
#ifdef rigo
	if(args_modified > 3 || com_vars_modified > 3)
	  if(this_is_a_function && pure_functions)
	    fprintf(list_fd,"\netc...");
#endif

	if(n != 0) {
	   sort_symbols(sym_list,n);

			/* Print the variables */

#ifdef rigo
	   if(do_symtab) {
	      fprintf(list_fd,"\nVariables:\n ");
	      imps += print_variables(sym_list,n);
	   }
#endif
        }
			/* Explain the asterisk on implicitly defined
			   identifiers.  Note that this message will
			   be given also if functions implicitly defined */
#ifdef rigo
	if(do_symtab && imps != 0) {
	     fprintf(list_fd,"\n* Variable not declared.");
	     fprintf(list_fd," Type has been implicitly defined.\n");
	     ++warning_count;
	}
#endif

	check_flags(sym_list,n,0,0,0,
		  "declared but never referenced",mod_name);
/* 	check_flags(sym_list,n,0,1,0, */
/* 		  "set but never used",mod_name); */
/* 	check_flags(sym_list,n,1,0,1, */
/* 		  "used before set",mod_name); */
/* 	check_flags(sym_list,n,1,1,1, */
/* 		  "may be used before set",mod_name); */

#ifdef rigo
	if(do_symtab || do_list)
	  fprintf(list_fd,"\n");
#endif

    }/* end if(do_symtab || usage_check) */

#ifdef rigo
			/* List all undeclared vars & functions */
    if(decls_required || implicit_none) {
        unsigned i,n;

	for(i=0,n=0;i<loc_symtab_top;i++) {
	    if(datatype_of(loc_symtab[i].type) == type_UNDECL
		&& ! loc_symtab[i].intrinsic /* omit intrinsics */
				/* omit subroutines called */
		&& (!loc_symtab[i].external || loc_symtab[i].invoked_as_func)
	       ) {
		sym_list[n++] = &loc_symtab[i];
	    }
	}
	if(n != 0) {
	    sort_symbols(sym_list,n);
	    identify_module(mod_name);
	    fprintf(list_fd,
		    "Identifiers of undeclared type");
	    (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }/*if(decls_required || implicit_none)*/

			/* Under -f77, list any nonstandard intrinsics used */
    if(f77_standard) {
      unsigned i,n;
      for(i=0,n=0;i<loc_symtab_top;i++) {
	if(storage_class_of(loc_symtab[i].type) == class_SUBPROGRAM
	   && loc_symtab[i].intrinsic &&
	   (loc_symtab[i].info.intrins_info->intrins_flags & I_NONF77)) {
	  sym_list[n++] = &loc_symtab[i];
	}
      }
      if(n != 0) {
	sort_symbols(sym_list,n);
	identify_module(mod_name);
	fprintf(list_fd,"Nonstandard intrinsic functions referenced:\n");
	(void) print_symbols(list_fd,sym_list,n,FALSE);
      }
    }/*if(f77_standard)*/


		/* issue -f77 warning for identifiers
		   longer than 6 characters
		*/
    if(f77_standard) {
        unsigned i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
	       if(strlen(loc_symtab[i].name) > (unsigned)6)
		  sym_list[n++] = &loc_symtab[i];
	}

	if(n != 0) {

	   sort_symbols(sym_list,n);

	   ++warning_count;

	   identify_module(mod_name);
	   fprintf(list_fd,
		   "Names longer than 6 chars (nonstandard):");
	   (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }

	/* If -f77 flag given, list names with underscore or dollarsign */

#if ALLOW_UNDERSCORES || ALLOW_DOLLARSIGNS
    if(f77_standard) {
        unsigned i,n;
	for(i=0,n=0;i<loc_symtab_top;i++) {
			/* Find all names with nonstd chars, but
			   exclude internal names like %MAIN */
	       if(has_nonalnum(loc_symtab[i].name) &&
		  loc_symtab[i].name[0] != '%')
		  sym_list[n++] = &loc_symtab[i];
	}

	if(n != 0) {

	   sort_symbols(sym_list,n);

	   ++warning_count;

	   identify_module(mod_name);

	   fprintf(list_fd,
		   "Names containing nonstandard characters");
	   (void) print_symbols(list_fd,sym_list,n,FALSE);
	}
    }/*if(f77_standard)*/
#endif

			/* Print out clashes in first six chars of name */
    if(sixclash) {
	 unsigned n;
	 n = find_sixclashes(sym_list);
	 if(n != 0) {
	    sort_symbols(sym_list,n);
	    identify_module(mod_name);
	    fprintf(list_fd,
		    "Identifiers which are not unique in first six chars");
	    (void) print_symbols(list_fd,sym_list,n,FALSE);
	 }/* end if(n != 0) */
    }/* end if(sixclash) */


		/* If portability flag was given, check equivalence
		   groups for mixed type. */
    if(port_check || local_wordsize==0) {
        unsigned i,j,n;
	unsigned imps=0;
	Lsymtab *equiv;

		/* scan thru table for equivalenced variables */
	for(i=0;i<loc_symtab_top;i++) {
	    if(storage_class_of(loc_symtab[i].type) == class_VAR
	       && loc_symtab[i].equiv_link != (equiv= &loc_symtab[i]) ){
		n=0;
		do {
		    if(equiv < &loc_symtab[i]) { /* skip groups done before */
			n=0;
			break;
		    }
		    sym_list[n++] = equiv;
		    equiv = equiv->equiv_link;
		} while(equiv != &loc_symtab[i]); /* complete the circle */
				/* Check for mixed types */
		if(n != 0) {
		    int mixed_type = FALSE, mixed_size = FALSE,
		        mixed_default_size = FALSE;
		    int t1,t2,s1,s2,defsize1,defsize2;

		    t1 = get_type(sym_list[0]);
		    s1 = get_size(sym_list[0],t1);
		    defsize1 = (s1 == size_DEFAULT);
		    if(s1 == size_DEFAULT) s1 = type_size[t1];
		    for(j=1; j<n; j++) {
		      t2 = get_type(sym_list[j]);
		      s2 = get_size(sym_list[j],t2);
		      defsize2 = (s2 == size_DEFAULT);
		      if(s2 == size_DEFAULT) s2 = type_size[t2];
		      if( t1 == t2 ) {
			if( t1 != type_STRING ){
				/* Same non-char types: size must match */
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
		      }
		      else {/* Different types */
				/* It is nonportable to equivalence:
				         Real*8 to Double or
					 Complex*16 to DComplex */
			if(type_category[t1] == type_category[t2]) {
			  if( s1 != s2 ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
				/* It is standard and portable to equivalence:
				         Real to Complex or
				         Double to DComplex */
			else if(equiv_type[t1] == equiv_type[t2]) {
			  if( ((type_category[t1] == type_COMPLEX)?
				s1 != 2*s2: s2 != 2*s1) ) {
			    mixed_size = TRUE;
			    break;
			  }
			  else if(defsize1 != defsize2) {
			    mixed_default_size = TRUE;
			    break;
			  }
			}
			else {
			  mixed_type = TRUE;
			  break;
			}
		      }/*end else different types*/

		      t1 = t2;
		      s1 = s2;
		      defsize1 = defsize2;
		    }/*end for j*/

		    if(mixed_type || mixed_size || mixed_default_size) {
			sort_symbols(sym_list,n);
			identify_module(mod_name);
			fprintf(list_fd,
			       "Mixed %s equivalenced (not portable):",
				    mixed_type?"types":
				      mixed_size?"sizes":
				       "default and explicit size items");

			imps += print_symbols(list_fd,sym_list,n,TRUE);
		    }
		}
	    }
	}
	if(imps != 0) {
	     identify_module(mod_name);
	     fprintf(list_fd,"* Variable not declared.");
	     fprintf(list_fd," Type has been implicitly defined.\n");
	}

    }/*if(port_check)*/

#endif

}/* print_loc_symbols */

#if 0
PRIVATE int
has_nonalnum(s)	/* Returns TRUE if s contains a non-alphanumeric character */
   char *s;
{
   while( *s != '\0' )
     if( ! isalnum( (int)(*s++) ) )
       return TRUE;
   return FALSE;
}
#endif

     /* This routine prints symbol names neatly.  If do_types is true
	also prints types, with * next to implicitly
	typed identifiers, and returns count thereof. */

#if 0
PRIVATE unsigned
print_symbols(fd,sym_list,n,do_types)
     FILE *fd;
     Lsymtab *sym_list[];
     unsigned n;
     int do_types;
{
     unsigned i,col=0,len,implicits=0;

     fprintf(fd,"\n");

     for(i=0;i<n;i++) {
	  len = strlen(sym_list[i]->name);/* len=actual length of name */
				/* Revise len to max(10,len)+extra 9=width
				   of field to be printed.  Adjust column
				   count to see where this will take us. */
	  col += len = (len <= 10? 10: len) + 9;
				/* If this will run past 78 start a new line */
	  if(col > 78) {
	    fprintf(fd,"\n");
	    col = len;
	  }
	  fprintf(fd,"%10s",sym_list[i]->name);/* Print the name in 10 cols */

	  if( do_types ) {	/* Optionally print the datatype */
	    if(sym_list[i]->intrinsic)
	      fprintf(fd,": intrns ");
	    else {
	      fprintf(fd,":");
	      (void) print_var_type(fd,sym_list[i]);
	      if(datatype_of(sym_list[i]->type) == type_UNDECL) {
		implicits++; /* Flag and count undeclareds */
		fprintf(fd,"*");
	      }
	      else if(sym_list[i]->size == size_DEFAULT)
		fprintf(fd," ");
	      fprintf(fd,"  ");
	    }
	  }
	  else			/* Otherwise just 9 blanks */
	    fprintf(fd,"%9s","");
     }

     fprintf(fd,"\n");

     return implicits;

}/*print_symbols*/
#endif



	/* This routine prints the variables nicely, and returns
	    count of number implicitly defined.
	 */
#if 0
PRIVATE unsigned
print_variables(sym_list,n)
     Lsymtab *sym_list[];
     unsigned n;
{
     unsigned i,implicits=0,adjustables=0;

     fprintf(list_fd,"\n ");

     for(i=0; i<4; i++) {
	  fprintf(list_fd,"%5sName Type Dims","");
		      /* 12345678901234567890 template for above*/
     }
     for(i=0; i<n; i++) {

	  if(i % 4 == 0)
	     fprintf(list_fd,"\n");
	  else
	     fprintf(list_fd," ");

	  fprintf(list_fd,"%10s",sym_list[i]->name);
	  adjustables += print_var_type(list_fd,sym_list[i]);

			/* Print a * next to implicitly declared variables */
	  if(datatype_of(sym_list[i]->type) == type_UNDECL ) {
	    implicits++;
	    fprintf(list_fd,"*");
	  }
	  else if(sym_list[i]->size == size_DEFAULT)
	    fprintf(list_fd," "); /* print blank if no size or * */


			/* print no. of dimensions next to var name */
	  if(sym_list[i]->array_var) {
		fprintf(list_fd," %ld",
			       array_dims(sym_list[i]->info.array_dim));
	  }
	  else {
	  	fprintf(list_fd,"%2s","");
	  }
    }

    if(adjustables > 0)
      fprintf(list_fd,"\nchar+ indicates adjustable size");
    fprintf(list_fd,"\n");

    return implicits;

}/*print_variables*/
#endif

int
print_var_type(fd,symt)	/* Prints type name then size if explicit */
			/* Returns 1 if adjustable size, else 0 */
     FILE *fd;
     Lsymtab *symt;
{
  int adjustable=0;
  int t = get_type(symt);
  int s = get_size(symt,t);

	  fprintf(fd," %4s",type_name[t]);

		/* Usually either size or * will be printed, and usually
		   size is 1 digit.  So mostly we print 1 column in
		   the next set of fprintf's.  Output will be ragged
		   if size > 9 or implicit type has explicit size. */
	  if( s != size_DEFAULT ) {
	    if(t != type_STRING || s > 1)
	      fprintf(fd,"%d",s);
	    else
	      if(s == size_ADJUSTABLE) {
		adjustable++;
		fprintf(fd,"+");
	      }
	      else
		fprintf(fd," ");
	  }
  return adjustable;
}

	/* Search thru local symbol table for clashes where identifiers
	   are not unique in 1st six characters. Return value =
	   number of clashes found, with pointers to symbol table
	   entries of clashers in array list. */
#if 0
PRIVATE unsigned
find_sixclashes(list)
	Lsymtab *list[];
{
	unsigned i,h, clashes=0;
	int class;
	unsigned long hnum;

	for(i=0; i<loc_symtab_top; i++) {	/* Scan thru symbol table */
	    class = storage_class_of(loc_symtab[i].type);
	    hnum = hash( loc_symtab[i].name );
				/* First look for a clash of any kind.
				   (N.B. this loop will never quit if hash
				   table is full, but let's not worry) */
	    while( (h=hnum % HASHSZ), hashtab[h].name != (char *)NULL) {
		/* Now see if the clashing name is used locally and still
		   clashes at 6 chars.  Treat common blocks separately. */

	     if((class == class_COMMON_BLOCK &&
	          (
		   hashtab[h].com_loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		)  ||
		 (class != class_COMMON_BLOCK &&
		  (
		   hashtab[h].loc_symtab != NULL
		   && strcmp( hashtab[h].name,loc_symtab[i].name) != 0
		   && strncmp(hashtab[h].name,loc_symtab[i].name,6) == 0
		  )
		 )
	       ) {
				/* If so, then i'th symbol is a clash */

			list[clashes++] = &loc_symtab[i];
			break;
		}
		else {
		    hnum = rehash(hnum);
		}
	    }
	}
	return clashes;
}
#endif


#ifdef DEBUG_SYMTABS
PRIVATE void
print_arg_array(arglist)        /* prints type and flag info for arguments */
	ArgListHeader *arglist;
{
	int i, count;
	ArgListElement *a;

	count = arglist->numargs;
	if(arglist->external_decl || arglist->actual_arg)
	  count = 0;
	a = arglist->arg_array;
	fprintf(list_fd,"\nArg list in module %s file %s line %u:",
		arglist->module->name, arglist->filename, arglist->line_num);
	fprintf(list_fd,"\n\tdef%d call%d ext%d arg%d",
		arglist->is_defn,
		arglist->is_call,
		arglist->external_decl,
		arglist->actual_arg);
	if(count == 0)
		fprintf(list_fd,"\n(Empty list)");
	else {
	    for (i=0; i<count; i++) {
		fprintf(list_fd,
			"\n\t%d %s: lv%d st%d as%d ub%d ar%d ae%d ex%d",
			i+1,
			type_name[datatype_of(a[i].type)],
				a[i].is_lvalue,
				a[i].set_flag,
				a[i].assigned_flag,
				a[i].used_before_set,
				a[i].array_var,
				a[i].array_element,
				a[i].declared_external);
		if(a[i].array_var)
		    fprintf(list_fd,"(%ld,%ld)",
			array_dims(a[i].info.array_dim),
			array_size(a[i].info.array_dim) );
		fprintf(list_fd,", ");
	    }
	}
}/* print_arg_array */


	       /* prints type and dimen info for common vars */
PRIVATE void
print_com_array(cmlist)
	ComListHeader *cmlist;
{
	int i, count;
	ComListElement *c;

	count = cmlist->numargs;
	c = cmlist->com_list_array;
	fprintf(list_fd,"\nCom list in module %s file %s line %u:",
		cmlist->module->name, cmlist->filename, cmlist->line_num);
	fprintf(list_fd,"\n\t");
	if(count == 0)
		fprintf(list_fd,"(Empty list)");
	else {
	    for (i=0; i<count; i++){
		fprintf(list_fd,"%s",type_name[datatype_of(c[i].type)]);
		if(c[i].dimen_info)
		    fprintf(list_fd,":%ldD(%ld)",array_dims(c[i].dimen_info),
					   array_size(c[i].dimen_info));
		fprintf(list_fd,", ");
	    }
	}
}/* print_com_array */
#endif /* DEBUG_SYMTABS */


#if 0 /* debugging code not currently in use */
PRIVATE void
print_tokenlist(toklist)        /* prints list of token names or types */
	TokenListHeader *toklist;
{
	int numargs=0;
	Token *t;
	fprintf(list_fd,"\n");
	if (toklist == NULL){
	    fprintf(list_fd,"\t(No list)");
	}
	else {
	    t = toklist->tokenlist;
	    while(t != NULL){
		++numargs;
		fprintf(list_fd," ");
		if ( is_true(ID_EXPR,t->subclass) )
		    fprintf(list_fd,"%s ",token_name(*t));
		else
		    fprintf(list_fd,"%s ",type_name[datatype_of(t->class)]);
		t = t->next_token;
	    }
	    if(numargs == 0)
		    fprintf(list_fd,"\t(Empty list)");
	}
}/* print_tokenlist */
#endif

void
debug_symtabs() 	/* Debugging output: hashtable and symbol tables */
{
#ifdef DEBUG_SYMTABS
  if(debug_loc_symtab) {
    fprintf(list_fd,"\n Debugging of local symbol table disabled");
    return;
  }

    if(debug_hashtab) {
        int i;
	fprintf(list_fd,"\n\nContents of hashtable\n");
	for(i=0; i<HASHSZ; i++) {
	    if(hashtab[i].name != NULL) {
	      fprintf(list_fd,"\n%4d %s",i,hashtab[i].name);
	      if(hashtab[i].loc_symtab != NULL)
		fprintf(list_fd," loc %d",hashtab[i].loc_symtab-loc_symtab);
	      if(hashtab[i].glob_symtab != NULL)
		fprintf(list_fd,
			" glob %d",hashtab[i].glob_symtab-glob_symtab);
	      if(hashtab[i].com_loc_symtab != NULL)
		fprintf(list_fd,
			" Cloc %d",hashtab[i].com_loc_symtab-loc_symtab);
	      if(hashtab[i].com_glob_symtab != NULL)
		fprintf(list_fd,
			" Cglob %d",hashtab[i].com_glob_symtab-glob_symtab);
	    }
	}
    }

    if(debug_glob_symtab) {
        int i;
	fprintf(list_fd,"\n\nContents of global symbol table");

	for(i=0; i<glob_symtab_top; i++) {
	    fprintf(list_fd,
		"\n%4d %s type 0x%x=%s,%s: ",
		i,
		glob_symtab[i].name,
		glob_symtab[i].type,
		class_name[storage_class_of(glob_symtab[i].type)],
		type_name[datatype_of(glob_symtab[i].type)]
	     );
	    fprintf(list_fd,
      "usd%d set%d asg%d ubs%d lib%d int%d invf%d vis%d smw%d incl%d ext%d ",
		glob_symtab[i].used_flag,
		glob_symtab[i].set_flag,
		glob_symtab[i].assigned_flag,
		glob_symtab[i].used_before_set,
		glob_symtab[i].library_module,
		glob_symtab[i].internal_entry,
		glob_symtab[i].invoked_as_func,
		glob_symtab[i].visited,
		glob_symtab[i].visited_somewhere,
		glob_symtab[i].defined_in_include,
		glob_symtab[i].declared_external
		    );
	    switch(storage_class_of(glob_symtab[i].type)){
		case class_COMMON_BLOCK:{
		    ComListHeader *clist;
		    clist=glob_symtab[i].info.comlist;
		    while(clist != NULL){
			print_com_array(clist);
			clist = clist->next;
		    }
		    break;
		}
		case class_SUBPROGRAM:{
		    ArgListHeader *alist;
		    alist=glob_symtab[i].info.arglist;
		    while(alist != NULL){
			print_arg_array(alist);
			alist = alist->next;
		    }
		    break;
		}
	    }
	}
    }
#endif
}/* debug_symtabs*/


#if 0
PRIVATE void
check_mixed_common(fd,sym_list,n)
     FILE *fd;
     Lsymtab *sym_list[];
     unsigned n;
{
    int i;
    for(i=0; i<n; i++) {
	ComListHeader *chead = sym_list[i]->info.comlist;
	ComListElement *clist;
	char *mod_name = chead->module->name;
	int j,nvars;
	int has_char=FALSE,has_nonchar=FALSE;
	int prev_size, this_size, this_type;

	if(chead == NULL)
	  continue;
	clist=chead->com_list_array;
	nvars = chead->numargs;

	for(j=0; j<nvars; j++) {

	   /* Check conformity to ANSI rule: no mixing char with other types */

	  if( (this_type=datatype_of(clist[j].type)) == type_STRING) {
	    has_char = TRUE;
	    this_size = 1;/* char type size is 1 for alignment purposes */
	  }
	  else { /* other types use declared sizes */
	    has_nonchar = TRUE;
	    if( (this_size=clist[j].size) == size_DEFAULT)
	      this_size = type_size[this_type];
	  }
	  if(has_char && has_nonchar) {
	    if(f77_standard){
	      identify_module(mod_name);
	      fprintf(fd,
		   "Common block %s line %u has mixed",
		   sym_list[i]->name,
		   chead->line_num);
	      fprintf(fd,
		   "\n  character and non-character variables (nonstandard)");
	    }
	    break;
	  }

	/* Check that variables are in descending order of type size */

	 if(j > 0) {
	  if( this_size > prev_size ) {
	    if(port_check) {
	      identify_module(mod_name);
	      fprintf(fd,
		    "Common block %s line %u has long data type",
		    sym_list[i]->name,
		    chead->line_num);
	      fprintf(fd,
		    "\n  following short data type (may not be portable)");
	    }
	    break;
	  }
	 }
	 prev_size = this_size;
	}
    }
}
#endif


PRIVATE void
check_flags(list,n,used,set,ubs,msg,mod_name)
	Lsymtab *list[];
	unsigned n;
	unsigned used,set,ubs;
	char *msg,*mod_name;
{
	extern	int	report_local_vars;
	extern	int	cross_scope_type;
	int matches=0,i;
	unsigned pattern;

	if (!report_local_vars)
		return;

	pattern = flag_combo(used,set,ubs);
	list_fd = stdout;

	for(i=0;i<n;i++) {
	    if( list[i]->common_var )	/* common vars are immune */
	       continue;
				/* for args, do only 'never used' */
	    if( list[i]->argument && pattern != flag_combo(0,0,0) )
	       continue;

#ifdef ALLOW_INCLUDE
				/* Skip variables 'declared but not used'
				   and parameters 'set but never used'
				   if defined in include file. */

	    if( list[i]->defined_in_include &&
	       ( pattern == flag_combo(0,0,0)
	       || (list[i]->parameter && pattern == flag_combo(0,1,0)) ) )
	        continue;
#endif
			/*  function return val: ignore 'set but never used' */
	    if( list[i]->entry_point && pattern == flag_combo(0,1,0) )
		continue;

	    if(flag_combo(list[i]->used_flag,list[i]->set_flag,
	       list[i]->used_before_set) == pattern) {
		 if(matches++ == 0) {
		   identify_module(mod_name);
/* 		   fprintf(list_fd, */
/* 			    "Variables %s:\n", */
/* 			    msg); */
		 }

		put_cross_ref(PAF_REF_TO_LOCAL_VAR,
			cross_scope_type,
			PAF_REF_SCOPE_LOCAL,
			NULL,
			mod_name,
			NULL,
			mod_name,	/* It will be the 'class' name too. */
			list[i]->name,
			NULL,
			current_filename,
			list[i]->line_num,
			PAF_REF_UNUSED);

/* 		 fprintf(list_fd,"%10s",list[i]->name); */
				/* arg never used: tag with asterisk */
/* 		 fprintf(list_fd,"%-9s", */
/* 			 list[i]->argument? (++unused_args,"*") : "" ); */
	    }
	}
/* 	if(unused_args > 0) */
/* 		fprintf(list_fd,"\n  * Dummy argument"); */
/* 	if(matches > 0) */
/* 		fprintf(list_fd,"\n"); */
}


