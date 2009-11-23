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

/* symtab.c:

Contains formerly separate modules:
   I. Symtab: symbol table maintenance routines.
  II. Hash:  hash table functions: hash(), kwd_hash(), rehash()
 III. Intrins: handles recognition & data typing of intrinsic functions.


    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/

/*
  I. Symtab


		Symbol table routines for Fortran program checker.

	  Shared functions defined:


	   call_func(id,arg)	 Handles function invocations.
	   call_subr(id,arg)	 Handles CALL statements.
	   declare_type(id,datatype,size) Handles TYPE statements.
	   def_arg_name(id)	 Handles func/subr argument lists.
	   def_array_dim(id,arg) Handles dimensioning declarations.
	   def_com_block(id)	 Handles common blocks and SAVE stmts.
	   def_com_variable(id)	 Handles common block lists.
       int def_curr_module(id)	 Identifies symbol as current module.
     	   def_equiv_name(id)	 Initializes equivalence list items.
	   def_ext_name(id)	 Handles external lists.
	   def_function(datatype,size,id,args)
	   		Installs function name in global table.
	   def_intrins_name(id)  Handles intrinsic lists.
	   def_parameter(id,value) Handles parameter_defn_item
	   def_stmt_function(id) Declares a statement function.
	   do_ASSIGN(id)	 Handles ASSIGN stmts.
	   do_assigned_GOTO(id)	 Handles assigned GOTO.
	   do_ENTRY(id,args,hashno) Processes ENTRY statement.
	   do_RETURN(hashno,keyword) Processes RETURN statement.
	   equivalence(id1,id2)	 equivalences two variables
       int get_type(symt)	 Finds out data type of symbol, or uses implicit
				 typing to establish its type.
       int get_size(symt,type)	 Finds out size of symbol's datatype.
	unsigned hash_lookup(s)	 Looks up identifier in hashtable.
	   init_globals()	 Initializes global symbol info.
	   init_symtab()	 Clears local symbol table & removes locals
				 from stringspace. Also restores default
				 implicit data typing.
 Gsymtab* install_global(t,datatype,storage_class) Installs indentifier in
				global symbol table.
 Lsymtab* install_local(id,t,datatype,storage_class) Installs indentifier in
				local symbol table.
ArgListHeader* make_arg_array(t) Converts list of tokens into list of
				 type-flag pairs.
ArgListHeader* make_dummy_arg_array(t) Converts list of tokens into list of
				 type-flag pairs.
ArgListHeader* make_arrayless_alist() Sets up argument list header for
				EXTERNAL decl or subprog as actual arg.
ComListHeader* make_com_array(t) Converts list of common block tokens into
				 list of dimen_info-type pairs.
	   process_lists()	 Places pointer to linked list of arrays in
				 global symbol table
	   ref_array(id,subscrs) Handles array references
	   ref_variable(id)	 Handles accessing variable name.
	   set_implicit_type(type,size,c1,c2) Processes IMPLICIT statement.
	   stmt_function_stmt(id) Finishes processing stmt func defn.
    char * token_name(t)	 Returns ptr to token's symbol's name.
	   use_actual_arg(id)	 Handles using a variable as actual arg.
	   use_io_keyword(id_keywd,id_val,class) Handles i/o control specifier.
	   use_lvalue(id)	 Handles assignment to a variable.
	   use_parameter(id)	 Handles data_constant_value &
				 data_repeat_factor.
	   use_variable(id)	 Sets used-flag for a variable used in expr.

*/

/*  private functions defined:
 arg_count(t)		Counts the number of arguments in a token list.
 call_external(symt,id,arg)	places token list of args into local symtab
 check_intrins_args(arg, defn) Checks call seq of intrinsic functions
 check_stmt_function_args(symt,id,arg)  ditto for statement functions
 find_intrinsic()		Looks up intrinsic functions in table
 find_io_keyword()		Looks up i/o control spec keywords
 reverse_tokenlist(t)		Reverses a linked list of tokens
 make_TL_head();		Initializes a tokenlist header
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#define SYMTAB
#include "ftnchek.h"
#include "symtab.h"
#include "fortran.h"
#include "sn.h"
#include "tcl.h"
#include <stdlib.h>

extern int current_module_hash;
extern int current_record_hash;
extern int report_local_vars;
extern int cross_scope_type;
extern	int	highlight;

PRIVATE
unsigned arg_count();

PRIVATE void
call_external(),
check_intrins_args(),
check_stmt_function_args();

PRIVATE int
find_io_keyword();

PRIVATE Token *
reverse_tokenlist();

PRIVATE TokenListHeader *	/* Initializes a tokenlist header */
make_TL_head();

PRIVATE
ArgListHeader *make_dummy_arg_array(),*make_arg_array(),
 *make_arrayless_alist();

PRIVATE
ComListHeader *make_com_array();

				/* Routines to allocate arglist and comlist
				   stuff are external for Turbo C workaround,
				   otherwise they are local.  */
#ifdef T_ALLOC
#define T_EXTERN extern
#else
#define T_EXTERN
#endif

T_EXTERN ArgListHeader *new_arglistheader();
T_EXTERN ArgListElement *new_arglistelement();
T_EXTERN ComListHeader *new_comlistheader();
T_EXTERN ComListElement *new_comlistelement();

PRIVATE
IntrinsInfo *find_intrinsic();

static char *datatype_name( int datatype );
static void report( Token *id, int typ );
static void print_func_argument_list( Token *t, char **buf);
static void report_name( char *name, Token *id, char *buf );
static void report_class_name( Token *id, char *cname, char *varn );

static void * SN_calloc (int size1, int size2)
{
	void * p;
	p = (void*)ckalloc (size1*size2);
	memset (p, 0, size1*size2);
	return p;
}

PRIVATE unsigned
arg_count(t)            /* Counts the number of arguments in a token list */
	Token *t;
{
	unsigned count;
	count = 0;
	while(t != NULL){
		count++;
		t = t->next_token;
	}
	return(count);
}

			/* This routine handles the saving of arg lists which
			   is done by call_func and call_subr.  Also called
			   by def_namelist to save its variable list. */
PRIVATE void
call_external(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
{
       	TokenListHeader *TH_ptr;

		/* Insert the new list onto linked list of token lists */
      	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (arg == NULL ? NULL: arg->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;
} /*call_external*/

void
call_func(id,arg)	/* Process function invocation */
	Token *id, *arg;
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	IntrinsInfo *defn;

	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(id,h,type_UNDECL,class_SUBPROGRAM);
       	   symt->info.toklist = NULL;
	}

	t = datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if(storage_class_of(symt->type) == class_VAR) {
	    symt->type = type_byte(class_SUBPROGRAM,t);
	    symt->info.toklist = NULL;
	  }


		/* See if intrinsic.  If so, set flag, save info */
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }

		/* Update set/used status of variables in arg list.  This
		   is deferred to now to allow intrinsics to be treated
		   as pure functions regardless of pure_function flag. */

	if(arg != NULL) {
	    Token *a=arg;
	    int nonpure = symt->intrinsic?
		(symt->info.intrins_info->intrins_flags&I_NONPURE)
		: ! pure_functions;

  	    while( (a=a->next_token) != NULL) {
	      if(is_true(ID_EXPR,a->subclass)){
		if( nonpure ) {
			     /* Treat impure function like subroutine call */
		  use_actual_arg(a);
		  use_variable(a);
		}
		else {
			     /* Pure-function invocation checks u-b-s */
		  use_function_arg(a);
		}
	      }
	    }
	}

		/* If intrinsic, do checking now.  Otherwise, save arg list
		   to be checked later. */

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
      switch(storage_class_of(symt->type)) {
	case class_SUBPROGRAM:
	  symt->external = TRUE;
	  if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	  }
			/* store arg list in local table */
	  call_external(symt,id,arg);
	  break;
	case class_STMT_FUNCTION:
	  symt->external = TRUE;
	  check_stmt_function_args(symt,id,arg);
	  break;
      }
    }

    symt->used_flag = TRUE;
    symt->invoked_as_func = TRUE;

} /*call_func*/


void
call_subr(id,arg)	/* Process call statements */
	Token *id, *arg;
{
	int t, h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
#ifndef STANDARD_INTRINSICS
	IntrinsInfo *defn;
#endif
	if( (symt = (hashtab[h].loc_symtab)) == NULL){
	   symt = install_local(id,h,type_SUBROUTINE,class_SUBPROGRAM);
   	   symt->info.toklist = NULL;
	}


	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

	if(t == type_UNDECL) {
		t = type_SUBROUTINE;
		symt->info.toklist = NULL;
	}
	symt->type = type_byte(class_SUBPROGRAM,t);

	/* Since nonstandard intrinsics include some subroutines,
	   see if it is in intrinsic list.  Or
	   if declared intrinsic, then accept it as such and
	   do checking now.  Otherwise, save arg list
	   to be checked later. */
#ifndef STANDARD_INTRINSICS
    if(!symt->external && !symt->intrinsic
		&& (defn = find_intrinsic(symt->name)) != NULL) {
			/* First encounter with intrinsic fcn: store info */
		symt->intrinsic = TRUE;
		symt->info.intrins_info = defn;
    }
#endif

    if(symt->intrinsic) {
			/* It is intrinsic: check it */
	check_intrins_args(id,arg);
    }
    else {		/* It is not intrinsic: install in global table */
	symt->external = TRUE;
	if((!symt->argument) && (gsymt=(hashtab[h].glob_symtab)) == NULL) {
		gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
		gsymt->info.arglist = NULL;
	}
			/* store arg list in local table */
	call_external(symt,id,arg);
    }

	symt->used_flag = TRUE;

}/*call_subr*/

		/* This routine catches syntax errors that have to
		   wait till END is seen.  At the moment, only looks if
		   CHARACTER*(*) declarations are put on the wrong thing.
		   Has to wait since can use it for ENTRY pt.
		   Also checks if things SAVED that shouldn't be.
		 */
void
check_loose_ends(curmodhash)
     int curmodhash;    /* current_module_hash from fortran.y */
{
  int i;
  for(i=0;i<loc_symtab_top;i++) {
    if( datatype_of(loc_symtab[i].type) == type_STRING &&
	loc_symtab[i].size == size_ADJUSTABLE &&
       !(loc_symtab[i].argument ||
	   loc_symtab[i].parameter ||
	     loc_symtab[i].entry_point) ) {
      syntax_error(NO_LINE_NUM,NO_COL_NUM,loc_symtab[i].name);
      msg_tail("cannot be adjustable size in module");
      msg_tail(hashtab[curmodhash].name);
    }
    if(loc_symtab[i].saved &&
        (loc_symtab[i].common_var ||
	 loc_symtab[i].argument ||
	 loc_symtab[i].external ||
	 loc_symtab[i].parameter ||
	 loc_symtab[i].entry_point) ) {
      syntax_error(NO_LINE_NUM,NO_COL_NUM,loc_symtab[i].name);
      msg_tail("cannot be declared in SAVE statement in module");
      msg_tail(hashtab[curmodhash].name);
    }
  }
}

		/* check out consistency of intrinsic argument list */
PRIVATE
void
check_intrins_args(id, arg)
	Token *id;
	Token *arg;
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	IntrinsInfo *defn=symt->info.intrins_info;
	unsigned args_given = ((arg == NULL)?0:arg_count(arg->next_token));
	int numargs;
	unsigned short flags;
	Token *t;

	numargs = defn->num_args;
	flags = defn->intrins_flags;

			/* positive numargs: must agree */
	if( (numargs >= 0 && (args_given != numargs))
			/* 1 or 2 arguments allowed */
	 || (numargs == I_1or2 && (args_given != 1 && args_given != 2))
			/* numargs == -2: 2 or more */
	 || (numargs == I_2up && (args_given < 2))
			/* 0 or 1 argument allowed */
	 || (numargs == I_0or1 && (args_given != 0 && args_given != 1)) ){
		unsigned line_num,col_num;
		if(arg==NULL) {line_num=id->line_num; col_num=id->col_num;}
		else {line_num = arg->line_num; col_num = arg->col_num;}

		syntax_error(line_num,col_num,
		  "wrong number of arguments for intrinsic function");
		msg_tail(defn->name);
	}
	if(arg != NULL) {

	  Token *prev_t,	/* one operand in type propagation  */
	         fake_op;	/* operator in binexpr_type call */

	  arg->next_token = t = reverse_tokenlist(arg->next_token);
				/* Copy type & size info into result */
	  arg->class = t->class;
	  arg->subclass = t->subclass;
	  arg->size = t->size;
	  prev_t = t;

	  while(t != NULL) {
	    if(intrins_arg_cmp(defn,t)) {
				/* Propagate data type thru the list.
				   Resulting type info is stored in
				   args token.  */
	      if(prev_t != t && ! (flags & I_MIXED_ARGS) ) {
				/* Set up a pretend expr term for binexpr */
		fake_op.class = ',';
		fake_op.line_num = prev_t->line_num;
		fake_op.col_num = prev_t->col_num;

		binexpr_type(prev_t,&fake_op,t,arg);
	      }
	      prev_t = t;
	    }
	    t = t->next_token;
	  }/* end while */

	}/* end arg != NULL */
}/* check_intrins_args */


PRIVATE
void
check_stmt_function_args(symt,id,arg)
	Lsymtab *symt;
	Token *id,*arg;
{
	unsigned n1,n2,n;
	int i;
	Token *t1,*t2;

	t1 = symt->info.toklist->tokenlist;
	t2 = ((arg==NULL)? NULL: reverse_tokenlist(arg->next_token));

	n1 = arg_count(t1);
	n2 = arg_count(t2);

	if(n1 != n2) {
	    syntax_error(id->line_num,id->col_num,
		"function invoked with incorrect number of arguments");
	}

	n = (n1 < n2? n1: n2);
	for(i=0; i<n; i++) {
#ifdef OLDSTUFF
	    if( t1->class != t2->class) {
		syntax_error(t2->line_num,t2->col_num,
		  "function argument is of incorrect datatype");
	    }
#else
	    stmt_fun_arg_cmp(symt,t1,t2);
#endif
	    t1 = t1->next_token;
	    t2 = t2->next_token;
	}
}

void
declare_type(id,datatype,size)
	Token *id;
	int datatype;
	long size;
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

	if( (symt) == NULL) {
	   symt = install_local(id,h,datatype,class_VAR);
	   symt->size = size;
	}
	else {           /* Symbol has been seen before: check it */

			/* Intrinsic: see if type is consistent */
	  if( symt->intrinsic ) {
	    IntrinsInfo *defn = symt->info.intrins_info;
	    int rettype = defn->result_type,
		argtype = defn->arg_type;
			/* N.B. this test catches many but not all errors */
	    if( (rettype != type_GENERIC && datatype != rettype)
	     || (rettype == type_GENERIC && !((1<<datatype) & argtype)) ){
		    warning(id->line_num,id->col_num,
				"Declared type ");
		    msg_tail(type_name[datatype]);
		    msg_tail(" is invalid for intrinsic function: ");
		    msg_tail(symt->name);
	      }
	  }

	  if(datatype_of(symt->type) != type_UNDECL) {
	      syntax_error(id->line_num,id->col_num,
		"Symbol redeclared: ");
	  	msg_tail(symt->name);
	  }
	  else {
			/* Now give it the declared type */
	      symt->type = type_byte(storage_class_of(symt->type),datatype);
	      symt->size = size;
	  }
	}
				/* Under -port warn if char size > 255 */
	if(port_check) {
	  if(datatype == type_STRING && size > 255)
	    nonportable(id->line_num,id->col_num,
			"character variable length exceeds 255");
	}

	{
		extern int current_struct_hash;

		if( current_struct_hash != -1 )
		{
			if( highlight != -1 )
			{
				put_symbol(PAF_MBR_VAR_DEF,
					hashtab[current_struct_hash].name,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)PAF_PUBLIC,
					datatype_name(datatype),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);

				if( datatype == type_RECORD )
				{
					put_symbol(PAF_CLASS_INHERIT,
						hashtab[current_struct_hash].name,
						hashtab[current_record_hash].name,
						current_filename,
						id->line_num,
						id->curr_index,
						0,0,
						(long)PAF_PUBLIC,NULL,NULL,NULL,NULL,
						0,0,0,0);
				}
			}
		}
		else
		{
			if( highlight != -1 && report_local_vars )
			{
				put_symbol(PAF_LOCAL_VAR_DEF,
					hashtab[current_module_hash].name,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)0,
					datatype_name(datatype),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);
			}
		}
	}

	symt->record_hash = current_record_hash;

}/*declare_type*/


void
def_arg_name(id)		/* Process items in argument list */

	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */

	}
	symt->argument = TRUE;
}/*def_arg_name*/


void
def_array_dim(id,arg)	/* Process dimension lists */
	Token *id,*arg;	     /* arg previously defined as int */
{
	int h=id->value.integer;
	Lsymtab *symt;


	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */
	   if(storage_class_of(symt->type) != class_VAR) {
	      syntax_error(id->line_num,id->col_num,
		"Entity cannot be dimensioned: ");
		msg_tail(symt->name);
	      return;
	   }
	}
	symt->array_var = TRUE;
	if(!equivalence_flag){      /* some checking should be done here */
	   if(symt->info.array_dim != 0)
	      syntax_error(id->line_num,id->col_num,
		"Array redimensioned");
	   else
	      symt->info.array_dim = array_dim_info(arg->class,arg->subclass);
	}
}/*def_array_dim*/


void
def_com_block(id,comlist)	/* Process common blocks and save_stmt */
	Token *id, *comlist;

{
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
   	TokenListHeader *TH_ptr;
	extern unsigned true_prev_stmt_line_num;/* set by fortran.y */

		/* Install name in global symbol table */
	if( (gsymt=hashtab[h].com_glob_symtab) == NULL) {
	   gsymt = install_global(h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   gsymt->info.comlist = NULL;
	}


	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(id,h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}
	if(pretty_flag) {

		/* Flag declarations of same block in separate statements
		   unless separated only by comments. Use front token
		   of previous tokenlist which is last token of decl. */
	  if(comlist != NULL && symt->info.toklist != NULL
	   && symt->info.toklist->tokenlist->line_num < true_prev_stmt_line_num) {
	    ugly_code(id->line_num,id->col_num,
		"Common block declared in more than one statement");
	  }
	}

		/* Insert the new list onto linked list of token lists */
	if(comlist != NULL) {
	  	/* Will be NULL only for SAVE, in which case skip */
	    TH_ptr= make_TL_head(id);

 	    TH_ptr->tokenlist = comlist->next_token;
	    TH_ptr->next = symt->info.toklist;
            symt->info.toklist = TH_ptr;
	}

   	symt->set_flag = TRUE;
	symt->used_flag = TRUE;
}/*def_com_block*/


void
def_com_variable(id)		/* Process items in common block list */
	Token *id;
{
	extern int current_common_hash;	/* rigo */
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {           /* Symbol has been seen before: check it */
	    if(symt->common_var) {
		syntax_error(id->line_num,id->col_num,
		     "Variable cannot be in two different common blocks");
	    }
	    else if(symt->entry_point || symt->parameter ||
		    symt->argument || symt->external || symt->intrinsic) {
		syntax_error(id->line_num,id->col_num,
		     "Item cannot be placed in common");
		return;
	    }
	    if(symt->size == size_ADJUSTABLE) {	/* CHARACTER *(*) */
	      syntax_error(id->line_num,id->col_num,
		    "Common variable cannot have adjustable size");
	      symt->size = 1;
	    }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{

	equiv->common_var = TRUE; /* set the flag even if not legit */
	equiv->common_hash = current_common_hash; /* rigo */
	equiv->common_orig_hash = h; /* rigo */
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

	if( highlight != -1 )
	{
		if( current_common_hash == -1 )
		{
			put_symbol(PAF_COMMON_MBR_VAR_DEF,
					blank_com_name,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)0,
					datatype_name(symt->type),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);
		}
		else
		{
			put_symbol(PAF_COMMON_MBR_VAR_DEF,
					hashtab[current_common_hash].name,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)0,
					datatype_name(symt->type),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);
		}
	}
}/*def_com_variable*/


	/* This guy sets the flag in symbol table saying the id is the
	   current module.  It returns the hash code for later reference.
	 */
int
def_curr_module(id)
	Token *id;
{
	int hashno = id->value.integer;
	hashtab[hashno].loc_symtab->is_current_module = TRUE;

	return hashno;
}/*def_curr_module*/




void
def_equiv_name(id)		/* Process equivalence list elements */
	Token *id;
{
  ref_variable(id);		/* Put it in symtab */
	/* No other action needed: processing of equiv pairs is
	   done by equivalence() */
}/*def_equiv_name*/



void
def_ext_name(id)		/* Process external lists */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(id,h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */

	    if(storage_class_of(symt->type) == class_VAR) {
	      symt->info.toklist = NULL;
	    }
	    symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

	if(symt->intrinsic){
	    syntax_error(id->line_num,id->col_num,
		"Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	    symt->external = TRUE;
	    if(!symt->argument){
	        TokenListHeader *TH_ptr;
		Gsymtab *gsymt;
		if( (gsymt=hashtab[h].glob_symtab) == NULL) {
	   	    gsymt = install_global(h,type_UNDECL,class_SUBPROGRAM);
	   	    gsymt->info.arglist = NULL;
		}
		TH_ptr=make_TL_head(id);

		TH_ptr->external_decl = TRUE;
		TH_ptr->next = symt->info.toklist;
		symt->info.toklist = TH_ptr;
	     }
	  }
      symt->declared_external = TRUE;
}/*def_ext_name*/



void
def_function(datatype,size,id,args)
				/* Installs function or subroutine name */
	int datatype;                     /* in global table */
	long size;
	Token *id,*args;
{
	int storage_class;
	int h=id->value.integer;
	Lsymtab *symt;
	Gsymtab *gsymt;
	TokenListHeader *TH_ptr;
   	storage_class = class_SUBPROGRAM;

	if((gsymt = (hashtab[h].glob_symtab)) == NULL) {
			/* Symbol is new to global symtab: install it */
	  gsymt = install_global(h,datatype,storage_class);
	  gsymt->size = size;
	  gsymt->info.arglist = NULL;
	}
	else {
			/* Symbol is already in global symtab. Put the
			   declared datatype into symbol table. */
	  gsymt->type = type_byte(storage_class,datatype);
	  gsymt->size = size;
	}

   	if((symt = (hashtab[id->value.integer].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it.
			   Since this is the current routine, it has
			   storage class of a variable. */
	   symt = install_local(id,h,datatype,class_VAR);
	   symt->size = size;
	}
	if(! symt->entry_point)	/* seen before but not as entry */
	   symt->info.toklist = NULL;


		/* Insert the new list onto linked list of token lists */
   	TH_ptr=make_TL_head(id);

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

	symt->entry_point = TRUE;

		/* library mode: set the flag so no complaint will
		   be issued if function never invoked.  Also, set
		   used_flag if this is a main program, for same reason. */
	if(library_mode)
		symt->library_module = TRUE;
	if(datatype == type_PROGRAM)
		symt->used_flag = TRUE;


#if 1 /* Zsolt Koppany, map PROGRAM to SUBROUTINE 28-sep-06 */
	if(datatype != type_BLOCK_DATA &&
		hashtab[id->value.integer].name[0] != '%')
#else
	if( datatype != type_PROGRAM && datatype != type_BLOCK_DATA )
#endif /* 1 */
	{
		if( highlight != -1 )
		{
			char	*arl = NULL;

			print_func_argument_list(args,&arl);

			if( datatype == type_SUBROUTINE || datatype == type_PROGRAM)
			{
				cross_scope_type = PAF_SUBR_DEF;
			}
			else
			{
				cross_scope_type = PAF_FUNC_DEF;
			}

			put_symbol(cross_scope_type,
				NULL,
				hashtab[id->value.integer].name,
				current_filename,
				id->line_num,
				id->curr_index,
				0,0,
				(long)0,datatype_name(datatype),
				arl,
				NULL,
				get_comment(current_filename,id->line_num),
				0,0,0,0);
			if (arl)
				ckfree(arl);
		}
	}

}/*def_function*/



void
def_intrins_name(id)		/* Process intrinsic lists */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt = hashtab[h].loc_symtab) == NULL){
	   symt = install_local(id,h,type_UNDECL,class_SUBPROGRAM);
	   symt->info.toklist = NULL;
        }
	else {
			/* Symbol seen before: check it & change class */
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }

	  symt->type = type_byte(class_SUBPROGRAM,datatype_of(symt->type));
	}

		/* Place info about intrinsic datatype in local symtab.
		   If not found, it will be treated as external.
		 */

	if(symt->external){
	    syntax_error(id->line_num,id->col_num,
	       "Cannot declare same subprogram both intrinsic and external:");
	    msg_tail(symt->name);
	}
	else{
	  IntrinsInfo *defn;
	  symt->declared_intrinsic = TRUE;
	  if( (defn=find_intrinsic(symt->name)) == NULL ) {
	     warning(id->line_num,id->col_num,
			"Unknown intrinsic function: ");
	     msg_tail(symt->name);
	     msg_tail("Treated as if user-defined");
				/* Here treat as if EXTERNAL declaration */
	     def_ext_name(id);
	     return;
	   }
	   else {
			/* Found in info table: set intrins flag and store
			   pointer to definition info. */
	     symt->intrinsic = TRUE;
	     symt->info.intrins_info = defn;
	   }
	}
	symt->declared_external = TRUE;
}/*def_intrins_name*/

void
def_namelist(id,list)		/* Process NAMELIST declaration */
     Token *id,*list;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) != NULL) {
	  syntax_error(id->line_num,id->col_num,
			"name is already in use");
	}
	else {
	  symt = install_local(id,h,type_NAMELIST,class_NAMELIST);
	  symt->info.toklist = NULL;
	  call_external(symt,id,list); /* attach list to symt->info.toklist */
	}

}/*def_namelist*/


void
def_namelist_item(id)		/* Process NAMELIST list elements */
	Token *id;
{
  ref_variable(id);		/* Put it in symtab */
}/*def_namelist_name*/


void				/* stub for future statement-label handler */
def_label(lab)
     Token *lab;
{
}

void
def_parameter(id,val)		/* Process parameter_defn_item */
	Token *id,*val;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}

	symt->set_flag = TRUE;
	symt->parameter = TRUE;

		/* Integer parameters: save value in symtab entry.  Other
		   types not saved.  Need these since used in array dims */
	switch(get_type(symt)) {
		case type_INTEGER:
			symt->info.int_value = int_expr_value(val);
#ifdef DEBUG_PARAMETERS
if(debug_latest)
fprintf(list_fd,"\nPARAMETER %s = %d",symt->name,symt->info.int_value);
#endif
			break;
			/* Character parameter: if declared adjustable
			   i.e. *(*) then inherit size of const */
		case type_STRING:
			if(symt->size == size_ADJUSTABLE
			   && datatype_of(val->class) == type_STRING)
			  symt->size = val->size;
			break;
		default:
			break;
	}

	if( highlight != -1 )	/* 13.05.96 Rigo */
	{
#if 1 /* Zsolt Koppany 14-oct-96 */
		put_symbol(PAF_CONS_DEF,
			NULL,
			hashtab[id->value.integer].name,
			current_filename,
			id->line_num,
			id->curr_index,
			0,0,
			(long)0,
			datatype_name(symt->type),NULL,NULL,
			get_comment(current_filename,id->line_num),
			0,0,0,0);
#else
		if( current_module_hash == -1 )
		{
			put_symbol(PAF_CONS_DEF,
					NULL,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)0,
					datatype_name(symt->type),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);
		}
		else
		{
			put_symbol(PAF_CONS_DEF,
					hashtab[current_module_hash].name,
					hashtab[id->value.integer].name,
					current_filename,
					id->line_num,
					id->curr_index,
					0,0,
					(long)0,
					datatype_name(symt->type),NULL,NULL,
					get_comment(current_filename,id->line_num),
					0,0,0,0);
		}
#endif /* Zsolt Koppany 14-oct-96 */
	}
}/*def_parameter*/



void    	       /* Installs statement function name in local table */
def_stmt_function(id, args)
	Token *id, *args;
{
	int t,h=id->value.integer;
	Lsymtab *symt;
   	TokenListHeader *TH_ptr;

   	if((symt = (hashtab[h].loc_symtab)) == NULL) {
			/* Symbol is new to local symtab: install it. */

	   symt = install_local(id,h,type_UNDECL,class_STMT_FUNCTION);
	   symt->info.toklist = NULL;
	}
	else {
	  if(storage_class_of(symt->type) == class_VAR) {
	    symt->info.toklist = NULL;
	  }
	}

		/* Save dummy arg list in symbol table */
    	TH_ptr= make_TL_head(id);

	TH_ptr->tokenlist = (args == NULL ? NULL: args->next_token);
	TH_ptr->next = symt->info.toklist;
	symt->info.toklist = TH_ptr;

		/* Reverse the token list for sake of checking phase */
	TH_ptr->tokenlist = reverse_tokenlist(TH_ptr->tokenlist);

	t=datatype_of(symt->type);
		/* Symbol seen before: check it & change class */

		/* check, check, check ... */
	if(storage_class_of(symt->type) == class_VAR)
	   symt->type = type_byte(class_STMT_FUNCTION,t);

	symt->external = TRUE;
}/*def_stmt_function*/




void
do_ASSIGN(id)		/* Process ASSIGN statement */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }
}/*do_ASSIGN*/




void
do_assigned_GOTO(id)		/* Process assigned_goto */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {
	   if(get_type(symt) != type_INTEGER) {
	      syntax_error(id->line_num,id->col_num,
		"Variable must be an integer: ");
	      msg_tail(symt->name);
	   }
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag)
	   equiv->used_before_set = TRUE;
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

}/*do_assigned_GOTO*/





void
do_ENTRY(id,args,hashno)	/* Processes ENTRY statement */
	Token *id,*args;
	int hashno;
{
	int datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
	    case type_COMMON_BLOCK:
	        syntax_error(id->line_num,NO_COL_NUM,
			"You cannot have an entry statement here");
		break;
	    case type_SUBROUTINE:	/* Subroutine entry */
		def_function(type_SUBROUTINE,size_DEFAULT,id,args);
		break;
	    default:		/* Function entry */
		def_function(type_UNDECL,size_DEFAULT,id,args);
		break;
	}
}/*do_ENTRY*/




	/* This routine checks whether a RETURN statement is valid at
	   the present location, and if it is, looks for possible
	   failure to assign return value of function.
	*/
void
do_RETURN(hashno,keyword)
	int hashno;	/* current module hash number */
	Token *keyword;	/* tok_RETURN, or tok_END if implied RETURN */
{
	int i,datatype;
	if(hashno == -1) {	/* -1 signifies headerless program */
	    datatype = type_PROGRAM;
	}
	else {
	    datatype = datatype_of(hashtab[hashno].loc_symtab->type);
	}
	switch(datatype) {
	    case type_PROGRAM:
	    case type_BLOCK_DATA:
		if(keyword->class == tok_RETURN)
		    syntax_error(keyword->line_num,keyword->col_num,
		    	"You cannot have a RETURN statement here!");
		break;
	    case type_SUBROUTINE:	/* Subroutine return: OK */
		break;
	    default:		/* Function return: check whether entry
				   points have been assigned values. */
		for(i=0; i<loc_symtab_top; i++) {
		    if(storage_class_of(loc_symtab[i].type) == class_VAR
			&& loc_symtab[i].entry_point
			&& ! loc_symtab[i].set_flag ) {
			    warning(keyword->line_num,keyword->col_num,
					loc_symtab[i].name);
			    msg_tail("not set when RETURN encountered");
		    }
		}
		break;
	}

}/*do_RETURN*/

void
equivalence(id1,id2)
     Token *id1, *id2;
{
	int h1=id1->value.integer, h2=id2->value.integer;
	Lsymtab *symt1,*symt2,*temp;

		/* install the variables in symtab if not seen before */
	if( (symt1=hashtab[h1].loc_symtab) == NULL) {
	   symt1 = install_local(id1,h1,type_UNDECL,class_VAR);
	}
	if( (symt2=hashtab[h2].loc_symtab) == NULL) {
	   symt2 = install_local(id2,h2,type_UNDECL,class_VAR);
	}
			/* Check for legality.  Ought to do complementary
			   checks elsewhere.
			 */
	if(symt1 == symt2
	   || symt1->parameter || symt2->parameter
	   || symt1->entry_point || symt2->entry_point
	   || symt1->argument || symt2->argument
	   || symt1->external || symt2->external) {

		syntax_error(id1->line_num,id1->col_num,
			     "illegal to equivalence these");
	}
		/* now swap equiv_links so their equiv lists are united */
	else {
	    temp = symt1->equiv_link;
	    symt1->equiv_link = symt2->equiv_link;
	    symt2->equiv_link = temp;
	}

#define XMAX(x,y) ((x)>(y)?(x):(y))

		/* If either guy is in common, both are in common */
	if(symt1->common_var || symt2->common_var) {
	    Lsymtab *equiv=symt1;
		int common_hash = -1;
		int common_orig_hash = -1;
	    do {
		equiv->common_var = TRUE;
		common_hash = XMAX( common_hash, equiv->common_hash );
		common_orig_hash = XMAX( common_orig_hash, equiv->common_orig_hash );
		equiv = equiv->equiv_link;
	    } while(equiv != symt1);

	    equiv=symt1;
	    do {
		equiv->common_hash = common_hash;
		equiv->common_orig_hash = common_orig_hash;
		equiv = equiv->equiv_link;
	    } while(equiv != symt1);
	}
}

int
get_size(symt,type) /* Returns size of symbol if explicitly declared
		       or declared using IMPLICIT type*size statement.
		       Otherwise returns size_DEFAULT. */
     Lsymtab *symt;
     int type;			/* Datatype: not used at present */
{
  int datasize=symt->size;
  if(datasize != size_DEFAULT)
    return datasize;		/* if declared, use it */
  else {
    int first_char= toupper((int)symt->name[0]);
#if ALLOW_DOLLARSIGNS
    if(first_char == '$')  first_char = 'Z'+1;
#endif
#if ALLOW_UNDERSCORES
    if(first_char == '_')  first_char = 'Z'+2;
#endif

    return implicit_size[first_char - 'A'];
  }
}

int
get_type(symt)	/* Returns data type of symbol, using implicit if necessary */
	Lsymtab *symt;
{
	int datatype = datatype_of(symt->type);

	if(datatype != type_UNDECL)	/* Declared? */
	   return datatype;		/*   Yes: use it */
	else if(storage_class_of(symt->type) == class_SUBPROGRAM
	     && !symt->invoked_as_func )
				/* Function never invoked: assume subr */
	   return type_SUBROUTINE;
	else {
	  int first_char=(int)symt->name[0];
	  if (islower(first_char))
	  {
		first_char = toupper(first_char);
	  }
#if ALLOW_DOLLARSIGNS
	  if(first_char == '$')  first_char = 'Z'+1;
#endif
#if ALLOW_UNDERSCORES
	  if(first_char == '_')  first_char = 'Z'+2;
#endif

	   return implicit_type[first_char - 'A'];
	}
}/*get_type*/


	/* hash_lookup finds identifier in hashtable and returns its
	   index.  If not found, a new hashtable entry is made for it,
	   and the identifier string s is copied to local stringspace.
	*/
unsigned
hash_lookup(s)
	char *s;
{
        unsigned h;
	unsigned long hnum;

	hnum = hash(s);

	while(h = hnum%HASHSZ, hashtab[h].name != NULL
	          && strcmp(hashtab[h].name,s) != 0) {
			  hnum = rehash(hnum);	/* Resolve clashes */
	}

	if(hashtab[h].name == NULL) {
		    hashtab[h].name = new_local_string(s);
		    hashtab[h].loc_symtab = NULL;
		    hashtab[h].glob_symtab = NULL;
		    hashtab[h].com_loc_symtab = NULL;
		    hashtab[h].com_glob_symtab = NULL;
		    hashtab[h].define = 0;
        }
	return h;
}/*hash_lookup*/

void
init_tables()			/* Allocates table space */
{
#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
	if( ((loc_symtab=(Lsymtab*)SN_calloc(LOCSYMTABSZ,sizeof(Lsymtab)))
		== (Lsymtab*)NULL) ||
	    ((glob_symtab=(Gsymtab*)SN_calloc(GLOBSYMTABSZ,sizeof(Gsymtab)))
		== (Gsymtab*)NULL) ||
	    ((hashtab=(HashTable*)SN_calloc(HASHSZ,sizeof(HashTable)))
		== (HashTable*)NULL) ||
	    ((strspace=(char*)SN_calloc(STRSPACESZ,sizeof(char)))
		== (char*)NULL) ||
	    ((tokenspace=(Token*)SN_calloc(TOKENSPACESZ,sizeof(Token)))
		== (Token*)NULL) ||
	    ((tokheadspace=
	      (TokenListHeader*)SN_calloc(TOKENSPACESZ,sizeof(TokenListHeader)))
		== (TokenListHeader*)NULL) ) {
	  oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
		       "Cannot malloc space for tables");
	}
#endif
}

void
init_globals()                	/* Clears the global symbol table */
{
	glob_str_bot = STRSPACESZ;
}/*init_globals*/



void
init_symtab()                     /* Clears the local symbol table */
{
	int i,h;
	unsigned long hnum;

	loc_symtab_top = 0;
	loc_str_top = 0;
	token_space_top = 0;
 	token_head_space_top = 0;

		      /* Clears the hash table */
	for(i=0;i<HASHSZ;i++) {
	    hashtab[i].name = NULL;
	    hashtab[i].loc_symtab = NULL;
	    hashtab[i].com_loc_symtab = NULL;
	    hashtab[i].glob_symtab = NULL;
	    hashtab[i].com_glob_symtab = NULL;
	}

		      /* Re-establishes global symbols */
	for(i=0;i<glob_symtab_top;i++) {
	    hnum = hash(glob_symtab[i].name);
	    while (h=hnum % HASHSZ, hashtab[h].name != NULL
	       && strcmp(hashtab[h].name,glob_symtab[i].name) != 0 ) {
	       hnum = rehash(hnum);
	    }
	    hashtab[h].name = glob_symtab[i].name;
	    if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK)
		hashtab[h].com_glob_symtab = &(glob_symtab[i]);
	    else
		hashtab[h].glob_symtab = &(glob_symtab[i]);

	}

		      /* Restores implicit typing to default values.
		         Note: 27 is '$', 28 is '_' which are default REAL */
	{
		int c;
		for( c=0; c<28; c++ ) {
	    	    implicit_type[c] = type_REAL;
		    implicit_size[c] = size_DEFAULT;
		}
		for( c='I'-'A'; c <= 'N'-'A'; c++ )
		    implicit_type[c] = type_INTEGER;
	}
}/*init_symtab*/



Gsymtab*
install_global(h,datatype,storage_class)	/* Install a global symbol */
	int h;			/* hash index */
	int datatype,storage_class;
{
	Gsymtab *gsymt = &glob_symtab[glob_symtab_top];

	if(glob_symtab_top == GLOBSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in global symbol table\n\
Recompile me with larger GLOBSYMTABSZ value\n"
#else
"out of space in global symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
			/* Store symtab pointer in hash table */
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_glob_symtab = gsymt;
	    else
		hashtab[h].glob_symtab = gsymt;

	    clear_symtab_entry(gsymt);

	 		/* Duplicate copy of string into global stringspace */
	    gsymt->name = new_global_string(hashtab[h].name);

			/* Set symtab info fields */
	    gsymt->type = type_byte(storage_class,datatype);
	    gsymt->size = type_size[datatype];
	    if(storage_class == class_COMMON_BLOCK)
		gsymt->info.comlist = NULL;
	    else
		gsymt->info.arglist = NULL;

	    gsymt->link.child_list = NULL;

	    ++glob_symtab_top;
	}
	return (gsymt);
}/*install_global*/


Lsymtab*
install_local(id,h,datatype,storage_class)	/* Install a local symbol */
	Token *id;
	int h;			/* hash index */
	int datatype,storage_class;
{
	Lsymtab *symt = &loc_symtab[loc_symtab_top];
	if(loc_symtab_top == LOCSYMTABSZ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of space in local symbol table\n\
Recompile me with larger LOCSYMTABSZ value\n"
#else
"out of space in local symbol table\n\
Recompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	else {
	    if(storage_class == class_COMMON_BLOCK)
		hashtab[h].com_loc_symtab = symt;
	    else
		hashtab[h].loc_symtab = symt;

	    clear_symtab_entry(symt);
		symt->common_hash = -1;
		symt->common_orig_hash = -1;
		symt->record_hash = -1;
		symt->line_num = id->line_num; /* rigo */
	    symt->name = hashtab[h].name;
	    symt->info.array_dim = 0;

		      /* Set symtab info fields */
	    symt->type = type_byte(storage_class,datatype);
	    symt->size = type_size[datatype];
	    symt->equiv_link = symt;	/* equivalenced only to self */
	    if(incdepth > 0)
	      symt->defined_in_include = TRUE;
	    ++loc_symtab_top;
	}
	return symt;
}/*install_local*/


		/* Get value specified by an integer-expression token.
		   This will be either an identifier, which should be a
		   parameter whose value is in the symbol table, or else
		   an expression token as propagated by exprtype.c
		   routines, with value stored in the token.
		*/
int
int_expr_value(t)
	Token *t;
{
  if(!is_true(EVALUATED_EXPR,t->subclass)) {/* something bogus */
				/* warn if error message not already given */
    if(is_true(PARAMETER_EXPR,t->subclass))
      warning(t->line_num,t->col_num,
	      "Constant not evaluated: value of 0 assumed");
  }
  else {
	if( is_true(ID_EXPR,t->subclass) ) {
		/* Identifier: better be a parameter */
	    int h=t->value.integer;
	    Lsymtab *symt = hashtab[h].loc_symtab;
	    if(symt == NULL || !(symt->parameter) ) {
		syntax_error(t->line_num,t->col_num,
			"symbolic constant required");
	    }
	    else {
		return symt->info.int_value;
	    }
	}
		/* Otherwise, it is a const or expr, use token.value.integer */
	else {
	    return t->value.integer;
	}
  }
				/* Unsuccessful: return value of 0 */
  return 0;
}/*int_expr_value*/


	/* Following routine converts a list of tokens into a list of type-
	   flag pairs. */

PRIVATE ArgListHeader *
make_arg_array(t)
	Token *t;		/* List of tokens */
{
	int i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
				 == (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for argument list");
	}
	s = t;		          /* List of tokens is in reverse order. */
	for(i=count-1; i>=0; i--){  /* Here we fill array in original order. */

	    arglist[i].type = s->class; /* use evaluated type, not symt */
	    arglist[i].size = s->size;
			/* Keep track of array and external declarations */
	    if( is_true(ID_EXPR,s->subclass) ){
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].info.array_dim = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		arglist[i].array_var = symt->array_var;
		arglist[i].declared_external = symt->declared_external;
	    }
	    else {
		arglist[i].info.array_dim = array_dim_info(0,1);
		arglist[i].array_var = FALSE;
		arglist[i].declared_external = FALSE;
	    }

	    arglist[i].array_element =
		arglist[i].array_var && !is_true(ARRAY_ID_EXPR,s->subclass);

	    if( is_true(LVALUE_EXPR,s->subclass) ){
		arglist[i].is_lvalue = TRUE;
			/* is_true(f,x) yields 0 or non-0: convert to 0 or 1 */
		arglist[i].set_flag =
			is_true(SET_FLAG,s->subclass)? TRUE: FALSE;
		arglist[i].assigned_flag =
			is_true(ASSIGNED_FLAG,s->subclass)? TRUE: FALSE;
		arglist[i].used_before_set =
			is_true(USED_BEFORE_SET,s->subclass)? TRUE: FALSE;
	    }
	    else {	/* it is an expression or constant, not an lvalue */
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = TRUE;
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
	    }
	    s = s->next_token;
	}
	alhead->numargs = count;
	alhead->is_defn = FALSE;
	alhead->is_call = TRUE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_arg_array */


	/* Following routine converts a list of common block tokens
	    into a list of dimen_info-type pairs. */

PRIVATE ComListHeader *
make_com_array(t)
	Token *t;		/* List of tokens */
{
	Token *s;
	Lsymtab *symt;
	int h, i;
	unsigned count;
	ComListHeader *clhead;
	ComListElement *comlist;

	count = arg_count(t);
	if(((clhead=new_comlistheader())
		 == (ComListHeader *) NULL) ||
	  (count != 0 &&
 	  ((comlist=new_comlistelement(count))
		 == (ComListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for common list");
	}
	s = t;
	for(i=count-1; i>=0; i--){
	   h = s->value.integer;
	   symt = hashtab[h].loc_symtab;
	   if( (comlist[i].dimen_info = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
	     comlist[i].dimen_info = array_dim_info(0,1);
       	   comlist[i].type = get_type(symt);
	   comlist[i].size = get_size(symt,(int)comlist[i].type);
	   comlist[i].used = symt->used_flag;
	   comlist[i].set = symt->set_flag;
	   comlist[i].used_before_set = symt->used_before_set;
	   comlist[i].assigned = symt->assigned_flag;
	   if (comlist[i].used)
		clhead->any_used = TRUE;
	   if (comlist[i].set)
		clhead->any_set = TRUE;
	   s = s->next_token;
	}
	clhead->numargs = count;
	if (count == 0)
		clhead->com_list_array = NULL;
	else
		clhead->com_list_array = comlist;
	return(clhead);
} /* make_com_array */


PRIVATE ArgListHeader *
make_dummy_arg_array (t)
	Token *t;		/* List of tokens */
{
	int i;
	unsigned count;
	Token *s;
	ArgListElement *arglist;
	ArgListHeader *alhead;

	count = arg_count(t);
	if(((alhead=new_arglistheader())
			 == (ArgListHeader *) NULL) ||
	  (count != 0 &&
          ((arglist=new_arglistelement(count))
			== (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for dummy argument list");
	}
	s = t;		          /* List of tokens is in reverse order. */
	for(i=count-1; i>=0; i--){  /* Here we fill array in original order. */
	    if( is_true(ID_EXPR,s->subclass) ){
	        int implied_type;
		int h = s->value.integer;
		Lsymtab *symt = hashtab[h].loc_symtab;
		if( (arglist[i].info.array_dim = symt->info.array_dim) == 0)
				/* change scalars to 0 dims, size 1 */
		  arglist[i].info.array_dim = array_dim_info(0,1);
		implied_type = get_type(symt);
		arglist[i].type = type_byte(storage_class_of(symt->type),
						implied_type);
		arglist[i].size = get_size(symt,implied_type);
		arglist[i].is_lvalue = TRUE;
		arglist[i].set_flag = symt->set_flag;
		arglist[i].assigned_flag = symt->assigned_flag;
		arglist[i].used_before_set = symt->used_before_set;
		arglist[i].array_var = symt->array_var;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = symt->declared_external;
	    }
	    else {	/* It is a label */
		arglist[i].info.array_dim = 0;
		arglist[i].type = s->class;
		arglist[i].size = 0;
		arglist[i].is_lvalue = FALSE;
		arglist[i].set_flag = FALSE;	/* Don't currently do labels */
		arglist[i].assigned_flag = FALSE;
		arglist[i].used_before_set = FALSE;
		arglist[i].array_var = FALSE;
		arglist[i].array_element = FALSE;
		arglist[i].declared_external = FALSE;
	    }
	    s = s->next_token;
	}
	alhead->numargs = count;
	alhead->is_defn = TRUE;
	alhead->is_call = FALSE;
	alhead->external_decl = FALSE;
	alhead->actual_arg = FALSE;

        if (count == 0)
		alhead->arg_array = NULL;
	else
		alhead->arg_array = arglist;
	return(alhead);
}/* make_dummy_arg_array */


	/* This routine makes an empty argument list: used for
	   EXTERNAL declarations of subprograms. */
PRIVATE ArgListHeader *
make_arrayless_alist()
{
	ArgListHeader *alhead;

	if(((alhead=new_arglistheader())
		 		 == (ArgListHeader *) NULL) ) {
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of malloc space for external decl");
	}

	alhead->numargs = 0;
	alhead->is_defn = FALSE;
	alhead->is_call = FALSE;
	alhead->arg_array = NULL;

	return(alhead);
}/* make_arrayless_arglist */

PRIVATE TokenListHeader *	/* Initializes a tokenlist header */
make_TL_head(t)
     Token *t;
{
  TokenListHeader *TH_ptr;
  if(token_head_space_top == TOKENSPACESZ) {
		oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
			   "Out of space for token list header");
  }
  TH_ptr= tokheadspace + token_head_space_top++;

	TH_ptr->line_num = t->line_num;
  	TH_ptr->filename = current_filename;
				/* Clear all the flags */
	TH_ptr->external_decl = FALSE;
	TH_ptr->actual_arg = FALSE;
	TH_ptr->tokenlist = NULL;
	TH_ptr->next = NULL;

  return TH_ptr;
}

		/* These routines allocate memory for arglist and comlist
		   headers and arrays.  For Turbo C the calloc calls limit
		   out at about 50KB, so a workaround uses separate modules
		   that allocate space from static arrays.  */
#ifndef T_ALLOC

ArgListElement *
new_arglistelement(count)
     unsigned count;
{
	
  arglist_element_used += count;	/* For -resources */
  return (ArgListElement *) SN_calloc(count,sizeof(ArgListElement));
}

ArgListHeader *
new_arglistheader()
{
  arglist_head_used++;
  return (ArgListHeader *) SN_calloc(1, sizeof(ArgListHeader));
}

ComListElement *
new_comlistelement(count)
     unsigned count;
{
  comlist_element_used += count;
  return SN_calloc(count,sizeof(ComListElement));
}

ComListHeader *
new_comlistheader()
{
  comlist_head_used++;
  return SN_calloc (1, sizeof(ComListHeader));
}

#endif /*T_ALLOC*/

		/* this routine allocates room in global part (top down)
		   of stringspace for string s, and copies it there */
char *
new_global_string(s)
	char *s;
{
	glob_str_bot -= strlen(s) + 1;    /*pre-decrement*/
	if( glob_str_bot < loc_str_top ) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of global stringspace\nRecompile me with larger STRSPACESZ value\n"
#else
"out of global stringspace\nRecompile me with LARGE_MACHINE option\n"
#endif
		);
	}
	return strcpy(strspace+glob_str_bot,s);
}/*new_global_string*/

		/* Allocate space for string s in local (bottom up)
		   string space, and copy it there */
char *
new_local_string(s)
	char *s;
{
	char *start = strspace + loc_str_top;
	loc_str_top += strlen(s) + 1;	/* post-increment */
	if(loc_str_top > glob_str_bot) {
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"out of local stringspace\nRecompile me with larger STRSPACESZ value\n"
#else
"out of local stringspace\nRecompile me with LARGE_MACHINE option\n"
#endif
		);
	}

	return strcpy(start,s);
}/* new_local_string */

Token *
new_token()			/* Returns pointer to space for a token */
{
  if(token_space_top == TOKENSPACESZ)
    return (Token *)NULL;
  else
    return tokenspace + token_space_top++;
}

	/* note_filename():  This routine is called by main prog to give
	   symbol table routines access to current input file name, to be
	   stored in function arg list headers and common list headers, for
	   the use in diagnostic messages. Since filenames are from argv,
	   they are permanent, so pointer is copied, not the string.
	*/
void
note_filename(s)
	char *s;
{
	current_filename = s;
	top_filename = s;
}/* note_filename */



void
process_lists(curmodhash)  /* Places pointer to linked list of arrays in
			      global symbol table */
	int curmodhash;    /* current_module_hash from fortran.y */
{
	int i, h;
	unsigned long hnum;
	Gsymtab *curr_gsymt;

	Gsymtab *gsymt;
	TokenListHeader *head_ptr;

	if( (curr_gsymt=
	     (curmodhash == -1) ? NULL:hashtab[curmodhash].glob_symtab)
	   == NULL) {
	  oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		  "module not in global symtab:");
	  oops_tail(hashtab[curmodhash].name);
	}
	else {
	  if(curr_gsymt->internal_entry) {/* protect ourself */
	    warning(NO_LINE_NUM,NO_COL_NUM,
		    "entry point redefined as module");
	    msg_tail(curr_gsymt->name);
	    msg_tail(": previous definition overridden");
	    curr_gsymt->link.child_list = NULL;
	  }
	  curr_gsymt->internal_entry = FALSE;
	}

	for (i=0; i<loc_symtab_top; i++){
				/* Skip things which are not true externals */
	    if(loc_symtab[i].argument || loc_symtab[i].intrinsic ||
		   loc_symtab[i].array_var)
		      continue;

	    head_ptr = loc_symtab[i].info.toklist;

	    hnum=hash(loc_symtab[i].name);
	    while(h=hnum%HASHSZ,hashtab[h].name != NULL
		 && strcmp(hashtab[h].name,loc_symtab[i].name)!=0){
		      hnum = rehash(hnum);      /* Resolve clashes */
	    }

	    switch (storage_class_of(loc_symtab[i].type)){
		    case class_COMMON_BLOCK:
			if(head_ptr != NULL) {
if((gsymt=hashtab[h].com_glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
		 "common block not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
			Token *tok_ptr;
                        ComListHeader *c;

				/* First we link up possibly multiple
				   declarations of the same common block
				   in this module into one big list */
		    	while (tok_ptr = head_ptr->tokenlist,
			       (head_ptr = head_ptr->next) != NULL){
			    while(tok_ptr->next_token != NULL){
			        tok_ptr = tok_ptr->next_token;
			    }
			    tok_ptr->next_token = head_ptr->tokenlist;
			}

				/* Now make it into array for global table */
		        c=make_com_array(loc_symtab[i].info.toklist->tokenlist);
			c->module = curr_gsymt;
			c->line_num = loc_symtab[i].info.toklist->line_num;
			c->filename = loc_symtab[i].info.toklist->filename;
			c->topfile = top_filename;
			c->saved = global_save || loc_symtab[i].saved;

                        c->next = gsymt->info.comlist;
			gsymt->info.comlist = c;
		/* Replace token list by comlist for project file use */
			loc_symtab[i].info.comlist = c;
}
			}/* end if(head_ptr != NULL) */

		        break;	/* end case class_COMMON_BLOCK */


			/* Are we inside a function or subroutine? */
		    case class_VAR:
		       if(loc_symtab[i].entry_point) {
if((gsymt=hashtab[h].glob_symtab) == NULL) {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                          ArgListHeader *a;
			  int implied_type;

				/* Make each token list into an array of
				   args for global table */
			  while (head_ptr != NULL){
			     a=make_dummy_arg_array(head_ptr->tokenlist);
			     implied_type = get_type(&(loc_symtab[i]));
			     a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			     a->size = get_size(&(loc_symtab[i]),implied_type);
			     a->module = curr_gsymt;
			     a->filename = head_ptr->filename;
			     a->topfile = top_filename;
			     a->line_num = head_ptr->line_num;

			     a->next = gsymt->info.arglist;
			     gsymt->info.arglist = a;
			/* store arglist in local symtab for project file */
			     loc_symtab[i].info.arglist = a;
			     head_ptr = head_ptr->next;
		          }/* end while (head_ptr != NULL) */

			  if(loc_symtab[i].set_flag)
			         gsymt->set_flag = TRUE;
			  if(loc_symtab[i].used_flag)
			         gsymt->used_flag = TRUE;
			  if(loc_symtab[i].declared_external)
				 gsymt->declared_external = TRUE;
			  if(loc_symtab[i].library_module)
				 gsymt->library_module = TRUE;
			  if(gsymt != curr_gsymt) {
			    gsymt->internal_entry = TRUE;
			    gsymt->link.module = curr_gsymt;
			  }
}
			}/* end if(loc_symtab[i].entry_point) */

		    	break; /* end case class_VAR */

                    case class_SUBPROGRAM:
if((gsymt=hashtab[h].glob_symtab) == NULL)
 {
    oops_message(OOPS_NONFATAL,NO_LINE_NUM,NO_COL_NUM,
    "subprog not in global symtab:");
    oops_tail(loc_symtab[i].name);
}
else {
                        ArgListHeader *a;
			int implied_type;
			while (head_ptr != NULL){
			  if(head_ptr->external_decl || head_ptr->actual_arg)
			    a=make_arrayless_alist();
			  else
			    a=make_arg_array(head_ptr->tokenlist);

			  implied_type = get_type(&(loc_symtab[i]));
			  a->type = type_byte(
			         class_SUBPROGRAM,implied_type);
			  a->size = get_size(&(loc_symtab[i]),implied_type);
			  a->module = curr_gsymt;
			  a->filename = head_ptr->filename;
			  a->topfile = top_filename;
			  a->line_num = head_ptr->line_num;
			  a->external_decl = head_ptr->external_decl;
			  a->actual_arg = head_ptr->actual_arg;

			  a->next = gsymt->info.arglist;
			  gsymt->info.arglist = a;
		/* put arglist into local symtab for project file use */
			  loc_symtab[i].info.arglist = a;
			  head_ptr = head_ptr->next;
		        }
			if(loc_symtab[i].used_flag)
			        gsymt->used_flag = TRUE;
if(debug_glob_symtab)
fprintf(list_fd,"\nmodule %s local used=%d global used=%d",
gsymt->name,loc_symtab[i].used_flag,gsymt->used_flag);
}
				/* Add this guy to linked list of children,
				   unless never actually used. */
			if(loc_symtab[i].used_flag) {
			  ChildList *node=
			    (ChildList *)SN_calloc(1,sizeof(ChildList));
			  node->child = gsymt;
			  node->next = curr_gsymt->link.child_list;
			  curr_gsymt->link.child_list = node;
			}

			break;/* end case class_SUBPROGRAM*/

	    }/* end switch */

        }/* end for (i=0; i<loc_symtab_top; i++) */

}/* process_lists */


void
ref_array(id,subscrs)   /* Array reference: install in symtab */
	Token *id, *subscrs;
{
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;

	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
		       "undeclared variable has dim info:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else{    /* check that subscrs match dimension info */


	  if(arg_count(subscrs->next_token)!=array_dims(symt->info.array_dim)){
	      syntax_error(subscrs->line_num,subscrs->col_num,
			"array");
	      msg_tail(symt->name);
	      msg_tail("referenced with wrong no. of subscripts");
	  }
	}
}/* ref_array */

void
ref_namelist(id,stmt_class)
     Token *id;
     int stmt_class;
{
	Token *t;
	TokenListHeader *toklist;
	int h=id->value.integer;
	Lsymtab *symt=hashtab[h].loc_symtab;
	if(symt == NULL){
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"undeclared identifier is a namelist:");
	   oops_tail(hashtab[h].name);
	   symt = install_local(id,h,type_NAMELIST,class_NAMELIST);
	   symt->info.toklist = NULL;
	}

			/* Go thru token list of namelist variables,
			   setting flags appropriately. */
	toklist = symt->info.toklist;
	if (toklist != NULL){
	    t = toklist->tokenlist;
	    while(t != NULL){
	        if(stmt_class == tok_READ)
		  use_lvalue(t);
		else
		  use_variable(t);
		t = t->next_token;
	    }
	}
}

void
ref_variable(id)	/* Variable reference: install in symtab */
	Token *id;
{
	int h=id->value.integer;

	if( hashtab[h].loc_symtab == NULL) {
	   (void) install_local(id,h,type_UNDECL,class_VAR);
	}

}/*ref_variable*/

		/* this guy reverses a tokenlist and returns a pointer
		   to the new head. */
PRIVATE Token *
reverse_tokenlist(t)
	Token *t;
{
	Token *curr,*next,*temp;

	if(t == NULL)
	    return t;

	curr = t;
	next = curr->next_token;
	while(next != NULL) {
		temp = next->next_token;
		next->next_token = curr;
		curr = next;
		next = temp;
	}
	t->next_token = NULL;		/* former head is now tail */
	return curr;			/* curr now points to new head */
}

void
save_com_block(id)	/* Process SAVEing of a common block */
	Token *id;	/* N.B. Legality checking deferred to END */
{
	int h=id->value.integer;
	Lsymtab *symt;

			/* N.B. SAVE does not create a global table entry */
	if( (symt = hashtab[h].com_loc_symtab) == NULL){
	   symt = install_local(id,h,type_COMMON_BLOCK,class_COMMON_BLOCK);
	   symt->info.toklist = NULL;
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else
	  symt->saved = TRUE;
}

void
save_variable(id)	/* Process SAVEing of a variable */
	Token *id;	/* N.B. Legality checking deferred to END */
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}

	if(symt->saved) {
	  syntax_error(id->line_num,id->col_num,
		       "redundant SAVE declaration");
	}
	else {		/* set flags for all equivalenced vars */
	  Lsymtab *equiv=symt;
	  do{
	    equiv->saved = TRUE;
	    equiv = equiv->equiv_link;
	  } while(equiv != symt);
	}
}

	/* Following routine sets the implicit typing of characters in
	   range c1 to c2 to the given type. */
void
set_implicit_type(type,size,c1,c2)
	int type;		/* Data type of IMPLICIT declaration */
        long size;		/* Type size or size_DEFAULT if not given */
	int c1;			/* First character of range */
	int c2;			/* Last character of range */
{
	int c;
#if ALLOW_DOLLARSIGNS
	  if(c1 == '$')  c1 = 'Z'+1;
	  if(c2 == '$')  c2 = 'Z'+1;
#endif
#if ALLOW_UNDERSCORES
	  if(c1 == '_')  c1 = 'Z'+2;
	  if(c2 == '_')  c2 = 'Z'+2;
#endif
	if(c2 < c1) {
		yyerror("IMPLICIT range must be in alphabetical order");
	}
	else {
		/* Fill in the lookup table for the given range of chars */
	  for(c=c1; c<=c2; c++) {
		implicit_type[c-'A'] = type;
		implicit_size[c-'A'] = size;
	  }
	}
}/*set_implicit_type*/

		/* Finish processing statement function.
		   Clears all used-before-set flags of ordinary
		   variables. Reason: statement functions are processed
		   like assignment to an array element, setting ubs flags.
		   At this point, no valid setting of ubs flags should
		   be possible, so clearing them will elim false messages.*/
void
stmt_function_stmt(id)
     Token *id;			/* Not used at present */
{
    int i;
    for(i=0; i<loc_symtab_top; i++) {
	if(storage_class_of(loc_symtab[i].type) == class_VAR &&
	   ! loc_symtab[i].parameter )
	  loc_symtab[i].used_before_set = FALSE;
    }
}/*stmt_function_stmt(id)*/

char *
token_name(t)
	Token t;
{
	return hashtab[t.value.integer].name;
}/*token_name*/




void
use_actual_arg(id)	/* like use_lvalue except does not set assigned_flag */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {
			/* If an external other than an intrinsic, set up
			   tokenlist for "call".  If intrinsic, check
			   legality of this usage.) */
	  if(storage_class_of(symt->type) == class_SUBPROGRAM) {
	    if(symt->intrinsic) {
	      IntrinsInfo *defn = symt->info.intrins_info;
	      if( !(symt->declared_intrinsic) ) {
		warning(id->line_num,id->col_num,
				defn->name);
		msg_tail("not declared INTRINSIC");
	      }
	      if( (defn->intrins_flags&I_NOTARG) ) {
		syntax_error(id->line_num,id->col_num,
				defn->name);
		msg_tail("intrinsic function cannot be a subprogram argument");
	      }
	    }
	    else {		/* External subprogram as actual arg */
	      TokenListHeader *TH_ptr;
	      TH_ptr= make_TL_head(id);

	      TH_ptr->actual_arg = TRUE;
	      TH_ptr->next = symt->info.toklist;
	      symt->info.toklist = TH_ptr;
	    }
	  }
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

	report( id, PAF_REF_PASS );

}/*use_actual_arg*/


void
use_function_arg(id)	/* Like use_variable but invokes use_actual_arg
			   if id is an external (subprogram) passed as
			   arg of a function. This routine is used when
			   pure_functions flag is set. */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}

	if(storage_class_of(symt->type) == class_SUBPROGRAM)
	  use_actual_arg(id);
	else
	  use_variable(id);

}/*use_function_arg*/

void
use_implied_do_index(id)
	Token *id;
{
		/* Like use_lvalue and use_variable but clears ubs flag.
	           This is because we cannot handle used-before-set
		   properly in this case, and the odds are that ubs
		   was set in the preceding I/O list. */
	int h=id->value.integer;
	Lsymtab *symt;

	use_lvalue(id);
	use_variable(id);
	symt=hashtab[h].loc_symtab;

	symt->used_before_set = FALSE;
}/*use_implied_do_index*/


	/* use_io_keyword handles keyword=value fields in i/o control lists */

#include "iokeywds.h"

void
use_io_keyword(keyword,value,stmt_class)
     Token *keyword,*value;
     int stmt_class;
{
    int i, k, stmt_flag=0, type_flag, setit,useit;
    int hkey=keyword->value.integer;

		/* Convert statement_class (a token class) into
		   a bit flag compatible with io_keywords table. */
    for(i=0; i<NUM_IO_STMTS; i++) {
	if(local_class[i].stmt_class == stmt_class) {
	    stmt_flag = local_class[i].stmt_flag;
	    break;
	}
    }
    if(stmt_flag == 0) {
      oops_message(OOPS_NONFATAL,keyword->line_num,keyword->col_num,
	"not an i/o statement class:");
      fprintf(stderr,"%d",stmt_class);
      return;
    }
		/* Convert value datatype into
		   a bit flag compatible with io_keywords table.
		   Note that '*' is handled by using type_UNDECL */
    if(value->class == '*')
	type_flag = STAR;
    else
	type_flag = (1<<datatype_of(value->class));

				/* Look up keyword in table*/
    k = find_io_keyword(hashtab[hkey].name);

		/* Not found or nonstandard: issue warning.  Note
		   that not-found is also nonstandard. */
    if(io_keywords[k].nonstandard
#ifdef VMS_IO /* special VMS case: OPEN(...,NAME=str,...) */
       || (io_keywords[k].special && stmt_flag==OP)
#endif /*VMS_IO*/
	   ) {
		/* If nonstandard and -f77 flag given, issue warning */
	if(f77_standard) {
	    nonstandard(keyword->line_num,keyword->col_num);
	}
	if(io_keywords[k].name == NULL) {
	    if(f77_standard) {	/* abbrev warning if nonstd message given */
		msg_tail(": unrecognized keyword");
	    }
	    else {
		warning(keyword->line_num,keyword->col_num,
		"Unrecognized keyword");
	    }
	    msg_tail(hashtab[hkey].name);
	    msg_tail("--  Ftnchek may process incorrectly");
	}
    }

	/* If label expected, switch integer const to label */
    if( (LAB & io_keywords[k].allowed_types)
       &&  (type_flag == INT && is_true(LIT_CONST,value->subclass))) {
	type_flag = LAB;
    }

	/*  Now check it out */


		/* Check if keyword is allowed with statement */

    if(!(stmt_flag & io_keywords[k].allowed_stmts)) {
	syntax_error(keyword->line_num,keyword->col_num,
		     "keyword illegal in this context");
	return;
    }

		/* Check if the type is OK */

    if( !(type_flag & io_keywords[k].allowed_types) ) {
	syntax_error(value->line_num,value->col_num,
		     "control specifier is incorrect type");
	return;
    }


	/* Now handle usage */

				/* internal file?: WRITE(UNIT=str,...) */
    if(stmt_flag == WR && type_flag == CHR
	    && io_keywords[k].allowed_types == UID) {
	setit = TRUE;
	useit = FALSE;
    }
				/* INQUIRE: set it if inquire_set flag true */
    else if(stmt_flag == INQ && io_keywords[k].inquire_set) {
	setit = TRUE;
	useit = FALSE;
    }
				/* otherwise use use/set flags in table */
    else {
	useit = io_keywords[k].implies_use;
	setit = io_keywords[k].implies_set;
    }

			/* Handle NML=namelist */
    if(type_flag == NML){
      ref_namelist(value,stmt_class);
    }
			/* Update usage status if a variable. */
    if(useit) {
	if( is_true(ID_EXPR,value->subclass)) {
	    use_variable(value);
	}
    }
    if(setit) {			/* if value is set, must be an lvalue */
	if( is_true(ID_EXPR,value->subclass)) {
	    use_lvalue(value);
	}
	else {
	    syntax_error(value->line_num,value->col_num,
			 "variable required");
	    return;
	}
    }
}


		/* Handle VMS OPEN keywords that have no =value */
void
use_special_open_keywd(id)
     Token *id;
{
#ifdef VMS_IO
  int i;
  char *id_name= hashtab[id->value.integer].name;

  for(i=0; i<NUM_SPECIAL_OPEN_KEYWDS; i++) {
    if(strcmp(id_name,special_open_keywds[i]) == 0) {
				/* found: report nonstandard if requested */
      if(f77_standard)
	nonstandard(id->line_num,id->col_num);
      return;
    }
  }
#endif/*VMS_IO*/
				/* not found or not VMS: report error */
  syntax_error(id->line_num,id->col_num,
	       "Illegal control-list item");
}

void
use_lvalue(id)	/* handles scalar lvalue */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;
	if((symt=hashtab[h].loc_symtab) == NULL) {
	    symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	else {
	  /*   check match to previous invocations and update  */
	}
    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	equiv->set_flag = TRUE;
	equiv->assigned_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

	if( ! symt->parameter )	/* 13.05.96 Rigo */
	{
		report( id, PAF_REF_WRITE );
	}

}/*use_lvalue*/



void                    /* Process data_constant_value & data_repeat_factor */
use_parameter(id)
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}
	if(! symt->parameter) {
		syntax_error(id->line_num,id->col_num,
			"must be a parameter");
		symt->parameter = TRUE;
	}

	if(! symt->set_flag) {
	   symt->used_before_set = TRUE;
	}
	symt->used_flag = TRUE;

}/*use_parameter*/


void
use_variable(id)		/* Set the use-flag of variable. */
	Token *id;
{
	int h=id->value.integer;
	Lsymtab *symt;

	if( (symt=hashtab[h].loc_symtab) == NULL) {
	   symt = install_local(id,h,type_UNDECL,class_VAR);
	}

    {		/* set flags for all equivalenced vars */
      Lsymtab *equiv=symt;
      do{
	if(! equiv->set_flag) {
	   equiv->used_before_set = TRUE;
	}
	equiv->used_flag = TRUE;
	equiv = equiv->equiv_link;
      } while(equiv != symt);
    }

	report( id, PAF_REF_READ );

}/*use_variable*/


/*  End of symtab.c */

/*

 II. Hash

*/

/*    hash.c:
 	performs a hash function

This was formerly a separate file.

*/

extern int sixclash;	/* flag to check clashes in 1st 6 chars of name */

unsigned long
hash(s)
    char *s;
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);
    if(sixclash && n > 6) n = 6;

    while (i < n) {
         wd = 0;
         for(j=1; j <= sizeof(long) && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (sizeof(long) - j) * 8;}

	sum ^= wd;}
    return sum;
}

		/* Same as hash() but always uses full length of keyword.
		   To keep the keyword table clash-free on any machine,
		   packs only 4 bytes per word even if long is bigger */
unsigned long
kwd_hash(s)
    char *s;
{
    unsigned long sum = 0, wd;
    int i = 0,j;

    int n = strlen(s);

    while (i < n) {
         wd = 0;
         for(j=1; j <= 4 && i < n; i++,j++) {
            wd += (unsigned long)(s[i] & 0xff) << (4 - j) * 8;}

	sum ^= wd;}
    return sum;
}



/*    rehash.c
        performs a rehash for resolving clashes.
*/

#ifdef COUNT_REHASHES
unsigned long rehash_count=0;
#endif

unsigned long
rehash(hnum)
    unsigned long hnum;
{
#ifdef COUNT_REHASHES
    rehash_count++;
#endif
    return hnum+1;
}


/*  End of hash */


/*

III. Intrins

*/

/* intrinsic.c:

	Handles datatyping of intrinsic functions.
*/


	/* File intrinsic.h contains information from Table 5, pp. 15-22
	   to 15-25 of the standard.  Note: num_args == -1 means 1 or 2 args,
	   num_args == -2 means 2 or more args.  Value of arg_type is the OR
	   of all allowable types (I, R, etc. as defined above).  Value of
	   result_type is type returned by function (type_INTEGER, etc.).
	   If result_type is type_GENERIC, function type is same as arg type.
	*/


IntrinsInfo intrinsic[]={
#include "intrinsics.h"
};

#define NUM_INTRINSICS (sizeof(intrinsic)/sizeof(intrinsic[0]))

#define EMPTY 255

unsigned char intrins_hashtab[INTRINS_HASHSZ];

/*    init_intrins_hashtab:
                 Initializes the intrinsic hash table by clearing it to EMPTY
                 and then hashes all the intrinsic names into the table.
*/

unsigned long
init_intrins_hashtab()
{
    unsigned i,h;
    unsigned long hnum;
    unsigned long numclashes=0;

    for(h=0;h<INTRINS_HASHSZ;h++) {
           intrins_hashtab[h] = EMPTY;
    }
    for(i=0; i < NUM_INTRINSICS; i++) {
	   hnum = kwd_hash(intrinsic[i].name);
	   while(h=hnum%INTRINS_HASHSZ, intrins_hashtab[h] != EMPTY) {
		hnum = rehash(hnum);
		numclashes++;
	   }
	   intrins_hashtab[h] = i;
    }
    return numclashes;
}

	/* Function to look up an intrinsic function name in table.
	   If found, returns ptr to table entry, otherwise NULL.
	*/
PRIVATE IntrinsInfo *
find_intrinsic(s)
	char *s;			/* given name */
{
	unsigned i, h;
	unsigned long hnum;

	hnum = kwd_hash(s);
	while( h=hnum%INTRINS_HASHSZ, (i=intrins_hashtab[h]) != EMPTY &&
		strcmp(s,intrinsic[i].name) != 0) {
			hnum = rehash(hnum);
	}
	if(i != EMPTY) {
	    return &intrinsic[i];
	}
	else
	    return (IntrinsInfo *)NULL;
}

	/* find_io_keyword looks up an i/o keyword in io_keywords
	   table and returns its index.  Uses simple linear search
	   since not worth hash overhead.  If not found, returns
	   index of last element of list, which is special. */
PRIVATE int
find_io_keyword(s)
     char *s;			/* given name */
{
    int i;
    for(i=0; io_keywords[i].name != NULL; i++) {
	if(strcmp(io_keywords[i].name, s) == 0) {
	    break;
	}
    }
    return i;
}


#ifdef DEVELOPMENT
print_sizeofs()			/* For development: print sizeof for
				   various data structures */
{
#define PrintObjSize(OBJ) fprintf(list_fd,#OBJ " size = %d\n",sizeof(OBJ))
  PrintObjSize(Token);
  PrintObjSize(Lsymtab);
  PrintObjSize(Gsymtab);
  PrintObjSize(HashTable);
  PrintObjSize(ArgListHeader);
  PrintObjSize(ArgListElement);
  PrintObjSize(ComListHeader);
  PrintObjSize(ComListElement);
  PrintObjSize(TokenListHeader);
  PrintObjSize(InfoUnion);
  PrintObjSize(IntrinsInfo);
  PrintObjSize(ChildList);
}
#endif


static char *datatype_name( int datatype )
{
	switch( datatype )
	{
		case type_UNDECL        : return "UNDECL"           ;
		case type_INTEGER       : return "INTEGER"          ;
		case type_REAL          : return "REAL"             ;
		case type_COMPLEX       : return "COMPLEX"          ;
		case type_LOGICAL       : return "LOGICAL"          ;
		case type_DCOMPLEX      : return "DOUBLE COMPLEX"   ;
		case type_DP            : return "DOUBLE PRECISION" ;
		case type_RECORD        : return "RECORD"           ;
		case type_STRING     	: return "STRING"           ;
		case type_POINTER       : return "POINTER"          ;
		case type_SUBROUTINE    : return "SUBROUTINE"       ;
		default                 : return "unknown"          ;
	}
}

static void report( Token *id, int typ )
{
	if( highlight == -1 )
		return;

	if( hashtab[id->value.integer].define )
	{
		return;
	}
	
	if( hashtab[id->value.integer].loc_symtab->common_var )
	{
		char *name;
		char *orig_name;
		char	buf[1000];
		int h;

		if(( h=hashtab[id->value.integer].loc_symtab->common_hash ) != -1 )
		{
			name = hashtab[h].name;
		}
		else
		{
			name = "%BLANK";
		}
#define ORIG_NAME
#ifdef ORIG_NAME
		if(( h=hashtab[id->value.integer].loc_symtab->common_orig_hash ) != -1 )
		{
			orig_name = hashtab[h].name;
		}
		else
#endif
		{
			orig_name = 0;
		}
		buf[0] = '\0';
		report_name(orig_name,id,buf);

		put_cross_ref(PAF_REF_TO_COMM_VAR,
			cross_scope_type,
			PAF_REF_SCOPE_GLOBAL,
			NULL,
			hashtab[current_module_hash].name,
			NULL,
			name,
			buf,
			NULL,
			current_filename,
			id->line_num,
			typ);

		if( hashtab[id->value.integer].loc_symtab->record_hash != -1 )
		{
			char	class_name[500];
			char	cvar_name[500];

			class_name[0] = '\0';
			cvar_name[0] = '\0';
			report_class_name(id,class_name,cvar_name);

			put_cross_ref(PAF_REF_TO_MBR_VAR,
				cross_scope_type,
				PAF_REF_SCOPE_GLOBAL,
				NULL,
				hashtab[current_module_hash].name,
				NULL,
				class_name,
				cvar_name,
				NULL,
				current_filename,
				id->line_num,
				typ);
		}
	}
	else if( report_local_vars )
	{
		char	buf[1000];

		buf[0] = '\0';
		report_name(0,id,buf);

		put_cross_ref(PAF_REF_TO_LOCAL_VAR,
			cross_scope_type,
			PAF_REF_SCOPE_LOCAL,
			NULL,
			hashtab[current_module_hash].name,
			NULL,
			hashtab[current_module_hash].name,
			buf,
			NULL,
			current_filename,
			id->line_num,
			typ);
	}
}

static void report_name( char *name, Token *id, char *buf)
{
	char	*pna;

	if( name ) pna = name;
	else       pna = hashtab[id->value.integer].name;

	strcat(buf,pna);

	if( id->dot_token )
	{
		strcat(buf,".");
		report_name(0,id->dot_token,buf);
	}
}

static void report_class_name( Token *id, char *cname, char *cvar)
{
	int h;
	char	*pna;

	if(( h = hashtab[id->value.integer].loc_symtab->record_hash ) != -1 )
	{
		pna = hashtab[h].name;
	}
	else
	{
		pna = hashtab[id->value.integer].name;
	}

	if( id->dot_token )
	{
		if (*cname == '\0')
			strcpy(cname,pna);

		if (*cvar)
			strcat(cvar,".");
		report_class_name( id->dot_token,cname,cvar);
	}
	else
		strcat(cvar,pna);
}

static void print_func_argument_list( Token *t, char **buf)
{
	int	len;
	char	*pb = *buf;

	if( t != NULL )
	{
	    while(( t = t->next_token ) != NULL )
		{
		    char *nm = hashtab[t->value.integer].name;

		    if (nm)
		    {
		    		len = strlen(nm);
				
		    		if (!pb)
				{
					pb = ckalloc(len + 1);
					memcpy(pb,nm,len + 1);
					*buf = pb;
				}
				else
				{
					int	cur_len = strlen(pb);
					pb = ckrealloc(pb,cur_len + len + 2);
					*buf = pb;
					pb[cur_len] = ' ';
					memcpy(&pb[cur_len + 1],nm,len + 1);
				}
			}
		}
	}
}

