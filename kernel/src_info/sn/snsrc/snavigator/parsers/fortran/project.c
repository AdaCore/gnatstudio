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


/* project.c:
	Project-file I/O routines.  Routines included:

	Shared routines:
	   void proj_file_out() writes data from symbol table to project file.
	   void proj_file_in() reads data from project file to symbol table.

	Private routines:
		int has_defn()	    TRUE if external has defn in current file
		int has_call()	    TRUE if external has call in current file
		int count_com_defns() Counts multiple common defns.
		void proj_alist_out() Outputs argument lists
		void proj_clist_out() Outputs common lists
		void proj_arg_info_in()  Inputs argument lists
		void proj_com_info_in()  Inputs common lists
*/

#include <stdio.h>
#include <string.h>
#include "ftnchek.h"
#define PROJECT
#include "symtab.h"
#include <string.h>
#include <tcl.h>

#if defined (__MSVC__) || defined(__STDC__) || defined(__osf__)
#include <stdlib.h>
#else
void exit();
#endif

/* Note: compilation option PROJ_KEEPALL

   Define the symbol PROJ_KEEPALL to make Ftnchek create project files
   with complete global symbol table information.  Otherwise, the default
   action is: in library mode, keep only subprogram definitions, those
   external references not defined in the current file, and only one
   instance of each common block.  In non-library mode, the default is to
   keep, besides the above, one call of a given routine from each module,
   and all common block declarations.
   PROJ_KEEPALL is useful mainly for debugging purposes.
*/
#define PROJFILE_COOKIE "FTNCHEK_" /* first part of magic cookie */

PRIVATE int has_defn(), has_call();
PRIVATE void proj_alist_out(),proj_clist_out(),
  proj_arg_info_in(),proj_com_info_in();

PRIVATE int count_com_defns();

static void * SN_calloc (int size1, int size2)
{
	void * p;
	p = ckalloc (size1*size2);
	memset (p, 0, size1*size2);
	return p;
}

PRIVATE int
has_defn(alist)			/* Returns TRUE if list has defns */
   ArgListHeader *alist;
{
  while( alist != NULL && alist->topfile == top_filename ) {
    if(alist->is_defn)
      return TRUE;
    alist = alist->next;
  }
  return FALSE;
}


PRIVATE int
has_call(alist)		/* Returns TRUE if list has calls or defns  */
   ArgListHeader *alist;
{
  while( alist != NULL && alist->topfile == top_filename) {
    if( alist->is_call || alist->actual_arg )
	return TRUE;
    alist = alist->next;
  }
  return FALSE;
}

PRIVATE int
count_com_defns(clist)		/* Returns number of common decls in list  */
   ComListHeader *clist;
{
  int count=0;
  while( clist != NULL && clist->topfile == top_filename ) {
    ++count;
    clist = clist->next;
  }
  return count;
}


	/* proj_file_out: writes data from symbol table to project file. */

#define WRITE_STR(LEADER,S)	(fprintf(fd,LEADER), fprintf(fd," %s",S))
#define WRITE_NUM(LEADER,NUM)	(fprintf(fd,LEADER), fprintf(fd," %ld",NUM))
#define NEXTLINE	fprintf(fd,"\n")

void
proj_file_out(fd)
     FILE *fd;
{
  Gsymtab *sym_list[GLOBSYMTABSZ]; /* temp. list of symtab entries to print */
  BYTE sym_has_defn[GLOBSYMTABSZ];
  BYTE sym_has_call[GLOBSYMTABSZ];

  if(fd == NULL)
    return;

  WRITE_STR(PROJFILE_COOKIE,PROJECT_VERSION); /* magic cookie */
  NEXTLINE;

  WRITE_STR("file",top_filename);
  NEXTLINE;

  {	/* Make list of subprograms defined or referenced in this file */
    int i,numexts,numdefns,numcalls,do_defns,pass;
    ArgListHeader *alist;
    for(i=0,numexts=numdefns=numcalls=0;i<glob_symtab_top;i++) {
      if(storage_class_of(glob_symtab[i].type) == class_SUBPROGRAM &&
	(alist=glob_symtab[i].info.arglist) != NULL) {
			/* Look for defns and calls of this guy. */

	if( (sym_has_defn[numexts]=has_defn(alist)) != (BYTE) FALSE )
	   numdefns++;
	if( (sym_has_call[numexts]= (has_call(alist)
		/* keep only externals not satisfied in this file */
#ifndef PROJ_KEEPALL
		    && (!library_mode || !sym_has_defn[numexts])
#endif
				  )) != (BYTE) FALSE )
	   numcalls++;
	if(sym_has_defn[numexts] || sym_has_call[numexts])
	  sym_list[numexts++] = &glob_symtab[i];
      }
    }

		/* List all subprogram defns, then all calls */
    for(pass=0,do_defns=TRUE; pass<2; pass++,do_defns=!do_defns) {

      if(do_defns)
	WRITE_NUM(" entries",(long)numdefns);
      else
	WRITE_NUM(" externals",(long)numcalls);
      NEXTLINE;

      for(i=0; i<numexts; i++) {
	if( (do_defns && sym_has_defn[i]) || (!do_defns && sym_has_call[i]) ){
	  if(do_defns)
	    WRITE_STR(" entry",sym_list[i]->name);
	  else
	    WRITE_STR(" external",sym_list[i]->name);

	  WRITE_NUM(" class",(long)storage_class_of(sym_list[i]->type));
	  WRITE_NUM(" type",(long)datatype_of(sym_list[i]->type));
	  WRITE_NUM(" size",(long)sym_list[i]->size);
	  fprintf(fd," flags %d %d %d %d %d %d %d %d",
		  sym_list[i]->used_flag,
		  sym_list[i]->set_flag,
		  sym_list[i]->invoked_as_func,
		  sym_list[i]->declared_external,
		  /* N.B. library_module included here but is not restored */
		  sym_list[i]->library_module,
		  0,	/* Flags for possible future use */
		  0,
		  0);
	  NEXTLINE;
	  proj_alist_out(sym_list[i],fd,do_defns,(int)sym_has_defn[i]);
	}
      }/* end for i */
      NEXTLINE;
    }/*end for pass */
  }

  {
    int i,numblocks,numdefns;
    ComListHeader *clist;
    for(i=0,numblocks=numdefns=0;i<glob_symtab_top;i++) {
      if(storage_class_of(glob_symtab[i].type) == class_COMMON_BLOCK
	 && (clist=glob_symtab[i].info.comlist) != NULL &&
	 clist->topfile == top_filename ) {
#ifndef PROJ_KEEPALL
			/* No keepall: save only one com decl if -lib mode */
	if(library_mode)
	  numdefns++;
	else
#endif			/* keepall or -nolib mode: keep all com decls */
	  numdefns += count_com_defns(clist);

	sym_list[numblocks++] = &glob_symtab[i];
      }
    }
    WRITE_NUM(" comblocks",(long)numdefns);
    NEXTLINE;
    for(i=0; i<numblocks; i++) {
      proj_clist_out(sym_list[i],fd);
    }
    NEXTLINE;
  }
}




	/* proj_alist_out: writes arglist data from symbol table to
	   project file. */

PRIVATE void
proj_alist_out(gsymt,fd,do_defns,locally_defined)
     Gsymtab *gsymt;
     FILE *fd;
     int do_defns,locally_defined;
{
  ArgListHeader *a=gsymt->info.arglist;
  ArgListElement *arg;
  int i,n;
  unsigned long diminfo;
  Gsymtab *last_calling_module;


		/* This loop runs thru only those arglists that were
		    created in the current top file. */
    last_calling_module = NULL;
    while( a != NULL && a->topfile == top_filename) {
		/* do_defns mode: output only definitions */
     if( (do_defns && a->is_defn) || (!do_defns && !a->is_defn) )
#ifndef PROJ_KEEPALL
		/* keep only externals not satisfied in this file in -lib
		   mode, otherwise keep one actual call from each module. */
    if( a->is_defn
       || !locally_defined
       || (!library_mode && (a->is_call || a->actual_arg)
	   && a->module != last_calling_module))
#endif
     {
      last_calling_module = a->module;
      if(a->is_defn)
	 fprintf(fd," defn\n");
      else
	 fprintf(fd," call\n");

      WRITE_STR(" module",a->module->name);
      WRITE_STR(" file",a->filename);
      WRITE_NUM(" line",(long)a->line_num);
      WRITE_NUM(" class",(long)storage_class_of(a->type));
      WRITE_NUM(" type",(long)datatype_of(a->type));
      WRITE_NUM(" size",(long)a->size);
      fprintf(fd," flags %d %d %d %d",
	      a->is_defn,
	      a->is_call,
	      a->external_decl,
	      a->actual_arg);
      NEXTLINE;
      n=a->numargs;
      if(a->is_defn || a->is_call) {
	WRITE_NUM(" args",(long)n);
	NEXTLINE;
      }

      /* Next lines, 1 per argument: type, array dims, array size, flags */
      arg = a->arg_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" arg",(long)i+1);
	WRITE_NUM(" class",(long)storage_class_of(arg[i].type));
	WRITE_NUM(" type",(long)datatype_of(arg[i].type));
	WRITE_NUM(" size",(long)arg[i].size);
	diminfo = (
		   ((storage_class_of(arg[i].type) == class_VAR) &&
		   is_computational_type(datatype_of(arg[i].type))) ?
		     arg[i].info.array_dim: 0 );
	WRITE_NUM(" dims",(long)array_dims(diminfo));
	WRITE_NUM(" elts",(long)array_size(diminfo));
	fprintf(fd," flags %d %d %d %d %d %d %d %d",
		arg[i].is_lvalue,
		arg[i].set_flag,
		arg[i].assigned_flag,
		arg[i].used_before_set,
		arg[i].array_var,
		arg[i].array_element,
		arg[i].declared_external,
		0);		/* possible flag for future use */
	NEXTLINE;
      }
     }/* end if(do_defn...)*/
     a = a->next;
   }/* end while(a!=NULL)*/
   fprintf(fd," end\n");
}/*proj_alist_out*/



	/* proj_clist_out writes common var list data from symbol
	   table to project file. */

PRIVATE void
proj_clist_out(gsymt,fd)
     Gsymtab *gsymt;
     FILE *fd;
{
    ComListHeader *c=gsymt->info.comlist;
    ComListElement *cvar;
    int i,n;

    while( c != NULL && c->topfile == top_filename ) {

      WRITE_STR(" block",gsymt->name);
      WRITE_NUM(" class",(long)storage_class_of(gsymt->type));
      WRITE_NUM(" type",(long)datatype_of(gsymt->type));
      NEXTLINE;
      WRITE_STR(" module",c->module->name);
      WRITE_STR(" file",c->filename);
      WRITE_NUM(" line",(long)c->line_num);
      fprintf(fd," flags %d %d %d %d",
	      c->any_used,
	      c->any_set,
	      c->saved,
	      0);		/* Flag for possible future use */
      NEXTLINE;
      WRITE_NUM(" vars",(long)(n=c->numargs));
      NEXTLINE;

    /* Next lines, 1 per variable: class, type, array dims, array size */
      cvar = c->com_list_array;
      for(i=0; i<n; i++) {
	WRITE_NUM(" var",(long)i+1);
	WRITE_NUM(" class",(long)storage_class_of(cvar[i].type));
	WRITE_NUM(" type",(long)datatype_of(cvar[i].type));
	WRITE_NUM(" size",(long)cvar[i].size);
	WRITE_NUM(" dims",(long)array_dims(cvar[i].dimen_info));
	WRITE_NUM(" elts",(long)array_size(cvar[i].dimen_info));
	fprintf(fd," flags %d %d %d %d %d %d %d %d",
		cvar[i].used,
		cvar[i].set,
		cvar[i].used_before_set,
		cvar[i].assigned,
		0,		/* possible flags for future use */
		0,
		0,
		0);
      NEXTLINE;
      }
			/* keepall or -nolib: loop thru all defns.
			   Otherwise only keep the first. */
#ifndef PROJ_KEEPALL
      if(library_mode)
	break;
#endif
      c = c->next;
    }/* end while c != NULL */
}

#undef WRITE_STR
#undef WRITE_NUM
#undef NEXTLINE


	/* proj_file_in:
	   Reads a project file, storing info in global symbol table.
	   See proj_file_out and its subroutines for the current
	   project file format.
	 */
#define MAXNAME 127 /* Max string that will be read in: see READ_STR below */


			/* Macros for error-flagging input */

PRIVATE int nil()/* to make lint happy */
{ return 0; }

#define READ_ERROR (oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,\
     "error reading project file"),nil())
#define READ_OK nil()

#define READ_FIRST_STR(LEADER,STR) (fscanf(fd,LEADER),fscanf(fd,"%127s",STR))
#define READ_STR(LEADER,STR) ((fscanf(fd,LEADER),\
			       fscanf(fd,"%127s",STR))==1? READ_OK:READ_ERROR)
#define READ_NUM(LEADER,NUM) ((fscanf(fd,LEADER),\
			       fscanf(fd,"%d",&NUM))==1? READ_OK:READ_ERROR)
#define READ_LONG(LEADER,NUM) ((fscanf(fd,LEADER),\
			       fscanf(fd,"%ld",&NUM))==1? READ_OK:READ_ERROR)
#define NEXTLINE {int c;while( (c=fgetc(fd)) != EOF && c != '\n') continue;\
		    if(c == EOF) READ_ERROR; else ++proj_line_num;}


unsigned proj_line_num;	/* Line number in proj file for diagnostic output */

void
proj_file_in(fd)
  FILE *fd;
{
  char buf[MAXNAME+1],*topfilename=NULL;
  int retval;
  unsigned numentries,ientry, numexts,iext, numblocks,iblock;


  proj_line_num = 1;

 while( (retval=READ_FIRST_STR(PROJFILE_COOKIE,buf)) == 1) {
   if( strcmp(buf,PROJECT_VERSION) != 0 ) {
     fprintf(stderr,
	 "\nProject file is not correct version -- must be re-created\n");
     exit(1);
   }
		/* Save filename in permanent storage */
   READ_STR("file",buf);
   topfilename = strcpy(ckalloc(strlen(buf)+1),buf);
   NEXTLINE;
#ifdef DEBUG_PROJECT
 printf("read file %s\n",topfilename);
#endif


  READ_NUM(" entries",numentries); /* Get no. of entry points */
  NEXTLINE;
#ifdef DEBUG_PROJECT
 printf("read entries %d\n",numentries);
#endif
				/* Read defn arglists */
  for(ientry=0; ientry<numentries; ientry++) {
      proj_arg_info_in(fd,topfilename,TRUE);
  }
  NEXTLINE;

  READ_NUM(" externals",numexts);	/* Get no. of external refs */
#ifdef DEBUG_PROJECT
 printf("read exts %d\n",numexts);
#endif
  NEXTLINE;

				/* Read invocation & ext def arglists */
  for(iext=0; iext<numexts; iext++) {
    proj_arg_info_in(fd,topfilename,FALSE);
  }
  NEXTLINE;


			/* Read common block info */

   READ_NUM(" comblocks",numblocks);
#ifdef DEBUG_PROJECT
 printf("read num blocks %d\n",numblocks);
#endif
   NEXTLINE;

   for(iblock=0; iblock<numblocks; iblock++) {
     proj_com_info_in(fd,topfilename);
   }
   NEXTLINE;

 }/* end while(retval == 1) */

 if(retval != EOF) READ_ERROR;

 init_symtab();		/* Clear out local strspace */
}

static char *prev_file_name="";/* used to reduce number of callocs */

			/* Read arglist info */
PRIVATE void
proj_arg_info_in(fd,filename,is_defn)
    FILE *fd;
    char *filename;		/* name of toplevel file */
    int is_defn;
  {
    char id_name[MAXNAME+1],module_name[MAXNAME+1],sentinel[6];
    char file_name[MAXNAME+1];
    int id_class,id_type;
    long id_size;
    unsigned
	      id_used_flag,
	      id_set_flag,
	      id_invoked,
	      id_declared,
	      id_library_module,
	      future1,future2,future3;

    unsigned h;
    Gsymtab *gsymt, *module;
    unsigned alist_class,alist_type,alist_is_defn,alist_is_call,
       alist_external_decl,alist_actual_arg;
    unsigned alist_line;
    long alist_size;
    unsigned numargs,iarg,arg_num,arg_class,arg_type,arg_dims;
    unsigned long arg_elts;
    long arg_size;
    unsigned			/* Flags for arguments */
		arg_is_lvalue,
		arg_set_flag,
		arg_assigned_flag,
		arg_used_before_set,
		arg_array_var,
		arg_array_element,
		arg_declared_external,
		arg_future_flag;	/* possible flag for future use */

    if(is_defn)
	READ_STR(" entry",id_name); /* Entry point name */
    else
	READ_STR(" external",id_name); /* External name */
    READ_NUM(" class",id_class); /* class as in symtab */
    READ_NUM(" type",id_type); /* type as in symtab */
    READ_LONG(" size",id_size); /* size as in symtab */
    if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
	      &id_used_flag,
	      &id_set_flag,
	      &id_invoked,
	      &id_declared,
	      &id_library_module,
	      &future1,&future2,&future3) != 8) READ_ERROR;
    NEXTLINE;

#ifdef DEBUG_PROJECT
 printf("read id name %s class %d type %d\n",
id_name,id_class,id_type);
#endif

				/* Create global symtab entry */
    h = hash_lookup(id_name);
    if( (gsymt = hashtab[h].glob_symtab) == NULL) {
      gsymt = install_global(h,id_type,class_SUBPROGRAM);
      gsymt->size = id_size;
    }
    else if(is_defn)
      gsymt->size = id_size;

		/* Set library_module flag if project file was created
		   with -lib mode in effect, or is now taken in -lib mode */
    if(is_defn && (library_mode || id_library_module)) {
      gsymt->library_module = TRUE;
    }

    if(id_used_flag)
      gsymt->used_flag = TRUE;
    if(id_set_flag)
      gsymt->set_flag = TRUE;
    if(id_invoked)
      gsymt->invoked_as_func = TRUE;
    if(id_declared)
      gsymt->declared_external = TRUE;

   while(   fscanf(fd,"%5s",sentinel),
#ifdef DEBUG_PROJECT
 printf("sentinel=[%s]\n",sentinel),
#endif
	 strcmp(sentinel,(is_defn?"defn":"call")) == 0) {
      ArgListHeader *ahead;
      ArgListElement *alist;

      NEXTLINE;

      READ_STR(" module",module_name);
      READ_STR(" file",file_name);
      READ_NUM(" line",alist_line); /* line number */
      READ_NUM(" class",alist_class);	/* class as in ArgListHeader */
      READ_NUM(" type",alist_type); /* type as in ArgListHeader */
      READ_LONG(" size",alist_size); /* size as in ArgListHeader */
      if(fscanf(fd," flags %d %d %d %d",
		&alist_is_defn,
		&alist_is_call,
		&alist_external_decl,
		&alist_actual_arg) != 4) READ_ERROR;
      NEXTLINE;
#ifdef DEBUG_PROJECT
 printf("read alist class %d type %d line %d\n",
alist_class,alist_type,alist_line);
#endif
		/* Find current module in symtab. If not there, make
		   a global symtab entry for it. It will be filled
		   in eventually when processing corresponding entry.
		 */

      h = hash_lookup(module_name);
      if( (module = hashtab[h].glob_symtab) == NULL) {
	module = install_global(h,type_UNDECL,class_SUBPROGRAM);
      }
      if(module->internal_entry) {
	warning(NO_LINE_NUM,NO_COL_NUM,
		"entry point redefined as module");
	msg_tail(module->name);
	msg_tail(": redefinition ignored");
      }
      else {
	if(is_defn) {
	  if(module != gsymt) {
#ifdef DEBUG_PROJECT
	    printf("\nLinking entry %s to module %s",
		   gsymt->name,module->name);
#endif
	    gsymt->internal_entry = TRUE;
	    gsymt->link.module=module; /* interior entry: link it to module */
	  }
	}
	else {			/* call: add to child list */
		/* Avoid duplication on child list.  It will have just
		   been placed there on previous project-file entry,
		   so it will be the first child on the list.
		*/
#ifdef DEBUG_PROJECT
	  printf("\nChild %s of module %s",
		 gsymt->name,module->name);
#endif
	  if(module->link.child_list == NULL
	     || module->link.child_list->child != gsymt) {
	    ChildList *node=
	      (ChildList *)SN_calloc(1,sizeof(ChildList));
#ifdef DEBUG_PROJECT
	    printf(" linked in");
#endif
	    node->child = gsymt;
	    node->next = module->link.child_list;
	    module->link.child_list = node;
	  }
#ifdef DEBUG_PROJECT
	  else {
	    printf(" (duplicate)");
	  }
#endif
	}
      }

      if(alist_is_defn || alist_is_call) {
	  READ_NUM(" args",numargs);
	  NEXTLINE;
      }
      else
	numargs = 0;

#ifdef DEBUG_PROJECT
 printf("read numargs %d\n",numargs);
#endif
/*
**      if(!is_defn) {
**	gsymt->used_flag = TRUE;
**      }
*/
				/* Create arglist structure */
      if(((ahead=(ArgListHeader *) SN_calloc(1, sizeof(ArgListHeader)))
		 		 == (ArgListHeader *) NULL) ||
	  (numargs != 0 &&
          ((alist=(ArgListElement *) SN_calloc(numargs,sizeof(ArgListElement)))
				 == (ArgListElement *) NULL))){
		oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
			     "out of malloc space for argument list");
      }

			/* Initialize arglist and link it to symtab */
      ahead->type = type_byte(alist_class,alist_type);
      ahead->size = alist_size;
      ahead->numargs = numargs;
      ahead->arg_array = (numargs==0? NULL: alist);
      ahead->module = module;
      ahead->topfile = filename;
			/* try to avoid reallocating space for same name */
      ahead->filename =
	(strcmp(file_name,filename)==0? filename:
	 (strcmp(file_name,prev_file_name)==0? prev_file_name:
	  (prev_file_name=strcpy(ckalloc(strlen(file_name)+1),file_name))));

      ahead->line_num = alist_line;
      ahead->is_defn = alist_is_defn;
      ahead->is_call = alist_is_call;
      ahead->external_decl = alist_external_decl;
      ahead->actual_arg = alist_actual_arg;
      ahead->next = gsymt->info.arglist;
      gsymt->info.arglist = ahead;

			/* Fill arglist array from project file */
      for(iarg=0; iarg<numargs; iarg++) {
	READ_NUM(" arg",arg_num);	if(arg_num != iarg+1) READ_ERROR;
	READ_NUM(" class",arg_class);
	READ_NUM(" type",arg_type);
	READ_LONG(" size",arg_size);
	READ_NUM(" dims",arg_dims);
	READ_LONG(" elts",arg_elts);
	if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
		&arg_is_lvalue,
		&arg_set_flag,
		&arg_assigned_flag,
		&arg_used_before_set,
		&arg_array_var,
		&arg_array_element,
		&arg_declared_external,
		&arg_future_flag) != 8) READ_ERROR;

	alist[iarg].info.array_dim = array_dim_info(arg_dims,arg_elts);
	alist[iarg].type = type_byte(arg_class,arg_type);
	alist[iarg].size = arg_size;
	alist[iarg].is_lvalue = arg_is_lvalue;
	alist[iarg].set_flag = arg_set_flag;
	alist[iarg].assigned_flag = arg_assigned_flag;
	alist[iarg].used_before_set = arg_used_before_set;
	alist[iarg].array_var = arg_array_var;
	alist[iarg].array_element = arg_array_element;
	alist[iarg].declared_external = arg_declared_external;
	NEXTLINE;
#ifdef DEBUG_PROJECT
 printf("read arg num %d\n",arg_num);
#endif
      }

    }/* end while( sentinel == "defn"|"call") */

    if(strcmp(sentinel,"end") != 0) READ_ERROR;
    NEXTLINE;
}


PRIVATE void
proj_com_info_in(fd,filename)
     FILE *fd;
     char *filename;
{
    char id_name[MAXNAME+1],module_name[MAXNAME+1];
    char file_name[MAXNAME+1];
    unsigned id_class,id_type;
    unsigned			/* Flags in ComListHeader */
		clist_any_used,
		clist_any_set,
		clist_saved,
		clist_future;
    unsigned clist_line;
    unsigned numvars,ivar,var_num,var_class,var_type,var_dims;
    unsigned long var_elts;
    unsigned			/* Flags for common variables */
		var_used,
		var_set,
		var_used_before_set,
		var_assigned,
		var_future_4,
		var_future_3,
		var_future_2,
		var_future_1;
    long var_size;
      int h;
      Gsymtab *gsymt, *module;
      ComListHeader *chead;
      ComListElement *clist;


    READ_STR(" block",id_name);
    READ_NUM(" class",id_class);
    READ_NUM(" type",id_type);
#ifdef DEBUG_PROJECT
 printf("read com name %s class %d type %d\n",
id_name,id_class,id_type);
#endif
    NEXTLINE;

    READ_STR(" module",module_name);
    READ_STR(" file",file_name);
    READ_NUM(" line",clist_line);
    if(fscanf(fd," flags %d %d %d %d",
		&clist_any_used,
		&clist_any_set,
		&clist_saved,
		&clist_future) != 4) READ_ERROR;
    NEXTLINE;

    READ_NUM(" vars",numvars);
#ifdef DEBUG_PROJECT
 printf("read flags %d %d %d %d line %d\n",
	clist_any_used,
	clist_any_set,
	clist_saved,
	clist_future,
	clist_line);
#endif
    NEXTLINE;
				/* Create global symtab entry */
    h = hash_lookup(id_name);
    if( (gsymt = hashtab[h].com_glob_symtab) == NULL)
      gsymt = install_global(h,id_type,id_class);


				/* Create arglist structure */
    if(((chead=(ComListHeader *) SN_calloc(1, sizeof(ComListHeader)))
		 		 == (ComListHeader *) NULL) ||
	  (numvars != 0 &&
          ((clist=(ComListElement *) SN_calloc(numvars,sizeof(ComListElement)))
				 == (ComListElement *) NULL))){
		oops_message(OOPS_FATAL,proj_line_num,NO_COL_NUM,
			     "out of malloc space for common list");
      }

		/* Find current module in symtab. If not there, make
		   a global symtab entry for it.  This is bogus, since
		   all modules should have been defined previously. */

      h = hash_lookup(module_name);
      if( (module = hashtab[h].glob_symtab) == NULL) {
	fprintf(stderr,"\nWarning-- something's bogus in project file\n");
	module = install_global(h,type_UNDECL,class_SUBPROGRAM);
      }

			/* Initialize arglist and link it to symtab */
      chead->numargs = numvars;
      chead->line_num = clist_line;
      chead->com_list_array = (numvars==0? NULL: clist);
      chead->module = module;
      chead->topfile = filename;
      chead->any_used = clist_any_used;
      chead->any_set = clist_any_set;
      chead->saved = clist_saved;
			/* try to avoid reallocating space for same name */
      chead->filename =
	(strcmp(file_name,filename)==0? filename:
	 (strcmp(file_name,prev_file_name)==0? prev_file_name:
	  (prev_file_name=strcpy(ckalloc(strlen(file_name)+1),file_name))));

      chead->next = gsymt->info.comlist;
      gsymt->info.comlist = chead;

			/* Fill comlist array from project file */
    for(ivar=0; ivar<numvars; ivar++) {
      READ_NUM(" var",var_num); if(var_num != ivar+1) READ_ERROR;
      READ_NUM(" class",var_class);
      READ_NUM(" type",var_type);
      READ_LONG(" size",var_size);
      READ_NUM(" dims",var_dims);
      READ_LONG(" elts",var_elts);
	if(fscanf(fd," flags %d %d %d %d %d %d %d %d",
		&var_used,
		&var_set,
		&var_used_before_set,
		&var_assigned,
		&var_future_4,
		&var_future_3,
		&var_future_2,
		&var_future_1) != 8) READ_ERROR;
      NEXTLINE;
#ifdef DEBUG_PROJECT
 printf("read class %d type %d dims %d size %d\n",var_class,var_type,
var_dims,var_size);
#endif
      clist[ivar].dimen_info = array_dim_info(var_dims,var_elts);
      clist[ivar].type = type_byte(var_class,var_type);
      clist[ivar].size = var_size;
      clist[ivar].used = var_used;
      clist[ivar].set = var_set;
      clist[ivar].used_before_set = var_used_before_set;
      clist[ivar].assigned = var_assigned;
    }
}/*proj_com_info_in*/


