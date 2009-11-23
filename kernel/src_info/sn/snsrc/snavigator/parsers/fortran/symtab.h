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

/* symtab.h:

	Shared declarations for symbol-table routines.  Note: uses
	declarations in ftnchek.h.

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/

#ifdef SYMTAB			/* "home" for variables is symtab.c */
#define SYM_SHARED
#else
#define SYM_SHARED extern
#endif

#ifdef DYNAMIC_TABLES
#ifdef __TURBOC__	/* Turbo C has only one free() */
#define cfree free
#endif
#endif

		/* Definitions of symbol table information */

/*	Token subclasses (classes are in fortran.h)
 */

#define relop_EQ	0
#define relop_NE	1
#define relop_LE	2
#define relop_LT	3
#define relop_GE	4
#define relop_GT	5



	/* Storage Class types for variables, consts, and externals */
#define class_VAR 0
#define class_SUBPROGRAM 1
#define class_COMMON_BLOCK 2
#define class_STMT_FUNCTION 3
#define class_LABEL 4
#define class_NAMELIST 5


	/* Data types for variables, consts, and externals */
	/* N.B. 0 thru 7 are wired into lookup tables in exprtype.c */
#define type_UNDECL 0
#define type_ERROR 0		/* for result of erroneous operation */
#define type_INTEGER 1
#define type_REAL 2
#define type_DP 3
#define type_COMPLEX 4
#define type_DCOMPLEX 5
#define type_LOGICAL 6
#define type_STRING 7
#define type_HOLLERITH 8
#define type_GENERIC 9
#define type_SUBROUTINE 10
#define type_COMMON_BLOCK 11
#define type_PROGRAM 12
#define type_BLOCK_DATA 13
#define type_LABEL 14
#define type_NAMELIST 15
#define type_POINTER 16
#define type_RECORD 17

#define size_DEFAULT	(0L)	/* code for standard numeric sizes */
#define size_ADJUSTABLE	(-1L)	/* codes for special char string lengths */
#define size_UNKNOWN	(-2L)

				/* test for types usable in exprs */
#define is_computational_type(t) ((unsigned)(t) <= (unsigned)type_HOLLERITH)
				/* test for numeric types */
#define is_numeric_type(t) ((unsigned)(t) <= (unsigned)type_DCOMPLEX)
				/* test for arith, char, or logical type */
#define is_const_type(t) (((t)>(unsigned)0) && ((t)<=(unsigned)type_STRING))
				/* test for numeric or logical type */
#define is_num_log_type(t) ((t) <= type_LOGICAL)
				/* test for real/d.p./complex/d.complx type */
#define is_float_type(t) ((t)>=type_REAL && (t)<=type_DCOMPLEX)

	/* Type categories equate DoubleP to Real, Double Complex
	   to Complex, and Hollerith to Int to simplify expression
	   type propagation and argument checking.  Computational
	   types only, except that since subroutine can be passed
	   as an argument, table goes up that high.  */
SYM_SHARED
unsigned char type_category[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_COMPLEX,
	type_COMPLEX,
	type_LOGICAL,
	type_STRING,
	type_INTEGER,
	type_GENERIC,
	type_SUBROUTINE,
}
#endif
;
	/* Equivalence types equate Real, DoubleP, Complex and Double
	   Complex, for use in checking mixed equivalence and mixed
	   common, since it is standard and portable to interpret complex
	   as a pair of real values: real part and imag part */
SYM_SHARED
unsigned char equiv_type[]
#ifdef SYMTAB
={	type_UNDECL,
	type_INTEGER,
	type_REAL,
	type_REAL,
	type_REAL,
	type_REAL,
	type_LOGICAL,
	type_STRING,
	type_INTEGER}
#endif
;

typedef unsigned char BYTE;

		/* Array of class and type name translations */
SYM_SHARED
char *class_name[]
#ifdef SYMTAB
 = {
	"",
	"subprog",
	"common",
	"stmt fun",
	"label",
	"namelist",
}
#endif
;
SYM_SHARED
char *type_name[]
#ifdef SYMTAB
 = {
	"undf",
	"intg",
	"real",
	"dble",
	"cplx",
	"dcpx",
	"logl",
	"char",
	"holl",
	"genr",
	"subr",
	"comm",
	"prog",
	"data",
	"labl",
	"naml",
}
#endif
;

/* Here declare typical sizes of objects of each data type, for use in
checking argument and common block size matchups.  BpW (bytes per word)
is defined in ftnchek.h */


SYM_SHARED
BYTE type_size[]
#ifdef SYMTAB
={
	0, /*undf*/
      BpW, /*intg*/
      BpW, /*real*/
    2*BpW, /*dble*/
    2*BpW, /*cplx*/
    4*BpW, /*dcpx*/
      BpW, /*logl*/
	1, /*char*/
      BpW, /*holl*/
	0, /*genr*/
	0, /*subr*/
	0, /*comm*/
	0, /*prog*/
	0, /*data*/
	0, /*labl*/
	0, /*naml*/
}
#endif
;



		/* implicit and default typing lookup table.  Big
		   enough to accommodate '$' and '_' too */
SYM_SHARED
int implicit_type[28],	/* indexed by [char - 'A'] */
    implicit_size[28];


	/* Declaration of Token data structure.  N.B. do not change without
	   consulting preamble of fortran.y for uses with nonterminals.
	 */
struct tokstruct {
	union {
		long integer;
		double dbl;
		char *string;
	} value;
	struct tokstruct *next_token;
	struct tokstruct *dot_token;
	long size;
	long class,subclass;
	unsigned line_num;	/* Line and column where token occurred */
	unsigned col_num : 8;
	int curr_index;
};

typedef struct tokstruct Token;

#define YYSTYPE Token	/* Type defn for yylval and Yacc stack */



SYM_SHARED
unsigned long loc_symtab_top,	/* Next avail spot in local symbol table */
   glob_symtab_top;		/* Ditto global */

	/* Stringspace is partitioned into local (growing from bottom up)
	   and global (growing from top down). */
SYM_SHARED
unsigned long loc_str_top,	/* Top of local stringspace */
   glob_str_bot;		/* Bottom of global stringspace */

SYM_SHARED
   unsigned long token_space_top,	/* Top of token space */
		 token_head_space_top;	/* Top of TL_head space */


SYM_SHARED
  int global_save;	/* module contains SAVE with no list */

		/* Define names for anonymous things */
#ifdef SYMTAB
char blank_com_name[] = "%BLANK",  /* id for blank common entry in symtab */
     unnamed_prog[]="%MAIN",	  /* id for unnamed program module */
     unnamed_block_data[]="%DAT00";  /* id for unnamed block data module */
int  block_data_number=0;       /* count of multiple anonymous block data */
#else
extern char blank_com_name[],
	    unnamed_prog[],
	    unnamed_block_data[];
extern int block_data_number;
#endif

                /* Symbol table argument list declarations */

typedef union {		/* InfoUnion: misc info about symtab entry */
	     unsigned long array_dim;	/* array size and no. of dims */
	     struct ALHead *arglist;	/* ptr to func/subr argument list */
	     struct CMHead *comlist;    /* ptr to common block list */
	     struct TLHead *toklist;  /* ptr to token list */
	     struct IInfo *intrins_info;/* ptr to intrinsic func info */
	     long int_value;		/* value of integer parameter */
} InfoUnion;

typedef struct {	/* ArgListElement: holds subprog argument data */
	InfoUnion info;
	long size;
	BYTE type;
	unsigned is_lvalue: 1,
		 set_flag: 1,
		 assigned_flag: 1,
		 used_before_set: 1,
		 array_var: 1,
		 array_element: 1,
		 declared_external: 1;
} ArgListElement;


typedef struct ALHead {	    /* ArgListHeader: head node of argument list */
	long size;
	BYTE type;
	short numargs;
	ArgListElement *arg_array;
	struct gSymtEntry *module;
	char *filename,*topfile;
	unsigned
	     line_num,
	     is_defn: 1,
	     is_call: 1,
	     external_decl: 1,	/* EXTERNAL decl, not arg list */
             actual_arg: 1;	/* subprog passed as arg */
	struct ALHead *next;
} ArgListHeader;

		/* Symbol table common block list declarations */

typedef struct {	/* ComListElement: holds common var data */
	unsigned long dimen_info;
	long size;
	BYTE type;
	unsigned		/* copies of flags from symtab */
	  used:1,
	  set:1,
	  used_before_set:1,
	  assigned:1;
} ComListElement;

typedef struct CMHead {	/* ComListHeader: head node of common var list */
	short numargs;
	unsigned line_num;
	ComListElement *com_list_array;
	struct gSymtEntry *module;
	char *filename,*topfile;
	struct CMHead *next;
	unsigned
	  any_used:1,		/* any of its variables accessed */
	  any_set:1,		/* any of its variables set */
	  saved:1;		/* declared in SAVE statement */
} ComListHeader;


typedef struct TLHead {	/* TokenListHeader: head node of token list */
	Token *tokenlist;
	struct TLHead *next;
	char *filename;
	unsigned line_num;
	unsigned
	  external_decl:1,
	  actual_arg:1;
} TokenListHeader;


			/* Structure for intrinsic-function info */
typedef struct IInfo{
	char *name;
	short num_args,
	      arg_type,
	      result_type;
	unsigned short
	      intrins_flags;	/* nonstandard,  mixed arg types */
} IntrinsInfo;

	/* Define special num_args values for intrinsics that have
	   variable numbers of arguments. */
#define I_1or2	(-1)		/* 1 or 2 arguments */
#define I_2up	(-2)		/* 2 or more arguments */
#define I_0or1	(-3)		/* 0 or 1 argument */

			/* for intrins_flags field */

	/* Integer-valued intrinsics that are evaluated if args const */
#define I_ABS		0x1
#define I_SIGN		0x2
#define I_DIM		0x3
#define I_MOD		0x4
#define I_MAX		0x5
#define I_MIN		0x6
#define I_ICHAR		0x7
#define I_LEN		0x8
#define I_INDEX		0x9
#define I_EVALUATED	0xf	/* any bit of digit set */

		/* Various properties of intrinsics*/
#define I_F77 		0x00	/* Standard intrinsic (no flag: placeholder) */
#define I_NONF77	0x10	/* Nonstandard */
#define I_MIXED_ARGS	0x20	/* Has mixed arg types */
#define I_NONPURE	0x40	/* Arg need not be set when called */
#define I_C_TO_R	0x80	/* Complex -> real in generic form */
#define I_NOTARG	0x100	/* Not allowed as actual argument */

			/* Structure for call-tree child list */
typedef struct childlist {
  struct gSymtEntry *child;	/* Pointer to child's symtab entry */
  struct childlist *next;/* Pointer to next child on list */
} ChildList;

		/*  Identifier symbol table declaration */


typedef struct lSymtEntry{
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	struct lSymtEntry *equiv_link;	/* Link for equivalence lists */
	long size;		/* Size of object in bytes */
	BYTE  type;		/* Type & storage class: see macros below */
			/* Flags */
	unsigned
	     used_flag: 1,	/* value is accessed (read from variable) */
	     set_flag: 1,	/* variable is set or passed as subr arg */
	     assigned_flag: 1,	/* value is really set (by assignment stmt) */
	     used_before_set: 1,/* set_flag is not set when used_flag is set */
	     is_current_module: 1, /* this symtab entry is the main module */
	     library_module: 1,	/* module was processed in -library mode */
	     array_var: 1,	/* variable is dimensioned */
	     common_var: 1,	/* variable is in common */
	     entry_point: 1,	/* name of an entry point */
	     parameter: 1,	/* name of a parameter */
	     argument: 1,	/* dummy argument */
	     external: 1,	/* function or subr called by this routine */
	     intrinsic: 1,	/* intrinsic function */
	     saved: 1,		/* named in SAVE statement */
	     invoked_as_func: 1, /* usage as f(x) was seen */
	     defined_in_include: 1, /* to suppress some warnings if unused */
	     declared_external: 1, /* explicitly declared external */
	     declared_intrinsic: 1; /* explicitly declared intrinsic */

	int common_hash;
	int common_orig_hash;
	int record_hash;
	unsigned line_num;			/* rigo */
} Lsymtab;

typedef struct gSymtEntry{	/* Global symbol table element */
	char *name;             /* Identifier name in stringspace */
	InfoUnion info;
	union {
	  struct childlist *child_list; /* List of callees (for module) */
	  struct gSymtEntry *module; /* Module (for interior entry) */
	} link;
	long size;
	BYTE  type;		/* Type & storage class: see macros below */
			/* Flags.  See remarks above */
	unsigned
	     used_flag: 1,
	     set_flag: 1,
	     assigned_flag: 1,
	     used_before_set: 1,
	     library_module: 1,
	     internal_entry: 1,	/* entry point other than at the top */
	     invoked_as_func: 1,
	     visited: 1,	   /* this entry point is in call tree */
	     visited_somewhere: 1, /* some entry point of module is in call tree */
	     defined_in_include: 1,
	     declared_external: 1;
} Gsymtab;


		/*  Identifier hashtable declaration  */

typedef struct hashEntry {
	char	*name;		/* Identifier name in stringspace */
	Lsymtab	*loc_symtab,	/* Local symtab entry for vars etc. */
		*com_loc_symtab;/* Local symtab entry for common blocks */
	Gsymtab	*glob_symtab,	/* Global symtab entry for vars etc. */
		*com_glob_symtab;/* Global symtab entry for common blocks */
	int define;
} HashTable;

		/* Macro to zero out symbol table entry */

#define clear_symtab_entry(S) {register int i;\
				 for(i=0;i<sizeof(*S);i++)((char*)S)[i]=0;}


	/* These macros pack and unpack datatype and storage class in type
	   field of symbol table entry. Datatype is least 4 bits. */

#define datatype_of(TYPE) ((unsigned)((TYPE) & 0xF))
#define storage_class_of(TYPE) ((unsigned)((TYPE) >> 4))
#define type_byte(SCLASS,DTYPE) ((unsigned)(((SCLASS)<<4) + (DTYPE)))


	/* This macro is for pattern matching in flag checking */

#define flag_combo(A,B,C) (((A)<<2) | ((B)<<1) | (C))


	/* These macros are for dimensions & sizes of arrays */

#define array_dims(dim_info) ((dim_info)&0xF)
#define array_size(dim_info) ((dim_info)>>4)
#define array_dim_info(dim,size) (((long)(size)<<4)+(dim))



		/* Defns used by expression type propagation mechanisms
		   in fortran.y and exprtype.c  The flags go in token.subclass
		 */

#define make_true(flag,x) ((x) |= (flag))		/* x.flag <-- true   */
#define make_false(flag,x) ((x) &= ~(flag))		/* x.flag <-- false  */
#define is_true(flag,x) ((x) & (flag))			/* x.flag == true?   */
#define copy_flag(flag,x,y)  ((x) |= ((y)&(flag)))	/* x.flag <-- y.flag */

#define ID_EXPR			0x1	/* a variable */
#define LVALUE_EXPR		0x2	/* assignable */
#define CONST_EXPR		0x4	/* compile-time constant per std 6.7*/
#define LIT_CONST		0x8	/* a number or string literal */
#define ARRAY_ID_EXPR		0x10	/* an array or array element */
#define INT_QUOTIENT_EXPR	0x20	/* contains INT/INT */
#define STMT_FUNCTION_EXPR	0x40
#define PARAMETER_EXPR		0x80 /* == CONST_EXPR || intrinsic || **real */
#define EVALUATED_EXPR		0x100  /* token.value has value of expr */
#define SET_FLAG		0x200  /* these are for id's and lvalues */
#define ASSIGNED_FLAG		0x400
#define USED_BEFORE_SET		0x800

#define COMPLEX_FLAG		0x1000	/* remembers complex_const_allowed */
#define CHAR_ID_EXPR		0x2000	/* char var or array elt not substr */
#define IN_ASSIGN		0x4000	/* for tracking assgn stmt lhs */
#define COMMA_FLAG 		0x8000/* keeps track of extra or missing commas
				   	in exprlists */
#define NONSTD_USAGE_FLAG	0x10000	/* concentrator for -f77 warnings */
#define SYNTAX_ERROR_FLAG	0x20000	/* concentrator for syntax errors */

#ifdef DYNAMIC_TABLES		/* tables will be mallocked at runtime */
SYM_SHARED
Lsymtab	*loc_symtab
#ifdef SYMTAB
  =(Lsymtab *)NULL
#endif
;
SYM_SHARED
Gsymtab *glob_symtab
#ifdef SYMTAB
  =(Gsymtab *)NULL
#endif
;
SYM_SHARED
HashTable *hashtab
#ifdef SYMTAB
  =(HashTable *)NULL
#endif
;
SYM_SHARED
char *strspace
#ifdef SYMTAB
  =(char *)NULL
#endif
;
SYM_SHARED
Token *tokenspace
#ifdef SYMTAB
  =(Token *)NULL
#endif
;

SYM_SHARED
TokenListHeader *tokheadspace
#ifdef SYMTAB
  =(TokenListHeader *)NULL
#endif
;
#else				/* static tables declared at compile time */
		/* Each major table is housed in a separate file so that
		   on IBM PC architecture with huge memory model
		   each will be in its own 64K segment not all in one. */
#ifndef PLSYMTAB
extern
#endif
Lsymtab	loc_symtab[LOCSYMTABSZ]; /* Local identifiers */
#ifndef PGSYMTAB
extern
#endif
Gsymtab glob_symtab[GLOBSYMTABSZ]; /* Global identifiers: subrs and com blks */
#ifndef EXPRTYPE
extern
#endif
HashTable hashtab[HASHSZ];	/* Hash table for identifier lookup */
#ifndef SYMTAB
extern
#endif
char strspace[STRSPACESZ];	/* String space for storing identifiers */
#ifndef FORLEX
extern
#endif
Token tokenspace[TOKENSPACESZ];	/* Tokens for arg lists etc */
#ifndef PROJECT
extern
#endif
TokenListHeader tokheadspace[TOKENSPACESZ];/* Tokenlist headers */
#endif


		/* Shared routines */

			/* in fortran.y/fortran.c */
void
check_seq_header(), check_stmt_sequence();

			/* in plsymtab.c */
void
debug_symtabs(), print_loc_symbols();

			/* in symtab.c */
void
check_loose_ends(),
call_func(), call_subr(), declare_type(), def_arg_name(),
def_array_dim(), def_com_block(), def_com_variable(),
def_equiv_name(), def_ext_name(), def_function(), def_intrins_name(),
def_namelist(), def_namelist_item(), def_parameter(),
def_stmt_function(), do_ASSIGN(), do_assigned_GOTO(), do_ENTRY(),
do_RETURN(), equivalence(),
process_lists(), ref_array(), ref_namelist(), ref_variable(),
save_com_block(), save_variable(), set_implicit_type(),
stmt_function_stmt(),use_actual_arg(), use_implied_do_index(),
use_io_keyword(), use_special_open_keywd(),
use_lvalue(), use_parameter(),
use_function_arg(), use_variable();

Token
*new_token();

Lsymtab
 *install_local();
Gsymtab
 *install_global();

unsigned
hash_lookup();

int
def_curr_module(), get_type(), int_expr_value();

char *
token_name();
				/* in hash.c (now symtab.c) */
unsigned long
hash(), kwd_hash(), rehash();

			/* in exprtype.c */
void
binexpr_type(),unexpr_type(),assignment_stmt_type(),
func_ref_expr(),primary_id_expr(),stmt_fun_arg_cmp();

int
intrins_arg_cmp();

