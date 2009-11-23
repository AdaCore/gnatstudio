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

#define LARGE_MACHINE   1

/* ftnchek.h:

	Common definitions for Fortran Program Checker

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.

*/

/* #define COPYRIGHT_DATE "Copyright (C) 1993 by Robert K. Moniot" */
#define COPYRIGHT_DATE  ""
#define COPYRIGHT_NOTICE ""

#define VERSION_NUMBER          ""
#define PATCHLEVEL              ""
#define PROJECT_VERSION         "P1" /* Project file format version number */
	/* The following system defines should be defined with the -D
	   (for UNIX) or /DEFINE (for VMS) compiler options in the makefile,
	   not here.  They are shown here so you know what to define.
	*/

/*#define VMS*/         /* Set flag for VAX/VMS   system-dependent defns. */
/*#define UNIX*/        /* Set flag for UNIX (ATT or BSD) defns. */
#ifdef __TURBOC__
#define MSDOS           /* Set flag for MSDOS (IBM PC) */
#define T_ALLOC         /* Specify workaround for malloc limits */
#endif

#ifdef UNIX
		/* For Unix systems, handle preprocessor
		   directives unless NO_UNIX_CPP defined. */
#ifndef NO_UNIX_CPP
#define UNIX_CPP
#endif
#endif

#ifdef VMS
	/* If VMS system, define flag for extra VMS-isms in
	   iokeywds.h table unless NO_VMS_IO is defined. These
	   can be independently enabled by defining VMS_IO. */
#ifndef NO_VMS_IO
#define VMS_IO
#endif /*NO_VMS_IO*/
	/* Likewise define flag for VMS behavior in INCLUDE stmt
	   (defaulting extension, /NOLIST feature) unless
	   NO_VMS_INCLUDE is defined.  */
#ifndef NO_VMS_INCLUDE
#define VMS_INCLUDE
#endif /*NO_VMS_INCLUDE*/
	/* Flag for VMS style tab-formatting of source.  Ugly as sin,
	   but the customers want it! */
#ifndef NO_VMS_TAB
#define VMS_TAB
#endif

#endif /*VMS*/

	/* set flag to allow options to start with '/' */
#ifndef OPTION_PREFIX_SLASH
#ifdef VMS
#define OPTION_PREFIX_SLASH
#endif
#ifdef MSDOS
#define OPTION_PREFIX_SLASH
#endif
#endif

		/* The following defines cause Ftnchek to permit various
		   nonstandard things.  Purists can remove these defines
		   to disable them.
		   Disable all by defining STRICT_SYNTAX.
		   Allow specific ones by defining them in makefile OPTIONS.
		*/

#ifndef STRICT_SYNTAX
#define ALLOW_UNDERSCORES 1     /* Underscores in variable names */
#define ALLOW_DOLLARSIGNS 1     /* Some systems allow $ in identifiers too */
#define INLINE_COMMENT_CHAR '!' /* Inline comments starting with '!' */
#define TYPELESS_CONSTANTS      /* Like Z'19AF' */
#define ALLOW_INCLUDE 1         /* The INCLUDE statement */
#define ALLOW_DO_ENDO 1 /* The various DO loop extensions (including WHILE) */
	/* VMS style <integer_expr> for format repeat spec or field size */
#define ALLOW_VARIABLE_FORMAT
#endif

		/* If STANDARD_INTRINSICS is defined, only F77 standard
		   intrinsic functions will be known to ftnchek.  Otherwise
		   there are three classes of nonstandard intrinsics:
		   common, Unix flavor, and VMS flavor.  The
		   Unix or VMS flavor will be selected if UNIX or
		   VMS rsptly is defined, unless suppressed with
		   NO_UNIX_INTRINSICS or NO_VMS_INTRINSICS.
		 */
#ifndef STANDARD_INTRINSICS

#define NONSTD_INTRINSICS       /* Common nonstandard intrinsic functions */

#ifdef UNIX
#ifndef NO_UNIX_INTRINSICS
#define UNIX_INTRINSICS         /* UNIX intrinsic functions */
#endif
	/* Note: RAND syntax varies.  Define RAND_NO_ARG to make ftnchek
	   expect X=RAND(). By default ftnchek expects X=RAND(ISEED).
	 */
#endif

#ifdef VMS
#ifndef NO_VMS_INTRINSICS
#define VMS_INTRINSICS          /* VMS intrinsic functions */
#endif
#endif

#endif

		/* Define to tolerate embedded blanks in numeric consts unless
		   feature turned off by defining NO_BLANKS_IN_NUMBERS. */
#ifndef NO_BLANKS_IN_NUMBERS
#define BLANKS_IN_NUMBERS
#endif

		/* Define default source and output file extensions.  These
		 * can be overridden by defines on compiler commandline.
		 */
#ifndef DEF_SRC_EXTENSION
#ifdef VMS
#define DEF_SRC_EXTENSION ".for"                /* VMS default extension */
#endif
#ifdef MSDOS
#define DEF_SRC_EXTENSION ".for"                /* IBM PC default extension */
#endif
#endif /* DEF_SRC_EXTENSION */

#ifndef DEF_SRC_EXTENSION
#define DEF_SRC_EXTENSION ".f"          /* Unix and all others */
#endif
		/* define default list-file extension */
#ifndef DEF_LIST_EXTENSION
#define DEF_LIST_EXTENSION ".lis"
#endif
		/* define default project-file extension */
#ifndef DEF_PROJ_EXTENSION
#define DEF_PROJ_EXTENSION ".prj"
#endif
		/* define project-file name for case of input from stdin */
#ifndef STDIN_PROJ_FILENAME
#define STDIN_PROJ_FILENAME "ftnchek.prj"
#endif

#ifndef ENV_PREFIX              /* prefix for option environment variables */
#define ENV_PREFIX "FTNCHEK_"
#endif

#ifndef MAXLINE
#define MAXLINE 132     /* Maximum input line length.  Ignores past this. */
#endif
#ifndef MAXSTR
#define MAXSTR 200      /* Longest string constant allowed */
#endif
#ifndef MAXIDSIZE
#define MAXIDSIZE 31    /* Longest identifier allowed */
#endif

#ifndef MAX_INCLUDE_DEPTH
#define MAX_INCLUDE_DEPTH 16    /* Max nesting depth of include files */
#endif

#ifndef ENV_INCLUDE_VAR
#define ENV_INCLUDE_VAR "INCLUDE" /* name of env variable for include dir */
#endif

#ifndef DEFAULT_INCLUDE_DIR
#ifdef UNIX
#define DEFAULT_INCLUDE_DIR "/usr/include"
#endif
#ifdef VMS
#define DEFAULT_INCLUDE_DIR "SYS$LIBRARY:"
#endif
#ifdef MSDOS
#define DEFAULT_INCLUDE_DIR "\\include"
#endif
#endif

#define KEYHASHSZ 195   /* Size of keyword hashtable -- do not change */
#define INTRINS_HASHSZ 326 /* Chosen to give few clashes -- change with care */

#ifdef SMALL_MACHINE            /* Use these for e.g. IBM PC */
#ifndef HASHSZ                  /* Hint: pick one with no square factors */
#define HASHSZ 798     /* SMALL_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 2000 /* SMALL_MACHINE Size of identifier string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 200 /* SMALL_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 200 /* SMALL_MACHINE Size of global symbol table */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 200 /* SMALL_MACHINE Max no. of tokens in token lists */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ   300 /* SMALL_MACHINE Max number of argument lists */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ    1000 /* SMALL_MACHINE Max number of arguments */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ   200 /* SMALL_MACHINE Max number of common lists */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ    1000 /* SMALL_MACHINE Max number of common elements */
#endif

#else  /* end if SMALL_MACHINE */

#ifdef LARGE_MACHINE            /* use these if space is no problem */
#ifndef HASHSZ          /* must be <= max int  */
#define HASHSZ 20930     /* LARGE_MACHINE Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 40000 /* LARGE_MACHINE Size of identifier string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 6000 /* LARGE_MACHINE Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 4000 /* LARGE_MACHINE Size of global symbol table */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 10000 /* LARGE_MACHINE Max tokens in token lists */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ   15000 /* LARGE_MACHINE Max number of argument lists */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ    50000   /* LARGE_MACHINE Max number of arguments */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ   10000 /* LARGE_MACHINE Max number of common lists */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ    50000 /* LARGE_MACHINE Max number of common elements */
#endif

#else           /* Defaults: Use these for average-size applications */

#ifndef HASHSZ
#define HASHSZ 2002     /* Default Size of symbol hashtable */
#endif
#ifndef STRSPACESZ
#define STRSPACESZ 4000 /* Default Size of identifier string space */
#endif
#ifndef LOCSYMTABSZ
#define LOCSYMTABSZ 600 /* Default Size of local symbol table */
#endif
#ifndef GLOBSYMTABSZ
#define GLOBSYMTABSZ 400 /* Default Size of global symbol table */
#endif
#ifndef TOKENSPACESZ
#define TOKENSPACESZ 1000 /* Default Max number of tokens in token lists */
#endif
#ifndef ARGLISTHEADSZ
#define ARGLISTHEADSZ   1500 /* Default Max number of argument lists */
#endif
#ifndef ARGLISTELTSZ
#define ARGLISTELTSZ    5000    /* Default Max number of arguments */
#endif
#ifndef COMLISTHEADSZ
#define COMLISTHEADSZ   1000 /* Default Max number of common lists */
#endif
#ifndef COMLISTELTSZ
#define COMLISTELTSZ    4000 /* Default Max number of common elements */
#endif

#endif /* end if LARGE_MACHINE else */

#endif/*end if SMALL_MACHINE else*/

/*  Default BpW = 4 bytes per word, which matches many machines.
    If the Fortran code does not declare explicit sizes of
    numeric variables (e.g. REAL*8), then the value of BpW will
    not matter, since the table conforms to the standard in that
    sizeof(INTEGER)=sizeof(REAL), and sizeof(DOUBLE)=sizeof(COMPLEX)
    =2*sizeof(REAL).  If the code does declare explicit sizes of
    numeric types, then the value of BpW will matter if explicit
    and default sizes are expected to match.  If you want to
    suppress warnings of this kind, you may change BpW to match
    your hardware.  Under the -portability option, explicit and
    default sizes never match: e.g. passing REAL*8 where DOUBLE
    PRECISION expected.  None of this applies to CHARACTER data:
    the default size (1) is well-defined, and the standard does
    not specify the ratio of sizeof(CHARACTER) to sizeof(REAL). */

#ifndef BpW
#define BpW 4   /* Bytes per Word: might want to use sizeof(float) instead */
#endif


#define FALSE 0
#define TRUE 1

#define NO_COL_NUM ((unsigned)999)/* Impossible column number to suppress
				 * printing in error messages
				 */
#define NO_LINE_NUM ((unsigned)0)/* Ditto for line number to suppress flushing
				 * of line if error not in local context
				 */
#ifndef WRAP_COLUMN
#define WRAP_COLUMN 79          /* When to wrap error messages to next line */
#endif

#define OOPS_NONFATAL 0         /* Severity of "oops" messages */
#define OOPS_FATAL 1

/* Shared variable and function defns start here */

#ifdef MAIN
#define SHARED          /* (nothing) */
#else
#define SHARED extern   /* Non-main routines declare shared vars extern */
#endif

#define PRIVATE static  /* For non-shared functions */


SHARED FILE
	    *input_fd,  /* Input file */
	    *list_fd,   /* Output file for listing */
	    *project_fd;/* Project file for symtab info summary */
SHARED char *current_filename,  /* name of current input file */
	    *top_filename;      /* name of toplevel parent input file */
SHARED int incdepth;


		/* Declare variables for command line options */
#ifdef MAIN
#define OPT(Type,Name,Value) Type Name=Value
#else
#define OPT(Type,Name,Value) extern Type Name
#endif

OPT(int,print_call_tree,FALSE); /* Print the call tree */
OPT(int,print_xref_list,FALSE); /* Print subprogram cross-references */
OPT(int,decls_required,FALSE);  /* List all undeclared identifiers */
OPT(int,div_check,FALSE);       /* Check for possible division by zero */
OPT(int,ext_def_check,TRUE);    /* Check defined status of externals*/
OPT(int,f77_standard,FALSE);    /* Warn of non-f77 constructs */
OPT(int,help_screen,FALSE);     /* Print out help screen */
OPT(int,hollerith_check,TRUE);  /* Warn about holleriths under -port */
OPT(int,library_mode,FALSE);    /* Set used-flag for all modules in file */
OPT(int,eol_is_space,TRUE);     /* Treat contd stmt linebreaks as space */
OPT(int,do_list,FALSE);         /* Listing flag */
OPT(int,novice_help,TRUE);      /* Extra help for novices */
OPT(int,port_check,FALSE);      /* Portability warnings */
OPT(int,pretty_flag,TRUE);      /* Warnings on deceiving appearances */
OPT(int,make_project_file,FALSE);/* Save symtab defns in .prj file */
OPT(int,pure_functions,TRUE);   /* Assume functions are pure */
OPT(int,print_ref_list,FALSE);  /* Print reference (who-calls-who) list */
OPT(int,sixclash,FALSE);        /* To check if names unique in 1st 6 chars */
OPT(int,print_topo_sort,FALSE); /* Topological sort of modules */
OPT(int,do_symtab,FALSE);       /* For symbol table printout */
OPT(int,trunc_check,TRUE);      /* Check for truncation pitfalls */
OPT(int,verbose,FALSE);         /* Verbose output format */
OPT(int,volatile_flag,FALSE);   /* Assume volatile vars and comblocks */
		/* Debugging flags */
OPT(int,debug_latest,FALSE);    /* debug the latest addition */
OPT(int,debug_glob_symtab,FALSE);/* global symtab contents */
OPT(int,debug_parser,FALSE);    /* grammar debug via DBG statements */
OPT(int,debug_hashtab,FALSE);   /* hash table contents */
OPT(int,debug_loc_symtab,FALSE); /* local symtab contents */
OPT(int,show_resources,FALSE);  /* space avail and used */
OPT(int,debug_lexer,FALSE);     /* list of tokens as scanned */
#ifdef MAIN
extern int yydebug;             /* grammar debug via yydebug */
#endif
		/* Declare variables for commandline settings */
OPT(int,argcheck_strictness,3); /* Strictness for checking args */
OPT(int,array_arg_check,3);/* Check array argument dims & size  */
/* OPT(int,max_stmt_col,72);    End of statement field ( <= MAXLINE )*/
OPT(int,max_stmt_col,MAXLINE);  /* End of statement field ( <= MAXLINE )*/
OPT(int,comcheck_strictness,3); /* 0 (no check) to 3 (exact type & size) */
OPT(int,usage_check,3);         /* Print set/used/ubs checks on variables */
OPT(int,local_wordsize,BpW);    /* Bytes per word to use for default sizes */
OPT(int,given_wordsize,BpW);    /* User's request as per -wordsize=n  */
OPT(int,wrap_column,WRAP_COLUMN);/* For wrapping error messages */

			/* Shorthands for checking control settings */
#define check_args_off  (argcheck_strictness == 0)
#define check_args_number (argcheck_strictness&01)
#define check_args_type (argcheck_strictness&02)
#define check_args_all  (argcheck_strictness == 3)
#define check_array_dims (array_arg_check&01) /* levels 1 and 3 */
#define check_array_size (array_arg_check&02) /* levels 2 and 3 */
#define check_set_used  (usage_check&01) /* levels 1 and 3 */
#define check_unused    (usage_check&02) /* levels 2 and 3 */
#define check_com_off   (comcheck_strictness == 0) /* no checking common */
#define check_com_lengths (comcheck_strictness >= 2) /* match lengths */
#define check_com_byname (comcheck_strictness == 3) /* match var by var */
#define check_com_tree (volatile_flag&&check_set_used) /* Check undef errors */
#define check_volatile_com (volatile_flag)/* Check not saved */

		/* Declare variables for commandline StrSettings */
OPT(char,*out_fname,(char *)NULL);      /* Output filename */
#ifdef ALLOW_INCLUDE
OPT(char,*include_path,(char *)NULL);   /* An include-file directory */
#endif

SHARED unsigned
    line_num,           /* line num of current char */
    col_num,            /* column num of current char */
    curr_index,         /* physical column num of current char */
    next_line_num,      /* line num of lookahead char */
    next_col_num;       /* column num of lookahead char */

SHARED unsigned
    tab_count,          /* Count of tabs in noncomment lines exc in strings */
    error_count,        /* Count of syntax error messages per file */
    warning_count;      /* Count of warning messages per file */

		/* Resource usage information: */
SHARED unsigned long
    max_loc_symtab,     /* amt of local symtab used */
    max_glob_symtab,    /* amt of global symtab used */
    max_loc_strings,    /* amt of local stringspace used */
    max_glob_strings,   /* amt of global stringspace used */
    max_tokenlists,     /* number of tokenlists constructed */
    max_token_space,    /* amt of token space used */

    arglist_element_used,       /* arg array elements allocated */
    arglist_head_used,          /* arg heads allocated (1 per call) */
    comlist_element_used,       /* com array elements allocated */
    comlist_head_used;          /* com heads allocated (1 per defn) */

SHARED int
    equivalence_flag,   /* true while parsing EQUIVALENCE statement */
    initial_flag,       /* true while only label or initial keywords read */
    implicit_flag,      /* true while parsing IMPLICIT statement */
    implicit_letter_flag, /* true while getting letters in IMPLICIT list */
    implicit_type_given,/* true if IMPLICIT type statement found */
    implicit_none,      /* true if IMPLICIT NONE statement found */
    prev_token_class,   /* token class of last-returned token */
    curr_stmt_class;    /* Token class of current stmt's leading token */

		/* Define linked-list structure for include-path list */
#ifdef ALLOW_INCLUDE
typedef struct IPNode {
  struct IPNode *link;          /* next path on the list */
  char *include_path;           /* one path (full directory name) */
} IncludePathNode;

SHARED IncludePathNode *include_path_list; /* header to the list */
#endif

		/* Declare shared routines */
	/* in advance.c */
void init_scan(),finish_scan(),open_include_file();
int flush_line_out();


	/* in ftnchek.c */
int yyerror(char *s);
void print_a_line(), syntax_error(), msg_tail(),
     warning(), ugly_code(), nonstandard(), nonportable(),
  oops_message(),oops_tail();
char *add_ext(),*new_ext();
int has_extension();

	/* in forlex.c */
int yylex();
void implied_id_token();

	/* in fortran.y/fortran.c */
int yyparse();
void init_parser();

	/* in intrinsics.c */
unsigned long init_intrins_hashtab();

	/* in keywords.c */
void init_keyhashtab();

	/* in pgsymtab.c */

void check_arglists(),check_comlists(),check_com_usage(),visit_children();

	/* in symtab.c */

char *new_local_string(),*new_global_string();
void init_tables(), init_globals(), init_symtab(), note_filename();

char * token_name();

	/* in exprtype.c */
void init_typesizes();

	/* in project.c */
void                            /* project file routines */
 proj_file_out(),proj_file_in();

char *print_line_num();

char *get_comment(char *filename, int lineno);

