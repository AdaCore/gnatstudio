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

/*  ftnchek.c:

	Main program for Fortran Syntax Checker.

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


	Top-level input/output is done here: opening and closing files,
	and printing error, warning, and informational messages.

	Shared functions defined:
		print_a_line()	Prints source code line.
		yyerror()	Error messages from yyparse and elsewhere.
		syntax_error()	Error messages with line and column num.
		warning()	Warning messages.
		nonportable()	Portability warnings.
		wrapup()	Look at cross references, etc.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef __MSVC__
#define	MAXPATHLEN _MAX_PATH
typedef	unsigned int pid_t;
#else
#include <sys/param.h>
#endif /* WIN32 */
#define MAIN
#include "ftnchek.h"
#include <tcl.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

PRIVATE void
error_summary(),get_env_options(),
make_env_name(), src_file_in();
#if 0
PRIVATE void
	lintstyle_error_message(),oldstyle_error_message(),
	error_message(),print_version_number(),set_option(),
	list_options(),open_outfile(),resource_summary(),wrapup();
#endif
#ifdef ALLOW_INCLUDE
PRIVATE void append_include_path();
#endif

PRIVATE int read_setting();

#define full_output	(do_list || do_symtab)

unsigned long intrins_clashes;	/* count of intrinsic hashtable clashes */
#ifdef COUNT_REHASHES
extern unsigned long rehash_count; /* count of calls to rehash() */
#endif

	/* Here we define the commandline options.  Most options are boolean
	   switchopts, with "no" prefix to unset them.  Others (called
	   settings) are numeric quantities, defined using "=num".
	   A third category (strsettings) are string quantities, eg filenames.
	   The argument "?" will cause list of options to be printed out.
	   For VMS, options can be prefixed with either "-" or "/",
	   but messages will use the canonical form. */

#ifdef OPTION_PREFIX_SLASH
#define OPT_PREFIX '/'	/* Canonical VMS prefix for commandline options */
#else
#define OPT_PREFIX '-'	/* Canonical Unix prefix for commandline options */
#endif

#define OPT_MATCH_LEN 3	/* Options are matched only in 1st 3 chars */
#define NUM_SWITCHES (sizeof(switchopt)/sizeof(switchopt[0]))
#define NUM_SETTINGS (sizeof(setting)/sizeof(setting[0]))
#define NUM_STRSETTINGS (sizeof(strsetting)/sizeof(strsetting[0]))

/*	Option definitions:
	   New options can be added to lists by inserting definition
	   here using same syntax as others, and declaring the variable
	   with OPT(type,name,default); in ftnchek.h.  No other changes
	   needed.
*/


		/* List of switches is defined first.  Each entry gives the
		   name and the corresponding flag variable to be set
		   or cleared.  See set_option() for processing of switches.

		   N.B. list_options() will suppress printing of any options
		   whose explanation starts with "debug" unless the -debug
		   switch was previously given.
		 */
struct {
    char *name;
    int *switchflag;
    char *explanation;
} switchopt[]={
	{"calltree",	&print_call_tree,"print subprogram call tree"},
	{"crossref",	&print_xref_list,"print call cross-reference list"},
	{"declare",	&decls_required,"list undeclared variables"},
	{"division",	&div_check,	"catch possible div by 0"},
	{"extern",	&ext_def_check,	"check if externals defined"},
	{"f77",		&f77_standard,	"warn of nonstandard constructs"},
	{"help",	&help_screen,	"print help screen"},
	{"hollerith",	&hollerith_check,"warn about holleriths under -port"},
	{"library",	&library_mode,	"treat next files as library"},
#ifdef EOLSKIP
	{"linebreak",	&eol_is_space,	"treat linebreaks as space"},
#endif
	{"list",	&do_list,	"print program listing"},
	{"novice",	&novice_help,	"extra help for novices"},
	{"portability",	&port_check,	"check for portability problems"},
	{"pretty",	&pretty_flag,	"warn of deceiving appearances"},
	{"project",	&make_project_file,	"create project file"},
	{"pure",	&pure_functions,"functions have no side effects"},
	{"reference",	&print_ref_list,"print who-calls-who reference list"},
	{"sixchar",	&sixclash,	"catch nonunique names"},
	{"sort",	&print_topo_sort,"prerequisite-order sort of modules"},
	{"symtab",	&do_symtab,	"print symbol table info"},
	{"truncation",	&trunc_check,	"check for truncation pitfalls"},
	{"verbose",	&verbose,	"verbose output"},
	{"volatile",	&volatile_flag,	"assume volatile common blocks"},

	{"debug",	&debug_latest,	"debug latest code"},
	{"global",	&debug_glob_symtab,	"debug global symtab info"},
	{"grammar",	&debug_parser,	"debug printout in parser"},
	{"hashtable",	&debug_hashtab,	"debug printout of hashtable"},
	{"local",	&debug_loc_symtab,	"debug local symtab info"},
	{"resources",	&show_resources,"debug info on resources"},
	{"tokens",	&debug_lexer,	"debug printout in lexer"},
#if YYDEBUG
	{"yydebug",	&yydebug,	"debug via yydebug"},
#endif
};


		/* List of settings is defined here. Each entry gives
		   the name, the corresponding variable, the range
		   of permitted values, the value for turning it off,
		   followed by brief explanation.
		   See set_option() for processing. */
struct {
    char *name;
    int *setvalue;
    int minlimit,maxlimit,turnoff;
    char *explanation;
} setting[]={
  {"arguments",	&argcheck_strictness, 0, 3, 0,
			"check args: 0=none 1=number 2=type 3=all"},
  {"array",	&array_arg_check, 0, 3, 0,
			"check array args: 0=none 1=dims 2=size 3=all"},
/*   {"columns",	&max_stmt_col,  72, MAXLINE, 72, */
/* 			"max line length processed"}, */
  {"columns",	&max_stmt_col,  MAXLINE, MAXLINE, MAXLINE,
			"max line length processed"},
  {"common",	&comcheck_strictness,  0, 3, 0,
			"common check: 0=none 3=most strict"},
  {"usage",	&usage_check,	0, 3, 0,
			"0=no check, 1=used-not-set 2=unused 3=all"},
  {"wordsize",	&given_wordsize, 0, 16, 0,
			"standard wordsize in bytes (0=no default)"},
  {"wrap",	&wrap_column, 0, 999, 0,
			"width of page to wrap error messages"},
};


		/* List of strsettings is defined here. Each entry gives
		   the name the corresponding string variable, and brief
		   explanation.  See set_option() for processing. */
struct {
    char *name;
    char **strvalue;
    char *explanation;
} strsetting[]={
#ifdef ALLOW_INCLUDE
  {"include",	&include_path,	"include-file directory"},
#endif
  {"output",	&out_fname,	"output file name"},
};

int must_open_outfile=FALSE;	/* Flag set to TRUE when out=name given */

FILE    *hig_fp;
FILE    *out_fp;
int      highlight;

#define	MAIN_MODULE

#include <tcl.h>
#include "sn.h"

/* Tcl encoding to translate from. The default (when equal to NULL) is
   to do no translation. */
Tcl_Encoding encoding = NULL;

char * group = "fortran";

extern int report_local_vars;
extern int max_line_width;

static	int
log_symbol_filename(FILE *fp, char *fname)
{
  char	*outfile = NULL;
  
  if (hig_fp)
    {
      fclose(hig_fp);
      hig_fp = NULL;
    }
  
  if (fname && freopen(fname,"r",stdin) == NULL)
    {
      printf("Error: unable to open file \"%s\",errno: %d\n",fname,errno);
      fflush(stdout);
      return 1;
    }
  
  if (fname)
    {
      if (highlight)
	{
	  outfile = Paf_tempnam(NULL,"hf");
	  if (fp)
	    {
	      fprintf(fp,"%s\n",outfile);
	    }
	  
	  hig_fp = fopen(outfile,"w+");
	}
      /* print filename only */
      printf("%s...\n",fname);
      fflush(stdout);
    }
  else
    {
      if (highlight)
	{
	  hig_fp = (fp) ? fp : stdout;
	}
    }
  
#ifndef NO_DATABASE
  if (fname)
    {
      put_file(fname,group,outfile);
    }
#endif
  
  return 0;
}

extern int sum_line;
#ifdef NO_DATABASE
FILE	*cross_ref_fp;
int	comment_database;
#else
extern FILE	*cross_ref_fp;
extern	int	comment_database;
#endif

int
main(int argc, char *argv[])
{
	extern	char	*optarg;
	extern	int	optind;
	int iarg;
	int filecount=0,actioncount=0;
	char *infile;
   	FILE    *list_fp = NULL;
   	FILE    *include_fp = NULL;
	char	*cross_ref_file = NULL;
	char	tmp[500];
	char	*pipe_cmd = NULL;
	char	*cachesize = NULL;
	pid_t	pid = 0;
	char	dirname[MAXPATHLEN];
	char	*db_prefix = NULL;
	char	*incl_to_pipe = NULL;
	char	*sn_host = NULL;
	char	*sn_pid = NULL;

	dirname[0] = '\0';

#ifdef VMS			/* VMS version: expand wildcards, etc. */
	shell_mung(&argc,&argv,1,NULL);
#endif

	list_fd = stdout;
	project_fd = (FILE *) NULL;
	error_count = 0;
	warning_count = 0;

	get_env_options();
#ifdef ALLOW_INCLUDE
	include_path_list = (IncludePathNode*) NULL;
	if(include_path != (char *)NULL) {
	  append_include_path(include_path);
	  include_path = (char *)NULL; /* clear it for the next one */
	}
#endif
	init_tables();		/* Initialize tables */
	init_keyhashtab();
	intrins_clashes = init_intrins_hashtab();
	init_globals();
	init_symtab();

	/* Character set encoding (as defined by Tcl). */
	Tcl_FindExecutable(argv[0]);

	while ((iarg = getopt(argc,argv,"e:s:n:hy:I:g:p:c:i:ltx:CrH:O:P:w:"))
	       != EOF)
	  {
	    switch (iarg)
	      {
	      case 's':
		if ((out_fp = fopen(optarg,"a")) == NULL)
		  {
		    fprintf(stderr,"Error: couldn't create \"%s\"\n",optarg);
		    exit(1);
		  }
		break;
		
	      case 'n':
		db_prefix = optarg;
		break;
		
	      case 'e':
		if ((encoding = Tcl_GetEncoding(NULL, optarg)) == NULL)
		  {
		    printf("Unable to locate `%s' encoding\n", optarg);
		    return 1;
		  }
		break;
		
	      case 'h':
		highlight = -1;
		break;
		
	      case 'y':
		list_fp = fopen(optarg,"r");
		break;
		
	      case 'I':
		include_fp = fopen(optarg,"r");
		break;
		
	      case 'g':
		group = optarg;
		break;
		
	      case 'p':
		pipe_cmd = optarg;
		break;
		
	      case 'c':
		cachesize = optarg;
		break;
		
	      case 'i':
		incl_to_pipe = optarg;
		break;
		
	      case 'H':
		sn_host = optarg;
		break;
		
	      case 'P':
		sn_pid = optarg;
		break;
		
	      case 'x':
		cross_ref_file = optarg;
		break;
		
	      case 'r':
		/* Remark (comment) support. */
		comment_database = TRUE;
		break;
		
	      case 'l':
		report_local_vars = 1;
		break;
		
	      case 'C':
		/* Parse *.h *.c as C++. Fall through. */
	      case 't':
		/* Drop /usr files. */
		break;
	      case 'w':
		max_line_width = atoi(optarg);
		break;
	      }
	  }

	if (include_fp)
	{
		while (fgets(tmp,sizeof(tmp) -1,include_fp))
		{
			char *pc = strchr( tmp, '\n' );

			if (pc)
				*pc = 0;
			append_include_path(SN_StrDup(tmp));
		}
		fclose(include_fp);
	}

	if (optind < argc || list_fp)
	{
		if (pipe_cmd)
		{
			pid = Paf_Pipe_Create(pipe_cmd,db_prefix,incl_to_pipe,
				cachesize,sn_host,sn_pid);
		}
#ifndef NO_DATABASE
		else
			Paf_db_init_tables(db_prefix,cachesize,NULL);
#endif
		if (cross_ref_file)
		{
			if (!(cross_ref_fp = fopen(cross_ref_file,"a")))
			{
				printf("Error: (open) \"%s, errno: %d\"\n",cross_ref_file,errno);
				exit(1);
			}
		}

		input_fd = stdin;
		if (list_fp)
		{
			while (fgets(tmp,sizeof(tmp) -1,list_fp))
			{
				++actioncount;  /* Cause exit w/o reading stdin below */

				++filecount;

				if ((infile = strchr(tmp,'\n')))
				{
					*infile = '\0';
				}
				if (!log_symbol_filename(out_fp,tmp))
				{
					src_file_in(tmp);
				}
			}
			fclose(list_fp);
		}
		else
		{
			if (optind == (argc - 1) && highlight != -1)
				highlight = 1;

			if (!log_symbol_filename(out_fp,argv[optind]))
				src_file_in(argv[optind]);
		}
		empty_comments();
	}
	else if (highlight && optind == argc)		/* Just highlight */
	{
		input_fd = stdin;
		log_symbol_filename(out_fp,NULL);
		src_file_in(NULL);
	}

	if (cross_ref_fp)
		fclose(cross_ref_fp);

	if (out_fp)
		fclose(out_fp);

	if (hig_fp && hig_fp != out_fp)
		fclose(hig_fp);

	Paf_Pipe_Close();

	if (encoding) {
	    Tcl_FreeEncoding(encoding);
	    Tcl_Finalize();
	}

	exit(0);

	return 0;/* make lint happy */
}

PRIVATE void
src_file_in(infile)
     char *infile;		/* input filename */
{
	if (infile)
		note_filename(infile);

	init_scan();
	init_parser();

	(void) yyparse();

	finish_scan();

	if(make_project_file) {
		  proj_file_out(project_fd);
		  (void) fclose(project_fd);
	}


	if(port_check && tab_count != 0) {
	  nonportable(NO_LINE_NUM,NO_COL_NUM,
		      "File contains tabs");
	}

	error_summary(infile);
}


PRIVATE void
error_summary(fname)		/* Print out count of errors in file */
	char *fname;
{
#ifdef ERROR_MESS
	FILE *fd = list_fd;

	if(full_output ||
	   (verbose && error_count+warning_count != 0))
	  fprintf(fd,"\n");

	if(full_output || verbose || error_count != 0)
	  fprintf(fd,"\n %u syntax error%s detected in file %s",
			error_count, error_count==1? "":"s",
			fname);

	if(warning_count != 0)
		fprintf(fd,"\n %u warning%s issued in file %s",
			warning_count, warning_count==1? "":"s",
			fname);

	if(full_output ||
	   (verbose && error_count+warning_count != 0))
	  fprintf(fd,"\n");

	error_count = 0;
	warning_count = 0;
#endif
}

#if 0
PRIVATE void
print_version_number()
{
  if(full_output || verbose)
    fprintf(list_fd,"\n");
  fprintf(list_fd,"%s",VERSION_NUMBER);
  if(help_screen)
    fprintf(list_fd," %s",PATCHLEVEL);
  if(full_output || verbose)
    fprintf(list_fd,"\n");
}
#endif

void
print_a_line(fd,line,num)  /* Print source line with line number */
	FILE *fd;
	char *line;
	unsigned num;
{
	fprintf(fd,"\n %6u %s",num,line);
}


int
yyerror(s)
	char *s;
{
#ifdef ERROR_MESS
	syntax_error(line_num,col_num,s);
#endif
	return 0;
}


void
syntax_error(lineno,colno,s)		/* Syntax error message */
	unsigned lineno,colno;
	char *s;
{
#ifdef ERROR_MESS
	++error_count;
	error_message(lineno,colno,s,"Error");
#endif
}

void
warning(lineno,colno,s)		/* Print warning message */
	unsigned lineno,colno;
	char *s;
{
#ifdef ERROR_MESS
	++warning_count;
	error_message(lineno,colno,s,"Warning");
#endif
}

void
ugly_code(lineno,colno,s)		/* -pretty message */
	unsigned lineno,colno;
	char *s;
{
#ifdef ERROR_MESS
	++warning_count;
	error_message(lineno,colno,s,"Possibly misleading appearance");
#endif
}

void
nonstandard(lineno,colno)
     unsigned lineno,colno;
{
#ifdef ERROR_MESS
	++warning_count;
	error_message(lineno,colno,"Nonstandard syntax","Warning");
#endif
}

void
nonportable(lineno,colno,s) /* Print warning about nonportable construction */
	unsigned lineno,colno;
	char *s;
{
#ifdef ERROR_MESS
	++warning_count;
	error_message(lineno,colno,s,"Nonportable usage");
#endif
}

/* error_message prints out error messages and warnings.  It
   now comes in two flavors.  If using lintstyle_error_message(),
   messages are produced in style like UNIX lint:

	"main.f", line nn, col nn: Error: your message here

   Otherwise messages by oldstyle_error_message in old ftnchek style:

   	Error near line nn col nn file main.f: your message here

   At this time, oldstyle_error_message is used when -novice is
   in effect, lintstyle_error_message otherwise.
*/

#ifdef ERROR_MESS
PRIVATE int errmsg_col;
#endif
	/* Crude macro to give number of digits in line and column numbers.
	   Used by line wrap computation. */
#define NUM_DIGITS(n) ((n)<10?1:((n)<100?2:((n)<1000?3:(n)<10000?4:5)))

#ifdef ERROR_MESS
PRIVATE void
error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
{
  if(novice_help)
    oldstyle_error_message(lineno,colno,s,tag);
  else
    lintstyle_error_message(lineno,colno,s,tag);
}

PRIVATE void
lintstyle_error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
{
	int icol;
	extern unsigned prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.
			 */

	if(lineno != NO_LINE_NUM) {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			fprintf(list_fd," ");
		    fprintf(list_fd,"^");
		}
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void) flush_line_out(prev_stmt_line_num);
	    }
	}

	fprintf(list_fd,"\n\"%s\"",current_filename);
	errmsg_col += 2+strlen(current_filename);

	if(lineno != NO_LINE_NUM) { /* nonlocal error-- don't flush */
	    if(colno == NO_COL_NUM) {
		fprintf(list_fd,
		   ", near line %u",lineno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else if(colno != 0) {
		fprintf(list_fd,
		   ", line %u col %u",lineno,colno);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	    else {		/* colno == 0 */
		fprintf(list_fd,
		   ", near line %u",prev_stmt_line_num);
		errmsg_col += 12+NUM_DIGITS(lineno);
	    }
	}

	fprintf(list_fd,": %s:",tag); /* "Warning", "Error", etc. */
	errmsg_col += 3+strlen(tag);

	msg_tail(s); /* now append the message string */
}

				/* Our own style messages */
PRIVATE void
oldstyle_error_message(lineno,colno,s,tag)
	unsigned lineno,colno;
	char *s,*tag;
{
	int icol;
	extern unsigned prev_stmt_line_num; /* shared with advance.c */

	errmsg_col=1;		/* Keep track of line length */

			/* Print the character ^ under the column number.
			   But if colno == 0, error occurred in prior line.
			   If colno is NO_COL_NUM, then print message
			   without any column number given.
			 */

	if(lineno == NO_LINE_NUM) { /* nonlocal error-- don't flush */
	  fprintf(list_fd,"\n%s",tag);
	  errmsg_col += strlen(tag);
	}
	else {
	    if(colno == NO_COL_NUM) {
		    /* colno == NO_COL_NUM means don't give column number.*/
		(void)flush_line_out(lineno);/* print line if not printed yet */
		fprintf(list_fd,
		   "\n%s near line %u",tag,lineno);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	    else if(colno != 0) {
			/* print line if not printed yet */
		if( flush_line_out(lineno) ) {
				/* If it was printed, put ^ under the col */
		    fprintf(list_fd,"\n%8s","");

		    for(icol=1; icol<colno; icol++)
			fprintf(list_fd," ");
		    fprintf(list_fd,"^");
		}
		fprintf(list_fd,
		   "\n%s near line %u col %u",tag,lineno,colno);
		errmsg_col += 16+NUM_DIGITS(lineno)+NUM_DIGITS(colno)
		  +(unsigned)strlen(tag);
	    }
	    else {		/* colno == 0 */
			/* print line if not printed yet */
		(void) flush_line_out(prev_stmt_line_num);
		fprintf(list_fd,
		   "\n%s near line %u",tag,prev_stmt_line_num);
		errmsg_col += 11+NUM_DIGITS(lineno)+(unsigned)strlen(tag);
	    }
	}

	if(!full_output		/* If not listing, append file name */
	   || incdepth > 0){	/* Append include-file name if we are in one */
	  if(lineno == NO_LINE_NUM) { /* if no line no, preposition needed */
	    fprintf(list_fd," in");
	    errmsg_col += 3;
	  }
	  fprintf(list_fd," file %s",current_filename);
	  errmsg_col += 6+(unsigned)strlen(current_filename);
	}

	fprintf(list_fd,":");
	errmsg_col++;

	msg_tail(s); /* now append the message string */
}
#endif /* ERROR_MESS */

		/* msg_tail appends string s to current error message.
		   It prints one word at a time, starting a new line
		   when the message gets to be too long for one line.
		 */
void
msg_tail(s)
    char *s;
{
#ifdef ERROR_MESS
	int wordstart,wordend,leading_skip,wordchars;

	fprintf(list_fd," ");
	errmsg_col++;
	wordstart=0;
		/* Each iteration of loop prints leading space and the
		   nonspace characters of a word.  Loop invariant: wordstart
		   is index of leading space at start of word, wordend is
		   index of space char following word. */
	while(s[wordstart] != '\0') {
	  leading_skip = TRUE;
	  for(wordend=wordstart; s[wordend] != '\0'; wordend++) {
	    if(leading_skip) {	/* If skipping leading space chars */
	      if(!isspace(s[wordend]))
		leading_skip = FALSE; /* go out of skip mode at nonspace */
	    }
	    else {		/* If scanning word chars */
	      if(isspace(s[wordend]))
		break;		/* quit loop when space char found */
	    }
	  }
	  wordchars = wordend-wordstart;
				/* If word doesn't fit, wrap to next line */
	  if( wrap_column > 0 && (errmsg_col += wordchars) > wrap_column) {
	    fprintf(list_fd,"\n");
	    errmsg_col = wordchars;
	  }
				/* Print the word */
	  while(wordstart < wordend) {
	    fputc(s[wordstart++],list_fd);
	  }
	}
#endif
}


void
oops_message(severity,lineno,colno,s)
	int severity;
	unsigned lineno,colno;
	char *s;
{
#ifdef ERROR_MESS
    {
	fprintf(stderr,"\nOops");
	if(lineno != NO_LINE_NUM) {
	  fprintf(stderr," at line %u",lineno);
	  if(colno != NO_COL_NUM)
	    fprintf(stderr," at col %u",colno);
	}
	fprintf(stderr," in file %s",current_filename);
	fprintf(stderr," -- %s",s);
	if(severity == OOPS_FATAL) {
	  fprintf(stderr,"\nFtnchek aborted\n");
	  (void) exit(1);
	}
    }
#endif
}

void
oops_tail(s)
	char *s;
{
#ifdef ERROR_MESS
    {
	fprintf(stderr," %s",s);
    }
#endif
}

/*	get_env_options picks up any options defined in the
	environment.  A switch or setting is defined according to
	the value of an environment variable whose name is the switch
	or setting name (uppercased), prefixed by the string
	ENV_PREFIX (e.g.  FTNCHEK_).  For settings and strsettings,
	the value of the environment variable gives the value to be
	used.  For switches, the environment variable is set to "0" or
	"NO" to turn the switch off, or to any other value (including
	null) to turn it on.
*/

PRIVATE void
get_env_options()
{
	char env_option_name[32];
	char *value;
	int i;
	for(i=0; i<NUM_SWITCHES; i++) {
			/* Construct the env variable name for switch i */
	    make_env_name( env_option_name, switchopt[i].name);

			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
	        *(switchopt[i].switchflag) =
			!(strcmp(value,"0")==0 || strcmp(value,"NO")==0 );
	    }

	}

	for(i=0; i<NUM_SETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, setting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		if(read_setting(value, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnoff) != 0)
			fprintf(stderr,"Env setting garbled: %s=%s: ignored\n",
				env_option_name,value);
	    }
	}


	for(i=0; i<NUM_STRSETTINGS; i++) {
			/* Construct the env variable name for setting i */
	    make_env_name( env_option_name, strsetting[i].name);
			/* See if it is defined */
	    if( (value = getenv(env_option_name)) != (char *)NULL) {
		    *(strsetting[i].strvalue) = value;

			/* Handle necessary action for  -out=listfile */
		if(strsetting[i].strvalue == &out_fname)
			must_open_outfile = TRUE;
	    }
	}
}

		/* Routine to concatenate ENV_PREFIX onto option name
		   and uppercase the result.
		*/
PRIVATE void
make_env_name( env_name, option_name)
	char *env_name, *option_name;
{
    int i,c;

    strcat(strcpy(env_name,ENV_PREFIX),option_name);
    for(i=sizeof(ENV_PREFIX)-1; (c=env_name[i]) != '\0'; i++) {
	if( islower(c) )
	    env_name[i] = toupper(c);
    }
}


	/* set_option processes an option from command line.  Argument s is
	   the option string. First s is compared against boolean switches
	   from list in switchopt[].  If s matches switch string,
	   corresponding flag is set to TRUE.  If no match, then s is compared
	   to the same switches prefixed by "no", and if match is found, then
	   flag is set to FALSE.  Finally, special flags are handled.  If still
	   no match, an error message is generated.
	 */

#if 0
PRIVATE void
set_option(s)
	char *s;
{
	int i;
		/* look for noswitch flags first since otherwise
		   an option starting with no might take precedence */
	if(strncmp(s+1,"no",2) == 0) {
	    for(i=0; i<NUM_SWITCHES; i++) {
		if( strncmp(s+3,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		    *(switchopt[i].switchflag) = FALSE;
		    return;
		}
	    }
	}

		/* -noswitch not found: look for nosetting flag */
	if(strncmp(s+1,"no",2) == 0) {
	    for(i=0; i<NUM_SETTINGS; i++) {
		if( strncmp(s+3,setting[i].name,OPT_MATCH_LEN) == 0) {
		    *(setting[i].setvalue) = setting[i].turnoff;
		    return;
		}
	    }
	}

				/* Next look for switches */
	for(i=0; i<NUM_SWITCHES; i++) {
	    if( strncmp(s+1,switchopt[i].name,OPT_MATCH_LEN) == 0) {
		*(switchopt[i].switchflag) = TRUE;
		return;
	    }
	}

		/* Handle settings of form "-opt=number" */
	for(i=0; i<NUM_SETTINGS; i++)
	    if( strncmp(s+1,setting[i].name,OPT_MATCH_LEN) == 0) {
		char *numstr;

		numstr = s + (OPT_MATCH_LEN + 1);
		while(*numstr != '\0')
		    if(*numstr++ == '=')	/* Find the = sign */
			break;

		if(read_setting(numstr, setting[i].setvalue, setting[i].name,
				setting[i].minlimit, setting[i].maxlimit,
				setting[i].turnoff) != 0)
			fprintf(stderr,"Setting garbled: %s: ignored\n",s);
		return;
	    }


		/* Handle settings of form "-opt=string" */
	for(i=0; i<NUM_STRSETTINGS; i++)
	    if( strncmp(s+1,strsetting[i].name,OPT_MATCH_LEN) == 0) {
		char *strstart;
#ifdef OPTION_PREFIX_SLASH
		int numchars;
#endif
		strstart = s + (OPT_MATCH_LEN + 1);
		while(*strstart != '=' && *strstart != '\0')
			strstart++;	/* Find the = sign */
		if(*strstart == '\0') {
		    fprintf(stderr,"String setting missing: %s: ignored\n",s);
		    return;
		}
		else {
		    *(strsetting[i].strvalue) = ++strstart;
				/* In VMS,MSDOS worlds, user might not leave
				   blank space between options.  If string
				   is followed by '/', must make a properly
				   terminated copy.  */
#ifdef OPTION_PREFIX_SLASH
		    for(numchars=0; strstart[numchars] != '\0'
			&& strstart[numchars] != '/'; numchars++)
		      continue;
		    if(strstart[numchars] != '\0') {
		      strncpy( *(strsetting[i].strvalue)=ckalloc(numchars+1),
			       strstart,numchars);
		    }
#endif

		}
			/* Handle necessary action for  -out=listfile */
		if(strsetting[i].strvalue == &out_fname) {
			must_open_outfile = TRUE;
		}
		return;
	    }


		/* No match found: issue error message */

	fprintf(stderr,"\nUnknown commandline switch: %s\n",s);
}
#endif

	/* Routine to read integer setting from string s and check if valid */

PRIVATE int
read_setting(s, setvalue, name, minlimit, maxlimit, turnoff)
	char *s;
	int *setvalue;
	char *name;
	int minlimit, maxlimit, turnoff;
{
	int given_val;

	if(strcmp(s,"NO")==0) {
	  *(setvalue) = turnoff;
	}
	else if(*s == '\0' || sscanf(s,"%d", &given_val) == 0) {
	    return -1;	/* error return: garbled setting */
	}
	else {		/* If outside limits, set to nearest limit */
	    int Ok=TRUE;
	    if(given_val < minlimit) {
		given_val = minlimit;
		Ok = FALSE;
	    }
	    else if(given_val > maxlimit) {
		given_val = maxlimit;
		Ok = FALSE;
	    }

	    if(! Ok ) {
		fprintf(stderr,"\nSetting: %s",name);
		fprintf(stderr," outside limits %d to %d",
				minlimit,maxlimit);
		fprintf(stderr,": set to %d\n",given_val);
	    }

	    *(setvalue) = given_val;
	}
	return 0;
}

#if 0
PRIVATE void
open_outfile(s)		/* open the output file for listing */
	char *s;
{
	char *fullname;		/* given name plus extension */
	FILE *fd;

	must_open_outfile = FALSE;	/* Turn off the flag */

	if(s == (char *) NULL || *s == '\0') {
		return;		/* No filename: no action  */
	}

	fullname = add_ext(s,DEF_LIST_EXTENSION);
	if( (fd = fopen(fullname,"w")) == NULL) {
		fprintf(stderr,"\nCannot open %s for output\n",fullname);
	}
	else {
		fprintf(stderr,"\nOutput sent to file %s\n",fullname);
		list_fd = fd;
	}
}


PRIVATE void
list_options(fd)/* List all commandline options, strsettings, and settings */
     FILE *fd;
{
	int i;

			/* Print the copyright notice */
	fprintf(fd,"\n%s",COPYRIGHT_DATE);
	fprintf(fd,"\n%s\n",COPYRIGHT_NOTICE);

		/* Note: Headings say "default" but to be accurate they
		   should say "current value".  This would be confusing. */
	fprintf(fd,"\nCommandline options [default]:");
	for(i=0; i<NUM_SWITCHES; i++) {

	  if( !debug_latest &&
	     strncmp(switchopt[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug switches unless debug mode */

	  fprintf(fd,"\n    %c[no]%s",OPT_PREFIX,switchopt[i].name);
	  fprintf(fd," [%s]",*(switchopt[i].switchflag)? "yes": "no");
	  fprintf(fd,": %s",switchopt[i].explanation);
	}
		/* String settings follow switches w/o their own heading */
	for(i=0; i<NUM_STRSETTINGS; i++) {
	  if( !debug_latest &&
	     strncmp(strsetting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  fprintf(fd,"\n    %c%s=str ",OPT_PREFIX,strsetting[i].name);
	  fprintf(fd,"[%s]",
	  	*(strsetting[i].strvalue)? *(strsetting[i].strvalue): "NONE");
	  fprintf(fd,": %s",strsetting[i].explanation);
	}

	fprintf(fd,"\nSettings (legal range) [default]:");
	for(i=0; i<NUM_SETTINGS; i++) {

	  if( !debug_latest &&
	     strncmp(setting[i].explanation,"debug",5) == 0)
	    continue;		/* skip debug settings unless debug mode */

	  fprintf(fd,"\n    %c%s=dd ",OPT_PREFIX,setting[i].name);
	  fprintf(fd,"(%d to %d) ",setting[i].minlimit,
		  setting[i].maxlimit);
	  fprintf(fd,"[%d]",*(setting[i].setvalue));
	  fprintf(fd,": %s",setting[i].explanation);
	}

    fprintf(fd,
    	"\n(First %d chars of option name significant)\n",OPT_MATCH_LEN);
}


PRIVATE void
wrapup()	/* look at cross references, etc. */
{
	if(debug_hashtab || debug_glob_symtab)
	  debug_symtabs();

	visit_children();	/* Make call tree & check visited status */
	check_com_usage();	/* Look for unused common stuff */
	check_comlists();	/* Look for common block mismatches */
	check_arglists();	/* Look for subprog defn/call mismatches */
}
#endif


#define MODE_DEFAULT_EXT 1
#define MODE_REPLACE_EXT 2
PRIVATE char *
append_extension(s,ext,mode)
     char *s,*ext;
     int mode;
{
		/* MODE_DEFAULT_EXT: Adds extension to file name s if
		   none is present, and returns a pointer to the
		   new name.  If extension was added, space is allocated
		   for the new name.  If not, simply  returns pointer
		   to original name.  MODE_REPLACE_EXT: same, except given
		   extension replaces given one if any.
		*/
	int i,len;
	char *newname;
#ifdef OPTION_PREFIX_SLASH	/* set len=chars to NUL or start of /opt */
	for(len=0; s[len] != '\0' && s[len] != '/'; len++)
	  continue;
#else
	len=(unsigned)strlen(s);
#endif
		/* Search backwards till find the dot, but do not
		   search past directory delimiter
		*/
	for(i=len-1; i>0; i--) {
	    if(s[i] == '.'
#ifdef UNIX
	       || s[i] == '/'
#endif
#ifdef VMS
	       || s[i] == ']' || s[i] == ':'
#endif
#ifdef MSDOS
	       || s[i] == '\\' || s[i] == ':'
#endif
	       )
		break;
	}

	if(mode == MODE_REPLACE_EXT) {
	  if(s[i] == '.')	/* declare length = up to the dot */
	    len = i;
	  newname = (char *) ckalloc( (unsigned)(len+(unsigned)strlen(ext)+1) );
	  (void)strncpy(newname,s,len);
	  (void)strcpy(newname+len,ext);
	}
	else {			/* MODE_DEFAULT_EXT */
#ifdef OPTION_PREFIX_SLASH
		/* create new string if new ext or trailing /option */
	  if(s[i] != '.' || s[len] != '\0') {
	    if(s[i] != '.') {	/* no extension given */
	      newname = (char *) ckalloc( (unsigned)(len+(unsigned)strlen(ext)+1) );
	      (void)strncpy(newname,s,len);
	      (void)strcpy(newname+len,ext);
	    }
	    else {		/* extension given but /option follows */
	      newname = (char *) ckalloc( (unsigned)(len+1) );
	      (void)strncpy(newname,s,len);
	    }
	  }
#else
	  if(s[i] != '.') {
	    newname = (char *) ckalloc( (unsigned)(len+(unsigned)strlen(ext)+1) );
	    (void)strcpy(newname,s);
	    (void)strcat(newname,ext);
	  }
#endif
	  else {
	    newname = s;	/* use as is */
	  }
	}

	return newname;
}

		/* Adds default extension to source file name, replacing
		   any that is present, and returns a pointer to the
		   new name.  Space is allocated for the new name.
		*/
char *
add_ext(s,ext)			/* adds default filename extension to s */
	char *s,*ext;
{
  return append_extension(s,ext,MODE_DEFAULT_EXT);
}

char *
new_ext(s,ext)
	char *s,*ext;
{
  return append_extension(s,ext,MODE_REPLACE_EXT);
}


PRIVATE int
cistrncmp(s1,s2,n)			/* case-insensitive strncmp */
     char *s1,*s2;
     unsigned n;
{
  while( n != 0 &&
      (isupper(*s1)?tolower(*s1):*s1) == (isupper(*s2)?tolower(*s2):*s2) ) {
    if(*s1 == '\0')
      return 0;
    if(*s2 == '\0')
      break;
    ++s1; ++s2; --n;
  }
  return n==0? 0: *s1 - *s2;
}

int
has_extension(name,ext)		/* true if name ends in ext */
  char *name,*ext;
{
  unsigned name_len, ext_len;
  int stem_len;
  ext_len = strlen(ext);

#ifdef VMS	/* shell_glob adds version number: filename.ext;1 */
  if(strrchr(name,';') != NULL) {
    name_len = strrchr(name,';') - name; /* distance to the semicolon */
  }
  else
#endif
    name_len=strlen(name);	/* distance to the null */

  stem_len = (unsigned)(name_len - ext_len); /* distance to the dot */

  if( stem_len >= 0 &&
     (name_len-stem_len) == ext_len &&
     cistrncmp(name+stem_len,ext,ext_len) == 0 )
    return TRUE;
  else
    return FALSE;
}

		/* Add an include directory path to list of paths */
#ifdef ALLOW_INCLUDE
PRIVATE void
append_include_path(new_path)
     char *new_path;
{
  IncludePathNode *new_path_node, *p;
  if((new_path_node=(IncludePathNode *)ckalloc(sizeof(IncludePathNode)))
     ==(IncludePathNode *)NULL)
    fprintf(stderr,"\nmalloc error getting path list");
  else {
    new_path_node->link = (IncludePathNode *)NULL;
    new_path_node->include_path = new_path;
				/* Append the new node at end of list */
    if((p=include_path_list) == (IncludePathNode *)NULL)
      include_path_list = new_path_node;
    else {
      while(p->link != (IncludePathNode *)NULL)
	p = p->link;
      p->link = new_path_node;
    }
  }
}
#endif/*ALLOW_INCLUDE*/

#if 0
PRIVATE void
resource_summary()
{
#ifdef DEVELOPMENT
	    if(debug_latest)
	      print_sizeofs();	/* give sizeof various things */
#endif
	    fprintf(list_fd,
    "\nMax namestring space used = %lu local, %lu global out of %lu chars",
			max_loc_strings,
			max_glob_strings,
			(unsigned long)STRSPACESZ);
	    fprintf(list_fd,
		"\nMax local symbols used =  %lu out of %lu available",
			max_loc_symtab,
			(unsigned long)LOCSYMTABSZ);
	    fprintf(list_fd,
		"\nMax global symbols used = %lu out of %lu available",
			max_glob_symtab,
			(unsigned long)GLOBSYMTABSZ);
	    fprintf(list_fd,
		"\nMax number of tokenlists used = %lu out of %lu available",
			max_tokenlists,
			(unsigned long)TOKENSPACESZ);
	    fprintf(list_fd,
		"\nMax tokenlist space used = %lu out of %lu available",
			max_token_space,
			(unsigned long)TOKENSPACESZ);
	    fprintf(list_fd,
		"\nNumber of subprogram invocations = %lu totaling %lu args",
			arglist_head_used,
			arglist_element_used);
	    fprintf(list_fd,
		"\nNumber of common block decls = %lu totaling %lu variables",
			comlist_head_used,
			comlist_element_used);
	    fprintf(list_fd,
		"\nIdentifier hashtable size = %6lu",
			(unsigned long)HASHSZ);
#ifdef KEY_HASH/* not used any more*/
	    fprintf(list_fd,
		"\nKeyword hashtable size = %6lu",
			(unsigned long)KEYHASHSZ);
#endif
#ifdef COUNT_REHASHES
	    fprintf(list_fd,
		"\nIdentifier rehash count = %6lu",
			rehash_count);
#endif
	    fprintf(list_fd,
		"\nIntrinsic function hashtable size=%6lu, clash count=%lu",
			(unsigned long)INTRINS_HASHSZ,
			intrins_clashes);
	    fprintf(list_fd,"\n\n");
}
#endif

