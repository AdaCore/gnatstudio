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

/* forlex.c:

	Tokenizing routines for Fortran program checker.

This version implements blank-insensitivity.

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


 Part I. yylex()  -- gives tokens to the parser.
 Part II. advance() -- bottom-level scanning of input stream.

*/

#define CASE_SENSITIVE


	/* Declarations shared by all modules */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#if defined (__MSVC__) || defined(__STDC__) || defined(__osf__)
#include <stdlib.h>
#else
char *getenv();
#endif

#include <tcl.h>

#include "ftnchek.h"
#define FORLEX
#include "symtab.h"
#include "fortran.h"
#include "sn.h"

int max_line_width = 0;

extern FILE *hig_fp;
extern int	highlight;
extern Tcl_Encoding encoding;

/* lexdefs.h:
		Macros and shared info for lexical analysis routines
*/

#define LEX_SHARED PRIVATE

#define EOL     '\n'    /* Character for end of line, not of statement */

extern YYSTYPE yylval;	  /* Lexical value for Yacc */


	/* Since EOS is special, need special macros for it */
#define makeupper(C) (((C) != EOS && islower((int)(C)))? toupper((int)(C)):(C))
#define iswhitespace(C) ( (C) != EOS && isspace((int)(C)) )
#define isadigit(C)     ( (C) != EOS && isdigit((int)(C)) )
#define isaletter(C)    ( (C) != EOS && isalpha((int)(C)) )
#define ishex(C) ((C) != EOS && (isdigit((int)(C)) ||\
			(toupper((int)(C))>='A' && toupper((int)(C))<='F') ))

	/* define isidletter to allow underscore and/or dollar sign or not */
				/* both underscore and dollar sign */
#define isidletter(C)    ( (C) != EOS && (isalpha((int)(C)) || \
					  (C) == '_' || (C) == '$' ) )

#define BCD(C) ((C)-'0')	/* Binary value of digit */
#define HEX(C) (isdigit(C)?BCD(C):(makeupper(C)-'A'+10)) /* Hex value */

				/* Blank-insensitive advance */
#define bi_advance()	do {advance();} while(iswhitespace(curr_char))

#define bi_advanceX()	{ do {advance(); i_white++; } while(iswhitespace(curr_char)); }

LEX_SHARED int
	inside_string,		/* TRUE when reading a string or hollerith */
	inside_hollerith,	/* TRUE when reading a hollerith */
	WHILE_expected,		/* DO seen and WHILE is coming up */
	contin_count,		/* Number of continuation lines of stmt */
  	prev_char,		/* shared between forlex.c and advance.c */
	curr_char,		/* Current input character */
	next_char;		/* Lookahead character */

#ifdef UNIX_CPP
LEX_SHARED char
	*next_filename;
LEX_SHARED int
	cpp_handled;
#endif

PRIVATE char acSymbol[1000];
PRIVATE int Symbol_line_num;
PRIVATE int Symbol_col_num;
PRIVATE int Symbol_curr_index;
PRIVATE char acCppInclude[1000];

extern int complex_const_allowed,    /* shared flags operated by fortran.y */
	   inside_format,
	   integer_context;
extern int stmt_sequence_no;	/* shared with fortran.y */

		/* Declare shared lexical routines */
LEX_SHARED
void advance();
LEX_SHARED
int is_keyword(), looking_at_cplx(), looking_at_keywd(), looking_at_relop();

#ifdef DEBUG_INCLUDE
LEX_SHARED
int debug_include=FALSE;
#endif


/*

Part I. yylex()

   Shared functions defined:
	yylex()			Returns next token.  Called from yyparse().
	implied_id_token(t,s)	Creates token for blank common declaration.

Note: compilation options LEX_STORE_STRINGS and LEX_STORE_HOLLERITHS:
  Define the macro name LEX_STORE_STRINGS to build a version of ftnchek that
  stores string constants, and LEX_STORE_HOLLERITHS to store hollerith
  constants.  Now that INCLUDE statements are supported, strings must
  be stored.  Holleriths are not used, so they need not be stored.
*/
#define LEX_STORE_STRINGS

#ifdef DEVELOPMENT		/* For maintaining the program */
abc
#define LEX_STORE_HOLLERITHS
#define DEBUG_FORLEX
#endif

#include <math.h>



	/* The following macro says whether a given character is legal,
	 * i.e. one of the stream control chars or a valid ANSI Fortran
	 * character.  Lower case letters are considered legal too.
	 * Nondigits in columns 1-6 (except EOF,EOS) are illegal.
	 * Hopefully this works for EBCDIC too.
	 */
#define islegal(C) ( ((C) == EOF) || ((C) == EOS) || \
	( (col_num >= 6 || isdigit(C)) && \
	 ((C) >= ' ' && (C) <= 'z' && \
	  legal_chars[toascii((int)(C))-toascii(' ')] == (C))) )

		/* Array has x where ASCII character is not valid */
PRIVATE char legal_chars[]=
" x\"x$xx'()*+,-./0123456789:x<=>xx\
ABCDEFGHIJKLMNOPQRSTUVWXYZxxxx_xabcdefghijklmnopqrstuvwxyz";

PRIVATE int
#if 0
	curr_index,		/* Index in line of curr_char */
#endif
	next_index;		/* Index in line of next_char */

		/* local functions defined */
PRIVATE void
#ifdef UNIX_CPP
	get_cpp_directive(),
#endif
	get_dot(), get_dotted_keyword(), get_edit_descriptor(), get_hollerith(),
	get_identifier(), get_illegal_token(), get_label(),
	get_letter(), get_number(), get_punctuation(),
	get_simple_punctuation(), get_string(),
#ifdef TYPELESS_CONSTANTS
get_binary_const(),
#endif
	get_complex_const();

static void save_comment(char *filename, char *func, char *classn, int line_num, int col_num, char *acComment);

PRIVATE void
	closeup();


		/*  Gets next token for Yacc.  Return value is token.class,
		 *  and a copy of the token is stored in yylval.
		 */
int
yylex()
{
    Token token;

	token.next_token = 0;
	token.dot_token  = 0;

	if( acCppInclude[0] )
	{
		char acFilename[1000];

		strcpy( acFilename, acCppInclude );
		acCppInclude[0] = 0;
    	open_include_file( SN_StrDup( acFilename ));
	}

	if( acSymbol[0] )
	{
		int h;
		Lsymtab *symt;

/* 		printf( "acSymbol: <%s> %d %d\n", acSymbol, Symbol_line_num, Symbol_col_num ); */
    	token.subclass = 0;
	    token.line_num = Symbol_line_num;
	    token.col_num = Symbol_col_num;
	    token.curr_index = Symbol_curr_index;
		token.class = tok_identifier;
		token.value.integer = h = hash_lookup(acSymbol);
		if((symt=hashtab[h].loc_symtab) != NULL && symt->array_var) {
		      token.class = tok_array_identifier;
		}
		acSymbol[0] = 0;
        yylval = token;
        return token.class;
	}

		/* Initialize token fields to scratch. */
    token.subclass = 0;
    token.value.integer = 0;

    if(curr_char == EOF) {
	token.class = EOF;
	token.line_num = line_num;
	token.col_num = col_num;
	token.curr_index = curr_index;
    }
    else /* not EOF */ {


		/* Skip leading spaces, and give error message if non-ANSI
		 * characters are found.
		 */

	while(iswhitespace(curr_char) || (! islegal(curr_char))  ) {
	  if(!iswhitespace(curr_char)) {
#ifdef UNIX_CPP
	    if(curr_char == '#' && col_num == 1) {
	       get_cpp_directive();	/* turn # line into EOS */
	       break;
	    }
	    else
#endif
		yyerror("Illegal character");
	  }
	  advance();
	}

	token.line_num = line_num;
	token.col_num = col_num;
	token.curr_index = curr_index;

	if(inside_format) {	/* Handle format stuff here to avoid trouble */
	  get_edit_descriptor(&token);
	}
	else if(isadigit(curr_char)) {
		if(col_num < 6)
			get_label(&token);      /* Stmt label */
		else
			get_number(&token);     /* Numeric or hollerith const */
	}
	else if(isidletter(curr_char)) {
		if(implicit_letter_flag)
			get_letter(&token);	/* letter in IMPLICIT list */
		else
			get_identifier(&token); /* Identifier or keyword */
	}
	else if(curr_char == '\'' || curr_char == '"') {
			get_string(&token);	/* Quoted string */
	}
	else if(curr_char == '.') {
			get_dot(&token);	 /* '.' lead-in */
	}
	else {
			get_punctuation(&token);  /* Punctuation character or EOS */
	}
    }/*end not EOF*/

    if(token.class == EOS) {
	implicit_flag=FALSE;	/* in case of errors, reset flags */
	implicit_letter_flag = FALSE;
    }


    prev_token_class = token.class;

    yylval = token;

/* 	printf( "Token: %d\n", token.class ); */
    return token.class;

} /* yylex */



	/* Fills argument with token for an identifer, as if an identifer
	 * with name given by string s had been lexed.  This will
	 * be called by parser when blank common declaration is seen,
	 * and when a main prog without program statement is found,
	 * and when an unnamed block data statement is found,
	 * so processing of named and unnamed cases can be handled uniformly.
	*/
void
implied_id_token(t,s)
	Token *t;
	char *s;
{
	int h;
	unsigned long hnum;

	hnum = hash(s);
	while( h=hnum%HASHSZ, hashtab[h].name != NULL &&
		strcmp(hashtab[h].name,s) != 0)
			hnum = rehash(hnum);
	if(hashtab[h].name == NULL) {	/* not seen before */
		hashtab[h].name = s;
		hashtab[h].loc_symtab = NULL;
		hashtab[h].glob_symtab = NULL;
		hashtab[h].com_loc_symtab = NULL;
		hashtab[h].com_glob_symtab = NULL;
	}
	t->class = tok_identifier;
	t->value.integer = h;

} /* implied_id_token */

#ifdef UNIX_CPP
		/* This does not create a token but just performs the
		   actions needed when a cpp directive is seen.  It
		   advances curr_char to the EOS.  The setting of
		   filename is delayed to this point because it is not
		   stored in tokens but is external, so changing it
		   must wait till the previous statement is fully
		   parsed and any error messages printed and arg or
		   com list headers completed.
		 */
#ifdef rigo
PRIVATE void
get_cpp_directive()
{
  if(next_filename != (char *)NULL) {
    current_filename = next_filename;
    if(incdepth == 0)
      top_filename = next_filename;
  }
  do {			/* Skip to end of directive.  It will become an EOS */
    advance();
  } while( curr_char != EOS);

  if(f77_standard || !cpp_handled) {
    nonstandard(line_num,col_num);
    msg_tail(": preprocessor directive");
    if(!cpp_handled)
      msg_tail("(not processed)");
  }
}/*get_cpp_directive*/
#endif

static void get_cpp_directive()
{
    char ac[1000];
    char *pc;
    char *pcFilename;
    char *pcDefinename;
    int my_line_num;

    if( next_filename != (char *)NULL )
    {
        current_filename = next_filename;
        if( incdepth == 0 )
        {
            top_filename = next_filename;
        }
    }

    pc = ac;
    my_line_num = line_num;

    do
    {                  /* Skip to end of directive.  It will become an EOS */
        advance();
        *pc++ = curr_char;
    } while( curr_char != EOS);

    pc[-1] = 0;

    for( pc = ac; *pc; pc++ )
    {
        if( ! isspace( *pc )) break;
    }

    if( strncmp( pc, "include", 7 ) == 0 )
    {
	    if( hig_fp )
	    {
		    if( strcmp( current_filename, top_filename ) == 0 )
		    {
			    fprintf( hig_fp, "%d key %d.%d %d.%d\n"
				       , PAF_HIGH
				       , my_line_num
				       , 0
				       , my_line_num
				       , pc - ac + 8
				       ); 
		    }
	    }

        pcFilename = 0;

        for( ; *pc; pc++ )
        {
            if( *pc == '"' || *pc == '<' )
            {
                pcFilename = pc+1;
                break;
            }
        }

        if( pcFilename )
        {
            for( pc = pcFilename; *pc; pc++ )
            {
                if( *pc == '"' || *pc == '>' )
                {
                   *pc = 0;
                   break;
                }
            }
			strcpy( acCppInclude, pcFilename );
        }
    }
    else if( strncmp( pc, "define", 6 ) == 0 )
    {
	int my_index;

	if( hig_fp )
	{
	    if( strcmp( current_filename, top_filename ) == 0 )
	    {
		    fprintf( hig_fp, "%d key %d.%d %d.%d\n"
			       , PAF_HIGH
			       , my_line_num
			       , 0
			       , my_line_num
			       , pc - ac + 7
			       ); 
	    }
	}

        pcDefinename = 0;
	pc += 6;

        for( ; *pc; pc++)
        {
	    if( ! iswhitespace( *pc ))
            {
                pcDefinename = pc;
                break;
            }
        }
	my_index = pc - ac + 1;

        if( pcDefinename )
        {
            for( pc = pcDefinename; *pc; pc++ )
            {
#ifndef CASE_SENSITIVE
				*pc = makeupper( *pc );
#endif
				if( iswhitespace( *pc ) || *pc == '(' )
                {
                   *pc = 0;
                   break;
                }
            }

				/* Identifier: find its hashtable entry or
				   create a new entry.	*/
			{
		    	int h;

		    	h = hash_lookup(pcDefinename);
		        hashtab[h].define = 1;
				if( highlight != -1 )
				{
					put_symbol(PAF_CONS_DEF,NULL,pcDefinename,
						current_filename,
						my_line_num,
						my_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,my_line_num),
						0,0,0,0);
				}
			}
        }
	}
}

#endif

PRIVATE void
get_dot(token)
	Token *token;
{
	closeup();		/* Advance till nonspace char in next_char */

	if(isadigit(next_char))
		get_number(token);		/* Numeric const */
	else if(isaletter(next_char))
		get_dotted_keyword(token);	/* .EQ. etc. */
	else
		get_simple_punctuation(token);	/* "." out of place */
}

#define MAX_DOTTED_KEYWD (sizeof(".FALSE.")/sizeof(char))

struct {
	char *name;
	int class,subclass;
 } dotted_keywords[]={
			{"EQ",tok_relop,relop_EQ},
			{"NE",tok_relop,relop_NE},
			{"LE",tok_relop,relop_LE},
			{"LT",tok_relop,relop_LT},
			{"GE",tok_relop,relop_GE},
			{"GT",tok_relop,relop_GT},
			{"AND",tok_AND,0},
			{"OR",tok_OR,0},
			{"NOT",tok_NOT,0},
			{"FALSE",tok_logical_const,FALSE},
			{"TRUE",tok_logical_const,TRUE},
			{"EQV",tok_EQV,0},
			{"NEQV",tok_NEQV,0},
			{NULL,0,0}
		    };


PRIVATE void
get_dotted_keyword(token)
	Token *token;
{
	char s[8];
	int i=0,
	    has_embedded_space,	/* Spaces inside keyword */
	    space_seen_lately;	/* Flag for catching embedded space */
	int j=0;

	initial_flag = FALSE;
				/* Watch for embedded space, but not
				   between dots and letters of keyword.
				   I.e.  ". eq ." is OK, but not ".e q." */
	has_embedded_space = FALSE;
	space_seen_lately = FALSE;

	bi_advance();      /* gobble the initial '.' */

	Symbol_line_num   = line_num;
	Symbol_col_num    = col_num;
	Symbol_curr_index = curr_index;

/* 	while(isaletter(curr_char)) { */
	while(isidletter(curr_char) || isadigit(curr_char)) {
	  if( i<7 )
	    s[i++] = makeupper(curr_char);

#ifdef CASE_SENSITIVE
	  acSymbol[j++] = curr_char;
#else
	  acSymbol[j++] = makeupper(curr_char);
#endif

	  if(space_seen_lately)
	    has_embedded_space = TRUE;

	   bi_advance();

	   space_seen_lately = iswhitespace(prev_char);
	}
	s[i]        = '\0';
	acSymbol[j] = '\0';

/* 	printf( "***** <%s>\n", acSymbol ); */

	for(i=0; dotted_keywords[i].name != NULL; i++) {
		if(strcmp(s,dotted_keywords[i].name) == 0) {
			token->class = dotted_keywords[i].class;
			token->subclass = dotted_keywords[i].subclass;
			token->value.string = dotted_keywords[i].name;
			if(curr_char != '.') {
				yyerror("Badly formed logical/relational operator or constant");
			}
			else {
				advance();      /* gobble the final '.' */
				acSymbol[0] = 0;
			}
			return;
		}
	}

	/* keyword not found */

	token->class = '.';

} /* get_dotted_keyword */

static void get_edit_descriptor(token)
Token *token;
{
    while( curr_char != EOS && curr_char != EOF )
    {
	advance();
    };
    token->class = EOS;
}

#ifdef rigo
PRIVATE void
get_edit_descriptor(token)
	Token *token;
{
    int i=0,c;
    long repeat_spec;
    char s[MAXIDSIZE+1];	/* string holding the descriptor: NOT STORED */

    if(isadigit(curr_char)) {	/* Digit: repeat spec or holl or kP or nX */
      repeat_spec = 0;
      do {
	repeat_spec = repeat_spec*10L + (long)BCD(curr_char);
	if( makeupper(next_char) == 'H' )
	  inside_hollerith = TRUE;/* get ready for hollerith*/
	bi_advance();
      } while(isadigit(curr_char));

      if( makeupper(curr_char) == 'H' ) {
				/* nH... pass off to hollerith routine */
	get_hollerith(token, (int)repeat_spec);
	return;
      }
      else {
				/* Otherwise it is a repeat spec or the
				   numeric part of kP or nX which we treat
				   as repeat specs too */
	token->class = tok_integer_const;
	token->value.integer = repeat_spec;
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nInteger const:\t\t%d",repeat_spec);
#endif
      }
    }/* end if digit */

    else if(isaletter(curr_char)) {
      c = makeupper(curr_char);
      s[i++] = c;
      bi_advance();
      switch(c) {

	case 'P':		/* P of kP  k seen previously */
	  if(prev_token_class != tok_integer_const) {
	    if(f77_standard){
	      nonstandard(token->line_num,token->col_num);
	      msg_tail(": P must follow a number");
	    }
	  }
	  break;

	case 'X':		/* X or nX */
	  break;

	case 'S':		/* S or SP or SS */
	  c = makeupper(curr_char);
	  if(c == 'S' || c == 'P') {
	    s[i++] = c;
	    bi_advance();
	  }
	  break;

	case 'B':		/* BN or BZ */
	  c = makeupper(curr_char);
	  if(c == 'N' || c == 'Z') {
	    s[i++] = c;
	    bi_advance();
	  }
	  else {
	    if(f77_standard){
	      nonstandard(token->line_num,token->col_num);
	      msg_tail(": N or Z expected after B");
	    }
	  }
	  break;

	case 'T':		/* Tc or TLc or TRc */
	  c = makeupper(curr_char);
	  if(c == 'L' || c == 'R') {
	    s[i++] = c;
	    bi_advance();
	  }
	  goto get_w_d;
				/* Iw, Ew.c and similar forms */
	case 'A':	case 'D':	case 'E':
	case 'F':	case 'G':	case 'L':
	case 'I':
get_w_d:				/* Get the w field if any */
	  while( isadigit(curr_char) ){
	    if(i < MAXIDSIZE)	/* Store it temporarily (up to a point) */
	      s[i++] = curr_char;
	    bi_advance();
	  }
			/* Include any dot followed by number (e.g. F10.5)
			*/
      	  if( curr_char == '.' ) {
	    do {
	      if(i < MAXIDSIZE)
		s[i++] = curr_char;
	      bi_advance();
	    } while( isadigit(curr_char) );
	  }
	  break;

	default:
	  if(f77_standard) {
	    nonstandard(token->line_num,token->col_num);
	    msg_tail(": edit descriptor");
	    s[i] = '\0';
	    msg_tail(s);
	  }
	  goto get_w_d;
      }/*end switch*/

      token->class = tok_edit_descriptor;
      token->value.string = NULL;
      s[i++] = '\0';
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nEdit descriptor:\t%s",s);
#endif
    }/*end else if isaletter*/

				/* Apostrophe means a string */
    else if( curr_char == '\'' || curr_char == '"' ) {
      get_string(token);
    }
				/* Otherwise it is mere punctuation. Handle
				   it here ourself to avoid complications. */
    else {
      get_simple_punctuation(token);
    }
}
#endif

PRIVATE void
get_hollerith(token,n)  /* Gets string of form nHaaaa */
	Token *token;
	int n;
{
	int i,last_col_num;
/* Holl. consts are not stored unless the macro name LEX_STORE_HOLLERITHS
   is defined. */
#ifdef LEX_STORE_HOLLERITHS
	int strsize=n;
	char *s;
#else
       	char *s = "Not stored";
#endif
	initial_flag = FALSE;
#ifdef LEX_STORE_HOLLERITHS
	if( (s=(char *)ckalloc((unsigned)(strsize+1))) == (char *)NULL ) {
	  oops_message(OOPS_NONFATAL,line_num,col_num,
		       "Out of string space for hollerith constant");
	  strsize=0;
	}
	memset (s, 0, (strsize+1));
#endif
	if(n==1)
	  inside_hollerith=FALSE;/* turn off flag ahead of next_char */
	advance();/* Gobble the 'H' */

	last_col_num = col_num;
	for(i=0; i<n; i++) {
	  while(curr_char == EOL) {
			/* Treat short line as if extended with blanks */
	    int col;
	    for(col=last_col_num; i<n && col<max_stmt_col; i++,col++) {
#ifdef LEX_STORE_HOLLERITHS
	      if(i < strsize)
		s[i] = ' ';
#endif
	    }
	    last_col_num = col_num;
	    advance();
	  }
	  if(i==n) break;

	  if(curr_char == EOS || curr_char == EOF) {
	    int col;
	    for(col=last_col_num; i<n && col<max_stmt_col; i++,col++) {
#ifdef LEX_STORE_HOLLERITHS
	      if(i < strsize)
		s[i] = ' ';
#endif
	    }
#ifdef LEX_STORE_HOLLERITHS
	    strsize=i;		/* in case it did not fill up */
#endif
	    break;
	  }
	  else {
#ifdef LEX_STORE_HOLLERITHS
	    s[i] = curr_char;
#endif
	    last_col_num = col_num;
	    if(i==n-2)/* turn flag off ahead of next_char*/
	      inside_hollerith = FALSE;
	    advance();
	  }
	}

#ifdef LEX_STORE_HOLLERITHS
	if(strsize > 0)
	  s[strsize] = '\0';
#endif

	inside_hollerith = FALSE;
	token->class = tok_hollerith;
	token->value.string = s;
	token->size = n;
#ifdef DEBUG_FORLEX
	if(debug_lexer)
		fprintf(list_fd,"\nHollerith:\t\t%s",s);
#endif

} /* get_hollerith */

#include "keywords.h"

	/* get_identifier reads a string of characters satisfying
	   isidletter.  As they are read and as long as they are
	   alphabetic, it looks for a match to a keyword, and
	   whenever one is found, checks with is_keyword to see
	   if the context is right.  If so, it returns the keyword.
	   Otherwise it keeps going and eventually returns the id.
	 */
PRIVATE void
get_identifier(token)
	Token *token;
{
	char s_upper[MAXIDSIZE+1];	/* string holding the identifier */
	char s_lower[MAXIDSIZE+1];	/* string holding the identifier */
	int c,		/* Uppercase version of current letter */
	    preceding_c,/* Char preceding latest id */
	    has_embedded_space,	/* Spaces inside keyword or id */
	    space_seen_lately,	/* Flag for catching embedded space */
	    i,		/* Index in s of current letter */
	    lo,hi,	/* Indices in keyword table where match may be */
	    klen,	/* Length of id read so far (after keyword test) */
	    keywd_class;/* Class number returned by is_keyword */
	int possible_keyword;
	int i_white;

	token->class = tok_identifier;
	keywd_class = FALSE;

	i = klen = 0;
	lo = 0;
	hi = NUM_KEYWORDS-1;
	i_white = 0;

	/* Define shorthand for the keyword letter under study */
#define KN(i) keywords[i].name
#define KL(i) keywords[i].name[klen]

	possible_keyword = TRUE;
	preceding_c = prev_char;
	has_embedded_space = FALSE;
	space_seen_lately = FALSE;

			/* This loop gets  letter [letter|digit]* forms */
	while(isidletter(curr_char) || isadigit(curr_char)) {
	  c = makeupper(curr_char); /* Get the next char of id */
	  if(i < MAXIDSIZE)	/* Store it (up to a point) */
	  {
	    s_upper[i] = c;
	    s_lower[i] = curr_char;
		i++;
	  }

	  if(space_seen_lately)
	  {
	    has_embedded_space = TRUE;
	  }

	  bi_advanceX();		/* Pull in the next character */

	  space_seen_lately = iswhitespace(prev_char);

				/* As long as it may yet be a keyword,
				   keep track of whether to invoke is_keyword.
				 */
	  if(possible_keyword) {

	    if(!isaletter(c)	/* If not alphabetic, cannot be keyword */
	       || klen >= sizeof(keywords[0].name)-1) /* or overlength */
	    {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
s[i] = '\0';
fprintf(list_fd,"\n%s not a keyword because",s);
if(!isaletter(c))
  fprintf(list_fd," non-letter at %c",c);
if(klen >= sizeof(keywords[0].name)-1)
  fprintf(list_fd,"length %d >= max %d",klen,sizeof(keywords[0].name)-1);
}
#endif
	      possible_keyword = FALSE;
	    }
	    else {
	      int mid;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
fprintf(list_fd,"\nklen=%d c=%c",klen,c);
fprintf(list_fd,"\nBisecting [lo,hi]=[%d,%d] \"%s\"..\"%s\"",
	   lo,hi,KN(lo),KN(hi));
}
#endif
				/* Bisect lo .. hi looking for match
				   on characters found so far. */
	      while(lo <= hi) {
		mid = (lo + hi)/2;
		if( KL(mid) < c ) {	/* No match in lower half */
		  lo = mid+1;
		}
		else if( KL(mid) > c ) {/* No match in upper half */
		  hi = mid-1;
		}
		else {		/* Match at midpoint: Bisect each
				   half to find the new subinterval. */
		  int midlo=mid, midhi=mid;
				/* Bisect lo .. mid */
		  while( lo < midlo-1 &&  KL(lo) != c) {
		    mid = (lo + midlo)/2;
		    if(  KL(mid) < c ) {
		      lo = mid+1;
		    }
		    else {	/* equal */
		      midlo = mid;
		    }
		  }
		  if( KL(lo) != c )
		    lo = midlo;
				/* Bisect mid .. hi */
		  while( midhi < hi-1 && KL(hi) != c ) {
		    mid = (midhi + hi)/2;
		    if( KL(mid) > c ) {
		      hi = mid-1;
		    }
		    else {	/* equal */
		      midhi = mid;
		    }
		  }
		  if( KL(hi) != c )
		    hi = midhi;

		  break;	/* After bisecting each half, we are done */
		}		/* end else KL(mid) == c */
	      }			/* end while(lo <= hi) */

	      klen++;		/* Now increment the length */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
fprintf(list_fd,"\nNew [lo,hi]=[%d,%d] \"%s\"..\"%s\"",
	   lo,hi,KN(lo),KN(hi));
}
#endif
			/* If range is null, a match has been ruled out. */
	      if(lo > hi) {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("BISECTION")) {
s[i]='\0';
fprintf(list_fd,"\nKeyword ruled out for %s at length %d since lo %d > hi %d",
	   s,klen,lo,hi);
}
#endif
		possible_keyword = FALSE;
	      }
			/* If length of first keyword in range is equal
			   to the new length, then we have a match at
			   this point.  Check it out with is_keyword.
			 */
	      else if(KN(lo)[klen] == '\0') {
		if( (keywd_class = is_keyword(lo)) != FALSE) {
		  token->class = keywd_class;	/* It's a keyword */
		  token->value.string = NULL;
		  s_upper[i] = 0;
		  s_lower[i] = 0;
          i++;
		  if( hig_fp )
		  {
			if( strcmp( current_filename, top_filename ) == 0 )
			{
 				fprintf( hig_fp, "%d key %d.%d %d.%d\n"
					   , PAF_HIGH
					   , token->line_num
					   , token->curr_index
					   , token->line_num
					   , token->curr_index + i_white
					   ); 
			}
		  }
		  break;	/* Quit the input loop */
		}
		else if(lo == hi) {	/* Match is unique and ruled out */
		  possible_keyword = FALSE;
		}
	      }
	    }/* end else isaletter(c) */
	  }/* end if(possible_keyword) */
	}/* end while(isidletter || isadigit) */

        if(keywd_class == FALSE) {		/* it is an identifier */

				/* Identifier: find its hashtable entry or
				   create a new entry.	*/
		    int h;
		    Lsymtab *symt;
#ifdef TYPELESS_CONSTANTS
				/* Watch out for const like X'nnn' */
		    if(i == 1 && curr_char == '\'') {
		      get_binary_const(token,s_upper[0],NULL);
		      return;
		    }
#endif
		    s_upper[i] = '\0';
		    s_lower[i] = '\0';
			i++;
#ifdef CASE_SENSITIVE
		    token->value.integer = h = hash_lookup(s_lower);
#else
		    token->value.integer = h = hash_lookup(s_upper);
#endif
				/* If it is an array give it a special token
				   class, so that arrays can be distinguished
				   from functions in the grammar. */
		    if((symt=hashtab[h].loc_symtab) != NULL
		       && symt->array_var) {
		      token->class = tok_array_identifier;

	  }
	}
				/* Check identifiers for being juxtaposed
				   to keywords or having internal space.
				   Keywords are immune to warning since
				   want to allow both GOTO and GO TO, etc.
				 */

	if(pretty_flag &&
	   (token->class==tok_identifier || token->class==tok_array_identifier)
	   && ( isidletter(preceding_c) || isadigit(preceding_c)
	       || has_embedded_space ) ) {

	      ugly_code(token->line_num,token->col_num,"identifier");
	      msg_tail(hashtab[token->value.integer].name);
#if 0	/* Keywords immune for now */
	      ugly_code(token->line_num,token->col_num,"keyword");
	      msg_tail(keywords[keytab_index[keywd_class-keytab_offset]].name);
#endif
	  if(has_embedded_space)
	    msg_tail("has embedded space");
	  else
	    msg_tail("not clearly separated from context");
	}

#ifdef DEBUG_FORLEX
	if(debug_lexer){
	    switch(token->class) {
		case tok_identifier:
			fprintf(list_fd,"\nIdentifier:\t\t%s",s);
			break;
		case tok_array_identifier:
			fprintf(list_fd,"\nArray_identifier:\t%s",s);
			break;
		default:
			fprintf(list_fd,"\nKeyword:\t\ttok_%s",s);
			break;
	    }
	}
#endif
} /* get_identifier */

/*  iskeyword:
	Determines (to the best of its current ability) whether a given
	identifier is a keyword or not.  Hopefully now no keywords are
	reserved.

	Method uses context from start of statement up to and including
	the character following the putative keyword to eliminate as
	many cases as possible.  Any non-IK keywords (those that need not
	be in the initial series of keywords of statement) have special
	code to handle them.  Any IK's that are always the second word of a
	pair are accepted if the predecessor was just seen.  The rest are
	handed off to looking_at_keywd which tries to see if
	it is an assignment statement.

	Note that some rules that could be used if F77 Standard were
	adhered to strictly are not used here.  The idea is to allow
	extensions, and leave catching syntax errors in the parser.
	For example, specification-statement keywords are not excluded
	after the first executable statement has been seen.  The status
	of a variable as declared array or character type is not consulted
	in ruling out an assignment statement if following parentheses
	are present.  Etc.
*/


		/* Macro to test if all the specified bits are set */
#define MATCH(CONTEXT) ((keywords[i].context & (CONTEXT)) == (CONTEXT))


LEX_SHARED int
is_keyword(i)
     int i;			/* Index in keywords table */
{
  int ans = FALSE;
  int putative_keyword_class;	/* Class of the supposed keyword */

  while(iswhitespace(curr_char))	      /* Move to lookahead char */
    advance();

#ifdef DEBUG_IS_KEYWORD
  if(debug_lexer){
    fprintf(list_fd,
		"\nkeyword %s: initialflag=%d implicitflag=%d ",
	    keywords[i].name,initial_flag,implicit_flag);
    fprintf(list_fd,
		"context=%o, next char=%c %o",keywords[i].context,
						curr_char,curr_char);
  }
#endif

  putative_keyword_class = keywords[i].class;

  if( !initial_flag && MATCH(IK) ) {
			/* Dispose of keywords which can only occur in initial
			   part of statement, if found elsewhere. */
    ans = FALSE;
  }

#if 0 /* This does not work: curr_stmt_class not cleared beforehand */
  else if(curr_stmt_class == tok_IF && MATCH(NI)) {
			/* Dispose of keywords which cannot occur in stmt
			   field of logical IF if that is where we are.
			 */
    ans = FALSE;
  }
#endif

  else if(MATCH(NA) && isalpha(curr_char)) {
			/* Dispose of keywords which cannot be followed
			   by alphabetic character if that is so.
			 */
    ans = FALSE;
  }

  else if(putative_keyword_class == tok_TO) {/* A non-IK case */
				/* TO always follows the word GO or
				   is followed by a variable
				   name (in ASSIGN statement).
				 */
#ifdef SPLIT_KEYWORDS

#define in_assign_stmt (curr_stmt_class == tok_ASSIGN)
    ans = (prev_token_class == (in_assign_stmt?
			          tok_integer_const:
				  tok_GO));
#else
    ans = ( curr_stmt_class == tok_ASSIGN
	   && prev_token_class == tok_integer_const);
#endif
  }
  else if(putative_keyword_class == tok_FUNCTION /* A non-IK case */
    && (stmt_sequence_no != 0 /* not the first statement of module */

        || !(initial_flag  /* if not initial can only be preceded by type */
	     || is_a_type_token(curr_stmt_class)) )) {
    ans = FALSE; /* otherwise it will be handled correctly by looking_at */
  }
  else if(putative_keyword_class == tok_WHILE) { /* A non-IK case */
    ans = WHILE_expected; /* Only occurs in DO label [,] WHILE */
    WHILE_expected = FALSE;
  }
		/* Remaining cases are IK in initial part */

			/*   Eliminate those which can are never followed
			     by '(' or '=' if that is what we have.
			 */
  else if(MATCH(NP) &&
	  (curr_char == '(' || curr_char == '=') ) {
    ans = FALSE;
  }

			/* Likewise with those that must be followed by
			   '(' but aren't  */
  else if(MATCH(MP) && curr_char != '(') {
    ans = FALSE;
  }

				/* PRECISION always follows the word DOUBLE */
  else if( putative_keyword_class == tok_PRECISION ){
    ans = (prev_token_class == tok_DOUBLE);
  }

				/* END DO: handle its DO here */
  else if( putative_keyword_class == tok_DO && curr_char == EOS ) {
	/* Also must have prev_token_class == tok_END, but
	   no need to check since end-of-statement suffices. */
    ans = TRUE;
  }


				/* Other type names always follow the word
				   IMPLICIT */
  else if( implicit_flag ) {
    ans =  MATCH(TY);
  }

  else {
		     /* Remaining cases are keywords that must be in
			initial position. If followed by '=' must be an
			identifier.  If followed by '(' then may be an array
			or character lvalue, so use looking_at to scan ahead
			to see if this is an assignment statement. */
      ans =  looking_at_keywd(putative_keyword_class);
  }


			/* Save initial token class for use by parser.
			   Either set it to keyword token or to id for
			   assignment stmt. */
  if(initial_flag) {
    curr_stmt_class = (ans? keywords[i].class: tok_identifier);
  }

		/* Turn off the initial-keyword flag if this is a
		   keyword that cannot be followed by another keyword
		   or if it is not a keyword.
		*/
  if(ans) {
    if(keywords[i].context & EK)
      initial_flag = FALSE;
    return keywords[i].class;
  }
  else {	/* If no more letters follow, then keyword here
		   is ruled out.  Turn off initial_flag. */
    if( ! isalpha(curr_char) )
      initial_flag = FALSE;
    return 0;	/* Not found in list */
  }
}/* End of is_keyword */


/*    init_keyhashtab:
*/
		/* Hashing is no longer used.  This guy now only
		   initializes the table of indices that allow
		   keywords to be looked up by their token class*/
void
init_keyhashtab()
{
  int i,k,kmin,kmax;
  kmin = kmax = keywords[0].class;	/* Find min and max token classes */
  for(i=1; i<NUM_KEYWORDS; i++) {
    k = keywords[i].class;
    if(k < kmin)  kmin = k;
    if(k > kmax)  kmax = k;
  }

  keytab_offset = kmin;	/* Index table from [kmin..kmax] -> [0..size-1] */
  keytab_size = (unsigned) (kmax-kmin+1);
  if( (keytab_index=(short *)ckalloc(keytab_size*sizeof(keytab_index[0])))
     == (short *)NULL) {
    oops_message(OOPS_FATAL,NO_LINE_NUM,NO_COL_NUM,
	   "cannot allocate space for keytab_index");
  }
  memset (keytab_index, 0, keytab_size*sizeof(keytab_index[0]));

				/* Now fill in the lookup table, indexed
				   by class - offset */
  for(i=0; i<NUM_KEYWORDS; i++) {
    k = keywords[i].class;
    keytab_index[k - keytab_offset] = i;
  }
}


PRIVATE void
get_illegal_token(token)	/* Handle an illegal input situation */
	Token *token;
{
	token->class = tok_illegal;
#ifdef DEBUG_FORLEX
	if(debug_lexer)
	     fprintf(list_fd,"\nILLEGAL TOKEN");
#endif

} /* get_illegal_token */



		/* Read a label from label field. */
PRIVATE void
get_label(token)
	Token *token;
{
	int value=0;
	int space_seen=FALSE, has_embedded_space=FALSE;
	while( isadigit(curr_char) && col_num < 6 ) {
	  if(space_seen)
	    has_embedded_space = TRUE;
	  value = value*10 + BCD(curr_char);
	  advance();
	  while(curr_char==' ' && col_num < 6) {
	    space_seen = TRUE;
	    advance();
	  }
	}
	if(pretty_flag && has_embedded_space) {
	      ugly_code(token->line_num,token->col_num,
			"label has embedded space");
	}
	token->class = tok_label;
	token->value.integer = value;
#ifdef DEBUG_FORLEX
	if(debug_lexer)
		fprintf(list_fd,"\nLabel:\t\t\t%d",value);
#endif

} /* get_label */


PRIVATE void
get_letter(token)		/* Gets letter in IMPLICIT list */
	Token *token;
{
	token->class = tok_letter;
	token->subclass = makeupper(curr_char);

#ifdef DEBUG_FORLEX
    if(debug_lexer)
	fprintf(list_fd,"\nLetter:\t\t\t%c",token->subclass);
#endif

	advance();

} /* get_letter */


	/* get_number reads a number and determines data type: integer,
	 * real, or double precision.
	 */
/* This belongs in ftnchek.h, perhaps.  Defines number of significant
   figures that are reasonable for a single-precision real constant.
   Works out to 9 for wordsize=4, 21 for wordsize=8. These allow
   for a couple of extra digits for rounding. Used in -trunc warning. */
#define REAL_SIGFIGS (local_wordsize==0? 8: (local_wordsize-1)*3)

PRIVATE void
get_number(token)
	Token *token;
{
	double dvalue,leftside,rightside,pwr_of_ten;
	int exponent,expsign,datatype,c;
	int sigfigs;

	initial_flag = FALSE;

	leftside = 0.0;
	sigfigs = 0;
	datatype = tok_integer_const;
	while(isadigit(curr_char)) {
		leftside = leftside*10.0 + (double)BCD(curr_char);
		++sigfigs;
		if( !integer_context && makeupper(next_char) == 'H' )
		  inside_hollerith = TRUE;/* get ready for hollerith*/
		bi_advance();
	}

		/* If context specifies integer expected, skip to end.
		   Otherwise scan on ahead for more. */
    if( integer_context) {
        if(sigfigs == 0) {
	    yyerror("integer expected");
	    advance();	/* gobble something to avoid infinite loop */
	}
    }
    else {/* not integer_context */
	if( makeupper(curr_char) == 'H' ){      /* nnH means hollerith */
		if(leftside == 0.0) {
			yyerror("Zero-length hollerith constant");
			inside_hollerith = FALSE;
			advance();
			get_illegal_token(token);
		}
		else {
			get_hollerith(token, (int)leftside);
		}
		return;
	}

	rightside = 0.0;
	pwr_of_ten = 1.0;
	closeup();		/* Pull in the lookahead character */
	if( curr_char == '.' &&
				/* don't be fooled by 1.eq.N or
				   I.eq.1.and. etc */
	   !looking_at_relop() ) {
		datatype = tok_real_const;
		bi_advance();
		while(isadigit(curr_char)) {
			rightside = rightside*10.0 + (double)BCD(curr_char);
			++sigfigs;
			pwr_of_ten *= 0.10;
			bi_advance();
		}
	}
#ifdef DEBUG_FORLEX
if(debug_lexer)
	dvalue = leftside + rightside*pwr_of_ten;
#endif

	exponent = 0;
	expsign = 1;

		/* Integer followed by E or D gives a real/d.p constant */

	if( ( (c = makeupper(curr_char)) == 'E' || c == 'D' ) )
	{
		datatype = ((c == 'E')? tok_real_const: tok_dp_const);
		bi_advance();
		if(curr_char == '+') {
			expsign = 1;
			bi_advance();
		}
		else if(curr_char == '-') {
			expsign = -1;
			bi_advance();
		}
		if(!isadigit(curr_char)) {
			yyerror("Badly formed real constant");
		}
		else while(isadigit(curr_char)) {
			exponent = exponent*10 + (curr_char-'0');
			bi_advance();
		}

	/*  Compute real value only if debugging. If it exceeds max magnitude,
	    computing it may cause crash. At this time, value of real const
	    is not used for anything. */
#ifdef DEBUG_FORLEX
if(debug_lexer)
		  dvalue *= pow(10.0, (double)(exponent*expsign));
else
#endif
		  dvalue = 0.0;

	}
    }/* end if(!integer_context) */
	token->class = datatype;
	switch(datatype) {
	   case tok_integer_const:
		token->value.integer = (long)leftside;
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nInteger const:\t\t%ld",token->value.integer);
#endif
		break;
	   case tok_real_const:
			/* store single as double lest it overflow */
		token->value.dbl = dvalue;
		if(trunc_check && sigfigs >= REAL_SIGFIGS) {
		  warning(token->line_num,token->col_num,
	"Single-precision real constant has more digits than are stored");
		}
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nReal const:\t\t%g",token->value.dbl);
#endif
		break;
	   case tok_dp_const:
		token->value.dbl = dvalue;
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nDouble const:\t\t%lg",token->value.dbl);
#endif
		break;
	}

} /* get_number */

     /* get_complex_constant reads an entity of the form (num,num)
      where num is any [signed] numeric constant.  It will only be
      called when looking_at() has guaranteed that there is one there.
      The token receives the real part as a number.  The imaginary part
      is not stored.  Whitespace is allowed between ( and num, around
      the comma, and between num and ) but not within num. */

PRIVATE void
get_complex_const(token)
	Token *token;
{
	Token imag_part;	/* temporary to hold imag part */
	double sign=1.0;
	int dble_size=FALSE;	/* flag to set if parts are D floats */
	int imag_dble_size=FALSE;/* if imaginary part D float */
	unsigned comma_line_num,comma_col_num;

	initial_flag = FALSE;

	bi_advance();		/* skip over the initial paren */

	if(curr_char == '+' || curr_char == '-') {
	  if(curr_char == '-') sign = -1.0;
	  bi_advance();
	}

#ifdef DEBUG_FORLEX
if(debug_lexer){
fprintf(list_fd,"\nComplex const:(");
if(sign < 0.0) fprintf(list_fd," -");
}
#endif
	get_number(token);
	switch(token->class) {
	   case tok_integer_const:
		token->value.dbl = sign*(double)token->value.integer;
		break;
	   case tok_dp_const:
		dble_size=TRUE;
			/*fallthru*/
	   case tok_real_const:
		token->value.dbl = sign*token->value.dbl;
		break;
	}

	while(iswhitespace(curr_char))
	  advance();


	comma_line_num = line_num;
	comma_col_num = col_num;
	bi_advance();		/* skip over the comma */

	if(curr_char == '+' || curr_char == '-') {
	     if(curr_char == '-') sign = -1.0;
	     bi_advance();
	}
#ifdef DEBUG_FORLEX
if(debug_lexer){
fprintf(list_fd,"\n,");
if(sign < 0.0) fprintf(list_fd," -");
}
#endif
	get_number(&imag_part);
	imag_dble_size = (imag_part.class == tok_dp_const);

	if(dble_size != imag_dble_size) {
	    warning(comma_line_num,comma_col_num,
		  "different precision in real and imaginary parts");
	}
	else if(f77_standard) {
	  if(dble_size)
	    warning(token->line_num,token->col_num,
		  "nonstandard double precision complex constant");
	}

	dble_size = (dble_size || imag_dble_size);

	while(iswhitespace(curr_char))
	   advance();

	advance();	/* skip over final paren */

	if(dble_size)
	  token->class = tok_dcomplex_const;
	else
	  token->class = tok_complex_const;
#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\n)");
#endif

}

#ifdef TYPELESS_CONSTANTS
		/* Routine to get constants of the forms:
			B'nnnn' 'nnnn'B  -- binary
			O'nnnn' 'nnnn'O  -- octal
		   	X'nnnn' Z'nnnn' 'nnnn'X 'nnnn'Z  -- hex
		   No check of whether digits are less than base.
		   Nonstandard warning is issued here since the constant
		   looks like a normal integer by the time the parser sees it.
		 */
PRIVATE void
get_binary_const(token,c,s)
     Token *token;
     int c;			/* base character: madeupper'ed by caller */
     char *s;			/* string of digits, or NULL */
{
  long value=0;
  int base;
  if(c == 'O')  base = 8;
  else if(c == 'X' || c == 'Z')  base = 16;
  else if(c == 'B') base = 2;
  else {
    syntax_error(token->line_num,token->col_num,
		 "Unknown base for typeless constant -- octal assumed");
    base = 8;
  }
	/* Two forms: X'nnnn' and 'nnnn'X. For the first, string has not
	   been scanned yet, and s is null.  For second, s=digit string. */
  if(s == NULL) {
    bi_advance();		/* gobble the leading quote */
    while(ishex(curr_char)){
      value = value*base + HEX(curr_char);
      bi_advance();
    }
    if(curr_char != '\'') {
      syntax_error(line_num,col_num, "Closing quote missing");
    }
    else
      advance();		/* gobble the trailing quote */
  }
  else {			/* Use the given string */
    while(*s != '\0') {
      if(!isspace(*s))		/* skip blanks */
	value = value*base + HEX(*s);
      s++;
    }
  }

  token->class = tok_integer_const;
  token->value.integer = value;

  if(f77_standard) {
    nonstandard(token->line_num,token->col_num);
  }

#ifdef DEBUG_FORLEX
if(debug_lexer)
fprintf(list_fd,"\nInteger const:\t\t%d",token->value.integer);
#endif

}/*get_binary_const*/

#endif/*TYPELESS_CONSTANTS*/


PRIVATE void
get_punctuation(token)
	Token *token;
{
	initial_flag = FALSE;
	closeup();
	if(curr_char == '*' && next_char == '*') {
		token->class = tok_power;
		advance();
	}
	else if(curr_char == '/' && next_char == '/' ) {
		token->class = tok_concat;
		advance();
	}
		/* paren can be the start of complex constant if everything
		   is just right. Maybe more tests needed here. */
	else if(complex_const_allowed && curr_char == '(' &&
	     (  (prev_token_class<256 && ispunct(prev_token_class))
	      || prev_token_class == tok_relop
	      || prev_token_class == tok_power )
	     && looking_at_cplx()) {
		get_complex_const(token);
		return;
	}
	else
		token->class = curr_char;

	advance();

#ifdef DEBUG_FORLEX
if(debug_lexer) {
	if(token->class == EOS)
		fprintf(list_fd,"\n\t\t\tEOS");
	else if(token->class == tok_power)
		fprintf(list_fd,"\nPunctuation:\t\t**");
	else if(token->class == tok_concat)
		fprintf(list_fd,"\nPunctuation:\t\t//");
	else
		fprintf(list_fd,"\nPunctuation:\t\t%c",token->class);
 }
#endif
} /* get_punctuation */


PRIVATE void
get_simple_punctuation(token)
	Token *token;
{
		/* Like get_punctuation but lacks special cases.  Just
		   gets the punctuation character.  */

	token->class = curr_char;
	advance();
#ifdef DEBUG_FORLEX
if(debug_lexer) {
	if(token->class == EOS)
		fprintf(list_fd,"\n\t\t\tEOS");
	else
		fprintf(list_fd,"\nPunctuation:\t\t%c",token->class);
}
#endif
} /* get_simple_punctuation */


PRIVATE void
get_string(token)       /* Gets string of form 'aaaa' */
	Token *token;
{
	int i,len,last_col_num;
	int first_char = curr_char;

/* String consts are not stored unless the macro name LEX_STORE_STRINGS
   is defined. */
#ifdef LEX_STORE_STRINGS
	char *s;
	char tmpstr[MAXSTR+1];
#else
	char *s = "Not stored";
#endif

	initial_flag = FALSE;
	inside_string = TRUE;
	last_col_num=col_num;
	advance();      /* Gobble leading quote */
	i = len = 0;
	for(;;) {
		while(curr_char == EOL) {
			/* Treat short line as if extended with blanks */
		    int col;
		    for(col=last_col_num; col<max_stmt_col; col++) {
#ifdef LEX_STORE_STRINGS
		      if(i < MAXSTR)
			tmpstr[i++] = ' ';
#endif
		      ++len;
		    }
		  last_col_num=col_num;
		  advance();
		}
		if(curr_char == EOS || curr_char == EOF) {
			yyerror("Closing quote missing from string");
			break;
		}
		if(curr_char == first_char) {
		  	inside_string = FALSE;/* assume so for now */
				    /* Handle possible continuation */
			if(next_char == EOL && col_num == max_stmt_col)
			  advance();

			last_col_num=col_num;
			advance();

			if(curr_char == first_char) { /* '' becomes ' in string */
				inside_string = TRUE; /* not a closing quote */
#ifdef LEX_STORE_STRINGS
				if(i < MAXSTR)
					tmpstr[i++] = curr_char;
#endif
				++len;
				last_col_num=col_num;
				advance();
			}
			else {
				break;  /* It was a closing quote after all */
			}
		}
		else {
#ifdef LEX_STORE_STRINGS
			if(i < MAXSTR)
				tmpstr[i++] = curr_char;
#endif
			++len;
			last_col_num=col_num;
			advance();
		}
	}
#ifdef LEX_STORE_STRINGS
	tmpstr[i++] = '\0';
#ifdef TYPELESS_CONSTANTS
				/* Watch for const like 'nnn'X */
	if(!inside_format) {
	  while(iswhitespace(curr_char))
	    advance();
	  if(isaletter(curr_char)) {
	    int c=makeupper(curr_char);
	    advance();		/* Gobble the base character */
	    get_binary_const(token,c,tmpstr);
	    return;
	  }
	}
#endif
	if( (s=(char *)ckalloc(i)) == (char *)NULL ) {
	  oops_message(OOPS_NONFATAL,line_num,col_num,
		       "Out of string space for character constant");
	}
	else {
		memset (s, 0, i);
		(void) strcpy(s,tmpstr);
	}
#endif
	if(len == 0) {
		warning(line_num,col_num,
			"Zero-length string not allowed\n");
		len = 1;
	}

	inside_string = FALSE;

	token->class = tok_string;
	token->value.string = s;
	token->size = len;
				/* Under -port warn if char size > 255 */
	if(port_check) {
	  if(len > 255)
	    nonportable(line_num,col_num,
			"character constant length exceeds 255");
	}

#ifdef DEBUG_FORLEX
	if(debug_lexer)
		fprintf(list_fd,"\nString:\t\t\t%s",s);
#endif
} /* get_string */


/* End of Forlex module */

/*
II. Advance
*/

/* advance.c:

	Low-level input routines for Fortran program checker.

	Shared functions defined:
		init_scan()	Initializes an input stream.
		finish_scan()	Finishes processing an input stream.
		advance()	Reads next char, removing comments and
				handling continuation lines.
			looking_at_x Handles lookahead up to end of line:
		looking_at_cplx() Identifies complex constant.
		looking_at_keywd() Identifies assgnmt stmts vs keywords.
		looking_at_relop() Distinguishes .EQ. from .Eexp .
		flush_line_out(n) Prints lines up to line n if not already
				printed, so error messages come out looking OK.
*/


	/* Define tab stops: nxttab[col_num] is column of next tab stop */

#define do8(X) X,X,X,X,X,X,X,X
PRIVATE int nxttab[]={ 0, do8(9), do8(17), do8(25), do8(33),
		do8(41), do8(49), do8(57), do8(65), do8(73), do8(81)};

PRIVATE int
	prev_comment_line,	/* True if previous line was comment */
	curr_comment_line,	/* True if current line is comment */
	noncomment_line_count,	/* Number of noncomment lines read so far */
	line_is_printed,	/* True if line has been flushed (printed) */
	prev_line_is_printed,	/* True if line has been flushed (printed) */
	sticky_EOF;		/* Signal to delay EOF a bit for sake
				   of error messages in include files. */
PRIVATE unsigned
	prev_line_num;		/* line number of previous input line */

unsigned prev_stmt_line_num;	/* line number of previous noncomment */

PRIVATE char
	lineA[MAXLINE+1],lineB[MAXLINE+1],  /* Buffers holding input lines */
	*prev_line,*line;		    /* Pointers to input buffers */

PRIVATE char
	*getstrn();

#ifdef UNIX_CPP
PRIVATE int
	take_cpp_line();	/* Reads #line directives and ignores others */
#endif

	/* Lookahead routines that scan the input
	   line for various things.  The is_whatever routines take a
	   string as argument and return TRUE if it satisfies the
	   criterion. The skip_whatever routines take an index and
	   string as argument and return the index of the next
	   nonspace character in the string after the expected thing,
	   which must be there in a syntactically correct program.
	   The given index points at the character after a known
	   lead-in (except for see_a_number, which can be given the
	   index of 1st char of number).  The see_whatever routines
	   are similar but return -1 if the expected thing is not
	   seen, which it need not be. */

PRIVATE int
	is_comment(), is_continuation();
#if 0
PRIVATE int, is_overlength();
#endif

PRIVATE int
	see_a_number(), see_dowhile(), see_expression(), see_keyword();

PRIVATE int
	skip_balanced_parens(), skip_idletters(), skip_quoted_string(),
	skip_hollerith();

#ifdef ALLOW_INCLUDE
/* Definition of structure for saving the input stream parameters while
   processing an include file.
*/

typedef struct {
  FILE     *input_fd;
  char	   *fname;
  char     line[MAXLINE];  /* MAXLINE is defined in ftnchek.h */
  int      curr_char;
  int      curr_index;
  int      next_char;
  int	   next_index;
  int	   col_num;
  int	   next_col_num;
  int	   line_is_printed;
  int	   do_list;
  unsigned line_num;
  unsigned next_line_num;
} IncludeFileStack;

PRIVATE IncludeFileStack include_stack[MAX_INCLUDE_DEPTH];
PRIVATE FILE* find_include(), *fopen_with_path();

#endif /*ALLOW_INCLUDE*/

PRIVATE void
	init_stream();
PRIVATE int
	push_include_file(),pop_include_file();

#ifdef ALLOW_INCLUDE		/* defns of include-file handlers */

PRIVATE int
push_include_file(fname,fd)
	char *fname;
	FILE *fd;
{
	 if (incdepth == MAX_INCLUDE_DEPTH) {
	   oops_message(OOPS_NONFATAL,line_num,NO_COL_NUM,
			"include files nested too deep");
	   return FALSE;
	 }

#ifdef DEBUG_INCLUDE
if(debug_include){
fprintf(list_fd,"\npush_include_file: curr_char=%c (%d)",curr_char,curr_char);
}
#endif

	 put_symbol (PAF_INCLUDE_DEF, NULL, fname, current_filename, line_num,
		     col_num, next_line_num, col_num+7, 0, NULL, NULL, NULL, NULL, line_num,
		     col_num, next_line_num, col_num+7);

	 include_stack[incdepth].input_fd = input_fd;
	 input_fd = fd;

	 include_stack[incdepth].fname = current_filename;
	 current_filename = fname;

	 strcpy(include_stack[incdepth].line,line);
	 include_stack[incdepth].curr_char = curr_char;
	 include_stack[incdepth].curr_index = curr_index;
	 include_stack[incdepth].next_char = next_char;
	 include_stack[incdepth].next_index = next_index;
	 include_stack[incdepth].col_num = col_num;
	 include_stack[incdepth].next_col_num = next_col_num;
	 include_stack[incdepth].line_is_printed = line_is_printed;
	 include_stack[incdepth].line_num = line_num;
	 include_stack[incdepth].next_line_num = next_line_num;
	 include_stack[incdepth].do_list = do_list;

	 incdepth++;

	 init_stream();

	 return TRUE;
}

PRIVATE int
pop_include_file()
{
#ifdef DEBUG_INCLUDE
if(debug_include){
fprintf(list_fd,"\npop_include_file: line %u = %s depth %d",line_num,line,
incdepth);
}
#endif

	 if (incdepth == 0) {	/* Stack empty: no include file to pop. */
	   return FALSE;
	 }
	 incdepth--;


	 if(do_list) {
	   flush_line_out(next_line_num);
	   fprintf(list_fd,"\nResuming file %s:",
		   include_stack[incdepth].fname);
	 }

	 fclose(input_fd);
	 input_fd = include_stack[incdepth].input_fd;

	 current_filename = include_stack[incdepth].fname;

	 strcpy(line,include_stack[incdepth].line);
	 curr_char = include_stack[incdepth].curr_char;
	 curr_index = include_stack[incdepth].curr_index;
	 next_char = include_stack[incdepth].next_char;
	 next_index = include_stack[incdepth].next_index;
	 col_num = include_stack[incdepth].col_num;
	 next_col_num = include_stack[incdepth].next_col_num;
	 line_is_printed = include_stack[incdepth].line_is_printed;
	 line_num = include_stack[incdepth].line_num;
	 next_line_num = include_stack[incdepth].next_line_num;
	 do_list = include_stack[incdepth].do_list;

	 curr_comment_line = FALSE;
	 prev_line_is_printed = TRUE;
	 initial_flag = TRUE;
	 sticky_EOF = TRUE;

	 return TRUE;
}


void
open_include_file(fname)
     char *fname;
{
  FILE *fd;
#ifdef VMS_INCLUDE
  int list_option=FALSE;	/* /[NO]LIST qualifier: default=NOLIST */
#endif /*VMS_INCLUDE*/

  if( highlight == -1 ) return;

#ifdef VMS_INCLUDE /* for VMS: default extension is .for */
  if(has_extension(fname,"/nolist")) {
    list_option = FALSE;
    fname[strlen(fname)-strlen("/nolist")] = '\0'; /* trim off qualifier */
  }
  else if(has_extension(fname,"/list")) {
    list_option = TRUE;
    fname[strlen(fname)-strlen("/list")] = '\0'; /* trim off qualifier */
  }
  fname = add_ext(fname, DEF_SRC_EXTENSION);
#endif

  if ((fd = find_include(&fname,"r")) == NULL) {
#if 0
    fprintf(list_fd,"\nerror opening include file %s\n",fname);
#endif
    return;
  }

			/* Print the INCLUDE line if do_list */
  if(do_list)
    flush_line_out(prev_line_num);

			/* Report inclusion of file */
  if(verbose || do_list)
    fprintf(list_fd,"\nIncluding file %s:",fname);

		/* Save the current input stream and then open
		   the include file as input stream. */
  if( push_include_file(fname,fd) ) {
#ifdef VMS_INCLUDE
	/* put /[NO]LIST option into effect */
      if(do_list != list_option)
	fprintf(list_fd," (listing %s)", list_option? "on":"off");
      do_list = list_option;
#endif /*VMS_INCLUDE*/
  }
  else
    fclose(fd);
}

PRIVATE FILE*
find_include(fname,mode)	/* looks for file locally or in include dir */
     char **fname,		/* If found, fname is returned with full path*/
     *mode;
{
  FILE *fp;
  char *env_include_var;
  IncludePathNode *p;
  extern char *group;

			/* Look first for bare filename */
  if( (fp=fopen(*fname,mode)) != NULL)
  {
#ifndef NO_DATABASE
	put_file(*fname,group,NULL);
#endif
	return fp;
  }

		      /* If not found, look in directories given
			 by include_path_list from -include options */

  for(p=include_path_list; p!=NULL; p=p->link) {
    if( (fp=fopen_with_path(p->include_path,fname,mode)) != (FILE *)NULL)
    {
	return fp;
    }
  }

		      /* If not found, look in directory given by
			 env variable ENV_INCLUDE_VAR (e.g. set by
			 % setenv INCLUDE ~/myinclude ) */

  if( (env_include_var=getenv(ENV_INCLUDE_VAR)) != NULL) {
    if( (fp=fopen_with_path(env_include_var,fname,mode)) != (FILE *)NULL)
      return fp;
  }

			/* Still not found: look in systemwide
			   default directory */

#ifdef DEFAULT_INCLUDE_DIR
  if( (fp=fopen_with_path(DEFAULT_INCLUDE_DIR,fname,mode)) != NULL)
    return fp;
#endif/* DEFAULT_INCLUDE_DIR */

				/* Not found anywhere: fail */
  return (FILE *)NULL;
}/*find_include*/

		/* Routine to open file with name given by include_path
		   followed by fname.  If successful, fname is replaced
		   by pointer to full name.  */
PRIVATE FILE *
fopen_with_path(include_path,fname,mode)
     char *include_path, **fname, *mode;
{
    extern char *group;
    FILE *fp;
    char tmpname[256];		/* holds name with path prepended */

    strcpy(tmpname,include_path);
				/* Add "/" or "\" if not provided */
    if(tmpname[strlen(tmpname)-1] != '/')
      strcat(tmpname,"/");
    strcat(tmpname,*fname);

    if( (fp=fopen(tmpname,mode)) != (FILE *)NULL) {
			/* Found: save new name in permanent space */
	*fname=ckalloc((strlen(tmpname)+1)*sizeof(char));
	strcpy(*fname,tmpname);

#ifndef NO_DATABASE
	put_file(*fname,group,NULL);
#endif
    }

    return fp;
}/*fopen_with_path*/

#else /* no ALLOW_INCLUDE */
				/* disabled forms of include handlers */
PRIVATE int
push_include_file(fname,fd)
	char *fname;
	FILE *fd;
{return FALSE;}

PRIVATE int
pop_include_file()
{return FALSE;}

void
open_include_file(fname)
     char *fname;
{}

#endif /*ALLOW_INCLUDE*/

void
init_scan()			/* Starts reading a file */
{
	tab_count = 0;
	incdepth = 0;

	line = lineA;		/* Start out reading into buffer A */
	prev_line = lineB;

	init_stream();
}

PRIVATE void
init_stream()		/* Initializes a new input stream */
{
	curr_comment_line = FALSE;
	inside_string = FALSE;
	inside_hollerith = FALSE;
	line_is_printed = TRUE;
	prev_line_is_printed = TRUE;
	noncomment_line_count = 0;

	next_index = -1;	/* Startup as if just read a blank line */
	next_char = EOS;
	curr_index = -1;
	curr_char = EOS;
	next_col_num = 0;
	next_line_num = 0;
	prev_line_num = prev_stmt_line_num = 0;
	sticky_EOF = TRUE;
	contin_count = 0;

	line[0] = '\0';
	advance();		/* put 1st two chars in the pipeline */
	advance();
	advance();		/* gobble the artificial initial EOS */
}


void
finish_scan()
{
		/* clean up if no END statement at EOF */
	check_seq_header((Token *)NULL);
		/* print last line if not already done */
	if(do_list)
	    flush_line_out(line_num);
}

#ifdef INLINE_COMMENT_CHAR
	/* macro is used on next_char: must look at curr_char to avoid
	   being fooled by '!' without messing up on 'xxx'! either.
	   Also don't be fooled by '''!''' which is the string '!'
	   Note that inside_string does not yet reflect curr_char.
	   Test is that inside_string is true but about to become false,
	   or false and not about to become true. Think about it. */

#define inline_comment(c) ( ((c)==INLINE_COMMENT_CHAR) &&\
		(inside_string == (curr_char == '\'')) && (!inside_hollerith) )
#endif

	/* closeup: Advances input stream till next_char is nonspace.  Fudges
	   things so that curr_char remains as it was. */
PRIVATE void
closeup()
{
  int
    save_curr_char = curr_char,
    save_prev_char = prev_char,
    save_line_num = line_num,
    save_col_num = col_num,
    save_curr_index = curr_index;

  while(iswhitespace(next_char))
    advance();

  curr_char = save_curr_char;
  prev_char = save_prev_char;
  line_num = save_line_num;
  col_num = save_col_num;
  curr_index = save_curr_index;
}

LEX_SHARED void
advance()
{
#ifdef EOLSKIP
    int eol_skip = FALSE;
#endif
    prev_char = curr_char;
#ifdef EOLSKIP
    do{
#endif
	while(next_char == EOF) {	  /* Stick at EOF */
		if(curr_char == EOS || curr_char == EOF) {

			 /* Pause to allow parse actions at end of stmt
			    to have correct file context before popping
			    the include file.  Effect is to send an extra
			    EOS to parser at end of file. */
		  if(sticky_EOF) {
		    sticky_EOF = FALSE;
		    return;
		  }
				/* At EOF: close include file if any,
				   otherwise yield an EOF character. */
		  if( ! pop_include_file() ) {
		    curr_char = EOF;
		  }
		  return;
		}
		else {
		  curr_char = EOS;
		  return;
		}
	}

	if(curr_char == EOS)
		initial_flag = TRUE;

#ifdef EOLSKIP
	if(! eol_skip) {
#endif
	    curr_char = next_char;	  /* Step to next char of input */
	    curr_index = next_index;
	    col_num = next_col_num;
	    line_num = next_line_num;

#ifdef EOLSKIP
	}
#endif

	if(next_char == '\t'){	   /* Handle tabs in input */

		next_col_num = nxttab[next_col_num];

		if( ! (inside_string || inside_hollerith) )
		    tab_count++;	/*  for portability warning */
	}
	else {
		next_col_num++;
	}

	next_char = line[++next_index];

			/* If end of line is reached, input a new line.
			 */
	while(next_col_num > max_stmt_col || next_char == '\0'
#ifdef INLINE_COMMENT_CHAR
	|| inline_comment(next_char)
#endif
	){
		do {
			if(( next_col_num > max_stmt_col && next_char != '\0' )
#ifdef INLINE_COMMENT_CHAR
				|| inline_comment(next_char)
#endif
				|| curr_comment_line
			)
			{
			  char c = makeupper(line[curr_index]);
			  if (c == 'C' || c == '*' || (c && line[curr_index + 1] == '!'))
			  {
				int i;
				int index = curr_index;
				char acComment[1000];
				char *comment;
				char *classn;
				char *function;
				extern int current_struct_hash;
				extern int current_module_hash;
				extern	int	comment_database;

			  	if( hig_fp && incdepth == 0)
			  	{
 					fprintf( hig_fp, "%d rem %d.%d %d.%dlineend\n"
						   , PAF_HIGH
						   , next_line_num
						   , next_index - 1
						   , next_line_num
						   , next_index - 1
						   ); 
				}

				if (comment_database)
				{
					for (i = 0, comment = acComment;;)
					{
						c = line[index++];
						if( c == EOL || c == 0 )
							break;
						acComment[i++] = c;
					}
					acComment[i] = 0;

					if (*comment)
						comment++;
					if (*comment == '!')
						comment++;

					if( current_struct_hash != -1 )
					{
						classn = hashtab[current_struct_hash].name;
					}
					else
						classn = NULL;
					if( current_module_hash != -1 && incdepth == 0)
					{
						function = hashtab[current_module_hash].name;
					}
					else
						function = NULL;

					save_comment(current_filename, function, classn,
						next_line_num, next_index,comment);
				}
			  }
			}

			if(do_list) /* print prev line if not printed yet */
			  flush_line_out(prev_line_num);

#ifdef INLINE_COMMENT_CHAR
			if( f77_standard ) {
			  if( !curr_comment_line && inline_comment(next_char)){
			      nonstandard(next_line_num,next_col_num);
			      msg_tail(": inline comment");
			  }
		        }
#endif

			    /* Swap input buffers to get ready for new line.
			       But throw away comment lines if do_list is
			       false, so error messages will work right.
			     */
			if(do_list || ! curr_comment_line) {
			    char *temp=line;
			    line = prev_line;
			    prev_line=temp;
			    if(! curr_comment_line)
			      prev_stmt_line_num = line_num;
			    prev_line_num = next_line_num;
			    prev_line_is_printed = line_is_printed;
			}

			++next_line_num;
			next_col_num = 1;		/* rigo */
			next_index   = 1;		/* rigo */
			curr_index   = 0;		/* rigo */
			line_is_printed = FALSE;
			if( getstrn(line,MAXLINE+1,input_fd) == NULL ) {
				next_char = EOF;
				line_is_printed = TRUE;
				return;
			}
#ifdef UNIX_CPP
			else
			  if(line[0] == '#')
			    cpp_handled = take_cpp_line(line);
#endif

			/*  Keep track of prior-comment-line situation */
			prev_comment_line = curr_comment_line;

		} while( (curr_comment_line = is_comment(line)) != FALSE);
		++noncomment_line_count;

			/* Handle continuation lines */
		if( (next_index = is_continuation(line)) != 0) {
				/* It is a continuation */
		    if( hig_fp )
		    {
		  	  if( strcmp( current_filename, top_filename ) == 0 )
			  {
 				  fprintf( hig_fp, "%d cont %d.%d %d.%d\n"
					     , PAF_HIGH
					     , next_line_num
					     , next_index
					     , next_line_num
					     , next_index+1
					     ); 
			  }
		    }
#ifdef EOLSKIP
		    if(eol_is_space) {
#endif
			next_char = EOL;
			next_col_num = 6;
#ifdef EOLSKIP
		    }
		    else {
			next_char = line[++next_index];
			next_col_num = 7;
			eol_skip = TRUE; /* skip continued leading space */
		    }
#endif
				/* Issue warnings if contin in funny places */
			if(noncomment_line_count == 1)
			    warning(next_line_num,(unsigned)6,
		    "Continuation mark found in first statement of file");
			if( pretty_flag && prev_comment_line )
			    ugly_code(next_line_num,(unsigned)6,
		    "Continuation follows comment or blank line");
		        if(contin_count++ == 19)
			  if(f77_standard) {
			    nonstandard(next_line_num,(unsigned)6);
			    msg_tail(": > 19 continuation lines");
			  }
		}
		else {
				/* It is not a continuation */
		    next_char = EOS;
		    next_col_num = 0;
		    next_index = -1;
		    contin_count = 0;
		}
	}/*end while( end of line reached )*/

		/* Avoid letting a '0' in column 6 become a token */
	if(next_col_num == 6 && next_char == '0')
		next_char = ' ';

#ifdef EOLSKIP
			/* elide EOL and following space of continued
			   stmts if requested */
	eol_skip = (eol_skip && isspace(next_char));
   }while(eol_skip);/*end do*/
#endif

}/* end advance */


	/*  Function which returns 0 if line is not a comment, 1 if it is.
	 *  Comment is ANSI standard: C or c or * in column 1, or blank line.
	 */

PRIVATE int
is_comment(s)
	char s[];
{
	int i,c= makeupper(s[0]);
	unsigned col;
	if( c == 'C' || c == '*' )
		return TRUE;

	for(i=0,col=1; s[i] != '\0'; i++)
		if( !isspace(s[i]))
#ifdef INLINE_COMMENT_CHAR
		/* Initial "!" starts a comment, except in col. 6 it
		   must be taken as continuation mark */
			 if(s[i]==INLINE_COMMENT_CHAR && col != 6) {
			     if(f77_standard) {
				 nonstandard(next_line_num,col);
				 msg_tail(": inline comment");
			     }
			     return TRUE;
			  }
			  else
			      return FALSE;
		else
			  if(s[i] == '\t') col = nxttab[col];
			  else		   col++;
#else
			return FALSE;
#endif
	return TRUE;		/* blank line */
}

	/* Here we handle Unix preprocessor lines.  The only ones
	   processed now are those that set the line number and filename.
	     Form 1: # line 10 "filename"
	     Form 2: # 10 "filename"
	   We replace next_filename and next_line_num by the
	   given values.
	 */
#ifdef UNIX_CPP
PRIVATE int
take_cpp_line(s)
     char *s;
{
  int linenum, nchars, handled;
  char *filename;

  handled=FALSE;

  do { ++s; } while( isspace(*s) );	/* Skip space after the '#' */

  if(strncmp(s,"line",4) == 0) {	/* Look for the keyword "line" */
    s += 4;			/* Skip the word "line" */
    while( isspace(*s) ) ++s;	/* Skip space after the word "line" */
  }

  if( isdigit(*s) ) {		/* See that we are now looking at a number */
    handled = TRUE;

			/* Get the line number */
    linenum=0;
    while( isdigit(*s) )
      linenum = linenum*10 + BCD(*s++);

			/* Now find the filename */

    filename = (char *)NULL;
    while( isspace(*s) ) ++s;	/* Skip space after the line number */

    if( *s == '"') {		/* Filename must be preceded by " */

      ++s;			/* Skip the " */

      nchars = 0;		/* Count chars in the filename */
      while( s[nchars] != '"' && s[nchars] != '\0')
	++nchars;

      if( s[nchars] == '"') {	/* Filename must be followed by " */

	if( (filename=(char *)ckalloc(nchars)) == (char *)NULL) {
	  oops_message(OOPS_NONFATAL,prev_line_num,(unsigned)1,
		       "cannot alloc space for filename");
	}
	else {
	  strncpy(filename,s,nchars);
	}
      }
    }
  }/*end handling #line */

  if(handled) {
    next_line_num = linenum-1;
    next_filename = filename;
  }

  return handled;		/* Return TRUE if it was a #line category */

}/*take_cpp_line*/
#endif

	/*  Function which returns 0 if line is a not continuation line.
	 *  If line is a continuation, returns index in line of
	 *  the continuation mark.  VMS_TAB adapted from code contributed
	 *  by roberson@zeno.ibd.nrc.ca (Walter Roberson).
	 */
PRIVATE int
is_continuation(s)
	char s[];
{
	int col,i,c;
				/* skip to col 6 */
	for(i=0,col=1; col < 6 && s[i] != '\0'; i++) {
		if(s[i] == '\t')
			col = nxttab[col];
		else
			col++;
	}
	c = s[i];
#ifdef VMS_TAB  /* <tab><digit> is a continuation card */
	if ( col > 6 && c != '0' && isdigit(c) ) {
	  if(f77_standard) {
	    nonstandard(next_line_num,col);
	    msg_tail(": continuation mark not in column 6");
	  }
	  return i;
	} else
#endif
	if ( col == 6 && c != '\0' && !isspace(c) && c != '0'
#ifdef UNIX_CPP
				/* Veto if it is a preprocessor line */
	    && s[0] != '#'
#endif
	    )
		return i;
	else
		return 0;

}

int
flush_line_out(n)	/* Prints lines up to line #n if not yet printed */
    unsigned n;		/* Returns TRUE if line was printed, else FALSE */
{
			/* Print previous line only if do_list TRUE */
	if( !prev_line_is_printed
	 && ((n == prev_line_num) || (n > prev_line_num && do_list)) ) {
	   print_a_line(list_fd,prev_line,prev_line_num);
	   prev_line_is_printed = TRUE;
	}
	if(n >= next_line_num && !line_is_printed) {
	   print_a_line(list_fd,line,next_line_num);
	   line_is_printed = TRUE;
	}
    return ( do_list ||
	     (prev_line_is_printed && n == prev_line_num) ||
      	     (line_is_printed && n == next_line_num) );
}


	/*  Function to read n-1 characters, or up to newline, whichever
	 *  comes first.  Differs from fgets in that the newline is replaced
	 *  by null, and characters up to newline (if any) past the n-1st
	 *  are read and thrown away.
	 *  Returns NULL when end-of-file or error is encountered.
	 */

static char *
getstrn(char *s, int n, FILE * fd)
{
  static Tcl_Encoding ascii = NULL;

  int c, i;
  unsigned char ch;
  char * ptr = NULL;
  Tcl_DString external, intermediate, internal;

  Tcl_DStringInit(&external);
  Tcl_DStringInit(&intermediate);
  Tcl_DStringInit(&internal);
  
  /* Do lookahead translation. Instead of doing getc(), we'll get it
     from our translated buffer instead. */
  
  /* Read another line. */
  while ((c = getc(fd)) != '\n')
    {
      if (c == EOF)
	{
	  /* FIXME: I think this is a bit incorrect, but I don't want
	     to change this function's behaviour. */
	  return NULL;
	}
      ch = (char) c;
      Tcl_DStringAppend(&external, &ch, sizeof(unsigned char));
    }
  
  /* Translate, but only if necessary. */
  if (encoding != NULL)
    {
      /* Load the `ascii' encoding if we haven't already. */
      if (ascii == NULL)
	{
	  ascii = Tcl_GetEncoding(NULL, "ascii");
	  if (ascii == NULL)
	    {
	      printf("Unable to locate ASCII encoding\n");
	      return NULL;
	    }
	}
      
      Tcl_ExternalToUtfDString(encoding,
			       Tcl_DStringValue(&external),
			       Tcl_DStringLength(&external),
			       &intermediate);
      Tcl_UtfToExternalDString(ascii,
			       Tcl_DStringValue(&intermediate),
			       Tcl_DStringLength(&intermediate),
			       &internal);
      
      ptr = Tcl_DStringValue(&internal);
    }
  else
    {
      /* No translation required. */
      ptr = Tcl_DStringValue(&external);
    }
  
  for (i = 0; *ptr != '\0' && i < n; i++, ptr++)
    {
      /* Truncate lines to max_line_width. */
      if (max_line_width > 0 && i >= max_line_width)
	{
	  break;
	}
      s[i] = *ptr;
    }
  s[i] = '\0';

  /* Clean up. */
  Tcl_DStringFree(&internal);
  Tcl_DStringFree(&intermediate);
  Tcl_DStringFree(&external);
  
  return s;
}


	/* Functions which look ahead as far as end of line to see if input
	   cursor is sitting at start of a token of the given class.  Used
	   to resolve ambiguities that need more than one token of lookahead.
	   */

LEX_SHARED int
looking_at_cplx()
{
    int indx;

    if( next_char != EOS )	/* Looking at next line already */
    {
	indx = next_index;

	if( (indx = see_a_number(indx,line,FALSE)) < 0 )
	  return FALSE;
	while(line[indx] != '\0' && isspace(line[indx]))
	  indx++;

	if( line[indx] != ',' )
	  return FALSE;
	++indx;

	if( (indx = see_a_number(indx,line,FALSE)) < 0 )
	  return FALSE;
	while(line[indx] != '\0' && isspace(line[indx]))
	  indx++;

	if(line[indx] != ')')
	  return FALSE;
    }

    return TRUE;	/* passed all the tests */

}

LEX_SHARED int
looking_at_keywd(token_class)
	int token_class;	/* Keyword class to be checked out */
{
				/* Distinguishing identifier from keyword.
				   If not sure, assumes true.   Ambiguity
				   must be resolved in current line. */
    int indx;
    int c;

    if( next_char != EOS )	/* Looking at next line already */
    {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd,"\nlooking_at: curr_char=%c then %c",
curr_char,line[next_index]);
#endif
				/* Skip over leading
				   stuff that could be rest of identifier */

	if(isidletter(curr_char) || isdigit(curr_char) ||
	   isspace(curr_char)){
	  indx = skip_idletters(next_index,line);
	  c = line[indx];	/* Store following character in c */
	  ++indx;   /* Leave index pointing at char after c */
	}
	else {
	  c = curr_char;	/* Otherwise next input char is c */
	  indx = next_index;
	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd," c=%c then %c",c,line[indx]);
#endif

	if(token_class == tok_DO) {
	  int opt_comma = FALSE;

		/* DO: we must by now have skipped over optional label
		  to optional comma or over optional label and
		  variable name to = sign.  Look for expression and comma.
		  DOWHILE will be found as single keyword, but we have
		  to spot DO label WHILE(expr) here.  DO of END DO
		  is not seen here. */

	  WHILE_expected = FALSE; /* most cases do not use it */

	  if(c == ',' && isdigit(curr_char)) {
				/* Skip optional comma after label.
				   First, back up and check that we saw
				   only digits so far. Do it here since
				   this is rare and not worth cluttering
				   the foregoing code. */
	    int i=next_index;
	    while(isdigit(line[i]) || isspace(line[i]))
	      ++i;
	    if(line[i] != ',')
	      return FALSE;
				/* Checks out OK: */
	    indx = skip_idletters(indx,line);	/* skip DO index or WHILE */
	    c = line[indx];
	    ++indx;
	    opt_comma = TRUE;
	  }

	  if(c == '=') {	/* Traditional DO form */
	    indx = see_expression(indx,line);
	    return (indx != -1 && line[indx] == ',') || opt_comma;
	  }
	  else {		/* Nonstandard variants */
	    if(c == '(') {
				/* DO label WHILE (expr): rescan from the
				   word DO to see if it fits. */
	      if( see_dowhile(next_index,line) != -1 )
		WHILE_expected = TRUE;
	      return WHILE_expected || opt_comma;
	    }
	    else
	      return opt_comma;	/* The comma is found only in DO forms */
	  }
	}/* end of tok_DO forms */

		/* Otherwise, look for an assignment statement.  If there
		   is no left paren, then must be an equals sign here
		   if it is an assignment statement. */
	if(c != '(') {
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd,"\n Conclude %s",
	(c != '=')? "keyword": "assignment stmt");
#endif
	      return (c != '=');
	}

	else {			/* sitting at parenthesis */

		/* Skip to end of balancing parenthesis. Then if = sign, it
		   must be an assignment statement.  If ( is found,
		   presumably it is an array substring assignment. So skip
		   once more to check for the = sign.) */


	indx = skip_balanced_parens(indx,line);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd," to %c",line[indx]);
#endif

	if(line[indx] == '(') {
	  ++indx;		/* Move past the paren */
	  indx = skip_balanced_parens(indx,line);

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd," to %c",line[indx]);
#endif

	}

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd," conclude %s",line[indx]!= '='?"keyword":"variable");
#endif

	return (line[indx] != '=');
      }
    }
				/* End of line: must be a keyword */
    return TRUE;

}/*looking_at_keywd*/

		/* This guy is called when an integer is followed by '.'
		   in cases where a real number or expression is allowed.
		   When an integer is followed by .E, it can either be a real
		   like 1.E10, or a comparison like (1.EQ.I).  This requires
		   looking for the 'Q' after the 'E'.  The other cases,
		   like ... 1.AND. ... are resolved by looking at next_char
		   to see if it is the 'D' of a d.p. constant or not.
		  */
LEX_SHARED int
looking_at_relop()
{
    int indx;
    int c;


    if( next_char != EOS )	/* Looking at next line already */
    {

#if 0				/* With closeup() this is no longer valid */
    if( eol_is_space && line_num != next_line_num )
	return FALSE;	/* Looking at next line already */
#endif
        indx = next_index;/* Start at next_char */

	while( (c=line[indx]) != '\0' && isspace(c))
	  ++indx;

	if( !isaletter( c ) )	/* next char must be letter */
		return FALSE;
	c = makeupper(c);
	if( c == 'D' )	/* D.P. exponent */
	  return FALSE;	/* No dotted keywords start with D */

			/* If next char is any other letter but 'E', cannot be
			    exponent.  If it is 'E', must be EQ or EQV to
			    be a relop.  So look ahead for the 'Q'. */
	else if( c == 'E' ) {
	  do {
	    ++indx;
	  } while( (c=line[indx]) != '\0' && isspace(c));

	  c = makeupper(c);
	  return (c == 'Q');
	}
        else		/* Next char not D or E: must be a dotted keyword */
	  return TRUE;
    }
				/* If EOS, then it is stmt like x=1. */
    return FALSE;

}

	/* see_a_number returns -1 if there is no valid numeric constant
	   in string s starting at index i.  If valid number found, it
	   returns the index of the next character after the constant.
	   Leading whitespace in s is skipped.*/


#define SKIP_SPACE    while(s[i] != '\0' && isspace(s[i])) i++

PRIVATE int
see_a_number(i,s,can_be_holl)
   int i;
   char s[];
   int can_be_holl;/* context indication */
{
   int digit_seen = FALSE;
   int isave;
   while(s[i] != '\0' && isspace(s[i]))
     i++;

			/* move past optional preceding sign */
   if(s[i] == '-' || s[i] == '+' ) {
     i++;
     SKIP_SPACE;
     can_be_holl = FALSE;
   }
   isave=i;

		/* move past ddd or ddd. or .ddd or ddd.ddd */
   if(isdigit(s[i]))
     digit_seen = TRUE;
   while(isdigit(s[i])) {
     i++;
     SKIP_SPACE;
   }
   if(s[i] == 'H' && can_be_holl) {
     return skip_hollerith(isave,s);
   }
   if(s[i] == '.') {
     i++;
     SKIP_SPACE;
     if(isdigit(s[i]))
       digit_seen = TRUE;
     while(isdigit(s[i])) {
       i++;
       SKIP_SPACE;
     }
   }

		/* no digits seen: bail out now */
   if(! digit_seen)
     return -1;

		/* look for exponential part.  The standard does not
		   allow D, but we will, just in case. */
   if(makeupper(s[i]) == 'E' || makeupper(s[i]) == 'D') {
     i++;
     SKIP_SPACE;
     if(s[i] == '+' || s[i] == '-') {
       i++;
       SKIP_SPACE;
     }
     if(!isdigit(s[i]))
       return -1;
     while(isdigit(s[i]) || isspace(s[i]))
       i++;
   }

   return i;
}/*see_a_number*/

	/* see_dowhile returns TRUE only if the stuff following the initial
	   DO is a label and the word WHILE followed by a parenthesized expr.
	   If not resolved on current line, assumes TRUE (how many arrays
	   are named DO10WHILE?).  The "DO WHILE" form is not handled
	   here so that DOWHILE will be gotten as a single token later.
	 */
PRIVATE int
see_dowhile(indx,ll)
     int indx;
     char ll[];
{
    int c;
				/* Skip over the label */
    while(isdigit(c=ll[indx]) || isspace(c) )
      ++indx;

    if(c == ',')		/* Skip optional comma */
      ++indx;

    indx = see_keyword(indx,ll,"WHILE");

    if( indx == -1 || ll[indx] != '(')  /* Look for the opening paren */
      return -1;

    ++indx;			/* skip the opening paren */
    indx = skip_balanced_parens(indx,ll);
				/* Now we should be at end of statement */
    return (ll[indx] == '\0')? indx: -1;
}/*see_dowhile*/


	/* Crude routine to scan forward past arithmetic expressions.
	   Function invocations and array or character elements will
	   have their parentheses skipped by skip_balanced_parens;
	   outside parens a comma will cause a halt.  Returns the index
	   of the nonblank character following the expression, or
	   -1 if something non-kosher was found (e.g. a faulty number)
	   It can be confused by holleriths containing significant
	   characters, i.e. ( ) ' !  and occurring outside parentheses.
	 */
PRIVATE int
see_expression(indx,ll)
     int indx;
     char ll[];
{
    int c;
    while(indx != -1 && (c=ll[indx]) != '=' && c != '\0') {
        if(isidletter(c))
	  indx = skip_idletters(indx,ll);
	else if(isdigit(c))
	  indx = see_a_number(indx,ll,TRUE);
	else if(isspace(c))
	  ++indx;
	else if(c == '(')
	  indx = skip_balanced_parens(indx+1,ll);
	else if(c == '+' || c == '-' || c == '/' || c == '*' || c == '.')
	  ++indx;
	else if(c == '\'')	/* embedded strings confuse things */
	  indx = skip_quoted_string(indx+1,ll);
	else break;
    }
    return indx;
}/*see_expression*/

	/* see_keyword returns -1 if the line (ignoring blanks and
	   uppercasing alphabetics) does not match the given string
	   matchstr.  If it does match, returns index of next nonspace
	   character. Note that index must be at start of keyword. */

PRIVATE int
see_keyword(indx,ll,matchstr)
     int indx;
     char ll[];
     char *matchstr;
{
    int c;
    while(*matchstr != 0 && (c=ll[indx]) != '\0') {
      if(! isspace(c) ) {
	if(makeupper(c) != *matchstr++)
	  return -1;
      }
      ++indx;
    }
    if(*matchstr == '\0') {	/* Match found */
      while(isspace(ll[indx]))
	++indx;
      return indx;
    }
    else			/* No match */
      return -1;
}/*see_keyword*/

		/* skip_balanced_parens returns index of the nonspace character
		   following the closing ')' that balances the opening
		   '(' preceding ll[indx], or of final nul if the
		   parentheses are not balanced within the line.
		*/
PRIVATE int
skip_balanced_parens(indx,ll)
     int indx;
     char ll[];
{
  int depth=1;		/* nesting depth in parens */
  int prevchar = '+';	/* arbitrary punctuation */
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd,"\nskipping ()...");
#endif

  while(ll[indx] != '\0' && depth > 0) {
#ifdef INLINE_COMMENT_CHAR
    if(ll[indx] == INLINE_COMMENT_CHAR) /* inline comment ends line */
      break;
#endif
    if(ll[indx] == '\'') {	/* embedded strings confuse things */
      indx = skip_quoted_string(indx+1,ll);
      prevchar = 'X';	/* Arbitrary non punctuation */
    }
    else if(ispunct(prevchar) && isdigit(ll[indx])) {
      indx = skip_hollerith(indx,ll); /* Skip hollerith or number */
      prevchar = ll[indx];
    }
    else {
				/* Keep track of nesting */
      if     (ll[indx] == '(') ++depth;
      else if(ll[indx] == ')') --depth;

      if(! isspace(ll[indx]) )
	prevchar = ll[indx];

      ++indx;
    }
  }
				/* We are now past the closing paren */
  while(ll[indx] != '\0' && isspace(ll[indx]))
    indx++;		/* skip trailing space */

  return indx;
}/*skip_balanced_parens*/


		/* skip_idletters returns index of the nonspace character
		   following a string of idletters: alphabetic characters
		   or digits, or underscore or dollar if those options are
		   enabled.  It does not look out for hollerith constants.
		*/
PRIVATE int
skip_idletters(indx,ll)
     int indx;
     char ll[];
{
	int c;
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
fprintf(list_fd,": skipping letters...");
#endif
	while(isidletter(c=ll[indx])
	      || isadigit(c) || isspace(c))
	  ++indx;
	return indx;
}/*skip_idletters*/

		/* Returns index of nonspace character following
		   quote mark that closes string whose opening quote
		   mark is before index. */
PRIVATE int
skip_quoted_string(indx,ll)
     int indx;
     char ll[];
{
  while(ll[indx] != '\0') {
    if(ll[indx] == '\'') {	/* Closing quote? */
      if(ll[++indx] != '\'') /* Quoted quote? */
	break;
    }
    ++indx;
  }

				/* We are now past the closing quote mark */
  while(ll[indx] != '\0' && isspace(ll[indx]))
    indx++;		/* skip trailing space */

  return indx;
}/*skip_quoted_string*/


			/* Skips holleriths.  Note: treats tabs within
			   hollerith as single characters. */
PRIVATE int
skip_hollerith(i,s)
   int i;
   char s[];
{
  int len=0;
  while(isdigit(s[i])) {
    len = len*10 + BCD(s[i]);
    i++;
    SKIP_SPACE;
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  fprintf(list_fd,"\nskip_hollerith: %d then %c:",
len,s[i]);
#endif
  if(makeupper(s[i]) != 'H')
    return i;

  i++;				/* Skip the 'H' */

  while(s[i] != '\0' && len > 0){ /* Move forward len characters */

#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  fprintf(list_fd,"%c",s[i]);
#endif
    --len; i++;
  }
#ifdef DEBUG_IS_KEYWORD
if(debug_lexer && getenv("VERBOSE"))
  fprintf(list_fd," to %c",s[i]);
#endif
  return i;
}/*skip_hollerith*/


#if 0
PRIVATE int
is_overlength(s,maxcol)	/* checks line for having nonblanks past col 72 */
	char *s;		/* The line to check */
        int maxcol;		/* Max columns to check to */
{
	int i,col;
	for(col=1,i=0; col<=maxcol && s[i] != '\0'; i++) {

	    if(col > 72 && !isspace(s[i]))
		return TRUE;

			/* Count columns taking tabs into consideration */
	    if(s[i] == '\t')
		col = nxttab[col];
	    else
		++col;
	}
	return FALSE;
}/*is_overlength*/
#endif

/* End of module Advance */

struct SaveComment_s
{
	int line_num;
	int	col_num;
	char filename[256];
	char acComment[300];
	char	class_name[100];
	char	func_name[100];
};

static struct SaveComment_s aSaveComment[20];
static int iSaveComment;

#define	COMMENT_DEBUG 0

static void save_comment(char *filename, char *func, char *classn, int line_num, int col_num, char *acComment)
{
	struct	SaveComment_s	*p;
#if COMMENT_DEBUG
	printf("Save comment: %s,%d,<%s>\n",filename,line_num,acComment);
#endif /* COMMENT_DEBUG */
	p = &aSaveComment[iSaveComment];
	if (p->line_num)
	{
		put_comment(p->class_name,p->func_name,p->filename,p->acComment,
			p->line_num,p->col_num);
	}
	strcpy(p->filename,filename);
	strcpy(p->class_name,classn ? classn : "");
	strcpy(p->func_name,func ? func : "");
	p->line_num = line_num;
	p->col_num = col_num;
	strncpy(aSaveComment[iSaveComment].acComment, acComment,
		sizeof(aSaveComment[0].acComment) -1);

	iSaveComment++;

	if(iSaveComment == 20)
		iSaveComment = 0;
}

char *
get_comment(char *filename, int line_num )
{
	int i;
	struct	SaveComment_s	*p;

#if COMMENT_DEBUG
	printf( "get comment: %s %d ", filename, line_num );
#endif

	for(i = 0; i < 20; i++)
	{
		p = &aSaveComment[i];
#if COMMENT_DEBUG2
		printf("    comment: %s %d\n"
	   		, p->filename
			, p->line_num);
#endif
		if(p->line_num == line_num && strcmp(p->filename,filename) == 0)
		{
#if COMMENT_DEBUG
			printf( "<%s>\n",p->acComment);
#endif
			p->line_num = 0;
			p->filename[0] = '\0';

			return p->acComment;
		}
	}

#if COMMENT_DEBUG
	printf( "Not found\n" );
#endif
	return NULL;
}

void
empty_comments()
{
	struct	SaveComment_s	*p;
	int	i;
	int	last;
	int	beg;

	if (iSaveComment == 19)
	{
		last = 19;
		beg = 0;
	}
	else
	{
		last = iSaveComment;
		beg = iSaveComment + 1;
	}

	for (i = beg; i != last; i++)
	{
		p = &aSaveComment[i];
		if (p->line_num)
		{
			put_comment(p->class_name,p->func_name,p->filename,p->acComment,
				p->line_num,p->col_num);

			p->acComment[0] = '\0';
			p->line_num = 0;
		}

		if (i == 19)
			i = -1;
	}
}

