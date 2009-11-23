/*

Copyright (c) 2000, 2001, Red Hat, Inc.

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
 * m4browser.l
 *
 * Copyright (C) 2000 Red Hat Inc.
 *
 * Description:
 * Lex input file for an M4 macro language processor.
 */

%{

#include <ctype.h>
#include <stdio.h>
#include "snptools.h"
#include "lexinput.h"

#undef yywrap
#define YY_SKIP_YYWRAP

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

static char group[] = "m4";

static char current_function[512] = {"GLOBAL"}; /* "" causes assert core dump */
static int result;

/*
 * Flush any function definitions that might be outstanding (ie. if its
 * label appears _last_ in a file.  When we reach EOF, check to see if its
 * defn needs to be flushed to the database.  Normally the occurence of
 * another label causes the defn to be stored.
 * 
 * Return 1 so flex will keep playing.
 */

int
yywrap()
{
  return(1);
}

%}

%x COMMENT

ws		[ \t]
symbol		[a-zA-Z0-9_]+
quoted-symbol	\[?{symbol}\]?
func-decl	("AU_DEFUN"|"AC_DEFUN"|"m4_define")

%%

.		{ sn_advance_column(yyleng); /* eat text */ }

\\\$		{ sn_advance_column(yyleng); /* ignore \$ */ }


\n		{ sn_advance_line(); sn_reset_column(); }

{ws}*(#|dnl).*\n {
  sn_advance_line();
  sn_reset_column();
}

\${symbol} {
  char * x = (char *) yytext;

  /* Trim leading $ off the front of the symbol */

  assert(*x == '$');
  x++;

  /* When reading a global variable, one simply needs to add an xref
     for the symbol in question. */

  result = sn_insert_xref(SN_REF_TO_GLOB_VAR,
                 SN_REF_TO_GLOB_VAR, /* PASSING 0 causes function to fail, what is the global var setting ??? */
		                     /* This SN_REF_TO_GLOB_VAR thing does not jive with the docs */
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 "GLOBAL", /* FIXME: A global var would not have a current function ??? */
                 NULL,
                 NULL,
                 x, /* refsymbol */
                 NULL,
                 sn_current_file(),
                 sn_line(),
                 SN_REF_READ);

  assert(result == 0);
  sn_advance_column(yyleng);
}

{symbol}= {
  char * x = (char *) yytext;
  char * y;
  
  /* Trim the '=' off the end of the string */
  
  for (y=x; *y ; y++) {
      if (*y == '=') {
          *y = 0;
	  break;
      }
  }

  /* Setting a global variable should emit
     a symbol declaration. I am not really
     sure if we should emit just one symbol
     declaration, or multiple ones. Multiple
     ones breaks xref but it does present
     the symbol multiple times in the
     file's symbol list which would be
     nice if it worked correctly */

  result = sn_insert_symbol(SN_GLOB_VAR_DEF,
                            NULL,
			    x,
			    sn_current_file(),
			    sn_line(), sn_column(),
			    sn_line(), sn_column() + (y - x),
			    0,
			    NULL,
			    NULL,
			    NULL,
			    NULL,
			    sn_line(), sn_column(),
			    sn_line(), sn_column() + (y - x));

  assert(result == 0);

  /* Add a read xref for this symbol name */

  result = sn_insert_xref(SN_REF_TO_GLOB_VAR,
                 SN_REF_TO_GLOB_VAR, /* PASSING 0 causes function to fail, what is the global var setting ??? */
		                     /* This SN_REF_TO_GLOB_VAR thing does not jive with the docs */
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 "GLOBAL", /* FIXME: A global var would not have a current function ??? */
                 NULL,
                 NULL,
                 x, /* refsymbol */
                 NULL,
                 sn_current_file(),
                 sn_line(),
                 SN_REF_WRITE);

  assert(result == 0);
  sn_advance_column(yyleng);
}

^{ws}*{func-decl}\({ws}*{quoted-symbol}{ws}*, {
  char * x = (char *) yytext;
  char * y;

  /* FIXME : Insert an xref for the AC_DEFUN call. */

  /* Skip AC_DEFUN( */

  for ( ; *x ; x++) {
      if (*x == '(') {
          x++;
          break;
      }
  }

  /* Skip {ws} and optional [ */

  for ( ; *x ; x++) {
      if (*x != ' ' &&
          *x != '\t' &&
          *x != '[') {
          break;
      }
  }

  /* x now points to the start of the macro name, find the end */

  sn_advance_column(x - yytext);

  for (y=x ; *y ; y++) {
      if (*y == ' ' ||
          *y == '\t' ||
          *y == ']' ||
          *y == ',') {
          *y = 0;
          break;
      }
  }

  /* x is now the name of the macro that is being defined */

  /*strcpy(current_function, x);*/

  result = sn_insert_symbol(SN_FUNC_DEF, NULL, x, sn_current_file(), 
		   sn_line(), sn_column(),
                   sn_line(), sn_column() + (y - x),
                   0 /* attribute */, NULL /* return type */,
                   NULL /* argument types */, NULL /* argument names */,
                   NULL /* comment */,
                   sn_line(), sn_column(),
                   sn_line(), sn_column() + (y - x) );

  assert(result == 0);

  /* FIXME : need to declare function too, can that be done at the
     same time or is this a bug in SN ?? */

  sn_advance_column(y - x);
}


{symbol}\( {
  char * x = (char *) yytext;
  char * y;

  for (y=x; *y ; y++) {
      if (*y == '(') {
          *y = 0;
          break;
      }
  }

  /* x is now the name of the macro to be invoked */

  result = sn_insert_xref(SN_REF_TO_FUNCTION,
                 SN_FUNC_DEF,
                 SN_REF_SCOPE_GLOBAL,
                 NULL,
                 current_function,
                 NULL,
                 NULL,
                 x,
                 NULL, 
                 sn_current_file(),
                 sn_line(),
                 SN_REF_PASS);

  assert(result == 0);

  sn_advance_column(yyleng);
}

%%
  
void
reset()
{
  sn_reset_line();
  sn_reset_column();
}

int
main(int argc, char *argv[])
{
  return sn_main(argc, argv, group, &yyin, yylex, reset);
}

