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
 * abrowser.l
 *
 * Copyright (C) 1997 Cygnus Solutions, Inc.
 *
 * Description:
 * Lex input file for the Source-Navigator PowerPC assembly parser.
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

enum { unknown, text, data };

static char *p;				/* general purpose pointer */
static char group[] = "asm";

static struct {
  char name[512];
  long line;
  long column;
  int length;
  unsigned char flag;
} last_label = { "", 0, 0, 0, 0 };

static int current_context = unknown;

void
emit_func_defn()
{
  last_label.line = sn_pop_line();
  last_label.column = sn_pop_column();

  sn_insert_symbol(SN_FUNC_DEF, NULL, last_label.name, sn_current_file(), 
		   last_label.line, last_label.column, sn_line(), 0,
		   0, NULL, NULL, NULL, NULL,
		   last_label.line, last_label.column, last_label.line,
		   last_label.column + last_label.length - 1);
}

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
  if (last_label.flag > 0) {
    emit_func_defn();
    last_label.flag = 0;
  }
  return(1);
}

/*
 * This function removes any trailing .[0-9]+ extensions that a compiler
 * might add to the symbol name to indicate that it is a static.  We are
 * more concerned with its real name than its storage class, so we'll drop
 * the numeric extension here.
 */

static
void drop_numeric_extn(char *text)
{
  if (strchr(text, '.') != NULL) {
    text += strlen(text) - 1;

    while (isdigit(*text) != 0) {
      *text = '\0';
      text--;
    }

    if (*text == '.') {
      *text = '\0';
      text--;
    }
  }
}

%}

%x COMMENT

alphas		[a-zA-Z]
digits		[0-9]
alphanums	({alphas}|{digits})
specials	[_"."]

ws		[ \t]
string          \".*\"
filename	{string}
symbol-name	[a-zA-Z_"."][a-zA-Z0-9_"."]*

%%

{string}	/* eat string literals */ 

".include"{ws}+{filename} {
  for (p = yytext; !isspace(*p); sn_advance_column(1), p++);
  for (; isspace(*p); sn_advance_column(1), p++);
}

"@function" {
  current_context = text;
  sn_advance_column(yyleng);
}

"@object" {
  current_context = data;
  sn_advance_column(yyleng);
}

^{ws}*(b[tclar]*){ws}+{symbol-name} {

  /* walk over the indentation (if any) */
  for (p = yytext; isspace(*p); sn_advance_column(1), p++);

  /* walk over the jump instruction mneumonic */
  for (; !isspace(*p); sn_advance_column(1), p++);

  /* walk over the whitespace between the opcode and the operand */
  for (; isspace(*p); sn_advance_column(1), p++);

  /* this is a candidate function if it is not a local label */

  if (strncmp(p, ".L", strlen(".L")) != 0 &&
      strncmp(last_label.name, ".L", strlen(".L")) != 0)
  {
    drop_numeric_extn(p);

    sn_insert_xref(SN_REF_TO_FUNCTION, SN_FUNC_DEF, SN_REF_SCOPE_GLOBAL,
                 NULL, last_label.name, NULL, NULL, p, NULL, 
                 sn_current_file(), sn_line(), SN_REF_PASS);
  }
  sn_advance_column(yyleng);
}

(".macro"){ws}+{symbol-name} {
  for (p = yytext; !isspace(*p); sn_advance_column(1), p++);
  for (; isspace(*p); sn_advance_column(1), p++);

  sn_insert_symbol(SN_MACRO_DEF, NULL, p, sn_current_file(), sn_line(),
                   sn_column(), sn_line(), sn_column() + (yytext + yyleng - p),
                   0, NULL, NULL, NULL, NULL, sn_line(), sn_column(),
                   sn_line(), sn_column() + (yytext + yyleng - p));

  sn_advance_column(yytext + yyleng - p);
}

("equate"){ws}+{symbol-name}{ws}*, {
  char * x = (char *) strstr(yytext, ",");
  char * y = x;
  while (*x)
  {
    if (isspace(*x))
      {
	x++; break;
      }
    else
      {
	x--;
      }
  }
  sn_advance_column(x - yytext);
  *y = 0;

  sn_insert_symbol(SN_CONS_DEF, NULL, x, sn_current_file(), sn_line(),
                   sn_column(), sn_line(), sn_column() + y - x, 0, NULL,
                   NULL, NULL, NULL, sn_line(), sn_column(), sn_line(),
                   sn_column() + y - x);

  sn_advance_column(y - x + 1); /* add one to jump over the trailing comma */
}

(\.l?comm){ws}+{symbol-name}{ws}*, {
  char * x = (char *) strstr(yytext, ",");
  char * y = x;
  while (*x)
  {
    if (isspace(*x))
    {
      x++;
      break;
    }
    else
    {
      x--;
    }
  }
  sn_advance_column(x - yytext);
  
  *y = 0;

  sn_insert_symbol(SN_GLOB_VAR_DEF, NULL, x, sn_current_file(), sn_line(),
                   sn_column(), sn_line(), sn_column() + y - x, 0, NULL,
                   NULL, NULL, NULL, sn_line(), sn_column(), sn_line(),
                   sn_column() + y - x);

  sn_advance_column(y - x + 1); /* jump over comma also */
}

^({symbol-name}":") {
  char * x = (char *) yytext;

  while (*x)
  {
    if (*x == ' ' || *x == '\t' || *x == ':')
      break;
    else
      x++;
  }
  *x = 0; 

  if (last_label.flag > 0) {
    emit_func_defn();
    last_label.flag = 0;
  }

  if (strncmp(yytext, ".L", strlen(".L")) != 0 &&
      strncmp(yytext, "gcc2_compiled", strlen("gcc2_compiled")) != 0)
  {
    /* we now have a suitable label to store in the database */
    drop_numeric_extn(yytext);

    switch (current_context) {
    case text:
      /* don't insert into the database! just flag the scanner to insert
	 the label name in last_buf the next time we detect a label of
	 _any_ kind. */

#if 0
      assert(last_label.flag != 1);
#endif
      sn_push_line();
      sn_push_column();

      last_label.length = yyleng;
      last_label.flag = 1;

      break;

    case data: /* fall through */
    case unknown:
      sn_insert_symbol(SN_GLOB_VAR_DEF, NULL, yytext,
		       sn_current_file(), sn_line(), sn_column(),
		       sn_line(), sn_column() + yyleng, 0, NULL,
		       NULL, NULL, NULL, sn_line(), sn_column(), sn_line(),
		       sn_column() + yyleng - 1);
      break;

    default:
      break;
    }
  }

  current_context = unknown;

  strcpy(last_label.name, yytext);
  sn_advance_column(yyleng);
}

"/*"		{ /* we really ought to buffer up the C comment as we eat
                     characters from the input so we don't just put "blah" 
                     in the S-N project dbase ;-) */ 
  sn_push_line();
  sn_push_column();
  BEGIN(COMMENT);
}

<COMMENT>\n	{
  sn_advance_line();
  sn_reset_column();
}

<COMMENT>\r	;

<COMMENT>.	{
  sn_advance_column(yyleng); 
}

<COMMENT>"*/"	{
  sn_advance_column(yyleng);

  sn_insert_comment(NULL, last_label.name, sn_current_file(), "blah",
                    sn_line(), sn_column());
  
  BEGIN(INITIAL);
}

(#).*\n {
  char *x;

  for (x = yytext, x++; isspace(*x); x++);

  sn_insert_comment(NULL, last_label.name, sn_current_file(), x, sn_line(),
                    sn_column());

  sn_advance_line();
  sn_reset_column();
}

.		{ sn_advance_column(yyleng); /* eat asm text */ }

\n		{ sn_advance_line(); sn_reset_column(); }

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


