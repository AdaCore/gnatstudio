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

%{
/*
 * sbrowser.l
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * A lex specification for a scanner to parse sendmail configuration files
 * and store relevant information into the Source-Navigator project database.
 */

#include <ctype.h>
#include "lexinput.h"
#include "tcl.h"
#include "snptools.h"

static char group[] = "sendmail";
static char *p;			/* a general purpose pointer */

static unsigned short flag = 0;
static char ruleset[512] = "";

#undef yywrap
#define	YY_SKIP_YYWRAP	1

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))
 
int
yywrap()
{
   if (flag)
   {
      long line = sn_pop_line();
      sn_insert_symbol(SN_SUBR_DEF, NULL, ruleset, sn_current_file(),
                       line, 0, sn_line(), 0, 0, NULL, NULL, NULL,
		       NULL, line, 0, line, strlen(ruleset));
      flag = 0;
   }
   return(1);
} 
  
%}

%x RULE

%%

^#.*\n {			/* a comment */

  yytext[yyleng - 1] = '\0';

  sn_insert_comment(NULL, ruleset, sn_current_file(), yytext,
		    sn_line(), sn_column());
  
  sn_advance_line();
  sn_reset_column();
}

^M.*,.*\n {
  p = strchr(yytext, ',');
  *p = '\0';

  p = yytext + 1;

  sn_insert_symbol(SN_FUNC_DEF, NULL, p, sn_current_file(),
		   sn_line(), sn_column() + 1, sn_line(), 
		   sn_column() + strlen(p), 0, NULL, NULL, NULL, NULL,
		   sn_line(), sn_column() + 1, sn_line(),
		   sn_column() + strlen(p) + 1);

  sn_advance_line();
  sn_reset_column();
}
  
^[CD].*\n {
  char buf[2] = { 0, 0 };
  
  buf[0] = yytext[1];
  
  sn_insert_symbol(SN_MACRO_DEF, NULL, buf, sn_current_file(),
                   sn_line(), 1, sn_line(), 2, 0, NULL, NULL, NULL,
                   NULL, sn_line(), 1, sn_line(), 2);

  sn_advance_line();
  sn_reset_column();
}

^F\{.*\}.*\n {
  char * filename;
  unsigned margin;

  p = strchr(yytext, '\n');
  *p = '\0';

  p = sn_afterlastwspace(yytext);
  margin = p - yytext;

  filename = ckalloc(strlen(p) + 1);
  strcpy(filename, p);

  p = strchr(yytext, '}') + 1;
  *p = '\0';

  p = yytext + 1; /* jump over `F' */

  sn_insert_symbol(SN_MACRO_DEF, NULL, p, sn_current_file(),
		   sn_line(), sn_column() + 1, sn_line(),
		   sn_column() + 1 + strlen(p), 0,
		   NULL, NULL, NULL, NULL, sn_line(), sn_column() + 1,
		   sn_line(), sn_column() + 1 + strlen(p));
 
  /* need to include an include db entry too */
  sn_insert_symbol(SN_INCLUDE_DEF, NULL, filename, sn_current_file(),
		   sn_line(), sn_column() + margin, sn_line(), yyleng - 1, 0,
		   NULL, NULL, NULL, NULL, sn_line(), sn_column() + margin,
		   sn_line(), yyleng - 1);

  sn_advance_line();
  sn_reset_column();

  ckfree(filename);
}

^F.*\n {
  unsigned margin;
  char * filename;
  char buf[2] = { 0, 0 };

  p = strchr(yytext, '\n');
  *p = '\0';

  buf[0] = yytext[1];

  filename = sn_afterlastwspace(yytext);

  sn_insert_symbol(SN_MACRO_DEF, NULL, buf, sn_current_file(),
		   sn_line(), sn_column() + 1, sn_line(), sn_column() + 2, 0,
		   NULL, NULL, NULL, NULL, sn_line(), sn_column() + 1,
		   sn_line(), sn_column() + 2);

  margin = filename - yytext;

  /* need to include an include db entry too */
  sn_insert_symbol(SN_INCLUDE_DEF, NULL, filename, sn_current_file(),
		   sn_line(), sn_column() + margin, sn_line(), yyleng - 1, 0,
		   NULL, NULL, NULL, NULL, sn_line(), sn_column() + margin,
		   sn_line(), yyleng - 1);
 
  sn_advance_line();
  sn_reset_column();
}

^S.*\n {
  if (flag)
  {
    long line = sn_pop_line();
    sn_insert_symbol(SN_SUBR_DEF, NULL, ruleset, sn_current_file(),
                     line, 0, sn_line(), 0, 0, NULL, NULL, NULL,
		     NULL, line, 0, line, strlen(ruleset));
    flag = 0;
  }

  sn_push_line();

  p = strchr(yytext, '\n');
  *p = '\0';

  p = yytext;
  while (*p != ' ' && *p != '\t') { p++; }
  while (*p == ' ' || *p == '\t') { p++; }
  
  /* go back one position */
  p--;
  *p = 'S';
 
  strcpy(ruleset, yytext);
  flag = 1;

  sn_advance_line();
  sn_reset_column();
}

^R {
  sn_advance_column(yyleng);
  BEGIN(RULE);
}

<RULE>$#.*[ \t] {
  p = yytext;
  while (*p != ' ' && *p != '\t') { p++; }

  *p = '\0';

  sn_insert_xref(SN_REF_TO_FUNCTION, SN_SUBR_DEF, SN_REF_SCOPE_GLOBAL,
		 NULL, ruleset, NULL, NULL, &yytext[2], NULL, sn_current_file(),
		 sn_line(), SN_REF_PASS);

  sn_advance_column(yyleng); 
}

<RULE>$=?[a-zA-Z] {
  p = yytext + 1;
  if (*p == '=') { p++; }

  sn_insert_xref(SN_REF_TO_DEFINE, SN_SUBR_DEF, SN_REF_SCOPE_GLOBAL,
		 NULL, ruleset, NULL, NULL, p, NULL, sn_current_file(),
		 sn_line(), SN_REF_READ);

  sn_advance_column(yyleng);
}

<RULE>$=\{.*\} {
  p = yytext + 1;
  if (*p == '=') { p++; }

  sn_insert_xref(SN_REF_TO_DEFINE, SN_SUBR_DEF, SN_REF_SCOPE_GLOBAL,
		 NULL, ruleset, NULL, NULL, p, NULL, sn_current_file(),
		 sn_line(), SN_REF_READ);

  sn_advance_column(yyleng);
}

<RULE>$\>[0-9]+ {
  p = strchr(yytext, '>');
  *p = 'S';			/* we know this for sure */

  sn_insert_xref(SN_REF_TO_SUBROUTINE, SN_SUBR_DEF, SN_REF_SCOPE_GLOBAL,
		 NULL, ruleset, NULL, NULL, p, NULL, sn_current_file(),
		 sn_line(), SN_REF_PASS);

  sn_advance_column(yyleng);
}

<RULE>\n {
  sn_advance_line();
  sn_reset_column();
  BEGIN(INITIAL);
}
  
<RULE>. { /* eat whitespace */
  sn_advance_column(yyleng);
}

\n {
   sn_advance_line();
   sn_reset_column();
}

\r ;

. { sn_advance_column(yyleng); }

%%

void
reset()
{
   sn_reset_line();
   sn_reset_column();
   sn_reset_encoding();
}

int
main(int argc, char *argv[])
{
   return sn_main(argc, argv, group, &yyin, yylex, reset);
}

