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
 * ebrowser.l
 *
 * Copyright (C) 1997 Cygnus Solutions
 *
 * Description:
 * A lex specification for a scanner to parse ELF programs and insert
 * relevant information into the Source-Navigator project database.
 */

#include <ctype.h>
#include <stdlib.h>
#include "snptools.h"

static char group[] = "elf";

static long brace_nest = 0;
static long string_caller = 0;
static long comment_caller = 0;

static char current_class[512];
static char current_fn[512];
static char inheritence[2000];

static void class_name();
static void class_var();
static void global_proc();
static void var_name();
static void define_const();

#undef yywrap
#define	YY_SKIP_YYWRAP	1
#define	yywrap()	(1)

%}

%x COMMENT
%x STRING
%s CLASS
%s VAR

name [a-z_][a-z_0-9@]*
cons [a-z_][a-z_0-9@#]*
wspace [ \t\r\n]

%%

^[ \t]*"'".*\n |
[ \t]*"'".*\n {
   sn_advance_line();
   sn_reset_column();
}

"/*" {
   sn_advance_column(yyleng);
   BEGIN(COMMENT);
}

<COMMENT>\n {
   /* line counting */
   sn_advance_line();
   sn_reset_column();
}

<COMMENT>\r ;

<COMMENT>. {
   /* eat up everything */
   sn_advance_column(yyleng);
}

<COMMENT>"*"+"/" {
   /* end of comment */
   sn_advance_column(yyleng);
   BEGIN(comment_caller);
}

\" {
   sn_advance_column(yyleng);
   BEGIN(STRING);
}

<STRING>\\. {
   /* eat up \. characters like \\ \" \{ */
   sn_advance_column(yyleng);
}

<STRING>\r ;

<STRING>\n {
   sn_advance_line();
   sn_reset_column();
}

<STRING>\" {
   /* end of string */
   sn_advance_column(yyleng);
   BEGIN(string_caller);
}

<STRING>. {
   /* eat up everything */
   sn_advance_column(yyleng);
}

^[ \t]*VAR[ \t]+FORMAT[ \t]+{name} |
^[ \t]*VAR[ \t]+ {
   sn_advance_column(yyleng);
   BEGIN(VAR);
   string_caller = comment_caller = YY_START;
}

<VAR>{name}[ \t]*","* {
   var_name(0);
   if (yytext[yyleng - 1] != ',')
   {
      BEGIN(INITIAL);
      string_caller = comment_caller = YY_START;
   }
   sn_count_chars(yytext, yyleng);
}

<VAR>FORMAT{wspace}+{name}{wspace}+{name} {
   var_name(1);
   sn_count_chars(yytext, yyleng);
}

<VAR>[ \t]+ {
   sn_advance_column(yyleng);
}

^[ \t]*FORMAT[ \t]+{name}   {
   if (YY_START == CLASS)
   {
      REJECT;
   }
   else
   {
      class_name();
      sn_advance_column(yyleng);
      BEGIN(CLASS);
      string_caller = comment_caller = YY_START;
   }
}

<CLASS>"FORMAT"{wspace}+{name}{wspace}+{name}[ \t]*","* {
   class_var(1);
   if (yytext[yyleng - 1] != ',')
   {
      if (inheritence[0])
      {
         sn_insert_symbol(PAF_CLASS_INHERIT, current_class, inheritence,
                          sn_current_file(), sn_line(), 0, 0, 0, 0L,
                          NULL, NULL, NULL, NULL, 0, 0, 0, 0);
         inheritence[0] = '\0';
      }
      BEGIN(INITIAL);
      string_caller = comment_caller = YY_START;
   }
   sn_count_chars(yytext, yyleng);
}

<CLASS>{name}[ \t]*","* {
   class_var(0);
   if (yytext[yyleng - 1] != ',')
   {
      if (inheritence[0])
      {
         sn_insert_symbol(PAF_CLASS_INHERIT, current_class, inheritence,
                          sn_current_file(), sn_line(), 0, 0, 0, 0L,
                          NULL, NULL, NULL, NULL, 0, 0, 0, 0);
         inheritence[0] = '\0';
      }
      BEGIN(INITIAL);
      string_caller = comment_caller = YY_START;
   }
   sn_advance_column(yyleng);
}

^[\t]*DEFINE[ \t]+{cons} {
   define_const();
   sn_advance_column(yyleng);
}

^[ \t]*(MACRO|UIMACRO|FUNCTION|RNRMACRO)[ \t]+{name} {
   brace_nest = 1;
   global_proc();
   sn_advance_column(yyleng);
}

^[ \t]*END(MACRO|FUNCTION|COMMAND) {
   current_fn[0] = '\0';
   sn_advance_column(yyleng);
   brace_nest = 0;
}

[a-z0-9_@]+@ {
   sn_advance_column(yyleng);
   sn_insert_xref(PAF_REF_TO_FUNCTION, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
                  NULL, current_fn, NULL, NULL, yytext, NULL, sn_current_file(),
                  sn_line(), PAF_REF_PASS);
}

[a-z][a-z0-9]*[\t ]*\( {
   int len;
   char *p, temp[100];

   sn_advance_column(yyleng);
 
   for (len = yyleng, p = &yytext[yyleng - 1]; isspace(*p) || *p == '('; p--, len--);

   strncpy(temp, yytext, len);
   temp[len] = 0;

   sn_insert_xref(PAF_REF_TO_FUNCTION, PAF_FUNC_DEF, PAF_REF_SCOPE_GLOBAL,
                  NULL, current_fn, NULL, NULL, temp, NULL, sn_current_file(),
                  sn_line(), PAF_REF_PASS);
}

[a-z0-9@_#&\.\-\+\$]+ {
   sn_advance_column(yyleng);
}

\n {
   sn_advance_line();
   sn_reset_column();
}

\\. { 
   /* eat up escape characters like \\ \" and \{ */
   sn_advance_column(yyleng);
}

\r ;

. {
   sn_advance_column(yyleng);
}

%%

void
reset()
{
   brace_nest = 0;
   comment_caller = 0;
   sn_reset_line();
   sn_reset_column();
}

int
main(int argc, char *argv[])
{
   return sn_main(argc, argv, group, &yyin, yylex, reset);
}

static void
class_name()
{
   char *p = sn_afterlastwspace(yytext);

   strcpy(current_class, p);

   sn_insert_symbol(PAF_CLASS_DEF, NULL, current_class,
                    sn_current_file(), sn_line(), p - yytext + sn_column(),
                    0, 0, 0L, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
}

static void
class_var(int lastw)
{
   char tmp[500], *p, *ptmp;

   if (lastw)
   {
      for (p = yytext; !isspace(*p); p++);
      for (; isspace(*p); p++);
      for (ptmp = tmp; !isspace(*p);) *ptmp++ = *p++;
      *ptmp = 0;

      strcat(inheritence, " ");
      strcat(inheritence, tmp);
   }
   p = sn_afterlastwspace(yytext);

   sn_insert_symbol(PAF_MBR_VAR_DEF, current_class, p, sn_current_file(),
                    sn_line(), p - yytext + sn_column(), 0, 0, 0L, NULL, NULL,
                    NULL, NULL, 0, 0, 0, 0);
}

static void
var_name(int lastw)
{
   char tmp[500], *ptmp;
   int cou;

   if (lastw)
   {
      strcpy(tmp, sn_afterlastwspace(yytext));
   }
   else
   {
      for(ptmp = yytext, cou = 0; !isspace(*ptmp) &&
          *ptmp != ',' && cou < yyleng; cou++, ptmp++);
      memcpy(tmp, yytext, cou);
      tmp[cou] = '\0';
   }

   if (brace_nest == 0)
   {
      char *p = sn_afterlastwspace(yytext);

      sn_insert_symbol(PAF_GLOB_VAR_DEF, NULL, p, sn_current_file(), sn_line(),
                       p - yytext + sn_column(), 0, 0, 0L, NULL, NULL, NULL, NULL,
                       0, 0, 0, 0);
   }	
}

static void
global_proc()
{
   char *p = sn_afterlastwspace(yytext);

   strcpy(current_fn, p);

   sn_insert_symbol(PAF_FUNC_DEF, NULL, p, sn_current_file(), sn_line(),
                    p - yytext + sn_column(), 0, 0, 0L, NULL, NULL, NULL, NULL,
                    0, 0, 0, 0);
}

static void
define_const()
{
   char *p;
   int begpos, endpos;

   for (begpos = sn_column(), p = yytext; isspace(*p); p++, begpos++);
   for (endpos = begpos; !isspace(*p); p++, endpos++);

   p = sn_afterlastwspace(yytext);

   sn_insert_symbol(PAF_CONS_DEF, NULL, p, sn_current_file(), sn_line(),
                    p - yytext + sn_column(), 0, 0, 0L, NULL, NULL, NULL,
                    NULL, 0, 0, 0, 0);
}

