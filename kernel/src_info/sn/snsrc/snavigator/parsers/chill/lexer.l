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
 * lexer.l
 *
 * Copyright (C) 1998 Cyngus Solutions
 * 
 * Description:
 * A GNU flex scanner for the GNU CHILL programming language.
 *
 * A few of these keywords are not even recognised in the current version
 * of the parser, but we will recognise them anyway in the hope that they
 * will generate warnings/errors to prevent the use of reserved words.
 */

%{

#include <stdio.h>
#include <string.h>

#include "symtab.h"
#include "snptools.h"
#include "parser.h"
#include "emit.h"

#include "lexinput.h"

#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))
 
/* For keeping count of how deep in an #ifdef nest we get. */
static int ifdef_nesting = 0;

#if 0
#define keyword(k) emitarg("keyword", k)
#define symbol(s) emitarg("symbol", s)
#define literal(kind, text) emitarg(kind"literal", text)
#define ident(i) emitarg("ident", i)
#define include(i) emitarg("include", i)
#else
#define keyword(k)
#define symbol(s)
#define literal(kind, text)
#define ident(i)
#define include(i)
#endif

%}

whitesp         [ \t]+
letter          [A-Za-z]
digit           [0-9]
digit-sequence  ({digit}|_)+

bin-digit       [01]
bin-sequence    ({bin-digit}|_)+

oct-digit       [0-7]
oct-sequence    ({oct-digit}|_)+

hex-digit       [0-9A-Fa-f]
hex-sequence    ({hex-digit}|_)+

bin-literal     [Bb]'{bin-sequence}
oct-literal     [Oo]'{oct-sequence}
dec-literal     ([Dd]')?{digit-sequence}
hex-literal     [Hh]'{hex-sequence}

bin-bit-literal	[Bb]'({bin-digit}|_)*'
oct-bit-literal [Oo]'({oct-digit}|_)*'
hex-bit-literal [Hh]'({hex-digit}|_)*'

char-string	[Cc]'{hex-sequence}'

char-literal    '(.|"^^"|\^{whitesp}?\(.*\))'
bitstr-literal  ({bin-bit-literal}|{oct-bit-literal}|{hex-bit-literal})
boolean-literal (FALSE|TRUE)
float-literal	((\.{digit-sequence})|({digit-sequence}\.)|({digit-sequence}\.{digit-sequence}))({exponent})?
integer-literal ({bin-literal}|{oct-literal}|{dec-literal}|{hex-literal})
string-literal  (\"(\"\"|[^\"])*\")|('(''|[^'])*')

empty-literal   NULL

exponent        [eE]-?{digit-sequence}

name            {letter}({letter}|{digit}|_)*

known-directvs  (use_seize_file|USE_SEIZE_FILE)(_restricted|_RESTRICTED)?

%x COMMENT
%x ELSEDIRECTIVE
%x PREDIRECTIVE
%x DIRECTIVE
%x POSTDIRECTIVE

%%

"/*"		{ BEGIN(COMMENT); sn_advance_column(2); }

--.*$ 		{ sn_advance_column(yyleng); }

^[ \t]*#[ \t]*include[ \t]*[\"\<].*[\"\>] {
  char *filename, *end;

  filename = strchr(yytext, '\"');
  if (!filename)
  {
     filename = strchr(yytext, '<');
  }
 
  /* Skip the leading delimiter. */
  filename++;

  end = strchr(filename, '\"');
  if (!end)
  {
     end = strchr(filename, '>');
  }

  /* Lop off the trailing delimiter. */
  *end = 0;

  include(filename);
  emit_include(filename, sn_line(), sn_column() + (filename - yytext - 1), 
	 		 sn_line(), sn_column() + strlen(filename) + 1);

  sn_advance_column(yyleng);
}

^[ \t]*#[ \t]*(if|ifdef).*\n {
  sn_advance_line();
  sn_reset_column();
  ifdef_nesting++;
}

^[ \t]*#[ \t]*(else|elseif).*\n {
  sn_advance_line();
  sn_reset_column();
  BEGIN(ELSEDIRECTIVE);
}

^[ \t]*#[ \t]*endif.*\n {
  assert(ifdef_nesting >= 0);

  sn_advance_line();
  sn_reset_column();

  /* Just left a non-nested #ifdef. */
  ifdef_nesting--;
}

<ELSEDIRECTIVE>^[ \t]*#[ \t]*endif.*\n {
  assert(ifdef_nesting >= 0);

  sn_advance_line();
  sn_reset_column();

  ifdef_nesting--;
  if (ifdef_nesting == 0)
  {
    /* Just fallen out of the nesting with this #endif. */
    BEGIN(INITIAL);
  }
}

<ELSEDIRECTIVE>. {
  /* Eat conditional code. */
  sn_advance_column(1);
}

<ELSEDIRECTIVE>\n {
  /* Ditto. */
  sn_advance_line();
  sn_reset_column();
}

^[ \t]*#.*$ {
  /* Ignore all other C preprocessor directives. */
  sn_advance_column(yyleng);
}

"<>" {
  sn_advance_column(2);
  BEGIN(PREDIRECTIVE);
}

<PREDIRECTIVE>[ \t]+ {
  sn_advance_column(yyleng);
}

<PREDIRECTIVE>\n {
  sn_advance_line();
  sn_reset_column();
}
 
<PREDIRECTIVE>. {
  unput(yytext[0]);
  BEGIN(DIRECTIVE);
}

<DIRECTIVE>{known-directvs}[ \t]*[\'\"].*[\'\"] {
  char *filename, *end;

  /* Skip leading text. */ 
  filename = strchr(yytext, '\'');
  if (!filename)
  {
    filename = strchr(yytext, '"');
  }

  /* Skip the leading quote marker. */
  filename++;
  
  /* Lop off the trailing quote marker. */
  end = strchr(filename, '\'');
  if (!end)
  {
    end = strchr(filename, '"');
  }
  *end = 0;

  include(filename);
  emit_include(filename, sn_line(), sn_column() + (filename - yytext - 1), 
			sn_line(), sn_column() + strlen(filename) + 1);

  sn_advance_column(yyleng);
  BEGIN(POSTDIRECTIVE);
}

<DIRECTIVE>[^\< \t\n]+ {
  sn_advance_column(yyleng);
  BEGIN(POSTDIRECTIVE);
}

<POSTDIRECTIVE>"<>" {
  sn_advance_column(2);
  BEGIN(INITIAL);
}

<POSTDIRECTIVE>[ \t]+ {
  sn_advance_column(yyleng);
}

<POSTDIRECTIVE>\n {
  sn_advance_line();
  sn_reset_column();
  BEGIN(INITIAL);
}

<POSTDIRECTIVE>. { sn_advance_column(1); /* eat junk we don't expect */ }

{bitstr-literal}    {
   literal("bitstring", yytext);
   sn_advance_column(yyleng);
   return BITLITERAL;
}

{boolean-literal}   {
    literal("boolean", yytext);
    sn_advance_column(yyleng); 
    return BOOLITERAL;
}

{char-literal}      {
    literal("char", yytext);
    sn_advance_column(yyleng); 
    return CHARLITERAL;
}

{empty-literal}     {
    literal("empty", yytext);
    sn_advance_column(yyleng);
    return EMPTYLITERAL;
}

{float-literal}     {
    literal("float", yytext);
    sn_advance_column(yyleng); 
    return FLOATLITERAL;
}

{integer-literal}   {
    literal("integer", yytext);
    sn_advance_column(yyleng);
    return INTLITERAL;
}

{char-string}|{string-literal} {
    literal("string", yytext);
    sn_advance_column(yyleng);
    return STRINGLITERAL;
}

"."         { symbol("dot"); sn_advance_column(1); return '.'; }
":="        { symbol("assignment"); sn_advance_column(2); return ASSIGN; }
"<="        { symbol("less-than-or-equal"); sn_advance_column(2); return LESSTHANEQ; }
">="        { symbol("greater-than-or-equal"); sn_advance_column(2); return GREATERTHANEQ; }
"<"         { symbol("less-than"); sn_advance_column(1); return '<'; }
">"         { symbol("greater-than"); sn_advance_column(1); return '>'; }
"/="        { symbol("not-equal"); sn_advance_column(2); return NOTEQ; }
"="         { symbol("equal"); sn_advance_column(1); return '='; }
":"         { symbol("colon"); sn_advance_column(1); return ':'; }

";"         {
			symbol("semicolon"); 
			yylval.punct.line = sn_line();
			yylval.punct.startcol = sn_column();
			sn_advance_column(1); 
			yylval.punct.endcol = sn_column();
			return ';';
	    }

","         { symbol("comma"); sn_advance_column(1); return ','; }
"(:"        { symbol("left-tuple-bracket"); sn_advance_column(2); return LEFTTUPLE; }
":)"        { symbol("right-tuple-bracket"); sn_advance_column(2); return RIGHTTUPLE; }
"("         { symbol("left-parenthesis"); sn_advance_column(1); return '('; }
")"         { symbol("right-parenthesis"); sn_advance_column(1); return ')'; }
"["         { symbol("left-square-bracket"); sn_advance_column(1); return LEFTTUPLE; }
"]"         { symbol("right-square-bracket"); sn_advance_column(1); return RIGHTTUPLE; }
"->"        { symbol("arrow"); sn_advance_column(2); return ARROW; }
"+"         { symbol("plus"); sn_advance_column(1); return '+'; }
"-"         { symbol("minus"); sn_advance_column(1); return '-'; }
"**"        { symbol("exponent"); sn_advance_column(2); return EXPONENT; }
"*"         { symbol("asterisk"); sn_advance_column(1); return '*'; }
"//"        { symbol("double-solidus"); sn_advance_column(2); return DOUBLESOLIDUS; }
"/"         { symbol("solidus"); sn_advance_column(1); return '/'; }
"#"         { symbol("sharp"); sn_advance_column(1); return '#'; }
\"          { symbol("quote"); sn_advance_column(1); return '"'; }
\!          { symbol("bang"); sn_advance_column(1); return '!'; }

access|ACCESS	  { keyword("ACCESS"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return ACCESS; }
after|AFTER	  { keyword("AFTER"); sn_advance_column(yyleng); return AFTER; }
all|ALL		  { keyword("ALL"); sn_advance_column(yyleng); return ALL; }
and|AND		  { keyword("AND"); sn_advance_column(yyleng); return AND; }
andif|ANDIF	  { keyword("ANDIF"); sn_advance_column(yyleng); return ANDIF; }
array|ARRAY	  { keyword("ARRAY"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return ARRAY; }
assert|ASSERT	  { keyword("ASSERT"); sn_advance_column(yyleng); return ASSERT; }
at|AT		  { keyword("AT"); sn_advance_column(yyleng); return AT; }
based|BASED	  { keyword("BASED"); sn_advance_column(yyleng); return BASED; }
begin|BEGIN	  { keyword("BEGIN"); sn_advance_column(yyleng); return BEG; }
bin|BIN		  { keyword("BIN"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return BIN; }
body|BODY	  { keyword("BODY"); sn_advance_column(yyleng); return BODY; }
bools|BOOLS	  { keyword("BOOLS"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return BOOLS; }
buffer|BUFFER	  { keyword("BUFFER"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return BUFFER; }
by|BY		  { keyword("BY"); sn_advance_column(yyleng); return BY; }
case|CASE	  { keyword("CASE"); sn_advance_column(yyleng); return CASE; }
cause|CAUSE	  { keyword("CAUSE"); sn_advance_column(yyleng); return CAUSE; }
chars|CHARS	  { keyword("CHARS"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return CHARS; }

context|CONTEXT   { keyword("CONTEXT"); sn_advance_column(yyleng); return CONTEXT; }
continue|CONTINUE { keyword("CONTINUE"); sn_advance_column(yyleng); return CONTINUE; }
cycle|CYCLE       { keyword("CYCLE"); sn_advance_column(yyleng); return CYCLE; }
dcl|DCL           { keyword("DCL"); sn_advance_column(yyleng); return DCL; }
delay|DELAY       { keyword("DELAY"); sn_advance_column(yyleng); return DELAY; }
do|DO             { keyword("DO"); sn_advance_column(yyleng); return DO; }
down|DOWN         { keyword("DOWN"); sn_advance_column(yyleng); return DOWN; }
dynamic|DYNAMIC   { keyword("DYNAMIC"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return DYNAMIC; }
else|ELSE         { keyword("ELSE"); sn_advance_column(yyleng); return ELSE; }
elsif|ELSIF       { keyword("ELSIF"); sn_advance_column(yyleng); return ELSIF; }
end|END	          { keyword("END"); sn_advance_column(yyleng); return END; }
esac|ESAC         { keyword("ESAC"); sn_advance_column(yyleng); return ESAC; }
event|EVENT       { keyword("EVENT"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return EVENT; }
ever|EVER         { keyword("EVER"); sn_advance_column(yyleng); return EVER; }

exceptions|EXCEPTIONS  { keyword("EXCEPTIONS"); sn_advance_column(yyleng); return EXCEPTIONS; }

exit|EXIT	  { keyword("EXIT"); sn_advance_column(yyleng); return EXIT; }
fi|FI		  { keyword("FI"); sn_advance_column(yyleng); return FI; }
for|FOR		  { keyword("FOR"); sn_advance_column(yyleng); return FOR; }
forbid|FORBID	  { keyword("FORBID"); sn_advance_column(yyleng); return FORBID; }
general|GENERAL	  { keyword("GENERAL"); sn_advance_column(yyleng); return GENERAL; }
goto|GOTO	  { keyword("GOTO"); sn_advance_column(yyleng); return GOTO; }
grant|GRANT	  { keyword("GRANT"); sn_advance_column(yyleng); return GRANT; }
if|IF		  { keyword("IF"); sn_advance_column(yyleng); return IF; }
in|IN		  { keyword("IN"); yylval.attrib.text = SN_StrDup(""); sn_advance_column(yyleng); return IN; }
init|INIT	  { keyword("INIT"); sn_advance_column(yyleng); return INIT; }
inline|INLINE	  { keyword("INLINE"); sn_advance_column(yyleng); return INLINE; }
inout|INOUT	  { keyword("INOUT"); yylval.attrib.text = SN_StrDup(yytext); sn_advance_column(yyleng); return INOUT; }
loc|LOC		  { keyword("LOC"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return LOC; }
mod|MOD		  { keyword("MOD"); sn_advance_column(yyleng); return MOD; }

module|MODULE	  {
			keyword("MODULE");
			yylval.keyword.line = sn_line();
			yylval.keyword.startcol = sn_column();
			sn_advance_column(yyleng); 
			yylval.keyword.endcol = sn_column();
			return MODULE;
		  }

newmode|NEWMODE   { keyword("NEWMODE"); sn_advance_column(yyleng); return NEWMODE; }
nonref|NONREF     { keyword("NONREF"); sn_advance_column(yyleng); return NONREF; }
nopack|NOPACK	  { keyword("NOPACK"); sn_advance_column(yyleng); return NOPACK; }
not|NOT           { keyword("NOT"); sn_advance_column(yyleng); return NOT; }
od|OD	          { keyword("OD"); sn_advance_column(yyleng); return OD; }
of|OF             { keyword("OF"); sn_advance_column(yyleng); return OF; }
on|ON	          { keyword("ON"); sn_advance_column(yyleng); return ON; }
or|OR             { keyword("OR"); sn_advance_column(yyleng); return OR; }
orif|ORIF         { keyword("ORIF"); sn_advance_column(yyleng); return ORIF; }
out|OUT	          { keyword("OUT"); yylval.attrib.text = SN_StrDup(yytext); sn_advance_column(yyleng); return OUT; }
pack|PACK         { keyword("PACK"); sn_advance_column(yyleng); return PACK; }
pos|POS	          { keyword("POS"); sn_advance_column(yyleng); return POS; }
powerset|POWERSET { keyword("POWERSET"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return POWERSET; }
prefixed|PREFIXED { keyword("PREFIXED"); sn_advance_column(yyleng); return PREFIXED; }
priority|PRIORITY { keyword("PRIORITY"); sn_advance_column(yyleng); return PRIORITY; }
proc|PROC         { keyword("PROC"); sn_advance_column(yyleng); return PROC; }
process|PROCESS   { keyword("PROCESS"); sn_advance_column(yyleng); return PROCESS; }
range|RANGE       { keyword("RANGE"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return RANGE; }
read|READ         { keyword("READ"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return READ; }
receive|RECEIVE   { keyword("RECEIVE"); sn_advance_column(yyleng); return RECEIVE; }
recursive|RECURSIVE { keyword("RECURSIVE"); sn_advance_column(yyleng); return RECURSIVE; }
ref|REF           { keyword("REF"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return REF; }
region|REGION     { keyword("REGION"); sn_advance_column(yyleng); return REGION; }
rem|REM	          { keyword("REM"); sn_advance_column(yyleng); return REM; }
remote|REMOTE     { keyword("REMOTE"); sn_advance_column(yyleng); return REMOTE; }
result|RESULT     { keyword("RESULT"); sn_advance_column(yyleng); return RESULT; }
return|RETURN     { keyword("RETURN"); sn_advance_column(yyleng); return RETURN; }
returns|RETURNS   { keyword("RETURNS"); sn_advance_column(yyleng); return RETURNS; }
row|ROW	          { keyword("ROW"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return ROW; }
seize|SEIZE       { keyword("SEIZE"); sn_advance_column(yyleng); return SEIZE; }
send|SEND         { keyword("SEND"); sn_advance_column(yyleng); return SEND; }
set|SET	          { keyword("SET"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return SET; }
signal|SIGNAL     { keyword("SIGNAL"); sn_advance_column(yyleng); return SIGNAL; }
simple|SIMPLE     { keyword("SIMPLE"); sn_advance_column(yyleng); return SIMPLE; }

spec|SPEC         {
			keyword("SPEC"); 
			yylval.keyword.line = sn_line();
			yylval.keyword.startcol = sn_column();
			sn_advance_column(yyleng); 
			yylval.keyword.endcol = sn_column();	
			return SPEC;
		  }

start|START	  { keyword("START"); sn_advance_column(yyleng); return START; }
static|STATIC	  { keyword("STATIC"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return STATIC; }
step|STEP	  { keyword("STEP"); sn_advance_column(yyleng); return STEP; }
stop|STOP	  { keyword("STOP"); sn_advance_column(yyleng); return STOP; }
struct|STRUCT	  { keyword("STRUCT"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return STRUCT; }
syn|SYN		  { keyword("SYN"); sn_advance_column(yyleng); return SYN; }
synmode|SYNMODE	  { keyword("SYNMODE"); sn_advance_column(yyleng); return SYNMODE; }
text|TEXT	  { keyword("TEXT"); yylval.type.text = SN_StrDup(yytext); sn_advance_column(yyleng); return TEXT; }
then|THEN	  { keyword("THEN"); sn_advance_column(yyleng); return THEN; }
this|THIS	  { keyword("THIS"); sn_advance_column(yyleng); return THIS; }
timeout|TIMEOUT	  { keyword("TIMEOUT"); sn_advance_column(yyleng); return TIMEOUT; }
to|TO		  { keyword("TO"); sn_advance_column(yyleng); return TO; }
up|UP		  { keyword("UP"); sn_advance_column(yyleng); return UP; }
varying|VARYING	  { keyword("VARYING"); sn_advance_column(yyleng); return VARYING; }
while|WHILE	  { keyword("WHILE"); sn_advance_column(yyleng); return WHILE; }
with|WITH	  { keyword("WITH"); sn_advance_column(yyleng); return WITH; }
xor|XOR		  { keyword("XOR"); sn_advance_column(yyleng); return XOR; }

{name}		  { 
			ident(yytext); 
			yylval.id.line = sn_line();
			yylval.id.startcol = sn_column();
			sn_advance_column(yyleng); 
			yylval.id.endcol = sn_column();
			yylval.id.type = unknown;
			yylval.id.name = SN_StrDup(yytext); 
			return NAME;
		  }

{whitesp}	  { sn_advance_column(yyleng); /* eat whitespace */ }
\n		  { sn_advance_line(); sn_reset_column(); }

.		  { sn_advance_column(1); /* silently discard unknown characters */ }

<COMMENT>"*/"	  { BEGIN(INITIAL); sn_advance_column(2); }

<COMMENT>.	  { sn_advance_column(1); }

<COMMENT>\n	  { sn_advance_line(); sn_reset_column(); }

%%

#define YY_MAIN 0

void
reset()
{
  sn_reset_line();
  sn_reset_column();
  sn_reset_encoding();
}

