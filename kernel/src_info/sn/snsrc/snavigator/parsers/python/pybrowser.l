/* -*- c -*- */
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
 * pybrowser.l
 *
 * Written by Thomas Heller <thomas.heller@ion-tof.com>
 *
 * Description:
 * A lex specification for a scanner to parse python source code files
 * and store relevant information into the Source-Navigator project database.
 */

#include <ctype.h>
#include "lexinput.h"
#include "tcl.h"
#include "snptools.h"

static char group[] = "python";

static char *mystrdup(char *);
static void var_or_const(int);

static struct stack *scope_stack;

static void push(int, int, char *);
static void pop(int);
static char *top(int *);
static int calc_indent (char*, char *);
static void baseclasses (char *, char *);
static void parse_triple(int);

/* XXX Is this correct? */
#undef YY_INPUT
#define YY_INPUT(buf,r,ms) (r = sn_encoded_input(buf, ms))

int
yywrap()
{
   return(1);
} 
  
%}

%%

\"\"\" {
    sn_advance_column (3);
    parse_triple((int)'"');
}

\'\'\' {
    sn_advance_column (3);
    parse_triple((int)'\'');
}

^[ \t]*def[ \t]+[a-zA-Z_][a-zA-Z_0-9]*[ \t]*\( {
    int ofs;
    char *p;
    int indent;
    char *classname;
    int code;
    int context;
    /* point just before the '(', then go backwards to find and mark
       the end of the name */
    p = yytext + yyleng - 1;
    while ((p[-1] == ' ') || (p[-1] == '\t'))
	--p;
    *p = '\0';
    /* now go backwards to find the beginning */
    p = yytext + yyleng-1;
    while ((p > yytext) && (p[-1] != ' ') && (p[-1] != '\t'))
	--p;
    ofs = p - yytext;

    indent = calc_indent (yytext, p);
    pop(indent);
    classname = top(&context);
    if (classname) {
	switch (context) {
	case SN_CLASS_DEF:
	    code = SN_MBR_FUNC_DEF;
	    break;
	default:
	    code = SN_FUNC_DEF;
	    classname = NULL;
	    break;
	}
    } else
	code = SN_FUNC_DEF;

    push(indent, code, p);

    sn_insert_symbol(code, classname, p, sn_current_file(),
		     sn_line(), sn_column()+ofs, sn_line(), 
		     sn_column()+ofs + strlen(p),
		     0, NULL, NULL, NULL, NULL,
		     sn_line(), sn_column()+ofs, sn_line(),
		     sn_column()+ofs + strlen(p));
}
  
^[ \t]*class[ \t]+[a-zA-Z_][a-zA-Z_0-9]*[ \t]*(\([^)]*\))?[ \t]*: {
    int ofs;
    char *p, *super;
    int indent;
    char *end;
    yytext[yyleng] = '\0';

    p = yytext;
    while (isspace (*p))
        ++p;
    indent = calc_indent (yytext, p);

    p += strlen ("class");
    while (isspace (*p))
        ++p;
    ofs = p - yytext;
    end = p;
    while (*end && (isalnum (*end) || (*end == '_')))
        ++end;
    *end = '\0';

    pop(indent);
    push(indent, SN_CLASS_DEF, p);
    sn_insert_symbol(SN_CLASS_DEF, NULL, p, sn_current_file(),
		     sn_line(), sn_column()+ofs, sn_line(), 
		     sn_column()+ofs + strlen(p),
		     0, NULL, NULL, NULL, NULL,
		     sn_line(), sn_column()+ofs, sn_line(),
		     sn_column()+ofs + strlen(p));
    ++end;
    super = end;
    while ((end < yytext+yyleng) && *end) {
	if (*end == '\n') {
            sn_advance_line();
            sn_reset_column();
	}
        ++end;
    }
    baseclasses (p, super);
}

^[A-Z_][A-Z0-9_]*[ \t]*= { var_or_const (SN_CONS_DEF); }
  
^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*= { var_or_const (SN_GLOB_VAR_DEF); }
  
\n {
   sn_advance_line();
   sn_reset_column();
}

. { sn_advance_column(yyleng); }

%%
struct stack {
    int indent;
    int code;
    char *name;
    struct stack *prev;
};

static struct stack *scope_stack = NULL;

static int calc_indent(char *start, char *stop)
{
    int indent = 0;
    char *cp = start;
    while (cp < stop) {
	switch (*cp++) {
	case ' ':
	    ++indent;
	    break;
	case '\t':
	    indent = (indent / 8) * 8 + 8;
	    break;
	default:
	    return indent;
	}
    }
    return indent;
}

static char *top(int *code)
{
    if (scope_stack) {
	*code = scope_stack->code;
	return scope_stack->name;
    }
    return NULL;
}

static void push(int indent, int code, char *name)
{
    struct stack *node;

    node = (struct stack *)malloc(sizeof (struct stack));
    node->indent = indent;
    node->code = code;
    node->name = mystrdup (name);
    node->prev = scope_stack;
    scope_stack = node;
}

static void pop(int indent)
{
    while (scope_stack && (scope_stack->indent >= indent)) {
	scope_stack = scope_stack->prev;
    }
}

static void baseclasses (char *classname, char *rest)
{
    /* We enter here with something like " ( Tester ):" */
    char *cp = rest;
    char *base;

    while (base = strtok (cp, "():, \t\r\n")) {
	if (strchr (base, '.'))
	    base = strrchr (base, '.') + 1;
	cp = NULL;
	/* I don't really know why we need a filename here, the
	   base class may be defined in a totally different file */
	sn_insert_symbol(SN_CLASS_INHERIT, classname, base,
			 sn_current_file(),
			 sn_line(), sn_column(), sn_line(), 
			 sn_column() + strlen(base),
			 0, NULL, NULL, NULL, NULL,
			 sn_line(), sn_column(), sn_line(),
			 sn_column() + strlen(base));
    }
}

static void var_or_const (int code)
{
    char *p;
    /* point just before the '=', then go backwards to find and mark
       the end of identifier */
    p = yytext + yyleng - 1;
    while ((p > yytext) && (p[-1] == ' ') || (p[-1] == '\t'))
        --p;
    *p = '\0';
    sn_insert_symbol(code, NULL, yytext, sn_current_file(),
		     sn_line(), sn_column(), sn_line(), 
		     sn_column() + strlen(yytext),
		     0, NULL, NULL, NULL, NULL,
		     sn_line(), sn_column(), sn_line(),
		     sn_column() + strlen(yytext));
}

static void parse_triple(int term)
{
    int c;
    int count;
    while (1) {
	c = input();
	sn_advance_column (1);
	if (c == term) {
	    ++count;
	    if (count == 3)
		return;
	} else {
	    count = 0;
	    switch (c) {
	    case '\\':
		c = input();
		if (c <= 0)
		    return;
		if (c != '\n') {
		    sn_advance_column (1);
		    c = -1;
		} else {
		    sn_advance_line ();
		}
		break;

	    case '\n':
		sn_advance_line ();
		sn_reset_column ();
		break;

	    case '\r':
		sn_reset_column ();
		break;

	    case EOF:
		return;
	    }
	}
    }
}

static char *mystrdup(char *name)
{
    char *p = malloc(strlen (name) + 1);
    if (!p)
	return NULL;
    strcpy (p, name);
    return p;
}

void reset()
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
