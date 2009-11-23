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

/* $Id$ */

#include "Parser.h"

#ifdef __cplusplus
extern "C" {
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#include <string.h>
}
#else
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#endif

#ifdef lex_interface
#define GetToken	yylex
     extern int yylex ARGS ((void));
#ifndef AttributeDef
#include "Position.h"
	   typedef struct { tPosition Position; } tScanAttribute;
	   tScanAttribute	Attribute = {{ 0, 0 }};
#endif
#ifndef ErrorAttributeDef
#define ErrorAttribute(Token, RepairAttribute)
#endif
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) * yyAttrStackPtr = yylval
#endif
#else
#include "Scanner.h"
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) (yyAttrStackPtr)->Scan = a
#endif
#endif

typedef unsigned short	yyStateRange	;
typedef unsigned short	yySymbolRange	;
typedef struct { yyStateRange Check, Next; } yytComb;
typedef enum {
yyNT0_intern	= 16,
yyNTprogram	= 17,
yyNTstatements	= 18,
yyNTstatement	= 19,
yyNTwords	= 20,
yyNTword	= 21,
yyNTqualification	= 22,
yyNTlocal_qualification	= 23,
yyNTglobal_qualification	= 24,
yyNTfragments	= 25,
yyNTfragment	= 26,
yyNTvariable	= 27,
yyNTsimple_local_qual	= 28,
yyNTsimple_global_qual	= 29,
yyNTend	= 30
} yytNonterminal;
typedef struct { short yyMode; rbool yyActions, yyMessages; } yytControl;

static	yytControl	yyControl	= { 0, rtrue, rtrue };
	rbool		Parser_Debug	= rfalse;

#define yyFirstTerminal	0
#define yyLastTerminal	15
#define yySetSize	16
#define yyFirstSymbol	0
#define yyLastSymbol	30
#define yyTTableMax	301
#define yyNTableMax	59
#define yyStartState	1
#define yyFirstReadState	1
#define yyLastReadState	42
#define yyFirstReadReduceState	43
#define yyLastReadReduceState	64
#define yyFirstReduceState	65
#define yyLastReduceState	127
#define yyLastState	127
#define yyLastStopState	65
#define YYTDefault
#define YYNDefault

#define yyFirstFinalState	yyFirstReadReduceState

#define ErrorMessages(Messages) yyControl.yyMessages = Messages
#define SemActions(Actions)	 yyControl.yyActions = Actions

#ifdef YYGetLook

#define GetLookahead(k)	yyGetLookahead ((k) - 1, yyTerminal)
#define GetAttribute(k, a)	xxGetAttribute ((k) - 1, yyTerminal, a)

static int yyGetLookahead	ARGS ((int yyk, yySymbolRange yyToken));
static void xxGetAttribute	ARGS ((int yyk, yySymbolRange yyToken,
				tScanAttribute * yyAttribute));

#endif

/* line 16 "Parser.lrk" */


#include <stdlib.h>
#include "Tree.h"
#include "Position.h"
#include "Eval.h"
#include "Trafo.h"
#include "itcl.h"

#define fprintf(x,y)

extern	FILE *	cross_ref_fp	;

	int	tcl80, itcl, itcl15;
	FILE *	yyin		;
	FILE *	hlfp		;
	int	hl		;
	char *	current_file	;

static	rbool	is_initialized	= rfalse;
static	rbool	report		= rfalse;
static	rbool	graphic		= rfalse;
static	tIdent	i_lparent, i_rparent;
static	int	dialect		;

static void get_options ARGS ((void))
{
   char * arg = getenv ("PAF_TCL");

   if (arg == NULL) return;

   while (* arg) {
      if (* arg == '-') {
	 switch (* ++ arg) {
	 case 'f': report = rtrue; break;
	 case 'g': graphic = rtrue; break;
	 case 'h':
	    (void) printf ("\nsyntax of PAF_TCL:\n\n");
	    (void) printf ("   [-f] [-h]\n\n");
	    (void) printf (" f : report error messages (default: do not report)\n");
	    (void) printf (" h : help\n\n");
	    break;
	 }
      }
      arg ++;
   }
}

void start_parser (fname, parse_cplpl, highl_fp, highlight)
   char	*	fname;
   int		parse_cplpl;
   FILE	*	highl_fp;
   int		highlight;
{
   current_file = fname;
   hlfp         = highl_fp;
   hl           = highlight;
   tcl80 = itcl = itcl15 = 0;

   if (! is_initialized) { get_options (); is_initialized = rtrue; }
   StoreMessages	(rtrue);
   InitStringMemory	();
   InitIdents		();
   if (fname) Attribute.Position.FileName = MakeIdent (fname, strlen (fname));
   else       Attribute.Position.FileName = NoIdent;
   BeginFile		("");
   Beginitcl		();
   BeginTrafo		();
   (void) Parser	();
   /* CheckTree		(TreeRoot); */
   Eval			(TreeRoot);
   itcl += itcl15;
   if (itcl > 0 && tcl80 > 0)
   			{ Interpret_tcl (TreeRoot); fprintf (stderr, "???\n"); }
   else if (tcl80 > 0)	{ Interpret_tcl (TreeRoot); fprintf (stderr, "tcl\n"); }
   else if (itcl > 0) {
        if (itcl15 > 0) { Interpret_itcl (TreeRoot); fprintf (stderr, "itcl15\n"); }
	else		{ Interpret_itcl (TreeRoot); fprintf (stderr, "itcl22\n"); }
   } else		{ Interpret_tcl (TreeRoot); fprintf (stderr, "zero\n"); }
#ifdef DRAWTREE
   if (graphic) DrawTree (TreeRoot);
#endif
   if (report) WriteMessages (stderr);
   CloseStringMemory	();
   ReleaseTreeModule	();
}

#define SHARE

#ifdef DRAWTREE
#undef SHARE
#endif

#ifdef SHARE

static tTree		snostmt, snoword, snotext;

#define dnostmt	snostmt
#define dnoword	snoword
#define dnotext	snotext

#else

#define dnostmt	mnostmt ()
#define dnoword	mnoword ()
#define dnotext	mnotext ()

#endif

typedef struct { tTree tree; } zzstatements;
typedef struct { tTree tree; } zzstatement;
typedef struct { tTree tree; } zzwords;
typedef struct { tTree tree; } zzword;
typedef struct { tTree tree; } zzqualification;
typedef struct { tTree tree; } zzlocal_qualification;
typedef struct { tTree tree; } zzglobal_qualification;
typedef struct { tTree tree; rbool is_simple; } zzfragments;
typedef struct { tTree tree; rbool is_simple; } zzfragment;
typedef struct { tTree tree; } zzvariable;
typedef struct { tTree tree; } zzsimple_local_qual;
typedef struct { tTree tree; } zzsimple_global_qual;

typedef union {
tScanAttribute Scan;
zzstatements statements;
zzstatement statement;
zzwords words;
zzword word;
zzqualification qualification;
zzlocal_qualification local_qualification;
zzglobal_qualification global_qualification;
zzfragments fragments;
zzfragment fragment;
zzvariable variable;
zzsimple_local_qual simple_local_qual;
zzsimple_global_qual simple_global_qual;
} tParsAttribute;


#if defined lex_interface & ! defined yylvalDef
     tParsAttribute yylval;
#endif

#ifndef yyInitStackSize
#define yyInitStackSize	100
#endif
#ifndef MY_ERROR
#define MY_ERROR
#endif
#define yyNoState		0
#define yystandard		1
#define yytrial		2
#define yybuffer		4
#define yyreparse		8
#define yyS			yySynAttribute
#define yyA			yyAttrStackPtr
#define YYACCEPT		goto yyAccept
#define YYABORT		goto yyAbort

#ifdef YYDEC_TABLE
#define yyDecrement(x)
#define yySetNT(x)
#else
#define yyDecrement(x)		yyStateStackPtr -= x; yyAttrStackPtr -= x;
#define yySetNT(x)		yyNonterminal = x;
#endif

#ifdef YYNDefault
#define yytNComb yytComb
#else
#define yytNComb yyStateRange
#endif

#if defined YYDEBUG | defined YYDCRP
static	long		yyCount		= 0;
static	FILE *		yyTrace		;

static	void		yyPrintState	ARGS ((yyStateRange yyState));
static	void		yyNl		ARGS ((void));

static	char *		yyRule		[] = { 0,
""
};
#endif
	char *		Parser_TokenName	[yyLastTerminal + 2] = {
"_EOF_",
"ident",
"character",
"$",
"\"(",
"\")",
"[",
"]",
"{",
"}",
"::",
"(",
")",
"space",
"EOF",
"end_of_command",
""
};
static	yytComb		yyTComb		[yyTTableMax + 1] = {
{   9,   65}, {  20,  114}, {  20,  114}, {  20,  114}, {  20,  114}, 
{  10,   67}, {  20,  114}, {  10,   67}, {  20,  114}, {  10,   67}, 
{  20,   35}, {  20,   36}, {  20,  114}, {  20,  114}, {  20,  114}, 
{  20,  114}, {  39,   80}, {  39,   80}, {  39,   80}, {  39,   80}, 
{  21,   51}, {  39,   80}, {  22,   52}, {  39,   80}, {  32,   54}, 
{  39,   80}, {  39,   80}, {  39,   45}, {  39,   80}, {  39,   80}, 
{  39,   80}, {  19,  110}, {  19,  110}, {  19,  110}, {  19,  110}, 
{  23,   53}, {  19,  110}, {   0,    0}, {  19,  110}, {   0,    0}, 
{  19,   33}, {  19,   34}, {  19,  110}, {  19,  110}, {  19,  110}, 
{  19,  110}, {  18,   59}, {  18,  109}, {  18,  109}, {  18,  109}, 
{   0,    0}, {  18,  109}, {   0,    0}, {  18,  109}, {   0,    0}, 
{  18,  109}, {  18,  109}, {  18,  109}, {  18,  109}, {  18,  109}, 
{  18,  109}, {  33,   58}, {  33,  120}, {  33,  120}, {  33,  120}, 
{   0,    0}, {  33,  120}, {   0,    0}, {  33,  120}, {   0,    0}, 
{  33,  120}, {  33,  120}, {  33,  120}, {  33,  120}, {  33,  120}, 
{  33,  120}, {  35,   60}, {  35,  123}, {  35,  123}, {  35,  123}, 
{   0,    0}, {  35,  123}, {   0,    0}, {  35,  123}, {   0,    0}, 
{  35,  123}, {  35,  123}, {  35,  123}, {  35,  123}, {  35,  123}, 
{  35,  123}, {  25,   91}, {  25,   91}, {  25,   91}, {  25,   91}, 
{   0,    0}, {  25,   91}, {   0,    0}, {  25,   91}, {   0,    0}, 
{  25,   91}, {  25,   91}, {  25,   48}, {  25,   91}, {  25,   91}, 
{  25,   91}, {  14,   82}, {  14,   82}, {  14,   82}, {  14,   82}, 
{   0,    0}, {  14,   82}, {   0,    0}, {  14,   82}, {   0,    0}, 
{  14,   30}, {  14,   31}, {  14,   82}, {  14,   82}, {  14,   82}, 
{  14,   82}, {  13,   78}, {  13,   78}, {  13,   78}, {  13,   78}, 
{   0,    0}, {  13,   78}, {   0,    0}, {  13,   78}, {   0,    0}, 
{  13,   28}, {  13,   29}, {  13,   78}, {  13,   78}, {  13,   78}, 
{  13,   78}, {   2,   57}, {   2,  101}, {   2,  101}, {   2,  101}, 
{   0,    0}, {   2,  101}, {   0,    0}, {   2,   17}, {   0,    0}, 
{   2,   18}, {   2,  101}, {   2,  101}, {   2,  101}, {   2,  101}, 
{   2,  101}, {  37,   88}, {  37,   88}, {  37,   88}, {  37,   88}, 
{   0,    0}, {  37,   88}, {   0,    0}, {  37,   88}, {   0,    0}, 
{  37,   88}, {  37,   88}, {  37,   47}, {  37,   88}, {  37,   88}, 
{  37,   88}, {  42,  116}, {  42,  116}, {  42,  116}, {  42,  116}, 
{   0,    0}, {  42,  116}, {   0,    0}, {  42,  116}, {   0,    0}, 
{  42,  116}, {  42,  116}, {  42,   56}, {  42,  116}, {  42,  116}, 
{  42,  116}, {  41,  112}, {  41,  112}, {  41,  112}, {  41,  112}, 
{   0,    0}, {  41,  112}, {   0,    0}, {  41,  112}, {   0,    0}, 
{  41,  112}, {  41,  112}, {  41,   55}, {  41,  112}, {  41,  112}, 
{  41,  112}, {  40,   84}, {  40,   84}, {  40,   84}, {  40,   84}, 
{   0,    0}, {  40,   84}, {   0,    0}, {  40,   84}, {   0,    0}, 
{  40,   84}, {  40,   84}, {  40,   46}, {  40,   84}, {  40,   84}, 
{  40,   84}, {   8,   50}, {   8,   49}, {   8,    2}, {   8,    3}, 
{   0,    0}, {   8,    4}, {   0,    0}, {   8,    5}, {   0,    0}, 
{   8,    6}, {   8,    7}, {   8,   43}, {   0,    0}, {   8,   62}, 
{   8,   64}, {  30,   98}, {  30,   98}, {  30,   98}, {  30,   98}, 
{  30,   98}, {  30,   98}, {  15,   99}, {  15,   99}, {  15,   99}, 
{  15,   99}, {  15,   99}, {  15,   99}, {  28,   95}, {  28,   95}, 
{  28,   95}, {  28,   95}, {  28,   95}, {  28,   95}, {   6,   86}, 
{   6,   24}, {   6,   86}, {   6,   86}, {   6,   86}, {   6,   86}, 
{   7,   90}, {   7,   90}, {   7,   90}, {   7,   90}, {  36,  115}, 
{  36,  115}, {  36,  115}, {  36,  115}, {   4,   67}, {  24,   87}, 
{  24,   87}, {  24,   87}, {  24,   87}, {   3,   67}, {   4,    8}, 
{   4,   61}, {   4,   63}, {  34,  111}, {  34,  111}, {  34,  111}, 
{  34,  111}, {   3,    8}, {   3,   61}, {   3,   63}, {  31,   83}, 
{  31,   83}, {  31,   83}, {  31,   83}, {  17,   67}, {   1,   67}, 
{   0,    0}, {   0,    0}, {  17,    8}, {  17,   61}, {  17,   63}, 
{  38,   44}, {  38,   73}, {  38,   73}, {  38,   73}, {  12,   72}, 
{  12,   72}, {  12,   72}, {   1,    8}, {   1,   61}, {   1,   63}, 
{  29,   79}, {  29,   79}, {  29,   79}, {  29,   79}, {  26,   27}, 
{  26,   61}, {  26,   63}, 
};
static	yytNComb	yyNComb		[yyNTableMax - yyLastTerminal] = {
{   0,    0}, {   0,    0}, {   5,   23}, {   5,   10}, {   5,   11}, 
{  10,   68}, {  12,   76}, {  12,   13}, {  12,   14}, {  12,   93}, 
{  17,   32}, {   2,  108}, {   2,   19}, {   2,   20}, {   5,   16}, 
{   1,    9}, {   1,   66}, {   6,   96}, {   6,   15}, {  27,   38}, 
{  27,   74}, {   8,   26}, {   8,   12}, {  29,   39}, {   4,   22}, 
{  31,   40}, {   3,   21}, {  34,   41}, {  24,   37}, {   7,   25}, 
{  36,   42}, {  16,   69}, {  26,   71}, {  28,   94}, {  15,  100}, 
{  30,   97}, {  11,   70}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytComb *	yyTBasePtr	[yyLastReadState + 1] = { 0,
& yyTComb [ 279], & yyTComb [ 135], & yyTComb [ 258], & yyTComb [ 251], 
& yyTComb [   0], & yyTComb [ 234], & yyTComb [ 238], & yyTComb [ 210], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [ 276], 
& yyTComb [ 120], & yyTComb [ 105], & yyTComb [ 222], & yyTComb [   0], 
& yyTComb [ 269], & yyTComb [  45], & yyTComb [  30], & yyTComb [   0], 
& yyTComb [  15], & yyTComb [  15], & yyTComb [  26], & yyTComb [ 247], 
& yyTComb [  90], & yyTComb [ 286], & yyTComb [   0], & yyTComb [ 228], 
& yyTComb [ 283], & yyTComb [ 216], & yyTComb [ 262], & yyTComb [  15], 
& yyTComb [  60], & yyTComb [ 255], & yyTComb [  75], & yyTComb [ 242], 
& yyTComb [ 150], & yyTComb [ 273], & yyTComb [  15], & yyTComb [ 195], 
& yyTComb [ 180], & yyTComb [ 165], 
};
static	yytNComb *	yyNBasePtr	[yyLastReadState + 1] = { 0,
& yyNComb [  -2], & yyNComb [ -16], & yyNComb [   8], & yyNComb [   6], 
& yyNComb [ -16], & yyNComb [  -8], & yyNComb [   7], & yyNComb [   1], 
& yyNComb [ -16], & yyNComb [ -13], & yyNComb [   6], & yyNComb [ -16], 
& yyNComb [ -16], & yyNComb [ -16], & yyNComb [   9], & yyNComb [  13], 
& yyNComb [  -8], & yyNComb [ -16], & yyNComb [ -16], & yyNComb [ -16], 
& yyNComb [ -16], & yyNComb [ -16], & yyNComb [ -16], & yyNComb [   6], 
& yyNComb [ -16], & yyNComb [   2], & yyNComb [  -2], & yyNComb [   8], 
& yyNComb [   1], & yyNComb [  10], & yyNComb [   3], & yyNComb [ -16], 
& yyNComb [ -16], & yyNComb [   5], & yyNComb [ -16], & yyNComb [   8], 
& yyNComb [ -16], & yyNComb [ -16], & yyNComb [ -16], & yyNComb [ -16], 
& yyNComb [ -16], & yyNComb [ -16], 
};
#ifdef YYTDefault
static	unsigned short	yyTDefault	[yyLastReadState + 1] = { 0,
    8,     0,     8,     8,    17,     7,    38,     0,     0,     1, 
   26,    38,     0,     0,     6,    10,     8,     0,     0,     0, 
    0,     0,     0,     7,     0,     0,     8,     6,     7,     6, 
    7,     0,     0,     7,     0,     7,     0,     8,     0,     0, 
    0,     0, 
};
#endif
#ifdef YYNDefault
static	unsigned short	yyNDefault	[yyLastReadState + 1] = { 0,
    5,     0,     5,     5,     8,     0,    29,    27,     0,     5, 
    0,     6,     0,     0,     6,     5,     5,     0,     0,     0, 
    0,     0,     0,    29,     0,     0,    29,     6,    12,     6, 
   29,     0,     0,    29,     0,    29,     0,    12,     0,     0, 
    0,     0, 
};
#endif
#if ! defined NO_RECOVER | defined YYDEC_TABLE
static	unsigned char	yyLength	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
    2,     1,     0,     2,     2,     2,     3,     1,     3,     1, 
    1,     2,     2,     1,     2,     3,     4,     1,     2,     3, 
    4,     1,     2,     3,     4,     1,     2,     3,     1,     3, 
    2,     2,     3,     2,     1,     2,     1,     1,     1,     3, 
    3,     3,     4,     2,     2,     1,     2,     3,     4,     1, 
    2,     3,     4,     1,     3,     2,     2,     3,     2,     1, 
    2,     1,     2, 
};
static	yytNonterminal	yyLeftHandSide	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
yyNT0_intern,
yyNTprogram,
yyNTstatements,
yyNTstatements,
yyNTstatements,
yyNTstatement,
yyNTstatement,
yyNTwords,
yyNTwords,
yyNTword,
yyNTword,
yyNTword,
yyNTword,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTqualification,
yyNTlocal_qualification,
yyNTlocal_qualification,
yyNTlocal_qualification,
yyNTglobal_qualification,
yyNTglobal_qualification,
yyNTglobal_qualification,
yyNTfragments,
yyNTfragments,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTfragment,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTvariable,
yyNTsimple_local_qual,
yyNTsimple_local_qual,
yyNTsimple_local_qual,
yyNTsimple_global_qual,
yyNTsimple_global_qual,
yyNTsimple_global_qual,
yyNTend,
yyNTend,
yyNTend,
yyNTend,
};
#endif
#ifndef NO_RECOVER
static	yySymbolRange	yyContinuation	[yyLastReadState + 1] = { 0,
    0,     2,     5,     7,     9,    10,    12,    14,     0,     0, 
   14,    13,     1,     1,    10,     0,     9,     2,     1,     1, 
    5,     7,     9,    12,     1,    14,     3,    10,    12,    10, 
   12,     9,     2,    12,     2,    12,     1,    13,     1,     1, 
    1,     1, 
};
static	unsigned short	yyCondition	[yyLastState - yyLastReduceState + 1] =
{ 0,
};
#endif
static	unsigned short	yyFinalToProd	[yyLastReadReduceState -
						yyFirstReadReduceState + 2] = {
   75,    77,    81,    85,    89,    92,   102,   103,   104,   105, 
  106,   107,   113,   117,   118,   119,   121,   122,   124,   125, 
  126,   127, 
0
};
static	unsigned short	yyStartLine	[yyLastStopState - yyFirstReduceState
							+ 2] = { 0,
198,
};
#ifdef YYaccDefault

static	unsigned long *	yyDefaultLook	[yyLastReadState + 1] = { 0,
};
#endif

static	int	yyParse			ARGS ((yyStateRange yyStartSymbol,
				yySymbolRange yyToken, int yyLine));

#ifndef NO_RECOVER
static	yyStateRange yyNext		ARGS ((yyStateRange yyState,
				yySymbolRange yySymbol));
static	void	yyErrorRecovery		ARGS ((yySymbolRange * yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
static	void	yyComputeContinuation	ARGS ((yyStateRange * yyStack,
				short yyStackPtr, tSet * yyContinueSet, rbool));
static	rbool	yyIsContinuation	ARGS ((yySymbolRange yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr,
				rbool));
static	void	yyComputeRestartPoints	ARGS ((yyStateRange * yyStateStack,
				short yyStackPtr, tSet * yyRestartSet));
#endif

#if defined YYTrialParse | defined YYReParse | defined YYGetLook

#ifndef yyInitBufferSize
#define yyInitBufferSize 100
#endif
#ifndef TOKENOP
#define TOKENOP
#endif
#ifndef BEFORE_TRIAL
#define BEFORE_TRIAL
#endif
#ifndef AFTER_TRIAL
#define AFTER_TRIAL
#endif

typedef struct { yySymbolRange	yyToken;
		 tScanAttribute	yyAttribute;
#ifdef YYMemoParse
		 short		yyStart;
#endif
	       } yytBuffer;

static yytBuffer *	yyBuffer	;
static unsigned long	yyBufferSize	= yyInitBufferSize;
static long		yyBufferNext	= 1;
static long		yyBufferLast	= 1;
static rbool		yyBufferClear	= rtrue;
static unsigned short	yyParseLevel	= 0;

static void yyBufferSet
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyToken)
#else
   (yyToken) yySymbolRange yyToken;
#endif
{
   if (yyBufferNext == yyBufferLast) {
      if (yyBufferClear) yyBufferLast = 0;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken	= yyToken;
      yyBuffer [yyBufferLast].yyAttribute= Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart	= 0;
#endif
      yyBufferNext = yyBufferLast;
   }
}

static int yyGetToken ARGS ((void))
{
   register yySymbolRange yyToken;

   if (yyBufferNext < yyBufferLast) {
      yyToken = yyBuffer [++ yyBufferNext].yyToken;
      Attribute = yyBuffer [yyBufferNext].yyAttribute;
   } else {
      yyToken = GetToken ();
      if ((yytrial | yybuffer) & yyControl.yyMode) {
	 if (++ yyBufferLast >= (long) yyBufferSize) {
	    ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			     (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (0);
	       (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		  yyBufferSize / 2, yyBufferSize); yyNl ();
	    }
#endif
	 }
	 yyBuffer [yyBufferLast].yyToken = yyToken;
	 yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
	 yyBuffer [yyBufferLast].yyStart = 0;
#endif
	 yyBufferNext = yyBufferLast;
      }
   }
   TOKENOP
   return yyToken;
}

#else
#define yyGetToken GetToken
#endif

#ifdef YYGetLook

static int yyGetLookahead
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken)
#else
   (yyk, yyToken) int yyk; yySymbolRange yyToken;
#endif
{
   if (yyk == 0) return yyToken;
   if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
   while (yyBufferNext + yyk > yyBufferLast) {
      if (yyBuffer [yyBufferLast].yyToken == EofToken) return EofToken;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
	       yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken = GetToken ();
      yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart = 0;
#endif
   }
   Attribute = yyBuffer [yyBufferNext].yyAttribute;
   return yyBuffer [yyBufferNext + yyk].yyToken;
}

static void xxGetAttribute
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken, tScanAttribute * yyAttribute)
#else
   (yyk, yyToken, yyAttribute)
   int yyk; yySymbolRange yyToken; tScanAttribute * yyAttribute;
#endif
{
   if (yyk == 0) * yyAttribute = Attribute;
   else {
      (void) yyGetLookahead (yyk, yyToken);
      * yyAttribute =
	 yyBuffer [Min (yyBufferNext + yyk, yyBufferLast)].yyAttribute;
   }
}

#endif

#ifdef YYReParse

#define BufferOn(Actions, Messages) yyBufferOn (Actions, Messages, yyTerminal)
#define BufferPosition	yyBufferNext

static yytControl yyPrevControl;

static long yyBufferOn
#if defined __STDC__ | defined __cplusplus
   (rbool yyActions, rbool yyMessages, yySymbolRange yyToken)
#else
   (yyActions, yyMessages, yyToken)
   rbool yyActions, yyMessages; yySymbolRange yyToken;
#endif
{
   if (yyControl.yyMode == yystandard) {
      yyPrevControl		= yyControl;
      yyControl.yyMode		= yybuffer;
      yyControl.yyActions	= yyActions;
      yyControl.yyMessages	= yyMessages;
      yyBufferSet (yyToken);
      yyBufferClear		= rfalse;
   }
   return yyBufferNext;
}

static long BufferOff ARGS ((void))
{
   if (yyControl.yyMode == yybuffer) yyControl = yyPrevControl;
   return yyBufferNext;
}

static void BufferClear ARGS ((void))
{
   yyBufferClear = rtrue;
}

#endif

#if defined YYDEBUG | defined YYDCRP

static void yyNl ARGS ((void))
{ (void) putc ('\n', yyTrace); (void) fflush (yyTrace); }

#endif

#ifdef YYDEBUG

static void yyPrintState
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState)
#else
   (yyState) yyStateRange yyState;
#endif
{
   (void) fprintf (yyTrace, "%4ld:", ++ yyCount);
   WritePosition  (yyTrace, Attribute.Position);
   (void) fprintf (yyTrace, ":%5d  %c  ", yyState,
      " ST-B---R" [yyControl.yyMode]);
#if defined YYTrialParse | defined YYReParse
   if (yyParseLevel > 0) {
      register int yyi = yyParseLevel;
      (void) fprintf (yyTrace, "%2d  ", yyi);
      do (void) fputs ("  ", yyTrace); while (-- yyi);
   } else
#endif
   (void) fputs ("    ", yyTrace);
}

static rbool yyPrintResult
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, int yyLine, rbool yyCondition)
#else
   (yyState, yyLine, yyCondition)
   yyStateRange	yyState;
   int		yyLine;
   rbool	yyCondition;
#endif
{
   if (Parser_Debug) {
      yyPrintState (yyState);
      (void) fprintf (yyTrace, "check   predicate in line %d, result = %d",
	 yyLine, yyCondition); yyNl ();
   }
   return yyCondition;
}

#else
#define yyPrintResult(State, Line, Condition) Condition
#endif

#if defined YYDEBUG | defined YYDEC_TABLE
#define yyGotoReduce(State, Rule)	{ yyState = State; goto yyReduce; }
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#else
#define yyGotoReduce(State, Rule)	goto Rule;
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#endif

static unsigned long	yyStateStackSize	= yyInitStackSize;
static yyStateRange *	yyStateStack		;
static yyStateRange *	yyEndOfStack		;
static unsigned long	yyAttrStackSize 	= yyInitStackSize;
static tParsAttribute * yyAttributeStack	;
#if defined YYTrialParse | defined YYReParse
static yyStateRange *	yyStateStackPtr 	;
static tParsAttribute * yyAttrStackPtr		;
#endif
static yyStateRange *	yyIsContStackPtr	;
static unsigned long	yyIsContStackSize	= yyInitStackSize;
static yyStateRange *	yyCompResStackPtr	;
static unsigned long	yyCompResStackSize	= yyInitStackSize;

int Parser ARGS ((void))
   {
      return Parser2 (yyStartState);
   }

int Parser2
#if defined __STDC__ | defined __cplusplus
   (int yyStartSymbol)
#else
   (yyStartSymbol) int yyStartSymbol;
#endif
   {
      int		yyErrorCount;
#if defined YYDEBUG | defined YYDCRP
      yyTrace		= stdout;
#endif
      BeginParser ();
      MakeArray ((char * *) & yyStateStack, & yyStateStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
		     (unsigned long) sizeof (tParsAttribute));
      MakeArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
		     (unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      MakeArray ((char * *) & yyBuffer, & yyBufferSize,
		     (unsigned long) sizeof (yytBuffer));
#endif
      yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#if defined YYTrialParse | defined YYReParse
      yyStateStackPtr	= yyStateStack;
      yyAttrStackPtr	= yyAttributeStack;
      yyBufferNext	= 1;
      yyBufferLast	= 1;
      yyParseLevel	= 0;
#endif
#ifdef YYDEBUG
      if (Parser_Debug) {
	 (void) fprintf (yyTrace,
      "  #|Position|State|Mod|Lev|Action |Terminal and Lookahead or Rule\n");
	 yyNl ();
      }
#endif
      yyControl.yyMode		= yystandard;
      yyControl.yyActions	= rtrue;
      yyControl.yyMessages	= rtrue;
      yyErrorCount = yyParse ((yyStateRange) yyStartSymbol,
	 (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
      ReleaseArray ((char * *) & yyStateStack, & yyStateStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			(unsigned long) sizeof (tParsAttribute));
      ReleaseArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			(unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      ReleaseArray ((char * *) & yyBuffer, & yyBufferSize,
			(unsigned long) sizeof (yytBuffer));
#endif
      return yyErrorCount;
   }

#ifdef YYTrialParse

#ifdef YYMemoParse
#define MemoryClear(Position) yyBuffer [Position].yyStart = 0
#endif

static int yyTrialParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      int	yyErrorCount		;
      unsigned long yyPrevStateStackPtr	= yyStateStackPtr - yyStateStack;
      unsigned long yyPrevAttrStackPtr	= yyAttrStackPtr - yyAttributeStack;
      long	yyPrevBufferNext	;
      yytControl yyPrevControl		;

      BEFORE_TRIAL
#ifdef YYMemoParse
      if (yyBuffer [yyBufferNext].yyStart ==   yyStartSymbol) return 0;
      if (yyBuffer [yyBufferNext].yyStart == - yyStartSymbol) return 1;
#endif
      yyPrevControl		= yyControl;
      yyStateStackPtr		++;
      yyAttrStackPtr		++;
      yyParseLevel		++;
      if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
      yyPrevBufferNext		= yyBufferNext;
      yyControl.yyMode		= yytrial;
      yyControl.yyActions	= rfalse;
      yyControl.yyMessages	= rfalse;
      yyErrorCount		= yyParse (yyStartSymbol, yyToken, yyLine);
#ifdef YYMemoParse
      yyBuffer [yyPrevBufferNext].yyStart = yyErrorCount ?
					- yyStartSymbol : yyStartSymbol;
#endif
      yyStateStackPtr		= yyStateStack + yyPrevStateStackPtr;
      yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
      yyBufferNext		= yyPrevBufferNext;
      yyControl			= yyPrevControl;
      yyParseLevel		--;
      Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      AFTER_TRIAL
      return yyErrorCount;
   }

#endif

#ifdef YYReParse

static int ReParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, int yyFrom, int yyTo, rbool yyActions,
      rbool yyMessages)
#else
   (yyStartSymbol, yyFrom, yyTo, yyActions, yyMessages)
   yyStateRange	yyStartSymbol		;
   int		yyFrom, yyTo		;
   rbool	yyActions, yyMessages	;
#endif
   {
      int yyErrorCount = 1;

      if (1 <= yyFrom && yyFrom <= yyTo && yyTo <= yyBufferLast) {
	 unsigned long yyPrevStateStackPtr = yyStateStackPtr - yyStateStack;
	 unsigned long yyPrevAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	 long	yyPrevBufferNext	= yyBufferNext;
	 int	yyToToken		= yyBuffer [yyTo].yyToken;
	 yytControl yyPrevControl	;

	 yyPrevControl		= yyControl;
	 yyStateStackPtr	++;
	 yyAttrStackPtr		++;
	 yyParseLevel		++;
	 yyBufferNext		= yyFrom - 1;
	 yyBuffer [yyTo].yyToken= EofToken;
	 yyControl.yyMode	= yyreparse;
	 yyControl.yyActions	= yyActions;
	 yyControl.yyMessages	= yyMessages;
	 yyErrorCount		= yyParse (yyStartSymbol,
	    (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
	 yyStateStackPtr	= yyStateStack + yyPrevStateStackPtr;
	 yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
	 yyBufferNext		= yyPrevBufferNext;
	 yyControl		= yyPrevControl;
	 yyParseLevel		--;
	 yyBuffer [yyTo].yyToken= yyToToken;
	 Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      } else {
      Message ("invalid call of ReParse", xxError, Attribute.Position);
      }
      return yyErrorCount;
   }

#endif

static char * yyGetTokenName
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyTerminal)
#else
   (yyTerminal) yySymbolRange yyTerminal;
#endif
   {
      if (yyTerminal <= yyLastTerminal && Parser_TokenName [yyTerminal])
         return Parser_TokenName [yyTerminal];
      else
         return "_unknown_";
   }

static int yyParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      register	yyStateRange	yyState		= yyStartSymbol;
      register	yySymbolRange	yyTerminal	= yyToken;
      register	rbool		yyIsRepairing	= rfalse;
		tParsAttribute	yySynAttribute	;   /* synthesized attribute */
		int		yyErrorCount	= 0;
#if ! (defined YYTrialParse | defined YYReParse)
      register	yyStateRange *	yyStateStackPtr	= yyStateStack;
      register	tParsAttribute *yyAttrStackPtr	= yyAttributeStack;
#endif
#ifdef YYDEBUG
		long		yyStartCount	= yyCount + 1;
		yySymbolRange	yyPrevTerminal	= yyToken;
#endif
#ifdef YYGetLook
		yySymbolRange	yy2;
#endif

/* line 155 "Parser.lrk" */



#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (yyStartSymbol);
	 (void) fprintf (yyTrace,
	    "parse   for predicate in line %d, lookahead: %s", yyLine,
	    yyGetTokenName (yyTerminal)); yyNl ();
      }
#endif

   yyParseLoop:
      for (;;) {
	 if (yyStateStackPtr >= yyEndOfStack) {
	    unsigned long yyyStateStackPtr = yyStateStackPtr - yyStateStack;
	    unsigned long yyyAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	    ExtendArray ((char * *) & yyStateStack, & yyStateStackSize,
			     (unsigned long) sizeof (yyStateRange));
	    ExtendArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			     (unsigned long) sizeof (tParsAttribute));
	    yyStateStackPtr	= yyStateStack + yyyStateStackPtr;
	    yyAttrStackPtr	= yyAttributeStack + yyyAttrStackPtr;
	    yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyState);
	       (void) fprintf (yyTrace, "extend  stack from %ld to %ld",
		  yyStateStackSize / 2, yyStateStackSize); yyNl ();
	    }
#endif
	 }
	 * yyStateStackPtr = yyState;

   yyTermTrans:
	 for (;;) { /* SPEC State = Next (State, Terminal); terminal transit */
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yyTerminal;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) {
	       yyState = yyTCombPtr->Next; break;
	    }
#ifdef YYTDefault
#ifdef YYaccDefault
	    if ((yylp = yyDefaultLook [yyState]) &&
	       (yylp [yyTerminal >> 5] >> (yyTerminal & 0x1f)) & 1) {
	       yyState = yyTDefault [yyState]; break;
	    }
#else
	    if ((yyState = yyTDefault [yyState]) != yyNoState) goto yyTermTrans;
#endif
#endif

							/* syntax error */
	    if (! yyIsRepairing) {			/* report and recover */
	       yySymbolRange yyyTerminal = (yySymbolRange) yyTerminal;

#ifdef YYTrialParse
	       if (yyControl.yyMode == yytrial) YYABORT;
#endif
	       MY_ERROR
#ifndef NO_RECOVER
	       yyErrorCount ++;
	       yyErrorRecovery (& yyyTerminal, yyStateStack,
				yyStateStackPtr - yyStateStack);
	       yyTerminal = yyyTerminal;
	       yyIsRepairing = rtrue;
#else
	       YYABORT;
#endif
	    }
#ifndef NO_RECOVER
	    yyState = * yyStateStackPtr;
	    for (;;) {
	       yySymbolRange	yyRepairToken;
	       tScanAttribute	yyRepairAttribute;
	       yyStateRange	yyNextState =
	          yyNext (yyState, (yySymbolRange) yyTerminal);
	       if (yyNextState != yyNoState &&	/* read or read reduce ? */
		   yyNextState <= yyLastReadReduceState) {
		  yyState = yyNextState;	/* restart point reached */
		  yyIsRepairing = rfalse;	/* stop error recovery */
		  goto yyFinal;
	       }
	       yyRepairToken = yyContinuation [yyState];	/* repair */
	       yyState = yyNext (yyState, yyRepairToken);
	       if (yyState > yyLastReduceState) {		/* dynamic ? */
		  yyState = yyCondition [yyState - yyLastReduceState];
	       }
	       if (yyState <= yyLastReadReduceState) {
						/* read or read reduce ? */
		  ErrorAttribute ((int) yyRepairToken,
					& yyRepairAttribute);
		  if (yyControl.yyMessages)
		     ErrorMessageI (xxTokenInserted, xxRepair,
			Attribute.Position, xxString,
			yyGetTokenName (yyRepairToken));
#ifdef YYDEBUG
		  if (Parser_Debug) {
		     yyPrintState (* yyStateStackPtr);
		     (void) fprintf (yyTrace, "insert  %s",
			yyGetTokenName (yyRepairToken)); yyNl ();
		     yyPrintState (* yyStateStackPtr);
		     (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
			yyGetTokenName (yyRepairToken),
			yyGetTokenName (yyTerminal)); yyNl ();
		  }
#endif
		  if (yyState >= yyFirstFinalState) { /* avoid second push */
		     yyState =
			yyFinalToProd [yyState - yyFirstReadReduceState];
		  }
		  yyGetAttribute (yyAttrStackPtr ++, yyRepairAttribute);
		  * ++ yyStateStackPtr = yyState;
	       }
	       if (yyState >= yyFirstFinalState) goto yyFinal;
							/* final state ? */
	    }
#endif
	 }

   yyFinal:
	 if (yyState >= yyFirstFinalState) {		/* final state ? */
	    if (yyState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStateStackPtr ++;
	       yyGetAttribute (yyAttrStackPtr ++, Attribute);
	       yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  yyStateStackPtr [0] = yyStateStackPtr [-1];
		  yyPrintState (* yyStateStackPtr);
		  (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		     yyGetTokenName (yyPrevTerminal),
		     yyGetTokenName (yyTerminal)); yyNl ();
		  yyPrevTerminal = yyTerminal;
	       }
#endif
	       yyIsRepairing = rfalse;
	    }

	    for (;;) {
	       register yytNonterminal yyNonterminal;	/* left-hand side */

   yyReduce:
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  if (yyState <= yyLastReadReduceState)	/* read reduce ? */
		     yyState = yyFinalToProd [yyState - yyFirstReadReduceState];
		  yyPrintState (* yyStateStackPtr);
		  if (yyState <= yyLastReduceState) {
		     (void) fprintf (yyTrace, "reduce  %s",
			yyRule [yyState - yyLastReadReduceState]); yyNl ();
		  } else {
		     (void) fprintf (yyTrace, "dynamic decision %d",
			yyState - yyLastReduceState); yyNl ();
		  }
	       }
#endif
#ifdef YYDEC_TABLE
	       if (yyLastStopState < yyState && yyState <= yyLastReduceState) {
		  register int yyd = yyLength [yyState - yyFirstReduceState];
		  yyStateStackPtr -= yyd;
		  yyAttrStackPtr  -= yyd;
		  yyNonterminal = yyLeftHandSide [yyState - yyFirstReduceState];
	       }
#endif
switch (yyState) {
case 65:
YYACCEPT;
case 66: yyDecrement (1) yySetNT (yyNTprogram) {
/* line 198 "Parser.lrk" */
 ;
{  TreeRoot = mprogram (yyA [0].statements.tree); } ;

} break;
case 67: yySetNT (yyNTstatements) {
/* line 201 "Parser.lrk" */
 yyS.statements.tree = dnostmt;
 ;

} break;
case 68: yyDecrement (2) yySetNT (yyNTstatements) {
/* line 204 "Parser.lrk" */
 yyS.statements.tree = Mstmt (yyA [1].statements.tree, yyA [0].statement.tree);
 ;

} break;
case 69: yyDecrement (2) yySetNT (yyNTstatements) {
/* line 207 "Parser.lrk" */
yyS.statements.tree = yyA [1].statements.tree;

} break;
case 70: yyDecrement (2) yySetNT (yyNTstatement) {
/* line 209 "Parser.lrk" */
 yyS.statement.tree = ReverseTree (yyA [0].words.tree);
 ;

} break;
case 71: yyDecrement (3) yySetNT (yyNTstatement) {
/* line 212 "Parser.lrk" */
 yyS.statement.tree = ReverseTree (yyA [1].words.tree);
 ;

} break;
case 72: yyDecrement (1) yySetNT (yyNTwords) {
/* line 215 "Parser.lrk" */
 yyS.words.tree = Mword (dnoword, yyA [0].word.tree);
 ;

} break;
case 73: yyDecrement (3) yySetNT (yyNTwords) {
/* line 218 "Parser.lrk" */
 yyS.words.tree = Mword (yyA [0].words.tree, yyA [2].word.tree);
 ;

} break;
case 74: yyDecrement (1) yySetNT (yyNTword) {
/* line 221 "Parser.lrk" */
yyS.word.tree = yyA [0].qualification.tree;

} break;
case 75:
case 43: yyDecrement (1) yySetNT (yyNTword) {
/* line 223 "Parser.lrk" */
 yyS.word.tree = mone_word (NoTree, i_rparent, yyA [0].Scan.Position);
 ;

} break;
case 76: yyDecrement (2) yySetNT (yyNTword) {
/* line 226 "Parser.lrk" */
 yyS.word.tree = mone_qualification (Mqualification (yyA [0].word.tree),
			yyA [1].qualification.tree);
 ;

} break;
case 77:
case 44: yyDecrement (2) yySetNT (yyNTword) {
/* line 230 "Parser.lrk" */
yyS.word.tree = yyA [0].word.tree;

} break;
case 78: yyDecrement (1) yySetNT (yyNTqualification) {
/* line 232 "Parser.lrk" */
yyS.qualification.tree = yyA [0].local_qualification.tree;

} break;
case 79: yyDecrement (2) yySetNT (yyNTqualification) {
/* line 234 "Parser.lrk" */
yyS.qualification.tree = yyA [0].local_qualification.tree;

} break;
case 80: yyDecrement (3) yySetNT (yyNTqualification) {
/* line 236 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position,
			yyA [0].local_qualification.tree, yyA [2].qualification.tree);
 ;

} break;
case 81:
case 45: yyDecrement (4) yySetNT (yyNTqualification) {
/* line 240 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position,
			yyA [0].local_qualification.tree, yyA [2].qualification.tree);
 ;

} break;
case 82: yyDecrement (1) yySetNT (yyNTqualification) {
/* line 244 "Parser.lrk" */
yyS.qualification.tree = yyA [0].global_qualification.tree;

} break;
case 83: yyDecrement (2) yySetNT (yyNTqualification) {
/* line 246 "Parser.lrk" */
yyS.qualification.tree = yyA [0].global_qualification.tree;

} break;
case 84: yyDecrement (3) yySetNT (yyNTqualification) {
/* line 248 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position,
			yyA [0].global_qualification.tree, yyA [2].qualification.tree);
 ;

} break;
case 85:
case 46: yyDecrement (4) yySetNT (yyNTqualification) {
/* line 252 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position,
			yyA [0].global_qualification.tree, yyA [2].qualification.tree);
 ;

} break;
case 86: yyDecrement (1) yySetNT (yyNTqualification) {
/* line 256 "Parser.lrk" */
 yyS.qualification.tree = mglobal_ident (yyA [0].Scan.Position, NoIdent);
 ;

} break;
case 87: yyDecrement (2) yySetNT (yyNTqualification) {
/* line 259 "Parser.lrk" */
 yyS.qualification.tree = mglobal_ident (yyA [1].Scan.Position, i_lparent);
 ;

} break;
case 88: yyDecrement (3) yySetNT (yyNTqualification) {
/* line 262 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position, mglobal_ident
			(yyA [0].Scan.Position, NoIdent), yyA [2].qualification.tree);
 ;

} break;
case 89:
case 47: yyDecrement (4) yySetNT (yyNTqualification) {
/* line 266 "Parser.lrk" */
 yyS.qualification.tree = msubscription (yyA [1].Scan.Position, mglobal_ident
			(yyA [0].Scan.Position, NoIdent), yyA [2].qualification.tree);
 ;

} break;
case 90: yyDecrement (1) yySetNT (yyNTqualification) {
/* line 270 "Parser.lrk" */
 yyS.qualification.tree = mlocal_ident (yyA [0].Scan.Position, i_lparent);
 ;

} break;
case 91: yyDecrement (2) yySetNT (yyNTqualification) {
/* line 273 "Parser.lrk" */
yyS.qualification.tree = yyA [1].qualification.tree;

} break;
case 92:
case 48: yyDecrement (3) yySetNT (yyNTqualification) {
/* line 275 "Parser.lrk" */
yyS.qualification.tree = yyA [1].qualification.tree;

} break;
case 93: yyDecrement (1) yySetNT (yyNTlocal_qualification) {
/* line 277 "Parser.lrk" */
 yyS.local_qualification.tree = yyA [0].fragments.is_simple ?
			mlocal_ident (yyA [0].fragments.tree->text.pos,
			   make_one_word (yyA [0].fragments.tree)) :
			mlocal_text (yyA [0].fragments.tree->text.pos,
			   yyA [0].fragments.tree);
 ;

} break;
case 94: yyDecrement (3) yySetNT (yyNTlocal_qualification) {
/* line 284 "Parser.lrk" */
 yyS.local_qualification.tree = yyA [2].fragments.is_simple ?
			mqualification (yyA [2].fragments.tree->text.pos,
			   yyA [0].local_qualification.tree, make_one_word (yyA [2].fragments.tree)) :
			mcomplex_qual (yyA [2].fragments.tree->text.pos,
			   yyA [0].local_qualification.tree, yyA [2].fragments.tree);
 ;

} break;
case 95: yyDecrement (2) yySetNT (yyNTlocal_qualification) {
/* line 291 "Parser.lrk" */
yyS.local_qualification.tree = yyA [0].local_qualification.tree;

} break;
case 96: yyDecrement (2) yySetNT (yyNTglobal_qualification) {
/* line 293 "Parser.lrk" */
 yyS.global_qualification.tree = yyA [1].fragments.is_simple ?
			mglobal_ident (yyA [1].fragments.tree->text.pos, make_one_word (yyA [1].fragments.tree)) :
			mglobal_text (yyA [1].fragments.tree->text.pos, yyA [1].fragments.tree);
 ;

} break;
case 97: yyDecrement (3) yySetNT (yyNTglobal_qualification) {
/* line 298 "Parser.lrk" */
 yyS.global_qualification.tree = yyA [2].fragments.is_simple ?
			mqualification (yyA [2].fragments.tree->text.pos,
			   yyA [0].global_qualification.tree, make_one_word (yyA [2].fragments.tree)) :
			mcomplex_qual (yyA [2].fragments.tree->text.pos,
			   yyA [0].global_qualification.tree, yyA [2].fragments.tree);
 ;

} break;
case 98: yyDecrement (2) yySetNT (yyNTglobal_qualification) {
/* line 305 "Parser.lrk" */
yyS.global_qualification.tree = yyA [0].global_qualification.tree;

} break;
case 99: yyDecrement (1) yySetNT (yyNTfragments) {
/* line 307 "Parser.lrk" */
yyS.fragments.is_simple = yyA [0].fragment.is_simple;
 yyS.fragments.tree = yyA [0].fragment.tree; yyS.fragments.tree->text.next = dnotext; ;
 ;

} break;
case 100: yyDecrement (2) yySetNT (yyNTfragments) {
/* line 311 "Parser.lrk" */
 yyS.fragments.tree = yyA [0].fragment.tree; yyS.fragments.tree->text.next = yyA [1].fragments.tree; ;

		  yyS.fragments.is_simple = yyA [0].fragment.is_simple & yyA [1].fragments.is_simple;
 ;

} break;
case 101: yyDecrement (1) yySetNT (yyNTfragment) {
/* line 316 "Parser.lrk" */
 yyS.fragment.tree = mcharacter (NoTree, yyA [0].Scan.Position, '$');

		  yyS.fragment.is_simple = rtrue;
 ;

} break;
case 102:
case 49: yyDecrement (1) yySetNT (yyNTfragment) {
/* line 321 "Parser.lrk" */
 yyS.fragment.tree = mcharacter (NoTree, yyA [0].Scan.Position, yyA [0].Scan.character.text);

		  yyS.fragment.is_simple = rtrue;
 ;

} break;
case 103:
case 50: yyDecrement (1) yySetNT (yyNTfragment) {
/* line 326 "Parser.lrk" */
 yyS.fragment.tree = mident (NoTree, yyA [0].Scan.Position, yyA [0].Scan.ident.ident);

		  yyS.fragment.is_simple = rtrue;
 ;

} break;
case 104:
case 51: yyDecrement (3) yySetNT (yyNTfragment) {
/* line 331 "Parser.lrk" */
 yyS.fragment.tree = mblock (NoTree, yyA [0].Scan.Position, yyA [2].Scan.Position, yyA [1].statements.tree, '"');

		  yyS.fragment.is_simple = rfalse;
 ;

} break;
case 105:
case 52: yyDecrement (3) yySetNT (yyNTfragment) {
/* line 336 "Parser.lrk" */
 yyS.fragment.tree = mblock (NoTree, yyA [0].Scan.Position, yyA [2].Scan.Position, yyA [1].statements.tree, '[');

		  yyS.fragment.is_simple = rfalse;
 ;

} break;
case 106:
case 53: yyDecrement (3) yySetNT (yyNTfragment) {
/* line 341 "Parser.lrk" */
 yyS.fragment.tree = mblock (NoTree, yyA [0].Scan.Position, yyA [2].Scan.Position, yyA [1].statements.tree, '{');

		  yyS.fragment.is_simple = rfalse;
 ;

} break;
case 107:
case 54: yyDecrement (4) yySetNT (yyNTfragment) {
/* line 346 "Parser.lrk" */
 yyS.fragment.tree = Mblock_content (yyA [0].Scan.Position, yyA [3].Scan.Position, yyA [2].statements.tree);

		  yyS.fragment.is_simple = rfalse;
 ;

} break;
case 108: yyDecrement (2) yySetNT (yyNTfragment) {
/* line 351 "Parser.lrk" */
 yyS.fragment.tree = mcontent (NoTree, yyA [0].Scan.Position, yyA [1].variable.tree);

		  yyS.fragment.is_simple = rfalse;
 ;

} break;
case 109: yyDecrement (2) yySetNT (yyNTfragment) {
/* line 356 "Parser.lrk" */
 yyS.fragment.tree = mcharacter (NoTree, yyA [0].Scan.Position, '$');

		  yyS.fragment.is_simple = rtrue;
 ;

} break;
case 110: yyDecrement (1) yySetNT (yyNTvariable) {
/* line 361 "Parser.lrk" */
yyS.variable.tree = yyA [0].simple_local_qual.tree;

} break;
case 111: yyDecrement (2) yySetNT (yyNTvariable) {
/* line 363 "Parser.lrk" */
yyS.variable.tree = yyA [0].simple_local_qual.tree;

} break;
case 112: yyDecrement (3) yySetNT (yyNTvariable) {
/* line 365 "Parser.lrk" */
 yyS.variable.tree = msubscription (yyA [1].Scan.Position, yyA [0].simple_local_qual.tree,
			yyA [2].qualification.tree);
 ;

} break;
case 113:
case 55: yyDecrement (4) yySetNT (yyNTvariable) {
/* line 369 "Parser.lrk" */
 yyS.variable.tree = msubscription (yyA [1].Scan.Position, yyA [0].simple_local_qual.tree,
			yyA [2].qualification.tree);
 ;

} break;
case 114: yyDecrement (1) yySetNT (yyNTvariable) {
/* line 373 "Parser.lrk" */
yyS.variable.tree = yyA [0].simple_global_qual.tree;

} break;
case 115: yyDecrement (2) yySetNT (yyNTvariable) {
/* line 375 "Parser.lrk" */
yyS.variable.tree = yyA [0].simple_global_qual.tree;

} break;
case 116: yyDecrement (3) yySetNT (yyNTvariable) {
/* line 377 "Parser.lrk" */
 yyS.variable.tree = msubscription (yyA [1].Scan.Position, yyA [0].simple_global_qual.tree,
			yyA [2].qualification.tree);
 ;

} break;
case 117:
case 56: yyDecrement (4) yySetNT (yyNTvariable) {
/* line 381 "Parser.lrk" */
 yyS.variable.tree = msubscription (yyA [1].Scan.Position, yyA [0].simple_global_qual.tree,
			yyA [2].qualification.tree);
 ;

} break;
case 118:
case 57: yyDecrement (1) yySetNT (yyNTsimple_local_qual) {
/* line 385 "Parser.lrk" */
 yyS.simple_local_qual.tree = mlocal_ident (yyA [0].Scan.Position, yyA [0].Scan.ident.ident);
 ;

} break;
case 119:
case 58: yyDecrement (3) yySetNT (yyNTsimple_local_qual) {
/* line 388 "Parser.lrk" */
 yyS.simple_local_qual.tree = mqualification (yyA [2].Scan.Position,
			yyA [0].simple_local_qual.tree, yyA [2].Scan.ident.ident);
 ;

} break;
case 120: yyDecrement (2) yySetNT (yyNTsimple_local_qual) {
/* line 392 "Parser.lrk" */
yyS.simple_local_qual.tree = yyA [0].simple_local_qual.tree;

} break;
case 121:
case 59: yyDecrement (2) yySetNT (yyNTsimple_global_qual) {
/* line 394 "Parser.lrk" */
 yyS.simple_global_qual.tree = mglobal_ident (yyA [1].Scan.Position, yyA [1].Scan.ident.ident);
 ;

} break;
case 122:
case 60: yyDecrement (3) yySetNT (yyNTsimple_global_qual) {
/* line 397 "Parser.lrk" */
 yyS.simple_global_qual.tree = mqualification (yyA [2].Scan.Position,
			yyA [0].simple_global_qual.tree, yyA [2].Scan.ident.ident);
 ;

} break;
case 123: yyDecrement (2) yySetNT (yyNTsimple_global_qual) {
/* line 401 "Parser.lrk" */
yyS.simple_global_qual.tree = yyA [0].simple_global_qual.tree;

} break;
case 124:
case 61: yyDecrement (1) yySetNT (yyNTend) {
} break;
case 125:
case 62: yyDecrement (2) yySetNT (yyNTend) {
} break;
case 126:
case 63: yyDecrement (1) yySetNT (yyNTend) {
} break;
case 127:
case 64: yyDecrement (2) yySetNT (yyNTend) {
} break;
default: switch (yyState) {
case 1: goto yyAbort;
case 2: goto yyRead;
case 3: goto yyReduce;
}
}

       /* SPEC State = Next (Top (), Nonterminal); nonterminal transition */
#ifdef YYNDefault
	       yyState = * yyStateStackPtr ++;
	       for (;;) {
		  register yytComb * yyNCombPtr =
				yyNBasePtr [yyState] + (int) yyNonterminal;
		  if (yyNCombPtr->Check == yyState) {
		     yyState = yyNCombPtr->Next; break;
		  }
		  yyState = yyNDefault [yyState];
	       }
#else
	       yyState = yyNBasePtr [* yyStateStackPtr ++] [yyNonterminal];
#endif
	       * yyAttrStackPtr ++ = yySynAttribute;
	       if (yyState < yyFirstFinalState) goto yyParseLoop;
							/* read reduce ? */
#ifdef YYDEBUG
	       yyStateStackPtr [0] = yyStateStackPtr [-1];
#endif
	    }

	 } else {					/* read */
   yyRead:  yyStateStackPtr ++;
	    yyGetAttribute (yyAttrStackPtr ++, Attribute);
	    yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyStateStackPtr [-1]);
	       (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		  yyGetTokenName (yyPrevTerminal),
		  yyGetTokenName (yyTerminal)); yyNl ();
	       yyPrevTerminal = yyTerminal;
	    }
#endif
	    yyIsRepairing = rfalse;
	 }
      }

   yyAbort:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "fail    parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return ++ yyErrorCount;

   yyAccept:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "accept  parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return yyErrorCount;
   }

#ifndef NO_RECOVER
static void yyErrorRecovery
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange * yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange *	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
#endif
   {
#define	yyContinueSize	5000
      rbool	yyTokensSkipped	;
      tSet	yyContinueSet	;
      tSet	yyRestartSet	;
      int	yyLength	;
      char	yyContinueString [yyContinueSize + 2];

      if (yyControl.yyMessages) {
   /* 1. report an error */
	 ErrorMessage (xxSyntaxError, xxError, Attribute.Position);

   /* 2. report the offending token */
	 (void) strcpy (yyContinueString, yyGetTokenName (* yyTerminal));
#ifdef SPELLING
	 if (strncmp (yyContinueString, TokenPtr, TokenLength)) {
	    yyContinueString [yyLength = strlen (yyContinueString)] = ' ';
	    (void) GetWord (& yyContinueString [++ yyLength]);
	 }
#endif
	 ErrorMessageI (xxTokenFound, xxInformation, Attribute.Position,
	    xxString, yyContinueString);

   /* 3. report the set of expected terminal symbols */
	 MakeSet (& yyContinueSet, (short) yyLastTerminal);
	 yyComputeContinuation (yyStateStack, yyStackPtr, & yyContinueSet,
	    rtrue);
	 yyLength = 0;
	 yyContinueString [0] = '\0';
	 while (! IsEmpty (& yyContinueSet)) {
	    char * yyTokenString = yyGetTokenName (Extract (& yyContinueSet));
	    int yyl = strlen (yyTokenString);
	    if (yyLength + yyl >= yyContinueSize) break;
	    (void) strcpy (& yyContinueString [yyLength], yyTokenString);
	    yyLength += yyl;
	    yyContinueString [yyLength ++] = ' ';
	 }
	 yyContinueString [-- yyLength] = '\0';
	 ErrorMessageI (xxExpectedTokens, xxInformation, Attribute.Position,
	    xxString, yyContinueString);
	 ReleaseSet (& yyContinueSet);
      }

   /* 4. compute the set of terminal symbols for restart of the parse */
      MakeSet (& yyRestartSet, (short) yyLastTerminal);
      yyComputeRestartPoints (yyStateStack, yyStackPtr, & yyRestartSet);

   /* 5. skip terminal symbols until a restart point is reached */
      yyTokensSkipped = rfalse;
      while (! IsElement (* yyTerminal, & yyRestartSet)) {
#ifdef YYDEBUG
	 yySymbolRange yyPrevTerminal = * yyTerminal;
#endif
	 * yyTerminal = yyGetToken ();
	 yyTokensSkipped = rtrue;
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (yyStateStack [yyStackPtr]);
	    (void) fprintf (yyTrace, "skip    %s, lookahead: %s",
	       yyGetTokenName (yyPrevTerminal),
	       yyGetTokenName (* yyTerminal)); yyNl ();
	 }
#endif
      }
      ReleaseSet (& yyRestartSet);

   /* 6. report the restart point */
      if (yyTokensSkipped & yyControl.yyMessages)
	 ErrorMessage (xxRestartPoint, xxInformation, Attribute.Position);
   }

/*
   compute the set of terminal symbols that can be accepted (read)
   in a given stack configuration (eventually after reduce actions)
*/

static void yyComputeContinuation
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStack, short yyStackPtr, tSet * yyContinueSet,
    rbool yyreduce)
#else
   (yyStack, yyStackPtr, yyContinueSet, yyreduce)
   yyStateRange *	yyStack		;
   short		yyStackPtr	;
   tSet *		yyContinueSet	;
   rbool		yyreduce	;
#endif
   {
      register yySymbolRange	yyTerminal;
      register yyStateRange	yyState = yyStack [yyStackPtr];

      AssignEmpty (yyContinueSet);
      for (yyTerminal = yyFirstTerminal; yyTerminal <= yyLastTerminal;
							yyTerminal ++) {
	 if (yyNext (yyState, yyTerminal) != yyNoState &&
	    yyIsContinuation (yyTerminal, yyStack, yyStackPtr, yyreduce)) {
	    Include (yyContinueSet, (short) yyTerminal);
	 }
      }
   }

/*
   check whether a given terminal symbol can be accepted (read)
   in a certain stack configuration (eventually after reduce actions)
*/

static rbool yyIsContinuation
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyTerminal, yyStateRange * yyStateStack, short yyStackPtr,
    rbool yyreduce)
#else
   (yyTerminal, yyStateStack, yyStackPtr, yyreduce)
   yySymbolRange	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
   rbool		yyreduce	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;

      while (yyStackPtr >= (short) yyIsContStackSize)  /* pass Stack by value */
	 ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyIsContStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyIsContStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      yState = yyIsContStackPtr [yyStackPtr];
      for (;;) {
	 yyIsContStackPtr [yyStackPtr] = yState;
	 yState = yyNext (yState, yyTerminal);
	 if (yState == yyNoState) return rfalse;

	 do {						/* reduce */
	    if (yState > yyLastReduceState) {		/* dynamic ? */
	       yState = yyCondition [yState - yyLastReduceState];
	    }
	    if (yState <= yyLastStopState) { /* read, read reduce, or accept? */
	       return rtrue;
	    } else if (yyreduce) {			/* reduce */
	       yyStackPtr -= yyLength [yState - yyFirstReduceState];
	       yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	    } else {
	       return rfalse;
	    }

	    yState = yyNext (yyIsContStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	    if (yyStackPtr >= (short) yyIsContStackSize)
	       ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
				(unsigned long) sizeof (yyStateRange));
	    yyStackPtr ++;
	 } while (yState >= yyFirstFinalState);
      }
   }

/*
   compute a set of terminal symbols that can be used to restart
   parsing in a given stack configuration. we simulate parsing until
   end of file using a suffix program synthesized by the function (array)
   yyContinuation. All symbols acceptable in the states reached during
   the simulation can be used to restart parsing.
*/

static void yyComputeRestartPoints
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStateStack, short yyStackPtr, tSet * yyRestartSet)
#else
   (yyStateStack, yyStackPtr, yyRestartSet)
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
   tSet *		yyRestartSet	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;
	       tSet		yyContinueSet	;

      while (yyStackPtr >= (short) yyCompResStackSize) /* pass Stack by value */
	 ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyCompResStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyCompResStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      MakeSet (& yyContinueSet, (short) yyLastTerminal);
      AssignEmpty (yyRestartSet);
      yState = yyCompResStackPtr [yyStackPtr];

      for (;;) {
	 if (yyStackPtr >= (short) yyCompResStackSize)
	    ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			     (unsigned long) sizeof (yyStateRange));
	 yyCompResStackPtr [yyStackPtr] = yState;
	 yyComputeContinuation (yyCompResStackPtr, yyStackPtr, & yyContinueSet,
	    rfalse);
	 Union (yyRestartSet, & yyContinueSet);
#ifdef YYDCRP
	 {
	    int yyLength = 0;
	    char yyContinueString [yyContinueSize + 2];
	    yyContinueString [0] = '\0';
	    while (! IsEmpty (& yyContinueSet)) {
	       char * yyTokenString = yyGetTokenName (Extract (& yyContinueSet));
	       int yyl = strlen (yyTokenString);
	       if (yyLength + yyl >= yyContinueSize) break;
	       (void) strcpy (& yyContinueString [yyLength], yyTokenString);
	       yyLength += yyl;
	       yyContinueString [yyLength ++] = ' ';
	    }
	    yyContinueString [-- yyLength] = '\0';
	    (void) fprintf (yyTrace, "%5d union %s", yState, yyContinueString);
	    yyNl ();
	 }
#endif
	 yState = yyNext (yState, yyContinuation [yState]);

	 if (yState >= yyFirstFinalState) {		/* final state ? */
	    if (yState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStackPtr ++;
	       yState = yyFinalToProd [yState - yyFirstReadReduceState];
#ifdef YYDCRP
	       yyCompResStackPtr [yyStackPtr] =
					yyCompResStackPtr [yyStackPtr - 1];
	       (void) fprintf (yyTrace, "%5d shift   %s",
		  yyCompResStackPtr [yyStackPtr], yyGetTokenName
		  (yyContinuation [yyCompResStackPtr [yyStackPtr]])); yyNl ();
#endif
	    }

	    do {					/* reduce */
	       if (yState > yyLastReduceState) {	/* dynamic ? */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d dynamic decision %d",
		    yyCompResStackPtr [yyStackPtr], yState - yyLastReduceState);
		  yyNl ();
#endif
		  yState = yyCondition [yState - yyLastReduceState];
	       }
	       if (yyFirstReduceState <= yState &&
		   yState <= yyLastStopState) {		/* accept */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d accept",
		     yyCompResStackPtr [yyStackPtr]); yyNl ();
#endif
		  ReleaseSet (& yyContinueSet);
		  return;
	       } else if (yState < yyFirstFinalState) {	/* read */
		  goto yyRead;
	       } else {					/* reduce */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d reduce  %s",
		     yyCompResStackPtr [yyStackPtr],
		     yyRule [yState - yyLastReadReduceState]); yyNl ();
#endif
		  yyStackPtr -= yyLength [yState - yyFirstReduceState];
		  yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	       }

	       yState = yyNext (yyCompResStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	       yyStackPtr ++;
	    } while (yState >= yyFirstFinalState);
	 } else {					/* read */
yyRead:
#ifdef YYDCRP
	    (void) fprintf (yyTrace, "%5d shift   %s",
	       yyCompResStackPtr [yyStackPtr], yyGetTokenName
	       (yyContinuation [yyCompResStackPtr [yyStackPtr]])); yyNl ();
#endif
	    yyStackPtr ++;
	 }
      }
   }

/* access the parse table:   Next : State x Symbol -> Action */

static yyStateRange yyNext
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, yySymbolRange yySymbol)
#else
   (yyState, yySymbol) yyStateRange yyState; yySymbolRange yySymbol;
#endif
   {
      if (yySymbol <= yyLastTerminal) {
	 for (;;) {
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yySymbol;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) return yyTCombPtr->Next;
#ifdef YYTDefault
#ifdef YYaccDefault
	    return (yylp = yyDefaultLook [yyState]) &&
	       (yylp [yySymbol >> 5] >> (yySymbol & 0x1f)) & 1 ?
		  yyTDefault [yyState] : yyNoState;
#else
	    if ((yyState = yyTDefault [yyState]) == yyNoState) return yyNoState;
#endif
#else
	    return yyNoState;
#endif
	 }
      }
#ifdef YYNDefault
      for (;;) {
	 register yytComb * yyNCombPtr = yyNBasePtr [yyState] + yySymbol;
	 if (yyNCombPtr->Check == yyState) return yyNCombPtr->Next;
	 yyState = yyNDefault [yyState];
      }
#else
      return yyNBasePtr [yyState] [yySymbol];
#endif
   }
#endif

void BeginParser ARGS ((void))
   {
/* line 158 "Parser.lrk" */


   BeginScanner ();
   i_lparent		= MakeIdent ("(" , 1);
   i_rparent		= MakeIdent (")" , 1);

#ifdef SHARE
   snostmt		= mnostmt ();
   snoword		= mnoword ();
   snotext		= mnotext ();
#endif


   }

void CloseParser ARGS ((void))
   {
/* line 172 "Parser.lrk" */


   }


