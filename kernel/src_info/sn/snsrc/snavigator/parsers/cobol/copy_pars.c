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

#include "copy_pars.h"

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
yyNT0_intern	= 463,
yyNTstart	= 464,
yyNTxx_start_1_2	= 465,
yyNTxx_start_2_2	= 466,
yyNTcopy	= 467,
yyNTcopy_name	= 468,
yyNTcopy_suppress_o	= 469,
yyNTcopy_replacing_o	= 470,
yyNTxx_copy_replacing_o_1_1	= 471,
yyNTcopy_replacing_l	= 472,
yyNTcopy_replacing_e	= 473,
yyNTreplace	= 474,
yyNTxx_replace_1_1	= 475,
yyNTreplace_l	= 476,
yyNTreplace_e	= 477,
yyNTreplacing_item_1	= 478,
yyNTpseudo_text_1	= 479,
yyNTreplacing_item_2	= 480,
yyNTxx_replacing_item_2_2_1	= 481,
yyNTpseudo_text_2	= 482,
yyNTreplacing_item	= 483,
yyNTtoken_l	= 484,
yyNTtoken_e	= 485,
yyNTtoken	= 486,
yyNTexpression	= 487,
yyNTmultiplicative_expression	= 488,
yyNTpower_expression	= 489,
yyNTunary_expression	= 490,
yyNTprimary_expression	= 491,
yyNTfunction_call	= 492,
yyNTfunction_name_1	= 493,
yyNTfunction_name_2	= 494,
yyNTargument_l	= 495,
yyNTidentifier	= 496,
yyNTqualification	= 497,
yyNTin_of	= 498,
yyNTindex_l	= 499,
yyNTindex	= 500,
yyNTreference_modifier	= 501,
yyNTchapter_name	= 502,
yyNTliteral	= 503,
yyNTinteger	= 504,
yyNTu_integer	= 505
} yytNonterminal;
typedef struct { short yyMode; rbool yyActions, yyMessages; } yytControl;

static	yytControl	yyControl	= { 0, rtrue, rtrue };
	rbool		copy_pars_Debug	= rfalse;

#define yyFirstTerminal	0
#define yyLastTerminal	462
#define yySetSize	463
#define yyFirstSymbol	0
#define yyLastSymbol	505
#define yyTTableMax	1265
#define yyNTableMax	607
#define yyStartState	1
#define yyFirstReadState	1
#define yyLastReadState	84
#define yyFirstReadReduceState	85
#define yyLastReadReduceState	140
#define yyFirstReduceState	141
#define yyLastReduceState	266
#define yyLastState	276
#define yyLastStopState	141
#define YYTDefault
#define YYNDefault
#define YYTrialParse
#define YYGetLook

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

/* line 5 "copy_pars.lrk" */


#include "Position.h"
#include "StringM.h"
#include "Idents.h"

#define yyInitBufferSize	32

#define TOKENOP	PrevEPos = CurrentEPos; CurrentEPos = Attribute.name.EPos;
#define BEFORE_TRIAL	tPosition SavePEPos, SaveCEPos; SavePEPos = PrevEPos; SaveCEPos = CurrentEPos;
#define AFTER_TRIAL	PrevEPos = SavePEPos; CurrentEPos = SaveCEPos;

extern	rbool		Copy ARGS ((tIdent ident, tPosition pos));

static	tPosition	PrevEPos, CurrentEPos;
static	tIdent		iCURRENT_DATE	;
static	tIdent		iWHEN_COMPILED	;

typedef struct { tIdent Ident; tPosition Pos; } zzcopy_name;
typedef struct { long Value; } zzinteger;
typedef struct { long Value; tScanAttribute Scan; } zzu_integer;

typedef union {
tScanAttribute Scan;
zzcopy_name copy_name;
zzinteger integer;
zzu_integer u_integer;
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
	char *		copy_pars_TokenName	[yyLastTerminal + 2] = {
"_EOF_",
"name",
"paragraph_name",
"unsigned_integer",
"plus_integer",
"minus_integer",
"level_number",
"real",
"string",
"pseudo_text",
"picture_string",
"illegal_character",
".",
0,
0,
0,
0,
0,
0,
"ALL",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"BY",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"FUNCTION",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"IN",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"LENGTH",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"OF",
"OFF",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"REPLACING",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"SUPPRESS",
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
"=",
"<",
">",
"(",
")",
"+",
"<>",
">=",
"<=",
"66",
"77",
"78",
"88",
"-",
":",
"&",
"**",
"*",
"/",
"==",
""
};
static	yytComb		yyTComb		[yyTTableMax + 1] = {
{   5,  141}, {  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, 
{  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, {  15,   87}, 
{  29,  233}, {  29,  233}, {  29,  233}, {  21,   35}, {  31,  154}, 
{  14,  156}, {  15,   88}, {  34,   20}, {  66,   75}, {  29,  233}, 
{  83,   84}, {  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, 
{  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, {  16,   86}, 
{  36,  224}, {  36,  224}, {  36,  224}, {  12,  241}, {  12,  241}, 
{  12,  241}, {  18,  161}, {  63,  155}, {  12,  241}, {  36,  224}, 
{  12,  241}, {   6,  147}, {   8,  148}, {  11,   85}, {  47,   58}, 
{  69,   20}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  29,  233}, {  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, 
{  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, {  19,   33}, 
{  35,  228}, {  35,  228}, {  35,  228}, {  69,  126}, {  64,   73}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  35,  228}, 
{  36,  224}, {  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, 
{  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, {   0,    0}, 
{  79,  235}, {  79,  235}, {  79,  235}, {  13,  242}, {  13,  242}, 
{  13,  242}, {   0,    0}, {   0,    0}, {  13,  242}, {  79,  235}, 
{  13,  242}, {  81,  135}, {  81,  133}, {  81,  134}, {  81,  136}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  35,  228}, {   0,    0}, {   0,    0}, {  20,  239}, {  20,  239}, 
{  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, 
{  20,  239}, {   0,    0}, {  20,  239}, {  20,  239}, {  20,  239}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  79,  235}, {  20,  239}, {   0,    0}, {  60,  183}, {  60,  183}, 
{  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, 
{  60,  183}, {   0,    0}, {  60,  183}, {  60,  183}, {  60,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  60,  183}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  20,  239}, {  76,  225}, {  76,  225}, 
{  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, 
{  76,  225}, {   0,    0}, {  76,  225}, {  76,  225}, {  76,  225}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  76,  225}, {  60,  183}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  24,   20}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  24,  131}, 
{  24,  132}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  29,  233}, {  38,   21}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  74,  129}, {  74,  130}, {  74,  135}, {   0,    0}, 
{   0,    0}, {  74,  136}, {  76,  225}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  29,  233}, {   7,  124}, 
{   0,    0}, {  36,  224}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  58,  243}, 
{   0,    0}, {  58,  243}, {  58,  243}, {  58,  243}, {  58,  243}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  36,  224}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  29,  233}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  58,  243}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  35,  228}, {   0,    0}, {   0,    0}, {  21,  122}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  36,  224}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  35,  228}, {   0,    0}, 
{   0,    0}, {  79,  235}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  29,  233}, {   7,  125}, {  22,   38}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  79,  235}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  35,  228}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  36,  224}, {   0,    0}, {  20,  239}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   1,   90}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  79,  235}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  20,  267}, {   0,    0}, {   0,    0}, {  60,  183}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  35,  228}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  60,  271}, {   0,    0}, {   0,    0}, {   0,    0}, {  20,  239}, 
{  30,   48}, {  31,  154}, {  14,  157}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  79,  235}, {   0,    0}, {  76,  225}, {  78,  247}, 
{   0,    0}, {  78,  247}, {  78,  247}, {  78,  247}, {  78,  247}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  60,  183}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  76,  225}, {   0,    0}, {  78,  247}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  24,   21}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  20,  268}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  76,  225}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  60,  272}, {  31,   89}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  28,  212}, {   0,    0}, 
{  28,  212}, {  28,  212}, {  28,  212}, {  28,  212}, {  28,  212}, 
{  28,  212}, {   0,    0}, {   0,    0}, {   0,    0}, {  24,   22}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  28,  212}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  76,  225}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  29,  233}, {  29,  233}, 
{  29,  233}, {  29,   47}, {  29,  233}, {  29,  233}, {  29,  233}, 
{  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, 
{  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, {  29,  233}, 
{  29,  233}, {  29,  233}, {  29,  233}, {  36,  224}, {  36,  224}, 
{  36,  224}, {  36,  270}, {  36,  224}, {  36,  224}, {  36,  224}, 
{  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, 
{  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, {  36,  224}, 
{  36,  224}, {  36,  224}, {  36,  224}, {  37,   51}, {  12,  241}, 
{  12,  241}, {  12,  241}, {  12,  241}, {   1,  163}, {   4,   10}, 
{   2,   10}, {  69,  123}, {   3,   10}, {  35,  228}, {  35,  228}, 
{  35,  228}, {  35,  269}, {  35,  228}, {  35,  228}, {  35,  228}, 
{  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, 
{  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, {  35,  228}, 
{  35,  228}, {  35,  228}, {  35,  228}, {  79,  235}, {  79,  235}, 
{  79,  235}, {  79,  276}, {  79,  235}, {  79,  235}, {  79,  235}, 
{  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, 
{  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, {  79,  235}, 
{  79,  235}, {  79,  235}, {  79,  235}, {  70,   79}, {  13,  242}, 
{  13,  242}, {  13,  242}, {  13,  242}, {  42,  127}, {  39,  121}, 
{  81,  137}, {  81,  138}, {  81,  139}, {  81,  140}, {  68,   76}, 
{  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, 
{  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, 
{  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, 
{  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, {  20,  239}, 
{  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, 
{  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, 
{  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, 
{  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, {  60,  183}, 
{  48,   62}, {  84,   93}, {  67,  231}, {  28,  212}, {  67,  231}, 
{  67,  231}, {  67,  231}, {  67,  231}, {  67,  231}, {  67,  231}, 
{  76,  225}, {  76,  225}, {  76,  225}, {  76,  275}, {  76,  225}, 
{  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, 
{  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, 
{  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, {  76,  225}, 
{  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  65,  258}, {  24,   23}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  10,   24}, {  72,   91}, {  28,  212}, 
{  32,   92}, {  33,  172}, {  80,   83}, {  65,  258}, {  50,   66}, 
{  10,   25}, {  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, 
{  61,  189}, {  61,  189}, {  61,  189}, {  75,   94}, {  61,  189}, 
{  61,  189}, {  61,  189}, {  74,  137}, {  74,  138}, {  74,  139}, 
{  74,  140}, {  54,  128}, {  54,   40}, {  61,  189}, {   9,   17}, 
{   0,    0}, {   0,    0}, {  17,   95}, {  17,   97}, {   0,    0}, 
{  54,   41}, {   0,    0}, {  54,   43}, {   0,    0}, {  65,  258}, 
{  58,  243}, {  17,   98}, {  17,   99}, {  17,   96}, {   0,    0}, 
{  58,  243}, {  58,  243}, {  58,  243}, {  58,  243}, {   0,    0}, 
{  17,  100}, {   0,    0}, {  73,   60}, {   0,    0}, {  26,   40}, 
{   0,    0}, {   0,    0}, {  59,   40}, {   0,    0}, {  61,  189}, 
{  73,   98}, {  73,   99}, {  26,   41}, {  26,   42}, {  26,   43}, 
{  59,   41}, {  59,   42}, {  59,   43}, {   0,    0}, {  73,  100}, 
{   0,    0}, {   0,    0}, {  57,  214}, {   0,    0}, {  57,  214}, 
{  57,  214}, {  57,  214}, {  57,  214}, {  57,  214}, {  57,  214}, 
{   0,    0}, {  17,  101}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  52,  209}, 
{  57,  214}, {  52,  209}, {  52,  209}, {  52,  209}, {  52,  209}, 
{  52,  209}, {  52,  209}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  73,  101}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  56,  213}, {  52,  209}, {  56,  213}, {  56,  213}, 
{  56,  213}, {  56,  213}, {  56,  213}, {  56,  213}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  53,  210}, {  56,  213}, 
{  53,  210}, {  53,  210}, {  53,  210}, {  53,  210}, {  53,  210}, 
{  53,  210}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  27,  208}, {  53,  210}, {  27,  208}, {  27,  208}, {  27,  208}, 
{  27,  208}, {  27,  208}, {  27,  208}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  67,  231}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  55,  211}, {  27,  208}, {  55,  211}, 
{  55,  211}, {  55,  211}, {  55,  211}, {  55,  211}, {  55,  211}, 
{  78,  247}, {  78,   81}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  78,  247}, {  78,  247}, {  78,  247}, {  78,  247}, {  78,   82}, 
{  55,  211}, {   0,    0}, {   0,    0}, {   0,    0}, {  77,  232}, 
{  65,  258}, {  77,  232}, {  77,  232}, {  77,  232}, {  77,  232}, 
{  77,  232}, {  77,  232}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  67,  231}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  65,  273}, {   0,    0}, {   0,    0}, 
{  61,  189}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  61,  189}, {  28,  212}, {  28,  212}, 
{  28,  212}, {  65,  258}, {  17,  102}, {   0,    0}, {  28,  212}, 
{  28,  212}, {  28,  212}, {  28,  212}, {  28,  212}, {  28,  212}, 
{  28,  212}, {  28,   46}, {  28,  212}, {  28,  212}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  17,  103}, 
{   0,    0}, {  73,   61}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  73,  103}, {   0,    0}, 
{  65,  274}, {   0,    0}, {  57,  214}, {  17,  104}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  52,  209}, 
{  61,  189}, {   0,    0}, {  73,  104}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  56,  213}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  17,  105}, {  57,  214}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  53,  210}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  52,  209}, {  73,  105}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  27,  208}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  56,  213}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  55,  211}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  53,  210}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  77,  232}, 
{   0,    0}, {  27,  208}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  55,  211}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  67,  231}, {  67,  231}, {   0,    0}, 
{  77,  232}, {   0,    0}, {   0,    0}, {  67,  231}, {  67,  231}, 
{  67,  231}, {  67,  231}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  65,  258}, {  65,  258}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, {  65,  258}, 
{  65,  258}, {  65,  258}, {  61,  189}, {  61,  189}, {  61,  189}, 
{  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, 
{  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, 
{  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, {  61,  189}, 
{  61,  189}, {  61,  189}, {   0,    0}, {   0,    0}, {  17,  118}, 
{  17,  115}, {  17,  119}, {  17,  107}, {  17,  108}, {  17,  111}, 
{  17,  117}, {  17,  120}, {  17,  116}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  17,  112}, {  17,  114}, {  17,  106}, 
{  17,  110}, {  17,  109}, {  17,  113}, {  73,  118}, {  73,  115}, 
{  73,  119}, {  73,  107}, {  73,  108}, {  73,  111}, {  73,  117}, 
{  73,  120}, {  73,  116}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  73,  112}, {  73,  114}, {  73,  106}, {  73,  110}, 
{  73,  109}, {  73,  113}, {  73,  172}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  57,  214}, {  57,  214}, {  57,  214}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  57,  214}, {  57,  214}, 
{  57,  214}, {  57,  214}, {  57,  214}, {  57,  214}, {  57,  214}, 
{   0,    0}, {  57,  214}, {  57,  214}, {   0,    0}, {  52,  209}, 
{  52,  209}, {  52,  209}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  52,  209}, {  52,  209}, {  52,  209}, {  52,  209}, {  52,  209}, 
{  52,  209}, {  52,  209}, {   0,    0}, {  52,   44}, {  52,   45}, 
{   0,    0}, {  56,  213}, {  56,  213}, {  56,  213}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  56,  213}, {  56,  213}, {  56,  213}, 
{  56,  213}, {  56,  213}, {  56,  213}, {  56,  213}, {   0,    0}, 
{  56,  213}, {  56,  213}, {   0,    0}, {  53,  210}, {  53,  210}, 
{  53,  210}, {   0,    0}, {   0,    0}, {   0,    0}, {  53,  210}, 
{  53,  210}, {  53,  210}, {  53,  210}, {  53,  210}, {  53,  210}, 
{  53,  210}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  27,  208}, {  27,  208}, {  27,  208}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  27,  208}, {  27,  208}, {  27,  208}, {  27,  208}, 
{  27,  208}, {  27,  208}, {  27,  208}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  55,  211}, {  55,  211}, {  55,  211}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  55,  211}, {  55,  211}, 
{  55,  211}, {  55,  211}, {  55,  211}, {  55,  211}, {  55,  211}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  77,  232}, 
{  77,  232}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  77,  232}, {  77,  232}, {  77,  232}, {  77,  232}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, 
};
static	yytNComb	yyNComb		[yyNTableMax - yyLastTerminal] = {
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  48,   63}, 
{  48,  158}, {   0,    0}, {   0,    0}, {   0,    0}, {  63,  159}, 
{   0,    0}, {   0,    0}, {  73,  160}, {  73,   80}, {  63,   64}, 
{  73,  170}, {   0,    0}, {   0,    0}, {  73,  177}, {  63,  167}, 
{   1,    5}, {  51,   67}, {  63,  177}, {   1,    6}, {   1,    7}, 
{  32,  179}, {  32,  181}, {  42,   54}, {  73,  174}, {  51,   68}, 
{   1,    8}, {   1,    9}, {  63,  174}, {  24,  217}, {  10,   26}, 
{  73,  184}, {  73,  255}, {  73,   65}, {  24,  220}, {  63,  184}, 
{  63,  255}, {  63,   65}, {  47,   59}, {  32,  184}, {  32,  255}, 
{  24,  221}, {  24,  255}, {  24,  258}, {  70,   78}, {  38,  238}, 
{  18,  165}, {  70,  244}, {  18,   19}, {  38,  223}, {  38,   29}, 
{  70,  246}, {  41,   53}, {  41,   28}, {  23,   39}, {  23,   27}, 
{  44,   56}, {  44,  215}, {  49,  150}, {  49,   30}, {  46,  216}, 
{  46,  219}, {  33,   50}, {  33,  166}, {  74,  176}, {  62,   72}, 
{  62,  178}, {  74,  254}, {  81,  248}, {  81,  258}, {   9,   18}, 
{   9,  164}, {   7,   14}, {  61,   36}, {  61,   37}, {  68,   77}, 
{  17,   32}, {  25,  218}, {  43,   55}, {  45,   57}, {  40,   52}, 
{   4,  144}, {  82,  249}, {   2,  142}, {   3,  143}, {  14,  149}, 
{  71,  175}, {  15,   31}, {  58,   69}, {  34,  240}, {  65,   74}, 
{  76,  227}, {  60,   71}, {  31,   49}, {  20,   34}, {   6,   11}, 
{   8,   16}, {  79,  237}, {  59,   70}, {  36,  226}, {  29,  236}, 
{   7,   15}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytComb *	yyTBasePtr	[yyLastReadState + 1] = { 0,
& yyTComb [  26], & yyTComb [  44], & yyTComb [  46], & yyTComb [  43], 
& yyTComb [   0], & yyTComb [  29], & yyTComb [   1], & yyTComb [  30], 
& yyTComb [ 192], & yyTComb [ 179], & yyTComb [  31], & yyTComb [  32], 
& yyTComb [  82], & yyTComb [   3], & yyTComb [   8], & yyTComb [  17], 
& yyTComb [ 656], & yyTComb [  24], & yyTComb [   9], & yyTComb [ 102], 
& yyTComb [  12], & yyTComb [   2], & yyTComb [   0], & yyTComb [ 177], 
& yyTComb [   0], & yyTComb [ 231], & yyTComb [ 764], & yyTComb [ 397], 
& yyTComb [   0], & yyTComb [   1], & yyTComb [   2], & yyTComb [ 168], 
& yyTComb [ 169], & yyTComb [  16], & yyTComb [  50], & yyTComb [  20], 
& yyTComb [  37], & yyTComb [   1], & yyTComb [  92], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [  91], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [  25], & yyTComb [ 123], 
& yyTComb [   0], & yyTComb [ 172], & yyTComb [   0], & yyTComb [ 713], 
& yyTComb [ 747], & yyTComb [ 204], & yyTComb [ 781], & yyTComb [ 730], 
& yyTComb [ 696], & yyTComb [ 218], & yyTComb [ 234], & yyTComb [ 122], 
& yyTComb [ 634], & yyTComb [   0], & yyTComb [  25], & yyTComb [  14], 
& yyTComb [ 614], & yyTComb [   9], & yyTComb [ 586], & yyTComb [  97], 
& yyTComb [  44], & yyTComb [  86], & yyTComb [   0], & yyTComb [ 166], 
& yyTComb [ 675], & yyTComb [ 195], & yyTComb [ 181], & yyTComb [ 152], 
& yyTComb [ 803], & yyTComb [ 343], & yyTComb [  70], & yyTComb [ 170], 
& yyTComb [  88], & yyTComb [   0], & yyTComb [  11], & yyTComb [ 124], 
};
static	yytNComb *	yyNBasePtr	[yyLastReadState + 1] = { 0,
& yyNComb [-439], & yyNComb [-409], & yyNComb [-408], & yyNComb [-411], 
& yyNComb [-463], & yyNComb [-361], & yyNComb [-388], & yyNComb [-361], 
& yyNComb [-397], & yyNComb [-448], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-376], & yyNComb [-372], & yyNComb [-463], 
& yyNComb [-399], & yyNComb [-422], & yyNComb [-463], & yyNComb [-395], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-424], & yyNComb [-453], 
& yyNComb [-405], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-392], & yyNComb [-463], & yyNComb [-367], & yyNComb [-455], 
& yyNComb [-410], & yyNComb [-399], & yyNComb [-463], & yyNComb [-393], 
& yyNComb [-463], & yyNComb [-438], & yyNComb [-463], & yyNComb [-399], 
& yyNComb [-427], & yyNComb [-455], & yyNComb [-401], & yyNComb [-424], 
& yyNComb [-401], & yyNComb [-421], & yyNComb [-440], & yyNComb [-463], 
& yyNComb [-403], & yyNComb [-463], & yyNComb [-461], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-463], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-402], & yyNComb [-392], & yyNComb [-397], 
& yyNComb [-411], & yyNComb [-410], & yyNComb [-459], & yyNComb [-463], 
& yyNComb [-399], & yyNComb [-463], & yyNComb [-463], & yyNComb [-403], 
& yyNComb [-463], & yyNComb [-444], & yyNComb [-407], & yyNComb [-463], 
& yyNComb [-463], & yyNComb [-429], & yyNComb [-463], & yyNComb [-401], 
& yyNComb [-463], & yyNComb [-463], & yyNComb [-395], & yyNComb [-463], 
& yyNComb [-427], & yyNComb [-413], & yyNComb [-463], & yyNComb [-463], 
};
#ifdef YYTDefault
static	unsigned short	yyTDefault	[yyLastReadState + 1] = { 0,
   15,     0,     0,     0,     0,     0,    31,     0,     0,    24, 
    0,     0,     0,     0,     0,     0,    47,     9,     0,     0, 
    0,     0,    10,    81,    24,     0,    52,     0,     0,     0, 
    0,    17,     0,     0,     0,     0,     0,    34,    54,    10, 
   10,    68,    10,    10,    10,    10,    10,    73,    14,     0, 
   10,     0,    52,     0,    52,    28,    28,     0,    58,     0, 
   21,    17,    48,     0,     0,     0,    54,    10,    81,    69, 
   74,    32,    47,     0,     0,     0,    67,     0,     0,     0, 
    0,    81,     0,     0, 
};
#endif
#ifdef YYNDefault
static	unsigned short	yyNDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     0,     0,     0,     0,     0,    18,    23, 
    0,     0,     0,    49,     0,     0,    62,     0,     0,     0, 
   61,     0,    41,    38,    24,     0,     0,     0,     0,     0, 
    0,    81,     0,     0,     0,     0,     0,     0,     0,    41, 
   44,    23,    41,    46,    44,    24,    23,    63,     0,     0, 
   23,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,    32,    38,     0,     0,     0,     0,    23,    70,    81, 
   74,    32,    38,     0,     0,     0,     0,     0,     0,     0, 
    0,    81,     0,     0, 
};
#endif
#if ! defined NO_RECOVER | defined YYDEC_TABLE
static	unsigned char	yyLength	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
    2,     1,     1,     1,     3,     3,     0,     0,     3,     5, 
    1,     1,     1,     0,     3,     0,     0,     1,     2,     3, 
    2,     1,     0,     1,     2,     3,     1,     3,     3,     1, 
    4,     0,     4,     1,     3,     3,     1,     1,     2,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     3,     3, 
    3,     1,     3,     3,     1,     3,     2,     2,     1,     1, 
    1,     3,     3,     2,     5,     3,     6,     1,     1,     1, 
    1,     2,     1,     5,     5,     2,     6,     1,     1,     3, 
    1,     1,     0,     2,     2,     1,     1,     3,     3,     4, 
    5,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1, 
};
static	yytNonterminal	yyLeftHandSide	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNT0_intern,
yyNTstart,
yyNTstart,
yyNTxx_start_1_2,
yyNTxx_start_2_2,
yyNTcopy,
yyNTcopy,
yyNTcopy_name,
yyNTcopy_name,
yyNTcopy_suppress_o,
yyNTcopy_suppress_o,
yyNTcopy_replacing_o,
yyNTcopy_replacing_o,
yyNTxx_copy_replacing_o_1_1,
yyNTcopy_replacing_l,
yyNTcopy_replacing_l,
yyNTcopy_replacing_e,
yyNTreplace,
yyNTreplace,
yyNTxx_replace_1_1,
yyNTreplace_l,
yyNTreplace_l,
yyNTreplace_e,
yyNTreplacing_item_1,
yyNTreplacing_item_1,
yyNTpseudo_text_1,
yyNTreplacing_item_2,
yyNTreplacing_item_2,
yyNTxx_replacing_item_2_2_1,
yyNTpseudo_text_2,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTreplacing_item,
yyNTtoken_l,
yyNTtoken_l,
yyNTtoken_e,
yyNTtoken_e,
yyNTtoken_e,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTtoken,
yyNTexpression,
yyNTexpression,
yyNTexpression,
yyNTexpression,
yyNTmultiplicative_expression,
yyNTmultiplicative_expression,
yyNTmultiplicative_expression,
yyNTpower_expression,
yyNTpower_expression,
yyNTunary_expression,
yyNTunary_expression,
yyNTunary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTprimary_expression,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_call,
yyNTfunction_name_1,
yyNTfunction_name_2,
yyNTfunction_name_2,
yyNTargument_l,
yyNTargument_l,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTidentifier,
yyNTqualification,
yyNTqualification,
yyNTin_of,
yyNTin_of,
yyNTindex_l,
yyNTindex_l,
yyNTindex_l,
yyNTindex,
yyNTindex,
yyNTindex,
yyNTindex,
yyNTreference_modifier,
yyNTreference_modifier,
yyNTchapter_name,
yyNTchapter_name,
yyNTchapter_name,
yyNTliteral,
yyNTliteral,
yyNTliteral,
yyNTinteger,
yyNTinteger,
yyNTinteger,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
yyNTu_integer,
};
#endif
#ifndef NO_RECOVER
static	yySymbolRange	yyContinuation	[yyLastReadState + 1] = { 0,
    1,   446,   446,   446,     0,    12,    12,    12,   462,     1, 
   12,     1,     1,    12,     1,    12,     1,    12,    50,     1, 
    1,   271,     1,     1,     1,   457,     1,     1,     1,   334, 
   12,   462,   462,     1,     1,     1,   446,     1,   447,     1, 
    1,   447,     1,     1,     1,     1,    19,     1,    12,   462, 
    1,     1,     1,   447,     1,     1,     1,     1,     1,     1, 
    1,     1,    12,    50,     1,     9,     1,   447,   447,   447, 
    1,   462,     1,     1,   462,     1,     1,     1,     1,   462, 
    3,     3,     9,   462, 
};
static	unsigned short	yyCondition	[yyLastState - yyLastReduceState + 1] =
{ 0,
   12,    13,   229,    10,    12,    13,    12,    13,    10,    10, 
};
#endif
static	unsigned short	yyFinalToProd	[yyLastReadReduceState -
						yyFirstReadReduceState + 2] = {
  145,   146,   151,   152,   153,   162,   168,   169,   171,   173, 
  180,   182,   183,   185,   186,   187,   188,   189,   190,   191, 
  192,   193,   194,   195,   196,   197,   198,   199,   200,   201, 
  202,   203,   204,   205,   206,   207,   222,   230,   234,   241, 
  242,   245,   250,   251,   252,   253,   256,   257,   259,   260, 
  261,   262,   263,   264,   265,   266, 
0
};
static	unsigned short	yyStartLine	[yyLastStopState - yyFirstReduceState
							+ 2] = { 0,
97,
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
				short yyStackPtr, tSet * yyContinueSet));
static	rbool	yyIsContinuation	ARGS ((yySymbolRange yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
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
	 if (copy_pars_Debug) {
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
	    if (copy_pars_Debug) {
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
	 if (copy_pars_Debug) {
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

#ifdef YYDEBUG

static void yyNl ARGS ((void))
{ (void) putc ('\n', yyTrace); (void) fflush (yyTrace); }

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
   if (copy_pars_Debug) {
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

int copy_pars ARGS ((void))
   {
      return copy_pars2 (yyStartState);
   }

int copy_pars2
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
      Begincopy_pars ();
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
      if (copy_pars_Debug) {
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

/* line 35 "copy_pars.lrk" */



#ifdef YYDEBUG
      if (copy_pars_Debug) {
	 yyPrintState (yyStartSymbol);
	 (void) fprintf (yyTrace,
	    "parse   for predicate in line %d, lookahead: %s", yyLine,
	    copy_pars_TokenName [yyTerminal]); yyNl ();
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
	    if (copy_pars_Debug) {
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
	       if (yyNext (yyState, (yySymbolRange) yyTerminal) == yyNoState) {
		  yySymbolRange		yyRepairToken;		/* repair */
		  tScanAttribute	yyRepairAttribute;

		  yyRepairToken = yyContinuation [yyState];
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
				copy_pars_TokenName [yyRepairToken]);
#ifdef YYDEBUG
		     if (copy_pars_Debug) {
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "insert  %s",
				copy_pars_TokenName [yyRepairToken]); yyNl ();
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "shift   %s, lookahead: %s",
			   copy_pars_TokenName [yyRepairToken],
			   copy_pars_TokenName [yyTerminal]); yyNl ();
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
	       } else {
		  yyState = yyNext (yyState, (yySymbolRange) yyTerminal);
		  goto yyFinal;
	       }
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
	       if (copy_pars_Debug) {
		  yyStateStackPtr [0] = yyStateStackPtr [-1];
		  yyPrintState (* yyStateStackPtr);
		  (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		     copy_pars_TokenName [yyPrevTerminal],
		     copy_pars_TokenName [yyTerminal]); yyNl ();
		  yyPrevTerminal = yyTerminal;
	       }
#endif
	       yyIsRepairing = rfalse;
	    }

	    for (;;) {
	       register yytNonterminal yyNonterminal;	/* left-hand side */

   yyReduce:
#ifdef YYDEBUG
	       if (copy_pars_Debug) {
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
case 141:
YYACCEPT;
case 142:
YYACCEPT;
case 143:
YYACCEPT;
case 144:
YYACCEPT;
case 145:
case 85: yyDecrement (3) yySetNT (yyNTstart) {
} break;
case 146:
case 86: yyDecrement (3) yySetNT (yyNTstart) {
} break;
case 147: yySetNT (yyNTxx_start_1_2) {
/* line 98 "copy_pars.lrk" */
 ;
{  YYACCEPT; ; } ;

} break;
case 148: yySetNT (yyNTxx_start_2_2) {
/* line 102 "copy_pars.lrk" */
 ;
{  YYACCEPT; ; } ;

} break;
case 149: yyDecrement (3) yySetNT (yyNTcopy) {
/* line 105 "copy_pars.lrk" */
 ;
{  (void) Copy (yyA [0].copy_name.Ident, yyA [0].copy_name.Pos); ; } ;

} break;
case 150: yyDecrement (5) yySetNT (yyNTcopy) {
/* line 108 "copy_pars.lrk" */
 ;
{  (void) Copy (yyA [0].copy_name.Ident, yyA [0].copy_name.Pos); ; } ;

} break;
case 151:
case 87: yyDecrement (1) yySetNT (yyNTcopy_name) {
/* line 111 "copy_pars.lrk" */
yyS.copy_name.Ident = yyA [0].Scan.name.Ident;
 yyS.copy_name.Pos  =  yyA [0].Scan.Position;
 ;

} break;
case 152:
case 88: yyDecrement (1) yySetNT (yyNTcopy_name) {
/* line 115 "copy_pars.lrk" */
 yyS.copy_name.Pos  =  yyA [0].Scan.Position;
 {
			      char word [128];
			      StGetString (yyA [0].Scan.string.Value, word);
			      yyS.copy_name.Ident = MakeIdent (& word [1], strlen (word) - 2);
			   } ;
 ;

} break;
case 153:
case 89: yyDecrement (1) yySetNT (yyNTcopy_suppress_o) {
} break;
case 154: yySetNT (yyNTcopy_suppress_o) {
} break;
case 155: yyDecrement (3) yySetNT (yyNTcopy_replacing_o) {
/* line 125 "copy_pars.lrk" */
 ;
{  end_replacing (); ; } ;

} break;
case 156: yySetNT (yyNTcopy_replacing_o) {
} break;
case 157: yySetNT (yyNTxx_copy_replacing_o_1_1) {
/* line 128 "copy_pars.lrk" */
 ;
{  begin_replacing (); ; } ;

} break;
case 158: yyDecrement (1) yySetNT (yyNTcopy_replacing_l) {
} break;
case 159: yyDecrement (2) yySetNT (yyNTcopy_replacing_l) {
} break;
case 160: yyDecrement (3) yySetNT (yyNTcopy_replacing_e) {
} break;
case 161: yyDecrement (2) yySetNT (yyNTreplace) {
/* line 135 "copy_pars.lrk" */
 ;
{  end_replacing (); ; } ;

} break;
case 162:
case 90: yyDecrement (1) yySetNT (yyNTreplace) {
} break;
case 163: yySetNT (yyNTxx_replace_1_1) {
/* line 138 "copy_pars.lrk" */
 ;
{  begin_replacing (); ; } ;

} break;
case 164: yyDecrement (1) yySetNT (yyNTreplace_l) {
} break;
case 165: yyDecrement (2) yySetNT (yyNTreplace_l) {
} break;
case 166: yyDecrement (3) yySetNT (yyNTreplace_e) {
} break;
case 167: yyDecrement (1) yySetNT (yyNTreplacing_item_1) {
} break;
case 168:
case 91: yyDecrement (3) yySetNT (yyNTreplacing_item_1) {
} break;
case 169:
case 92: yyDecrement (3) yySetNT (yyNTpseudo_text_1) {
} break;
case 170: yyDecrement (1) yySetNT (yyNTreplacing_item_2) {
} break;
case 171:
case 93: yyDecrement (4) yySetNT (yyNTreplacing_item_2) {
} break;
case 172: yySetNT (yyNTxx_replacing_item_2_2_1) {
/* line 150 "copy_pars.lrk" */
 ;
{  start_pseudo_text (); ; } ;

} break;
case 173:
case 94: yyDecrement (4) yySetNT (yyNTpseudo_text_2) {
} break;
case 174: yyDecrement (1) yySetNT (yyNTreplacing_item) {
} break;
case 175: yyDecrement (3) yySetNT (yyNTreplacing_item) {
} break;
case 176: yyDecrement (3) yySetNT (yyNTreplacing_item) {
} break;
case 177: yyDecrement (1) yySetNT (yyNTreplacing_item) {
} break;
case 178: yyDecrement (1) yySetNT (yyNTtoken_l) {
} break;
case 179: yyDecrement (2) yySetNT (yyNTtoken_l) {
} break;
case 180:
case 95: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 181: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 182:
case 96: yyDecrement (1) yySetNT (yyNTtoken_e) {
} break;
case 183:
case 97:
yy43: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 184: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 185:
case 98: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 186:
case 99: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 187:
case 100: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 188:
case 101: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 189:
case 102: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 190:
case 103: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 191:
case 104: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 192:
case 105: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 193:
case 106: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 194:
case 107: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 195:
case 108: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 196:
case 109: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 197:
case 110: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 198:
case 111: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 199:
case 112: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 200:
case 113: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 201:
case 114: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 202:
case 115: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 203:
case 116: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 204:
case 117: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 205:
case 118: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 206:
case 119: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 207:
case 120: yyDecrement (1) yySetNT (yyNTtoken) {
} break;
case 208: yyDecrement (1) yySetNT (yyNTexpression) {
} break;
case 209: yyDecrement (3) yySetNT (yyNTexpression) {
} break;
case 210: yyDecrement (3) yySetNT (yyNTexpression) {
} break;
case 211: yyDecrement (3) yySetNT (yyNTexpression) {
} break;
case 212: yyDecrement (1) yySetNT (yyNTmultiplicative_expression) {
} break;
case 213: yyDecrement (3) yySetNT (yyNTmultiplicative_expression) {
} break;
case 214: yyDecrement (3) yySetNT (yyNTmultiplicative_expression) {
} break;
case 215: yyDecrement (1) yySetNT (yyNTpower_expression) {
} break;
case 216: yyDecrement (3) yySetNT (yyNTpower_expression) {
} break;
case 217: yyDecrement (2) yySetNT (yyNTunary_expression) {
} break;
case 218: yyDecrement (2) yySetNT (yyNTunary_expression) {
} break;
case 219: yyDecrement (1) yySetNT (yyNTunary_expression) {
} break;
case 220: yyDecrement (1) yySetNT (yyNTprimary_expression) {
} break;
case 221: yyDecrement (1) yySetNT (yyNTprimary_expression) {
} break;
case 222:
case 121: yyDecrement (3) yySetNT (yyNTprimary_expression) {
} break;
case 223: yyDecrement (3) yySetNT (yyNTprimary_expression) {
} break;
case 224:
yy84: yyDecrement (2) yySetNT (yyNTfunction_call) {
} break;
case 225:
yy85: yyDecrement (5) yySetNT (yyNTfunction_call) {
} break;
case 226: yyDecrement (3) yySetNT (yyNTfunction_call) {
} break;
case 227: yyDecrement (6) yySetNT (yyNTfunction_call) {
} break;
case 228:
yy88: yyDecrement (1) yySetNT (yyNTfunction_name_1) {
} break;
case 229:
yy89: yyDecrement (1) yySetNT (yyNTfunction_name_2) {
} break;
case 230:
case 122: yyDecrement (1) yySetNT (yyNTfunction_name_2) {
} break;
case 231: yyDecrement (1) yySetNT (yyNTargument_l) {
} break;
case 232: yyDecrement (2) yySetNT (yyNTargument_l) {
} break;
case 233: yyDecrement (1) yySetNT (yyNTidentifier) {
} break;
case 234:
case 123: yyDecrement (5) yySetNT (yyNTidentifier) {
} break;
case 235:
yy95: yyDecrement (5) yySetNT (yyNTidentifier) {
} break;
case 236: yyDecrement (2) yySetNT (yyNTidentifier) {
} break;
case 237: yyDecrement (6) yySetNT (yyNTidentifier) {
} break;
case 238: yyDecrement (1) yySetNT (yyNTidentifier) {
} break;
case 239:
yy99: yyDecrement (1) yySetNT (yyNTqualification) {
} break;
case 240: yyDecrement (3) yySetNT (yyNTqualification) {
} break;
case 241:
case 124: yyDecrement (1) yySetNT (yyNTin_of) {
} break;
case 242:
case 125: yyDecrement (1) yySetNT (yyNTin_of) {
} break;
case 243: yySetNT (yyNTindex_l) {
} break;
case 244: yyDecrement (2) yySetNT (yyNTindex_l) {
} break;
case 245:
case 126: yyDecrement (2) yySetNT (yyNTindex_l) {
} break;
case 246: yyDecrement (1) yySetNT (yyNTindex) {
} break;
case 247: yyDecrement (1) yySetNT (yyNTindex) {
} break;
case 248: yyDecrement (3) yySetNT (yyNTindex) {
} break;
case 249: yyDecrement (3) yySetNT (yyNTindex) {
} break;
case 250:
case 127: yyDecrement (4) yySetNT (yyNTreference_modifier) {
} break;
case 251:
case 128: yyDecrement (5) yySetNT (yyNTreference_modifier) {
} break;
case 252:
case 129: yyDecrement (1) yySetNT (yyNTchapter_name) {
} break;
case 253:
case 130: yyDecrement (1) yySetNT (yyNTchapter_name) {
} break;
case 254: yyDecrement (1) yySetNT (yyNTchapter_name) {
} break;
case 255: yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 256:
case 131: yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 257:
case 132: yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 258:
yy118: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 238 "copy_pars.lrk" */
yyS.integer.Value = yyA [0].u_integer.Value;

} break;
case 259:
case 133: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 240 "copy_pars.lrk" */
yyS.integer.Value = yyA [0].Scan.plus_integer.Value;

} break;
case 260:
case 134: yyDecrement (1) yySetNT (yyNTinteger) {
} if (yyControl.yyActions) {
/* line 242 "copy_pars.lrk" */
yyS.integer.Value = yyA [0].Scan.minus_integer.Value;

} break;
case 261:
case 135: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 244 "copy_pars.lrk" */
yyS.u_integer.Value = yyA [0].Scan.unsigned_integer.Value;
 yyS.u_integer.Scan = yyA [0].Scan	;
 ;

} break;
case 262:
case 136: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 248 "copy_pars.lrk" */
yyS.u_integer.Value = yyA [0].Scan.level_number.Value;
 yyS.u_integer.Scan = yyA [0].Scan	;
 ;

} break;
case 263:
case 137: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 252 "copy_pars.lrk" */
 yyS.u_integer.Value = 66;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 264:
case 138: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 256 "copy_pars.lrk" */
 yyS.u_integer.Value = 77;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 265:
case 139: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 260 "copy_pars.lrk" */
 yyS.u_integer.Value = 78;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 266:
case 140: yyDecrement (1) yySetNT (yyNTu_integer) {
} if (yyControl.yyActions) {
/* line 264 "copy_pars.lrk" */
 yyS.u_integer.Value = 88;
	  yyS.u_integer.Scan = yyA [0].Scan		;
 ;

} break;
case 267: /* STATE 28 */
/* line 219 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 219,
( GetLookahead (2) == 50         
))) yyGotoReduce (239, yy99)
else { yyGotoRead (12)
}
case 268: /* STATE 28 */
/* line 219 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 219,
( GetLookahead (2) == 50         
))) yyGotoReduce (239, yy99)
else { yyGotoRead (13)
}
case 269: /* STATE 93 */
/* line 208 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 208,
( yyA [-1].Scan.name.Ident == iCURRENT_DATE || yyA [-1].Scan.name.Ident == iWHEN_COMPILED 
))) yyGotoReduce (228, yy88)
else { yyGotoReduce (229, yy89)
}
case 270: /* STATE 95 */
if (yyPrintResult (* yyStateStackPtr, 204, yyTrialParse (
2, yyTerminal, 204) != 0)) yyGotoReduce (224, yy84)
else { yyGotoRead (10)
}
case 271: /* STATE 131 */
/* line 163 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 163,
( GetLookahead (2) == 50         
))) yyGotoReduce (183, yy43)
else { yyGotoRead (12)
}
case 272: /* STATE 131 */
/* line 163 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 163,
( GetLookahead (2) == 50         
))) yyGotoReduce (183, yy43)
else { yyGotoRead (13)
}
case 273: /* STATE 140 */
/* line 238 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 238,
( GetLookahead (2) == 50         
))) yyGotoReduce (258, yy118)
else { yyGotoRead (12)
}
case 274: /* STATE 140 */
/* line 238 "copy_pars.lrk" */
if (yyPrintResult (* yyStateStackPtr, 238,
( GetLookahead (2) == 50         
))) yyGotoReduce (258, yy118)
else { yyGotoRead (13)
}
case 275: /* STATE 154 */
if (yyPrintResult (* yyStateStackPtr, 205, yyTrialParse (
3, yyTerminal, 205) != 0)) yyGotoReduce (225, yy85)
else { yyGotoRead (10)
}
case 276: /* STATE 161 */
if (yyPrintResult (* yyStateStackPtr, 215, yyTrialParse (
4, yyTerminal, 215) != 0)) yyGotoReduce (235, yy95)
else { yyGotoRead (10)
}
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
	    if (copy_pars_Debug) {
	       yyPrintState (yyStateStackPtr [-1]);
	       (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		  copy_pars_TokenName [yyPrevTerminal],
		  copy_pars_TokenName [yyTerminal]); yyNl ();
	       yyPrevTerminal = yyTerminal;
	    }
#endif
	    yyIsRepairing = rfalse;
	 }
      }

   yyAbort:
#ifdef YYDEBUG
      if (copy_pars_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "fail    parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return ++ yyErrorCount;

   yyAccept:
#ifdef YYDEBUG
      if (copy_pars_Debug) {
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
	 (void) strcpy (yyContinueString, copy_pars_TokenName [* yyTerminal]);
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
	 yyComputeContinuation (yyStateStack, yyStackPtr, & yyContinueSet);
	 yyLength = 0;
	 yyContinueString [0] = '\0';
	 while (! IsEmpty (& yyContinueSet)) {
	    char * yyTokenString = copy_pars_TokenName [Extract (& yyContinueSet)];
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
	 if (copy_pars_Debug) {
	    yyPrintState (yyStateStack [yyStackPtr]);
	    (void) fprintf (yyTrace, "skip    %s, lookahead: %s",
	       copy_pars_TokenName [yyPrevTerminal],
	       copy_pars_TokenName [* yyTerminal]); yyNl ();
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
   (yyStateRange * yyStack, short yyStackPtr, tSet * yyContinueSet)
#else
   (yyStack, yyStackPtr, yyContinueSet)
   yyStateRange *	yyStack		;
   short		yyStackPtr	;
   tSet *		yyContinueSet	;
#endif
   {
      register yySymbolRange	yyTerminal;
      register yyStateRange	yyState = yyStack [yyStackPtr];

      AssignEmpty (yyContinueSet);
      for (yyTerminal = yyFirstTerminal; yyTerminal <= yyLastTerminal;
							yyTerminal ++) {
	 if (yyNext (yyState, yyTerminal) != yyNoState &&
	    yyIsContinuation (yyTerminal, yyStack, yyStackPtr)) {
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
   (yySymbolRange yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
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
	    } else {					/* reduce */
	       yyStackPtr -= yyLength [yState - yyFirstReduceState];
	       yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
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
	 yyComputeContinuation (yyCompResStackPtr, yyStackPtr, & yyContinueSet);
	 Union (yyRestartSet, & yyContinueSet);
	 yState = yyNext (yState, yyContinuation [yState]);

	 if (yState >= yyFirstFinalState) {		/* final state ? */
	    if (yState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStackPtr ++;
	       yState = yyFinalToProd [yState - yyFirstReadReduceState];
#ifdef YYDCRP
	       yyCompResStackPtr [yyStackPtr] =
					yyCompResStackPtr [yyStackPtr - 1];
	       (void) fprintf (yyTrace, "%5d shift   %s\n",
		  yyCompResStackPtr [yyStackPtr],
		  copy_pars_TokenName [yyContinuation [yyCompResStackPtr
		  [yyStackPtr]]]);
#endif
	    }

	    do {					/* reduce */
	       if (yState > yyLastReduceState) {	/* dynamic ? */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d dynamic decision %d\n",
		  yyCompResStackPtr [yyStackPtr], yState - yyLastReduceState);
#endif
		  yState = yyCondition [yState - yyLastReduceState];
	       }
	       if (yyFirstReduceState <= yState &&
		   yState <= yyLastStopState) {		/* accept */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d accept\n",
		     yyCompResStackPtr [yyStackPtr]);
#endif
		  ReleaseSet (& yyContinueSet);
		  return;
	       } else if (yState < yyFirstFinalState) {	/* read */
		  goto yyRead;
	       } else {					/* reduce */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d reduce  %s\n",
		     yyCompResStackPtr [yyStackPtr],
		     yyRule [yState - yyLastReadReduceState]);
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
	    (void) fprintf (yyTrace, "%5d shift   %s\n",
	       yyCompResStackPtr [yyStackPtr],
	       copy_pars_TokenName [yyContinuation [yyCompResStackPtr [yyStackPtr]]]);
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

void Begincopy_pars ARGS ((void))
   {
/* line 38 "copy_pars.lrk" */


   iCURRENT_DATE	= MakeIdent ("CURRENT-DATE"	, 12);
   iWHEN_COMPILED	= MakeIdent ("WHEN-COMPILED"	, 13);


   }

void Closecopy_pars ARGS ((void))
   {
/* line 45 "copy_pars.lrk" */


   }


