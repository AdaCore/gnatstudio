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

#include "directives.h"

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
yyNT0_intern	= 15,
yyNTdirective	= 16,
yyNTxx_directive_1_3	= 17,
yyNTliteral	= 18,
yyNToperator	= 19
} yytNonterminal;
typedef struct { short yyMode; rbool yyActions, yyMessages; } yytControl;

static	yytControl	yyControl	= { 0, rtrue, rtrue };
	rbool		directives_Debug	= rfalse;

#define yyFirstTerminal	0
#define yyLastTerminal	14
#define yySetSize	15
#define yyFirstSymbol	0
#define yyLastSymbol	19
#define yyTTableMax	26
#define yyNTableMax	27
#define yyStartState	1
#define yyFirstReadState	1
#define yyLastReadState	12
#define yyFirstReadReduceState	13
#define yyLastReadReduceState	23
#define yyFirstReduceState	24
#define yyLastReduceState	40
#define yyLastState	40
#define yyLastStopState	24
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

/* line 5 "directives.lrk" */


#include "Source.h"


typedef union {
tScanAttribute Scan;
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
"0_intern : directive _EOF_ ",
"directive : SET SOURCEFORMAT xx_directive_1_3 string ",
"directive : SET name literal ",
"directive : SET name ",
"directive : DISPLAY string ",
"directive : ELSE ",
"directive : END ",
"directive : IF literal operator literal ",
"directive : IF literal NOT operator literal ",
"directive : IF literal DEFINED ",
"directive : IF literal NOT DEFINED ",
"xx_directive_1_3 : ",
"literal : unsigned_integer ",
"literal : string ",
"operator : '<' ",
"operator : '>' ",
"operator : '=' ",
""
};
#endif
	char *		directives_TokenName	[yyLastTerminal + 2] = {
"_EOF_",
"name",
"SET",
"unsigned_integer",
"SOURCEFORMAT",
"DISPLAY",
"ELSE",
"END",
"string",
"IF",
"<",
">",
"=",
"NOT",
"DEFINED",
""
};
static	yytComb		yyTComb		[yyTTableMax + 1] = {
{   6,   27}, {   5,   24}, {   1,    2}, {   4,   19}, {   0,    0}, 
{   1,    3}, {   1,   15}, {   1,   16}, {   4,   20}, {   1,    4}, 
{  10,   21}, {  10,   22}, {  10,   23}, {   2,    6}, {  10,   18}, 
{   9,   13}, {   2,    7}, {   8,   10}, {   8,   17}, {   7,   35}, 
{   3,   14}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, 
};
static	yytNComb	yyNComb		[yyNTableMax - yyLastTerminal] = {
{   0,    0}, {   1,    5}, {   0,    0}, {   0,    0}, {  10,   12}, 
{  11,   31}, {   4,    8}, {   8,   11}, {   6,   26}, {  12,   32}, 
{   7,    9}, {   0,    0}, {   0,    0}, 
};
static	yytComb *	yyTBasePtr	[yyLastReadState + 1] = { 0,
& yyTComb [   0], & yyTComb [  12], & yyTComb [  12], & yyTComb [   0], 
& yyTComb [   1], & yyTComb [   0], & yyTComb [  11], & yyTComb [   4], 
& yyTComb [   7], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
};
static	yytNComb *	yyNBasePtr	[yyLastReadState + 1] = { 0,
& yyNComb [ -15], & yyNComb [ -15], & yyNComb [ -15], & yyNComb [ -12], 
& yyNComb [ -15], & yyNComb [ -10], & yyNComb [  -7], & yyNComb [ -12], 
& yyNComb [ -15], & yyNComb [ -15], & yyNComb [ -13], & yyNComb [  -9], 
};
#ifdef YYTDefault
static	unsigned short	yyTDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     0,     0,     4,     0,    10,     0,     0, 
    4,     4, 
};
#endif
#ifdef YYNDefault
static	unsigned short	yyNDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0, 
};
#endif
#if ! defined NO_RECOVER | defined YYDEC_TABLE
static	unsigned char	yyLength	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
    2,     4,     3,     2,     2,     1,     1,     4,     5,     3, 
    4,     0,     1,     1,     1,     1,     1, 
};
static	yytNonterminal	yyLeftHandSide	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
yyNT0_intern,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTdirective,
yyNTxx_directive_1_3,
yyNTliteral,
yyNTliteral,
yyNToperator,
yyNToperator,
yyNToperator,
};
#endif
#ifndef NO_RECOVER
static	yySymbolRange	yyContinuation	[yyLastReadState + 1] = { 0,
    6,     1,     8,     3,     0,     0,     8,    14,     8,    14, 
    3,     3, 
};
static	unsigned short	yyCondition	[yyLastState - yyLastReduceState + 1] =
{ 0,
};
#endif
static	unsigned short	yyFinalToProd	[yyLastReadReduceState -
						yyFirstReadReduceState + 2] = {
   25,    28,    29,    30,    33,    34,    36,    37,    38,    39, 
   40, 
0
};
static	unsigned short	yyStartLine	[yyLastStopState - yyFirstReduceState
							+ 2] = { 0,
46,
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
	 if (directives_Debug) {
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
	    if (directives_Debug) {
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
	 if (directives_Debug) {
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
   if (directives_Debug) {
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

int directives ARGS ((void))
   {
      return directives2 (yyStartState);
   }

int directives2
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
      Begindirectives ();
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
      if (directives_Debug) {
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

/* line 15 "directives.lrk" */



#ifdef YYDEBUG
      if (directives_Debug) {
	 yyPrintState (yyStartSymbol);
	 (void) fprintf (yyTrace,
	    "parse   for predicate in line %d, lookahead: %s", yyLine,
	    directives_TokenName [yyTerminal]); yyNl ();
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
	    if (directives_Debug) {
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
				directives_TokenName [yyRepairToken]);
#ifdef YYDEBUG
		     if (directives_Debug) {
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "insert  %s",
				directives_TokenName [yyRepairToken]); yyNl ();
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "shift   %s, lookahead: %s",
			   directives_TokenName [yyRepairToken],
			   directives_TokenName [yyTerminal]); yyNl ();
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
	       if (directives_Debug) {
		  yyStateStackPtr [0] = yyStateStackPtr [-1];
		  yyPrintState (* yyStateStackPtr);
		  (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		     directives_TokenName [yyPrevTerminal],
		     directives_TokenName [yyTerminal]); yyNl ();
		  yyPrevTerminal = yyTerminal;
	       }
#endif
	       yyIsRepairing = rfalse;
	    }

	    for (;;) {
	       register yytNonterminal yyNonterminal;	/* left-hand side */

   yyReduce:
#ifdef YYDEBUG
	       if (directives_Debug) {
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
case 24:
YYACCEPT;
case 25:
case 13: /* directive : SET SOURCEFORMAT xx_directive_1_3 \
string */
yyDecrement (4) yySetNT (yyNTdirective) {
} break;
case 26: /* directive : SET name literal */
yyDecrement (3) yySetNT (yyNTdirective) {
} break;
case 27: /* directive : SET name */
yyDecrement (2) yySetNT (yyNTdirective) {
} break;
case 28:
case 14: /* directive : DISPLAY string */
yyDecrement (2) yySetNT (yyNTdirective) {
} break;
case 29:
case 15: /* directive : ELSE */
yyDecrement (1) yySetNT (yyNTdirective) {
} break;
case 30:
case 16: /* directive : END */
yyDecrement (1) yySetNT (yyNTdirective) {
} break;
case 31: /* directive : IF literal operator literal */
yyDecrement (4) yySetNT (yyNTdirective) {
} break;
case 32: /* directive : IF literal NOT operator literal */
yyDecrement (5) yySetNT (yyNTdirective) {
} break;
case 33:
case 17: /* directive : IF literal DEFINED */
yyDecrement (3) yySetNT (yyNTdirective) {
} break;
case 34:
case 18: /* directive : IF literal NOT DEFINED */
yyDecrement (4) yySetNT (yyNTdirective) {
} break;
case 35: /* xx_directive_1_3 : */
yySetNT (yyNTxx_directive_1_3) {
/* line 47 "directives.lrk" */
 ;
{  char s [256]; StGetString (Attribute.string.Value, s);
		       if (strncmp (s + 1, "FREE", 4) == 0) {
			  free_format = rtrue; MaxColumn = 1024;
		       } else if (strncmp (s + 1, "FIXED", 5) == 0) {
			  free_format = rfalse; MaxColumn = 72;
		       }
		; } ;

} break;
case 36:
case 19: /* literal : unsigned_integer */
yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 37:
case 20: /* literal : string */
yyDecrement (1) yySetNT (yyNTliteral) {
} break;
case 38:
case 21: /* operator : '<' */
yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 39:
case 22: /* operator : '>' */
yyDecrement (1) yySetNT (yyNToperator) {
} break;
case 40:
case 23: /* operator : '=' */
yyDecrement (1) yySetNT (yyNToperator) {
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
	    if (directives_Debug) {
	       yyPrintState (yyStateStackPtr [-1]);
	       (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		  directives_TokenName [yyPrevTerminal],
		  directives_TokenName [yyTerminal]); yyNl ();
	       yyPrevTerminal = yyTerminal;
	    }
#endif
	    yyIsRepairing = rfalse;
	 }
      }

   yyAbort:
#ifdef YYDEBUG
      if (directives_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "fail    parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return ++ yyErrorCount;

   yyAccept:
#ifdef YYDEBUG
      if (directives_Debug) {
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
	 (void) strcpy (yyContinueString, directives_TokenName [* yyTerminal]);
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
	    char * yyTokenString = directives_TokenName [Extract (& yyContinueSet)];
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
	 if (directives_Debug) {
	    yyPrintState (yyStateStack [yyStackPtr]);
	    (void) fprintf (yyTrace, "skip    %s, lookahead: %s",
	       directives_TokenName [yyPrevTerminal],
	       directives_TokenName [* yyTerminal]); yyNl ();
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
		  directives_TokenName [yyContinuation [yyCompResStackPtr
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
	       directives_TokenName [yyContinuation [yyCompResStackPtr [yyStackPtr]]]);
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

void Begindirectives ARGS ((void))
   {
/* line 18 "directives.lrk" */


   }

void Closedirectives ARGS ((void))
   {
/* line 21 "directives.lrk" */


   }


