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

#define rbool		char
#define rtrue		1
#define rfalse		0
#define StdIn		0

#include "Scanner.h"

#ifdef __cplusplus
extern "C" {
#include "Source.h"
#include "rSystem.h"
#include "General.h"
#include "DynArray.h"
#include "Position.h"
}
#else
#include "Source.h"
#include "rSystem.h"
#include "General.h"
#include "DynArray.h"
#include "Position.h"
#endif

#include <stdio.h>
#if defined __STDC__ | defined __cplusplus
#include <stdlib.h>
#endif

#define yyStart(State)	{ yyPreviousStart = yyStartState; yyStartState = State;}
#define yyPrevious	{ yyStateRange s = yyStartState; \
			yyStartState = yyPreviousStart; yyPreviousStart = s; }
#define yyEcho		{ (void) fwrite (TokenPtr, 1, \
			TokenLength, stdout); }
#define yyEol(Column)	{ yyLineCount ++; \
			yyLineStart = (unsigned char *) TokenPtr + \
			TokenLength - 1 - (Column); }
#define output(c)	(void) putchar ((int) c)
#define yyColumn(Ptr)	((int) ((Ptr) - (char *) yyLineStart))
#define yyOffset(Ptr)	(yyFileOffset + ((Ptr) - yyChBufferStart2))

#define yyDNoState		0
#define yyFirstCh	(unsigned char) '\0'
#define yyLastCh	(unsigned char) '\377'
#define yyEolCh	(unsigned char) '\12'
#define yyEobCh	(unsigned char) '\177'
#define yyDStateCount	46
#define yyTableSize	1163
#define yyEobState	26
#define yyDefaultState	27
#define STD	1
#define Comment	3

static void yyExit ARGS ((void))
{ rExit (1); }

typedef unsigned short	yyStateRange;
typedef struct { yyStateRange yyCheck, yyNext; } yyCombType;

	char *		TokenPtr	;
	int		TokenLength	;
	tScanAttribute	Attribute	;
	void		(* Scanner_Exit)	ARGS ((void)) = yyExit;

static	void		yyInitialize	ARGS ((void));
static	void		yyErrorMessage	ARGS ((int yyErrorCode));
#ifdef xxinput
static	char		input		ARGS ((void));
#endif
static	void		unput		ARGS ((char));
static	void		yyLess		ARGS ((int));

static	yyCombType	yyComb		[yyTableSize   + 1] = {
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   30}, 
{   1,   28}, {   1,   16}, {   1,   16}, {   1,   17}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   32}, {   1,   16}, {   1,   45}, 
{   1,   46}, {   1,   36}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   38}, {   1,   37}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   22}, {   1,   41}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   34}, 
{  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, 
{  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, 
{  12,   12}, {  14,   14}, {  15,   12}, {  17,    9}, {  12,   12}, 
{  21,   15}, {  34,   35}, {  10,   20}, {  10,   20}, {  10,   20}, 
{  10,   20}, {  10,   20}, {  10,   20}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   1,   39}, {   1,    6}, {   1,   42}, {   1,   16}, 
{   0,    0}, {   1,   16}, {   0,    0}, {  12,   12}, {  19,   24}, 
{  19,   24}, {  19,   24}, {  19,   24}, {  19,   24}, {  19,   24}, 
{  19,   24}, {  19,   24}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  10,   20}, 
{  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, {  10,   20}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   1,   40}, {   1,   16}, 
{   1,   44}, {   1,   16}, {  25,   26}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, {   1,   16}, 
{   1,   16}, {   3,   31}, {   3,   29}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  20,   13}, {  20,   13}, 
{  20,   13}, {  20,   13}, {  20,   13}, {  20,   13}, {  20,   13}, 
{  20,   13}, {  20,   13}, {  20,   13}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   3,   33}, 
{  20,   13}, {  20,   13}, {  20,   13}, {  20,   13}, {  20,   13}, 
{  20,   13}, {  24,   11}, {  24,   11}, {  24,   11}, {  24,   11}, 
{  24,   11}, {  24,   11}, {  24,   11}, {  24,   11}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  20,   13}, {  20,   13}, {  20,   13}, 
{  20,   13}, {  20,   13}, {  20,   13}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   3,   43}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   0,    0}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, {   5,    5}, 
{   5,    5}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   0,    0}, {   6,   18}, {   6,   18}, {   6,   23}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   19}, 
{   6,   19}, {   6,   19}, {   6,   19}, {   6,   19}, {   6,   19}, 
{   6,   19}, {   6,   19}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   10}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   0,    0}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, {   6,   18}, 
{   6,   18}, {   6,   18}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   7,    7}, 
{   0,    0}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, {   7,    7}, 
{   7,    7}, {   7,    7}, {   8,    8}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   8,    8}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   8,    8}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   8,   21}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yyCombType *	yyBasePtr	[yyDStateCount + 1] = { 0,
& yyComb [   0], & yyComb [   0], & yyComb [ 247], & yyComb [   0], 
& yyComb [ 330], & yyComb [ 586], & yyComb [ 794], & yyComb [ 908], 
& yyComb [   0], & yyComb [  17], & yyComb [   0], & yyComb [  66], 
& yyComb [   0], & yyComb [  18], & yyComb [  67], & yyComb [   0], 
& yyComb [  68], & yyComb [   0], & yyComb [  51], & yyComb [ 215], 
& yyComb [  67], & yyComb [   0], & yyComb [   0], & yyComb [ 238], 
& yyComb [   0], & yyComb [   0], & yyComb [   0], & yyComb [   0], 
& yyComb [   0], & yyComb [   0], & yyComb [   0], & yyComb [   0], 
& yyComb [   0], & yyComb [  17], & yyComb [   0], & yyComb [   0], 
& yyComb [   0], & yyComb [   0], & yyComb [   0], & yyComb [   0], 
& yyComb [   0], & yyComb [   0], & yyComb [   0], & yyComb [   0], 
& yyComb [   0], & yyComb [   0], 
};
static	yyStateRange	yyDefault	[yyDStateCount + 1] = { 0,
    7,     1,     5,     3,    25,    15,    25,    25,    25,    25, 
    0,    25,     0,    25,    25,    25,     8,     0,    25,    25, 
   15,    14,    15,    25,     0,     0,     0,     9,     0,     8, 
    5,     8,     5,    16,     0,    16,    16,    16,    16,    16, 
   16,    16,     5,    16,    16,    16, 
};
static	yyStateRange	yyEobTrans	[yyDStateCount + 1] = { 0,
   16,    16,     5,     5,     5,    18,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    5,     0,     5,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     5,     0,     0,     0, 
};

static	yyStateRange	yyStartState	= STD;
static	yyStateRange	yyPreviousStart	= STD;
static	short		yySourceFile	= StdIn;
static	rbool		yyEof		= rfalse;
static	long		yyBytesRead	= 0;
static	long		yyFileOffset	= 0;
static	unsigned int	yyLineCount	= 1;
static	unsigned char *	yyLineStart	;
static	char *		yyChBufferStart2;

					/* Start State Stack: StStSt	*/

#if defined xxyyPush | defined xxyyPop
#define		yyInitStStStackSize	16

static	yyStateRange *	yyStStStackPtr	;
static	unsigned long	yyStStStackSize	= 0;
static	unsigned int	yyStStStackIdx	= 0;
#endif

#ifdef xxyyPush
static void yyPush
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState)
#else
   (yyState) yyStateRange yyState;
#endif
{
   if (yyStStStackIdx == yyStStStackSize) {
      if (yyStStStackSize == 0) {
	 yyStStStackSize = yyInitStStStackSize;
	 MakeArray ((char * *) & yyStStStackPtr, & yyStStStackSize,
			(unsigned long) sizeof (yyStateRange));
      } else {
	 ExtendArray ((char * *) & yyStStStackPtr, & yyStStStackSize,
			(unsigned long) sizeof (yyStateRange));
      }
      if (yyStStStackPtr == NULL) yyErrorMessage (1);
   }
   yyStStStackPtr [yyStStStackIdx ++] = yyStartState;
   yyStart (yyState);
}
#endif

#ifdef xxyyPop
static void yyPop ARGS ((void))
{
   yyPreviousStart = yyStartState;
   if (yyStStStackIdx > 0)
      yyStartState = yyStStStackPtr [-- yyStStStackIdx];
   else
      yyErrorMessage (4);
}
#endif

#ifdef xxGetLower
static	unsigned char	yyToLower	[] = {
'\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7',
'\10', '\11', '\12', '\13', '\14', '\15', '\16', '\17',
'\20', '\21', '\22', '\23', '\24', '\25', '\26', '\27',
'\30', '\31', '\32', '\33', '\34', '\35', '\36', '\37',
' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
'@', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '[', '\\', ']', '^', '_',
'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}','~','\177',
'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
'\300', '\301', '\302', '\303', '\304', '\305', '\306', '\307',
'\310', '\311', '\312', '\313', '\314', '\315', '\316', '\317',
'\320', '\321', '\322', '\323', '\324', '\325', '\326', '\327',
'\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};
#endif

#ifdef xxGetUpper
static	unsigned char	yyToUpper	[] = {
'\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7',
'\10', '\11', '\12', '\13', '\14', '\15', '\16', '\17',
'\20', '\21', '\22', '\23', '\24', '\25', '\26', '\27',
'\30', '\31', '\32', '\33', '\34', '\35', '\36', '\37',
' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
'`', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '{', '|', '}','~','\177',
'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
'\300', '\301', '\302', '\303', '\304', '\305', '\306', '\307',
'\310', '\311', '\312', '\313', '\314', '\315', '\316', '\317',
'\320', '\321', '\322', '\323', '\324', '\325', '\326', '\327',
'\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};
#endif

static void yyTab1	ARGS ((int yya));

#define yyTab		yyTab1 (0)
#define yyTab2(a,b)	yyTab1 (a)

/* line 28 "tcl.rex" */

#include "ratc.h"
#include "Reuse.h"

void ErrorAttribute
#if defined __STDC__ | defined __cplusplus
 (int Token, tScanAttribute * pAttribute)
#else
 (Token, pAttribute) int Token; tScanAttribute * pAttribute;
#endif
{
 pAttribute->Position = Attribute.Position;
 switch (Token) {
 case /* ident */ 1: 
 pAttribute->ident.ident = NoIdent	;
 ;
 break;
 case /* character */ 2: 
 pAttribute->character.text  = '?'	;
 ;
 break;
 }
}


static	rbool	EOF_reached	= rfalse;
static	rbool	first_word	= rtrue;

static	int	KlLvl;
static	char	KPop;
static	char	KlStack [255];

#define r_brace  '}'
#define push(ch) KlStack [++ KlLvl] = ch;

static rbool is_open (char SymClose)
  {
    register int i;
    switch (SymClose) {
    case '"':
      for (i = KlLvl; i >= 0; i --) {
	switch (KlStack [i]) {
	case '[': return rfalse;
	case '"': return rtrue;
	}
      }
      return rfalse;
    case ']':
      for (i = KlLvl; i >= 0; i --) {
	switch (KlStack [i]) {
	case '[': return rtrue;
        case '"': return rfalse;
        case '{': return rfalse;
	}
      }
      return rfalse;
    case '}':
      for (i = KlLvl; i >= 0; i --)
	if (KlStack [i] == '{') {
	   return rtrue;
	}
      return rfalse;
    }
    return rfalse;
  }


#ifndef yySetPosition
#define yySetPosition Attribute.Position.Line   = yyLineCount; \
Attribute.Position.Column = (int) ((unsigned char *) TokenPtr - yyLineStart);
#endif

#undef yyTab
#undef yyTab2

#ifndef yyInitBufferSize
#define yyInitBufferSize	1024 * 8 + 256
#endif
#ifndef yyInitFileStackSize
#define yyInitFileStackSize	8
#endif
#ifndef yyTabSpace
#define yyTabSpace		8
#endif

static void yyTab1
#if defined __STDC__ | defined __cplusplus
   (int yya)
#else
   (yya) int yya;
#endif
   { yyLineStart -= (yyTabSpace - 1 - ((unsigned char *) TokenPtr -
	yyLineStart + yya - 1)) & (yyTabSpace - 1); }

#define yyTab		yyLineStart -= (yyTabSpace - 1 - \
((unsigned char *) TokenPtr - yyLineStart - 1)) & (yyTabSpace - 1)
#define yyTab1(a)	yyLineStart -= (yyTabSpace - 1 - \
((unsigned char *) TokenPtr - yyLineStart + (a) - 1)) & (yyTabSpace - 1)
#define yyTab2(a,b)	yyLineStart -= (yyTabSpace - 1 - \
((unsigned char *) TokenPtr - yyLineStart + (a) - 1)) & (yyTabSpace - 1)

static	yyStateRange	yyInitStateStack [4] = { yyDefaultState };
static	yyStateRange *	yyStateStack	= yyInitStateStack;
static	unsigned long	yyStateStackSize= 0;

static	unsigned char	yyInitChBuffer [] = {
   '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
   '\0', '\0', '\0', '\0', '\0', '\0', '\0', yyEolCh, yyEobCh, '\0', };
static	unsigned char *	yyChBufferPtr	= yyInitChBuffer;
static	unsigned long	yyChBufferSize	= 0;
static	unsigned char *	yyChBufferStart	= & yyInitChBuffer [16];
static	unsigned char *	yyChBufferIndex	= & yyInitChBuffer [16];

typedef	struct {
	short		yySourceFile	;
	rbool		yyEof		;
	unsigned char *	yyChBufferPtr	;
	unsigned char *	yyChBufferStart	;
	unsigned long	yyChBufferSize	;
	unsigned char *	yyChBufferIndex	;
	long		yyBytesRead	;
	long		yyFileOffset	;
	unsigned int	yyLineCount	;
	unsigned char *	yyLineStart	;
	}		yytFileStack	;

static	yytFileStack *	yyFileStack	;
static	unsigned long	yyFileStackSize	= 0;
static	yytFileStack *	yyFileStackPtr	;

int GetToken ARGS ((void))
{
   register	yyStateRange	yyState;
   register	yyStateRange *	yyStatePtr;
   register	unsigned char * yyChBufferIndexReg;
   register	yyCombType * *	yyBasePtrReg = yyBasePtr;
/* line 95 "tcl.rex" */

  char word [256];

/*
 * Abbau des Stacks geöffneter Klammern bis zur nächsten passenden Klammerebene.
 * d.h. ggf werden im Source nicht vorhandene Klammerzu-Zeichen zurückgeliefert.
 * aus {[} --> {[end_of_command]end_of_command}
 * der Parser kann bei Bedarf die 'unechten' Tokens an einem Attribut erkennen.
 */

  if (KPop != 'X') {
  L_KlPop:
    switch (KlStack [KlLvl]) {
    case '"' : KlStack [KlLvl] = 'A';
    	       first_word = rtrue; return 15;
    case '{' : KlStack [KlLvl] = '}';
    	       first_word = rtrue; return 15;
    case '[' : KlStack [KlLvl] = ']';
    	       first_word = rtrue; return 15;
    case 'A' : if (KlStack [KlLvl --] == KPop) { KPop = 'X'; }
    	       first_word = rfalse; return 5;
    case '}' : if (KlStack [KlLvl --] == KPop) { KPop = 'X'; }
    	       first_word = rfalse; return 9;
    case ']' : if (KlStack [KlLvl --] == KPop) { KPop = 'X'; }
    	       first_word = rfalse; return 7;
    }
  }

  if (EOF_reached) return EofToken;


yyBegin:
   yyState		= yyStartState;		/* initialize */
   yyStatePtr		= & yyStateStack [1];
   yyChBufferIndexReg	= yyChBufferIndex;
   TokenPtr		= (char *) yyChBufferIndexReg;

   /* ASSERT yyChBuffer [yyChBufferIndex] == first character */

yyContinue:		/* continue after sentinel or skipping blanks */
   for (;;) {		/* execute as many state transitions as possible */
			/* determine next state and get next character */
      register yyCombType * yyTablePtr =
			(yyBasePtrReg [yyState] + * yyChBufferIndexReg ++);
      if (yyTablePtr->yyCheck == yyState) {
	 yyState = yyTablePtr->yyNext;
	 * yyStatePtr ++ = yyState;		/* push state */
	 goto yyContinue;
      }
      yyChBufferIndexReg --;			/* reconsider character */
      if ((yyState = yyDefault [yyState]) == yyDNoState) break;
   }

   for (;;) {				/* search for last final state */
      TokenLength =
	    (int) (yyChBufferIndexReg - (unsigned char *) TokenPtr);
      yyChBufferIndex = yyChBufferIndexReg;
switch (* -- yyStatePtr) {
case 5:;
case 31:;
case 33:;
case 43:;
/* line 155 "tcl.rex" */
{ yyPrevious; 
} yyy1: goto yyBegin;
case 29:;
yySetPosition
/* line 157 "tcl.rex" */
{ yyPrevious; yyEol (0); first_word = rtrue; return 15; 
} yyy2: goto yyBegin;
case 12:;
yySetPosition
/* line 160 "tcl.rex" */
{ char * p = strchr (TokenPtr, '\n');
		    yyEol (TokenPtr + TokenLength - p - 1);
		    return 13;
		  
} yyy3: goto yyBegin;
case 46:;
yySetPosition
/* line 165 "tcl.rex" */
{ if (first_word && ! is_open (r_brace)) {
		       yyStart (Comment);
		    } else {
		       Attribute.character.text = '#';
		       first_word = rfalse; return 2;
		    }
		  
} yyy4: goto yyBegin;
case 45:;
yySetPosition
/* line 173 "tcl.rex" */
{ if (is_open ('"')) {
		       KPop = 'A'; goto L_KlPop;
		    } else {
		       push ('"'); first_word = rfalse; return 4;
		    }
		  
} yyy5: goto yyBegin;
case 44:;
yySetPosition
/* line 180 "tcl.rex" */
{ if (is_open (* TokenPtr)) {
		       KPop = * TokenPtr; goto L_KlPop;
		    } else {
		       Attribute.character.text = * TokenPtr;
		       first_word = rfalse; return 2;
		    }
		  
} yyy6: goto yyBegin;
case 42:;
yySetPosition
/* line 180 "tcl.rex" */
{ if (is_open (* TokenPtr)) {
		       KPop = * TokenPtr; goto L_KlPop;
		    } else {
		       Attribute.character.text = * TokenPtr;
		       first_word = rfalse; return 2;
		    }
		  
} yyy7: goto yyBegin;
case 41:;
yySetPosition
/* line 188 "tcl.rex" */
{ first_word = rtrue; return 15; 
} yyy8: goto yyBegin;
case 9:;
case 28:;
yySetPosition
/* line 190 "tcl.rex" */
{ yyEol (0); first_word = rtrue; return 15; 
} yyy9: goto yyBegin;
case 8:;
case 17:;
case 30:;
case 32:;
yySetPosition
/* line 192 "tcl.rex" */
{ return 13; 
} yyy10: goto yyBegin;
case 40:;
yySetPosition
/* line 194 "tcl.rex" */
{ push ('{'); first_word = rfalse; return 8; 
} yyy11: goto yyBegin;
case 39:;
yySetPosition
/* line 195 "tcl.rex" */
{ push ('['); first_word = rfalse; return 6; 
} yyy12: goto yyBegin;
case 38:;
yySetPosition
/* line 196 "tcl.rex" */
{ first_word = rfalse; return 11; 
} yyy13: goto yyBegin;
case 37:;
yySetPosition
/* line 197 "tcl.rex" */
{ first_word = rfalse; return 12; 
} yyy14: goto yyBegin;
case 36:;
yySetPosition
/* line 198 "tcl.rex" */
{ first_word = rfalse; return 3; 
} yyy15: goto yyBegin;
case 14:;
yySetPosition
/* line 199 "tcl.rex" */
{ first_word = rfalse; return 10; 
} yyy16: goto yyBegin;
case 35:;
yySetPosition
/* line 200 "tcl.rex" */
{ first_word = rfalse; return 10; 
} yyy17: goto yyBegin;
case 7:;
yySetPosition
/* line 202 "tcl.rex" */
{ Attribute.ident.ident = MakeIdent (TokenPtr, TokenLength);
		    first_word = rfalse; return 1;
		  
} yyy18: goto yyBegin;
case 11:;
case 19:;
case 24:;
yySetPosition
/* line 207 "tcl.rex" */
{ unsigned int i;
		    GetWord (word);
		    sscanf (word + 1, "%o", & i);
		    Attribute.character.text = i;
		    first_word = rfalse; return 2;
		  
} yyy19: goto yyBegin;
case 13:;
case 20:;
yySetPosition
/* line 215 "tcl.rex" */
{ unsigned int i;
		    GetWord (word);
		    sscanf (word + 2, "%x", & i);
		    Attribute.character.text = i;
		    first_word = rfalse; return 2;
		  
} yyy20: goto yyBegin;
case 10:;
case 18:;
case 23:;
yySetPosition
/* line 222 "tcl.rex" */
{ Attribute.character.text = TokenPtr [1];
		    first_word = rfalse; return 2;
		  
} yyy21: goto yyBegin;
case 6:;
case 16:;
case 22:;
case 34:;
yySetPosition
/* line 226 "tcl.rex" */
{ Attribute.character.text = * TokenPtr;
		    first_word = rfalse; return 2;
		  
} yyy22: goto yyBegin;
case 1:;
case 2:;
case 3:;
case 4:;
case 15:;
case 21:;
case 25:;
	 /* non final states */
	 yyChBufferIndexReg --;			/* return character */
	 break;

case 27:
	 yySetPosition
      /* TokenLength   = 1; */
	 yyChBufferIndex = ++ yyChBufferIndexReg;
	 {
/* line 138 "tcl.rex" */

   MessageI ("illegal character", xxError, Attribute.Position,
      xxCharacter, TokenPtr);

	 }
	 goto yyBegin;

      case yyDNoState:
	 goto yyBegin;

case 26:
	 yyChBufferIndex = -- yyChBufferIndexReg; /* undo last state transit */
	 if (-- TokenLength == 0) {		/* get previous state */
	    yyState = yyStartState;
	 } else {
	    yyState = * (yyStatePtr - 1);
	 }

	 if (yyChBufferIndex != & yyChBufferStart [yyBytesRead]) {
					/* end of buffer sentinel in buffer */
	    if ((yyState = yyEobTrans [yyState]) == yyDNoState) continue;
	    yyChBufferIndexReg ++;
	    * yyStatePtr ++ = yyState;		/* push state */
	    goto yyContinue;
	 }
						/* end of buffer reached */
	 if (! yyEof) {
	    register char * yySource;
	    register char * yyTarget;
	    unsigned long yyChBufferFree;

	    if (yyChBufferSize == 0) {
	       yyInitialize ();
	       yyChBufferIndexReg = yyChBufferIndex;
	    }
	    yySource = TokenPtr - 1;
	    yyTarget = (char *) & yyChBufferPtr
		[(yyMaxAlign - 1 - TokenLength) & (yyMaxAlign - 1)];
	    yyChBufferFree = Exp2 (Log2 (yyChBufferSize - 4 -
		yyMaxAlign - TokenLength));
		/* copy initial part of token in front of the input buffer */
	    if (yySource > yyTarget) {
	       TokenPtr = yyTarget + 1;
	       do * yyTarget ++ = * yySource ++;
	       while (yySource < (char *) yyChBufferIndexReg);
	       yyLineStart += (unsigned char *) yyTarget - yyChBufferStart -
				yyBytesRead;
	       yyChBufferStart = (unsigned char *) yyTarget;
	    } else {
	       yyChBufferStart = yyChBufferIndexReg;
	    }
	    yyChBufferStart2 = (char *) yyChBufferStart;
						/* extend buffer if necessary */
	    if (yyChBufferFree < yyChBufferSize >> 3 /* / 8 */ ) {
	       register unsigned long yyDelta;
	       register unsigned char * yyOldChBufferPtr = yyChBufferPtr;
	       ExtendArray ((char * *) & yyChBufferPtr, & yyChBufferSize,
				(unsigned long) sizeof (char));
	       if (yyChBufferPtr == NULL) yyErrorMessage (1);
	       yyDelta = yyChBufferPtr - yyOldChBufferPtr;
	       yyChBufferStart	+= yyDelta;
	       yyLineStart	+= yyDelta;
	       TokenPtr	+= yyDelta;
	       yyChBufferStart2	 = (char *) yyChBufferStart;
	       yyChBufferFree = Exp2 (Log2 (yyChBufferSize - 4 -
			yyMaxAlign - TokenLength));
	       if (yyStateStackSize < yyChBufferSize) {
		  yyStateRange * yyOldStateStack = yyStateStack;
		  ExtendArray ((char * *) & yyStateStack, & yyStateStackSize,
				   (unsigned long) sizeof (yyStateRange));
		  if (yyStateStack == NULL) yyErrorMessage (1);
		  yyStatePtr	+= yyStateStack - yyOldStateStack;
	       }
	    }
						/* read buffer and restart */
	    yyChBufferIndex = yyChBufferIndexReg = yyChBufferStart;
	    yyFileOffset += yyBytesRead;
	    yyBytesRead = GetLine (yySourceFile,
				(char *) yyChBufferIndex, (int) yyChBufferFree);
	    if (yyBytesRead <= 0) { yyBytesRead = 0; yyEof = rtrue; }
	    yyChBufferStart [yyBytesRead    ] = yyEobCh;
	    yyChBufferStart [yyBytesRead + 1] = '\0';
	    goto yyContinue;
	 }

	 if (TokenLength == 0) {		/* end of file reached */
	    yySetPosition
	    CloseFile ();
	    if (yyFileStackPtr == yyFileStack) {
/* line 126 "tcl.rex" */
 EOF_reached = rtrue; return 14; 
	    }
	    if (yyFileStackPtr == yyFileStack) return EofToken;
	    goto yyBegin;
	 }
	 break;

      default:
	 yyErrorMessage (0);
      }
   }
}

static void yyInitialize ARGS ((void))
   {
      if (yyFileStackSize == 0) {
	 yyStateStackSize = yyInitBufferSize;
	 MakeArray ((char * *) & yyStateStack, & yyStateStackSize,
		       (unsigned long) sizeof (yyStateRange));
	 if (yyStateStack == NULL) yyErrorMessage (1);
	 yyStateStack [0] = yyDefaultState;
	 yyFileStackSize = yyInitFileStackSize;
	 MakeArray ((char * *) & yyFileStack, & yyFileStackSize,
			(unsigned long) sizeof (yytFileStack));
	 if (yyFileStack == NULL) yyErrorMessage (1);
	 yyFileStackPtr = yyFileStack;
      }

      if (yyFileStackPtr >= yyFileStack + yyFileStackSize - 1) {
	 unsigned long yyyFileStackPtr = yyFileStackPtr - yyFileStack;
	 ExtendArray ((char * *) & yyFileStack, & yyFileStackSize,
			   (unsigned long) sizeof (yytFileStack));
	 if (yyFileStack == NULL) yyErrorMessage (1);
	 yyFileStackPtr = yyFileStack + yyyFileStackPtr;
      }
      yyFileStackPtr ++;			/* push file */
      yyFileStackPtr->yySourceFile	= yySourceFile		;
      yyFileStackPtr->yyEof		= yyEof			;
      yyFileStackPtr->yyChBufferPtr	= yyChBufferPtr		;
      yyFileStackPtr->yyChBufferStart	= yyChBufferStart	;
      yyFileStackPtr->yyChBufferSize	= yyChBufferSize	;
      yyFileStackPtr->yyChBufferIndex	= yyChBufferIndex	;
      yyFileStackPtr->yyBytesRead	= yyBytesRead		;
      yyFileStackPtr->yyFileOffset	= yyFileOffset		;
      yyFileStackPtr->yyLineCount	= yyLineCount		;
      yyFileStackPtr->yyLineStart	= yyLineStart		;
						/* initialize file state */
      yyChBufferSize	   = yyInitBufferSize;
      MakeArray ((char * *) & yyChBufferPtr, & yyChBufferSize,
			(unsigned long) sizeof (char));
      if (yyChBufferPtr == NULL) yyErrorMessage (1);
      yyChBufferStart	   = & yyChBufferPtr [yyMaxAlign];
      yyChBufferStart2	   = (char *) yyChBufferStart;
      yyChBufferStart [-1] = yyEolCh;		/* begin of line indicator */
      yyChBufferStart [ 0] = yyEobCh;		/* end of buffer sentinel */
      yyChBufferStart [ 1] = '\0';
      yyChBufferIndex	   = yyChBufferStart;
      TokenPtr	   = (char *) yyChBufferStart;
      yyEof		   = rfalse;
      yyBytesRead	   = 0;
      yyFileOffset	   = 0;
      yyLineCount	   = 1;
      yyLineStart	   = & yyChBufferStart [-1];
   }

void BeginFile
#if defined __STDC__ | defined __cplusplus
   (char * yyFileName)
#else
   (yyFileName) char * yyFileName;
#endif
   {
      yyInitialize ();
      yySourceFile = yyFileName == NULL ? StdIn : BeginSource (yyFileName);
      if (yySourceFile < 0) yyErrorMessage (3);
   }

void CloseFile ARGS ((void))
   {
      if (yyFileStackPtr == yyFileStack) yyErrorMessage (2);
      CloseSource (yySourceFile);
      ReleaseArray ((char * *) & yyChBufferPtr, & yyChBufferSize,
			(unsigned long) sizeof (char));
						/* pop file */
      yySourceFile	= yyFileStackPtr->yySourceFile		;
      yyEof		= yyFileStackPtr->yyEof			;
      yyChBufferPtr	= yyFileStackPtr->yyChBufferPtr		;
      yyChBufferStart	= yyFileStackPtr->yyChBufferStart	;
      yyChBufferStart2	= (char *) yyChBufferStart		;
      yyChBufferSize	= yyFileStackPtr->yyChBufferSize	;
      yyChBufferIndex	= yyFileStackPtr->yyChBufferIndex	;
      yyBytesRead	= yyFileStackPtr->yyBytesRead		;
      yyFileOffset	= yyFileStackPtr->yyFileOffset		;
      yyLineCount	= yyFileStackPtr->yyLineCount		;
      yyLineStart	= yyFileStackPtr->yyLineStart		;
      yyFileStackPtr --;
      if (yyFileStackPtr == yyFileStack) ResetScanner ();
   }

#ifdef xxGetWord
int GetWord
#if defined __STDC__ | defined __cplusplus
   (char * yyWord)
#else
   (yyWord) char * yyWord;
#endif
   {
      register char * yySource		= TokenPtr;
      register char * yyTarget			= yyWord;
      register char * yyChBufferIndexReg	= (char *) yyChBufferIndex;

      while (yySource < yyChBufferIndexReg)
	 * yyTarget ++ = * yySource ++;
      * yyTarget = '\0';
      return (int) (yyChBufferIndexReg - TokenPtr);
   }
#endif

#ifdef xxGetLower
int GetLower
#if defined __STDC__ | defined __cplusplus
   (char * yyWord)
#else
   (yyWord) char * yyWord;
#endif
   {
      register unsigned char * yySource	= (unsigned char *) TokenPtr;
      register unsigned char * yyTarget		= (unsigned char *) yyWord;
      register unsigned char * yyChBufferIndexReg = yyChBufferIndex;

      while (yySource < yyChBufferIndexReg)
	 * yyTarget ++ = yyToLower [* yySource ++];
      * yyTarget = '\0';
      return (int) (yyChBufferIndexReg - (unsigned char *) TokenPtr);
   }
#endif

#ifdef xxGetUpper
int GetUpper
#if defined __STDC__ | defined __cplusplus
   (char * yyWord)
#else
   (yyWord) char * yyWord;
#endif
   {
      register unsigned char * yySource	= (unsigned char *) TokenPtr;
      register unsigned char * yyTarget		= (unsigned char *) yyWord;
      register unsigned char * yyChBufferIndexReg = yyChBufferIndex;

      while (yySource < yyChBufferIndexReg)
	 * yyTarget ++ = yyToUpper [* yySource ++];
      * yyTarget = '\0';
      return (int) (yyChBufferIndexReg - (unsigned char *) TokenPtr);
   }
#endif

#ifdef xxinput
static char input ARGS ((void))
   {
      if (yyChBufferIndex == & yyChBufferStart [yyBytesRead]) {
	 if (! yyEof) {
	    yyLineStart -= yyBytesRead;
	    yyChBufferIndex = yyChBufferStart = yyChBufferPtr;
	    yyChBufferStart2 = (char *) yyChBufferStart;
	    yyFileOffset += yyBytesRead;
	    yyBytesRead = GetLine (yySourceFile, (char *) yyChBufferIndex,
	       (int) Exp2 (Log2 (yyChBufferSize)));
	    if (yyBytesRead <= 0) { yyBytesRead = 0; yyEof = rtrue; }
	    yyChBufferStart [yyBytesRead    ] = yyEobCh;
	    yyChBufferStart [yyBytesRead + 1] = '\0';
	 }
      }
      if (yyChBufferIndex == & yyChBufferStart [yyBytesRead]) return '\0';
      else return * yyChBufferIndex ++;
   }
#endif

static void unput
#if defined __STDC__ | defined __cplusplus
   (char yyc)
#else
   (yyc) char yyc;
#endif
   { * (-- yyChBufferIndex) = yyc; }

static void yyLess
#if defined __STDC__ | defined __cplusplus
   (int yyn)
#else
   (yyn) int yyn;
#endif
   { yyChBufferIndex -= TokenLength - yyn; TokenLength = yyn; }

void BeginScanner ARGS ((void))
   {
/* line 128 "tcl.rex" */

   EOF_reached	= rfalse;
   first_word	= rtrue;
   KlLvl	= 0;
   KPop		= 'X';
   KlStack [0]	= 'X';

   yyStart (STD);

   }

void CloseScanner ARGS ((void))
   {
   }

void ResetScanner ARGS ((void))
   {
      yyChBufferPtr	= yyInitChBuffer;
      yyChBufferSize	= 0;
      if (yyStateStackSize != 0)
	 ReleaseArray ((char * *) & yyStateStack, & yyStateStackSize,
			(unsigned long) sizeof (yyStateRange));
      yyStateStack	= yyInitStateStack;
      yyStateStackSize	= 0;
      if (yyFileStackSize != 0)
	 ReleaseArray ((char * *) & yyFileStack, & yyFileStackSize,
			(unsigned long) sizeof (yytFileStack));
      yyFileStackSize	= 0;
#if defined xxyyPush | defined xxyyPop
      if (yyStStStackSize != 0)
	 ReleaseArray ((char * *) & yyStStStackPtr, & yyStStStackSize,
			(unsigned long) sizeof (yyStateRange));
      yyStStStackSize	= 0;
      yyStStStackIdx	= 0;
#endif
      yyStartState	= STD;
      yyPreviousStart	= STD;
      yySourceFile	= StdIn;
   }

static void yyErrorMessage
#if defined __STDC__ | defined __cplusplus
   (int yyErrorCode)
#else
   (yyErrorCode) int yyErrorCode;
#endif
   {
      WritePosition (stderr, Attribute.Position);
      switch (yyErrorCode) {
      case 0: (void) fprintf (stderr, ": Scanner: internal error\n"); break;
      case 1: (void) fprintf (stderr, ": Scanner: out of memory\n"); break;
      case 2: (void) fprintf (stderr,
      ": Scanner: file stack underflow (too many calls of CloseFile)\n");
	      break;
      case 3: (void) fprintf (stderr,
	      ": Scanner: cannot open input file\n"); break;
      case 4: (void) fprintf (stderr,
      ": Scanner: start stack underflow (too many calls of yyPop)\n"); break;
      }
      Scanner_Exit ();
   }

