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

#ifndef yyScanner
#define yyScanner

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* line 1 "java.rex" */

#include "Position.h"
#include "StringM.h"
#include "Idents.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

typedef struct { tPosition zzPos; tIdent ident; } zzIdentifier;
typedef struct { tPosition zzPos; long value; } zzIntegerLiteral;
typedef struct { tPosition zzPos; long value; } zzLongLiteral;
typedef struct { tPosition zzPos; tStringRef value; } zzFloatingPointLiteral;
typedef struct { tPosition zzPos; tStringRef value; } zzDoubleLiteral;
typedef struct { tPosition zzPos; tStringRef value; } zzCharacterLiteral;
typedef struct { tPosition zzPos; tStringRef value; } zzStringLiteral;

typedef union {
tPosition Position;
zzIdentifier Identifier;
zzIntegerLiteral IntegerLiteral;
zzLongLiteral LongLiteral;
zzFloatingPointLiteral FloatingPointLiteral;
zzDoubleLiteral DoubleLiteral;
zzCharacterLiteral CharacterLiteral;
zzStringLiteral StringLiteral;
} tScanAttribute;

extern void ErrorAttribute ARGS((int Token, tScanAttribute * pAttribute));


 
#define EofToken	0
 
#ifdef lex_interface
#define GetToken	yylex
#define TokenLength	yyleng
#endif

extern	char *		TokenPtr	;
extern	int		TokenLength	;
extern	tScanAttribute	Attribute	;
extern	void		(* Scanner_Exit)	ARGS ((void));
 
extern	void		BeginScanner	ARGS ((void));
extern	void		BeginFile	ARGS ((char * yyFileName));
extern	int		GetToken	ARGS ((void));
#ifdef xxGetWord
extern	int		GetWord	ARGS ((char * yyWord));
#endif
#ifdef xxGetLower
extern	int		GetLower	ARGS ((char * yyWord));
#endif
#ifdef xxGetUpper
extern	int		GetUpper	ARGS ((char * yyWord));
#endif
extern	void		CloseFile	ARGS ((void));
extern	void		CloseScanner	ARGS ((void));

#endif

