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

/* line 21 "cobol.rex" */

#include "ratc.h"
#include "Position.h"
#include "StringM.h"
#include "Idents.h"

extern	rbool	ansi_copy		;
extern	rbool	IsDebugging		;
extern	rbool	decimal_point_is_comma	;

extern	void	begin_replacing		ARGS ((void));
extern	void	end_replacing		ARGS ((void));
extern	void	start_pseudo_text	ARGS ((void));
extern	void	Start_Comment_Entry	ARGS ((void));

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

typedef struct { tPosition zzPos; tPosition EPos; tIdent Ident; } zzname;
typedef struct { tPosition zzPos; tPosition EPos; tIdent Ident; } zzparagraph_name;
typedef struct { tPosition zzPos; tPosition EPos; long Value; } zzunsigned_integer;
typedef struct { tPosition zzPos; tPosition EPos; long Value; } zzplus_integer;
typedef struct { tPosition zzPos; tPosition EPos; long Value; } zzminus_integer;
typedef struct { tPosition zzPos; tPosition EPos; int Value; } zzlevel_number;
typedef struct { tPosition zzPos; tPosition EPos; tStringRef Value; } zzreal;
typedef struct { tPosition zzPos; tPosition EPos; tStringRef Value; } zzstring;
typedef struct { tPosition zzPos; tPosition EPos; tStringRef Value; } zzpseudo_text;
typedef struct { tPosition zzPos; tPosition EPos; tStringRef Value; } zzpicture_string;
typedef struct { tPosition zzPos; tPosition EPos; char Value; } zzillegal_character;

typedef union {
tPosition Position;
zzname name;
zzparagraph_name paragraph_name;
zzunsigned_integer unsigned_integer;
zzplus_integer plus_integer;
zzminus_integer minus_integer;
zzlevel_number level_number;
zzreal real;
zzstring string;
zzpseudo_text pseudo_text;
zzpicture_string picture_string;
zzillegal_character illegal_character;
} tScanAttribute;

extern void ErrorAttribute ARGS((int Token, tScanAttribute * pAttribute));


 
#define EofToken	0
#define xxGetWord
#define xxGetUpper
 
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

