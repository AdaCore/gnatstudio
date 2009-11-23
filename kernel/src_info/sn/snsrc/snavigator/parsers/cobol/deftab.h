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

#ifndef yydeftab
#define yydeftab

/* Ich, Doktor Josef Grosch, Informatiker, Nov. 1994 */

#include "Position.h"
#include "Idents.h"
#include "Scanner.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

#define IB	00	/* INDEXED BY of data description	*/
#define MN	90	/* mnemonic  name of SPECIAL-NAMES	*/
#define CN	91	/* condition name of SPECIAL-NAMES	*/
#define FD	92
#define SD	93
#define CD	94
#define RD	95
#define PN	96	/* program name of PROGRAM-ID		*/

typedef struct tdecl {
   tPosition	position;
   tPosition	end_pos	;
   tPosition	e_pos	;
   char		level	;
   tIdent	name	;
   struct tdecl *next	;
   struct tdecl *fields	;
} tdecl, * tpdecl;

typedef struct tuse {
   tIdent	name	;
   tPosition	position;
   tPosition	end_pos	;
   struct tuse * next	;
   struct tuse * fields	;
   char		section	;
} tuse, * tpuse;

typedef struct tlabel {
   tIdent	name	;
   tIdent	name2	;
   tpdecl	scope	;
   tPosition	position;
   tPosition	end_pos	;
   tPosition	e_pos	;
   struct tlabel * next	;
} tlabel, * tplabel;

extern	char	Section		;
extern	rbool	NameCheck	;
extern	int	acc		;

extern	void	BeginDeftab	ARGS ((void));
extern	tpdecl	Declare		ARGS ((int level, tScanAttribute Attribute, int Type, tPosition));
extern	tpdecl	DeclareLabel	ARGS ((tScanAttribute Attribute, int Type, tPosition));
extern	void	DeclareEnd	ARGS ((int Type, tPosition));
extern	tpdecl	UseName		ARGS ((tScanAttribute Attribute));
extern	tpdecl	UseField	ARGS ((tScanAttribute Attribute, tpdecl decls));
extern	void	UseForward	ARGS ((tScanAttribute Attribute));
extern	void	UseFieldForward	ARGS ((tScanAttribute Attribute));
extern	void	UseLabel	ARGS ((tScanAttribute Attribute));
extern	void	UseLabel2	ARGS ((tScanAttribute Attribute, tScanAttribute Attribute2));
extern	void	UseLabelExtern	ARGS ((tScanAttribute Attribute));
extern	void	CloseDeftab	ARGS ((void));
extern	void	ReleaseDeftab	ARGS ((void));
extern	void	PutDeftab	ARGS ((void));

#endif

