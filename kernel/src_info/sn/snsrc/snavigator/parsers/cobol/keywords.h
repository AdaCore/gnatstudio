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

/* Ich, Doktor Josef Grosch, Informatiker, Nov. 1994 */

#include "Idents.h"

#define ans74	0x00000001L
#define ans85	0x00000002L
#define os	0x00000004L
#define vs2	0x00000008L
#define vs3	0x00000010L
#define vs4	0x00000020L
#define sa1	0x00000040L
#define sa2	0x00000080L
#define x	0x00000100L
#define mf1	0x00000200L
#define mf2	0x00000400L
#define mf3	0x00000800L
#define mf4	0x00001000L
#define mf5	0x00002000L
#define mf6	0x00004000L
#define mf7	0x00008000L
#define mf8	0x00010000L
#define ms	0x00020000L
#define rm	0x00040000L
#define c370	0x00080000L
#define dvs	0x00100000L
#define er	0x00200000L

#define copy_replace	0x80000000L

#define vs	vs2 | vs3 | vs4
#define sa	sa1 | sa2
#define mf	mf1 | mf2 | mf3 | mf4 | mf5 | mf6 | mf7 | mf8
#define all	ans74 | ans85 | os | vs | sa | x | mf | ms | rm | dvs

typedef struct { unsigned long mask; short code; } tkeyword;
typedef struct { char * option; unsigned long mask; } toption;

extern unsigned long	dialect		;
extern tkeyword		keywords	[];
extern tIdent		max_keyword	;
extern toption		options		[];
extern char		keyword2stat	[];

extern void		init_keywords	ARGS ((void));

