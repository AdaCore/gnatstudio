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

#ifndef yyPosition
#define yyPosition

/* $Id$ */

/* $Log$
/* Revision 1.1  2002/02/21 14:13:00  taras
/* Added SN sources
/*
/* Revision 1.1.1.1  2002/01/23 08:26:01  taras
/* Imported sources
/*
/* Revision 1.2  2000/04/20 00:38:40  spolk
/* 2000-04-19  Syd Polk  <spolk@redhat.com>
/*
/* 	* Merged from snavigator-elix-990915-branch.
/*
/* Revision 1.1.1.1.18.2  2000/02/11 23:54:45  spolk
/* 2000-02-11  Syd Polk  <spolk@cygnus.com>
/*
/* 	* configure.in: Added SUITE_NAME
/* 	* config.h.in configure: Regenerated.
/* 	* bitmaps/splashsn.gif: Added Shadow Man.
/* 	* english.txt.in: Added SUITE_NAME
/* 	* hyper/tclsql.c: Added SUITE_NAME
/* 	* gui/misc.tcl: Mucked around with text in About Box.
/* 	Changed all copyrights from "Red Hat Source-Navigator" to
/* 	"Source-Navigator"
/* 	Regenerated all Makefile.in.
/*
/* Revision 1.1.1.1.18.1  2000/02/10 02:12:55  spolk
/* 2000-02-09  Syd Polk  <spolk@cygnus.com>
/*
/* 	* configure.in: More adjustments to product names and the like.
/* 	Got rid of --enable-production.
/* 	* configure: Regenerated.
/* 	* install/cdkey_mangle.c: Removed.
/* 	Added GPL copyright notice to all source files.
/* 	Regenerated all Makefile.in files.
/*
/* Revision 1.1.1.1  1998/03/16 18:39:12  khamis
/* Souce-Navigator in a new devo tree
/*
/* Revision 1.4  1998/01/27 12:20:48  zkoppany
/* Modifications for the Tcl parser.
/*
 * Revision 1.1  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.0  1992/08/07  14:31:43  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

#include <stdio.h>
#include "Idents.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

typedef struct { unsigned short Line, Column; tIdent FileName; } tPosition;

extern tPosition NoPosition;
			/* A default position (0, 0).			*/

extern int  Compare	  ARGS((tPosition yyPosition1, tPosition yyPosition2));
			/* Returns -1 if Position1 < Position2.		*/
			/* Returns  0 if Position1 = Position2.		*/
			/* Returns  1 if Position1 > Position2.		*/

extern void WritePosition ARGS((FILE * yyFile, tPosition yyPosition));
			/* The 'Position' is printed on the 'File'.	*/

extern void ReadPosition ARGS((FILE * yyFile, tPosition * yyPosition));
			/* The 'Position' is read from the 'File'.	*/

#endif

