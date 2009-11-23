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

#ifndef yyIdents
#define yyIdents

/* $Id$ */

/*
 * $Log$
 * Revision 1.1  2002/02/21 14:13:00  taras
 * Added SN sources
 *
 * Revision 1.1.1.1  2002/01/23 08:26:01  taras
 * Imported sources
 *
 * Revision 1.2  2000/04/20 00:38:40  spolk
 * 2000-04-19  Syd Polk  <spolk@redhat.com>
 *
 * 	* Merged from snavigator-elix-990915-branch.
 *
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:43  spolk
 * 2000-02-11  Syd Polk  <spolk@cygnus.com>
 *
 * 	* configure.in: Added SUITE_NAME
 * 	* config.h.in configure: Regenerated.
 * 	* bitmaps/splashsn.gif: Added Shadow Man.
 * 	* english.txt.in: Added SUITE_NAME
 * 	* hyper/tclsql.c: Added SUITE_NAME
 * 	* gui/misc.tcl: Mucked around with text in About Box.
 * 	Changed all copyrights from "Red Hat Source-Navigator" to
 * 	"Source-Navigator"
 * 	Regenerated all Makefile.in.
 *
 * Revision 1.1.1.1.18.1  2000/02/10 02:12:55  spolk
 * 2000-02-09  Syd Polk  <spolk@cygnus.com>
 *
 * 	* configure.in: More adjustments to product names and the like.
 * 	Got rid of --enable-production.
 * 	* configure: Regenerated.
 * 	* install/cdkey_mangle.c: Removed.
 * 	Added GPL copyright notice to all source files.
 * 	Regenerated all Makefile.in files.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:12  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:46  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.17  1997/11/30 22:29:41  grosch
 * eliminated use of type cardinal
 * added function GetCStr
 *
 * Revision 1.16  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.15  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.14  1996/07/25  16:51:34  grosch
 * adaption to MS VC++
 *
 * Revision 1.13  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.12  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.11  1995/02/23  20:11:10  grosch
 * renamed Positions to Position, StringMem to StringM, Relations to Relation
 * output lines with at most 132 characters
 *
 * Revision 1.10  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.9  1994/06/13  09:36:11  grosch
 * cosmetic changes
 *
 * Revision 1.8  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.7  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/09/18  15:18:47  grosch
 * added procedure GetStringRef
 * 
 * Revision 1.4  91/07/17  17:23:08  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.3  91/01/21  12:13:21  grosch
 * some performance improvements
 * 
 * Revision 1.2  90/07/04  14:33:56  grosch
 * introduced conditional include
 * 
 * Revision 1.1  89/12/08  17:22:12  grosch
 * added variable NoIdent
 * 
 * Revision 1.0  88/10/04  11:44:39  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include <stdio.h>
#include "ratc.h"
#include "StringM.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

typedef long	tIdent;

extern	tIdent	NoIdent; /* A default identifier (empty string)		*/

extern	tIdent	MakeIdent	ARGS ((register char * yystring,
				       register int yylength));
			/* The string (of length) is mapped to a unique	*/
			/* identifier (an integer) which is returned.	*/

extern	void	GetString	ARGS ((tIdent yyident, char * yystring));
			/* Returns the string whose identifier is 'ident'.*/

extern	tStringRef GetStringRef ARGS ((tIdent yyident));
			/* Returns a reference to the string identified	*/
			/* by 'ident'.					*/

extern	char *	GetCStr		ARGS ((tIdent yyident));
			/* Returns the address of the string identified	*/
			/* by 'ident'.					*/

extern	tIdent	MaxIdent	ARGS ((void));
			/* Returns the currently maximal identifier.	*/

extern	void	WriteIdent	ARGS ((FILE * yyfile, tIdent yyident));
			/* The string encoded by the identifier 'ident'	*/
			/* is printed on the file.			*/

extern	void	WriteIdents	ARGS ((void));
			/* The contents of the identifier table is	*/
			/* printed on the standard output.		*/

extern	void	InitIdents	ARGS ((void));
			/* The identifier table is initialized.		*/

extern	void	WriteHashTable	ARGS ((void));

#endif

