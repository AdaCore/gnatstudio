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

#ifndef yyStringM
#define yyStringM

/* $Id$ */

/*
 * $Log$
 * Revision 1.1  2002/02/21 14:13:01  taras
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
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:48  spolk
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
 * Revision 1.1.1.1  1998/03/16 18:39:13  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:59  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.15  1997/12/05 12:44:46  grosch
 * added function CloseStringMemory
 *
 * Revision 1.14  1997/11/30 22:31:05  grosch
 * eliminated use of type cardinal
 * added function StGetCStr
 * added null character to strings
 *
 * Revision 1.13  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.12  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.11  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.10  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.9  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.8  1994/04/05  09:00:35  grosch
 * added prefix 'r' to true and false for improved portability
 *
 * Revision 1.7  1994/01/29  22:13:50  grosch
 * renamed bool to rbool
 *
 * Revision 1.6  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.5  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.4  1992/01/30  13:12:51  grosch
 * complete redesign: pointer instead of array index
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  91/07/17  17:23:48  grosch
 * introduced ARGS trick for ANSI compatibility
 *
 * Revision 1.1  90/07/04  14:34:08  grosch
 * introduced conditional include
 *
 * Revision 1.0  88/10/04  11:44:47  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include <stdio.h>
#include "ratc.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

#define BeginStringMemory InitStringMemory

typedef unsigned short * tStringRef;

extern	tStringRef PutString	ARGS ((register char * yys,
				       register unsigned long yylength));
			/* Stores string 's' in the string memory and	*/
			/* returns a handle to the stored string.	*/

extern	void	StGetString	ARGS ((register tStringRef yyr,
				       register char * yys));
			/* Returns the string 's' from the string	*/
			/* memory having the handle 'r'.		*/

/* extern unsigned short LengthSt ARGS ((register tStringRef yyr)); */
#define LengthSt(stringref) (* stringref)
#define StLength(stringref) (* stringref)
			/* Returns the length of the string having	*/
			/* the handle 'r'.				*/

/* extern char * StGetCStr	ARGS ((register tStringRef yyr)); */
#define StGetCStr(stringref) ((char *) stringref + sizeof (unsigned short))
			/* Returns the address of the string having	*/
			/* the handle 'r'.				*/

extern	rbool	IsEqualSt	ARGS ((tStringRef yyr, register char * yys));
			/* Compares the string having the handle 'r'	*/
			/* and the C string 's'.			*/
			/* Returns rtrue if both are equal.		*/
			/* Works only, if both strings have the same	*/
			/* length. This has to be checked before.		*/

extern	void	WriteString	ARGS ((FILE * yyf, tStringRef yyr));
			/* The string having the handle 'r' is printed	*/
			/* on the file 'f'.				*/

extern	void	WriteStringMemory ARGS ((void));
			/* The contents of the string memory is printed	*/
			/* on standard output.				*/

extern	void	InitStringMemory ARGS ((void));
			/* The string memory is initialized.		*/

extern	void	CloseStringMemory ARGS ((void));
			/* The string memory is finalized.		*/

#endif

