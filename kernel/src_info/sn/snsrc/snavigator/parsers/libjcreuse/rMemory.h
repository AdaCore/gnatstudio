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

#ifndef yyrMemory
#define yyrMemory

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
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:50  spolk
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
 * Revision 1.4  1998/01/27 12:21:02  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.14  1997/12/05 12:45:27  grosch
 * fixed silly bug at: #define Free rmFree
 *
 * Revision 1.13  1997/11/30 22:31:56  grosch
 * eliminated use of type cardinal
 * added function CloserMemory
 *
 * Revision 1.12  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.11  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.10  1996/09/17  08:57:48  grosch
 * added procedure WriteMemory
 *
 * Revision 1.9  1996/08/06  13:26:45  grosch
 * adaptions to MS Visual C++ 1.52
 *
 * Revision 1.8  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.7  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.6  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.5  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.4  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/07/17  17:23:14  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.2  90/12/14  15:55:53  grosch
 * introduced variable MemoryUsed
 * 
 * Revision 1.1  90/07/04  14:34:00  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:42  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* avoid linker problems with MS VC++ */
#ifdef _MSC_VER
#define Free rmFree
#endif

#define BeginrMemory InitrMemory

extern unsigned long MemoryUsed	;
			/* Holds the total amount of memory managed by	*/
			/* this module.					*/

extern void	InitrMemory	ARGS ((void));
			/* The memory module is initialized.		*/

extern char *	Alloc		ARGS ((register unsigned long yyByteCount));
			/* Returns a pointer to dynamically allocated	*/
			/* space of size 'ByteCount' bytes.		*/

extern void	Free		ARGS ((unsigned long yyByteCount, char * yya));
			/* The dynamically allocated space starting at	*/
			/* address 'a' of size 'ByteCount' bytes is	*/
			/* released.					*/

extern void	CloserMemory	ARGS ((void));
			/* All memory managed by this module is		*/
			/* released.					*/

extern void	WriteMemory	ARGS ((void));

#endif

