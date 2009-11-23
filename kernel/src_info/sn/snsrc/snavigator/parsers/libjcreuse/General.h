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

#ifndef yyGeneral
#define yyGeneral

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
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:42  spolk
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
 * Revision 1.4  1998/01/27 12:20:43  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.11  1997/11/30 22:28:06  grosch
 * eliminate use of type cardinal
 *
 * Revision 1.10  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.9  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.8  1996/06/05  12:00:48  grosch
 * adaption to MS VC++
 *
 * Revision 1.7  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.6  1993/10/28  13:34:32  grosch
 * improved storage allocation and alignment handling
 *
 * Revision 1.5  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.4  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/07/17  17:23:06  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.2  90/09/04  17:32:09  grosch
 * automatic determination of alignment
 * 
 * Revision 1.1  90/07/04  14:33:54  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:37  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "ratc.h"

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

typedef struct { char yychar; double yydouble; } yyForAlign;

#define yyMaxAlign	(sizeof (yyForAlign) - sizeof (double))

#define yyAlignedSize(size) ((size + yyMaxAlign - 1) & ~ (long) (yyMaxAlign - 1))

#define Min(a,b) ((a <= b) ? a : b)
			/* Returns the minimum of 'a' and 'b'.		*/
#define Max(a,b) ((a >= b) ? a : b)
			/* Returns the maximum of 'a' and 'b'.		*/

extern unsigned long	Log2 ARGS ((register unsigned long yyx));
			/* Returns the logarithm to the base 2 of 'x'.	*/
extern unsigned long	Exp2 ARGS ((register unsigned long yyx));
			/* Returns 2 to the power of 'x'.		*/

#endif

