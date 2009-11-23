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

/*
 * $Log$
 * Revision 1.1  2002/02/21 14:13:00  taras
 * Added SN sources
 *
 * Revision 1.1.1.1  2002/01/23 08:26:01  taras
 * Imported sources
 *
 * Revision 1.3  2000/04/20 00:38:40  spolk
 * 2000-04-19  Syd Polk  <spolk@redhat.com>
 *
 * 	* Merged from snavigator-elix-990915-branch.
 *
 * Revision 1.2.16.2  2000/02/11 23:54:42  spolk
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
 * Revision 1.2.16.1  2000/02/10 02:12:55  spolk
 * 2000-02-09  Syd Polk  <spolk@cygnus.com>
 *
 * 	* configure.in: More adjustments to product names and the like.
 * 	Got rid of --enable-production.
 * 	* configure: Regenerated.
 * 	* install/cdkey_mangle.c: Removed.
 * 	Added GPL copyright notice to all source files.
 * 	Regenerated all Makefile.in files.
 *
 * Revision 1.2  1998/05/21 04:51:01  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:12  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:41  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.11  1997/11/30 22:28:06  grosch
 * eliminate use of type cardinal
 *
 * Revision 1.10  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.9  1995/03/20 15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.8  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.7  1993/10/28  13:34:32  grosch
 * improved storage allocation and alignment handling
 *
 * Revision 1.6  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.5  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.4  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  90/09/04  17:32:08  grosch
 * automatic determination of alignment
 * 
 * Revision 1.1  90/07/04  14:33:53  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:37  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "ratc.h"
#include "General.h"

unsigned long Log2	/* Returns the logarithm to the base 2 of 'x'.	*/
#if defined __STDC__ | defined __cplusplus
   (register unsigned long x)
#else
   (x) register unsigned long x;
#endif
   {
      register unsigned long y = 0;

      if (x >= 65536) { y += 16; x >>= 16; }
      if (x >=   256) { y +=  8; x >>=  8; }
      if (x >=    16) { y +=  4; x >>=  4; }
      if (x >=     4) { y +=  2; x >>=  2; }
      if (x >=     2) { y +=  1;           }
      return y;
   }

unsigned long Exp2	/* Returns 2 to the power of 'x'.		*/
#if defined __STDC__ | defined __cplusplus
   (register unsigned long x)
#else
   (x) register unsigned long x;
#endif
   {
      register long y = 1;

      if (x >= 16) { x -= 16; y <<= 16; }
      if (x >=  8) { x -=  8; y <<=  8; }
      if (x >=  4) { x -=  4; y <<=  4; }
      if (x >=  2) { x -=  2; y <<=  2; }
      if (x >=  1) {          y <<=  1; }
      return y;
   }

