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

#ifndef yyDynArray
#define yyDynArray

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
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:40  spolk
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
 * Revision 1.5  1998/01/27 12:20:37  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.10  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.9  1997/05/10 18:19:14  grosch
 * fixed problems with static arrays which are increased using ExtendArray
 *
 * Revision 1.8  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.7  1995/06/26  15:59:41  grosch
 * added functions ResizeArray and ShrinkArray
 *
 * Revision 1.6  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.5  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.4  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  91/07/17  17:23:02  grosch
 * introduced ARGS trick for ANSI compatibility
 *
 * Revision 1.1  90/07/04  14:33:52  grosch
 * introduced conditional include
 *
 * Revision 1.0  88/10/04  11:44:36  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* allocate a static array for later extension with ExtendArray where
   ElmtSize == AlignedSize (ElmtSize)
   ASSERT ElmtCount * ElmtSize % sizeof (long) == 0 */

#define CreatePtr(Ptr, Array, Type)					\
	static Type * Ptr = & Array.yyArray [0];

#define CreateArray1(Ptr, Array, Type, ElmtCount, Init)		\
	static struct { double yyAlign; Type yyArray [ElmtCount]; }	\
	Array = { 0.0, Init };						\
	CreatePtr (Ptr, Array, Type)

#define CreateArray1a(Ptr, Array, Type, ElmtCount)			\
	static struct { double yyAlign; Type yyArray [ElmtCount]; } Array

#define CreateArray1b(Ptr, Array, Type, ElmtCount)			\
	CreatePtr (Ptr, Array, Type)

/* allocate a static array for later extension with ExtendArray where
   ElmtSize != AlignedSize (ElmtSize), ElmtSize > yyMaxAlign */

#define CreateArray2(Ptr, Array, Type, ElmtCount, Init)		\
	static struct { double yyAlign; Type yyArray			\
	[(ElmtCount + 1) * yyAlignedSize (sizeof (Type)) / sizeof (Type)]; } \
	Array = { 0.0, Init }; 						\
	CreatePtr (Ptr, Array, Type)

#define CreateArray2a(Ptr, Array, Type, ElmtCount)			\
	static struct { double yyAlign; Type yyArray			\
	[(ElmtCount + 1) * yyAlignedSize (sizeof (Type)) / sizeof (Type)]; } \
	Array

#define CreateArray2b(Ptr, Array, Type, ElmtCount)			\
	CreatePtr (Ptr, Array, Type)

extern void MakeArray    ARGS ((char * *	yyArrayPtr,
				unsigned long *	yyElmtCount,
				unsigned long	yyElmtSize));
			/* 'ArrayPtr' is set to the start address of a	*/
			/* memory space to hold an array of 'ElmtCount' */
			/* elements each of size 'ElmtSize' bytes.	*/

extern void ResizeArray  ARGS ((char * *	yyArrayPtr,
				unsigned long *	yyOldElmtCount,
				unsigned long	yyNewElmtCount,
				unsigned long	yyElmtSize));
			/* The memory space for the array is changed	*/
			/* to 'NewElmtCount' elements.			*/

extern void ExtendArray  ARGS ((char * *	yyArrayPtr,
				unsigned long *	yyElmtCount,
				unsigned long	yyElmtSize));
			/* The memory space for the array is increased	*/
			/* by doubling the number of elements.		*/

extern void ShrinkArray  ARGS ((char * *	yyArrayPtr,
				unsigned long *	yyElmtCount,
				unsigned long	yyElmtSize));
			/* The memory space for the array is reduced	*/
			/* by halving the number of elements.		*/

extern void ReleaseArray ARGS ((char * *	yyArrayPtr,
				unsigned long *	yyElmtCount,
				unsigned long	yyElmtSize));
			/* The memory space for the array is released.	*/

#endif

