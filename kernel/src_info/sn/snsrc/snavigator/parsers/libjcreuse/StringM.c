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
 * Revision 1.1  2002/02/21 14:13:01  taras
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
 * Revision 1.2.16.2  2000/02/11 23:54:46  spolk
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
 * Revision 1.2  1998/05/21 04:51:06  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:13  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:57  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.31  1997/12/16 18:08:39  grosch
 * tribute to MS VC++ 5.0
 *
 * Revision 1.30  1997/12/05 12:44:46  grosch
 * added function CloseStringMemory
 *
 * Revision 1.29  1997/11/30 22:31:05  grosch
 * eliminated use of type cardinal
 * added function StGetCStr
 * added null character to strings
 *
 * Revision 1.28  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.27  1997/05/06 10:16:44  grosch
 * fixed problem of adjusting Size when calling Free
 * disabled Free because of Speicherverschnitt
 *
 * Revision 1.26  1997/04/08 13:37:48  grosch
 * added prefix yy to MemorySize
 *
 * Revision 1.25  1996/08/14  10:37:24  grosch
 * adaption to DLL's for Microsoft Visual C++
 *
 * Revision 1.24  1996/06/05  12:00:48  grosch
 * adaption to MS VC++
 *
 * Revision 1.23  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.22  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.21  1995/02/23  20:11:46  grosch
 * renamed Positions to Position, StringMem to StringM, Relations to Relation
 * output lines with at most 132 characters
 *
 * Revision 1.20  1994/12/31  09:51:40  grosch
 * adaption to HP cc
 *
 * Revision 1.19  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.18  1994/11/07  12:39:10  grosch
 * fixed bug at freeing of rest of memory block
 *
 * Revision 1.17  1994/06/13  09:37:20  grosch
 * fix alignment for DEC Alpha
 *
 * Revision 1.16  1994/04/05  09:00:35  grosch
 * added prefix 'r' to true and false for improved portability
 *
 * Revision 1.15  1994/01/29  22:13:50  grosch
 * renamed bool to rbool
 *
 * Revision 1.14  1993/10/28  13:34:32  grosch
 * improved storage allocation and alignment handling
 *
 * Revision 1.13  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.12  1993/01/16  11:22:26  grosch
 * adaption to machines where int takes 2-bytes
 *
 * Revision 1.11  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.10  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.9  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.8  1992/01/30  13:12:51  grosch
 * complete redesign: pointer instead of array index
 *
 * Revision 1.7  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/07/17  17:23:46  grosch
 * introduced ARGS trick for ANSI compatibility
 *
 * Revision 1.4  91/01/21  12:13:25  grosch
 * some performance improvements
 *
 * Revision 1.3  90/09/20  09:12:28  grosch
 * calmed down lint
 *
 * Revision 1.2  90/07/04  14:34:07  grosch
 * introduced conditional include
 *
 * Revision 1.1  89/08/23  16:04:27  grosch
 * bug fix in PutString: stringReg initialized later
 *
 * Revision 1.0  88/10/04  11:44:47  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "Reuse.h"
#include "StringM.h"
#include "General.h"
#include "rMemory.h"

#define yyMemorySize	1024 * 20

typedef struct sBlock {
   struct sBlock *	Next;
   tStringRef		Last;
   unsigned long	Size;
   char			Memory [yyMemorySize];
} tBlock;

struct lBlock {
   struct lBlock *	Next;
   tStringRef		Last;
   unsigned long	Size;
};

static	tBlock *	BlockList	= NULL;
static	unsigned long	MemorySpaceLeft	= 0;
static	tStringRef	MemoryFreePtr;

tStringRef PutString
#if defined __STDC__ | defined __cplusplus
   (register char * string, register unsigned long length)
#else
   (string, length)
   register char *		string;
   register unsigned long	length;
#endif
   {
      register char *	stringReg;
      register unsigned long NeededSpace = (length + 4) & 0xfffffffe;
      register tStringRef StartPtr;

      if (MemorySpaceLeft < NeededSpace) {
	 tBlock * BlockPtr;
	 if (BlockList != NULL) {
	 /* this would produce Speicherverschnitt
	    char * FreePtr = (char *) yyAlignedSize ((long) MemoryFreePtr);
	    long Rest = MemorySpaceLeft - (FreePtr - (char *) MemoryFreePtr);
	    if (Rest >= (long) sizeof (char *)) {
	       Free ((unsigned long) Rest, FreePtr);
	       BlockList->Size -= Rest;
	    }
	 */
	    BlockList->Last = MemoryFreePtr;
	 }
	 MemorySpaceLeft = Max (NeededSpace, yyMemorySize);
	 BlockPtr = (tBlock *) Alloc (MemorySpaceLeft + sizeof (struct lBlock));
	 BlockPtr->Size = MemorySpaceLeft;
	 BlockPtr->Next = BlockList;
	 BlockList = BlockPtr;
	 MemoryFreePtr = (tStringRef) BlockPtr->Memory;
      }
      StartPtr = MemoryFreePtr;
      * StartPtr = (unsigned short) length;
      stringReg = (char *) MemoryFreePtr + sizeof (unsigned short);
      while (length -- > 0) * stringReg ++ = * string ++;
      * stringReg = '\0';
      MemorySpaceLeft -= NeededSpace;
      MemoryFreePtr   += NeededSpace >> 1;
      return StartPtr;
   }

void StGetString
#if defined __STDC__ | defined __cplusplus
   (register tStringRef stringref, register char * string)
#else
   (stringref, string)
   register tStringRef	stringref;
   register char *	string;
#endif
   {
      (void) memcpy (string, StGetCStr (stringref), StLength (stringref));
      string [StLength (stringref)] = '\0';
   }

rbool IsEqualSt
#if defined __STDC__ | defined __cplusplus
   (tStringRef stringref, register char * string)
#else
   (stringref, string)
   tStringRef		stringref;
   register char *	string;
#endif
   {
      register long	length	  = LengthSt (stringref);
      register char *	stringReg = StGetCStr (stringref);

      while (length -- > 0)
	 if (* stringReg ++ != * string ++)
	    return rfalse;
      return rtrue;
   }

void WriteString
#if defined __STDC__ | defined __cplusplus
   (FILE * file, tStringRef stringref)
#else
   (file, stringref)
   FILE *	file;
   tStringRef	stringref;
#endif
   {
      (void) fputs (StGetCStr (stringref), file);
   }

void WriteStringMemory
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
#ifndef _USRDLL
      tBlock *	BlockPtr = BlockList;
      long	Size	 = 0;

      if (BlockPtr != NULL) BlockPtr->Last = MemoryFreePtr;
      while (BlockPtr != NULL) {
	 tStringRef StringPtr = (tStringRef) BlockPtr->Memory;
         Size += (char *) BlockPtr->Last - (char *) StringPtr;

	 while (StringPtr < BlockPtr->Last) {
	    long length = LengthSt (StringPtr) + 3;
	    (void) printf ("%08lx %s\n", (unsigned long) StringPtr,
	        StGetCStr (StringPtr));
	    if (length & 1) length ++;
	    StringPtr += length >> 1;
	 }
	 BlockPtr = BlockPtr->Next;
      }
      (void) printf ("\n%5ld Bytes\n", Size);
#endif
   }

void InitStringMemory
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
      BlockList		= NULL;
      MemorySpaceLeft	= 0;
   }

void CloseStringMemory
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
      register tBlock * Next;
      while (BlockList != NULL) {
	 Next = BlockList->Next;
	 Free (BlockList->Size + sizeof (struct lBlock), (char *) BlockList);
	 BlockList = Next;
      }
      BlockList		= NULL;
      MemorySpaceLeft	= 0;
   }

