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
 * Revision 1.2.16.2  2000/02/11 23:54:48  spolk
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
 * Revision 1.2  1998/05/21 04:51:07  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:13  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:21:00  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.25  1997/12/16 18:08:39  grosch
 * tribute to MS VC++ 5.0
 *
 * Revision 1.24  1997/11/30 22:31:56  grosch
 * eliminated use of type cardinal
 * added function CloserMemory
 *
 * Revision 1.23  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.22  1996/09/17 08:57:48  grosch
 * added procedure WriteMemory
 *
 * Revision 1.21  1996/06/05  12:00:48  grosch
 * adaption to MS VC++
 *
 * Revision 1.20  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.19  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.18  1994/12/31  09:51:40  grosch
 * adaption to HP cc
 *
 * Revision 1.17  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.16  1994/06/13  09:37:20  grosch
 * fix alignment for DEC Alpha
 *
 * Revision 1.15  1993/10/28  13:34:32  grosch
 * improved storage allocation and alignment handling
 *
 * Revision 1.14  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.13  1992/06/24  12:23:15  grosch
 * changed cNoMoreSpace from -1 to 0
 *
 * Revision 1.12  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.11  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.10  1992/01/30  13:16:06  grosch
 * redesign of interface to operating system
 *
 * Revision 1.9  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.8  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.7  91/01/21  12:13:22  grosch
 * some performance improvements
 * 
 * Revision 1.6  90/12/14  15:55:52  grosch
 * introduced variable MemoryUsed
 * 
 * Revision 1.5  90/09/14  11:20:46  grosch
 * removed superfluous declarations for automatic alignment
 * 
 * Revision 1.4  90/09/04  17:32:10  grosch
 * automatic determination of alignment
 * 
 * Revision 1.3  90/07/04  14:33:58  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  20:16:43  grosch
 * added alignment for MIPS processor
 * 
 * Revision 1.1  89/06/06  10:28:54  grosch
 * fixed lint problem at call of Free
 * 
 * Revision 1.0  88/10/04  11:44:41  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "ratc.h"
#include "rMemory.h"
#include "rSystem.h"
#include "General.h"
#include <stdio.h>

#define MinSizeSmallBlock	sizeof (tBlockPtr)
#define MaxSizeSmallBlock	62	/* 64 - 2	*/
#define MinSizeLargeBlockLog	6	/* Log2 64	*/
#define MaxSizeLargeBlockLog	24	/* Log2 2**24	*/
#define PoolSize		10240L
#define NIL			(tBlockPtr) NULL

unsigned long MemoryUsed = 0;

struct tBlock {
   struct tBlock *	Successor;
   unsigned long	Size;
};
typedef struct tBlock *	tBlockPtr;
typedef short		tSmallBlockRange;
typedef short		tLargeBlockRange;

static	tBlockPtr	SmallChain [MaxSizeSmallBlock    + 1] = { 0,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL,
};
static	tBlockPtr	LargeChain [MaxSizeLargeBlockLog + 1] = { 0,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL,
};
static	char *		PoolFreePtr = 0;
static	char *		PoolEndPtr  = 0;
static	tBlockPtr	BlockList   = NULL;

char * Alloc
#if defined __STDC__ | defined __cplusplus
   (register unsigned long ByteCount)
#else
   (ByteCount) register unsigned long ByteCount;
#endif

/* Returns a pointer to dynamically allocated	*/
/* space of size 'ByteCount' bytes.		*/

{
   ByteCount = yyAlignedSize (ByteCount);

   if (ByteCount <= MaxSizeSmallBlock) {	/* handle small block */
      if (ByteCount < MinSizeSmallBlock) ByteCount = MinSizeSmallBlock;
      if (SmallChain [ByteCount] != NIL) {	/* obtain block from freelist */
	 register tBlockPtr CurrentBlock = SmallChain [ByteCount];
	 SmallChain [ByteCount] = CurrentBlock->Successor;
	 return (char *) CurrentBlock;
      } else {				/* obtain block from storage pool */
	 register unsigned long FreeBytes;
	 register char *	PrevFreePtr;
						/* release old pool */
	 if ((FreeBytes = PoolEndPtr - PoolFreePtr) < ByteCount) {
	    if (FreeBytes >= MinSizeSmallBlock) Free (FreeBytes, PoolFreePtr);
	    PoolFreePtr = Alloc (PoolSize);	/* allocate new pool */
	    PoolEndPtr  = PoolFreePtr + PoolSize;
	 }
	 PrevFreePtr = PoolFreePtr;
	 PoolFreePtr += ByteCount;
	 return PrevFreePtr;
      }
   } else {					/* handle large block */

      /* 1. search in LargeChain [Log2 (ByteCount)] using BEST FIT */

      register short		ChainNumber	= (short) Log2 (ByteCount);
      register tBlockPtr	CurrentBlock	= LargeChain [ChainNumber];
      register tBlockPtr	PreviousBlock	= (tBlockPtr)
						  & (LargeChain [ChainNumber]);
      register tBlockPtr	BestBlock	= NIL;
      register unsigned long	BestBlockSize	= 1000000000;
      register tBlockPtr	PredecessorBlock;
      register tLargeBlockRange	j		;
      register unsigned long	CurrentBlockSize;

      while (CurrentBlock) {
	 CurrentBlockSize = CurrentBlock->Size;
	 if (CurrentBlockSize >= ByteCount) {
	    if (CurrentBlockSize == ByteCount) { /* exact match */
	       PreviousBlock->Successor = CurrentBlock->Successor;
	       return (char *) CurrentBlock;
	    }
	    if (CurrentBlockSize < BestBlockSize) { /* improve approximation */
	       BestBlock	= CurrentBlock;
	       BestBlockSize	= CurrentBlockSize;
	       PredecessorBlock	= PreviousBlock;
	    }
	 }
	 PreviousBlock	= CurrentBlock;
	 CurrentBlock	= CurrentBlock->Successor;
      }

      if (BestBlock) {
	 PredecessorBlock->Successor = BestBlock->Successor;
	 if (BestBlockSize - ByteCount >= MinSizeSmallBlock) {
	    Free (BestBlockSize - ByteCount, (char *) BestBlock + ByteCount);
	 }
	 return (char *) BestBlock;
      }

      /* 2. search in LargeChain [j], j > Log2 (ByteCount), using FIRST FIT */

      for (j = ChainNumber+1; j <= MaxSizeLargeBlockLog; j ++) {
	 CurrentBlock = LargeChain [j];
	 if (CurrentBlock != NIL) {
	    LargeChain [j] = CurrentBlock->Successor;
	    if (CurrentBlock->Size - ByteCount >= MinSizeSmallBlock) {
	       Free (CurrentBlock->Size - ByteCount,
			(char *) CurrentBlock + ByteCount);
	    }
	    return (char *) CurrentBlock;
	 }
      }

      if (ByteCount < PoolSize) {	/* 3. obtain block from storage pool */
	 register unsigned long FreeBytes;
	 register char *	PrevFreePtr;
						/* release old pool */
	 if ((FreeBytes = PoolEndPtr - PoolFreePtr) < ByteCount) {
	    if (FreeBytes >= MinSizeSmallBlock) Free (FreeBytes, PoolFreePtr);
	    PoolFreePtr = Alloc (PoolSize);	/* allocate new pool */
	    PoolEndPtr  = PoolFreePtr + PoolSize;
	 }
	 PrevFreePtr = PoolFreePtr;
	 PoolFreePtr += ByteCount;
	 return PrevFreePtr;
      } else {				/* 4. allocate individual block */
	 ByteCount += sizeof (double);
	 CurrentBlock = (tBlockPtr) rAlloc ((long) ByteCount);
	 if (CurrentBlock != NIL) {
	    CurrentBlock->Successor = BlockList;
	    BlockList = CurrentBlock;
	    MemoryUsed += ByteCount;
	    return (char *) CurrentBlock + sizeof (double);
	 } else {
	    return (char *) CurrentBlock;
	 }
      }
   }
}

void Free
#if defined __STDC__ | defined __cplusplus
   (unsigned long ByteCount, char * a)
#else
   (ByteCount, a) unsigned long ByteCount; char * a;
#endif

/* The dynamically allocated space starting at	*/
/* address 'a' of size 'ByteCount' bytes is	*/
/* released.					*/

{
   register tBlockPtr		BlockPtr;
   register tLargeBlockRange	ChainNumber;

   ByteCount = yyAlignedSize (ByteCount);

   BlockPtr = (tBlockPtr) a;
   if (ByteCount <= MaxSizeSmallBlock) {
      if (ByteCount < MinSizeSmallBlock) ByteCount = MinSizeSmallBlock;
      BlockPtr->Successor	= SmallChain [ByteCount];
      SmallChain [ByteCount]	= BlockPtr;
   } else {
      ChainNumber		= (short) Log2 (ByteCount);
      BlockPtr->Successor	= LargeChain [ChainNumber];
      BlockPtr->Size		= ByteCount;
      LargeChain [ChainNumber]	= BlockPtr;
   }
}

static rbool IsCorrupted
#if defined __STDC__ | defined __cplusplus
   (tBlockPtr Ptr)
#else
   (Ptr) tBlockPtr Ptr;
#endif
{
   return (unsigned) Ptr % yyMaxAlign != 0;
}

void WriteMemory ARGS ((void))
{
   tBlockPtr	BlockPtr;
   int		i, j, Count;
   unsigned long MemoryFree = PoolEndPtr - PoolFreePtr;

   (void) printf ("PoolFreePtr, PoolEndPtr = %08lx %08lx\n\n",
      (unsigned long) PoolFreePtr, (unsigned long) PoolEndPtr);
   (void) printf ("SmallChain:\n");
   for (i = MinSizeSmallBlock; i <= MaxSizeSmallBlock; i += 2) {
      (void) printf ("%2d:", i);
      Count = 0;
      BlockPtr = (tBlockPtr) SmallChain [i];
      for (;;) {
	 if (BlockPtr == NIL) break;
	 if (Count == 8) { (void) printf ("\n    "); Count = 0; }
	 Count ++;
	 (void) printf (" %08lx", (unsigned long) BlockPtr);
	 MemoryFree += i;
	 if (IsCorrupted (BlockPtr)) { (void) printf (" <="); break; }
	 BlockPtr = BlockPtr->Successor;
      }
      (void) printf ("\n");
   }
   (void) printf ("\nLargeChain:\n");
   for (j = MinSizeLargeBlockLog; j <= MaxSizeLargeBlockLog; j ++) {
      (void) printf ("%2d:", j);
      Count = 0;
      BlockPtr = (tBlockPtr) LargeChain [j];
      for (;;) {
	 if (BlockPtr == NIL) break;
	 if (Count == 5) { (void) printf ("\n    "); Count = 0; }
	 Count ++;
	 (void) printf (" %08lx", (unsigned long) BlockPtr);
	 if (IsCorrupted (BlockPtr)) { (void) printf (" <="); break; }
	 MemoryFree += BlockPtr->Size;
	 (void) printf ("%7ld", BlockPtr->Size);
	 BlockPtr = BlockPtr->Successor;
      }
      (void) printf ("\n");
   }
   (void) printf ("\nMemoryFree = %ld\n\n", MemoryFree);
}

void InitrMemory ARGS ((void))
{
   register int i;

   for (i = MinSizeSmallBlock; i <= MaxSizeSmallBlock; i += 2) {
      SmallChain [i] = NIL;
   }
   for (i = MinSizeLargeBlockLog; i <= MaxSizeLargeBlockLog; i ++) {
      LargeChain [i] = NIL;
   }
   MemoryUsed	= 0;
   PoolFreePtr	= 0;
   PoolEndPtr	= 0;
   BlockList	= NULL;
}

void CloserMemory ARGS ((void))
{
   while (BlockList) {
      tBlockPtr NextBlock = BlockList->Successor;
      rFree ((char *) BlockList);
      BlockList = NextBlock;
   }
   BeginrMemory ();
}

