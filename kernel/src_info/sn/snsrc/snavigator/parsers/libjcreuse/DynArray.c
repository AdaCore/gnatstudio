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

#include "ratc.h"
#include "DynArray.h"
#include <stdio.h>
#include "General.h"
#include "rMemory.h"

/* INVARIANT ElmtCount * AlignedSize (ElmtSize) % sizeof (long) == 0 */

static unsigned long AlignedSize
#if defined __STDC__ | defined __cplusplus
   (unsigned long ElmtSize)
#else
   (ElmtSize) unsigned long ElmtSize;
#endif
   {
      register unsigned long Align;

      if (ElmtSize >= yyMaxAlign) {
	 Align = yyMaxAlign;
      } else {
	 Align = Exp2 (Log2 (ElmtSize + ElmtSize - 2));
      }
      return ElmtSize + Align - 1 - (ElmtSize - 1) % Align;
   }

void MakeArray
#if defined __STDC__ | defined __cplusplus
   (char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize)
#else
   (ArrayPtr, ElmtCount, ElmtSize)
   char * *	ArrayPtr	;
   unsigned long * ElmtCount	;
   unsigned long ElmtSize	;
#endif
   {
      ElmtSize = AlignedSize (ElmtSize);
      switch (ElmtSize % 4) {
      case 0: break;
      case 2: if (* ElmtCount & 1) (* ElmtCount) ++; break;
      case 1:
      case 3: * ElmtCount += sizeof (long) - 1 - (* ElmtCount - 1) %
				sizeof (long); break;
      }
      * ArrayPtr = Alloc (* ElmtCount * ElmtSize);
#ifndef _USRDLL
      if (* ArrayPtr == NULL)
	 (void) fprintf (stderr, "MakeArray: out of memory\n");
#endif
   }

void ResizeArray
#if defined __STDC__ | defined __cplusplus
   (char * * ArrayPtr, unsigned long * OldElmtCount,
   unsigned long NewElmtCount, unsigned long ElmtSize)
#else
   (ArrayPtr, OldElmtCount, NewElmtCount, ElmtSize)
   char * *	ArrayPtr	;
   unsigned long * OldElmtCount	;

   unsigned long NewElmtCount	;
   unsigned long ElmtSize	;
#endif
   {
      ElmtSize = AlignedSize (ElmtSize);
      switch (ElmtSize % 4) {
      case 0: break;
      case 2: if (NewElmtCount & 1) NewElmtCount ++; break;
      case 1:
      case 3: NewElmtCount += sizeof (long) - 1 - (NewElmtCount - 1) %
			      sizeof (long); break;
      }
      if (NewElmtCount < * OldElmtCount) {
	 unsigned long ByteCount = AlignedSize (NewElmtCount * ElmtSize);
	 if (* OldElmtCount * ElmtSize - ByteCount >= yyMaxAlign) {
	    Free (* OldElmtCount * ElmtSize - ByteCount, * ArrayPtr + ByteCount);
	    * OldElmtCount = NewElmtCount;
	 }
      } else if (NewElmtCount > * OldElmtCount) {
		  char * NewPtr	= Alloc (NewElmtCount * ElmtSize);
	 register long * Source	= (long *) * ArrayPtr;
	 register long * Target	= (long *) NewPtr;
	 register long	 i	= * OldElmtCount * ElmtSize / sizeof (long);

	 if (NewPtr == NULL)
#ifndef _USRDLL
	    (void) fprintf (stderr, "ResizeArray: out of memory\n");
#else
	    ;
#endif
	 else {
	    do {
	       * Target ++ = * Source ++;
	    } while (-- i > 0);

	    Free (* OldElmtCount * ElmtSize, * ArrayPtr);
	 }
	 * ArrayPtr = NewPtr;
	 * OldElmtCount = NewElmtCount;
      }
   }

void ExtendArray
#if defined __STDC__ | defined __cplusplus
   (char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize)
#else
   (ArrayPtr, ElmtCount, ElmtSize)
   char * *	ArrayPtr	;
   unsigned long * ElmtCount	;
   unsigned long ElmtSize	;
#endif
   {
      ResizeArray (ArrayPtr, ElmtCount, * ElmtCount * 2, ElmtSize);
   }

void ShrinkArray
#if defined __STDC__ | defined __cplusplus
   (char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize)
#else
   (ArrayPtr, ElmtCount, ElmtSize)
   char * *	ArrayPtr	;
   unsigned long * ElmtCount	;
   unsigned long ElmtSize	;
#endif
   {
		char *	NewPtr	;
      register	long *	Source	;
      register	long *	Target	;
      register	long	i	;
      unsigned	long	NewCount= (* ElmtCount) / 2;

      ElmtSize = AlignedSize (ElmtSize);
      NewPtr = Alloc (NewCount * ElmtSize);
      Source = (long *) * ArrayPtr;
      Target = (long *) NewPtr;
      i      = NewCount * ElmtSize / sizeof (long);

      if (NewPtr == NULL)
#ifndef _USRDLL
	 (void) fprintf (stderr, "ShrinkArray: out of memory\n");
#else
	 ;
#endif
      else {
	 do {
	    * Target ++ = * Source ++;
	 } while (-- i > 0);

	 Free (* ElmtCount * ElmtSize, * ArrayPtr);
	 * ElmtCount = NewCount;
      }
      * ArrayPtr = NewPtr;
   }

void ReleaseArray
#if defined __STDC__ | defined __cplusplus
   (char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize)
#else
   (ArrayPtr, ElmtCount, ElmtSize)
   char * *	ArrayPtr	;
   unsigned long * ElmtCount	;
   unsigned long ElmtSize	;
#endif
   {
      ElmtSize = AlignedSize (ElmtSize);
      Free (* ElmtCount * ElmtSize, * ArrayPtr);
   }

