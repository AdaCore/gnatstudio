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
 * Revision 1.2.16.2  2000/02/11 23:54:45  spolk
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
 * Revision 1.2  1998/05/21 04:51:04  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:13  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:51  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.25  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.24  1996/08/14 10:37:24  grosch
 * adaption to DLL's for Microsoft Visual C++
 *
 * Revision 1.23  1996/08/01  08:33:05  grosch
 * adaptions to MS Visual C++ 1.52
 *
 * Revision 1.22  1996/07/04  17:15:43  grosch
 * added function ResizeSet
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
 * Revision 1.18  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.17  1994/07/21  21:08:52  grosch
 * changed element type from cardinal to int
 *
 * Revision 1.16  1994/07/05  21:09:16  grosch
 * adaption to 64 bit machines like DEC Alpha
 *
 * Revision 1.15  1994/07/03  11:22:39  grosch
 * adaption to 64 bit machines like DEC Alpha
 *
 * Revision 1.14  1994/04/05  09:00:35  grosch
 * added prefix 'r' to true and false for improved portability
 *
 * Revision 1.13  1994/01/29  22:13:50  grosch
 * renamed bool to rbool
 *
 * Revision 1.12  1993/08/20  08:12:15  grosch
 * calmed down lint
 *
 * Revision 1.11  1993/01/16  11:22:26  grosch
 * adaption to machines where int takes 2-bytes
 *
 * Revision 1.10  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.9  1992/03/30  09:53:37  grosch
 * fixed bug in IsSubset: removed operator !
 *
 * Revision 1.8  1992/02/06  09:29:54  grosch
 * fixed bug: stdio and ANSI C
 *
 * Revision 1.7  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/07/17  17:23:36  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.4  90/09/20  09:12:25  grosch
 * calmed down lint
 * 
 * Revision 1.3  90/07/04  14:34:02  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  17:25:01  grosch
 * complete redesign in order to increase efficiency
 * 
 * Revision 1.1  89/01/09  17:29:23  grosch
 * added functions Size, Minimum, and Maximum
 * 
 * Revision 1.0  88/10/04  11:44:44  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "ratc.h"
#include "Sets.h"
#include "DynArray.h"
#include "General.h"

#define NoCard			-1

#ifdef PCS10
#define CALL(f)	(* f)
#else
#define CALL(f)	f
#endif

void MakeSet
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, int MaxSize)
#else
   (Set, MaxSize) tSet * Set; int MaxSize;
#endif
   {
      unsigned long	ElmtCount	;

      ElmtCount = (MaxSize + BitsPerBitset - (MaxSize & MaskBitsPerBitset)) /
			BitsPerBitset;
      MakeArray ((char * *) & Set->BitsetPtr, & ElmtCount,
		    (unsigned long) sizeof (BITSET));
      Set->MaxElmt = MaxSize;
      Set->LastBitset = (int) ElmtCount - 1;
      AssignEmpty (Set);
   }

void ResizeSet
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, int MaxSize)
#else
   (Set, MaxSize) tSet * Set; int MaxSize;
#endif
   {
      BITSET *		OldBitsetPtr	= Set->BitsetPtr;
      int		OldLastBitset	= Set->LastBitset;
      unsigned long	ElmtCount	;
      register int	i, j		;

      ElmtCount = (MaxSize + BitsPerBitset - (MaxSize & MaskBitsPerBitset)) /
			BitsPerBitset;
      MakeArray ((char * *) & Set->BitsetPtr, & ElmtCount,
		    (unsigned long) sizeof (BITSET));
      if (Set->BitsetPtr == NULL) { Set->MaxElmt = 0; return; }
      Set->MaxElmt = MaxSize;
      Set->LastBitset = (int) ElmtCount - 1;
      j = Min (Set->LastBitset, OldLastBitset);
      for (i = 0; i <= j; i ++)
	 Set->BitsetPtr [i] = OldBitsetPtr [i];
      for (; i <= Set->LastBitset; i ++)
	 Set->BitsetPtr [i] = 0;
      ElmtCount = OldLastBitset + 1;
      ReleaseArray ((char * *) & OldBitsetPtr, & ElmtCount,
			(unsigned long) sizeof (BITSET));
   }

void ReleaseSet
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      unsigned long	ElmtCount	;

      ElmtCount = Set->LastBitset + 1;
      ReleaseArray ((char * *) & Set->BitsetPtr, & ElmtCount,
			(unsigned long) sizeof (BITSET));
   }

void Union
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ |= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Min (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Max (rSet1->LastElmt , rSet2->LastElmt );
   }

void Difference
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {* s1 ++ &= ~ * s2 ++;} while (-- i);
      rSet1->Card = NoCard;
   }

void Intersection
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ &= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Max (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Min (rSet1->LastElmt , rSet2->LastElmt);
   }

void SymDiff
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ ^= * s2 ++;} while (-- i);
      rSet1->Card      = NoCard;
      rSet1->FirstElmt = Min (rSet1->FirstElmt, rSet2->FirstElmt);
      rSet1->LastElmt  = Max (rSet1->LastElmt , rSet2->LastElmt);
   }

void Complement
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet *	rSet	= Set;
      register int	i 	= rSet->LastBitset;
      register BITSET *	s1	= rSet->BitsetPtr;

      while (i --) {* s1 = ~ * s1; s1 ++;}
      * s1 = ((2L << (rSet->MaxElmt & MaskBitsPerBitset)) - 1) & ~ * s1;
      if (rSet->Card != NoCard)
	 rSet->Card = (short) rSet->MaxElmt + 1 - rSet->Card;
      rSet->FirstElmt = 0;
      rSet->LastElmt  = rSet->MaxElmt;
   }

void Include
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, int Elmt)
#else
   (Set, Elmt) tSet * Set; int Elmt;
#endif
   {
      register tSet *	rSet	= Set;
      register int	rElmt	= Elmt;

      rSet->BitsetPtr [rElmt / BitsPerBitset] |=
	 1L << (rElmt & MaskBitsPerBitset);
      rSet->Card      = NoCard;
      rSet->FirstElmt = Min (rSet->FirstElmt, rElmt);
      rSet->LastElmt  = Max (rSet->LastElmt , rElmt);
   }

void Exclude
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, int Elmt)
#else
   (Set, Elmt) tSet * Set; int Elmt;
#endif
   {
      register tSet *	rSet	= Set;
      register int	rElmt	= Elmt;

      rSet->BitsetPtr [rElmt / BitsPerBitset] &=
	 ~ (1L << (rElmt & MaskBitsPerBitset));
      rSet->Card = NoCard;
      if (rElmt == rSet->FirstElmt && rElmt < rSet->MaxElmt) rSet->FirstElmt ++;
      if (rElmt == rSet->LastElmt && rElmt > 0) rSet->LastElmt --;
   }

int Card
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet * rSet = Set;

      if (rSet->Card == NoCard) {
	 register int i, n;
	 register int last = rSet->LastElmt;

	 n = 0;
	 for (i = rSet->FirstElmt; i <= last; i ++) {
	    if (IsElement (i, rSet)) n ++;
	 }
	 rSet->Card = n;
      }
      return rSet->Card;
   }
    
int Minimum
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet *	rSet	= Set;
      register int	i;
      register int	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet)) {
	    rSet->FirstElmt = i;
	    return i;
	 }
      }
      return 0;
   }
    
int Maximum
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet *	rSet	= Set;
      register int	i;
      register int	first	= rSet->FirstElmt;

      for (i = rSet->LastElmt; i >= first; i --) {
	 if (IsElement (i, rSet)) {
	    rSet->LastElmt = i;
	    return i;
	 }
      }
      return 0;
   }
    
int Extract
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register int i = Minimum (Set);
      Exclude (Set, i);
      return i;
   }

rbool IsSubset
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {
	 if (* s1 ++ & ~ * s2 ++) return rfalse;
      } while (-- i);
      return rtrue;
   }

rbool IsEqual
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;

      do {
	 if (* s1 ++ != * s2 ++) return rfalse;
      } while (-- i);
      return rtrue;
   }
    
rbool IsEmpty
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet * rSet = Set;

      if (rSet->FirstElmt <= rSet->LastElmt) {
	 register int i  = rSet->LastBitset + 1;
	 register BITSET * s1 = rSet->BitsetPtr;

	 do {
	    if (* s1 ++ != 0) return rfalse;
	 } while (-- i);
      }
      return rtrue;
   }
    
rbool Forall
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, rbool (* Proc) (int))
#else
   (Set, Proc) tSet * Set; rbool (* Proc) ();
#endif
   {
      register tSet *	rSet	= Set;
      register int	i;
      register int	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && ! CALL(Proc) (i)) return rfalse;
      }
      return rtrue;
   }
    
rbool Exists
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, rbool (* Proc) (int))
#else
   (Set, Proc) tSet * Set; rbool (* Proc) ();
#endif
   {
      register tSet *	rSet	= Set;
      register int	i;
      register int	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && CALL(Proc) (i)) return rtrue;
      }
      return rfalse;
   }
    
rbool Exists1
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, rbool (* Proc) (int))
#else
   (Set, Proc) tSet * Set; rbool (* Proc) ();
#endif
   {
      register tSet *	rSet	= Set;
      register int	i, n;
      register int	last	= rSet->LastElmt;

      n = 0;
      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet) && CALL(Proc) (i)) n ++;
      }
      return n == 1;
   }

void Assign
#if defined __STDC__ | defined __cplusplus
   (tSet * Set1, tSet * Set2)
#else
   (Set1, Set2) tSet * Set1; tSet * Set2;
#endif
   {
      register tSet *	rSet1	= Set1;
      register int	i 	= rSet1->LastBitset + 1;
      register BITSET *	s1	= rSet1->BitsetPtr;
      register BITSET *	s2	= Set2->BitsetPtr;
      register tSet *	rSet2	= Set2;

      do {* s1 ++ = * s2 ++;} while (-- i);
      rSet1->Card      = rSet2->Card;
      rSet1->FirstElmt = rSet2->FirstElmt;
      rSet1->LastElmt  = rSet2->LastElmt;
   }

void AssignElmt
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, int Elmt)
#else
   (Set, Elmt) tSet * Set; int Elmt;
#endif
   {
      register tSet *	rSet	= Set;
      register int	rElmt	= Elmt;

      AssignEmpty (rSet);
      Include (rSet, rElmt);
      rSet->Card      = 1;
      rSet->FirstElmt = rElmt;
      rSet->LastElmt  = rElmt;
   }

void AssignEmpty
#if defined __STDC__ | defined __cplusplus
   (tSet * Set)
#else
   (Set) tSet * Set;
#endif
   {
      register tSet *	rSet	= Set;
      register int	i 	= rSet->LastBitset + 1;
      register BITSET *	s1	= rSet->BitsetPtr;

      do {* s1 ++ = 0;} while (-- i);
      rSet->Card      = 0;
      rSet->FirstElmt = rSet->MaxElmt;
      rSet->LastElmt  = 0;
   }

void ForallDo
#if defined __STDC__ | defined __cplusplus
   (tSet * Set, void (* Proc) (int))
#else
   (Set, Proc) tSet * Set; void (* Proc) ();
#endif
   {
      register tSet *	rSet	= Set;
      register int	i;
      register int	last	= rSet->LastElmt;

      for (i = rSet->FirstElmt; i <= last; i ++) {
	 if (IsElement (i, rSet)) CALL(Proc) (i);
      }
   }

void ReadSet
#if defined __STDC__ | defined __cplusplus
   (FILE * File, tSet * Set)
#else
   (File, Set) FILE * File; tSet * Set;
#endif
   {
#ifndef _USRDLL
      register tSet * rSet = Set;
      long i; int card = 0;

      while (fgetc (File) != '{');
      AssignEmpty (rSet);
      for (;;) {
	 if (fgetc (File) == '}') break;
	 (void) fscanf (File, "%ld", & i);
	 Include (rSet, (short) i);
	 card ++;
      }
      rSet->Card = card;
#endif
   }

static FILE * g;

void PrintElmt
#if defined __STDC__ | defined __cplusplus
   (int Elmt)
#else
   (Elmt) int Elmt;
#endif
   {
      (void) fprintf (g, " %d", Elmt);
   }

void WriteSet
#if defined __STDC__ | defined __cplusplus
   (FILE * File, tSet * Set)
#else
   (File, Set) FILE * File; tSet * Set;
#endif
   {
      g = File;
      (void) fprintf (File, "{");
      ForallDo (Set, PrintElmt);
      (void) fprintf (File, "}");
   }

void InitSets
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
   }

