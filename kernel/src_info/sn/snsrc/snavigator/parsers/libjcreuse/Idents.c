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
 * Revision 1.2.16.2  2000/02/11 23:54:43  spolk
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
 * Revision 1.2  1998/05/21 04:51:02  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:12  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:44  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.20  1997/12/16 18:08:39  grosch
 * tribute to MS VC++ 5.0
 *
 * Revision 1.19  1997/11/30 22:29:41  grosch
 * eliminated use of type cardinal
 * added function GetCStr
 *
 * Revision 1.18  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.17  1996/08/13 13:20:23  grosch
 * adaption to DLL's for Microsoft Visual C++
 *
 * Revision 1.16  1996/07/25  16:51:34  grosch
 * adaption to MS VC++
 *
 * Revision 1.15  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.14  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.13  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.12  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.11  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.10  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.9  1992/01/30  13:13:54  grosch
 * redesign of StringMem: pointer instead of array index
 *
 * Revision 1.8  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.7  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.6  91/09/18  15:18:46  grosch
 * added procedure GetStringRef
 * 
 * Revision 1.5  91/07/17  17:23:07  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.4  91/01/21  12:13:20  grosch
 * some performance improvements
 * 
 * Revision 1.3  90/09/20  09:12:22  grosch
 * calmed down lint
 * 
 * Revision 1.2  90/07/04  14:33:55  grosch
 * introduced conditional include
 * 
 * Revision 1.1  89/06/06  10:28:25  grosch
 * added public variable NoIdent
 * 
 * Revision 1.0  88/10/04  11:44:38  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

#include "ratc.h"
#include "Idents.h"
#include "DynArray.h"

#define InitialTableSize	1024
#define HashTableSize		256
#define cNoIdent		0

tIdent	NoIdent = 1;

typedef struct {
   tStringRef	String;
   int		Length;
   tIdent	Collision;
} IdentTableEntry;

static	unsigned short	Null		= 0;
static	IdentTableEntry	* TablePtr	= NULL;
static	unsigned long	IdentTableSize	= InitialTableSize;
static	tIdent		IdentCount	= 1;

static	tIdent		HashTable [HashTableSize] = {
1	   , cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent, cNoIdent,
};

tIdent MakeIdent
#if defined __STDC__ | defined __cplusplus
   (register char * string, register int length)
#else
   (string, length)
   register char *	string;
   register int		length;
#endif
   {
      register	int		HashTableIndex;
      register	tIdent		CurIdent;
      register	IdentTableEntry	* TablePtrReg;
   
      HashTableIndex = length == 0 ? 0 :	/* hash */
	 ((int) * string + (int) string [length - 1] * 11
			 + length * 26) & (HashTableSize - 1);

      CurIdent = HashTable [HashTableIndex];	/* search */
      while (CurIdent != cNoIdent) {
	 TablePtrReg = & TablePtr [CurIdent];
	 if (TablePtrReg->Length == length && IsEqualSt (TablePtrReg->String, string))
	    return CurIdent;			/* found */
	 CurIdent = TablePtrReg->Collision;
      }

      if (++ IdentCount == (long) IdentTableSize)	/* not found: enter */
	 ExtendArray ((char * *) & TablePtr, & IdentTableSize,
			(long) sizeof (IdentTableEntry));
      TablePtrReg = & TablePtr [IdentCount];
      TablePtrReg->String	= PutString (string, length);
      TablePtrReg->Length	= length;
      TablePtrReg->Collision	= HashTable [HashTableIndex];
      HashTable [HashTableIndex] = IdentCount;
      return IdentCount;
   }

void GetString
#if defined __STDC__ | defined __cplusplus
   (tIdent ident, char * string)
#else
   (ident, string)
   tIdent	ident;
   char *	string;
#endif
   {
      StGetString (TablePtr [ident].String, string);
   }

tStringRef GetStringRef
#if defined __STDC__ | defined __cplusplus
   (tIdent ident)
#else
   (ident) tIdent ident;
#endif
   {
      return TablePtr [ident].String;
   }

char * GetCStr
#if defined __STDC__ | defined __cplusplus
   (tIdent ident)
#else
   (ident) tIdent ident;
#endif
   {
      return StGetCStr (TablePtr [ident].String);
   }

tIdent MaxIdent
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
      return IdentCount;
   }

void WriteIdent
#if defined __STDC__ | defined __cplusplus
   (FILE * file, tIdent ident)
#else
   (file, ident)
   FILE *	file;
   tIdent	ident;
#endif
   {
      char	string [256];

      GetString (ident, string);
      (void) fputs (string, file);
   }

void WriteIdents
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
#ifndef _USRDLL
      tIdent	i;

      for (i = 1; i <= IdentCount; i ++) {
	 (void) printf ("%5ld ", i);
	 WriteIdent (stdout, i);
	 (void) fputc ('\n', stdout);
      }
#endif
   }

void WriteHashTable
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
#ifndef _USRDLL
      tIdent	CurIdent;
      int	i;
      int	Count;
   
      for (i = 0; i < HashTableSize; i ++) {
	 (void) printf ("%5d", i);

	 Count = 0;
	 CurIdent = HashTable [i];
	 while (CurIdent != cNoIdent) {
	    Count ++;
	    CurIdent = TablePtr [CurIdent].Collision;
	 }
	 (void) printf ("%5d", Count);

	 CurIdent = HashTable [i];
	 while (CurIdent != cNoIdent) {
	    (void) fputc (' ', stdout);
	    WriteIdent (stdout, CurIdent);
	    CurIdent = TablePtr [CurIdent].Collision;
	 }
	 (void) fputc ('\n', stdout);
      }
      (void) printf ("\nIdents = %5ld\n", IdentCount);
#endif
   }
    
void InitIdents
#if defined __STDC__ | defined __cplusplus
   (void)
#else
   ()
#endif
   {
      register int i;
      for (i = 0; i < HashTableSize; i ++) HashTable [i] = cNoIdent;

      if (TablePtr == NULL) {
	 MakeArray ((char * *) & TablePtr, & IdentTableSize,
			   (long) sizeof (IdentTableEntry));
	 TablePtr [0].String	= 0;
	 TablePtr [0].Length	= 0;
	 TablePtr [0].Collision	= 0;
      }
      IdentCount		= 0;
      NoIdent			= MakeIdent ("", 0);
   }

