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
 * Revision 1.2.16.2  2000/02/11 23:54:40  spolk
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
 * Revision 1.2  1998/05/21 04:51:00  bje
 * 	* parsers/libjcreuse/*.c: Removed RCS Id keyword.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:12  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:38  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.21  1997/11/30 22:40:23  grosch
 * changed type of ErrorCode from int to long
 * added missing initializations
 * suppress call of WritePosition if NoPosition
 * added functions BeginErrors and GetCount
 *
 * Revision 1.20  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.19  1997/05/06 15:11:37  grosch
 * calmed down purify
 *
 * Revision 1.18  1996/08/13 13:20:23  grosch
 * adaption to DLL's for Microsoft Visual C++
 *
 * Revision 1.17  1996/08/01  08:33:05  grosch
 * adaptions to MS Visual C++ 1.52
 *
 * Revision 1.16  1996/07/25  16:51:34  grosch
 * adaption to MS VC++
 *
 * Revision 1.15  1996/07/04  17:15:19  grosch
 * introduced files Reuse.h and config.h for system configuration
 *
 * Revision 1.14  1996/07/04  09:53:45  grosch
 * introduced files Reuse.h and config.h for system configuration
 *
 * Revision 1.13  1996/06/05  12:00:48  grosch
 * adaption to MS VC++
 *
 * Revision 1.12  1995/08/14  13:56:25  grosch
 * corrections because of purify
 *
 * Revision 1.11  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.10  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.9  1995/02/23  20:11:10  grosch
 * renamed Positions to Position, StringMem to StringM, Relations to Relation
 * output lines with at most 132 characters
 *
 * Revision 1.8  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.7  1994/11/10  11:26:40  grosch
 * fixed bug in declaration of FoundString
 *
 * Revision 1.6  1994/11/07  12:39:52  grosch
 * added cpp variables BRIEF, FIRST, and TRUNCATE
 *
 * Revision 1.5  1994/06/16  19:05:46  grosch
 * added fflush
 *
 * Revision 1.4  1994/04/05  09:00:35  grosch
 * added prefix 'r' to true and false for improved portability
 *
 * Revision 1.3  1994/01/29  22:13:50  grosch
 * renamed bool to rbool
 *
 * Revision 1.2  1993/08/18  15:01:05  grosch
 * rename System and Memory to rSystem and rMemory
 *
 * Revision 1.1  1992/08/13  12:29:12  grosch
 * fix bugs with ANSI C
 *
 * Revision 1.0  1992/08/07  14:31:40  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Nov. 1994 */

#include "Errors.h"
#include "Reuse.h"
#if HAVE_STDLIB_H | defined _MSC_VER
#include <stdlib.h>
#endif

#ifdef __cplusplus
extern "C" {
#include "rSystem.h"
#include "rMemory.h"
#include "Sets.h"
#include "Idents.h"
}
#else
#include "rSystem.h"
#include "rMemory.h"
#include "Sets.h"
#include "Idents.h"
#endif

#define BRIEF
#define FIRST
#define TRUNCATE

#define MaxError	500

static void yyExit ARGS ((void)) { rExit (1); }

void (* Errors_Exit) ARGS ((void)) = yyExit;

typedef struct {
   tPosition	Position	;
   rbool	IsErrorCode	;
   short	ErrorNumber	;
   long		ErrorCode	;
   short	ErrorClass	;
   short	InfoClass	;
   union {
      int	vInteger	;
      short	vShort		;
      long	vLong		;
      float	vReal		;
      rbool	vBoolean	;
      char	vCharacter	;
      tStringRef vString	;
      tSet *	vSet		;
      tIdent	vIdent		;
      tPosition	vPosition	;
   } Info;
} tError;

static void WriteHead	ARGS ((tPosition Position, int ErrorClass));
static void WriteCode	ARGS ((long ErrorCode));
static void WriteInfo	ARGS ((int InfoClass, char * Info));
static void WriteMessage ARGS ((rbool IsErrorCode, long ErrorCode,
	int ErrorClass, tPosition Position, int InfoClass, char * Info));
static void StoreMessage ARGS ((rbool IsErrorCode, long ErrorCode,
	int ErrorClass, tPosition Position, int InfoClass, char * Info));
static int IsLess	ARGS ((tError * i, tError * j));

static tError	ErrorTable [MaxError + 1];
static int	MessageCount	= 0;
static rbool	IsStore		= rfalse;
static void (*	HandleMessage) ARGS ((rbool IsErrorCode, long ErrorCode,
			int ErrorClass, tPosition Position, int InfoClass,
			char * Info)) = WriteMessage;
static FILE *	Out		= NULL;
static unsigned long PrevLine	= 0;
static char	FoundString	[5000];
static int	nErrors		[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };

void BeginErrors ARGS ((void))
{
   int i; for (i = 0; i < 8; i ++) nErrors [i] = 0;
   PrevLine	= 0;
   MessageCount	= 0;
}

void ErrorMessage
#if defined __STDC__ | defined __cplusplus
   (long ErrorCode, int ErrorClass, tPosition Position)
#else
   (ErrorCode, ErrorClass, Position)
   long ErrorCode, ErrorClass; tPosition Position;
#endif
{
   nErrors [ErrorClass] ++;
   (* HandleMessage) (rtrue, ErrorCode, ErrorClass, Position, xxNone, NULL);
}

void ErrorMessageI
#if defined __STDC__ | defined __cplusplus
   (long ErrorCode, int ErrorClass, tPosition Position, int InfoClass,
      char * Info)
#else
   (ErrorCode, ErrorClass, Position, InfoClass, Info)
   long ErrorCode, ErrorClass; tPosition Position; int InfoClass; char * Info;
#endif
{
   nErrors [ErrorClass] ++;
   (* HandleMessage) (rtrue, ErrorCode, ErrorClass, Position, InfoClass, Info);
}

void Message
#if defined __STDC__ | defined __cplusplus
   (char * ErrorText, int ErrorClass, tPosition Position)
#else
   (ErrorText, ErrorClass, Position)
   char * ErrorText; int ErrorClass; tPosition Position;
#endif
{
   nErrors [ErrorClass] ++;
   (* HandleMessage) (rfalse, MakeIdent (ErrorText, strlen (ErrorText)),
      ErrorClass, Position, xxNone, NULL);
}

void MessageI
#if defined __STDC__ | defined __cplusplus
   (char * ErrorText, int ErrorClass, tPosition Position, int InfoClass,
      char * Info)
#else
   (ErrorText, ErrorClass, Position, InfoClass, Info)
   char * ErrorText; int ErrorClass; tPosition Position; int InfoClass;
   char * Info;
#endif
{
   nErrors [ErrorClass] ++;
   (* HandleMessage) (rfalse, MakeIdent (ErrorText, strlen (ErrorText)),
      ErrorClass, Position, InfoClass, Info);
}

static void WriteHead
#if defined __STDC__ | defined __cplusplus
   (tPosition Position, int ErrorClass)
#else
   (Position, ErrorClass) tPosition Position; int ErrorClass;
#endif
{
#ifndef _USRDLL
   if (Out == NULL) Out = stderr;
#endif
   if (Position.Line != 0) {
      WritePosition (Out, Position);
      (void) fputs (": ", Out);
   }
   switch (ErrorClass) {
   case xxFatal		: (void) fputs ("Fatal       ", Out); break;
   case xxRestriction	: (void) fputs ("Restriction ", Out); break;
   case xxError		: (void) fputs ("Error       ", Out); break;
   case xxWarning	: (void) fputs ("Warning     ", Out); break;
   case xxRepair	: (void) fputs ("Repair      ", Out); break;
   case xxNote		: (void) fputs ("Note        ", Out); break;
   case xxInformation	: (void) fputs ("Information ", Out); break;
   default		: (void) fprintf (Out, "Error class: %d ", ErrorClass);
   }
}

static void WriteCode
#if defined __STDC__ | defined __cplusplus
   (long ErrorCode)
#else
   (ErrorCode) long ErrorCode;
#endif
{
   switch (ErrorCode) {
   case xxNoText	: break;
   case xxSyntaxError	: (void) fputs ("syntax error"		, Out); break;
   case xxExpectedTokens: (void) fputs ("expected tokens"	, Out); break;
   case xxRestartPoint	: (void) fputs ("restart point"		, Out); break;
   case xxTokenInserted	: (void) fputs ("token inserted "	, Out); break;
   case xxTooManyErrors	: (void) fputs ("too many messages"	, Out); break;
   case xxTokenFound	: (void) fputs ("token found    "	, Out); break;
   case xxFoundExpected	: (void) fputs ("found/expected "	, Out); break;
   default		: (void) fprintf (Out, " error code: %ld", ErrorCode);
   }
}

static void WriteInfo
#if defined __STDC__ | defined __cplusplus
   (int InfoClass, char * Info)
#else
   (InfoClass, Info) int InfoClass; char * Info;
#endif
{
   int i;
   if (InfoClass == xxNone) return;
   (void) fputs (": ", Out);
   switch (InfoClass) {
   case xxInteger	: (void) fprintf (Out, "%d", * (int *) Info); break;
   case xxShort		: i =  * (short *) Info; (void) fprintf (Out, "%d", i);
			  break;
   case xxLong		: (void) fprintf (Out, "%ld", * (long *) Info); break;
   case xxReal		: (void) fprintf (Out, "%e", * (float *) Info); break;
   case xxBoolean	: (void) fprintf (Out, "%c", * (rbool *) Info ?
					'T' : 'F'); break;
   case xxCharacter	: (void) fprintf (Out, "%c", * Info); break;
   case xxString	: 
#ifdef TRUNCATE
			  { register int i = 0;
			    while (i <= 25 && Info [i] != '\0')
			       (void) fputc (Info [i ++], Out);
			    if (Info [i] != '\0') {
			       while (Info [i] != ' ' && Info [i] != '\0')
				  (void) fputc (Info [i ++], Out);
			       if (Info [i] != '\0') (void) fputs (" ...", Out);
			    }
			  }
#else
			  (void) fputs	 (Info, Out);
#endif
			  break;
   case xxSet		: WriteSet	 (Out, (tSet *) Info); break;
   case xxIdent		: WriteIdent	 (Out, * (tIdent *) Info); break;
   case xxPosition	: WritePosition	 (Out, * (tPosition *) Info); break;
   default		: (void) fprintf (Out, "info class: %d", InfoClass);
   }
}

static void WriteMessage
#if defined __STDC__ | defined __cplusplus
   (rbool IsErrorCode, long ErrorCode, int ErrorClass, tPosition Position,
      int InfoClass, char * Info)
#else
   (IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info)
   rbool IsErrorCode; long ErrorCode, ErrorClass; tPosition Position;
   int InfoClass; char * Info;
#endif
{
   if (IsErrorCode) {
#ifdef BRIEF
      switch (ErrorCode) {
      case xxTokenFound		: (void) strcpy (FoundString, Info);
				  (void) strcat (FoundString, "/");
      case xxSyntaxError	:
      case xxRestartPoint	:
      case xxTokenInserted	: return;
      case xxExpectedTokens	: ErrorCode = xxFoundExpected;
				  ErrorClass = xxError;
				  Info = strcat (FoundString, Info);
      }
#endif
#ifdef FIRST
      if (Position.Line == PrevLine) return; else PrevLine = Position.Line;
#endif
   }
   WriteHead (Position, ErrorClass);
   if (IsErrorCode) WriteCode (ErrorCode);
   else WriteIdent (Out, (tIdent) ErrorCode);
   WriteInfo (InfoClass, Info);
   (void) fputc ('\n', Out); (void) fflush (Out);
   if (ErrorClass == xxFatal && ! IsStore) (* Errors_Exit) ();
}

void WriteMessages
#if defined __STDC__ | defined __cplusplus
   (FILE * File)
#else
   (File) FILE * File;
#endif
{
   int		i;
   char *	Info = NULL;
   int		length;
   char		string1 [256];
   char *	string2;

   qsort ((char *) & ErrorTable [1], MessageCount, sizeof (tError),
#ifdef _MSC_VER
      (int (*) (const void *, const void *)) IsLess);
#else
      (int (*) ARGS ((const void *, const void *))) IsLess);
#endif
   PrevLine = 0;
   Out = File;
   for (i = 1; i <= MessageCount; i ++) {
      register tError * With = & ErrorTable [i];

      switch (With->InfoClass) {
      case xxInteger	: Info = (char *) & With->Info.vInteger	; break;
      case xxShort	: Info = (char *) & With->Info.vShort	; break;
      case xxLong	: Info = (char *) & With->Info.vLong	; break;
      case xxReal	: Info = (char *) & With->Info.vReal	; break;
      case xxBoolean	: Info = (char *) & With->Info.vBoolean	; break;
      case xxCharacter	: Info = (char *) & With->Info.vCharacter; break;
      case xxString	: length = LengthSt (With->Info.vString);
			  if (length < 256) {
			     StGetString (With->Info.vString, string1);
			     Info = string1;
			  } else {
			     string2 =
				(char *) Alloc ((unsigned long) ++ length);
			     StGetString (With->Info.vString, string2);
			     Info = string2;
			  }
			  break;
      case xxSet	: Info = (char *) With->Info.vSet	; break;
      case xxIdent	: Info = (char *) & With->Info.vIdent	; break;
      case xxPosition	: Info = (char *) & With->Info.vPosition; break;
      }
      WriteMessage (With->IsErrorCode, With->ErrorCode, With->ErrorClass,
	 With->Position, With->InfoClass, Info);
      if (With->InfoClass == xxString && length >= 256)
	 Free ((unsigned long) length, (char *) string2);
   }
#ifndef _USRDLL
   Out = stderr;
#endif
}

static void StoreMessage
#if defined __STDC__ | defined __cplusplus
   (rbool IsErrorCode, long ErrorCode, int ErrorClass, tPosition Position,
      int InfoClass, char * Info)
#else
   (IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info)
   rbool IsErrorCode; long ErrorCode, ErrorClass; tPosition Position;
   int InfoClass; char * Info;
#endif
{
  if (MessageCount < MaxError) {
    MessageCount ++;
    {
      register tError * With = & ErrorTable [MessageCount];

      With->Position	= Position;
      With->IsErrorCode	= IsErrorCode;
      With->ErrorNumber	= MessageCount;
      With->ErrorCode	= ErrorCode;
      With->ErrorClass	= ErrorClass;
      With->InfoClass	= InfoClass;
      switch (With->InfoClass) {
      case xxInteger	: With->Info.vInteger	= * (int	*) Info; break;
      case xxShort	: With->Info.vShort	= * (short	*) Info; break;
      case xxLong	: With->Info.vLong	= * (long	*) Info; break;
      case xxReal	: With->Info.vReal	= * (float	*) Info; break;
      case xxBoolean	: With->Info.vBoolean	= * (rbool	*) Info; break;
      case xxCharacter	: With->Info.vCharacter	= * (char	*) Info; break;
      case xxString	: With->Info.vString	=
					PutString (Info, strlen (Info)); break;
      case xxSet	: With->Info.vSet	=
				(tSet *) Alloc ((unsigned long) sizeof (tSet));
        		  MakeSet (With->Info.vSet, Size ((tSet *) Info));
        		  Assign (With->Info.vSet, (tSet *) Info); break;
      case xxIdent	: With->Info.vIdent	= * (tIdent	*) Info; break;
      case xxPosition	: With->Info.vPosition	= * (tPosition	*) Info; break;
      }
    }
  } else {
    {
      register tError * With = & ErrorTable [MessageCount];

      With->IsErrorCode	= rtrue;
      With->ErrorCode	= xxTooManyErrors;
      With->ErrorClass	= xxRestriction;
      With->InfoClass	= xxNone;
    }
  }
  if (ErrorClass == xxFatal) {
#ifndef _USRDLL
     WriteMessages (stderr);
#endif
     (* Errors_Exit) ();
  }
}

static int IsLess
#if defined __STDC__ | defined __cplusplus
   (tError * i, tError * j)
#else
   (i, j) tError * i, * j;
#endif
{
  register int r = Compare (i->Position, j->Position);
  return r != 0 ? r : i->ErrorNumber - j->ErrorNumber;
}

void StoreMessages
#if defined __STDC__ | defined __cplusplus
   (rbool Store)
#else
   (Store) rbool Store;
#endif
{
  if (Store) {
    HandleMessage = StoreMessage;
    MessageCount  = 0;
  } else {
    HandleMessage = WriteMessage;
  }
  IsStore = Store;
}

int GetCount
#if defined __STDC__ | defined __cplusplus
   (int ErrorClass)
#else
   (ErrorClass) int ErrorClass;
#endif
{
   return nErrors [ErrorClass];
}

