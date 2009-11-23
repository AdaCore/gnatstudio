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

/* Ich, Doktor Josef Grosch, Informatiker, Dez. 1996 */

#include <sys/stat.h>
#include <stdlib.h>
#include "cobol.h"
#include "Errors.h"
#include "Position.h"
#include "rMemory.h"
#include "StringM.h"
#include "Idents.h"
#include "Source.h"
#include "Scanner.h"
#include "Parser.h"
#include "keywords.h"
#include "deftab.h"

	FILE *	yyin;
	FILE *	include_fp = NULL;
	FILE *	hlfp;
	int	hl;
	char *	current_file;

	int	error_count	= 0;

static	rbool	do_copy		= rtrue;
static	char	extension [16]	= "cbl";

static	char *	incl_file [32];
static	int	incl_cnt	= 0;

static void init_include ()
{
   if (include_fp && incl_cnt == 0) {
      for (;;) {
	 int l;
	 char buffer [256];
	 char * p = fgets (buffer, 256, include_fp);
	 if (! p || incl_cnt >= 31) return;
	 l = strlen (buffer);
	 buffer [l - 1] = '\0';
	 incl_file [++ incl_cnt] = (char *) Alloc (l);
	 (void) strcpy (incl_file [incl_cnt], buffer);
      }
   }
}

static	rbool	is_initialized	= rfalse;
static	rbool	report		= rfalse;

static void get_options ARGS ((void))
{
   char * arg	= getenv ("PAF_COBOL");

   if (arg == NULL) return;

   while (* arg) {
      if (* arg == '-') {
	 switch (* ++ arg) {
	 case 'f': report = rtrue; break;

	 case 'h':
	    (void) printf ("\nsyntax of PAF_COBOL:\n\n");
	    (void) printf ("   [-f] [-h]\n\n");
	    (void) printf (" f : report error messages (default: do not report)\n");
	    (void) printf (" h : help\n\n");
	    break;
	 }
      }
      arg ++;
   }
}

void start_parser (fname, parse_cplpl, highl_fp, highlight)
   char	*	fname;
   int		parse_cplpl;
   FILE	*	highl_fp;
   int		highlight;
{
   current_file	= fname;
   hlfp		= highl_fp;
   hl		= highlight;
   if (! is_initialized) { get_options (); is_initialized = rtrue; }
   StoreMessages	(rtrue);
   InitStringMemory	();
   InitIdents		();
   init_include		();
   if (fname) Attribute.Position.FileName = MakeIdent (fname, strlen (fname));
   else	      Attribute.Position.FileName = NoIdent;
   push			(Attribute.Position.FileName);
   init_keywords	();
   BeginScanner		();
   BeginDeftab		();
   BeginFile		("");
   error_count += Parser ();
   CloseParser		();
   CloseDeftab		();
   PutDeftab		();
   ReleaseDeftab	();
   if (report) WriteMessages (stderr);
   CloseStringMemory	();
}

rbool Copy
#if defined __STDC__ | defined __cplusplus
   (tIdent ident, tPosition pos)
#else
   (ident, pos) tIdent ident; tPosition pos;
#endif
{
   struct stat		buf;
   char			string [256];
   register char *	p;
   int			i;

   if (do_copy && ident != NoIdent) {
      for (i = 1; i <= incl_cnt; i ++) {
	 char * b, * a = string + strlen (incl_file [i]);
	 (void) strcpy (string, incl_file [i]);
	 * a ++ = '/';
	 GetString (ident, a);
	 b = a + LengthSt (GetStringRef (ident));
	 strcpy (b + 1, extension);

	 * b = '\0';					/* try filename as is */
	 if (stat (string, & buf) >= 0) goto CopyFile;
	 * b = '.';
	 if (stat (string, & buf) >= 0) goto CopyFile;

	 for (p = a; * p; p ++) * p = tolower (* p);	/* try lower case */
	 * b = '\0';
	 if (stat (string, & buf) >= 0) goto CopyFile;
	 * b = '.';
	 if (stat (string, & buf) >= 0) goto CopyFile;

	 for (p = a; * p; p ++) * p = toupper (* p);	/* try upper case */
	 * b = '\0';
	 if (stat (string, & buf) >= 0) goto CopyFile;
	 * b = '.';
	 if (stat (string, & buf) >= 0) goto CopyFile;
      }
      MessageI ("cannot access include file", xxError, pos, xxIdent, (char *) & ident);
      return rfalse;
   CopyFile: ;
      BeginFile (string);
      push (ident);
   }
   return rtrue;
}

