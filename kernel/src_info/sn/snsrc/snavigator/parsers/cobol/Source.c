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

/* Ich, Doktor Josef Grosch, Informatiker, Feb. 1997 */

#include <stdio.h>
#include "Source.h"
#include "Scanner.h"
#include "rSystem.h"
#include "General.h"
#include "cobol.h"
#include "keywords.h"

#ifdef WIN32
#define	OPEN_MODE   "rb"
#else
#define	OPEN_MODE   "r"
#endif

extern	FILE *	yyin; 

	int	MaxColumn		= 72;
	rbool	free_format		= rfalse;

static	int	file_stackptr		= 0;
static	FILE *	file_stack		[32];

static	int	file_name_stackptr	= 0;
static	tIdent	file_name_stack		[32];
static	tStringRef file_name2_stack	[32];

static	char	the_current_file	[256];

void push
#if defined __STDC__ | defined __cplusplus
   (tIdent filename)
#else
   (filename) tIdent filename;
#endif
{
   file_name_stack [++ file_name_stackptr] =
   Attribute.name.EPos.FileName =
   Attribute.Position.FileName = filename;
}

void pop ARGS ((void))
{
   Attribute.name.EPos.FileName =
   Attribute.Position.FileName = file_name_stack [-- file_name_stackptr];
}

int BeginSource
#if defined __STDC__ | defined __cplusplus
   (char * FileName)
#else
   (FileName) char * FileName;
#endif
{
   if (* FileName) {
      file_stack [++ file_stackptr] = yyin;
      yyin = fopen (FileName, OPEN_MODE);
      file_name2_stack [file_stackptr] =
	 PutString (current_file, strlen (current_file));
      current_file = strcpy (the_current_file, FileName);
   } else {
      file_name_stack [0] = Attribute.Position.FileName;
   }
   return 0;
}

int GetLine
#if defined __STDC__ | defined __cplusplus
   (int File, char * Buffer, int Size)
#else
   (File, Buffer, Size) int File; char * Buffer; int Size;
#endif
{
   register FILE *	FilePtr		= yyin;
   register int		ch		;
   register char *	to		= Buffer;
   register int		LineLength	;

   do {					/* read one line		*/
      * to ++ = ch = getc (FilePtr);
   } while (ch != '\n' && ch != EOF);
   LineLength = to - Buffer;		/* compute length of line	*/
   if (ch == EOF)			/* detect end of file		*/
      if (LineLength == 1) return 0;
      else LineLength --;		/* remove EOF character		*/
   			/* truncate line to at most MaxColumn characters */
   if (LineLength > MaxColumn) {
      LineLength = MaxColumn + 1;
      Buffer [MaxColumn] = '\n';
   }
   if (dialect & (os | vs)) {		/* check for CBL or PROCESS in SNA */
      int i = 7;
      to = Buffer;
      while (* to == ' ' && i --)  /* search for first non-blank character */
	 to ++;
      if (i >= 0 &&
	 (strncmp (to, "CBL"    , 3) == 0 && (ch = to [3]) ||
	  strncmp (to, "PROCESS", 7) == 0 && (ch = to [7]))) {
	 switch (ch) {
	 case ' ':
	 case '\t':
	 case '\r':
	 case '\n': return LineLength;
	 }
      }
   }
   if (! free_format &&
       strncmp (Buffer, "-INC ", 5) && strncmp (Buffer, "-INCLUDE ", 9)) {
      int RemainToBlank	= 6;
      to = Buffer;			/* clear sequence number area	*/
      while (* to != '\t' && * to != '\n' && RemainToBlank --)
	 * to ++ = ' ';
      if (RemainToBlank < 0) {		/* move indicator to column one	*/
         Buffer [0] = Buffer [6]; Buffer [6] = ' ';
      }
   }
   return LineLength;
}

void CloseSource
#if defined __STDC__ | defined __cplusplus
   (int File)
#else
   (File) int File;
#endif
{
   if (file_stackptr > 0) {
      fclose (yyin);
      yyin = file_stack [file_stackptr];
      StGetString (file_name2_stack [file_stackptr --], the_current_file);
   }
   pop ();
}

