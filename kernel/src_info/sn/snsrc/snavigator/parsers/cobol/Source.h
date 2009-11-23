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

#ifndef xySource
#define xySource

/* $Id$ */

/* Ich, Doktor Josef Grosch, Informatiker, March 1997 */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

#include "ratc.h"
#include <string.h>
#include "Idents.h"

extern int	MaxColumn	;
extern rbool	free_format	;

extern void	push	ARGS ((tIdent filename));
extern void	pop	ARGS ((void));

extern int  BeginSource ARGS ((char * FileName));

   /*
      BeginSource is called from the scanner to open files.
      If not called input is read form standard input.
   */

extern int  GetLine	ARGS ((int File, char * Buffer, int Size));

   /*
      GetLine is called to fill a buffer starting at address 'Buffer'
      with a block of maximal 'Size' characters. Lines are terminated
      by newline characters (ASCII = 0xa). GetLine returns the number
      of characters transferred. Reasonable block sizes are between 128
      and 2048 or the length of a line. Smaller block sizes -
      especially block size 1 - will drastically slow down the scanner.
   */

extern void CloseSource	ARGS ((int File));

   /*
      CloseSource is called from the scanner at end of file respectively
      at end of input. It can be used to close files.
   */

#endif

