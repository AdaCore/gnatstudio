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

/* $Log$
/* Revision 1.1  2002/02/21 14:13:01  taras
/* Added SN sources
/*
/* Revision 1.1.1.1  2002/01/23 08:26:01  taras
/* Imported sources
/*
/* Revision 1.2  2000/04/20 00:38:40  spolk
/* 2000-04-19  Syd Polk  <spolk@redhat.com>
/*
/* 	* Merged from snavigator-elix-990915-branch.
/*
/* Revision 1.1.1.1.18.2  2000/02/11 23:54:46  spolk
/* 2000-02-11  Syd Polk  <spolk@cygnus.com>
/*
/* 	* configure.in: Added SUITE_NAME
/* 	* config.h.in configure: Regenerated.
/* 	* bitmaps/splashsn.gif: Added Shadow Man.
/* 	* english.txt.in: Added SUITE_NAME
/* 	* hyper/tclsql.c: Added SUITE_NAME
/* 	* gui/misc.tcl: Mucked around with text in About Box.
/* 	Changed all copyrights from "Red Hat Source-Navigator" to
/* 	"Source-Navigator"
/* 	Regenerated all Makefile.in.
/*
/* Revision 1.1.1.1.18.1  2000/02/10 02:12:55  spolk
/* 2000-02-09  Syd Polk  <spolk@cygnus.com>
/*
/* 	* configure.in: More adjustments to product names and the like.
/* 	Got rid of --enable-production.
/* 	* configure: Regenerated.
/* 	* install/cdkey_mangle.c: Removed.
/* 	Added GPL copyright notice to all source files.
/* 	Regenerated all Makefile.in files.
/*
/* Revision 1.1.1.1  1998/03/16 18:39:13  khamis
/* Souce-Navigator in a new devo tree
/*
/* Revision 1.4  1998/01/27 12:20:56  zkoppany
/* Modifications for the Tcl parser.
/*
 * Revision 1.1  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.0  1992/08/07  14:31:44  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

extern int  BeginSource  ARGS ((char * yyFileName));

   /*
      BeginSource is called from the scanner to open files.
      If not called input is read form standard input.
   */

extern int  GetLine      ARGS ((int yyFile, char * yyBuffer, int yySize));

   /*
      GetLine is called to fill a buffer starting at address 'Buffer'
      with a block of maximal 'Size' characters. Lines are terminated
      by newline characters (ASCII = 0xa). GetLine returns the number
      of characters transferred. Reasonable block sizes are between 128
      and 2048 or the length of a line. Smaller block sizes -
      especially block size 1 - will drastically slow down the scanner.
   */

extern void CloseSource  ARGS ((int yyFile));

   /*
      CloseSource is called from the scanner at end of file respectively
      at end of input. It can be used to close files.
   */

#endif

