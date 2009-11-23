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

#ifndef yyErrors
#define yyErrors

/* $Id$ */

/*
 * $Log$
 * Revision 1.1  2002/02/21 14:13:00  taras
 * Added SN sources
 *
 * Revision 1.1.1.1  2002/01/23 08:26:01  taras
 * Imported sources
 *
 * Revision 1.2  2000/04/20 00:38:40  spolk
 * 2000-04-19  Syd Polk  <spolk@redhat.com>
 *
 * 	* Merged from snavigator-elix-990915-branch.
 *
 * Revision 1.1.1.1.18.2  2000/02/11 23:54:41  spolk
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
 * Revision 1.1.1.1.18.1  2000/02/10 02:12:55  spolk
 * 2000-02-09  Syd Polk  <spolk@cygnus.com>
 *
 * 	* configure.in: More adjustments to product names and the like.
 * 	Got rid of --enable-production.
 * 	* configure: Regenerated.
 * 	* install/cdkey_mangle.c: Removed.
 * 	Added GPL copyright notice to all source files.
 * 	Regenerated all Makefile.in files.
 *
 * Revision 1.1.1.1  1998/03/16 18:39:12  khamis
 * Souce-Navigator in a new devo tree
 *
 * Revision 1.4  1998/01/27 12:20:40  zkoppany
 * Modifications for the Tcl parser.
 *
 * Revision 1.10  1997/11/30 22:40:23  grosch
 * changed type of ErrorCode from int to long
 * added missing initializations
 * suppress call of WritePosition if NoPosition
 * added functions BeginErrors and GetCount
 *
 * Revision 1.9  1997/05/27 13:25:04  grosch
 * truncation of filenames to 8.3 format
 *
 * Revision 1.8  1997/04/08 13:38:16  grosch
 * added prefix yy to all argument names
 *
 * Revision 1.7  1995/05/09  13:53:42  grosch
 * added void to argument list of functions without arguments
 *
 * Revision 1.6  1995/03/20  15:37:30  grosch
 * truncate lines to at most 80 characters
 *
 * Revision 1.5  1995/02/23  20:11:10  grosch
 * renamed Positions to Position, StringMem to StringM, Relations to Relation
 * output lines with at most 132 characters
 *
 * Revision 1.4  1994/12/04  19:29:55  grosch
 * added ANSI-C prototypes
 *
 * Revision 1.3  1994/11/07  12:39:52  grosch
 * added cpp variables BRIEF, FIRST, and TRUNCATE
 *
 * Revision 1.2  1994/01/29  22:13:50  grosch
 * renamed bool to rbool
 *
 * Revision 1.1  1992/08/13  12:14:11  grosch
 * deleted redefinition of bool
 *
 * Revision 1.0  1992/08/07  14:31:41  grosch
 * Initial revision
 *
 */

/* Ich, Doktor Josef Grosch, Informatiker, Juli 1992 */

#include <stdio.h>
#include "ratc.h"
#include "Position.h"

#define xxNoText		0
#define xxSyntaxError		1	/* error codes		*/
#define xxExpectedTokens	2
#define xxRestartPoint		3
#define xxTokenInserted	4
#define xxTooManyErrors	5
#define xxTokenFound		6
#define xxFoundExpected	7

#define xxFatal		1	/* error classes	*/
#define xxRestriction		2
#define xxError		3
#define xxWarning		4
#define xxRepair		5
#define xxNote			6
#define xxInformation		7

#define xxNone			0
#define xxInteger		1	/* info classes		*/
#define xxShort		2
#define xxLong			3
#define xxReal			4
#define xxBoolean		5
#define xxCharacter		6
#define xxString		7
#define xxSet			8
#define xxIdent		9
#define xxPosition		10

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

extern void (* Errors_Exit) ARGS ((void));
			/* Refers to a procedure that specifies		*/
			/* what to do if 'ErrorClass' = Fatal.		*/
			/* Default: terminate program execution.	*/

extern void BeginErrors	  ARGS ((void));
			/* Initialize the 'Errors' module.		*/

extern void StoreMessages ARGS ((rbool yyStore));
			/* Messages are stored if 'Store' = TRUE	*/
			/* for printing with the routine 'WriteMessages'*/
			/* otherwise they are printed immediately.	*/
			/* If 'Store'=TRUE the message store is cleared.*/

extern void ErrorMessage  ARGS ((long yyErrorCode, int yyErrorClass,
				tPosition yyPosition));
			/* Report a message represented by an integer	*/
			/* 'ErrorCode' and classified by 'ErrorClass'.	*/

extern void ErrorMessageI ARGS ((long yyErrorCode, int yyErrorClass,
				tPosition yyPosition, int yyInfoClass,
				char * yyInfo));
			/* Like the previous routine with additional	*/
			/* information of type 'InfoClass' at the	*/
			/* address 'Info'.				*/

extern void Message	  ARGS ((char * yyErrorText, int yyErrorClass,
				tPosition yyPosition));
			/* Report a message represented by a string	*/
			/* 'ErrorText' and classified by 'ErrorClass'.	*/

extern void MessageI	  ARGS ((char * yyErrorText, int yyErrorClass,
				tPosition yyPosition, int yyInfoClass,
				char * yyInfo));
			/* Like the previous routine with additional	*/
			/* information of type 'InfoClass' at the	*/
			/* address 'Info'.				*/

extern void WriteMessages ARGS ((FILE * yyFile));
			/* The stored messages are sorted by their	*/
			/* source position and printed on 'File'.	*/

extern int  GetCount	  ARGS ((int yyErrorClass));
   			/* Return the number of messages of class	*/
			/* 'yyErrorClass'.				*/

#endif

