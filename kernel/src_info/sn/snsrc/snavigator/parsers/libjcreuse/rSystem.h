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

#ifndef yySystem
#define yySystem

/* $Id$ */

/* $Log:
 */

/* Ich, Doktor Josef Grosch, Informatiker, Jan. 1992 */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* interface for machine dependencies */

#ifndef rbool
#define rbool char
#endif
#define tFile int

/* binary IO */

extern tFile	OpenInput	ARGS ((char * yyFileName));
			/* Opens the file whose name is given by the	*/
			/* string parameter 'FileName' for input.	*/
			/* Returns an integer file descriptor.		*/

extern tFile	OpenOutput	ARGS ((char * yyFileName));
			/* Opens the file whose name is given by the	*/
			/* string parameter 'FileName' for output.	*/
			/* Returns an integer file descriptor.		*/

extern int	rRead		ARGS ((tFile yyFile, char * yyBuffer,
					int yySize));
			/* Reads 'Size' bytes from file 'tFile' and	*/
			/* stores them in a buffer starting at address	*/
			/* 'Buffer'.					*/
			/* Returns the number of bytes actually read.	*/

extern int	rWrite		ARGS ((tFile yyFile, char * yyBuffer,
					int yySize));
			/* Writes 'Size' bytes from a buffer starting	*/
			/* at address 'Buffer' to file 'tFile'.		*/
			/* Returns the number of bytes actually written.*/

extern void	rClose		ARGS ((tFile yyFile));
			/* Closes file 'tFile'.				*/

extern rbool IsCharacterSpecial	ARGS ((tFile yyFile));
			/* Returns TRUE when file 'tFile' is connected	*/
			/* to a character device like a terminal.	*/


/* calls other than IO */

extern char *	rAlloc		ARGS ((long yyByteCount));
			/* Returns a pointer to dynamically allocated	*/
			/* memory space of size 'ByteCount' bytes.	*/
			/* Returns NIL if space is exhausted.		*/

extern void	rFree		ARGS ((char * yyPtr));
			/* The dynamically allocated memory space	*/
			/* pointed to by 'Ptr' is released.		*/

extern long	rTime		ARGS ((void));
			/* Returns consumed cpu-time in milliseconds.	*/

extern int	GetArgCount	ARGS ((void));
			/* Returns number of arguments.			*/

extern void	GetArgument	ARGS ((int yyArgNum, char * yyArgument));
			/* Stores a string-valued argument whose index	*/
			/* is 'ArgNum' in the memory area 'Argument'.	*/

extern char *	GetEnvVar	ARGS ((char * yyName));
			/* Returns a pointer to the environment		*/
			/* variable named 'Name'.			*/

extern void	PutArgs		ARGS ((int yyArgc, char * * yyArgv));
			/* Dummy procedure that passes the values	*/
			/* 'argc' and 'argv' from Modula-2 to C.	*/

extern int	rErrNo		ARGS ((void));
			/* Returns the current system error code.	*/

extern int	rSystem		ARGS ((char * yyString));
			/* Executes an operating system command given	*/
			/* as the string 'String'. Returns an exit or	*/
			/* return code.					*/

extern void	rExit		ARGS ((int yyStatus));
			/* Terminates program execution and passes the	*/
			/* value 'Status' to the operating system.	*/

extern void	BEGIN_System	ARGS ((void));
			/* Dummy procedure with empty body.		*/

#endif

