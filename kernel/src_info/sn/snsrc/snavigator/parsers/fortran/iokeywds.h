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

/* iokeywds.h:
      Contains definitions of I/O control-list specifier keywords
      and their properties.  If VMS or VMS_IO is defined, supports
      many VMS-specific keywords needed to deal with VMS files.
      (Suppressed if NO_VMS_IO is defined.)  You may add other
      locally supported specifiers as appropriate (order is not
      important).

    Copyright (C) 1993 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/



	/* Define bit flag for each I/O command */
#define RD  0x1			/* read */
#define WR  0x2			/* write */
#define OP  0x4			/* open */
#define CL  0x8			/* close */
#define INQ 0x10		/* inquire */
#define BSP 0x20		/* backspace */
#define ENF 0x40		/* endfile */
#define REW 0x80		/* rewind */
#define ANYSTMT (RD|WR|OP|CL|INQ|BSP|ENF|REW)

	/* Define bit flag for each type of specifier value.  All types
	   (see symtab.h) are < 16 so these values will fit in a short.*/
#define LAB (1<<type_LABEL)	/* label */
#define CHR (1<<type_STRING)	/* character */
#define INT (1<<type_INTEGER)	/* integer */
#define LOG (1<<type_LOGICAL)	/* logical */
#define STAR (1<<type_UNDECL)	/* does duty for '*' */
#define FID (LAB|CHR|STAR)	/* format ID */
#define UID (INT|CHR|STAR)	/* unit ID */
#define NML (1<<type_NAMELIST)	/* namelist name */
#define ANYTYPE (unsigned short)(~0) /* used for unknown keywords */

struct {
    char *name;			/* Name of keyword */
    unsigned short allowed_stmts, /* Where keyword can occur */
    		   allowed_types; /* datatypes allowed for value */
    unsigned		/* Flags for handling the specifier value */
      implies_use: 1,		/* Value is used */
      implies_set: 1,		/* Var is set (except INQUIRE) */
      inquire_set: 1,		/* Var is set by INQUIRE */
      nonstandard: 1,		/* not Fortran 77 standard keyword */
      special:	   1;		/* Indicates special cases */
} io_keywords[]={
			/* List has commonest ones first for fast lookup */
/*Name	       Stmts   Types   UseSetInqStdSpcl */
{"END",		RD,	LAB,	1, 0, 0, 0, 0},
{"ERR",		ANYSTMT,LAB,	1, 0, 0, 0, 0},
{"FILE",	OP|INQ,	CHR,	1, 0, 0, 0, 0},
{"UNIT",	ANYSTMT,UID,	1, 0, 0, 0, 0},
{"STATUS",	OP|CL,	CHR,	1, 0, 0, 0, 0},
			/* The rest are alphabetical. */
{"ACCESS",	OP|INQ,	CHR,	1, 0, 1, 0, 0},
{"BLANK",	OP|INQ,	CHR,	1, 0, 1, 0, 0},
{"DIRECT",	INQ,	CHR,	0, 0, 1, 0, 0},
{"EXIST",	INQ,	LOG,	0, 0, 1, 0, 0},
{"FMT",		RD|WR,	FID,	1, 0, 0, 0, 0},
{"FORM",	OP|INQ,	CHR,	1, 0, 1, 0, 0},
{"FORMATTED",	INQ,	CHR,	0, 0, 1, 0, 0},
{"IOSTAT",	ANYSTMT,INT,	0, 1, 1, 0, 0},
{"NAMED",	INQ,	LOG,	0, 0, 1, 0, 0},
{"NEXTREC",	INQ,	INT,	0, 0, 1, 0, 0},
{"NML",		RD|WR,	NML,	1, 0, 0, 1, 0},
{"NUMBER",	INQ,	INT,	0, 0, 1, 0, 0},
{"OPENED",	INQ,	LOG,	0, 0, 1, 0, 0},
{"REC",		RD|WR,	INT,	1, 0, 0, 0, 0},
{"RECL",	OP|INQ,	INT,	1, 0, 1, 0, 0},
{"SEQUENTIAL",	INQ,	CHR,	0, 0, 1, 0, 0},
{"UNFORMATTED",	INQ,	CHR,	0, 0, 1, 0, 0},
			/* NAME is a special case for VMS */
#ifndef VMS_IO
{"NAME",	INQ,	CHR,	0, 0, 1, 0, 0}, /* normal definition */
#else
{"NAME",	OP|INQ,	CHR,	1, 0, 1, 0, 1}, /* VMS definition */
#endif /*VMS_IO*/
			/* Other simple VMS-isms go here. */
#ifdef VMS_IO
{"BLOCKSIZE",	OP,	INT,	1, 0, 0, 1, 0},
{"BUFFERCOUNT",	OP,	INT,	1, 0, 0, 1, 0},
{"CARRIAGECONTROL",OP|INQ,CHR,	1, 0, 1, 1, 0},
{"DEFAULTFILE",	OP,	CHR,	1, 0, 0, 1, 0},
{"DISP",	OP|CL,	CHR,	1, 0, 0, 1, 0},
{"DISPOSE",	OP|CL,	CHR,	1, 0, 0, 1, 0},
{"EXTENDSIZE",	OP,	INT,	1, 0, 0, 1, 0},
{"INITIALSIZE",	OP,	INT,	1, 0, 0, 1, 0},
{"MAXREC",	OP,	INT,	1, 0, 0, 1, 0},
{"ORGANIZATION",OP|INQ,	CHR,	1, 0, 1, 1, 0},
{"RECORDSIZE",	OP,	INT,	1, 0, 0, 1, 0},
{"RECORDTYPE",	OP|INQ,	CHR,	1, 0, 1, 1, 0},
{"TYPE",	OP,	CHR,	1, 0, 0, 1, 0},
#endif /*VMS_IO*/
			/* Last entry (for not-founds) has defns that should
			   do the right thing most of the time. */
{NULL,	     ANYSTMT,ANYTYPE,	1, 0, 1, 1, 0},
};

	/* Lookup table which maps statement classes into
	   the corresponding bit fields of io_keywords table.
	   Order: commonest first for faster lookup. */
struct {
	short stmt_class, stmt_flag;
} local_class[]= {
{tok_READ,	 RD},
{tok_WRITE,	 WR},
{tok_OPEN,	 OP},
{tok_CLOSE,	 CL},
{tok_BACKSPACE,	 BSP},
{tok_ENDFILE,	 ENF},
{tok_REWIND,	 REW},
{tok_INQUIRE,	 INQ},
};
#define NUM_IO_STMTS (sizeof(local_class)/sizeof(local_class[0]))


	/* The following table contains special keywords for the VMS
	   form of OPEN statement.  These keywords occur alone, i.e.
	   without the =value normally required for I/O control list
	   keywords. */
#ifdef VMS_IO
char *special_open_keywds[]={
"NOSPANBLOCKS",
"READONLY",
"SHARED",
};
#define NUM_SPECIAL_OPEN_KEYWDS (sizeof(special_open_keywds) \
			       / sizeof(special_open_keywds[0]))

#endif /*VMS_IO*/

