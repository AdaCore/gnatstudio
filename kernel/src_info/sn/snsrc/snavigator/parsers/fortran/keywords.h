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

/* keywords.h:
	Contains names and characteristics of keywords for use by
	get_identifier and is_keyword in disambiguating
	keywords from identifiers.

	Included by forlex.c.

Must define SPLIT_KEYWORDS if using the old forlex with keyword hashing etc.

*/

#define IK 0x01	/* Allowed only in initial keyword of a statement (can be
		   preceded only by non-EK keywords) */
#define NP 0x02	/* Never followed by ( or =  */
#define MP 0x04	/* Must be followed by ( */
#define NI 0x08	/* Disallowed in logical IF */
#define EK 0x10	/* Cannot be followed by IK keyword: turns off initial_flag */
#define TY 0x20	/* Data type name */
#define NA 0x40	/* Never followed by alphabetic */


				/* Bisection search done at each
				   length step requires fixed-length
				   strings for keyword names.  Macro
				   below identifies which keyword is
				   the longest.
				 */
#define LONGEST_KEYWORD "DOUBLEPRECISION"

				/* Number of keywords in table */
#define NUM_KEYWORDS (sizeof(keywords)/sizeof(keywords[0]))

	/* Macro to determine whether a token class C is that of a data
	   type (for purposes of is_keyword) */
#ifndef OLDDEF
#define is_a_type_token(C) (((C)>=keytab_offset&&\
	        (C)-keytab_offset<keytab_size)?\
		  (keywords[keytab_index[(C)-keytab_offset]].context&TY):FALSE)
#else
				/* This is a simpler defn that will work
				   for is_keyword's needs. */
#define is_a_type_token(C) ((C)>=tok_BYTE && ((C)<=tok_REAL))
#endif

		/* Keyword list must be maintained in alphabetical
		   order.  New keywords can be added so long as their
		   context info is specified.  No other source code
		   changes are necessary, but of course new keywords
		   won't be recognized by the parser till you add
		   productions to fortran.y.  Also, if IK flag is not
		   set, is_keyword will have to look at it specially.
		 */
struct {
	char name[sizeof(LONGEST_KEYWORD)];
	short class,		/* token class */
	      context;		/* local-context flags */
} keywords[]={
{"ACCEPT",	tok_ACCEPT,	IK | EK},
{"ASSIGN",	tok_ASSIGN,	IK | NP | EK | NA},
{"BACKSPACE",	tok_BACKSPACE,	IK | EK},
#ifdef SPLIT_KEYWORDS
{"BLOCK",	tok_BLOCK,	IK | NP | NI},
#endif
{"BLOCKDATA",	tok_BLOCKDATA,	IK | EK | NP | NI},
{"BYTE",	tok_BYTE,	IK | NI | EK | TY},
{"CALL",	tok_CALL,	IK | NP | EK},
{"CHARACTER",	tok_CHARACTER,	IK | NI | EK | TY},
{"CLOSE",	tok_CLOSE,	IK | EK | MP | NA},
{"COMMON",	tok_COMMON,	IK | NP | NI | EK},
{"COMPLEX",	tok_COMPLEX,	IK | NI | EK | TY},
{"CONTINUE",	tok_CONTINUE,	IK | NP | EK | NA},
{"DATA",	tok_DATA,	IK | NI | EK},
{"DIMENSION",	tok_DIMENSION,	IK | NP | NI | EK},
{"DO",		tok_DO,		IK | NP | NI},
#ifdef SPLIT_KEYWORDS
{"DOUBLE",	tok_DOUBLE,	IK | NP | NI},
#endif
{"DOUBLECOMPLEX",tok_DOUBLECOMPLEX,	IK | NI | EK | TY},
{"DOUBLEPRECISION",tok_DOUBLEPRECISION,	IK | NI | EK | TY},
{"DOWHILE",	tok_DOWHILE,	IK | NI | EK},
{"ELSE",	tok_ELSE,	IK | NP | NI | NA}, /* simple ELSE only */
{"ELSEIF",	tok_ELSEIF,	IK | NI | EK | NA},
{"END",		tok_END,	IK | NP | NI | NA}, /* simple END only */
{"ENDDO",	tok_ENDDO,	IK | NP | NI | EK | NA},
{"ENDFILE",	tok_ENDFILE,	IK | EK},
{"ENDIF",	tok_ENDIF,	IK | NP | NI | EK | NA},
{"ENDMAP",	tok_ENDMAP,	IK | NP | NI | EK},
{"ENDSTRUCTURE",	tok_ENDSTRUCTURE,	IK | NP | NI | EK},
{"ENDUNION",	tok_ENDUNION,	IK | NP | NI | EK},
{"ENTRY",	tok_ENTRY,	IK | NP | NI | EK},
{"EQUIVALENCE",	tok_EQUIVALENCE,IK | NI | EK | MP | NA},
{"EXTERNAL",	tok_EXTERNAL,	IK | NP | NI | EK},
{"FILE",	tok_FILE,	IK | EK},
{"FORMAT",	tok_FORMAT,	IK | NI | EK | MP | NA},
{"FUNCTION",	tok_FUNCTION,	NP | NI | EK},
#ifdef SPLIT_KEYWORDS
{"GO",		tok_GO,		IK | NP},
#endif
{"GOTO",	tok_GOTO,	IK | EK},
{"IF",		tok_IF,		IK | NI | EK | NA},
{"IMPLICIT",	tok_IMPLICIT,	IK | NP | NI},
{"INCLUDE",	tok_INCLUDE,	IK | NP | NI | EK | NA},
{"INQUIRE",	tok_INQUIRE,	IK | EK | MP | NA},
{"INTEGER",	tok_INTEGER,	IK | NI | EK | TY},
{"INTRINSIC",	tok_INTRINSIC,	IK | NP | NI | EK},
{"LOGICAL",	tok_LOGICAL,	IK | NI | EK | TY},
{"MAP",	tok_MAP,	IK | NP | NI | EK},
{"NAMELIST",	tok_NAMELIST,	IK | NP | NI | EK},
{"NONE",	tok_NONE,	IK | NI | EK | TY | NA},
{"OPEN",	tok_OPEN,	IK | EK | MP | NA},
{"PARAMETER",	tok_PARAMETER,	IK | NI | EK | MP | NA},
{"PAUSE",	tok_PAUSE,	IK | NP | EK},
{"POINTER",	tok_POINTER,	IK | NI | EK | TY},
{"PRECISION",	tok_PRECISION,	IK | NI | EK | TY},
{"PRINT",	tok_PRINT,	IK | EK},
{"PROGRAM",	tok_PROGRAM,	IK | NP | NI | EK},
{"READ",	tok_READ,	IK | EK},
{"REAL",	tok_REAL,	IK | NI | EK | TY},
{"RECORD",	tok_RECORD,	IK | NI | EK | TY},
{"RETURN",	tok_RETURN,	IK | EK},
{"REWIND",	tok_REWIND,	IK | EK},
{"SAVE",	tok_SAVE,	IK | NP | NI | EK},
{"STOP",	tok_STOP,	IK | NP | EK},
{"STRUCTURE",	tok_STRUCTURE,	IK | NP | NI | EK},
{"SUBROUTINE",	tok_SUBROUTINE,	IK | NP | NI | EK},
{"THEN",	tok_THEN,	IK | NP | EK | NA},
{"TO",		tok_TO,		NI | EK},
{"TYPE",	tok_TYPE,	IK | EK},
{"UNION",	tok_UNION,	IK | NP | NI | EK},
{"WHILE",	tok_WHILE,	NI | EK | MP | NA},
{"WRITE",	tok_WRITE,	IK | EK | MP | NA},
};


		/* Lookup table to allow index in keywords table of
		   a given keyword to be found by its token number.
		   Initialized by init_keyhashtab. */
PRIVATE short
  keytab_offset,	/* lowest keyword token number */
  *keytab_index;	/* array of keyword indices  */
PRIVATE unsigned
  keytab_size;		/* Number of elements in keytab_index */

