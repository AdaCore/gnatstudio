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

#ifndef yyParser
#define yyParser

/* $Id$ */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* line 2 "Parser.lrk" */



#ifdef yacc_interface
#define Parser			yyparse
#define yyInitStackSize	YYMAXDEPTH
#endif
#define rbool		char
#define rtrue		1
#define rfalse		0
					/* named constants for start symbols */
#define yyprograms	1
#define yydescriptions	2

extern	rbool	Parser_Debug;
extern	char *	Parser_TokenName	[];

extern	void	BeginParser	ARGS ((void));
extern	int	Parser		ARGS ((void));
extern	int	Parser2		ARGS ((int yyStartSymbol));
extern	void	CloseParser	ARGS ((void));

#endif

