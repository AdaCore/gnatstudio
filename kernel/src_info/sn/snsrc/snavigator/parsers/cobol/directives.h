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

#ifndef yydirectives
#define yydirectives

/* $Id$ */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* line 2 "directives.lrk" */



#ifdef yacc_interface
#define directives			yyparse
#define yyInitStackSize	YYMAXDEPTH
#endif
#define rbool		char
#define rtrue		1
#define rfalse		0
					/* named constants for start symbols */

extern	rbool	directives_Debug;
extern	char *	directives_TokenName	[];

extern	void	Begindirectives	ARGS ((void));
extern	int	directives		ARGS ((void));
extern	int	directives2		ARGS ((int yyStartSymbol));
extern	void	Closedirectives	ARGS ((void));

#endif

