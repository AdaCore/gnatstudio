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

#ifndef yycopy_pars
#define yycopy_pars

/* $Id$ */

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

/* line 2 "copy_pars.lrk" */



#ifdef yacc_interface
#define copy_pars			yyparse
#define yyInitStackSize	YYMAXDEPTH
#endif
#define rbool		char
#define rtrue		1
#define rfalse		0
					/* named constants for start symbols */

extern	rbool	copy_pars_Debug;
extern	char *	copy_pars_TokenName	[];

extern	void	Begincopy_pars	ARGS ((void));
extern	int	copy_pars		ARGS ((void));
extern	int	copy_pars2		ARGS ((int yyStartSymbol));
extern	void	Closecopy_pars	ARGS ((void));

#endif

