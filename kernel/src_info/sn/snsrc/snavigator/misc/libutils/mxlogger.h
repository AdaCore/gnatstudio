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

#include <stdio.h>

#ifdef	MAIN_MODULE
#define	GLOBAL
#else
#define	GLOBAL	extern
#endif

GLOBAL	FILE   *LOGFP;			/* log file */
GLOBAL	short	debug;			/* = 1 debug on, = 0 debug off */

#define	DEBUG	1	/* It MUST be always 1 ! Zsolt Koppany: 8-May-97 */
#if DEBUG
#define	LOGGER(a)	{if (debug && LOGFP) {\
				fprintf a; \
				fflush (LOGFP);\
			}}
#define	LOGGER2(a)	{if (debug > 1 && LOGFP) {\
				fprintf a;\
				fflush(LOGFP);\
			}}
#define	LOGGER3(a)	{if (debug > 2 && LOGFP) {\
				fprintf a;\
				fflush(LOGFP);\
			}}
#else
#define	LOGGER(a)	;
#define	LOGSQL(a)	;
#define	LOGGER2(a)	;
#define	LOGGER3(a)	;
#endif

#ifndef TRUE
#define	TRUE	1
#endif

#ifndef	FALSE
#define	FALSE	0
#endif

