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

#ifndef	MXDEFINE_H
#define	MXDEFINE_H

#include <config.h>

#include <stdio.h>
#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif 
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>

#if defined(_WINDOWS) || defined(WIN32)

#undef	DEBUG
#define	DEBUG	1

#include <windows.h>
#include <io.h>
#include <commdlg.h>
#include <process.h>
#ifndef __CYGWIN32__
#include <direct.h>
#endif

#if !defined (MAXPATHLEN) && defined (_MAX_PATH)
#define	MAXPATHLEN	_MAX_PATH
#endif
#if !defined (MAXPATHLEN) && defined (MAX_PATH)
#define	MAXPATHLEN	MAX_PATH
#endif

#ifdef __CYGWIN32__
#include <sys/param.h>
#endif

#ifdef WIN32
#define	XWORD	DWORD
#define	mxGetWindowWord(p1,p2)		GetWindowLong(p1,p2)
#define	mxSetWindowWord(p1,p2,p3)	SetWindowLong(p1,p2,p3)
#else
#define	XWORD	WORD
#define	mxGetWindowWord(p1,p2)		GetWindowWord(p1,p2)
#define	mxSetWindowWord(p1,p2,p3)	SetWindowWord(p1,p2,p3)
#define sprintf		wsprintf
#define vsprintf  	wvsprintf
#endif /* WIN32 */

#ifndef	R_OK
#define	R_OK	0
#endif /* R_OK */

#else /* _WINDOWS */

#include <sys/param.h>

#include <unistd.h>
#include <pwd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/wait.h>

#endif /* _WINDOWS */

#ifndef MAXPATHLEN
#define	MAXPATHLEN	512
#endif

#endif /* MXDEFINE_H */

