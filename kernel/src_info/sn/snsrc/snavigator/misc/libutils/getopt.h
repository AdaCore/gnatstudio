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

/*
 * Copyright (c) 1987, 1993, 1994, 1996
 *  The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *  This product includes software developed by the University of
 *  California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "config.h"

#if  !defined(__GETOPT_H__) && !HAVE_GETOPT
#define __GETOPT_H__

#if !defined(PROTOTYPE)
#if defined(__STDC__) || defined(_MSDOS) || defined(_WIN32)
#define PROTOTYPE(x) x
#else
#define PROTOTYPE(x) ()
#endif
#endif

#ifndef _MACINTOSH
#if defined(__MWERKS__) || defined(applec) || defined(THINK_C)
#define _MACINTOSH
#endif
#endif

/*
#if defined(_MSDOS) || defined(_WIN32) || defined(_MACINTOSH)
#include "win-mac.h"
#endif
*/

#ifndef KRB5_CALLCONV
#define KRB5_CALLCONV
#define KRB5_DLLIMP
#define KRB5_CALLCONV_C
#endif
#ifndef FAR
#define FAR
#define NEAR
#endif

struct option {
	char *	name;
	int  has_arg;
	int * flag;
	int val;
};

KRB5_DLLIMP extern int   opterr;      /* if error message should be printed */
KRB5_DLLIMP extern int   optind;      /* index into parent argv vector */
KRB5_DLLIMP extern int   optopt;      /* character checked for validity */
KRB5_DLLIMP extern int   optreset;    /* reset getopt */
KRB5_DLLIMP extern char *optarg;      /* argument associated with option */

KRB5_DLLIMP int KRB5_CALLCONV
getopt PROTOTYPE ((int, char * const *, const char *));

KRB5_DLLIMP int KRB5_CALLCONV
getopt_long PROTOTYPE ((int, char **, char *, struct option *, int *));

#endif

