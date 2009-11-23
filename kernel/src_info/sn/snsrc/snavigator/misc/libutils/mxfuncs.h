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

#ifndef _MXFUNCS
#define	_MXFUNCS

#ifdef WIN32

#ifndef HAS_STDARG
#define HAS_STDARG 1
#endif /* HAS_STDARG */

#ifndef USE_PROTOTYPE
#define USE_PROTOTYPE 1
#endif /* USE_PROTOTYPE */

#endif /* WIN32 */

#if defined(__STDC__) || defined(HAS_STDARG)
#include <stdarg.h>
#else
#include <varargs.h>

#endif

#if defined(__STDC__) || defined(HAS_STDARG)
#   define MX_VARARGS(type, name) (type name, ...)
#   define MX_VARARGS_DEF(type, name) (type name, ...)
#   define MX_VARARGS_START(type, name, list) (va_start(list, name), name)
#else
#   ifdef __cplusplus
#       define MX_VARARGS(type, name) (type name, ...)
#       define MX_VARARGS_DEF(type, name) (type va_alist, ...)
#   else
#       define MX_VARARGS(type, name) ()
#       define MX_VARARGS_DEF(type, name) (va_alist)
#   endif
#   define MX_VARARGS_START(type, name, list) \
        (va_start(list), va_arg(list, type))
#endif

/*
 * Definitions that allow this header file to be used either with or
 * without ANSI C features like function prototypes.
 */

#undef _ANSI_ARGS_
#undef CONST
#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus) || defined(USE_PROTOTYPE)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#   define CONST const
#else
#   define _ANSI_ARGS_(x)	()
#   define CONST
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/*
 * Macro to use instead of "void" for arguments that must have
 * type "void *" in ANSI C;  maps them to type "char *" in
 * non-ANSI systems.
 */

#ifndef VOID
#   ifdef __STDC__
#       define VOID void
#   else
#       define VOID char
#   endif
#endif

EXTERN int DbReturn _ANSI_ARGS_((int dbcode));
EXTERN int CheckDbReturn _ANSI_ARGS_((int dbcode));

EXTERN char *GetHomeDir _ANSI_ARGS_(());

#ifdef _TCL
EXTERN void Tcl_XListInit _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int  Itcl_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int Tcl_InfoCmd _ANSI_ARGS_((ClientData cli, Tcl_Interp *in, int argc, char **argv));

EXTERN int Sn_Highlight_Text _ANSI_ARGS_((ClientData cli, Tcl_Interp *in, int argc, char **argv));
EXTERN int brace_balance _ANSI_ARGS_((ClientData cli, Tcl_Interp *in, int argc, char **argv));
#endif /* _TCL */

#endif /* _MXFUNCS */

