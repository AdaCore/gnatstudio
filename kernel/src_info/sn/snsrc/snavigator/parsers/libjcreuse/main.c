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
#include <sys/stat.h>
#include "Position.h"
#include "Errors.h"
#include "Parser.h"

extern	FILE	*yyin;

FILE *	cross_ref_fp = (FILE *) 1;

main
#if defined __STDC__ | defined __cplusplus
   (int argc, char * argv [])
#else
   (argc, argv) int argc; char * argv [];
#endif
{
   FILE * hlfp;

   while (argc > 1) {
      char * arg = argv [1];
      if (arg [0] != '-') {
	 struct stat buf;
	 if (stat (arg, & buf) < 0) {
	    MessageI ("cannot access file", xxError, NoPosition, xxString, arg);
	 } else {
	    yyin = fopen (arg, "r");
	    hlfp = fopen ("hlf", "w");
	    (void) printf("%s...\n", arg);
	    (void) fflush(stdout);
	    start_parser (arg, 0, hlfp, 1);
	    fclose (yyin);
	    fclose (hlfp);
	 }
      } else {
	 cross_ref_fp = NULL;
      }
      argc --;
      argv ++;
   }
   return 0;
}

int put_symbol
#if defined __STDC__ | defined __cplusplus
   (int type, char *scope, char *sym_name, char *file,
   int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr,
   char *ret, char *arg_types, char *args, char *comment,
   int high_start_lineno, int high_start_colpos, int high_end_lineno, int high_end_colpos)
#else
   (type, scope, sym_name, file,
   start_lineno, start_colpos, end_lineno, end_colpos, attr,
   ret, arg_types, args, comment,
   high_start_lineno, high_start_colpos, high_end_lineno, high_end_colpos)
   int type; char *scope; char *sym_name; char *file;
   int start_lineno; int start_colpos; int end_lineno; int end_colpos; unsigned long attr;
   char *ret; char *arg_types; char *args; char *comment;
   int high_start_lineno; int high_start_colpos; int high_end_lineno; int high_end_colpos;
#endif
{
   printf ("SY %2d %s %s %s %d %d %d %d 0x%x %s %s %s %s\n",
      type, scope, sym_name, file, start_lineno, start_colpos, end_lineno,
      end_colpos, attr, ret, arg_types, args, comment);
}

int put_cross_ref
#if defined __STDC__ | defined __cplusplus
   (int type, int scope_type, int scope_lev,
   char *fnc_cls, char *fnc, char *fnc_arg_types, char *scope, char *what,
   char *arg_types, char *file, int lineno, int acc)
#else
   (type, scope_type, scope_lev,
   fnc_cls, fnc, fnc_arg_types, scope, what,
   arg_types, file, lineno, acc)
   int type; int scope_type; int scope_lev;
   char *fnc_cls; char *fnc; char *fnc_arg_types; char *scope; char *what;
   char *arg_types; char *file; int lineno; int acc;
#endif
{
   printf ("%2d %d %d %s %s %s %s %s %s %s %d %d\n",
      type, scope_type, scope_lev, fnc_cls, fnc, fnc_arg_types, scope, what,
      arg_types, file, lineno, acc);
}

