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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "crossref.h"
#include "paf.h"

#define PRINT

int (*Paf_IsSkip)( char *yytext, int yyleng );

static void process_file( char *filename );

extern int main( int argc, char *argv[] )
{
   if( argc == 1 )
   {
      printf( "no argument\n" );
      exit( -1 );
   }
   else
   {
      int i;
      for( i = 1; i < argc; i++ )
      {
         process_file( argv[i] );
      }
   }
}

static void process_file( char *filename )
{
   FILE *pfile;
   char acLine[10000];

   if(( pfile = fopen( filename, "r" )) == 0 )
   {
      perror( filename );
      return;
   }

   while( fgets( acLine, sizeof( acLine ), pfile ))
   {
      Paf_insert_cross_ref_qry( acLine );
   }

   fclose( pfile );
   return;
}

/*
extern int Put_symbol (int type,char *scope,char *sym_name,char *file, int start_lineno,int start_colpos,int end_lineno,int end_colpos,unsigned long attr, char *ret,char *arg_types,char *args, char *comment, int high_start_lineno,int high_start_colpos,int high_end_lineno, int high_end_colpos )
{
   printf( "Fatal error: put_symbol calling\n" );
   exit( -1 );
}
*/

extern char *Paf_Search_Include_dir( char *name )
{
   return name;
}

/*
extern void Paf_panic( void )
{
   exit( -1 );
}
*/

extern int get_symbol (char *class_name,char *scope_local,char *name,char *arg_types, char *scope,char *ret_type, char *ret_define, int exact)
{
   scope   [0] = 0;
   ret_type[0] = 0;
   ret_define[0] = 0;

#ifdef PRINT
   printf( "get_symbol: %s|%s|%s|%s\n"
         , class_name
         , scope_local
         , name
         , arg_types
         );
#endif
   return 0;
}

extern int get_class_or_typedef( char *name, char *type )
{
#ifdef PRINT
   printf( "get_class_or_typedef: %s\n"
         , name
         );
#endif
   return 0;
}

extern int put_cross_ref ( int type,int scope_type,int scope_lev,
   char *fnc_cls,char *fnc,char *fnc_arg_types,char *scope,char *what,
   char *arg_types,char *file,int lineno,int acc)
{
#ifdef PRINT
   printf( "crossref: %d %d %d %s %s %s %s %s %s %s %d %d\n"
         , type
         , scope_type
         , scope_lev
         , fnc_cls
         , fnc
         , fnc_arg_types
         , scope
         , what
         , arg_types
         , file
         , lineno
         , acc );
#endif
   return 0;
}



