/*

Copyright (c) 2000, 2001, Red Hat, Inc.

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
#include <sys/stat.h>
#include <fcntl.h>

#include <tcl.h>
#include "crossrefP.h"

#define PRINT
Tcl_Encoding encoding = NULL;
int (*Paf_IsSkip)( char *yytext, int yyleng );

static void process_file( char *filename );
static void start_parser( char *filename_a, int is_cpp, int mode );

extern int main( int argc, char *argv[] )
{
   extern char *optarg;
   extern int   optind;
   char *pcInputFiles = 0;

   scope_g = SN_StrDup( "" );

   if( argc == 1 )
   {
      printf( "no argument\n" );
      exit( -1 );
   }
   else
   {
      int i;
      int opt;

      while(( opt = getopt( argc, argv, "y:" )) != EOF )
      {
         switch( opt )
         {
         case 'y' : pcInputFiles = optarg; break;
         }
      }

      if( pcInputFiles == 0 )
      {
         for( i = optind; i < argc; i++ )
         {
            process_file( argv[i] );
         }
      }
      else
      {
         FILE *input_files_fp;
         char acBuffer[1000];

         input_files_fp = fopen( pcInputFiles, "r" );

         if( input_files_fp == 0 )
         {
            perror( pcInputFiles );
            exit( -1 );
         }

         while( fgets( acBuffer, sizeof( acBuffer ), input_files_fp ))
         {
            char *pc;

            if( acBuffer[0] == '#' )
            {
               continue;
            }

            if( pc = strchr( acBuffer, '\n' ))
            {
               *pc = 0;
            }

            process_file( acBuffer );
         }
      }
   }
}

static void process_file( char *filename )
{
   fprintf( stderr, "file: %s\n", filename );

   if(( yyfd = open( filename, O_RDONLY )) >= 0 )
   {
      start_parser( filename, 1, 0 );
      close( yyfd );
   }
   else
   {
      perror( filename );
   }
}

static void start_parser( char *filename_a, int is_cpp, int mode )
{
   extern int yyfd;

   if( yyfd == -1 ) return;

   mode_g = mode;
   filename_g = filename_a;
   is_cpp_g = is_cpp;

   ivt = 0;
   iva = 0;
   init_stack();
   niveau = 0;

   /* beolvassuk az egesz file-t */
   if( f_ReadFile( yyfd ))
   {
      return;
   }

   if( mode == -1 )
   {
      Token_t Token;

      while( True )
      {
         int lex;
         Token = yylex();
         lex = Token->lex;
         f_TokenFreeAll( Token );
         if( lex == 0 ) break;
      }
   }
   else
   {
      while( token( 0 ) != 0 )
      {
         Declaration_t Declaration;

         if(( Declaration = f_Declaration( LEVEL_2 )))
         {
/*          f_DeclarationPrint( Declaration ); */
            f_DeclarationDestroy( Declaration );
         }
         else
         {
            f_DeclarationSkip();
         }

         if( niveau != 0 )
         {
#ifdef TEST
            printf( "bad niveau: %d (line:%d)\n", niveau, f_lineno( 0 ));
            exit( -1 );
#endif
         }
      }
   }
   f_read_end();
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

extern void Paf_panic( int i )
{
   exit( i );
}

extern int get_symbol (char *class_name,char *scope_local,char *name,char *arg_types, char *scope,char *ret_type, char *ret_define, int exact)
{
   scope   [0] = 0;
   ret_type[0] = 0;
   ret_define[0] = 0;

#ifdef PRINT
   printf( "get_symbol: %s|%s|%s|%s\n"
         , class_name ? class_name : ""
         , scope_local ? scope_local : ""
         , name ? name : ""
         , arg_types ? arg_types : ""
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

extern char *paf_type_to_string( int paf_type );

extern int put_cross_ref ( int type,int scope_type,int scope_lev,
   char *fnc_cls,char *fnc,char *fnc_arg_types,char *scope,char *what,
   char *arg_types,char *file,int lineno,int acc)
{
#ifdef PRINT
   printf( "crossref: %d %d %d %s %s %s %s %s %s %s %d %d\n"
         , type
         , scope_type
         , scope_lev
         , fnc_cls ? fnc_cls : "NULL"
         , fnc ? fnc : "NULL"
         , fnc_arg_types ? fnc_arg_types : "NULL"
         , scope ? scope : "NULL"
         , what ? what : "NULL"
         , arg_types ? arg_types : "NULL"
         , file ? file : "NULL"
         , lineno
         , acc );
#endif
   return 0;
}


