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
#ifndef _MSC_VER
#include <unistd.h>
#endif
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <tcl.h>

/* Valid debugging macros for this file:
	#define PRINT
	#define PRINT_FILE
	#define TRACE_FILE
*/

#ifdef WIN32
#define  OPEN_MODE   O_RDONLY|O_BINARY

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#else
#define  OPEN_MODE   O_RDONLY
#include <sys/param.h>
#endif /* WIN32 */

#include "crossrefP.h"

/* globalis valtozok */
char *filename_g;
int is_cpp_g;
int niveau;
Comp_t CompAct;
int niveauComp;
Symtab_t SymtabVariable;
Symtab_t SymtabClass;
int template_arg;
int mode_g;
int pass = 2;
FILE *hig_fp;
FILE *test_fp;
FILE *pf;
char prev_filename_g[MAXPATHLEN] = {0};

/* az aktualis sor elemei */
int type_g;
char *file_g;
int start_lineno_g;
int start_charno_g;
int end_lineno_g;
int end_charno_g;
int attr_g;
char *ret_g;
char *scope_g;
char *sym_name_g;
char *arg_types_g;
char *arg_names_g;
char *arg_pos_g;
char *arg_type_pos_g;
int ret_lineno_g;
int ret_charno_g;

int yyfd = -1;

extern void Paf_Cpp_Cross_Ref_Clean()
{
   free_lex_buffers();

   SymtabDestroy( SymtabVariable );
   SymtabDestroy( SymtabClass    );

   free_token_buffers();

   f_MacroFreeAll();
   MacroFreeTable();

   SymtabClass    = NULL;
   SymtabVariable = NULL;

   if (filename_g)
   {
      ckfree((char*)filename_g);
      filename_g = NULL;
   }
}

extern Boolean_t f_Statement ();

extern void Paf_decl_xref (char* pcLine) {
   char *pcEnd, c;
   int i = 0;

   while( pcEnd = strchr( pcLine, ';' ) )
   {
      *pcEnd = 0;
      switch( i++ )
      {
      case  0: break;
      case  1: type_g         = atoi  ( pcLine ); break;
      case  2: file_g         = SN_StrDup( pcLine ); break;
      case  3: start_lineno_g = atoi  ( pcLine ); break;
      case  4: start_charno_g = atoi  ( pcLine ); break;
      case  5: end_lineno_g   = atoi  ( pcLine ); break;
      case  6: end_charno_g   = atoi  ( pcLine ); break;
      case  7: scope_g        = SN_StrDup( pcLine ); break;
      case  8: sym_name_g     = SN_StrDup( pcLine ); break;
      case  9: arg_types_g    = SN_StrDup( pcLine ); break;
      case 10: is_cpp_g       = atoi  ( pcLine ); break;
      }
      pcLine = pcEnd + 1;
   }
   if (strcmp(file_g,prev_filename_g) != 0)
   {
      printf("Scanning %s\n",file_g);  /* Informs SN which file is being parsed. */
      fflush(stdout);

      strcpy(prev_filename_g,file_g);
   }

   if( filename_g == 0 || strcmp( filename_g, file_g ) != 0 )
   {
      if( yyfd >= 0 )
      {
         close( yyfd );
         yyfd = -1;
      }

      if( test_fp )
      {
         fclose( test_fp );
         test_fp = 0;
      }

      yyfd = open( file_g, OPEN_MODE );

      if( yyfd == -1 )
      {
         fprintf( stderr, "file cannot open: %s, %d\n", file_g, errno );
         return;
      }

      mode_g = 0;
      keyw_cpp = is_cpp_g;      /* default keyword processing is equal to
                                   default keyword highlighting */
      ivt = 0;
      iva = 0;
      init_stack();
      niveau = 0;
      niveauComp = 0;
      CompAct = 0;
      template_arg = False;

      if( filename_g )
      {
         ckfree( (char*)filename_g );
      }

      filename_g = SN_StrDup( file_g );

      /* beolvassuk az egesz file-t */
      if( f_ReadFile( yyfd ))
      {
         return;
      }
   }

   while( token( 0 ) != 0 )
   {
      if( f_lineno( 0 ) == start_lineno_g && f_charno( 0 ) == start_charno_g )
      {
         f_Statement();
         break;
      }
      step( 1 );
   }

   if( file_g )
   {
      ckfree( (char*)file_g );
      file_g = NULL;
   }
   if( scope_g )
   {
      ckfree( (char*)scope_g );
      scope_g = NULL;
   }
   if( sym_name_g )
   {
      ckfree( (char*)sym_name_g );
      sym_name_g = NULL;
   }
   f_read_end();

   return;
}

extern void Paf_insert_cross_ref_qry( char *pcLine )
{
   char *pcEnd, c;
   int i = 0;
#if DEBUG_CROSS_REF
   char acFilename[1000];
#endif

#ifdef TRACE_FILE
   if( pf == 0 )
   {
      pf = fopen( "dbimp.trace", "w+" );
   }
#endif

   while( pcEnd = strchr( pcLine, ';' ) )
   {
      *pcEnd = 0;
      switch( i++ )
      {
      case  0: break;
      case  1: type_g         = atoi  ( pcLine ); break;
      case  2: file_g         = SN_StrDup( pcLine ); break;
      case  3: start_lineno_g = atoi  ( pcLine ); break;
      case  4: start_charno_g = atoi  ( pcLine ); break;
      case  5: end_lineno_g   = atoi  ( pcLine ); break;
      case  6: end_charno_g   = atoi  ( pcLine ); break;
      case  7: attr_g         = atoi  ( pcLine ); break;
      case  8: ret_g          = SN_StrDup( pcLine ); break;
      case  9: scope_g        = SN_StrDup( pcLine ); break;
      case 10: sym_name_g     = SN_StrDup( pcLine ); break;
      case 11: arg_types_g    = SN_StrDup( pcLine ); break;
      case 12: arg_names_g    = SN_StrDup( pcLine ); break;
      case 13: is_cpp_g       = atoi  ( pcLine ); break;
      case 14: arg_pos_g      = SN_StrDup( pcLine ); break;
      case 15: arg_type_pos_g = SN_StrDup( pcLine ); break;
      case 16: sscanf (pcLine, "%d%c%d", &ret_lineno_g, &c, &ret_charno_g);
                                                     break;
      }
      pcLine = pcEnd + 1;
   }
   if (strcmp(file_g,prev_filename_g) != 0)
   {
      printf("Scanning %s\n",file_g);  /* Informs SN which file is being parsed. */
      fflush(stdout);

      strcpy(prev_filename_g,file_g);
   }

   if( filename_g == 0 || strcmp( filename_g, file_g ) != 0 )
   {
      if( yyfd >= 0 )
      {
         close( yyfd );
         yyfd = -1;
      }

      if( test_fp )
      {
         fclose( test_fp );
         test_fp = 0;
      }

      yyfd = open( file_g, OPEN_MODE );

      if( yyfd == -1 )
      {
         fprintf( stderr, "file cannot open: %s, %d\n", file_g, errno );
         return;
      }

#ifdef PRINT_FILE
      {
         struct stat buf;
         if( fstat( yyfd, &buf ))
         {
            fprintf( stderr, "fstat error: %d by file: %s\n", errno, filename_g );
            return;
         }

         printf( "file: %s size: %6d\n", file_g, buf.st_size );
      }
#endif

#if DEBUG_CROSS_REF
      sprintf( acFilename, "%s.HIGH", file_g );
      test_fp = fopen( acFilename, "w+" );
#endif

      mode_g = 0;

      keyw_cpp = is_cpp_g;      /* default keyword processing is equal to
                                   default keyword highlighting */
      ivt = 0;
      iva = 0;
      init_stack();
      niveau = 0;
      niveauComp = 0;
      CompAct = 0;
      template_arg = False;

      if( filename_g )
      {
         ckfree( (char*)filename_g );
      }

      filename_g = SN_StrDup( file_g );

      /* beolvassuk az egesz file-t */
      if( f_ReadFile( yyfd ))
      {
         return;
      }
   }

   while( token( 0 ) != 0 )
   {
      if( f_lineno( 0 ) == start_lineno_g && f_charno( 0 ) == start_charno_g )
      {
         f_CompoundStatement( arg_types_g, arg_names_g, arg_pos_g, arg_type_pos_g);
         { /* add reference to the returned type */
            Type_t Type = f_TypeFromString( ret_g );

            if( Type != 0 )
                f_TypeBasic( Type, ret_lineno_g, ret_charno_g );
         }

         if( f_lineno( -1 ) != end_lineno_g || f_charno( -1 ) != end_charno_g )
         {
/* Ez elofordulhat, mert az elso menet okosabban meg tudja talalni
a compound statement veget.
*/
#ifdef PRINT
            printf( "beg of compound statement: pass1: %d.%d\n"
                  , start_lineno_g
                  , start_charno_g
                  );
            printf( "end of compound statement: pass1: %d.%d pass2: %d.%d\n"
                  , end_lineno_g
                  , end_charno_g
                  , f_lineno( -1 )
                  , f_charno( -1 )
                  );
#endif
/* Ilyenkor ujranyittatjuk a file-t */
            if( filename_g )  /* Zsolt Koppany, 21-apr-97 */
            {
               ckfree((char*) filename_g );
            }
            filename_g = 0;
         }
         break;
      }
      step( 1 );
   }

   if( file_g )
   {
      ckfree( (char*)file_g );
      file_g = NULL;
   }
   if( ret_g )
   {
      ckfree( (char*)ret_g );
      ret_g = NULL;
   }
   if( scope_g )
   {
      ckfree( (char*)scope_g );
      scope_g = NULL;
   }
   if( sym_name_g )
   {
      ckfree( (char*)sym_name_g );
      sym_name_g = NULL;
   }
   if( arg_types_g )
   {
      ckfree( (char*)arg_types_g );
      arg_types_g = NULL;
   }
   if( arg_names_g )
   {
      ckfree( (char*)arg_names_g );
      arg_names_g = NULL;
   }
   if( arg_pos_g )
   {
      ckfree( (char*)arg_pos_g );
      arg_pos_g = NULL;
   }
   if( arg_type_pos_g )
   {
      ckfree( (char*)arg_type_pos_g );
      arg_type_pos_g = NULL;
   }

   f_read_end();

   return;
}

extern int Put_symbol (int type, char *scope, char *sym_name, char *file, int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr, char *ret, char *arg_types, char *args, char *reserved, int start_lineno_highlight, int start_colpos_highlight, int end_lineno_highlight, int end_colpos_highlight )
{
/*
   fprintf( stderr, "Fatal error: put_symbol is called from pass2\n" );
   fflush( stderr );
*/
   /* to enable local variables during second pass */
   put_symbol (type, scope, sym_name, file, start_lineno, start_colpos,
               end_lineno, end_colpos, attr, ret, arg_types, args, reserved,
               start_lineno_highlight, start_colpos_highlight,
               end_lineno_highlight, end_colpos_highlight);
   return 0;
}

/* We need a dummy function, that we can link. */
extern void save_comment(int lineno, int charno, char *text, int length)
{
}

