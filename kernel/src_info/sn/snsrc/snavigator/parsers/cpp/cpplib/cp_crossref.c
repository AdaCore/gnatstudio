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

extern void Paf_insert_cross_ref_qry( char *pcLine )
{
   char *pcEnd;
   int i = 0;
   static char prev_filename[MAXPATHLEN]={0};
#if DEBUG_CROSS_REF
   char acFilename[1000];
#endif

#ifdef TRACE_FILE
   if( pf == 0 )
   {
      pf = fopen( "dbimp.trace", "w+" );
   }
#endif

   while( pcEnd = strchr( pcLine, ';' ))
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
      }
      pcLine = pcEnd + 1;
   }
   if (strcmp(file_g,prev_filename) != 0)
   {
      printf("Scanning %s\n",file_g);  /* Informs SN which file is being parsed. */
      fflush(stdout);

      strcpy(prev_filename,file_g);
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
         f_CompoundStatement( arg_types_g, arg_names_g);
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

   f_read_end();

   return;
}

extern int Put_symbol (int type, char *scope, char *sym_name, char *file, int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr, char *ret, char *arg_types, char *args, char *reserved, int start_lineno_highlight, int start_colpos_highlight, int end_lineno_highlight, int end_colpos_highlight )
{
   fprintf( stderr, "Fatal error: put_symbol is called from pass2\n" );
   fflush( stderr );

   return 0;
}

/* We need a dummy function, that we can link. */
extern void save_comment(int lineno, int charno, char *text, int length)
{
}

