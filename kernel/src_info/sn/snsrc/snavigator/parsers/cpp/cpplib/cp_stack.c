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

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "crossrefP.h"

#undef TRACE

#define GetStatus( depth ) sIfstack[depth].status
#define IfstackStatusMax( depth ) sIfstack[depth].status_max
#define IfstackElse( depth ) sIfstack[depth]._else
#define IfstackUndef( depth ) sIfstack[depth].undef

static int depth;
struct sIfstack sIfstack[1000];  /* old: 100 */
static char acStr[ 200000 ];  /* old: 100000 */
static char *pcStr = acStr;
static char *pcEnd = acStr + 199000;

ZZSTYPE v[ ZZSIZE ];   /* value stack */
int ivt;               /* index of first empty */
int iva;               /* index of 0 */
int keyw_cpp;  /* are the cpp keywords as keywords or identifiers processed ? */

static void SetStatus( int depth, int value );
static void HighLightBeg( void );
static void HighLight( void );
static void HighLightEnd( void );

/*****************************************************************************/

extern char *Strdup( char *pc, int leng )
{
   register char *pcReturn;
   register int i;

   if( pcStr + leng >= pcEnd )
   {
      pcStr = acStr;
   }

   pcReturn = pcStr;

/* memcpy((void *) pcStr, (void *)pc, leng ); */

   for( i = 0; i < leng; i++ )
   {
      pcStr[i] = pc[i];
   }

   pcStr += leng;
   *pcStr++ = 0;

   return pcReturn;
}

#ifdef STRDUP_OLD
static char *Strdup( char *pc )
{
   register char *pc1 = pcStr;
   register char *pc2 = pc;
   register char *pcReturn = pcStr;

   while( *pc2 )
   {
      *pc1++ = *pc2++;
   }
   *pc1++ = 0;

   if( pc1 > pcEnd )
   {
      pc1 = acStr;
   }

   pcStr = pc1;
   return pcReturn;
}
#endif

extern void concat_token( char *pname, int iva_from )
{
   int iva_to;
   int bFirst = True;
   char *pcIdent;

   if( pname == 0 ) return;

   iva_to = iva;
   iva    = iva_from;

   while( True )
   {
      if( iva >= iva_to )
      {
         iva = iva_to;
         return;
      }

      pcIdent = ident( 0 );

      if( bFirst )
      {
         bFirst = False;
      }
      else
      {
         if( isalnum( pname[-1] ) || pname[-1] == '_' || pname[-1] == '$' )
         {
            strcpy( pname, " " );
            pname++;
         }
      }

      strcpy( pname, pcIdent );
      pname += identlen( 0 );

      step( 1 );
   }
}

extern void init_stack( void )
{
   depth = 0;
   sIfstack[0].status = 1;
   pcStr = acStr;
}

static char acBuffer[10000];
static int iBuffer;

extern char *ident( int i )
{
   sString_t sString;
   int leng;
   char *pcRet;

   if( ivt <= iva+i )
   {
      f_read( i );
   }

   sString = v[(iva+i)&ZZMASK].yytext;

   if( iBuffer + sString.leng > sizeof( acBuffer ) - 1 )
   {
      iBuffer = 0;
   }

   if( sString.leng > sizeof( acBuffer ) - 1 )
      leng = sizeof( acBuffer ) - 1;
   else
      leng = sString.leng;

   pcRet = acBuffer + iBuffer;

   strncpy( pcRet, (const char *) sString.text, leng );
   pcRet[leng] = 0;

   iBuffer += leng;

   return pcRet;
}

#ifdef FUNCTION_MACRO

extern char *identlen( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yytext.leng;
}

extern int token( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   if( keyw_cpp )
      return v[(iva+i)&ZZMASK].yytoken_cpp;
   else
      return v[(iva+i)&ZZMASK].yytoken;
}

extern int identleng( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yytext.leng;
}

extern int f_lineno( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yylineno;
}

extern int f_charno( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yycharno;
}
#endif

static Token_t TokenAll;
static Token_t TokenAct;
static Token_t TokenEnd;

extern void f_read_end( void )
{
   if( TokenAll )
   {
      f_TokenFreeAll( TokenAll );
   }

   TokenAll = 0;  /* 12.11.97 rigo */
   TokenAct = 0;  /* 12.11.97 rigo */
   TokenEnd = 0;  /* 12.11.97 rigo */
}

extern void f_read( int i )
{
   register ZZSTYPE *pvt;

   while( ivt <= iva + i )
   {
      if( TokenAll == 0 )
      {
         TokenAll = yylex();
         TokenAct = TokenAll;
         TokenEnd = TokenAll->TokenPrev;
      }
      else
      {
         if( TokenAct == TokenEnd )
         {
            f_TokenFreeAll( TokenAll );
            TokenAll = yylex();
            TokenAct = TokenAll;
            TokenEnd = TokenAll->TokenPrev;
         }
         else
         {
            TokenAct = TokenAct->TokenNext;
         }
      }

#ifdef VERSION_COMPARE
      /* minden #if agba belemegy ( meg az #if 0 -ba is ) */
      TokenAct->mode = CPLEX_UNDEF;
#endif

/*    f_TokenOnePrint( TokenAct ); */

      switch( TokenAct->lex )
      {
      case SN_PP_IF:
         depth++;
#ifdef PP_TEST
printf( "#if niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         sIfstack[depth].niveau = niveau;
         sIfstack[depth].status_max = False;
         IfstackUndef( depth ) = False;
         IfstackElse( depth ) = False;

         if( TokenAct->mode == CPLEX_UNDEF )
            IfstackUndef( depth ) = True;

         if( IfstackUndef( depth ))
         {
            SetStatus( depth, True );
         }
         else
         {
            if( TokenAct->mode == CPLEX_FALSE )
               SetStatus( depth, False );
            else
               SetStatus( depth, True );
         }
         HighLightBeg();
         continue;
         break;

      case SN_PP_ELSE:
#ifdef PP_TEST
printf( "#else niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         if( depth > 0 )
         {
            IfstackElse( depth ) = True;
            if( IfstackUndef( depth ))
            {
               if( niveau != sIfstack[depth].niveau )
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
            else
            {
               if( IfstackStatusMax( depth ))
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
         }
         HighLight();
         continue;
         break;

      case SN_PP_ELIF:
#ifdef PP_TEST
printf( "#elif niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         if( depth > 0 )
         {
            IfstackElse( depth ) = True;
            if( TokenAct->mode == CPLEX_UNDEF )
               IfstackUndef( depth ) = True;

            if( IfstackUndef( depth ))
            {
               if( niveau != sIfstack[depth].niveau )
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
            else
            {
               if( IfstackStatusMax( depth ))
               {
                  SetStatus( depth, False );
               }
               else
               {
                  if( TokenAct->mode == CPLEX_FALSE )
                     SetStatus( depth, False );
                  else
                     SetStatus( depth, True );
               }
            }
         }
         HighLight();
         continue;
         break;

      case SN_PP_ENDIF:
#ifdef PP_TEST
printf( "#endif niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         HighLightEnd();
         depth--;
         if( depth < 0 )
         {
            depth = 0;
         }
         continue;
         break;

      case SN_PP_DEFINE:
         if( GetStatus( depth ))
         {
/*             Put_Macro( TokenAct ); */
         }
         continue;
         break;

      case SN_PP_INCLUDE:
         if( GetStatus( depth ))
         {
/*             Put_Include( TokenAct ); */
         }
         continue;
         break;

      case 0:
         goto process;

      default:
         if( ! GetStatus( depth ))
         {
            continue;
         }

process:
         pvt = v + ( ivt&ZZMASK );

         if( TokenAct->lex == SN_IDENTIFIER && TokenAct->keyword > 0 )
         {
            if( TokenAct->is_cpp )
            {
               pvt->yytoken     = TokenAct->lex;
               pvt->yytoken_cpp = TokenAct->keyword;
            }
            else
            {
               pvt->yytoken     = TokenAct->keyword;
               pvt->yytoken_cpp = TokenAct->keyword;
            }
         }
         else
         {
            pvt->yytoken     = TokenAct->lex;
            pvt->yytoken_cpp = TokenAct->lex;
         }
         pvt->yytext   = TokenAct->sString;
         pvt->yylineno = TokenAct->lineno_beg;
         pvt->yycharno = TokenAct->charno_beg;
         ivt++;
         break;
      }
   }
}

static void SetStatus( int depth, int value )
{
   if( sIfstack[depth-1].status )
   {
      sIfstack[depth].status = value;
   }
   else
   {
      sIfstack[depth].status = False;
   }

   if( sIfstack[depth].status )
   {
      sIfstack[depth].status_max = True;
   }
}

static int highlight_lineno_beg;
static int highlight_charno_beg;
static int highlight;

static void HighLightBeg( void )
{
   if( sIfstack[depth-1].status )
   {
      if( ! sIfstack[depth].status )
      {
         highlight = True;
         highlight_lineno_beg = TokenAct->lineno_beg;
         highlight_charno_beg = 0;
      }
      else
      {
         highlight = False;
      }
   }
}

static void HighLight( void )
{
   if( sIfstack[depth-1].status )
   {
      HighLightEnd();
      HighLightBeg();
   }
}

static void HighLightEnd( void )
{
   if( sIfstack[depth-1].status )
   {
      if( highlight )
      {
      }
   }
}



