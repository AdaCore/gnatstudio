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
#include <string.h>
#include <stdlib.h>
#include <tcl.h>

#undef TOKEN_TRACE

#include "cpdefines.h"
#include "cplex.h"

static Token_t __MacroProcess( Token_t TokenToProcess, Macro_t *pMacroUsed );
static Token_t f_ProcessTokenMacroInput( Macro_t *pMacroUsed, int *pmacro );
static Param_t f_ProcessParamCreate( Macro_t *pMacroUsed );
static Token_t f_ProcessParamToken( Macro_t *pMacroUsed, int *pbContinue );
static void f_ProcessParamFree( Param_t Param );
static Token_t f_ProcessTokenInput( void );
static void f_ProcessTokenUnput( Token_t Token );

static Token_t TokenProcessInput;
static Token_t TokenProcessFirst;
static Token_t TokenProcessUnput;

extern void f_MacroProcess( Macro_t Macro )
{
   Token_t TokenToProcess = Macro->Token;
   Token_t TokenProcessed;
   Macro_t MacroUsed;
   Macro_t MacroTilt = 0;
   Macro_t MacroWork;

   int bFirst = True;

   if( Macro->Token == 0 )    /* 12.11.97 rigo */
   {
      Macro->TokenProcessed = 0;
      return;
   }

   while( True )
   {
      MacroUsed = 0;

      TokenProcessed = __MacroProcess( TokenToProcess, &MacroUsed );

      if( bFirst ) { bFirst = False; }
      else         { f_TokenFreeAll( TokenToProcess ); }

      if( MacroUsed )
      {
         MacroWork = MacroUsed;

         WHILE_MACRO( MacroWork )
            MacroWork->tilt = True;
         END_WHILE

         f_MacroAppend( &MacroTilt, MacroUsed );
         TokenToProcess = TokenProcessed;
      }
      else
      {
         Macro->TokenProcessed = TokenProcessed;
         break;
      }
   }

   /* visszaallitjuk a tilt flag-eket, es szetbontjuk a lancot */
   MacroWork = MacroTilt;
   WHILE_MACRO( MacroWork )
      MacroWork->tilt = False;
      MacroWork->MacroNext = MacroWork;
      MacroWork->MacroPrev = MacroWork;
   END_WHILE
}

static Token_t __MacroProcess( Token_t TokenToProcess, Macro_t *pMacroUsed )
{
   Token_t Token;
   Token_t TokenProcessed;
   int macro;
   Token_t TokenProcessInputSave;
   Token_t TokenProcessFirstSave;
   Token_t TokenProcessUnputSave;

   TokenProcessed    = 0;

   TokenProcessInputSave = TokenProcessInput;
   TokenProcessFirstSave = TokenProcessFirst;
   TokenProcessUnputSave = TokenProcessUnput;

   TokenProcessInput = TokenToProcess;
   TokenProcessFirst = 0;
   TokenProcessUnput = 0;

   while( True )
   {
/*    27.11.97 rigo */
/*    if(( Token = f_ProcessTokenMacroInput( pMacroUsed, &macro )) == 0 ) */
/*    { */
/*       break; */
/*    } */

      Token = f_ProcessTokenMacroInput( pMacroUsed, &macro );
      if( ! macro )
      {
         if( Token == 0 ) break;   /* nincs tobb Token */
      }
      else
      {
         if( Token == 0 ) continue;   /* ures macrodefinicio */
      }

      f_TokenAppend( &TokenProcessed, Token );
   }

   TokenProcessInput = TokenProcessInputSave;
   TokenProcessFirst = TokenProcessFirstSave;
   TokenProcessUnput = TokenProcessUnputSave;

   return TokenProcessed;
}

static Token_t f_ProcessTokenMacroInput( Macro_t *pMacroUsed, int *pmacro )
{
   Token_t TokenMerge;
   Token_t TokenNext;
   Macro_t Macro;
   Param_t Param;
   Token_t Token;

   Token = f_ProcessTokenInput();

#ifdef TRACE
   if( Token )
   {
      printf( "ProcessTokenInput: %*.*s\n"
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text );
   }
   else
   {
      printf( "ProcessTokenInput: NULL\n" );
   }
#endif

   *pmacro = False;

   if( Token )
   {
      if( Token->lex == SN_IDENTIFIER && Token->iParameter == -1 )
      {
         if(( Macro = f_MacroFind( Token->sString )) && ! Macro->tilt )
         {
            if( Macro->typ == CPP_DEFINE )
            {
               TokenMerge = f_MacroMerge( Macro, Token, (Param_t) 0, False );
               *pmacro = True;
               f_MacroAppend( pMacroUsed, Macro );
               f_TokenFree( Token );
               return TokenMerge;
            }
            else if( Macro->typ == CPP_MACRO )
            {
               TokenNext = f_ProcessTokenInput();

               if( TokenNext && TokenNext->lex == '(' )
               {
                  f_TokenFree( TokenNext );
                  Param = f_ProcessParamCreate( pMacroUsed );
                  TokenMerge = f_MacroMerge( Macro, Token, Param, False );
                  *pmacro = True;
                  f_MacroAppend( pMacroUsed, Macro );
                  f_TokenFree( Token );
                  f_ProcessParamFree( Param );
                  return TokenMerge;
               }
               else
               {
                  f_ProcessTokenUnput( TokenNext );
                  return Token;
               }
            }
            else /* Macro->typ == CPP_UNDEF */
            {
            }
         }
      }
   }

   return Token;
}

static Param_t f_ProcessParamCreate( Macro_t *pMacroUsed )
{
   Param_t Param = (Param_t) ckalloc( sizeof( sParam_t ));
   Token_t Token;

   Param->iToken = 0;
   Param->pToken = (Token_t *) ckalloc( 100 * sizeof( Token_t ));

   while( True )
   {
      int bContinue;

      Token = f_ProcessParamToken( pMacroUsed, &bContinue );
      Param->pToken[Param->iToken++] = Token;
      if( ! bContinue )
      {
         break;
      }
   }

   return Param;
}

static Token_t f_ProcessParamToken( Macro_t *pMacroUsed, int *pbContinue )
{
   Token_t TokenReturn = 0;
   Token_t Token;
   int p = 0;
   int lex;

   while( True )
   {
      int macro;

      Token = f_ProcessTokenMacroInput( pMacroUsed, &macro );

      if( ! macro )
      {
     if( Token == 0 )   /* nincs tobb Token */ /* 27.11.97 rigo */
     {
        return TokenReturn;
     }

         lex = Token->lex;

         if( p == 0 )
         {
            if( lex == ',' )
            {
               *pbContinue = True;
               f_TokenFree( Token );
               return TokenReturn;
            }
            if( lex == ')' || lex == 0 )
            {
               *pbContinue = False;
               f_TokenFree( Token );
               return TokenReturn;
            }
         }
         /**/ if( lex == '(' ) p++;
         else if( lex == ')' ) p--;
      }
      else
      {
     if( Token == 0 )   /* ures a macrodefinicio */ /* 27.11.97 rigo */
     {
        continue;
     }
      }

      f_TokenAppend( &TokenReturn, Token );
   }
}

static void f_ProcessParamFree( Param_t Param )
{
   int i;

   for( i = 0; i < Param->iToken; i++ )
   {
      Token_t Token = Param->pToken[i];
      f_TokenFreeAll( Token );
   }

   ckfree((char*) Param->pToken );
   ckfree((char*) Param );
}

static Token_t f_ProcessTokenInput( void )
{
   Token_t Token;

   if( TokenProcessUnput )
   {
      Token = TokenProcessUnput;
      TokenProcessUnput = 0;
#ifdef TOKEN_TRACE
      printf( "Input: %*.*s\n"
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text );
#endif
      return Token;
   }

   if( TokenProcessFirst == 0 )
   {
      TokenProcessFirst = TokenProcessInput;
#ifdef TOKEN_TRACE
      printf( "Input: %*.*s\n"
            , TokenProcessInput->sString.leng
            , TokenProcessInput->sString.leng
            , TokenProcessInput->sString.text );
#endif
      return f_TokenDuplicate( TokenProcessInput );
   }
   else
   {
      if( TokenProcessInput->TokenNext == TokenProcessFirst )
      {
#ifdef TOKEN_TRACE
         printf( "Input: <NULL>\n" );
#endif
         return 0;
      }
      else
      {
         TokenProcessInput = TokenProcessInput->TokenNext;
#ifdef TOKEN_TRACE
         printf( "Input: %*.*s\n"
               , TokenProcessInput->sString.leng
               , TokenProcessInput->sString.leng
               , TokenProcessInput->sString.text );
#endif
         return f_TokenDuplicate( TokenProcessInput );
      }
   }
}

static void f_ProcessTokenUnput( Token_t Token )
{
   if( TokenProcessUnput )
   {
      printf( "Fatal error by f_ProcessTokenUnput\n" );
      exit( -1 );
   }

   TokenProcessUnput = Token;
}


