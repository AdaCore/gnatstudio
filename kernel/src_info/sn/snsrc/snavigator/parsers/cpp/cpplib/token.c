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
#include <stdlib.h>
#include <tcl.h>
#include "cplex.h"

static Token_t TokenFree_m;
static int iTokenFree_m;
static Token_t aTokenFreeMallocArray[100];   /* 18.11.97 rigo */
static int iTokenFreeMallocArray;            /* 18.11.97 rigo */
static Token_t TokenFreeMalloc;
static int iTokenFreeMalloc;
#ifdef TOKEN_ALL_CHECK
static Token_t TokenAll_m;
#endif
static int iTokenAll_m;
static int iTokenFromMalloc_m;

int bCheckOfToken2 = False;

extern Token_t f_TokenCreate( void )
{
   Token_t Token;

   if( TokenFree_m )
   {
      Token = TokenFree_m;
      TokenFree_m = TokenFree_m->TokenFreeNext;
      iTokenFree_m--;
      Token->keyword = 0;
      Token->iParameter = -1;
      Token->is_free    = False;
      Token->pcValue    = 0;
      Token->TokenNext  = Token;
      Token->TokenPrev  = Token;
      Token->TokenFreeNext = 0;
      return Token;
   }
   else
   {
      if( iTokenFreeMalloc == 0 )
      {
         iTokenFreeMalloc = 1000;
         aTokenFreeMallocArray[iTokenFreeMallocArray++] =   /* 18.11.97 rigo */
         TokenFreeMalloc = (Token_t) ckalloc( iTokenFreeMalloc * sizeof( sToken_t ));

         iTokenAll_m  += iTokenFreeMalloc;
         iTokenFree_m += iTokenFreeMalloc;
/*          printf( "TokenAll: %7d\n", iTokenAll_m ); */
      }
      Token = TokenFreeMalloc + --iTokenFreeMalloc;
      iTokenFromMalloc_m++;
#ifdef TOKEN_ALL_CHECK
      Token->TokenAllNext = TokenAll_m;
      TokenAll_m = Token;
#endif
      iTokenFree_m--;
      Token->keyword = 0;
      Token->iParameter = -1;
      Token->is_free    = False;
      Token->pcValue    = 0;
      Token->TokenNext  = Token;
      Token->TokenPrev  = Token;
      Token->TokenFreeNext = 0;
      return Token;
   }
}

extern void f_TokenFree( Token_t Token )
{
   if( Token->is_free )
   {
      fprintf( stderr, "Fatal error: f_TokenFree on a free Token: %*.*s\n"
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text
            );
      *(int*)0 = 0;
   }

   Token->is_free = True;
   if( Token->pcValue )
   {
      ckfree( Token->pcValue );
      Token->pcValue = 0;
   }
   Token->TokenFreeNext = TokenFree_m;
   TokenFree_m = Token;
   iTokenFree_m++;
}

extern void free_token_buffers() /* 18.11.97 rigo */
{
   int i;

   for( i = 0; i < iTokenFreeMallocArray; i++ )
   {
      ckfree ((char*) aTokenFreeMallocArray[i] );
      aTokenFreeMallocArray[i] = 0;
   }
   iTokenFreeMallocArray = 0;
   TokenFreeMalloc = NULL;
   iTokenFreeMalloc = 0;
}

extern void f_TokenFreeAll( Token_t Token )
{
   WHILE_TOKEN( Token )

      if( Token->is_free )
      {
         fprintf( stderr, "Fatal error: f_TokenFree on a free Token: %*.*s\n"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               );
         *(int*)0 = 0;
      }

      Token->is_free = True;
      if( Token->pcValue )
      {
         ckfree( Token->pcValue );
         Token->pcValue = 0;
      }
      Token->TokenFreeNext = TokenFree_m;
      TokenFree_m = Token;
      iTokenFree_m++;

   END_WHILE
}

extern void f_TokenAppend( Token_t *pTokenBeg1, Token_t TokenBeg2 )
{
   if( *pTokenBeg1 == 0 )
   {
      *pTokenBeg1 = TokenBeg2;
   }
   else
   {
      Token_t TokenBeg1 = *pTokenBeg1;
      Token_t TokenEnd1 = TokenBeg1->TokenPrev;
      Token_t TokenEnd2 = TokenBeg2->TokenPrev;

      TokenBeg1->TokenPrev = TokenEnd2;
      TokenBeg2->TokenPrev = TokenEnd1;
      TokenEnd1->TokenNext = TokenBeg2;
      TokenEnd2->TokenNext = TokenBeg1;
   }
}

extern void f_TokenDelete( Token_t Token )
{
   Token_t TokenPrev = Token->TokenPrev;
   Token_t TokenNext = Token->TokenNext;

/*    printf( "*** TokenDelete ***\n" ); */
/*    f_TokenOnePrint( Token ); */
/*    f_TokenOnePrint( TokenPrev ); */
/*    f_TokenOnePrint( TokenNext ); */
   TokenPrev->TokenNext = TokenNext;
   TokenNext->TokenPrev = TokenPrev;
}

extern Token_t f_TokenDuplicate( Token_t Token )
{
   Token_t TokenReturn = f_TokenCreate();

   TokenReturn->lex          = Token->lex;
   TokenReturn->sString      = Token->sString;
   TokenReturn->keyword      = Token->keyword;
   TokenReturn->is_cpp       = Token->is_cpp;
   TokenReturn->iParameter   = Token->iParameter;
   TokenReturn->lineno_beg   = Token->lineno_beg;
   TokenReturn->charno_beg   = Token->charno_beg;
   TokenReturn->lineno_end   = Token->lineno_end;
   TokenReturn->charno_end   = Token->charno_end;

   return TokenReturn;
}

extern void f_TokenCheck( void )
{
#ifdef TOKEN_ALL_CHECK
   Token_t Token;
   int i;
#endif

   printf( "TokenAll       : %d\n", iTokenAll_m        );
   printf( "TokenFree      : %d\n", iTokenFree_m       );
   printf( "TokenFromMalloc: %d\n", iTokenFromMalloc_m );

#ifdef TOKEN_ALL_CHECK
   i = 0;
   for( Token = TokenAll_m; Token; Token = Token->TokenAllNext )
   {
      i++;
      if( ! Token->is_free )
      {
         printf( "Token not free: %*.*s (%d,%d)\n"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               , Token->lineno_beg
               , Token->charno_beg
               );
      }
   }
   printf( "Token in TokenAll chain: %d\n", i );
#endif
}

extern void f_TokenPrint( Token_t Token )
{
   Token_t TokenOrig;
   Token_t TokenLast;

   TokenOrig = Token;

   printf( "*********** TOKEN ***********\n" );

   WHILE_TOKEN( Token )

      printf( "Token: %3d <%*.*s>\n"
            , Token->lex
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text
            );

      TokenLast = Token;

   END_WHILE

   if( TokenLast->TokenNext != TokenOrig )
      printf( "error 1\n" );
   if( TokenOrig->TokenPrev != TokenLast )
      printf( "error 2\n" );
}


