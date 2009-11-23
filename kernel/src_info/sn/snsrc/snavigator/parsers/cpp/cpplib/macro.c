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
#include <stdlib.h>

#include <tcl.h>

#include "srchtbl.h"
#include "cpdefines.h"
#include "cplex.h"
#include "cpkeyw.h"
#include "sn.h"

#define my_isxdigit(x) (isxdigit(x))
#define my_isdigit(x) (isdigit(x))
#define my_isalnum(x) (isalnum(x))

# define input() (yyleng++,(yychar=(*yyptr++)))

# define unput() (yyleng--,yyptr--)

# define d_TokenReturn(x) \
   { \
      Token_t Token = f_TokenCreate(); \
      Token->lex          = x; \
      Token->sString.text = yytext; \
      Token->sString.leng = yyleng; \
      return Token; \
   }

int (*Paf_Word) (char *word,int len,char **parameter_list,char **macro);

static SearchTable *pbtrMacro;
static unsigned char *yyptr;

static Macro_t f_MacroCreate( sString_t sStringName );
static void f_MacroDestroy( Macro_t Macro );
static int f_MacroProcess1( Macro_t Macro );
static int f_MacroProcess2( Macro_t Macro, sString_t sStringName, sString_t *psStringParameter, int iParameter );
static Token_t my_TokenInput( void );
static Token_t f_ParameterPosition( Param_t Param, int position );
static void __TokenPrint( Token_t Token );
static char *Strdup( char *pc );
static void FreeMacroEntry( SearchEntry *entry );

int iMacroCount;

/* Cache for Paf_Word function.
 * This function read and process the macros, and save the processed values in
 * a cache.
 */
extern Macro_t f_MacroFind( sString_t sString )
{
   SearchEntry entry;
   SearchEntry *pentry;

   if( Paf_Word == 0 ) return 0;

   if( pbtrMacro == 0 ) 
   {
      pbtrMacro = SearchTableCreate(1000,SEARCH_HASH_TABLE,FreeMacroEntry);
   }

   entry.key      = sString.text;
   entry.key_len  = sString.leng;
   entry.data     = 0;
   entry.data_len = 0;
   entry.flag     = SEARCH_DUP_KEY;

   if(( pentry = (*pbtrMacro->search)( &pbtrMacro, entry )))
   {
      return (Macro_t) pentry->data;
   }
   else
   {
      int retval;
      char *parameter_list;
      char *macro;

      retval = (*Paf_Word)( sString.text, sString.leng, &parameter_list, &macro );

      if( retval == PAF_WORD_DEFINE ||
          retval == PAF_WORD_REPLACE ) /* 27.11.97 rigo */
      {
         Macro_t Macro = f_MacroCreate( sString );

         Macro->parameter_list = Strdup( parameter_list );
         Macro->macro          = Strdup( macro );
         Macro->paf_typ        = retval; /* 27.11.97 rigo */
         Macro->typ            = parameter_list == 0 ? CPP_DEFINE : CPP_MACRO;

         if( f_MacroProcess1( Macro ) == 0 ) /* OK */
         {
            iMacroCount++;
            entry.data = Macro;
            (*pbtrMacro->add)( &pbtrMacro, entry );
            f_MacroProcess( Macro );
            return Macro;
         }
         else
         {
            f_MacroDestroy( Macro );
            return 0;
         }
      }
      else if( retval == PAF_WORD_UNDEF )
      {
         Macro_t Macro = f_MacroCreate( sString );

         Macro->paf_typ = retval; /* 27.11.97 rigo */
         Macro->typ = CPP_UNDEF;
         iMacroCount++;
         entry.data = Macro;
         (*pbtrMacro->add)( &pbtrMacro, entry );
         return Macro;
      }
      else  /* retval == PAF_WORD_NONE */
      {
         return 0;
      }
   }
}

static Macro_t f_MacroCreate( sString_t sStringName )
{
   Macro_t Macro;

   Macro = (Macro_t) ckalloc( sizeof( Macro[0] ));

   Macro->parameter_list = 0;
   Macro->macro          = 0;
   Macro->paf_typ        = 0; /* 27.11.97 rigo */
   Macro->typ            = 0;
   Macro->tilt           = False;
   Macro->sStringName    = sStringName;
   Macro->Token          = 0;
   Macro->TokenProcessed = 0;
   Macro->MacroNext      = Macro;
   Macro->MacroPrev      = Macro;

   return Macro;
}

static void f_MacroDestroy( Macro_t Macro )
{
   extern int bCheckOfToken2;

   if( Macro )
   {
#ifdef TEST
      printf( "MacroDestroy: %*.*s\n"
            , Macro->sStringName.leng
            , Macro->sStringName.leng
            , Macro->sStringName.text
            );
#endif

      if( Macro->parameter_list ) ckfree( Macro->parameter_list );
      if( Macro->macro )          ckfree( Macro->macro );
      bCheckOfToken2 = True;
      f_TokenFreeAll( Macro->Token          );
      f_TokenFreeAll( Macro->TokenProcessed );
      bCheckOfToken2 = False;
      ckfree ((char*)Macro);    /* 27.11.97 rigo */
   }
}

static int f_MacroProcess1( Macro_t Macro )
{
   Token_t Token;
   sString_t sStringName;
   sString_t asStringParameter[100];
   int iParameter = 0;

   if( Macro->parameter_list )
   {
      yyptr = (unsigned char *) Macro->parameter_list;

      while( True )
      {
         if(( Token = my_TokenInput())->lex == SN_IDENTIFIER )
         {
            asStringParameter[iParameter++] = Token->sString;
            f_TokenFree( Token );
         }
         else if( iParameter == 0 && Token->lex == 0 )
         {
            f_TokenFree( Token );
            return f_MacroProcess2( Macro, sStringName, asStringParameter, iParameter );
         }
         else
         {
            printf( "lex error [3]: %s\n", Macro->parameter_list );
            f_TokenFree( Token );
            return -1;  /* ERROR */
         }

         if(( Token = my_TokenInput())->lex == 0 )
         {
            f_TokenFree( Token );
            return f_MacroProcess2( Macro, sStringName, asStringParameter, iParameter );
         }
         else if( Token->lex != ',' )
         {
            printf( "lex error [4]: %s\n", Macro->parameter_list );
            f_TokenFree( Token );
            return -1;  /* ERROR */
         }
         f_TokenFree( Token );
      }
   }
   else
   { /********************* define *********************/
      return f_MacroProcess2( Macro, sStringName, 0, 0 );
   }
}

static int f_MacroProcess2( Macro_t Macro, sString_t sStringName, sString_t *psStringParameter, int iParameter )
{
   Token_t Token;

   if( Macro->macro == 0 ) return 0;   /* 25.11.97 rigo */

   yyptr = (unsigned char *) Macro->macro;

   while( True )
   {
      Token = my_TokenInput();
      Token->iParameter = -1;

      if( Token->lex == 0 )
      {
         f_TokenFree( Token );
         break;
      }
      if( Token->lex == SN_IDENTIFIER )
      {
         int leng = Token->sString.leng;
         int i;

         for( i = 0; i < iParameter; i++ )
         {
            if( leng == psStringParameter[i].leng &&
                strncmp( (const char *) Token->sString.text
                       , (const char *) psStringParameter[i].text
                       , leng ) == 0 )
            {
               Token->iParameter = i;
            }
         }
      }

      f_TokenAppend( &Macro->Token, Token );
   }

   return 0;   /* OK */
}

static Token_t my_TokenInput( void )
{
   unsigned char *yytext;
   int yyleng;
   int yychar;
   int yycharprev;

/*    if( TokenUnput ) */
/*    { */
/*       Token_t Token = TokenUnput; */
/*       TokenUnput = 0; */
/*       return Token; */
/*    } */

   while( 1 )
   {
      yytext = yyptr;
      yyleng = 0;

      input();

      switch( yychar )
      {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
         if( yychar == '0' && ( input() == 'X' || yychar == 'x' ))
         {   /* hexa nummer */
            while( my_isxdigit( input()))
            {
            }
            if( yychar == 'l' || yychar == 'L' )
            {
               d_TokenReturn( SN_LONGconstant )
            }
            else if( yychar == 'u' || yychar == 'U' )
            {
               d_TokenReturn( SN_INTEGERconstant )
            }
            else
            {
               unput();
               d_TokenReturn( SN_INTEGERconstant )
            }
         }
         else
         {
            int bFloat = False;
            unput();

            while( my_isdigit( input()))
            {
            }
            if( yychar == '.' )
            {
               bFloat = True;
               while( my_isdigit( input()))
               {
               }
            }
            if( yychar == 'E' || yychar == 'e' )
            {
               bFloat = True;
               input();
               if( yychar == '+' || yychar == '-' )
               {
               }
               else
               {
                  unput();
               }
               while( my_isdigit( input()))
               {
               }
               if( yychar == '.' )
               {
                  while( my_isdigit( input()))
                  {
                  }
               }
            }
            if( yychar == 'l' || yychar == 'L' )
            {
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_LONGconstant )
            }
            else if( yychar == 'u' || yychar == 'U' )
            {
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_INTEGERconstant )
            }
            else
            {
               unput();
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_INTEGERconstant )
            }
         }

      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'x':
      case 'y':
      case 'v':
      case 'w':
      case 'z':

      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'X':
      case 'Y':
      case 'V':
      case 'W':
      case 'Z':

      case '_':
      case '$':

         {
            register int ihash = 0;
            register int j = 0;
            int keyword;
            int is_cpp;

            do
            {
               if( j++ < 6 && ihash < HASH_MAX )
               {
                  ihash += c_hash[ (int) yychar ];
               }
            }
            while( my_isalnum( input()) || yychar == '_' || yychar == '$' );

            unput();

            if( ihash < HASH_MAX &&
                LexKeyWordTab[ihash].leng == yyleng &&
                strncmp( LexKeyWordTab[ihash].pcName, (char *)yytext, yyleng ) == 0 )
            {
               keyword = LexKeyWordTab[ihash].wLexId; 
               is_cpp  = LexKeyWordTab[ihash].is_cpp; 
            }
            else
            {
               keyword = 0;
               is_cpp  = 0;
            }
            {
               Token_t Token = f_TokenCreate();
               Token->lex          = SN_IDENTIFIER;
               Token->keyword      = keyword;
               Token->is_cpp       = is_cpp;
               Token->sString.text = yytext;
               Token->sString.leng = yyleng;
               return Token;
            }
         }

      case ' ':
      case '\t':
      case '\r':
      case '\n':
         break;

      case '"':
         while( input())
         {
            if( yychar == '\\' )
            {
/* A \ utani karaktert lenyeljuk.
   Ha \ utan cr+lf jon, akkor midkettot lenyeljuk */
               input();
               if( yychar == '\r' )
               {
                  input();
                  if( yychar != '\n' )
                  {
                     unput();
                  }
               }
            }
            else if( yychar == '"' )
            {
               break;
            }
            else if( yychar == '\n' )
            {
               unput();
               break;
            }
         }
         d_TokenReturn( SN_STRINGliteral )

      case '\'':
         while( input())
         {
            if( yychar == '\\' )
            {
/* A \ utani karaktert lenyeljuk.
   Ha \ utan cr+lf jon, akkor midkettot lenyeljuk */
               input();
               if( yychar == '\r' )
               {
                  input();
                  if( yychar != '\n' )
                  {
                     unput();
                  }
               }
            }
            else if( yychar == '\'' )
            {
               break;
            }
            else if( yychar == '\n' )
            {
               unput();
               break;
            }
         }
         d_TokenReturn( SN_CHARACTERconstant )

      case '#':
         d_TokenReturn( yychar )

      case '<':   /* <, <<, <=, <<= */
         switch( input())
         {
         case '<':
            switch( input())
            {
            case '=':
               d_TokenReturn( SN_LSassign )
            default:
               unput();
               d_TokenReturn( SN_LS )
            }

         case '=':
            d_TokenReturn( SN_LE )
         default:
            unput();
            d_TokenReturn( '<' )
         }

      case '>':   /* >, >>, >=, >>= */
         switch( input())
         {
         case '>':
            switch( input())
            {
            case '=':
               d_TokenReturn( SN_RSassign )
            default:
               unput();
               d_TokenReturn( SN_RS )
            }

         case '=':
            d_TokenReturn( SN_GE )

         default:
            unput();
            d_TokenReturn( '>' )
         }

      case '=':   /* =, == */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_EQ )

         default:
            unput();
            d_TokenReturn( '=' )
         }

      case '!':   /* !, != */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_NE )

         default:
            unput();
            d_TokenReturn( '!' )
         }

      case '+':   /* +, ++, += */
         switch( input())
         {
         case '+':
            d_TokenReturn( SN_ICR )

         case '=':
            d_TokenReturn( SN_PLUSassign )

         default:
            unput();
            d_TokenReturn( '+' )
         }

      case '-':   /* -, --, -=, ->, ->* */
         switch( input())
         {
         case '>':
            switch( input())
            {
            case '*':
               d_TokenReturn( SN_ARROWstar )
            default:
               unput();
               d_TokenReturn( SN_ARROW )
            }
               
         case '-':
            d_TokenReturn( SN_DECR )

         case '=':
            d_TokenReturn( SN_MINUSassign )

         default:
            unput();
            d_TokenReturn( '-' )
         }

      case '*':   /* *, *= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_MULTassign )

         default:
            unput();
            d_TokenReturn( '*' )
         }

      case '/':   /* /, /=, / *, // */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_DIVassign )

         case '*':   /* comment */
            yycharprev = 0;
            while( input())
            {
               if( yychar == '/' && yycharprev == '*' )
               {
                  break;
               }
               yycharprev = yychar;
            }
            break;

         case '/':   /* comment */
            while( input()) 
            {
               if( yychar == '\n' )
               {
                  break;
               }
            }
            break;

         default:
            unput();
            d_TokenReturn( '/' )
         }
         break;

      case '|':   /* |, ||, |= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ORassign )

         case '|':
            d_TokenReturn( SN_OROR )

         default:
            unput();
            d_TokenReturn( '|' )
         }

      case '^':   /* ^, ^= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ERassign )

         default:
            unput();
            d_TokenReturn( '^' )
         }

      case '%':   /* %, %= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_MODassign )

         default:
            unput();
            d_TokenReturn( '%' )
         }

      case '&':   /* &, &&, &= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ANDassign )

         case '&':
            d_TokenReturn( SN_ANDAND )

         default:
            unput();
            d_TokenReturn( '&' )
         }

      case ':':   /* :, :: */
         switch( input())
         {
         case ':':
            d_TokenReturn( SN_CLCL )

         default:
            unput();
            d_TokenReturn( ':' )
         }

      case '.':   /* ., .*, ... */
         switch( input())
         {
         case '*':
            d_TokenReturn( SN_DOTstar )

         case '.':
            switch( input())
            {
            case '.':
               d_TokenReturn( SN_ELLIPSIS )
            default:
               unput();
               break;
            }
            unput();
            d_TokenReturn( '.' )

         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            while( my_isdigit( input()))
            {
            }
            if( yychar == 'E' || yychar == 'e' )
            {
               input();
               if( yychar == '+' || yychar == '-' )
               {
               }
               else
               {
                  unput();
               }
               while( my_isdigit( input()))
               {
               }
               if( yychar == '.' )
               {
                  while( my_isdigit( input()))
                  {
                  }
               }
            }
            if( !( yychar == 'l' || yychar == 'L' ))
            {
               unput();
            }
            d_TokenReturn( SN_FLOATINGconstant )

         default:
            unput();
            d_TokenReturn( '.' )
         }

      case '(':
      case ')':
      case '[':
      case ']':
      case '{':
      case '}':
      case ',':
      case ';':
      case '~':
      case '?':
      case '@':
         d_TokenReturn( yychar )

      case 0:
         yyleng = 0;
         d_TokenReturn( 0 )

      default:
         break;
      }
   }
}

extern Token_t f_MacroMerge( Macro_t Macro, Token_t TokenBasic, Param_t Param, int bProcessed )
{
   Token_t TokenReturn = 0;
   Token_t Token;
   Token_t TokenMacro;

   if( bProcessed ) TokenMacro = Macro->TokenProcessed;
   else             TokenMacro = Macro->Token;

/*    f_MacroPrint( Macro ); */
/*    f_TokenPrint( TokenMacro ); */

   WHILE_TOKEN( TokenMacro )

      if( TokenMacro->iParameter >= 0 && Param )
      {
         Token_t TokenParameter;

         TokenParameter = f_ParameterPosition( Param, TokenMacro->iParameter );

         WHILE_TOKEN( TokenParameter )

            Token = f_TokenDuplicate( TokenParameter );
            f_TokenAppend( &TokenReturn, Token );
   
         END_WHILE
      }
      else
      {
         Token = f_TokenCreate();

         Token->lex        = TokenMacro->lex;
         Token->sString    = TokenMacro->sString;
         Token->keyword    = TokenMacro->keyword;       /* 15.01.97 rigo */
         Token->is_cpp     = TokenMacro->is_cpp;        /* 15.01.97 rigo */
         Token->lineno_beg = TokenBasic->lineno_beg;
         Token->charno_beg = TokenBasic->charno_beg;
         Token->lineno_end = TokenBasic->lineno_end;
         Token->charno_end = TokenBasic->charno_end;

         f_TokenAppend( &TokenReturn, Token );
      }

   END_WHILE

   return TokenReturn;
}

extern void f_MacroTokenPrint( Token_t Token )
{
   WHILE_TOKEN( Token )

#ifdef DETAIL
      printf( "%s [%d %d] %*.*s "
            , Token->TokenFreeNext == 0 ? "    " : "free"
            , Token
            , Token->sString.text
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text
            );
#else
      unsigned char *text = Token->sString.text;
      int            leng = Token->sString.leng;

      printf( "%3d ", Token->lex );

      if( Token->lex == SN_PP_IF )
      {
         printf( "#if %d\n", Token->mode );
      }
      else if( Token->lex == SN_PP_ELIF )
      {
         printf( "#elif %d\n", Token->mode );
      }
      else if( Token->lex == SN_PP_ELSE )
      {
         printf( "#else\n" );
      }
      else if( Token->lex == SN_PP_ENDIF )
      {
         printf( "#endif\n" );
      }
      else if( Token->lex == SN_PP_DEFINE )
      {
         printf( "#define %*.*s\n"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               );
      }
      else if( Token->lex == SN_PP_INCLUDE )
      {
         printf( "#include %*.*s\n"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               );
      }
      else
      {
         while( leng-- )
         {
            putchar( *text++ );
         }
         putchar( '\n' );
      }

#endif

      if( Token->TokenFreeNext )
      {
         printf( "fatal error by f_MacroTokenPrint\n" );
         exit( -1 );
      }

   END_WHILE

   printf( "\n" );
}

static Token_t f_ParameterPosition( Param_t Param, int position )
{
   if( position < Param->iToken )
   {
      return Param->pToken[position];
   }
   else
   {
      return 0;
   }
}

extern void f_MacroAppend( Macro_t *pMacroBeg1, Macro_t MacroBeg2 )
{
   if( *pMacroBeg1 == 0 )
   {
      *pMacroBeg1 = MacroBeg2;
   }
   else
   {
      Macro_t MacroBeg1 = *pMacroBeg1;
      Macro_t MacroEnd1 = MacroBeg1->MacroPrev;
      Macro_t MacroEnd2 = MacroBeg2->MacroPrev;

      MacroBeg1->MacroPrev = MacroEnd2;
      MacroBeg2->MacroPrev = MacroEnd1;
      MacroEnd1->MacroNext = MacroBeg2;
      MacroEnd2->MacroNext = MacroBeg1;
   }
}

extern void f_MacroPrint( Macro_t Macro )
{
   if( Macro->typ == CPP_DEFINE )
   {
      printf( "#define %*.*s %s\n"
            , Macro->sStringName.leng
            , Macro->sStringName.leng
            , Macro->sStringName.text
            , Macro->macro ? Macro->macro : ""
            );
   }
   else if( Macro->typ == CPP_MACRO )
   {
      printf( "#define %*.*s( %s ) %s\n"
            , Macro->sStringName.leng
            , Macro->sStringName.leng
            , Macro->sStringName.text
            , Macro->parameter_list
            , Macro->macro ? Macro->macro : ""
            );
   }
   else /* Macro->typ == CPP_UNDEF */
   {
      printf( "#undef %*.*s\n"
            , Macro->sStringName.leng
            , Macro->sStringName.leng
            , Macro->sStringName.text
            );
   }
   
   __TokenPrint( Macro->Token );
   __TokenPrint( Macro->TokenProcessed );
}

extern void f_MacroFreeAll( void )
{
   iMacroCount = 0;

   if (pbtrMacro)
      (*pbtrMacro->destroy)( &pbtrMacro );
}

static void __TokenPrint( Token_t Token )
{
   WHILE_TOKEN( Token )

      unsigned char *text = Token->sString.text;
      int            leng = Token->sString.leng;

      while( leng-- )
      {
         putchar( *text++ );
      }

      putchar( ' ' );

   END_WHILE

   printf( "\n" );
}

static char *Strdup( char *pc )
{
   if( pc ) return SN_StrDup( pc );
   else     return 0;
}

extern void f_TokenOnePrint( Token_t Token )
{
   unsigned char *text = Token->sString.text;
   int            leng = Token->sString.leng;

   printf( "\tToken: %3d ", Token->lex );

   if( Token->lex == SN_PP_IF )
   {
      printf( "#if %d\n", Token->mode );
   }
   else if( Token->lex == SN_PP_ELIF )
   {
      printf( "#elif %d\n", Token->mode );
   }
   else if( Token->lex == SN_PP_ELSE )
   {
      printf( "#else\n" );
   }
   else if( Token->lex == SN_PP_ENDIF )
   {
      printf( "#endif\n" );
   }
   else if( Token->lex == SN_PP_DEFINE )
   {
      printf( "#define %*.*s\n"
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text
            );
   }
   else if( Token->lex == SN_PP_INCLUDE )
   {
      printf( "#include %*.*s\n"
            , Token->sString.leng
            , Token->sString.leng
            , Token->sString.text
            );
   }
   else
   {
      while( leng-- )
      {
         putchar( *text++ );
      }
      putchar( '\n' );
   }
}

static void FreeMacroEntry( SearchEntry *entry )
{
#if 0	/* Found macros will be removed at a different location.
		 * It means that the not found macros will cause a small
		 * memory leak. */
   Macro_t Macro = entry->data;
   f_MacroDestroy( Macro );
#endif
}

