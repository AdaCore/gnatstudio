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
#include "operator.h"
#include "cpdefines.h"
#include "cplex.h"

#define Save()    Token_t TokenActSave = TokenAct
#define Restore() TokenAct = TokenActSave

#define CEXP_DEF   0
#define CEXP_UNDEF 1
#define CEXP_NULL  2


typedef struct sCexp Cexp_t;

struct sCexp
{
   int typ;       /* CEXP_DEF, CEXP_UNDEF, CEXP_NULL */
   int value;
};

static Token_t TokenEnd;
static Token_t TokenAct;
static Cexp_t CexpNull = { CEXP_NULL, 0 };


static Cexp_t constant_expression( void );
static Cexp_t conditional_expression( void );
static Cexp_t logical_or_expression( void );
static Cexp_t logical_and_expression( void );
static Cexp_t inclusive_or_expression( void );
static Cexp_t exclusive_or_expression( void );
static Cexp_t and_expression( void );
static Cexp_t equality_expression( void );
static Cexp_t relational_expression( void );
static Cexp_t shift_expression( void );
static Cexp_t additive_expression( void );
static Cexp_t multiplicative_expression( void );
static Cexp_t unary_expression( void );
static Cexp_t primary_expression( void );
static Cexp_t CexpCreateOp1( int operator, Cexp_t Cexp1 );
static Cexp_t CexpCreateOp2( int operator, Cexp_t Cexp1, Cexp_t Cexp2 );
static Cexp_t CexpCreateOp3( int operator, Cexp_t Cexp1, Cexp_t Cexp2, Cexp_t Cexp3 );
static int token( int i );
static void step( int i );
static sString_t ident( int i );
static long f_Atol( sString_t sString );
static long f_Ctol( sString_t sString );

extern int __ConstantExpression( Token_t Token )
{
   Cexp_t Cexp;

   TokenAct = Token;
   TokenEnd = Token;

   Cexp = constant_expression();

   if( Cexp.typ == CEXP_NULL  ) return CPLEX_TRUE;
   if( Cexp.typ == CEXP_UNDEF ) return CPLEX_UNDEF;

   if( Cexp.value )
   {
      return CPLEX_TRUE;
   }
   else
   {
      return CPLEX_FALSE;
   }
}

static Cexp_t constant_expression( void )
{
   return conditional_expression();
}

static Cexp_t conditional_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   Cexp_t Cexp3;
   Save();

#ifdef TRACE
   printf( "conditional_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = logical_or_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   if( token( 0 ) != '?' )
   {
      return Cexp1;
   }

   step( 1 );

   if(( Cexp2 = constant_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp2;
   }

   if( token( 0 ) != ':' )
   {
      Restore();
      return CexpNull;
   }

   step( 1 );

   if(( Cexp3 = conditional_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp3;
   }
   return CexpCreateOp3( OPERATOR_CONDITIONAL, Cexp1, Cexp2, Cexp3 );
}

static Cexp_t logical_or_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "logical_or_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = logical_and_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_OROR: operator = OPERATOR_OROR; break;
      default  : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = logical_and_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t logical_and_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "logical_and_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = inclusive_or_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_ANDAND: operator = OPERATOR_ANDAND; break;
      default    : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = inclusive_or_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t inclusive_or_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "inclusive_or_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = exclusive_or_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '|': operator = OPERATOR_OR; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = exclusive_or_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}


static Cexp_t exclusive_or_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "exclusive_or_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = and_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '^': operator = OPERATOR_ER; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = and_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t and_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "and_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = equality_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '&': operator = OPERATOR_AND; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = equality_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t equality_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "equality_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = relational_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_EQ: operator = OPERATOR_EQ; break;
      case SN_NE: operator = OPERATOR_NE; break;
      default: return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = relational_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t relational_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "relational_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = shift_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '<': operator = OPERATOR_L ; break;
      case '>': operator = OPERATOR_G ; break;
      case SN_LE : operator = OPERATOR_LE; break;
      case SN_GE : operator = OPERATOR_GE; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = shift_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t shift_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "shift_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = additive_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_LS : operator = OPERATOR_LS; break;
      case SN_RS : operator = OPERATOR_RS; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = additive_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t additive_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "additive_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = multiplicative_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '+': operator = OPERATOR_PLUS ; break;
      case '-': operator = OPERATOR_MINUS; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = multiplicative_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t multiplicative_expression( void )
{
   Cexp_t Cexp1;
   Cexp_t Cexp2;
   int operator;
   Save();

#ifdef TRACE
   printf( "multiplicative_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp1 = unary_expression()).typ == CEXP_NULL )
   {
      Restore();
      return Cexp1;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '*': operator = OPERATOR_MULT; break;
      case '/': operator = OPERATOR_DIV ; break;
      case '%': operator = OPERATOR_MOD ; break;
      default : return Cexp1;
      }

      step( 1 );

      if(( Cexp2 = unary_expression()).typ == CEXP_NULL )
      {
         Restore();
         return Cexp2;
      }
      Cexp1 = CexpCreateOp2( operator, Cexp1, Cexp2 );
   }
}

static Cexp_t unary_expression( void )
{
   Cexp_t Cexp;
   Cexp_t Cexp1;
   int operator;
   Save();

#ifdef TRACE
   printf( "unary_expression: %s\n", ident( 0 ));
#endif

   if(( Cexp = primary_expression()).typ != CEXP_NULL )
   {
      goto ret;
   }

   switch( token( 0 ))
   {
   case '+': operator = OPERATOR_UPLUS ; goto process;
   case '-': operator = OPERATOR_UMINUS; goto process;
   case '!': operator = OPERATOR_NOT   ; goto process;
   case '~': operator = OPERATOR_BNOT  ; goto process;

process:
      step( 1 );
      if(( Cexp1 = unary_expression()).typ != CEXP_NULL )
      {
         Cexp = CexpCreateOp1( operator, Cexp1 );
         goto ret;
      }
      break;

   default:
      break;
   }

   Restore();
   Cexp = CexpNull;
ret:
   return Cexp;
}

static Cexp_t primary_expression( void )
{
   Cexp_t Cexp;
   Save();

#ifdef TRACE
   printf( "primary_expression: %s\n", ident( 0 ));
#endif

   switch( token( 0 ))
   {
   case SN_INTEGERconstant:
      Cexp.typ = CEXP_DEF;
      Cexp.value = f_Atol( ident( 0 ));
      step( 1 );
      goto ret;

   case SN_LONGconstant:
      Cexp.typ = CEXP_DEF;
      Cexp.value = f_Atol( ident( 0 ));
      step( 1 );
      goto ret;

   case SN_CHARACTERconstant:
      Cexp.typ = CEXP_DEF;
      Cexp.value = f_Ctol( ident( 0 ));
      step( 1 );
      goto ret;

   case SN_FLOATINGconstant:
      Cexp.typ = CEXP_UNDEF;
      step( 1 );
      goto ret;

   case SN_STRINGliteral:
      Cexp.typ = CEXP_UNDEF;
      step( 1 );
      while( token( 0 ) == SN_STRINGliteral )
      {
         step( 1 );
      }
      goto ret;

   case '(':
      step( 1 );
      if(( Cexp = constant_expression()).typ == CEXP_NULL )
      {
         Restore();
         goto ret;
      }
      if( token( 0 ) == ')' )
      {
         step( 1 );
      }
      goto ret;

   case SN_IDENTIFIER:
      if( f_StringCompare( ident( 0 ), "defined" ) == 0 )
      {
         printf( "error: invalid use of \"defined\" operator\n" );
         step( 1 );
      }
      else
      {
         Cexp.typ = CEXP_UNDEF;
         step( 1 );
      }
      goto ret;

   case SN_UNDEF:
      Cexp.typ = CEXP_UNDEF;
      step( 1 );
      goto ret;

   case SN_TRUE:
      Cexp.typ = CEXP_DEF;
      Cexp.value = True;
      step( 1 );
      goto ret;

   case SN_FALSE:
      Cexp.typ = CEXP_DEF;
      Cexp.value = False;
      step( 1 );
      goto ret;

   default:
/*       sString = ident(0); */
/*       printf( "unexpected <%*.*s> line: %d\n", sString.leng, sString.leng, sString.text, TokenAct->lineno_beg ); */
      Restore();
      Cexp = CexpNull;
      goto ret;
   }
ret:
   return Cexp;
}

static Cexp_t CexpCreateOp1( int operator, Cexp_t Cexp1 )
{
   Cexp_t Cexp;

   if( Cexp1.typ == CEXP_UNDEF )
   {
      Cexp.typ = CEXP_UNDEF;
   }
   else
   {
      Cexp.typ = CEXP_DEF;
      switch( operator )
      {
      case OPERATOR_UPLUS : Cexp.value = + Cexp1.value; break;
      case OPERATOR_UMINUS: Cexp.value = - Cexp1.value; break;
      case OPERATOR_NOT   : Cexp.value = ! Cexp1.value; break;
      case OPERATOR_BNOT  : Cexp.value = ~ Cexp1.value; break;
      }
   }

   return Cexp;
}

static Cexp_t CexpCreateOp2( int operator, Cexp_t Cexp1, Cexp_t Cexp2 )
{
   Cexp_t Cexp;

   if( Cexp1.typ == CEXP_UNDEF || Cexp2.typ == CEXP_UNDEF )
   {
      if( operator == OPERATOR_OROR )
      {
         if( Cexp1.typ == CEXP_UNDEF && Cexp2.typ == CEXP_UNDEF )
         {
            Cexp.typ = CEXP_UNDEF;
         }
         else if( Cexp1.typ == CEXP_UNDEF )
         {
            Cexp.typ = CEXP_DEF;
            if( Cexp2.value ) Cexp.value = 1;
            else              Cexp.value = 0;
         }
         else /* Cexp2.typ == CEXP_UNDEF */
         {
            Cexp.typ = CEXP_DEF;
            if( Cexp1.value ) Cexp.value = 1;
            else              Cexp.value = 0;
         }
      }
      Cexp.typ = CEXP_UNDEF;
   }
   else
   {
      Cexp.typ = CEXP_DEF;

      switch( operator )
      {
         case OPERATOR_OROR  : Cexp.value = Cexp1.value || Cexp2.value; break;
         case OPERATOR_ANDAND: Cexp.value = Cexp1.value && Cexp2.value; break;
         case OPERATOR_OR    : Cexp.value = Cexp1.value |  Cexp2.value; break;
         case OPERATOR_ER    : Cexp.value = Cexp1.value ^  Cexp2.value; break;
         case OPERATOR_AND   : Cexp.value = Cexp1.value &  Cexp2.value; break;
         case OPERATOR_EQ    : Cexp.value = Cexp1.value == Cexp2.value; break;
         case OPERATOR_NE    : Cexp.value = Cexp1.value != Cexp2.value; break;
         case OPERATOR_L     : Cexp.value = Cexp1.value <  Cexp2.value; break;
         case OPERATOR_G     : Cexp.value = Cexp1.value >  Cexp2.value; break;
         case OPERATOR_LE    : Cexp.value = Cexp1.value <= Cexp2.value; break;
         case OPERATOR_GE    : Cexp.value = Cexp1.value >= Cexp2.value; break;
         case OPERATOR_LS    : Cexp.value = Cexp1.value << Cexp2.value; break;
         case OPERATOR_RS    : Cexp.value = Cexp1.value >> Cexp2.value; break;
         case OPERATOR_PLUS  : Cexp.value = Cexp1.value +  Cexp2.value; break;
         case OPERATOR_MINUS : Cexp.value = Cexp1.value -  Cexp2.value; break;
         case OPERATOR_MULT  : Cexp.value = Cexp1.value *  Cexp2.value; break;
         case OPERATOR_DIV   : Cexp.value = Cexp1.value /  Cexp2.value; break;
         case OPERATOR_MOD   : Cexp.value = Cexp1.value %  Cexp2.value; break;
      }
   }

   return Cexp;
}

static Cexp_t CexpCreateOp3( int operator, Cexp_t Cexp1, Cexp_t Cexp2, Cexp_t Cexp3 )
{
   Cexp_t Cexp;

   if( Cexp1.typ == CEXP_DEF )
   {
      Cexp = Cexp1.value ? Cexp2 : Cexp3;
   }
   else
   {
      Cexp.typ = CEXP_UNDEF;
   }

   return Cexp;
}

static int token( int i )
{
   return TokenAct->lex;
}

static void step( int i )
{
   if( TokenAct->TokenNext != TokenEnd )
   {
      TokenAct = TokenAct->TokenNext;
   }
}

static sString_t ident( int i )
{
   return TokenAct->sString;
}

#define OCTAL   1
#define DECIMAL 2
#define HEXA    3

static long f_Atol( sString_t sString )
{
   int i;
   unsigned char *pc;
   int mode = DECIMAL;
   long value = 0;
   long digit;

   for( i = 0, pc = sString.text; i < sString.leng; i++, pc++ )
   {
      if( i == 0 && *pc == '0' )
      {
         mode = OCTAL;
         continue;
      }

      if( i == 1 && mode == OCTAL && ( *pc == 'x' || *pc == 'X' ))
      {
         mode = HEXA;
         continue;
      }

      switch( mode )
      {
      case OCTAL:
         if( *pc < '0' || *pc > '7' )
         {
            return value;
         }
         value = value * 8 + *pc - '0';
         break;

      case DECIMAL:
         if( *pc < '0' || *pc > '9' )
         {
            return value;
         }
         value = value * 10 + *pc - '0';
         break;

      case HEXA:
         if( strchr( "0123456789abcdefABCDEF", *pc ) == 0 )
         {
            return value;
         }
         /**/ if( *pc >= '0' && *pc <= '9' ) digit = *pc - '0';
         else if( *pc >= 'A' && *pc <= 'F' ) digit = *pc - 'A' + 10;
         else if( *pc >= 'a' && *pc <= 'f' ) digit = *pc - 'a' + 10;

         value = value * 16 + digit;
         break;
      }
   }

   return value;
}

static long f_Ctol( sString_t sString )
{
   int leng;
   unsigned char *text;

   text = sString.text;
   leng = sString.leng;
   
   if( *text == '\'' ) { text++; leng--; }
   if( leng == 0 ) return (long) 0;
   if( text[leng-1] == '\'' ) { leng--; }
   if( leng == 0 ) return (long) 0;
   if( *text == '\\' )
   {
      text++;
      leng--;
      if( leng == 0 ) return (long) 0;

      switch( *text )
      {
      case 'n': return (long) '\n';
      case 't': return (long) '\t';
      case 'r': return (long) '\r';
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
         {
            long value = 0;
            for( ; leng > 0 && *text >= '0' && *text <= '7'; text++, leng-- )
            {
               value = value * 8 + ( *text - '0' );
            }
            return value;
         }
      default:
         return (long) *text;
      }
   }
   else
   {
      return (long) *text;
   }
}


