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

#include <tcl.h>

#include "crossrefP.h"
#include "operator.h"

/* #define TRACE */

static Expr_t assignment_expression( void );
static Expr_t constant_expression( void );
static Expr_t conditional_expression( void );
static Expr_t logical_or_expression( void );
static Expr_t logical_and_expression( void );
static Expr_t inclusive_or_expression( void );
static Expr_t exclusive_or_expression( void );
static Expr_t and_expression( void );
static Expr_t equality_expression( void );
static Expr_t relational_expression( void );
static Expr_t shift_expression( void );
static Expr_t additive_expression( void );
static Expr_t multiplicative_expression( void );
static Expr_t pm_expression( void );
static Expr_t cast_expression( void );
static Expr_t unary_expression( void );
static Expr_t postfix_expression( void );
static Expr_t primary_expression( void );
static Expr_t allocation_expression( void );
static List_t placement_opt( void );
static Expr_t deallocation_expression( void );
static Expr_t expression_member_name( void );
static List_t expression_list( void );
static List_t expression_list_opt( void );
/* static void skip_function_arg( void ); */
static Expr_t ExprCreateOp1( int operator, Expr_t Expr1 );
static Expr_t ExprCreateOp2( int operator, Expr_t Expr1, Expr_t Expr2 );
static Expr_t ExprCreateOp2List( int operator, Expr_t Expr1, List_t ListExpr );
static Expr_t ExprCreateOp3( int operator, Expr_t Expr1, Expr_t Expr2, Expr_t Expr3 );
static Expr_t ExprCreateName( Name_t Name );
static Expr_t ExprCreateMemberName( Name_t Name );
static Expr_t ExprCreateType( Type_t Type );
static Expr_t ExprCreateNew( Expr_t Expr1, Init_t Init, List_t ListExpr );
static Boolean_t f_IsNameSimpleTypeName( Name_t Name );

extern Expr_t f_Expression( void )
{
   return expression();
}

extern Expr_t f_ConstantExpression( void )
{
   return constant_expression();
}

extern Boolean_t f_expression( void )
{
   Expr_t Expr;

   if(( Expr = expression()))
   {
      f_ExprProcess( Expr );
      f_ExprDestroy( Expr );
      return True;
   }
   else
   {
      return False;
   }
}

extern Boolean_t f_constant_expression( void )
{
   Expr_t Expr;

   if(( Expr = constant_expression()))
   {
      f_ExprProcess( Expr );
      f_ExprDestroy( Expr );
      return True;
   }
   else
   {
      return False;
   }
}

extern Boolean_t f_assignment_expression( void )
{
   Expr_t Expr;

   if(( Expr = assignment_expression()))
   {
      f_ExprProcess( Expr );
      f_ExprDestroy( Expr );
      return True;
   }
   else
   {
      return False;
   }
}

extern Expr_t f_AssignmentExpression( void )
{
   return assignment_expression();
}

extern List_t f_ExpressionList( void )
{
   return expression_list();
}

extern Expr_t expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = assignment_expression()) == 0 )
   {
      Restore();
#ifdef PRINT
      printf( "No expression: %s\n", ident( 0 ));
#endif
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case ',': operator = OPERATOR_KOMMA; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = assignment_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
#ifdef PRINT
         printf( "No expression: %s\n", ident( 0 ));
#endif
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t assignment_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "assignment_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = conditional_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '='           : operator = OPERATOR_ASSIGN     ; break;
      case SN_MULTassign : operator = OPERATOR_MULTassign ; break;
      case SN_DIVassign  : operator = OPERATOR_DIVassign  ; break;
      case SN_MODassign  : operator = OPERATOR_MODassign  ; break;
      case SN_PLUSassign : operator = OPERATOR_PLUSassign ; break;
      case SN_MINUSassign: operator = OPERATOR_MINUSassign; break;
      case SN_LSassign   : operator = OPERATOR_LSassign   ; break;
      case SN_RSassign   : operator = OPERATOR_RSassign   ; break;
      case SN_ANDassign  : operator = OPERATOR_ANDassign  ; break;
      case SN_ERassign   : operator = OPERATOR_ERassign   ; break;
      case SN_ORassign   : operator = OPERATOR_ORassign   ; break;
      default            : return Expr1;
      }

      step( 1 );

      if(( Expr2 = conditional_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t constant_expression( void )
{
   return conditional_expression();
}

static Expr_t conditional_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   Expr_t Expr3;
   Save();

#ifdef TRACE
   printf( "conditional_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = logical_or_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   if( token( 0 ) != '?' )
   {
      return Expr1;
   }

   step( 1 );

   if(( Expr2 = expression()) == 0 )
   {
      f_ExprDestroy( Expr1 );
      Restore();
      return 0;
   }

   if( token( 0 ) != ':' )
   {
      f_ExprDestroy( Expr1 );
      f_ExprDestroy( Expr2 );
      Restore();
      return 0;
   }

   step( 1 );

   if(( Expr3 = conditional_expression()) == 0 )
   {
      f_ExprDestroy( Expr1 );
      f_ExprDestroy( Expr2 );
      Restore();
      return 0;
   }
   return ExprCreateOp3( OPERATOR_CONDITIONAL, Expr1, Expr2, Expr3 );
}

static Expr_t logical_or_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "logical_or_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = logical_and_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_OROR: operator = OPERATOR_OROR; break;
      default  : return Expr1;
      }

      step( 1 );

      if(( Expr2 = logical_and_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t logical_and_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "logical_and_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = inclusive_or_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_ANDAND: operator = OPERATOR_ANDAND; break;
      default    : return Expr1;
      }

      step( 1 );

      if(( Expr2 = inclusive_or_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t inclusive_or_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "inclusive_or_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = exclusive_or_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '|': operator = OPERATOR_OR; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = exclusive_or_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}


static Expr_t exclusive_or_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "exclusive_or_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = and_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '^': operator = OPERATOR_ER; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = and_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t and_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "and_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = equality_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '&': operator = OPERATOR_AND; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = equality_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t equality_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "equality_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = relational_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_EQ: operator = OPERATOR_EQ; break;
      case SN_NE: operator = OPERATOR_NE; break;
      default: return Expr1;
      }

      step( 1 );

      if(( Expr2 = relational_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t relational_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "relational_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = shift_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '<': operator = OPERATOR_L ; break;
      case '>':
         if( template_arg )
         {
            return Expr1;
         }
         operator = OPERATOR_G ;
         break;
      case SN_LE : operator = OPERATOR_LE; break;
      case SN_GE : operator = OPERATOR_GE; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = shift_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t shift_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "shift_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = additive_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_LS : operator = OPERATOR_LS; break;
      case SN_RS : operator = OPERATOR_RS; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = additive_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t additive_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "additive_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = multiplicative_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '+': operator = OPERATOR_PLUS ; break;
      case '-': operator = OPERATOR_MINUS; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = multiplicative_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t multiplicative_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "multiplicative_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = pm_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '*': operator = OPERATOR_MULT; break;
      case '/': operator = OPERATOR_DIV ; break;
      case '%': operator = OPERATOR_MOD ; break;
      default : return Expr1;
      }

      step( 1 );

      if(( Expr2 = pm_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

static Expr_t pm_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   int operator;
   Save();

#ifdef TRACE
   printf( "pm_expression: %s\n", ident( 0 ));
#endif

   if(( Expr1 = cast_expression()) == 0 )
   {
      Restore();
      return 0;
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_DOTstar  : operator = OPERATOR_DOTstar  ; break;
      case SN_ARROWstar: operator = OPERATOR_ARROWstar; break;
      default       : return Expr1;
      }

      step( 1 );

      if(( Expr2 = cast_expression()) == 0 )
      {
         f_ExprDestroy( Expr1 );
         Restore();
         return 0;
      }
      Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
   }
}

#define OLD_VERSION_OF_CAST
#ifdef OLD_VERSION_OF_CAST

static Expr_t cast_expression( void )
{
   Expr_t Expr;
   Expr_t Expr1;
   Expr_t Expr2;
   Type_t Type;
   Save();

#ifdef TRACE
   printf( "cast_expression: %s\n", ident( 0 ));
#endif

   if( token( 0 ) == '(' )
   {
      int lineno_beg;
      int charno_beg;
      int lineno_end;
      int charno_end;
      Save();

      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      step( 1 );
      if(( Type = f_TypeName( ")" )))
      {
         if( token( 0 ) == ')' )
         {
            lineno_end = f_lineno( 0 );
            charno_end = f_charno( 0 ) + identlen( 0 );
            step( 1 );
            if(( Expr2 = cast_expression()))
            {
               Expr1 = ExprCreateType( Type );
               Expr1->lineno_beg = lineno_beg;
               Expr1->charno_beg = charno_beg;
               Expr1->lineno_end = lineno_end;
               Expr1->charno_end = lineno_end;
               return ExprCreateOp2( OPERATOR_CAST, Expr1, Expr2 );
            }
         }
         f_TypeDestroy( Type );
      }
      Restore();
   }

   if(( Expr = unary_expression()))
   {
      return Expr;
   }

   Restore();
   return 0;
}

#else

/* elobb megnezzuk, hogy lehet-e ertelmezni a kifejezest, cast nelkul,
** es ha nem, akkor nezzuk meg, hogy hatha cast-tal kezdodik
*/

static Expr_t cast_expression( void )
{
   Expr_t Expr;
   Expr_t Expr1;
   Expr_t Expr2;
   Type_t TypeCast;
   Expr_t ExprCast;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   Save();

#ifdef TRACE
   printf( "cast_expression: %s\n", ident( 0 ));
#endif

   if( token( 0 ) == '(' )
   {
      int iva_expr;
      int iva_type;
      int token_after_par;
      Save();

      step( 1 );
      if(( ExprCast = expression()))
      {
         if( token( 0 ) == ')' )
         {
            step( 1 );
            iva_expr = iva;
            token_after_par = token( 0 );
         }
         else
         {
            f_ExprDestroy( ExprCast );
            ExprCast = 0;
         }
      }
      Restore();

      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      step( 1 );
      if(( TypeCast = f_TypeName( ")" )))
      {
         if( token( 0 ) == ')' )
         {
            lineno_end = f_lineno( 0 );
            charno_end = f_charno( 0 ) + identlen( 0 );
            step( 1 );
            iva_type = iva;
         }
         else
         {
            f_TypeDestroy( TypeCast );
            TypeCast = 0;
         }
      }

      Restore();

      if( ExprCast && TypeCast )
      {
         if( iva_type != iva_expr )
         {
            f_InternalError( 72 );
         }
         switch( token_after_par )
         {
         case '+':
         case '-':
         /* inkabb az ExprCast-ot vesszuk */
            f_ExprDestroy( ExprCast );
            f_TypeDestroy( TypeCast );
            goto unary_expression;
         default:
         /* inkabb a  TypeCast-ot vesszuk */
            f_ExprDestroy( ExprCast );
            goto cast_expression;
         }
      }
      else if( ExprCast )  /* TypeCast == 0 */
      {
         f_ExprDestroy( ExprCast );
         goto unary_expression;
      }
      else if( TypeCast )  /* ExprCast == 0 */
      {
cast_expression:
         iva = iva_type;
         if(( Expr2 = cast_expression()))
         {
            Expr1 = ExprCreateType( TypeCast );
            Expr1->lineno_beg = lineno_beg;
            Expr1->charno_beg = charno_beg;
            Expr1->lineno_end = lineno_end;
            Expr1->charno_end = charno_end;
            return ExprCreateOp2( OPERATOR_CAST, Expr1, Expr2 );
         }
      }
      else  /* ExprCast == 0 && TypeCast == 0 */
      {
         return 0;
      }
   }

unary_expression:

   if(( Expr = unary_expression()))
   {
      return Expr;
   }

   Restore();
   return 0;
}
#endif   /* OLD_VERSION_OF_CAST */

static Expr_t unary_expression( void )
{
   Expr_t Expr;
   Expr_t Expr1;
   Type_t Type;
   int operator;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   Save();

#ifdef TRACE
   printf( "unary_expression: %s\n", ident( 0 ));
#endif

   if(( Expr = allocation_expression()))
   {
      goto ret;
   }

   if(( Expr = deallocation_expression()))
   {
      goto ret;
   }

   if( token( 0 ) != '~' )    /* 24.02.97 rigo : ~WS_BORDER miatt */
   {
      if(( Expr = postfix_expression()))
      /* ezt csak az allocation es deallocation kifejezesek utan ellenorizzuk,
         hogy a new nevu valtozok is atmenjenek
         ( 24.02.97: erre nem lesz szukseg, ha a pass1 pontosan felmeri,
         hogy c vagy c++ projektrol van-e szo */
      {
         goto ret;
      }
   }

   switch( token( 0 ))
   {
   case SN_SIZEOF:
      step( 1 );

      if( token( 0 ) == '(' )
      {
         Save();
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         step( 1 );

         if(( Type = f_TypeName( ")" )))
         {
            if( token( 0 ) == ')' )
            {
               lineno_end = f_lineno( 0 );
               charno_end = f_charno( 0 ) + identlen( 0 );

               step( 1 );
               Expr1 = ExprCreateType( Type );
               Expr1->lineno_beg = lineno_beg;
               Expr1->charno_beg = charno_beg;
               Expr1->lineno_end = lineno_end;
               Expr1->charno_end = charno_end;
               Expr = ExprCreateOp1( OPERATOR_SIZEOF_TYPENAME, Expr1 );
               goto ret;
            }
            else
            {
               f_TypeDestroy( Type );
            }
         }
         Restore();
      }

      if(( Expr1 = unary_expression()))
      {
         Expr = ExprCreateOp1( OPERATOR_SIZEOF_EXPRESSION, Expr1 );
         goto ret;
      }
      break;

   case SN_ICR:
      step( 1 );
      if(( Expr1 = unary_expression()))
      {
         Expr = ExprCreateOp1( OPERATOR_PRE_ICR, Expr1 );
         goto ret;
      }
      break;

   case SN_DECR:
      step( 1 );
      if(( Expr1 = unary_expression()))
      {
         Expr = ExprCreateOp1( OPERATOR_PRE_DECR, Expr1 );
         goto ret;
      }
      break;

   case '*': operator = OPERATOR_STAR  ; goto process;
   case '&': operator = OPERATOR_ET    ; goto process;
   case '+': operator = OPERATOR_UPLUS ; goto process;
   case '-': operator = OPERATOR_UMINUS; goto process;
   case '!': operator = OPERATOR_NOT   ; goto process;
   case '~': operator = OPERATOR_BNOT  ; goto process;

process:
      step( 1 );
      if(( Expr1 = cast_expression()))
      {
         Expr = ExprCreateOp1( operator, Expr1 );
         goto ret;
      }
      break;

   default:
      break;
   }

   Restore();
   Expr = 0;
ret:
#ifdef TRACE
   printf( "unary_expression: return %d\n", Expr != 0 );
#endif
   return Expr;
}

static Expr_t postfix_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr2;
   List_t ListExpr;
   Name_t Name;
   Type_t Type;
   int operator;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   Save();

#ifdef TRACE
   printf( "postfix_expression: %s\n", ident( 0 ));
#endif

   Expr1 = 0;
   Expr2 = 0;

   switch( token( 0 ))
   {
   case SN_CHAR      : Type = f_TypeCreateChar    (); goto label;
   case SN_SHORT     : Type = f_TypeCreateShort   (); goto label;
   case SN_INT       : Type = f_TypeCreateInt     (); goto label;
   case SN_LONG      : Type = f_TypeCreateLong    (); goto label;
   case SN_SIGNED    : Type = f_TypeCreateSigned  (); goto label;
   case SN_UNSIGNED  : Type = f_TypeCreateUnsigned(); goto label;
   case SN_FLOAT     : Type = f_TypeCreateFloat   (); goto label;
   case SN_DOUBLE    : Type = f_TypeCreateDouble  (); goto label;
   case SN_BOOL      : Type = f_TypeCreateBool    (); goto label;
   case SN_VOID      : Type = f_TypeCreateVoid    (); goto label;
label:
      step( 1 );
      if( token( 0 ) == '(' )
      {
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         step( 1 );

         ListExpr = expression_list_opt();

         if( token( 0 ) == ')' )
         {
            lineno_end = f_lineno( 0 );
            charno_end = f_charno( 0 ) + identlen( 0 );
            step( 1 );
            Expr1 = ExprCreateType( Type );
            Expr1->lineno_beg = lineno_beg;
            Expr1->charno_beg = charno_beg;
            Expr1->lineno_end = lineno_end;
            Expr1->charno_end = charno_end;
            Expr1 = ExprCreateOp2List( OPERATOR_CONSTRUCTOR_CALL, Expr1, ListExpr );
         }
         else
         {
            f_TypeDestroy( Type );
            f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy );
            Expr1 = 0;
            ListExpr = 0;
            Restore();
         }
      }
      else
      {
         f_TypeDestroy( Type );
         Restore();
      }
      break;

   case SN_IDENTIFIER:
   case SN_CLCL:
      if(( Name = f_CompleteClassName()))
      {
         if( token( 0 ) == '(' )
         {
            lineno_beg = f_lineno( 0 );
            charno_beg = f_charno( 0 );

            step( 1 );

            if( f_IsNameSimpleTypeName( Name ))
            {
               ListExpr = expression_list_opt();

               if( token( 0 ) == ')' )
               {
                  lineno_end = f_lineno( 0 );
                  charno_end = f_charno( 0 ) + identlen( 0 );
                  step( 1 );
                  Type = f_TypeCreateName( Name );
                  Expr1 = ExprCreateType( Type );
                  Expr1->lineno_beg = lineno_beg;
                  Expr1->charno_beg = charno_beg;
                  Expr1->lineno_end = lineno_end;
                  Expr1->charno_end = charno_end;
                  Expr1 = ExprCreateOp2List( OPERATOR_CONSTRUCTOR_CALL, Expr1, ListExpr );
               }
               else
               {
                  f_NameDestroy( Name );
                  f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy );
                  Expr1 = 0;
                  ListExpr = 0;
                  Restore();
               }
            }
            else
            {
               f_NameDestroy( Name );
               Restore();
            }
         }
         else
         {
            f_NameDestroy( Name );
            Restore();
         }
      }
      else
      {
         Restore();
      }
      break;
   }

   if( Expr1 == 0 )
   {
      if(( Expr1 = primary_expression()) == 0 )
      {
         Restore();
         return 0;
      }
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '[' :
         operator = OPERATOR_ARRAY;
         step( 1 );

         if(( Expr2 = expression()) == 0 )
         {
            f_ExprDestroy( Expr1 );
            Restore();
            return 0;
         }
         if( token( 0 ) == ']' )
         {
            step( 1 );
         }
         Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
         break;

      case '(' :
         operator = OPERATOR_FUNCTION_CALL;
         step( 1 );

         ListExpr = expression_list_opt();

         f_StepTo( ')', ';', '{', '}', 0 );

         if( token( 0 ) == ')' )
         {
            step( 1 );
         }
         else
         {
/*          f_InternalError( 29 ); */
/* ez nem komoly, mert minden hibas c programban elojohet */
#ifdef CORVEX
            printf( "expression_list is not terminated width ')': %s:%d.%d\n"
                  , filename_g
                  , f_lineno( 0 )
                  , f_charno( 0 )
                  );
#endif
            f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy );
            f_ExprDestroy( Expr1 );
            Restore();
            return 0;
         }

         Expr1 = ExprCreateOp2List( operator, Expr1, ListExpr );
         break;

      case '.' :
         operator = OPERATOR_DOT;
         step( 1 );

         if(( Expr2 = expression_member_name()) == 0 )
         {
            f_ExprDestroy( Expr1 );
            Restore();
            return 0;
         }
         Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
         break;

      case SN_ARROW :
         operator = OPERATOR_ARROW;
         step( 1 );

         if(( Expr2 = expression_member_name()) == 0 )
         {
            f_ExprDestroy( Expr1 );
            Restore();
            return 0;
         }
         Expr1 = ExprCreateOp2( operator, Expr1, Expr2 );
         break;

      case SN_ICR:
         operator = OPERATOR_POST_ICR;
         step( 1 );
         Expr1 = ExprCreateOp1( operator, Expr1 );
         break;

      case SN_DECR:
         operator = OPERATOR_POST_DECR;
         step( 1 );
         Expr1 = ExprCreateOp1( operator, Expr1 );
         break;

      default  :
         return Expr1;
      }
   }
}

static Expr_t primary_expression( void )
{
   Expr_t Expr;
   Type_t Type;
   Name_t Name;
   int lineno_beg;
   int charno_beg;
   Save();

#ifdef TRACE
   printf( "primary_expression: %s\n", ident( 0 ));
#endif

   switch( token( 0 ))
   {
/* a c-ben elofordulhatnak mint valtozok */
   case SN_ASM      :
   case SN_CATCH    :
   case SN_CLASS    :
   case SN_DELETE   :
   case SN_FRIEND   :
   case SN_INLINE   :
   case SN_NEW      :
   case SN_OPERATOR :
   case SN_OVERLOAD :
   case SN_PRIVATE  :
   case SN_PROTECTED:
   case SN_PUBLIC   :
   case SN_TEMPLATE :
/* case SN_THIS     : */
   case SN_THROW    :
   case SN_TRY      :
   case SN_VIRTUAL  :
   case SN_VOLATILE :
      Expr = ExprCreateName( f_NameCreate( ident( 0 )));
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_INTEGERconstant:
      Type = f_TypeCreateInt();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_LONGconstant:
      Type = f_TypeCreateLong();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_CHARACTERconstant:
      Type = f_TypeCreateChar();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_FLOATINGconstant:
      Type = f_TypeCreateFloat();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_STRINGliteral:
      Type = f_TypeCreateString();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      step( 1 );
      while( token( 0 ) == SN_STRINGliteral )
      {
         step( 1 );
      }
      Expr->lineno_end = f_lineno( -1 );
      Expr->charno_end = f_charno( -1 ) + identlen( -1 );
      goto ret;

   case SN_THIS:
      Type = f_TypeCreate();
      Expr = ExprCreateType( Type );
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      goto ret;

   case SN_CLCL:
      step( 1 );
      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      if(( Name = f_Name()))
      {
         char *pcName = Malloc( strlen( Name->pcName ) + 2 + 1 );
         strcpy( pcName, "::" );
         strcat( pcName, Name->pcName );
         ckfree( (char*)Name->pcName );
         Name->pcName = pcName;
         Expr = ExprCreateName( Name );
         Expr->lineno_beg = lineno_beg;
         Expr->charno_beg = charno_beg;
         Expr->lineno_end = f_lineno( -1 );
         Expr->charno_end = f_charno( -1 ) + identlen( -1 );
         goto ret;
      }
      else
      {
         Restore();
         Expr = 0;
         goto ret;
      }

   case '(':
      step( 1 );
      if(( Expr = expression()) == 0 )
      {
         Restore();
         goto ret;
      }
      if( token( 0 ) == ')' )
      {
         step( 1 );
         goto ret;
      }
      else  /* 03.02.97 rigo */
      {
         Restore();
         f_ExprDestroy( Expr );
         Expr = 0;
         goto ret;
      }

   default: /* name */
/*    printf( "vor f_Name: %s\n", ident( 0 )); */
      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      if(( Name = f_Name()))
      {
/*          printf( "after f_Name: OK: %s\n", ident( 0 )); */
         Expr = ExprCreateName( Name );
         Expr->lineno_beg = lineno_beg;
         Expr->charno_beg = charno_beg;
         Expr->lineno_end = f_lineno( -1 );
         Expr->charno_end = f_charno( -1 ) + identlen( -1 );
         goto ret;
      }
      else
      {
/*          printf( "after f_Name: false: %s\n", ident( 0 )); */
         Restore();
         Expr = 0;
         goto ret;
      }
   }
ret:
#ifdef TRACE
   printf( "primary_expression: return: %d\n", Expr != 0 );
#endif
   return Expr;
}

static Expr_t allocation_expression( void )
{
   Expr_t Expr1;
   Expr_t Expr;
   List_t ListExpr;
   Type_t Type;
   Init_t Init;
   int lineno_beg;
   int charno_beg;

   Save();

#ifdef TRACE
   printf( "allocation_expression: %s\n", ident( 0 ));
#endif

   if( token( 0 ) == SN_CLCL )
   {
      step( 1 );
   }

   if( token( 0 ) == SN_NEW )
   {
      step( 1 );
      ListExpr = placement_opt();

      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      if( token( 0 ) == '(' )
      {
         step( 1 );
         if(( Type = f_TypeName( ")" )))
         {
            Expr1 = ExprCreateType( Type );
            Expr1->lineno_beg = lineno_beg;
            Expr1->charno_beg = charno_beg;
            Expr1->lineno_end = f_lineno( 0 );
            Expr1->charno_end = f_charno( 0 ) + identlen( 0 );
            if( token( 0 ) == ')' )
            {
               step( 1 );
            }
            Init = f_NewInitializer();
            Expr = ExprCreateNew( Expr1, Init, ListExpr );
            return Expr;
         }
      } 
      else
      {
         if(( Type = f_NewTypeName()))
         {
            Expr1 = ExprCreateType( Type );
            Expr1->lineno_beg = lineno_beg;
            Expr1->charno_beg = charno_beg;
            Expr1->lineno_end = f_lineno( -1 );
            Expr1->charno_end = f_charno( -1 ) + identlen( -1 );
            Init = f_NewInitializer();
            Expr = ExprCreateNew( Expr1, Init, ListExpr );
            return Expr;
         }
      }
      f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy ); /* 08.12.97 rigo */
   }

   Restore();
   return 0;
}

static List_t placement_opt( void )
{
   List_t ListExpr;
   Save();

   if( token( 0 ) == '(' )
   {
      step( 1 );
      if(( ListExpr = expression_list()))
      {
         if( token( 0 ) == ')' )
         {
            step( 1 );
         }
         return ListExpr;
      }
   }

   Restore();
   return 0;
}

static Expr_t deallocation_expression( void )
{
   Expr_t Expr1;
   Save();

#ifdef TRACE
   printf( "deallocation_expression: %s\n", ident( 0 ));
#endif

   if( token( 0 ) == SN_CLCL )
   {
      step( 1 );
   }

   if( token( 0 ) == SN_DELETE )
   {
      step( 1 );

      /* opcionalis [] */
      if( token( 0 ) == '[' && token( 1 ) == ']' )
      {
         step( 2 );
      }

      if(( Expr1 = cast_expression()))
      {
         return ExprCreateOp1( OPERATOR_DELETE, Expr1 );
      }
   }
   Restore();
   return 0;
}

static List_t expression_list( void )
{
   List_t ListExpr;
   Expr_t Expr;
   Save();

   niveau++;

   if(( Expr = assignment_expression()) == 0 )
   {
      Restore();
      niveau--;
      return 0;
   }

   ListExpr = f_ListCreate();
   f_ListAddLast( &ListExpr, (Elem_t) Expr );

   while( True )
   {
      switch( token( 0 ))
      {
      case ',': break;
      default :
         niveau--;
         return ListExpr;
      }

      step( 1 );

      if(( Expr = assignment_expression()) == 0 )
      {
         f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy );
         Restore();
         niveau--;
         return 0;
      }

      f_ListAddLast( &ListExpr, (Elem_t) Expr );
   }
}

static List_t expression_list_opt( void )
{
   List_t ListExpr;
   Expr_t Expr;
/*   Save(); */

   niveau++;

   ListExpr = f_ListCreate();

   if(( Expr = assignment_expression()) == 0 )
   {
      niveau--;
      return ListExpr;
   }

   f_ListAddLast( &ListExpr, (Elem_t) Expr );

   while( True )
   {
      switch( token( 0 ))
      {
      case ',':
         break;

      default :
         niveau--;
         return ListExpr;
      }

      step( 1 );

      if(( Expr = assignment_expression()) == 0 )
      {
/*           f_ListDestroy( ListExpr, (void(*)()) f_ExprDestroy ); */
/*           Restore(); */
         niveau--;
         return ListExpr;
      }

      f_ListAddLast( &ListExpr, (Elem_t) Expr );
   }
}

static Expr_t expression_member_name( void )
{
   Expr_t Expr;
   Name_t Name;
   int lineno_beg;
   int charno_beg;
   Save();

   switch( token( 0 ))
   {
   case SN_ASM      :
   case SN_CATCH    :
   case SN_CLASS    :
   case SN_DELETE   :
   case SN_FRIEND   :
   case SN_INLINE   :
   case SN_NEW      :
/* case SN_OPERATOR : */
   case SN_OVERLOAD :
   case SN_PRIVATE  :
   case SN_PROTECTED:
   case SN_PUBLIC   :
   case SN_TEMPLATE :
   case SN_THIS     :
   case SN_THROW    :
   case SN_TRY      :
   case SN_VIRTUAL  :
   case SN_VOLATILE :
      Expr = ExprCreateMemberName( f_NameCreate( ident( 0 )));
      Expr->lineno_beg = f_lineno( 0 );
      Expr->charno_beg = f_charno( 0 );
      Expr->lineno_end = f_lineno( 0 );
      Expr->charno_end = f_charno( 0 ) + identlen( 0 );
      step( 1 );
      return Expr;
   }

   lineno_beg = f_lineno( 0 );
   charno_beg = f_charno( 0 );
   if(( Name = f_Name()))
   {
      Expr = ExprCreateMemberName( Name );
      Expr->lineno_beg = lineno_beg;
      Expr->charno_beg = charno_beg;
      Expr->lineno_end = f_lineno( -1 );
      Expr->charno_end = f_charno( -1 ) + identlen( -1 );
      return Expr;
   }
   else
   {
      Restore();
      return 0;
   }
}

/*
static void skip_function_arg( void )
{
   int paren = 0;
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case '(':
         paren++;
         break;

      case ')':
         paren--;
         if( paren == 0 )
         {
            step( 1 );
            niveau--;
            return;
         }
         break;
      }
      step( 1 );
   }
}
*/

static Expr_t ExprCreateOp1( int operator, Expr_t Expr1 )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = operator;
   Expr->Name      = 0;
   Expr->Type      = 0;
   Expr->Expr1     = Expr1;
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateOp2( int operator, Expr_t Expr1, Expr_t Expr2 )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = operator;
   Expr->Name      = 0;
   Expr->Type      = 0;
   Expr->Expr1     = Expr1;
   Expr->Expr2     = Expr2;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateOp3( int operator, Expr_t Expr1, Expr_t Expr2, Expr_t Expr3 )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = operator;
   Expr->Name      = 0;
   Expr->Type      = 0;
   Expr->Expr1     = Expr1;
   Expr->Expr2     = Expr2;
   Expr->Expr3     = Expr3;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateOp2List( int operator, Expr_t Expr1, List_t ListExpr )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = operator;
   Expr->Name      = 0;
   Expr->Type      = 0;
   Expr->Expr1     = Expr1;
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = ListExpr;

   return Expr;
}

static Expr_t ExprCreateName( Name_t Name )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = OPERATOR_NAME;
   Expr->Name      = Name;
   Expr->Type      = 0;
   Expr->Expr1     = 0;
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateMemberName( Name_t Name )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = OPERATOR_NAME;
   Expr->Name      = Name;
   Expr->Type      = 0;
   Expr->Expr1     = 0;
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateType( Type_t Type )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = OPERATOR_TYPE;
   Expr->Name      = 0;
   Expr->Type      = Type;
   Expr->Expr1     = 0;
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = 0;
   Expr->ListExpr  = 0;

   return Expr;
}

static Expr_t ExprCreateNew( Expr_t Expr1, Init_t Init, List_t ListExpr )
{
   Expr_t Expr = (Expr_t) Malloc( sizeof( Expr[0] ));

   Expr->ExprNext  = 0;
   Expr->iCheck    = EXPR_CHECK;
   Expr->_operator = OPERATOR_NEW;
   Expr->Name      = 0;
   Expr->Type      = 0;
   Expr->Expr1     = Expr1;      /* ExprType */
   Expr->Expr2     = 0;
   Expr->Expr3     = 0;
   Expr->Init      = Init;
   Expr->ListExpr  = ListExpr;

   return Expr;
}

extern void f_ExprDestroy( Expr_t Expr )
{
   if( Expr )
   {
      if( Expr->iCheck != EXPR_CHECK ) Abort();
      if( Expr->Name      ) f_NameDestroy( Expr->Name  );
      if( Expr->Type      ) f_TypeDestroy( Expr->Type  );
      if( Expr->Expr1     ) f_ExprDestroy( Expr->Expr1 );
      if( Expr->Expr2     ) f_ExprDestroy( Expr->Expr2 );
      if( Expr->Expr3     ) f_ExprDestroy( Expr->Expr3 );
      if( Expr->Init      ) f_InitDestroy( Expr->Init  );
      if( Expr->ListExpr  ) f_ListDestroy( Expr->ListExpr, (void(*)()) f_ExprDestroy );
      ckfree( (char*)Expr );
   }
}

static Boolean_t f_IsNameSimpleTypeName( Name_t Name )
{
   char *pcLess  ;
   char *pcColon ;

   pcLess  = strrchr( Name->pcName, '<' );
   pcColon = strrchr( Name->pcName, ':' );

   if( pcLess  == 0 ) return False;
   if( pcColon == 0 ) return True;
   if( pcLess > pcColon ) return True;

   return False;
}


