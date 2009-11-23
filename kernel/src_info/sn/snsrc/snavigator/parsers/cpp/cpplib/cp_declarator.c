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

static int test = 0;

extern Declarator_t f_Declarator( Boolean_t may_function );
extern Declarator_t f_InitDeclarator( Boolean_t may_function );
extern Declarator_t f_MemberDeclarator( void );
extern Declarator_t f_AbstractDeclarator( void );
extern Declarator_t f_NewDeclarator( void );
extern Declarator_t f_ConversionDeclarator( void );
extern List_t f_InitDeclaratorList( Boolean_t may_function );
extern List_t f_MemberDeclaratorList( void );
extern Declarator_t f_DeclaratorCreate( void );
extern void f_DeclaratorDestroy( Declarator_t Declarator );

static Declarator_t f_DeclaratorParen( Boolean_t may_function );
static Declarator_t f_DeclaratorPtr( Boolean_t may_function );
static Declarator_t f_DeclaratorSymbol( Boolean_t may_function );
static Declarator_t f_DeclaratorPtrOrDname( Boolean_t may_function );
static Declarator_t f_DeclaratorDname( Boolean_t may_function );
static List_t __AbstractDeclarator( void );
static Oper_t f_PtrOperator( void );
static Oper_t f_SimplePtrOperator( void );
static List_t f_FunctionOrArrayList( Boolean_t may_function );
static List_t f_ArrayList( void );
static Oper_t f_OperArray( void );
static Oper_t f_OperFunction( void );
static void f_CvQualifierList( Oper_t Oper );
static void f_OperStrcat( char *pc, Oper_t Oper, int type_prev );

extern Declarator_t f_Declarator( Boolean_t may_function )
{
   switch( token( 0 ))
   {
   case '('           : return f_DeclaratorParen( may_function );
   case '*'           :
   case '&'           : return f_DeclaratorPtr( may_function );
   case SN_ASM        :
   case SN_CATCH      :
   case SN_DELETE     :
   case SN_NAMESPACE  :
   case SN_OVERLOAD   :
   case SN_TEMPLATE   :
   case SN_THIS       :
   case SN_THROW      :
   case SN_TRY        :
   case SN_USING      :
   case SN_CLASS      : return f_DeclaratorSymbol( may_function );
   case SN_IDENTIFIER :
   case SN_CLCL       : return f_DeclaratorPtrOrDname( may_function );
   case SN_OPERATOR   :
   case '~'           : return f_DeclaratorDname( may_function );
   default            : return 0;
   }
}

extern Declarator_t f_InitDeclarator( Boolean_t may_function )
{
   Declarator_t Declarator;
   Save();
   niveau++;

   if(( Declarator = f_Declarator( may_function )))
   {
      if( token( 0 ) == '=' )
      {
         step( 1 );
         if(( Declarator->Init = f_Init( 0 )) == 0 )
         {
            f_StepTo( ',', ';', 0 );
         }
      }
      else if( token( 0 ) == '(' )
      {
         step( 1 );
         if(( Declarator->ListExpr = f_ExpressionList()))
         {
            f_StepTo( ')', 0 );
            step( 1 );
         }
         else
         {
            f_DeclaratorDestroy( Declarator );
            niveau--;
            Restore();
            return 0;
         }
      }
      niveau--;
      return Declarator;
   }
   else
   {
      niveau--;
      return 0;
   }
}

extern Declarator_t f_MemberDeclarator( void )
{
   Declarator_t Declarator;
   niveau++;

   if( token( 0 ) == ':' || ( f_IsLiteral( token( 0 )) && token( 1 ) == ':' ))
   {
      Declarator = f_DeclaratorCreate();
      if( token( 0 ) == ':' )
      {
         step( 1 );
         Declarator->Expr = f_ConstantExpression();
      }
      else
      {
         Declarator->Name = f_NameCreate( ident( 0 ));
         Declarator->lineno_beg = f_lineno( 0 );
         Declarator->charno_beg = f_charno( 0 );
         Declarator->lineno_end = f_lineno( 0 );
         Declarator->charno_end = f_charno( 0 ) + identlen( 0 );
         step( 2 );
         Declarator->Expr = f_ConstantExpression();
      }
      niveau--;    /* Fix from rigo 16-jun-97 */
      return Declarator;
   }

   if(( Declarator = f_Declarator( True )))  /* 12.03.97 rigo: may_function = True */
   {
      if( token( 0 ) == '=' )
      {
         step( 1 );
         if(( Declarator->Init = f_Init( 0 )) == 0 )
         {
            f_StepTo( ',', ';', 0 );
         }
      }
      niveau--;
      return Declarator;
   }
   else
   {
      niveau--;
      return 0;
   }
}

extern Declarator_t f_AbstractDeclarator( void )
{
   List_t List;
   Declarator_t Declarator;

   if(( List = __AbstractDeclarator()))
   {
      Declarator = f_DeclaratorCreate();
      Declarator->ListOper = List;
   }
   else
   {
      Declarator = 0;
   }

   return Declarator;
}

extern Declarator_t f_NewDeclarator( void )
{
   Declarator_t Declarator;
   Oper_t OperPointer;
/* Save(); */

   switch( token( 0 ))
   {
   case '*':
   case SN_IDENTIFIER:
   case SN_CLCL:
      if(( OperPointer = f_SimplePtrOperator()) == 0 )
      {
         return 0;
      }
      if(( Declarator = f_NewDeclarator()))
      {
         f_ListAddLast( &Declarator->ListOper, (Elem_t) OperPointer );
      }
      return Declarator;

   case '[':
      Declarator = f_DeclaratorCreate();
      Declarator->ListOper = f_ArrayList();
      return Declarator;

   default:
      return 0;
   }
}

extern Declarator_t f_ConversionDeclarator( void )
{
   Declarator_t Declarator;
   Oper_t OperPointer;

/* Save(); */

   switch( token( 0 ))
   {
   case '*':
   case SN_IDENTIFIER:
   case SN_CLCL:
      if(( OperPointer = f_PtrOperator()) == 0 )
      {
         return 0;
      }
      Declarator = f_DeclaratorCreate();
      f_ListAddLast( &Declarator->ListOper, (Elem_t) OperPointer );
      return Declarator;

   default:
      return 0;
   }
}

extern List_t f_InitDeclaratorList( Boolean_t may_function )
{
   Save();
   List_t List = f_ListCreate();
   Declarator_t Declarator;

   while( True )
   {
      switch( token( 0 ))
      {
      case ',':
         step( 1 );
         break;

      case ';':
         step( 1 );
         return List;

      default:
         if(( Declarator = f_InitDeclarator( may_function )))
         {
            f_ListAddLast( &List, (Elem_t) Declarator );
            break;
         }
         else
         {
            Restore();
            f_ListDestroy( List, (void(*)()) f_DeclaratorDestroy );
            return 0;
         }
      }
   }
}

extern List_t f_MemberDeclaratorList( void )
{
/* Save(); */
   List_t List = f_ListCreate();
   Declarator_t Declarator;

   while( True )
   {
      switch( token( 0 ))
      {
      case ',':
         step( 1 );
         break;

      case 0:
      case ';':
      case '}':
         return List;

      default:
         if(( Declarator = f_MemberDeclarator()))  /* 15.02.97 rigo */
         {
            f_ListAddLast( &List, (Elem_t) Declarator );
         }
         else
         {
            f_StepTo( ',', ';', '}', 0 );
         }
         break;
      }
   }
}

static Declarator_t f_DeclaratorParen( Boolean_t may_function )
{
   Declarator_t Declarator;
   List_t ListOper;
   Save();
   niveau++;

   if( test ) printf( "f_DeclaratorParen: %s\n", ident( 0 ));

   step( 1 );

   if(( Declarator = f_Declarator( may_function )))
   {
      if( token( 0 ) == ')' )
      {
         Oper_t Oper;
         Oper = (Oper_t) f_ListLastElem( Declarator->ListOper );
         if( ! ( Oper && ( Oper->type == POINTER_STAR || Oper->type == POINTER_AMPERSAND )))
         {
            Declarator->bStrange = True;  /* 01.02.97 rigo */
         }
         step( 1 );
         if( Declarator->ListOper ) /* 12.03.97 rigo */
         {
            may_function = True;
         }
         ListOper = f_FunctionOrArrayList( may_function );
         if( ListOper == 0 )
         {
            Declarator->bStrange = True;  /* 01.02.97 rigo */
         }
         f_ListConcat( &Declarator->ListOper, ListOper );
         niveau--;
         if( test ) printf( "return OK\n" );
         return Declarator;
      }
      else
      {
         f_DeclaratorDestroy( Declarator );
         Restore();
         niveau--;
         if( test ) printf( "return 0\n" );
         return 0;
      }
   }
   else
   {
      Restore();
      niveau--;
      if( test ) printf( "return 0\n" );
      return 0;
   }
}

static Declarator_t f_DeclaratorPtr( Boolean_t may_function )
{
   Declarator_t Declarator;
   Oper_t OperPointer;
   Save();
   niveau++;

   if( test ) printf( "f_DeclaratorPtr: %s\n", ident( 0 ));

   if(( OperPointer = f_PtrOperator()))
   {
      if(( Declarator = f_Declarator( True )))
      {
         f_ListAddLast( &Declarator->ListOper, (Elem_t) OperPointer );
         niveau--;
         if( test ) printf( "return OK\n" );
         return Declarator;
      }
      else
      {
         f_OperDestroy( OperPointer );
         Restore();
         niveau--;
         if( test ) printf( "return 0\n" );
         return 0;
      }
   }
   else
   {
      Restore();
      niveau--;
      if( test ) printf( "return 0\n" );
      return 0;
   }
}

static Declarator_t f_DeclaratorSymbol( Boolean_t may_function )
{
   Declarator_t Declarator;

   if( test ) printf( "f_DeclaratorSymbol: %s\n", ident( 0 ));

   Declarator = f_DeclaratorCreate();
   Declarator->Name = f_NameCreate( ident( 0 ));
   Declarator->lineno_beg = f_lineno( 0 );
   Declarator->charno_beg = f_charno( 0 );
   Declarator->lineno_end = f_lineno( 0 );
   Declarator->charno_end = f_charno( 0 ) + identlen( 0 );
   step( 1 );
   Declarator->ListOper = f_FunctionOrArrayList( may_function );

   if( test ) printf( "return OK\n" );
   return Declarator;
}

static Declarator_t f_DeclaratorPtrOrDname( Boolean_t may_function )
{
   Declarator_t Declarator;
   Oper_t OperPointer;
   Name_t Name;
   int lineno_beg = f_lineno( 0 );
   int charno_beg = f_charno( 0 );
   Save();
   niveau++;

   if(( OperPointer = f_PtrOperator()))
   {
      if(( Declarator = f_Declarator( True )))
      {
         f_ListAddLast( &Declarator->ListOper, (Elem_t) OperPointer );
         niveau--;
         if( test ) printf( "return OK\n" );
         return Declarator;
      }
      else
      {
         f_OperDestroy( OperPointer );
         Restore();
         niveau--;
         if( test ) printf( "return 0\n" );
         return 0;
      }
   }
   else if(( Name = f_Dname()))
   {
      Declarator = f_DeclaratorCreate();
      Declarator->Name = Name;
      Declarator->lineno_beg = lineno_beg;
      Declarator->charno_beg = charno_beg;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identlen( -1 );
      Declarator->ListOper = f_FunctionOrArrayList( may_function );
      niveau--;
      if( test ) printf( "return OK\n" );
      return Declarator;
   }
   else
   {
      Restore();
      niveau--;
      if( test ) printf( "return 0\n" );
      return 0;
   }
}

static Declarator_t f_DeclaratorDname( Boolean_t may_function )
{
   Name_t Name;
   Declarator_t Declarator;
   int lineno_beg = f_lineno( 0 );
   int charno_beg = f_charno( 0 );
   Save();
   niveau++;

   if( test ) printf( "f_DeclaratorDname: %s\n", ident( 0 ));

   if(( Name = f_Dname()))
   {
      Declarator = f_DeclaratorCreate();
      Declarator->Name = Name;
      Declarator->lineno_beg = lineno_beg;
      Declarator->charno_beg = charno_beg;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identlen( -1 );
      Declarator->ListOper = f_FunctionOrArrayList( may_function );
      niveau--;
      if( test ) printf( "return OK\n" );
      return Declarator;
   }
   else
   {
      Restore();
      niveau--;
      if( test ) printf( "return 0\n" );
      return 0;
   }
}

static List_t __AbstractDeclarator( void )
{
   List_t List = 0;
   Oper_t OperPointer;

   Save();

   switch( token( 0 ))
   {
   case '*':
   case '&':
   case SN_IDENTIFIER:
   case SN_CLCL:
      if(( OperPointer = f_PtrOperator()) == 0 )
      {
         Restore();
         return 0;
      }
      List = __AbstractDeclarator();
      f_ListAddLast( &List, (Elem_t) OperPointer );
      return List;

   case '[':
      return f_FunctionOrArrayList( True );

   case '(':
      step( 1 );
      if( List = __AbstractDeclarator())
      {
         if( token( 0 ) == ')' )
         {
            step( 1 );
            f_ListConcat( &List, f_FunctionOrArrayList( True ));
            return List;
         }
         else
         {
            Restore();
            f_ListDestroy( List, (void(*)()) f_OperDestroy );
         }
      }
      else
      {
         Restore();
      }
      return f_FunctionOrArrayList( True );

   default:
      return 0;
   }
}

static Oper_t f_PtrOperator( void )
{
   Oper_t Oper = f_OperCreate();
   Save();

   switch( token( 0 ))
   {
   case '*':
      Oper->type = POINTER_STAR;
      step( 1 );
      f_CvQualifierList( Oper );
      return Oper;

   case '&':
      Oper->type = POINTER_AMPERSAND;
      step( 1 );
      f_CvQualifierList( Oper );
      return Oper;

   case SN_IDENTIFIER:
   case SN_CLCL:
      Oper->Name = f_CompleteClassName();
      if( token( 0 ) != SN_CLCL )
      {
         /* int ( far * kfunc )(); esetet le kell kezelni !! */
         Restore();
         f_OperDestroy( Oper );
#ifdef rigo
         return 0;
#else
         /* ez itt egy veszelyes fejlesztes, ami utan jol le kell
               tesztelni a programot 22.02.97 rigo */
         step( 1 );  /* atlepjuk az identifier-t */
         if(( Oper = f_PtrOperator()))
         {
            return Oper;
         }
         else
         {
            Restore();
            return 0;
         }
#endif
      }
      step( 1 );
      switch( token( 0 ))
      {
      case '*':
         Oper->type = POINTER_STAR;
         step( 1 );
         f_CvQualifierList( Oper );
         return Oper;

      default:
         Restore();
         f_OperDestroy( Oper );
         return 0;
      }
      
   default:
      Restore();
      f_OperDestroy( Oper );
      return 0;
   }
}

static Oper_t f_SimplePtrOperator( void )
{
   Oper_t Oper = f_OperCreate();
   Save();

   switch( token( 0 ))
   {
   case '*':
      Oper->type = POINTER_STAR;
      step( 1 );
      f_CvQualifierList( Oper );
      return Oper;

   case SN_IDENTIFIER:
   case SN_CLCL:
      Oper->Name = f_CompleteClassName();
      if( token( 0 ) != SN_CLCL )
      {
         Restore();
         f_OperDestroy( Oper );
         return 0;
      }
      step( 1 );
      switch( token( 0 ))
      {
      case '*':
         Oper->type = POINTER_STAR;
         step( 1 );
         f_CvQualifierList( Oper );
         return Oper;

      default:
         Restore();
         f_OperDestroy( Oper );
         return 0;
      }
      
   default:
      Restore();
      f_OperDestroy( Oper );
      return 0;
   }
}

static List_t f_FunctionOrArrayList( Boolean_t may_function )
{
   List_t List = 0;
   Oper_t Oper;
   int iFunction = 0;

   while( True )
   {
      switch( token( 0 ))
      {
      case '[':
         if(( Oper = f_OperArray()))
         {
            f_ListAddLast( &List, (Elem_t) Oper );
            break;
         }
         else
         {
            return List;
         }

      case SN_IDENTIFIER:
      case '(':
         if( ! may_function ) /* 12.03.97 rigo */
         {
            if (List)        /* Zsolt Koppany 11-jun-97; mem leak fix */
            {
               f_ListDestroy( List, (void(*)()) f_OperDestroy );

               List = 0;
            }

            return 0;
         }
         if( ++iFunction == 2 )  /* nem lehet ket fuggveny egy listaban */
         {
            return List;
         }

         if(( Oper = f_OperFunction()))
         {
            f_ListAddLast( &List, (Elem_t) Oper );
            break;
         }
         else
         {
            return List;
         }

      default:
         return List;
      }
   }
}

static List_t f_ArrayList( void )
{
   List_t List = 0;
   Oper_t Oper;

   while( True )
   {
      switch( token( 0 ))
      {
      case '[':
         if(( Oper = f_OperArray()))
         {
            f_ListAddLast( &List, (Elem_t) Oper );
            break;
         }
         else
         {
            return List;
         }

      default:
         return List;
      }
   }
}

static Oper_t f_OperArray( void )
{
   Oper_t Oper;

   step( 1 );
   Oper = f_OperCreate();
   Oper->type = ARRAY;
   Oper->Expr = expression();
   f_StepTo( ']', 0 );
   step( 1 );

   return Oper;
}

static Oper_t f_OperFunction( void )
{
   Oper_t Oper;
   Save();

   if( test ) printf( "f_OperFunction: %s\n", ident( 0 ));

   /* int a ANSI(( int b )); miatt */

   if( token( 0 ) == SN_IDENTIFIER )
   {
      step( 1 );
      if( token( 0 ) == '(' && token( 1 ) == '(' )
      {
/*
         printf( "****** ide csak ritkan jon be %s(%d.%d)\n", filename_g, f_lineno( 0 ), f_charno( 0 ));
*/
         Oper = f_OperCreate();
         Oper->type = FUNCTION;
         step( 1 );
         if(( Oper->ListDeclaration = f_ArgumentDeclarationList()))
         {
            f_StepTo( ')', 0 );
            step( 1 );
            f_CvQualifierList( Oper );
            return Oper;
         }
         else
         {
            f_OperDestroy( Oper );
            Restore();
            return 0;
         }
      }
      else
      {
         Restore();
         return 0;
      }
   }
   else  /* token( 0 ) == '(' */
   {
      Oper = f_OperCreate();
      Oper->type = FUNCTION;
      if(( Oper->ListDeclaration = f_ArgumentDeclarationList()))
      {
         f_CvQualifierList( Oper );
         return Oper;
      }
      else
      {
         f_OperDestroy( Oper );
         Restore();
         return 0;
      }
   }
}

static void f_CvQualifierList( Oper_t Oper )
{
   while( True )
   {
      switch( token( 0 ))
      {
      case SN_CONST   : Oper->s_const    = True; break;
      case SN_VOLATILE: Oper->s_volatile = True; break;
      default      : return;
      }
      step( 1 );
   }
}

extern Declarator_t f_DeclaratorCreate( void )
{
   Declarator_t Declarator = (Declarator_t) Malloc( sizeof( Declarator[0] ));
   memset((void *) Declarator, 0, sizeof( Declarator[0] ));
   Declarator->iCheck = DECLARATOR_CHECK;
   return Declarator;
}

extern Declarator_t f_DeclaratorDuplicate( Declarator_t Declarator )
{
   Declarator_t DeclaratorDuplicate;

   if( Declarator == 0 )
   {
      return 0;
   }

   if( Declarator->iCheck != DECLARATOR_CHECK ) Abort();

   DeclaratorDuplicate = f_DeclaratorCreate();

   DeclaratorDuplicate->Name     = f_NameDuplicate( Declarator->Name );
   DeclaratorDuplicate->ListOper = f_ListDuplicate( Declarator->ListOper
                                    , (Elem_t(*)(Elem_t)) f_OperDuplicate );
   return DeclaratorDuplicate;
}

extern void f_DeclaratorDestroy( Declarator_t Declarator )
{
   if( Declarator )
   {
      if( Declarator->iCheck != DECLARATOR_CHECK ) Abort();
      f_NameDestroy( Declarator->Name );
      f_ExprDestroy( Declarator->Expr );
      f_InitDestroy( Declarator->Init );
      f_ListDestroy( Declarator->ListOper, (void(*)()) f_OperDestroy );
      f_ListDestroy( Declarator->ListExpr, (void(*)()) f_ExprDestroy );
      ckfree( (char*)Declarator );
   }
}

extern void f_DeclaratorStrcat( char *pc, Declarator_t Declarator, int exact )
{
   Oper_t Oper;
   char ac[10000];   /* old: 1000 */
   int type;

   if( Declarator->Name && exact )
   {
      strcpy( ac, Declarator->Name->pcName );
   }
   else
   {
      ac[0] = 0;
   }

   type = 0;
   for( Oper = (Oper_t) d_ElemFirst( Declarator->ListOper )
      ; Oper
      ; Oper = Oper->OperNext )
   {
      f_OperStrcat( ac, Oper, type );
      type = Oper->type;
   }

   f_Strcat( pc, ac );
}

static void f_OperStrcat( char *pc, Oper_t Oper, int type_prev )
{
   Declaration_t Declaration;
   int bFirst;
   char ac[10000];   /* old: 1000 */
   ac[0] = 0;

   switch( Oper->type )
   {
   case POINTER_STAR:
      if( Oper->Name )
      {
         f_Strcat( ac, Oper->Name->pcName );
         f_Strcat( ac, "::" );
      }
      f_Strcat( ac, "*" );
      if( Oper->s_const    ) { f_Strcat( ac, "const"    ); }
      if( Oper->s_volatile ) { f_Strcat( ac, "volatile" ); }
      f_Strcat( ac, pc );
      strcpy( pc, ac );
      break;

   case POINTER_AMPERSAND:
      f_Strcat( ac, "&" );
      if( Oper->s_const    ) { f_Strcat( ac, "const"    ); }
      if( Oper->s_volatile ) { f_Strcat( ac, "volatile" ); }
      f_Strcat( ac, pc );
      strcpy( pc, ac );
      break;

   case ARRAY:
      if( type_prev == POINTER_STAR || type_prev == POINTER_AMPERSAND )
      {
         f_Strcat( ac, "(" );
         f_Strcat( ac, pc );
         f_Strcat( ac, ")" );
         strcpy( pc, ac );
      }
      f_Strcat( pc, "[]" );
      break;

   case FUNCTION:
      if( type_prev == POINTER_STAR || type_prev == POINTER_AMPERSAND )
      {
         f_Strcat( ac, "(" );
         f_Strcat( ac, pc );
         f_Strcat( ac, ")" );
         strcpy( pc, ac );
      }
      f_Strcat( pc, "(" );

      bFirst = True;
      for( Declaration = (Declaration_t) d_ElemFirst( Oper->ListDeclaration )
         ; Declaration
         ; Declaration = Declaration->DeclarationNext
         )
      {
         if( bFirst )
         {
            bFirst = False;
         }
         else
         {
            f_Strcat( pc, "," );
         }
         f_DeclarationStrcat( pc, Declaration );
      }
      f_Strcat( pc, ")" );
      break;
   default:
      f_InternalError( 11 );
      break;
   }
}

extern Boolean_t f_DeclaratorIsFunctionDefinition( Declarator_t Declarator )
{
   if( Declarator )
   {
      Oper_t Oper;

      if(( Oper = (Oper_t) d_ElemFirst( Declarator->ListOper )))
      {
         if( Oper->type == FUNCTION )
         {
            return True;
         }
      }
   }

   return False;
}

extern void f_DeclaratorProcess( Declarator_t Declarator )
{
   if( Declarator )
   {
      Expr_t Expr;
      Oper_t Oper;

      if( Declarator->Expr ) f_ExprProcess( Declarator->Expr );
      if( Declarator->Init ) f_InitProcess( Declarator->Init );

      for( Expr = (Expr_t) d_ElemFirst( Declarator->ListExpr )
         ; Expr
         ; Expr = Expr->ExprNext )
      {
         f_ExprProcess( Expr );
      }

      for( Oper = (Oper_t) d_ElemFirst( Declarator->ListOper )
         ; Oper
         ; Oper = Oper->OperNext )
      {
         f_OperProcess( Oper );
      }
   }
}

