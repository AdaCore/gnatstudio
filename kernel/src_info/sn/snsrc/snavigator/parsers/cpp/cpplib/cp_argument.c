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
#include "crossrefP.h"

static int test = 0;

static Declaration_t f_ArgumentDeclaration( void );
static Declarator_t f_ArgumentDeclarator( void );
static Declarator_t f_ArgDeclarator( void );
static Declarator_t f_ArgAbstractDeclarator( void );

extern List_t f_ArgumentDeclarationList( void )
{
   Save();
   List_t List;
   Declaration_t Declaration;

   if( token( 0 ) != '(' )
   {
      return 0;
   }

   step( 1 );

   List = f_ListCreate();

   if( token( 0 ) == ')' )
   {
      step( 1 );
      return List;   /* empty argument_declaration_list */
   }

   while( True )
   {
      if( token( 0 ) == SN_ELLIPSIS )
      {
         step( 1 );
         if( token( 0 ) == ')' )
         {
            step( 1 );
            Declaration = f_DeclarationCreate( DECLARATION_ARGUMENT );
            Declaration->s_ellipsis = True;
            f_ListAddLast( &List, (Elem_t) Declaration );
            return List;
         }
         else  /* csak az utolso argumentum lehet SN_ELLIPSIS */
         {
            f_ListDestroy( List, (void(*)()) f_DeclarationDestroy );
            Restore();
            return 0;
         }
      }

      if(( Declaration = f_ArgumentDeclaration()))
      {
         f_ListAddLast( &List, (Elem_t) Declaration );
         switch( token( 0 ))
         {
         case ','     : step( 1 ); break;
         case ')'     : step( 1 ); return List;
         case SN_ELLIPSIS: break;
         default      : 
            f_ListDestroy( List, (void(*)()) f_DeclarationDestroy );
            Restore();
            return 0;
         }
      }  
      else
      {
         f_ListDestroy( List, (void(*)()) f_DeclarationDestroy );
         Restore();
         return 0;
      }
   }
}

static Declaration_t f_ArgumentDeclaration( void )
{
   Declaration_t Declaration = f_DeclarationCreate( DECLARATION_ARGUMENT );
   Declarator_t Declarator;
   int iTypeSpec = 0;
   int iStorage = 0;
   int iModifier = 0;
   Name_t Name;
   Save();
   niveau++;

   if( test ) printf( "f_ArgumentDeclaration: %s\n", ident( 0 ));

   while( True )
   {
      switch( token( 0 ))
      {
   /* storage class specifiers */
      case SN_REGISTER  :
         Declaration->storage_class = SN_REGISTER;
         iStorage++;
         step( 1 );
         break;

      case SN_AUTO  :
         Declaration->storage_class = SN_AUTO;
         iStorage++;
         step( 1 );
         break;

   /* fct specifiers */
      case SN_INLINE    :
         Declaration->fct_specifier = SN_INLINE;
         iModifier++;
         step( 1 );
         break;

      case SN_VIRTUAL   :
         Declaration->fct_specifier = SN_VIRTUAL;
         iModifier++;
         step( 1 );
         break;

      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         iTypeSpec++;
         if(( Declaration->Class = f_Class()) == 0 )
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            f_SyntaxError( 2 );
            niveau--;
            return 0;
         }
         break;

      case SN_ENUM       :
         iTypeSpec++;
         if(( Declaration->Enum = f_Enum()) == 0 )
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            f_SyntaxError( 3 );
            niveau--;
            return 0;
         }
         break;

      case SN_CONST      : Declaration->s_const    = True; goto modifier;
      case SN_VOLATILE   : Declaration->s_volatile = True; goto modifier;
modifier:
         iModifier++;
         step( 1 );
         break;

      case SN_CHAR       : Declaration->s_char     = True; goto type;
      case SN_SHORT      : Declaration->s_short    = True; goto type;
      case SN_INT        : Declaration->s_int      = True; goto type;
      case SN_LONG       : Declaration->s_long     = True; goto type;
      case SN_SIGNED     : Declaration->s_signed   = True; goto type;
      case SN_UNSIGNED   : Declaration->s_unsigned = True; goto type;
      case SN_FLOAT      : Declaration->s_float    = True; goto type;
      case SN_DOUBLE     : Declaration->s_double   = True; goto type;
      case SN_BOOL       : Declaration->s_bool     = True; goto type;
      case SN_VOID       : Declaration->s_void     = True; goto type;
type:
         iTypeSpec++;
         step( 1 );
         break;

      case ',':
      case ')':
      case SN_ELLIPSIS:
         if( iTypeSpec > 0 || iStorage > 0 || iModifier > 0 )
         {  /* optional abstract_declarator */
            niveau--;
            return Declaration;
         }
         else
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            f_SyntaxError( 4 );
            niveau--;
            return 0;
         }

      case '=':
         if( iTypeSpec > 0 || iStorage > 0 || iModifier > 0 )
         {  /* optional abstract_declarator */
            step( 1 );
            Declarator = f_DeclaratorCreate();
            Declarator->Expr = f_AssignmentExpression();
            f_ListAddLast( &Declaration->ListDeclarator, (Elem_t) Declarator );
            niveau--;
            return Declaration;
         }
         else
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            f_SyntaxError( 5 );
            niveau--;
            return 0;
         }

      default:
         if( iTypeSpec == 0 && iStorage == 0 && iModifier == 0 )
         {
            if(( Declaration->Name = f_CompleteClassName()))
            {
               iTypeSpec++;
            }
            else
            {
               f_DeclarationDestroy( Declaration );
               Restore();
               niveau--;
               return 0;
            }
         }
         else if( iTypeSpec == 0 )
         {
            if(( Declaration->Name = f_CompleteClassName()))
            {
               iTypeSpec++;
            }
            else if(( Declarator = f_ArgumentDeclarator()))
            {
               f_ListAddLast( &Declaration->ListDeclarator, (Elem_t) Declarator );
               niveau--;
               return Declaration;
            }
            else
            {
/* A constructor hivasok nagyon hasonlitanak a fuggveny deklaraciokra, ezert
   itt nem szabad nagyvonaluan atlepni a feldolgozhatatlan token-t */
               f_DeclarationDestroy( Declaration );
               Restore();
               niveau--;
               return 0;
            }
         }
         else
         {
            if(( Declarator = f_ArgumentDeclarator()))
            {
               f_ListAddLast( &Declaration->ListDeclarator, (Elem_t) Declarator );
               niveau--;
               return Declaration;
            }

            if(( Name = f_CompleteClassName()))
            {
               iTypeSpec++;
               if( Declaration->Name )
               {
#ifdef CORVEX
                  printf( "error: second name: %s %s (%s:%d.%d)\n"
                        , Declaration->Name->pcName
                        , Name->pcName
                        , filename_g
                        , f_lineno( 0 )
                        , f_charno( 0 )
                        );
#endif
                  f_NameDestroy( Name );
               }
               else
               {
                  Declaration->Name = Name;
               }
            }
            else
            {
/*             step( 1 );
   A constructor hivasok nagyon hasonlitanak a fuggveny deklaraciokra, ezert
   itt nem szabad nagyvonaluan atlepni a feldolgozhatatlan token-t */
               f_DeclarationDestroy( Declaration );
               Restore();
/*                f_SyntaxError( 7 ); */
               niveau--;
               return 0;
            }
         }
         break;
      }
   }
}

static Declarator_t f_ArgumentDeclarator( void )
{
   Declarator_t Declarator;

   if(( Declarator = f_ArgDeclarator()))
   {
      return Declarator;
   }

   if(( Declarator = f_ArgAbstractDeclarator()))
   {
      return Declarator;
   }

   return 0;
}

static Declarator_t f_ArgDeclarator( void )
{
   Declarator_t Declarator;
   Save();

   if(( Declarator = f_Declarator( False ))) /* 12.03.97 rigo: may_function = False */
   {
      if( token( 0 ) == '=' )
      {
         step( 1 );
         Declarator->Expr = f_AssignmentExpression();
      }

      switch( token( 0 ))
      {
         case ',':
         case ')':
         case SN_ELLIPSIS:
            return Declarator;
         default:
            f_DeclaratorDestroy( Declarator );
            Restore();  /* 21.02.97 rigo */
            return 0;
      }
   }
   Restore();  /* 21.02.97 rigo */
   return 0;
}

static Declarator_t f_ArgAbstractDeclarator( void )
{
   Declarator_t Declarator;
   Save();

   if(( Declarator = f_AbstractDeclarator()))
   {
      if( token( 0 ) == '=' )
      {
         step( 1 );
         Declarator->Expr = f_AssignmentExpression();
      }

      switch( token( 0 ))
      {
         case ',':
         case ')':
         case SN_ELLIPSIS:
            return Declarator;
         default:
            f_DeclaratorDestroy( Declarator );
            Restore();  /* 21.02.97 rigo */
            return 0;
      }
   }
   Restore();  /* 21.02.97 rigo */
   return 0;
}


