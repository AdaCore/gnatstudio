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
#include <string.h>

#include <tcl.h>

#include "crossrefP.h"

/* Valid debugging macros for this file:
	#define PRINT
        #define SYMTAB_TRACE
*/

extern Declaration_t f_Declaration( int iLevel );
extern Declaration_t f_DeclarationCreate( int iType );
extern DeclarationSpecial_t f_DeclarationSpecialCreate( int iType );
extern void f_DeclarationDestroy( Declaration_t Declaration );
extern void f_DeclarationSpecialDestroy( DeclarationSpecial_t DeclarationSpecial );

static Declaration_t f_DeclarationAsm( void );
static Declaration_t f_DeclarationTemplate( void );
static Declaration_t f_DeclarationLinkageSpecification( void );
static Declaration_t f_DeclarationUsing( void );
static Declaration_t f_DeclarationNamespace( void );
static Declaration_t f_DeclarationObject( int iLevel );
static List_t __InitDeclaratorList( Boolean_t may_function );
static List_t __MemberDeclaratorList( void );
static void ctor_initializer( void );
static Boolean_t f_IsDeclarationStrange( Declaration_t Declaration );
static Boolean_t f_DeclaratorOldStyleFunctionDefinition( Declarator_t Declarator );
static void f_DeclaratorOldStyleFunctionDefinitionProcess( Declarator_t Declarator_a );
static Boolean_t f_DeclaratorHasParameterList( Declarator_t Declarator );
static Declaration_t f_DeclaratorFindParameter( Declarator_t Declarator, char *pcName );
static void f_DeclarationCopy( Declaration_t DeclarationDest, Declaration_t DeclarationSrc, Declarator_t DeclaratorSrc );
static Boolean_t f_DeclarationIsParameter( Declaration_t Declaration );
static void _DeclarationProcess( Declaration_t Declaration, int record );
static void _DeclarationSpecialProcess( DeclarationSpecial_t DeclarationSpecial, int record );
static Type_t f_TypeDeclaratorConcat( Type_t Type, Declarator_t Declarator );

extern Declaration_t f_Declaration( int iLevel )
{
   Declaration_t Declaration;
   char ac[20000];   /* old: 10000 */
   ac[0] = 0;

   switch( token( 0 ))
   {
   case SN_ASM      : return f_DeclarationAsm();
   case SN_TEMPLATE : return f_DeclarationTemplate();
   case SN_NAMESPACE: return f_DeclarationNamespace();
   case SN_USING    : return f_DeclarationUsing();
   case SN_EXTERN   :
      if( token( 1 ) == SN_STRINGliteral )
      {
         return f_DeclarationLinkageSpecification();
      }
      else
      {
         if(( Declaration = f_DeclarationObject( iLevel )))
         {
#ifdef PRINT
            f_DeclarationStrcat( ac, Declaration );
            printf( "declaration: %s\n", ac );
#endif
         }
         return Declaration;
      }
   default       :
      if(( Declaration = f_DeclarationObject( iLevel )))
      {
#ifdef PRINT
         f_DeclarationStrcat( ac, Declaration );
         printf( "declaration: %s\n", ac );
#endif
      }
      return Declaration;
   }
}

static Declaration_t f_DeclarationAsm( void )
{
   DeclarationSpecial_t DeclarationSpecial;
   niveau++;

   step( 1 );

   DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_ASM );

   if( token( 0 ) == LBRACE )
   {
      step( 1 );
      f_StepTo( RBRACE, 0 );
      step( 1 );
   }
   else if( token( 0 ) == '(' )
   {
      step( 1 );
      f_StepTo( ')', 0 );
      step( 1 );
   }

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   niveau--;
   return (Declaration_t) DeclarationSpecial;
}

static Declaration_t f_DeclarationTemplate( void )
{
   DeclarationSpecial_t DeclarationSpecial;
   Declaration_t Declaration2;
/* Save(); */
   niveau++;

   DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_TEMPLATE );

   step( 1 );

   if( token( 0 ) == '<' )
   {
      DeclarationSpecial->Name1 = f_TemplateArgumentList();
   }

   if(( Declaration2 = f_Declaration( LEVEL_2 )))
   {
      f_ListAddLast( &DeclarationSpecial->ListDeclaration, (Elem_t) Declaration2 );
      niveau--;
      return (Declaration_t) DeclarationSpecial;
   }
   else
   {
      f_DeclarationSpecialDestroy( DeclarationSpecial );
      f_StepTo( ';', 0 );
      step( 1 );
      niveau--;
      return 0;
   }
}

static Declaration_t f_DeclarationLinkageSpecification( void )
{
   DeclarationSpecial_t DeclarationSpecial;
   Declaration_t Declaration2;
   niveau++;

   DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_LINKAGE_SPECIFICATION );

   step( 1 );
   do
   {
      step( 1 );
   } while( token( 0 ) == SN_STRINGliteral );

   if( token( 0 ) == LBRACE )
   {
      step( 1 );
      while( token( 0 ) != RBRACE && token( 0 ) != 0 )
      {
         if(( Declaration2 = f_Declaration( LEVEL_2 )))
         {
            f_ListAddLast( &DeclarationSpecial->ListDeclaration
                         , (Elem_t) Declaration2 );
         }
         else
         {
            f_DeclarationSkip();
         }
      }
      step( 1 );
   }
   else
   {
      if(( Declaration2 = f_Declaration( LEVEL_2 )))
      {
         f_ListAddLast( &DeclarationSpecial->ListDeclaration
                      , (Elem_t) Declaration2 );
      }
      else
      {
         f_DeclarationSkip();
      }
   }

   niveau--;
   return (Declaration_t) DeclarationSpecial;
}

static Declaration_t f_DeclarationUsing( void )
{
   DeclarationSpecial_t DeclarationSpecial;
   Name_t Name;
/* Save(); */
   niveau++;

   if( token( 1 ) == SN_NAMESPACE )
   {
      step( 2 );
      if(( Name = f_NamespaceName()))
      {
         DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_USING_DIRECTIVE );
         DeclarationSpecial->Name1 = Name;
         f_StepTo( ';', 0 );
         step( 1 );
         niveau--;
         return (Declaration_t) DeclarationSpecial;
      }
      else
      {
         f_StepTo( ';', 0 );
         step( 1 );
         niveau--;
         return 0;
      }
   }
   else
   {
      step( 1 );
      if(( Name = f_NamespaceName()))
      {
         DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_USING );
         DeclarationSpecial->Name1 = Name;
         f_StepTo( ';', 0 );
         step( 1 );
         niveau--;
         return (Declaration_t) DeclarationSpecial;
      }
      else
      {
         f_StepTo( ';', 0 );
         step( 1 );
         niveau--;
         return 0;
      }
   }
}

static Declaration_t f_DeclarationNamespace( void )
{
   DeclarationSpecial_t DeclarationSpecial;
   Declaration_t Declaration2;
   Name_t Name1 = 0;
   Name_t Name2 = 0;
/* Save(); */
   niveau++;

   step( 1 );

   if( token( 0 ) == SN_IDENTIFIER )
   {
      Name1 = f_NameCreate( ident( 0 ));
      step( 1 );
      if( token( 0 ) == '=' )
      {
         step( 1 );
         if(( Name2 = f_NamespaceName()))
         {
            DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_NAMESPACE_ALIAS_DEFINITION );
            DeclarationSpecial->Name1 = Name1;
            DeclarationSpecial->Name2 = Name2;
            f_StepTo( ';', 0 );
            step( 1 );
            niveau--;
            return (Declaration_t) DeclarationSpecial;
         }
         else
         {
            f_NameDestroy( Name1 );
            f_StepTo( ';', 0 );
            step( 1 );
            niveau--;
            return 0;
         }
      }
   }

   if( token( 0 ) == LBRACE )
   {
      DeclarationSpecial = f_DeclarationSpecialCreate( DECLARATION_NAMESPACE_DEFINITION );
      DeclarationSpecial->Name1 = Name1;
      step( 1 );
      while( token( 0 ) != RBRACE && token( 0 ) != 0 )
      {
         if(( Declaration2 = f_Declaration( LEVEL_2 )))
         {
            f_ListAddLast( &DeclarationSpecial->ListDeclaration
                         , (Elem_t) Declaration2 );
         }
         else
         {
            f_DeclarationSkip();
         }
      }
      step( 1 );
      niveau--;
      return (Declaration_t) DeclarationSpecial;
   }
   else
   {
      f_NameDestroy( Name1 );
      f_StepTo( ';', 0 );
      step( 1 );
      niveau--;
      return 0;
   }
}

static Declaration_t f_DeclarationObject( int iLevel )
{
   Declaration_t Declaration;
   Class_t Class;
   Enum_t Enum;
   Name_t Name;
   List_t List;
   int iTypeSpec = 0;
   int iCvSpec = 0;
   int iStorage = 0;
   int iFctSpec = 0;
   int storage_class;
   Boolean_t may_function; /* 12.03.97 rigo */
   Save();

   niveau++;

   if( CompAct && token( 0 ) == SN_IDENTIFIER && token( 1 ) == ';' )
   {
      Restore();
      niveau--;
      return 0;
   }

   Declaration = f_DeclarationCreate( DECLARATION_OBJECT );

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_PUBLIC     : storage_class = 0     ; goto spec_storage;
      case SN_PRIVATE    : storage_class = SN_STATIC; goto spec_storage;
      case SN_PROTECTED  : storage_class = SN_STATIC; goto spec_storage;
spec_storage:
         step( 1 );
         if( iStorage + iTypeSpec + iFctSpec == 0 )
         {
            iStorage++;
            iLevel = LEVEL_2;
            Declaration->storage_class = storage_class;
         }
         else
         {
            goto identifier;
         }
         break;

      case SN_AUTO       : Declaration->storage_class = SN_AUTO    ; goto label;
      case SN_REGISTER   : Declaration->storage_class = SN_REGISTER; goto label;
      case SN_STATIC     : Declaration->storage_class = SN_STATIC  ; goto label;
      case SN_EXTERN     : Declaration->storage_class = SN_EXTERN  ; goto label;
      case SN_FRIEND     : Declaration->storage_class = SN_FRIEND  ; goto label;
      case SN_TYPEDEF    : Declaration->storage_class = SN_TYPEDEF ; goto label;
label:
         step( 1 );
         iStorage++;
         iLevel = LEVEL_2;
         break;

      case SN_INLINE     : Declaration->fct_specifier = SN_INLINE  ; goto fct_spec;
      case SN_VIRTUAL    : Declaration->fct_specifier = SN_VIRTUAL ; goto fct_spec;
fct_spec:
         step( 1 );
         iFctSpec++;
         iLevel = LEVEL_2;
         break;

      case SN_CLASS      :
         if(( Class = f_Class()) == 0 )
         {
            goto identifier;
         }
         else
         {
            Declaration->Class = Class;
            iTypeSpec++;
            iLevel = LEVEL_2;
         }
         break;

      case SN_STRUCT     :
      case SN_UNION      :
         if(( Class = f_Class()) == 0 )
         {
            f_InternalError( 33 );
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }
         else
         {
            Declaration->Class = Class;
            iTypeSpec++;
            iLevel = LEVEL_2;
         }
         break;

      case SN_ENUM       :
         if(( Enum = f_Enum()) == 0 )
         {
            f_InternalError( 34 );
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }
         else
         {
            Declaration->Enum = Enum;
            iTypeSpec++;
            iLevel = LEVEL_2;
         }
         break;

      case SN_CONST      : Declaration->s_const       = True; goto cv;
      case SN_VOLATILE   : Declaration->s_volatile    = True; goto cv;
cv:
         step( 1 );
         iCvSpec++;
         iLevel = LEVEL_2;
         break;

      case SN_CHAR       : Declaration->s_char        = True; goto type;
      case SN_SHORT      : Declaration->s_short       = True; goto type;
      case SN_INT        : Declaration->s_int         = True; goto type;
      case SN_LONG       : Declaration->s_long        = True; goto type;
      case SN_SIGNED     : Declaration->s_signed      = True; goto type;
      case SN_UNSIGNED   : Declaration->s_unsigned    = True; goto type;
      case SN_FLOAT      : Declaration->s_float       = True; goto type;
      case SN_DOUBLE     : Declaration->s_double      = True; goto type;
      case SN_BOOL       : Declaration->s_bool        = True; goto type;
      case SN_VOID       : Declaration->s_void        = True; goto type;
type:

	 step( 1 );
	 /* We could have a global variable or a function declaration. */
         iTypeSpec++;
         iLevel = LEVEL_2;
         break;

      case ';'        :
         /* empty declarator_list */
         step( 1 );
         niveau--;
         return Declaration;

      case SN_ASM        : /* 13.09.96 rigo */
      case SN_CATCH      : /* 13.09.96 rigo */
      case SN_DELETE     : /* 13.09.96 rigo */
      case SN_NAMESPACE  : /* 13.09.96 rigo */
      case SN_NEW        : /* 13.09.96 rigo */
      case SN_OVERLOAD   : /* 13.09.96 rigo */
      case SN_TEMPLATE   : /* 13.09.96 rigo */
      case SN_THIS       : /* 13.09.96 rigo */
      case SN_THROW      : /* 13.09.96 rigo */
      case SN_TRY        : /* 13.09.96 rigo */
      case SN_USING      : /* 13.09.96 rigo */
      case SN_IDENTIFIER :
      case SN_CLCL       :
identifier:
/*       if( CompAct && iTypeSpec + iStorage + iFctSpec == 0 ) 24.02.97 rigo */
/* register hdr *ptr; -t nem tudta megerteni */
         if( CompAct && iTypeSpec + iFctSpec == 0 )
         {
complete_class_name:
            if(( Name = f_CompleteClassName()))
            {
               if( Declaration->Name == 0 )
               {
                  iTypeSpec++;
                  Declaration->Name = Name;
               }
               else
               {
                  f_NameDestroy( Name );
               }
               break;
            }
            else
            {
               f_DeclarationDestroy( Declaration );
               Restore();
               niveau--;
               return 0;
            }
         }
         else
         {
            /* 12.03.97 rigo */
            if( CompAct == 0 || Declaration->storage_class == SN_EXTERN )
            {
               may_function = True;
            }
            else
            {
               may_function = False;
            }

            if(( List = __InitDeclaratorList( may_function )))
            {
               f_ListConcat( &Declaration->ListDeclarator, List );
               if( iLevel == LEVEL_1 && f_IsDeclarationStrange( Declaration ))
               {
                  f_DeclarationDestroy( Declaration );
                  Declaration = 0;
                  Restore();
               }
               niveau--;
               return Declaration;
            }
            else
            {
               goto complete_class_name;
            }
         }
         break;

      case SN_OPERATOR   :
      case '('        :
      case '*'        :
      case '&'        :
      case '~'        :

         /* 12.03.97 rigo */
         if( CompAct == 0 || Declaration->storage_class == SN_EXTERN )
         {
            may_function = True;
         }
         else
         {
            may_function = False;
         }

         if(( List = __InitDeclaratorList( may_function )))
         {
            f_ListConcat( &Declaration->ListDeclarator, List );
            if( iLevel == LEVEL_1 && f_IsDeclarationStrange( Declaration ))
            {
               f_DeclarationDestroy( Declaration );
               Declaration = 0;
               Restore();
            }
            niveau--;
            return Declaration;
         }
         else
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }

      default         :
         f_DeclarationDestroy( Declaration );
         Restore();
         niveau--;
         return 0;
      }
   }
}

/* Az f_DeclarationObject-tal csaknem azonos.
   Elteresek:
   (1) nem kell a CompAct figyeleset vegezni
   (2) az InitDeclarator helyett MemberDeclarator-t hivunk
   (3) az InitDeclaratorList helyett MemberDeclaratorList-t hivunk
   (4) a ':' is bevezetheti a Declarator-t (bitfield)
*/

extern Declaration_t f_MemberDeclaration( void )
{
   Declaration_t Declaration;
   Class_t Class;
   Enum_t Enum;
   Name_t Name;
   List_t List;
   int iTypeSpec = 0;
   int iStorage = 0;
   int iFctSpec = 0;
   int storage_class;
   Save();

   niveau++;

   Declaration = f_DeclarationCreate( DECLARATION_MEMBER );

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_PUBLIC     : storage_class = 0     ; goto spec_storage;
      case SN_PRIVATE    : storage_class = SN_STATIC; goto spec_storage;
      case SN_PROTECTED  : storage_class = SN_STATIC; goto spec_storage;
spec_storage:
         step( 1 );
         if( iStorage + iTypeSpec + iFctSpec == 0 )
         {
            iStorage++;
            Declaration->storage_class = storage_class;
         }
         else
         {
            goto identifier;
         }
         break;

      case SN_AUTO       : Declaration->storage_class = SN_AUTO    ; goto label;
      case SN_REGISTER   : Declaration->storage_class = SN_REGISTER; goto label;
      case SN_STATIC     : Declaration->storage_class = SN_STATIC  ; goto label;
      case SN_EXTERN     : Declaration->storage_class = SN_EXTERN  ; goto label;
      case SN_FRIEND     : Declaration->storage_class = SN_FRIEND  ; goto label;
      case SN_TYPEDEF    : Declaration->storage_class = SN_TYPEDEF ; goto label;
label:
         step( 1 );
         iStorage++;
         break;

      case SN_INLINE     : Declaration->fct_specifier = SN_INLINE  ; goto fct_spec;
      case SN_VIRTUAL    : Declaration->fct_specifier = SN_VIRTUAL ; goto fct_spec;
fct_spec:
         step( 1 );
         iFctSpec++;
         break;

      case SN_CLASS      :
         if(( Class = f_Class()) == 0 )
         {
            goto identifier;
         }
         else
         {
            Declaration->Class = Class;
         }
         break;

      case SN_STRUCT     :
      case SN_UNION      :
         if(( Class = f_Class()) == 0 )
         {
            f_InternalError( 35 );
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }
         else
         {
            Declaration->Class = Class;
         }
         break;

      case SN_ENUM       :
         if(( Enum = f_Enum()) == 0 )
         {
            f_InternalError( 35 );
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }
         else
         {
            Declaration->Enum = Enum;
         }
         break;

      case SN_CONST      : Declaration->s_const       = True; goto type;
      case SN_VOLATILE   : Declaration->s_volatile    = True; goto type;
      case SN_CHAR       : Declaration->s_char        = True; goto type;
      case SN_SHORT      : Declaration->s_short       = True; goto type;
      case SN_INT        : Declaration->s_int         = True; goto type;
      case SN_LONG       : Declaration->s_long        = True; goto type;
      case SN_SIGNED     : Declaration->s_signed      = True; goto type;
      case SN_UNSIGNED   : Declaration->s_unsigned    = True; goto type;
      case SN_FLOAT      : Declaration->s_float       = True; goto type;
      case SN_DOUBLE     : Declaration->s_double      = True; goto type;
      case SN_BOOL       : Declaration->s_bool        = True; goto type;
      case SN_VOID       : Declaration->s_void        = True; goto type;
type:
         step( 1 );
         iTypeSpec++;
         break;

      case ';'        :
         /* empty declarator_list */
         step( 1 );
         niveau--;
         return Declaration;

      case SN_ASM        : /* 13.09.96 rigo */
      case SN_CATCH      : /* 13.09.96 rigo */
      case SN_DELETE     : /* 13.09.96 rigo */
      case SN_NAMESPACE  : /* 13.09.96 rigo */
      case SN_NEW        : /* 13.09.96 rigo */
      case SN_OVERLOAD   : /* 13.09.96 rigo */
      case SN_TEMPLATE   : /* 13.09.96 rigo */
      case SN_THIS       : /* 13.09.96 rigo */
      case SN_THROW      : /* 13.09.96 rigo */
      case SN_TRY        : /* 13.09.96 rigo */
      case SN_USING      : /* 13.09.96 rigo */
      case SN_IDENTIFIER :
      case SN_CLCL       :
identifier:
         if( False ) /* optionalis a decl_specifiers */
         {
complete_class_name:
            if(( Name = f_CompleteClassName()))
            {
               if( Declaration->Name == 0 )
               {
                  iTypeSpec++;
                  Declaration->Name = Name;
               }
               else
               {
                  f_NameDestroy( Name );
               }
               break;
            }
            else
            {
               printf( "Error by MemberDeclaration: ident: %s (%s:%d.%d)\n"
                     , ident( 0 )
                     , filename_g
                     , f_lineno( 0 )
                     , f_charno( 0 )
                     );
               f_DeclarationDestroy( Declaration );
               Restore();
               niveau--;
               return 0;
            }
         }
         else
         {
            if(( List = __MemberDeclaratorList()))
            {
               f_ListConcat( &Declaration->ListDeclarator, List );
               niveau--;
               return Declaration;
            }
            else
            {
               goto complete_class_name;
            }
         }
         break;

      case SN_OPERATOR   :
      case '('        :
      case '*'        :
      case '&'        :
      case '~'        :
      case ':'        : /* a bitfield-ek miatt */
         if(( List = __MemberDeclaratorList()))
         {
            f_ListConcat( &Declaration->ListDeclarator, List );
            niveau--;
            return Declaration;
         }
         else
         {
            f_DeclarationDestroy( Declaration );
            Restore();
            niveau--;
            return 0;
         }

      default         :
         f_DeclarationDestroy( Declaration );
         Restore();
         niveau--;
         return 0;
      }
   }
}

extern Declaration_t f_DeclarationCreate( int iType )
{
   Declaration_t Declaration = (Declaration_t) Malloc( sizeof( Declaration[0] ));
   memset((void *) Declaration, 0, sizeof( Declaration[0] ));
   Declaration->iType = iType;
   Declaration->lineno = f_lineno( 0 );
   Declaration->iCheck = DECLARATION_CHECK;

   return Declaration;
}

extern DeclarationSpecial_t f_DeclarationSpecialCreate( int iType )
{
   DeclarationSpecial_t DeclarationSpecial = (DeclarationSpecial_t) Malloc( sizeof( DeclarationSpecial[0] ));
   memset((void *) DeclarationSpecial, 0, sizeof( DeclarationSpecial[0] ));
   DeclarationSpecial->iType = iType;
   DeclarationSpecial->iCheck = DECLARATION_CHECK;

   return DeclarationSpecial;
}

extern Declaration_t f_DeclarationDuplicate( Declaration_t Declaration )
{
   Declaration_t DeclarationDuplicate;

   if( Declaration == 0 )
   {
      return 0;
   }

   if( Declaration->iCheck != DECLARATION_CHECK ) Abort();

   DeclarationDuplicate = f_DeclarationCreate( Declaration->iType );

   DeclarationDuplicate->storage_class   = Declaration->storage_class;
   DeclarationDuplicate->fct_specifier   = Declaration->fct_specifier;
   DeclarationDuplicate->s_ellipsis      = Declaration->s_ellipsis   ;
   DeclarationDuplicate->s_const         = Declaration->s_const      ;
   DeclarationDuplicate->s_volatile      = Declaration->s_volatile   ;
   DeclarationDuplicate->s_char          = Declaration->s_char       ;
   DeclarationDuplicate->s_short         = Declaration->s_short      ;
   DeclarationDuplicate->s_int           = Declaration->s_int        ;
   DeclarationDuplicate->s_long          = Declaration->s_long       ;
   DeclarationDuplicate->s_signed        = Declaration->s_signed     ;
   DeclarationDuplicate->s_unsigned      = Declaration->s_unsigned   ;
   DeclarationDuplicate->s_float         = Declaration->s_float      ;
   DeclarationDuplicate->s_double        = Declaration->s_double     ;
   DeclarationDuplicate->s_bool          = Declaration->s_bool       ;
   DeclarationDuplicate->s_void          = Declaration->s_void       ;

   DeclarationDuplicate->Name            = f_NameDuplicate( Declaration->Name );
   DeclarationDuplicate->Class           = 0;
   DeclarationDuplicate->Enum            = 0;
   DeclarationDuplicate->ListDeclarator  =
               f_ListDuplicate( Declaration->ListDeclarator
                              , (Elem_t(*)(Elem_t)) f_DeclaratorDuplicate );

   DeclarationDuplicate->lineno          = Declaration->lineno;

   return DeclarationDuplicate;
}

extern void f_DeclarationDestroy( Declaration_t Declaration )
{
   if( Declaration )
   {
/*       printf( "DeclarationDestroy: %d %d\n", Declaration->iType, f_lineno( 0 )); */
      if( Declaration->iCheck != DECLARATION_CHECK ) Abort();

      switch( Declaration->iType )
      {
      case DECLARATION_ASM                        :
      case DECLARATION_TEMPLATE                   :
      case DECLARATION_LINKAGE_SPECIFICATION      :
      case DECLARATION_NAMESPACE_DEFINITION       :
      case DECLARATION_NAMESPACE_ALIAS_DEFINITION :
      case DECLARATION_USING_DIRECTIVE            :
      case DECLARATION_USING                      :
         f_DeclarationSpecialDestroy((DeclarationSpecial_t) Declaration );
         break;

      case DECLARATION_OBJECT                     :
      case DECLARATION_MEMBER                     :
      case DECLARATION_ARGUMENT                   :
         f_NameDestroy( Declaration->Name );
         f_ClassDestroy( Declaration->Class );
         f_EnumDestroy( Declaration->Enum );
         f_ListDestroy( Declaration->ListDeclarator, (void(*)()) f_DeclaratorDestroy );
         ckfree( (char*)Declaration );
         break;
      default:
         printf( "unknown declaration type: %d ( %d )\n"
               , Declaration->iType
               , (int) Declaration );
         f_InternalError( 3 );
         break;
      }
   }
}

extern void f_DeclarationSpecialDestroy( DeclarationSpecial_t DeclarationSpecial )
{
   if( DeclarationSpecial )
   {
      if( DeclarationSpecial->iCheck != DECLARATION_CHECK ) Abort();

      f_NameDestroy( DeclarationSpecial->Name1 );
      f_NameDestroy( DeclarationSpecial->Name2 );
      f_ListDestroy( DeclarationSpecial->ListDeclaration, (void(*)()) f_DeclarationDestroy );
/*       f_ListDestroy( DeclarationSpecial->ListTemplateArgument, (void(*)()) f_TemplateArgumentDestroy ); */
      f_DeclarationDestroy( DeclarationSpecial->Declaration );
      ckfree( (char*)DeclarationSpecial );
   }
}

extern void f_DeclarationStrcat( char *pc, Declaration_t Declaration )
{
   Declarator_t Declarator;
   Boolean_t bType = False;   /* is type explicit defined ? */

   if( Declaration->s_ellipsis )
   {
      f_Strcat( pc, "..." );
/*       return; */
   }

   switch( Declaration->storage_class )
   {
   case SN_EXTERN  : f_Strcat( pc, "extern"   ); break;
   case SN_STATIC  : f_Strcat( pc, "static"   ); break;
   case SN_AUTO    : f_Strcat( pc, "auto"     ); break;
   case SN_REGISTER: f_Strcat( pc, "register" ); break;
   case SN_FRIEND  : f_Strcat( pc, "friend"   ); break;
   case SN_TYPEDEF : f_Strcat( pc, "typedef"  ); break;
   case 0       : break;
   default:
      printf( "unknown storage class: %d ( %d )\n"
            , Declaration->storage_class
            , (int) Declaration
            );
      f_InternalError( 4 );
      break;
   }

   switch( Declaration->fct_specifier )
   {
   case SN_INLINE  : f_Strcat( pc, "inline"   ); break;
   case SN_VIRTUAL : f_Strcat( pc, "virtual"  ); break;
   case 0       : break;
   default      : f_InternalError( 5 ); break;
   }

   if( Declaration->s_const         ) f_Strcat( pc, "const" );
   if( Declaration->s_volatile      ) f_Strcat( pc, "volatile" );
   if( Declaration->s_unsigned      ) f_Strcat( pc, "unsigned" );
   if( Declaration->s_short         )
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double   ) f_Strcat( pc, "short" ), bType = True;
   if( Declaration->s_long          )
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double   ) f_Strcat( pc, "long" ), bType = True;
   if( Declaration->s_char          ) f_Strcat( pc, "char" ), bType = True;
   if( Declaration->s_int           )
      /**/ if( Declaration->s_long  ) f_Strcat( pc, "long" ), bType = True;
      else if( Declaration->s_short ) f_Strcat( pc, "short" ), bType = True;
      else                            f_Strcat( pc, "int" ), bType = True;
   if( Declaration->s_float         )
      if( Declaration->s_long       ) f_Strcat( pc, "double" ), bType = True;
      else                            f_Strcat( pc, "float" ), bType = True;
   if( Declaration->s_double        )
      if( Declaration->s_short      ) f_Strcat( pc, "float" ), bType = True;
      else                            f_Strcat( pc, "double" ), bType = True;
   if( Declaration->s_bool          ) f_Strcat( pc, "bool" ), bType = True;
   if( Declaration->s_void          ) f_Strcat( pc, "void" ), bType = True;
   if( Declaration->Name )
   {
      f_Strcat( pc, Declaration->Name->pcName );
      bType = True;
   }
   if( Declaration->Class )
   {
      if( Declaration->Class->Name )
      {
         f_Strcat( pc, Declaration->Class->Name->pcName );
         bType = True;
      }
      else
      {
         f_Strcat( pc, "anonymous" );
         bType = True;
      }
   }
   if( Declaration->Enum )
   {
      if( Declaration->Enum->Name )
      {
         f_Strcat( pc, Declaration->Enum->Name->pcName );
         bType = True;
      }
      else
      {
         f_Strcat( pc, "anonymous" );
         bType = True;
      }
   }

   if( ! bType )
   {
      f_Strcat( pc, "int" );
      bType = True;
   }

   if( Declaration->ListDeclarator )
   {
      Boolean_t bFirst = True;

      for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator )
         ; Declarator
         ; Declarator = Declarator->DeclaratorNext )
      {
         if( bFirst )
         {
            bFirst = False;
         }
         else
         {
            f_Strcat( pc, "," );
         }
         f_DeclaratorStrcat( pc, Declarator, 0 );
      }
   }
}

static List_t __InitDeclaratorList( Boolean_t may_function )
{
   Declarator_t Declarator;
   List_t List = 0;
   Save();

   if(( Declarator = f_InitDeclarator( may_function )))
   {
      if( token( 0 ) == ';' )
      {
         step( 1 );
         f_ListAddLast( &List, (Elem_t) Declarator );
         return List;
      }
      else if( token( 0 ) == ',' )
      {
         step( 1 );
         f_ListAddLast( &List, (Elem_t) Declarator );
         f_ListConcat( &List, f_InitDeclaratorList( may_function ));
         return List;
      }
      else if( CompAct == 0 && f_DeclaratorIsFunctionDefinition( Declarator ))
      {
         if( token( 0 ) == LBRACE )
         {
            f_CompoundStatement( 0, 0 );
            f_ListAddLast( &List, (Elem_t) Declarator );
            return List;
         }
         else if( token( 0 ) == ':' )
         {
            ctor_initializer();
            f_CompoundStatement( 0, 0 );
            f_ListAddLast( &List, (Elem_t) Declarator );
            return List;
         }
         else if( token( 0 ) == SN_RETURN )  /* csak a gnu c++ -ban */
         {
            Declarator_t DeclaratorReturn;

            step( 1 );
            if(( DeclaratorReturn = f_InitDeclarator( False )))
            {
               if( token( 0 ) == ';' ) step( 1 );
               f_CompoundStatement( 0, 0 );
               f_ListAddLast( &List, (Elem_t) Declarator );
               return List;
            }
            else
            {
               f_DeclaratorDestroy( Declarator );
               Restore();
               return 0;
            }
         }
         else
         {
            /* lehet, hogy old_style_function_definition */
            if( f_DeclaratorOldStyleFunctionDefinition( Declarator ))
            {
               f_DeclaratorOldStyleFunctionDefinitionProcess( Declarator );
               if( token( 0 ) == LBRACE )
               {
                  f_CompoundStatement( 0, 0 );
                  f_ListAddLast( &List, (Elem_t) Declarator );
                  return List;
               }
               else
               {
                  f_InternalError( 12 );
               }
            }
            f_DeclaratorDestroy( Declarator );
            Restore();
            return 0;
         }
      }
      else
      {
         f_DeclaratorDestroy( Declarator );
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

static List_t __MemberDeclaratorList( void )
{
   Declarator_t Declarator;
   List_t List = 0;
   Save();

   if(( Declarator = f_MemberDeclarator()))
   {
      if( token( 0 ) == ';' )
      {
         step( 1 );
         f_ListAddLast( &List, (Elem_t) Declarator );
         return List;
      }
      else if( token( 0 ) == ',' )
      {
         step( 1 );
         f_ListAddLast( &List, (Elem_t) Declarator );
         f_ListConcat( &List, f_MemberDeclaratorList());
         return List;
      }
      else if( f_DeclaratorIsFunctionDefinition( Declarator ))
      {
         if( token( 0 ) == LBRACE )
         {
            f_CompoundStatement( 0, 0 );
            f_ListAddLast( &List, (Elem_t) Declarator );
            return List;
         }
         else if( token( 0 ) == ':' )
         {
            ctor_initializer();
            f_CompoundStatement( 0, 0 );
            f_ListAddLast( &List, (Elem_t) Declarator );
            return List;
         }
         else
         {
            f_DeclaratorDestroy( Declarator );
            Restore();
            return 0;
         }
      }
      else
      {
         f_DeclaratorDestroy( Declarator );
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

static void ctor_initializer( void )
{
/*    printf( "ctor_initializer: %d.%d\n", f_lineno(0), f_charno(0)); */

   niveau++;
   while( True )
   {
      switch( token( 0 ))
      {
      case LBRACE:
         niveau--;
         return;
      case  0 : 
         niveau--;
         return;
      default :
         break;
      }
      step( 1 );
   }
}

static Boolean_t f_IsDeclarationStrange( Declaration_t Declaration )
{
   Declarator_t Declarator;

   for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator )
      ; Declarator
      ; Declarator = Declarator->DeclaratorNext )
   {
      if( Declarator->bStrange ) return True;
   }

   return False;
}

static Boolean_t f_DeclaratorOldStyleFunctionDefinition( Declarator_t Declarator_a )
{
   Save();
   niveau++;

   if( ! f_DeclaratorHasParameterList( Declarator_a ))
   {
      niveau--;
      Restore();
      return False;
   }

   while( True )
   {
      Declaration_t Declaration;

      if( token( 0 ) == LBRACE )
      {
         niveau--;
         Restore();
         return True;
      }

      if( token( 0 ) == 0 )
      {
         niveau--;
         Restore();
         return False;
      }

      if(( Declaration = f_DeclarationObject( LEVEL_2 )))
      {
         Declarator_t Declarator;

         for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator)
            ; Declarator
            ; Declarator = Declarator->DeclaratorNext )
         {
            Declaration_t DeclarationParameter;
            DeclarationParameter = f_DeclaratorFindParameter( Declarator_a, Declarator->Name->pcName );

            if( ! DeclarationParameter )
            {
               f_DeclarationDestroy( Declaration );
               niveau--;
               Restore();
               return False;
            }
         }
         f_DeclarationDestroy( Declaration );
      }
      else
      {
         niveau--;
         Restore();
         return False;
      }
   }
}

static void f_DeclaratorOldStyleFunctionDefinitionProcess( Declarator_t Declarator_a )
{
   niveau++;

   while( True )
   {
      Declaration_t Declaration;

      if( token( 0 ) == LBRACE )
      {
         niveau--;
         return;
      }

      if( token( 0 ) == 0 )
      {
         f_InternalError( 23 );
         niveau--;
         return;
      }

      if(( Declaration = f_DeclarationObject( LEVEL_2 )))
      {
         Declarator_t Declarator;
         Declarator_t DeclaratorNext;

         for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator )
            ; Declarator
            ; Declarator = DeclaratorNext )
         {
            Declaration_t DeclarationParameter;
            DeclarationParameter = f_DeclaratorFindParameter( Declarator_a, Declarator->Name->pcName );

            if( ! DeclarationParameter )
            {
               f_InternalError( 25 );
               niveau--;
               return;
            }
            else
            {
               DeclaratorNext = Declarator->DeclaratorNext;
               f_ListRemoveFirst( Declaration->ListDeclarator );
               f_DeclarationCopy( DeclarationParameter, Declaration, Declarator );
            }
         }
         f_DeclarationDestroy( Declaration );
      }
      else
      {
         f_InternalError( 24 );
         niveau--;
         return;
      }
   }
}

static Boolean_t f_DeclaratorHasParameterList( Declarator_t Declarator )
{
   if( Declarator )
   {
      if( d_ElemFirst( Declarator->ListOper ))
      {
         Oper_t Oper = (Oper_t) d_ElemFirst( Declarator->ListOper );

         if( Oper->type == FUNCTION )
         {
            Declaration_t Declaration;

            for( Declaration = (Declaration_t) d_ElemFirst( Oper->ListDeclaration )
               ; Declaration
               ; Declaration = Declaration->DeclarationNext )
            {
               if( ! f_DeclarationIsParameter( Declaration ))
               {
                  return False;
               }
            }
            return True;
         }
      }
   }

   return False;
}

static Boolean_t f_DeclarationIsParameter( Declaration_t Declaration )
{
   if( Declaration->storage_class  ) return False;
   if( Declaration->fct_specifier  ) return False;
   if( Declaration->s_ellipsis     ) return False;
   if( Declaration->s_const        ) return False;
   if( Declaration->s_volatile     ) return False;
   if( Declaration->s_char         ) return False;
   if( Declaration->s_short        ) return False;
   if( Declaration->s_int          ) return False;
   if( Declaration->s_long         ) return False;
   if( Declaration->s_signed       ) return False;
   if( Declaration->s_unsigned     ) return False;
   if( Declaration->s_float        ) return False;
   if( Declaration->s_double       ) return False;
   if( Declaration->s_bool         ) return False;
   if( Declaration->s_void         ) return False;
   if( Declaration->Class          ) return False;
   if( Declaration->Enum           ) return False;
   if( Declaration->ListDeclarator ) return False;

   return True;
}

static Declaration_t f_DeclaratorFindParameter( Declarator_t Declarator, char *pcName )
{
   if( Declarator )
   {
      if( d_ElemFirst( Declarator->ListOper ))
      {
         Oper_t Oper = (Oper_t) d_ElemFirst( Declarator->ListOper );

         if( Oper->type == FUNCTION )
         {
            Declaration_t Declaration;
            for( Declaration = (Declaration_t) d_ElemFirst( Oper->ListDeclaration )
               ; Declaration
               ; Declaration = Declaration->DeclarationNext )
            {
               if( Declaration && Declaration->Name )
               {
                  if( strcmp( Declaration->Name->pcName, pcName ) == 0 )
                  {
                     return Declaration;
                  }
               }
            }
         }
      }
   }

   return 0;
}

static void f_DeclarationCopy( Declaration_t DeclarationDest, Declaration_t DeclarationSrc, Declarator_t DeclaratorSrc )
{
   if( DeclarationDest->ListDeclarator == 0 )
   {
      f_ListAddLast( &DeclarationDest->ListDeclarator, (Elem_t) DeclaratorSrc );

      DeclarationDest->storage_class = DeclarationSrc->storage_class;
      DeclarationDest->fct_specifier = DeclarationSrc->fct_specifier;
      DeclarationDest->s_ellipsis    = DeclarationSrc->s_ellipsis;
      DeclarationDest->s_const       = DeclarationSrc->s_const;
      DeclarationDest->s_volatile    = DeclarationSrc->s_volatile;
      DeclarationDest->s_char        = DeclarationSrc->s_char;
      DeclarationDest->s_short       = DeclarationSrc->s_short;
      DeclarationDest->s_int         = DeclarationSrc->s_int;
      DeclarationDest->s_long        = DeclarationSrc->s_long;
      DeclarationDest->s_signed      = DeclarationSrc->s_signed;
      DeclarationDest->s_unsigned    = DeclarationSrc->s_unsigned;
      DeclarationDest->s_float       = DeclarationSrc->s_float;
      DeclarationDest->s_double      = DeclarationSrc->s_double;
      DeclarationDest->s_bool        = DeclarationSrc->s_bool;
      DeclarationDest->s_void        = DeclarationSrc->s_void;
      DeclarationDest->Name          = DeclarationSrc->Name;
      DeclarationDest->Class         = DeclarationSrc->Class;
      DeclarationDest->Enum          = DeclarationSrc->Enum;

      DeclarationSrc->Name  = f_NameDup     ( DeclarationDest->Name  );
      DeclarationSrc->Class = f_ClassPoorDup( DeclarationDest->Class );
      DeclarationSrc->Enum  = f_EnumPoorDup ( DeclarationDest->Enum  );
   }
   else
   {
      f_DeclaratorDestroy( DeclaratorSrc );
   }
}

extern void f_DeclarationProcess( Declaration_t Declaration, int record )
{
   switch( Declaration->iType )
   {
      case DECLARATION_ASM                        :
      case DECLARATION_TEMPLATE                   :
      case DECLARATION_LINKAGE_SPECIFICATION      :
      case DECLARATION_NAMESPACE_DEFINITION       :
      case DECLARATION_NAMESPACE_ALIAS_DEFINITION :
      case DECLARATION_USING_DIRECTIVE            :
      case DECLARATION_USING                      :
         _DeclarationSpecialProcess((DeclarationSpecial_t) Declaration, record );
         return;
         break;

      case DECLARATION_OBJECT                     :
      case DECLARATION_MEMBER                     :
      case DECLARATION_ARGUMENT                   :
         _DeclarationProcess( Declaration, record );
         break;

      default:
         f_InternalError( 3 );
         break;
   }
}

static void _DeclarationProcess( Declaration_t Declaration, int record )
{
   Declarator_t Declarator;
   Declarator_t DeclaratorNext;
   Type_t Type;
   Type_t TypeBasic;
   Type_t TypeToDestroy = 0;

   TypeBasic = f_TypeFromDeclaration( Declaration );

   /* feloldjuk az esetleges typedef-eket */
   TypeBasic = f_TypeBasic( TypeBasic, Declaration->lineno );

   for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator )
      ; Declarator
      ; Declarator = DeclaratorNext )
   {
      DeclaratorNext = Declarator->DeclaratorNext;

      if( record )
      {
         f_ListRemoveFirst( Declaration->ListDeclarator );

         Type = f_TypeDeclaratorConcat( TypeBasic, Declarator );

         f_PutConstructor( Type, Declaration->lineno, CONSTRUCTOR | DESTRUCTOR );

         if( test_fp )
         {
            fprintf( test_fp
                   ,"%d xref_l %d.%d %d.%d\n"
                   , PAF_HIGH
                   , Declarator->lineno_beg
                   , Declarator->charno_beg
                   , Declarator->lineno_end
                   , Declarator->charno_end
                   );
         }

         if( SymtabInsert( SymtabVariable
                         , Type->Declarator->Name->pcName
                         , (void *) Type
                         , niveauComp ))
         {
#ifdef SYMTAB_TRACE
            char acType[10000];  /* old: 1000 */

            f_TypeToString( Type, acType, 1 );
            printf( "recorded type: %s ----- name: %s\n"
                  , acType
                  , Declarator->Name->pcName
                  );
#endif
         }
         else
         {
            TypeToDestroy = Type;
         }
      }

      /* TODO or BUG:
              There is something wrong with the Declarator->Init part
	      of the expression failing to correctly parser:
	                  int foo;
              But has no problems correctly understanding:
	                  int foo = 1;
      */

      if( Declarator->Expr || Declarator->Init )   /* inicializalt valtozo */
      {
         if( Declarator->Name )
         {

            /* This will output a declaration of a local variable. */

	    Put_cross_ref( PAF_REF_TO_LOCAL_VAR
                        , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                        , PAF_REF_SCOPE_LOCAL
                        , scope_g
                        , sym_name_g
                        , arg_types_g
                        , sym_name_g
                        , Declarator->Name->pcName
                        , 0
                        , filename_g
                        , Declaration->lineno
                        , PAF_REF_WRITE
                        );
         }
      }

      f_DeclaratorProcess( Declarator );

      /* ezt csak a vegen torolhetjuk, mert a Type alatti Declarator-ra
         meg szuksegunk volt */
      f_TypeDestroy( TypeToDestroy );
      TypeToDestroy = 0;
   }

   f_TypeDestroy( TypeBasic );
}


static void _DeclarationSpecialProcess( DeclarationSpecial_t DeclarationSpecial, int record )
{
   Declaration_t Declaration;

   for( Declaration = (Declaration_t) d_ElemFirst( DeclarationSpecial->ListDeclaration )
      ; Declaration
      ; Declaration = Declaration->DeclarationNext
      )
   {
      f_DeclarationProcess( Declaration, record );
   }

   if( DeclarationSpecial->Declaration )
   {
      f_DeclarationProcess( Declaration, record );
   }
}

extern void f_DeclarationSkip( void )
{
   f_StepTo( ';', '(', '{', 0 );

   switch( token( 0 ))
   {
   case ';':
      step( 1 );
      break;
   case '(':
      step( 1 );
      f_StepTo( ')', 0 );
      step( 1 );
      break;
   case '{':
      step( 1 );
      f_StepTo( '}', 0 );
      step( 1 );
      break;
   }
}

extern void f_DeclarationPrint( Declaration_t Declaration )
{
   Declarator_t Declarator;

   for( Declarator = (Declarator_t) d_ElemFirst( Declaration->ListDeclarator )
      ; Declarator
      ; Declarator = Declarator->DeclaratorNext )
   {
      Type_t Type;
      char acType[10000];  /* old: 1000 */

      Type = f_TypeFromDeclarationAndDeclarator( Declaration, Declarator );

      f_TypeToString( Type, acType, 1 );

      printf( "processed type: %s ----- name: %s\n"
            , acType
            , Declarator->Name->pcName
            );
      Type->Declarator = 0;

      f_TypeDestroy( Type );
   }
}

extern void Put_cross_class_or_typedef_ref( int type, int scope_type, int scope_lev, char *fnc_cls, char *fnc, char *fnc_arg_types, char *scope, char *what, char *arg_types, char *file, int lineno, int acc )
{
   if( type )
   {
      Put_cross_ref( type
                   , scope_type
                   , scope_lev
                   , fnc_cls
                   , fnc
                   , fnc_arg_types
                   , scope
                   , what
                   , arg_types
                   , file
                   , lineno
                   , acc
                   );

      /* Ha type == PAF_REF_TO_TYPE_DEF akkor, jobb lenne
         ha azzal a tipussal dolgoznank, ami a typedef mogott van */

      if( type == PAF_REF_TO_CLASS )
      {
         char acName[10000];   /* old: 1000 */
         char acScope[10000];  /* old: 100 */
         char acRetType[10000];/* old: 100 */
         int paf_type_ret;

         /* Ha van egyaltalan konstruktora a class-nak ? */

         if(( paf_type_ret = Get_symbol( fnc_cls
                                        , what
                                        , what
                                        , 0
                                        , acScope
                                        , acRetType
                                        , NULL
                                        , 0 )))
         {
            Put_cross_ref( PAF_REF_TO_MBR_FUNC
                         , scope_type
                         , scope_lev
                         , fnc_cls
                         , fnc
                         , fnc_arg_types
                         , acScope
                         , what
                         , arg_types
                         , file
                         , lineno
                         , acc
                         );
         }

         /* Ha van egyaltalan destruktora a class-nak ? */

         sprintf( acName, "~%s", what );

         if(( paf_type_ret = Get_symbol( fnc_cls
                                        , what
                                        , acName
                                        , 0
                                        , acScope
                                        , acRetType
                                        , NULL
                                        , 0 )))
         {
            Put_cross_ref( PAF_REF_TO_MBR_FUNC
                         , scope_type
                         , scope_lev
                         , fnc_cls
                         , fnc
                         , fnc_arg_types
                         , acScope
                         , acName
                         , arg_types
                         , file
                         , lineno
                         , acc
                         );
         }
      }
   }
}

/*
** Feloldjuk a typedef-eket.
** Az osszes kozbulso lepest lejelentjuk.
*/

extern Type_t f_TypeBasic( Type_t Type, int lineno )
{
   Type_t TypeBasic = Type;
   Type_t TypeTempo = 0;
   char *name;
   int paf_type;
   char acType[10000];  /* old: 1000 */

   /*
    * to break recursion in user's typedefs, like
    *      typedef type1 type2;
    *      typedef type2 type1;
    * In the upoun case SN can hang for ever.
    * (limited only for 3 levels)
    */
#  define PREV_I 3
   char *name_prev[PREV_I] = {0, 0, 0};
   int i=0, j=0, cmp_prev=0;
   
   while( True )
   {
      name = f_NameFromType( TypeBasic );

      if( name == 0 )
      {
         break;
      }

      /* see if this typedef has been visited */
      cmp_prev = 0;
      for (i=0; i<PREV_I; i++)
      {
	  if( name_prev[i] && strcmp( name_prev[i], name ) == 0 )
	  {
	      cmp_prev = 1;
	      break;
	  }
      }
      if (cmp_prev)
          break;

      if(( paf_type = Get_class_or_typedef( name, acType )) == 0 )
      {
	      break;
      }
   
      Put_cross_ref( paf_type
                   , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                   , PAF_REF_SCOPE_GLOBAL
                   , scope_g[0] ? scope_g : 0
                   , sym_name_g
                   , arg_types_g
                   , 0
                   , name
                   , 0
                   , filename_g
                   , lineno
                   , PAF_REF_READ
                   );

      TypeTempo = f_TypeFromString( acType );

      if( TypeTempo )
      {
         if( TypeTempo->Declarator == 0 )
         {
            if( TypeBasic )
            {
               TypeTempo->Declarator = TypeBasic->Declarator;
               TypeBasic->Declarator = 0;
            }
         }
         else
         {
            if( TypeBasic && TypeBasic->Declarator )
            {
               List_t ListOper = TypeTempo->Declarator->ListOper;
               TypeTempo->Declarator->ListOper = TypeBasic->Declarator->ListOper;
               TypeBasic->Declarator->ListOper = 0;

               f_ListConcat( &(TypeTempo->Declarator->ListOper), ListOper );
               TypeTempo->Declarator->Name = TypeBasic->Declarator->Name;
               TypeBasic->Declarator->Name = 0;
            }
         }

         f_TypeDestroy( TypeBasic );
         TypeBasic = TypeTempo;
         TypeTempo = 0;
      }
      else
      {
         break;
      }

      /* store visited types on the stack */
      if (j>=PREV_I)
         j = 0;
      if( name_prev[j] )
      {
         ckfree( (char*)name_prev[j] );
         name_prev[j] = 0;
      }
      name_prev[j] = name;
      j++;

      if( paf_type != PAF_REF_TO_TYPEDEF )
      {
         break;
      }
   }

   if( name )
   {
      ckfree( (char*)name );
      name = 0;
   }

   /* free visiting stack */
   for (i=0; i<PREV_I; i++)
     if( name_prev[i] )
     {
	ckfree( (char*)name_prev[i] );
	name_prev[i] = 0;
     }

   return TypeBasic;
}

static Type_t f_TypeDeclaratorConcat( Type_t Type_a, Declarator_t Declarator )
{
   Type_t Type = f_TypeDuplicate( Type_a );

   if( Type )
   {
      if( Type->Declarator == 0 )
      {
         Type->Declarator = Declarator;
      }
      else
      {
         Declarator_t Declarator2 = Type->Declarator;
         Type->Declarator = Declarator;
         Declarator = Declarator2;

         f_ListConcat( &(Type->Declarator->ListOper), Declarator->ListOper );
         Declarator->ListOper = 0;
         f_DeclaratorDestroy( Declarator );
      }
   }

   return Type;
}

/* a declaration eseteben, ha nem pointer tipusrol van szo, akkor a mode
** alapjan CONSTRUCTOR es/vagy DESTRUCTOR jelentes
*/

extern void f_PutConstructor( Type_t Type, int lineno, int mode )
{
   if( Type && Type->Declarator )
   {
      Oper_t Oper = (Oper_t) d_ElemFirst( Type->Declarator->ListOper );
      if( Oper == 0 || Oper->type == ARRAY ) /* tiszta class, vagy class tömb */
      {
         char *name;
         if(( name = f_NameFromType( Type )))
         {
            int paf_type;
            char acType[10000];  /* old: 1000 */
            if(( paf_type = Get_class_or_typedef( name, acType )) == PAF_REF_TO_CLASS )
            {
               char acName[10000];     /* old: 1000 */
               char acScope[10000];    /* old: 100 */
               char acRetType[10000];  /* old: 100 */
               int paf_type_ret;
      
               if( mode & CONSTRUCTOR )
               {
                  /* Ha van egyaltalan konstruktora a class-nak ? */

                  if(( paf_type_ret = Get_symbol( scope_g[0] ? scope_g : 0
                                                , name
                                                , name
                                                , 0
                                                , acScope
                                                , acRetType
                                                , NULL
                                                , 0 )))
                  {
                     Put_cross_ref( PAF_REF_TO_MBR_FUNC
                                  , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                                  , PAF_REF_SCOPE_GLOBAL
                                  , scope_g[0] ? scope_g : 0
                                  , sym_name_g
                                  , arg_types_g
                                  , acScope
                                  , name
                                  , 0
                                  , filename_g
                                  , lineno
                                  , PAF_REF_READ
                                  );
                  }
               }

               if( mode & DESTRUCTOR )
               {
                  /* Ha van egyaltalan destruktora a class-nak ? */

                  sprintf( acName, "~%s", name );

                  if(( paf_type_ret = Get_symbol( scope_g[0] ? scope_g : 0
                                                , name
                                                , acName
                                                , 0
                                                , acScope
                                                , acRetType
                                                , NULL
                                                , 0 )))
                  {
                     Put_cross_ref( PAF_REF_TO_MBR_FUNC
                                  , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                                  , PAF_REF_SCOPE_GLOBAL
                                  , scope_g[0] ? scope_g : 0
                                  , sym_name_g
                                  , arg_types_g
                                  , acScope
                                  , acName
                                  , 0
                                  , filename_g
                                  , lineno
                                  , PAF_REF_READ
                                  );
                  }
               }
            }
            ckfree( (char*)name );
         }
      }
   }
}

/* a new es/vagy delete operatorok eseteben
** a mode alapjan CONSTRUCTOR es/vagy DESTRUCTOR jelentes
*/

extern void f_PutConstructorByNewOrDelete( Type_t Type, int lineno, int mode )
{
   if( Type )
   {
      char *name;
      if(( name = f_NameFromType( Type )))
      {
         int paf_type;
         char acType[10000];  /* old: 1000 */
         if(( paf_type = Get_class_or_typedef( name, acType )) == PAF_REF_TO_CLASS )
         {
            char acName[10000];     /* old: 1000 */
            char acScope[10000];    /* old: 100 */
            char acRetType[10000];  /* old: 100 */
            int paf_type_ret;
   
            if( mode & CONSTRUCTOR )
            {
               /* Ha van egyaltalan konstruktora a class-nak ? */

               if(( paf_type_ret = Get_symbol( scope_g[0] ? scope_g : 0
                                             , name
                                             , name
                                             , 0
                                             , acScope
                                             , acRetType
                                             , NULL
                                             , 0 )))
               {
                  Put_cross_ref( PAF_REF_TO_MBR_FUNC
                               , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                               , PAF_REF_SCOPE_GLOBAL
                               , scope_g[0] ? scope_g : 0
                               , sym_name_g
                               , arg_types_g
                               , acScope
                               , name
                               , 0
                               , filename_g
                               , lineno
                               , PAF_REF_READ
                               );
               }
            }

            if( mode & DESTRUCTOR )
            {
               /* Ha van egyaltalan destruktora a class-nak ? */

               sprintf( acName, "~%s", name );

               if(( paf_type_ret = Get_symbol( scope_g[0] ? scope_g : 0
                                             , name
                                             , acName
                                             , 0
                                             , acScope
                                             , acRetType
                                             , NULL
                                             , 0 )))
               {
                  Put_cross_ref( PAF_REF_TO_MBR_FUNC
                               , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                               , PAF_REF_SCOPE_GLOBAL
                               , scope_g[0] ? scope_g : 0
                               , sym_name_g
                               , arg_types_g
                               , acScope
                               , acName
                               , 0
                               , filename_g
                               , lineno
                               , PAF_REF_READ
                               );
               }
            }
         }
         ckfree( (char*)name );
      }
   }
}


