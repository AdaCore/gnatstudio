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
#include <ctype.h>

#include <tcl.h>

#include "crossrefP.h"

#define TokenSave()     int iTokenA_Save = iTokenA
#define TokenRestore()  iTokenA = iTokenA_Save

#undef TEST_TYPE

static int iTokenA;
static int iTokenT;

static Declaration_t f_TypeCastToDeclaration( Type_t Type );
static Type_t TypeName( void );
static Declarator_t AbstractDeclarator( void );
static Oper_t PtrOperator( void );
static List_t FunctionOrArrayList( void );
static void CvQualifierList( Oper_t Oper );
static Name_t CompleteClassName( void );
static List_t ArgumentDeclarationList( void );
static void f_TypeStrcat( char *pc, Type_t Type, int exact );

static void TokenInit( char *pc );
static int Token( int i );
static void TokenStep( int i );
static char *TokenIdent( int i );
static void TokenRead( int i );
static int TokenLex( void );
static void TemplateArgList( char *pname );

struct _sToken
{
   int iToken;
   char *pcToken;
   int iLength;
};

static struct _sToken asToken[10000];   /* old: 1000 */
static char *pcLex;
static char *pcLexBeg;
static char *pcToken;
static int iLength;

extern Type_t f_TypeCreate( void )
{
   Type_t Type = (Type_t) Malloc( sizeof( Type[0] ));
   memset((void*) Type, 0, sizeof( Type[0] ));
   Type->iCheck = TYPE_CHECK;
   return Type;
}

extern Type_t f_TypeDuplicate( Type_t Type )
{
   Type_t TypeDuplicate;

   if( Type == 0 )
   {
      return 0;
   }

   if( Type->iCheck != TYPE_CHECK ) Abort();

   TypeDuplicate = f_TypeCreate();

   TypeDuplicate->s_unknown     = Type->s_unknown    ;
   TypeDuplicate->fct_specifier = Type->fct_specifier;
   TypeDuplicate->s_ellipsis    = Type->s_ellipsis   ;
   TypeDuplicate->s_const       = Type->s_const      ;
   TypeDuplicate->s_volatile    = Type->s_volatile   ;
   TypeDuplicate->s_char        = Type->s_char       ;
   TypeDuplicate->s_short       = Type->s_short      ;
   TypeDuplicate->s_int         = Type->s_int        ;
   TypeDuplicate->s_long        = Type->s_long       ;
   TypeDuplicate->s_signed      = Type->s_signed     ;
   TypeDuplicate->s_unsigned    = Type->s_unsigned   ;
   TypeDuplicate->s_float       = Type->s_float      ;
   TypeDuplicate->s_double      = Type->s_double     ;
   TypeDuplicate->s_bool        = Type->s_bool       ;
   TypeDuplicate->s_void        = Type->s_void       ;
   TypeDuplicate->Name          = f_NameDuplicate      ( Type->Name         );
   TypeDuplicate->Class         = 0;
   TypeDuplicate->Enum          = 0;
   TypeDuplicate->Declarator    = f_DeclaratorDuplicate( Type->Declarator   );

   return TypeDuplicate;
}

extern void f_TypeDestroy( Type_t Type )
{
   if( Type )
   {
      if( Type->iCheck != TYPE_CHECK ) Abort();
      if( Type->Name       ) f_NameDestroy       ( Type->Name       );
      if( Type->Class      ) f_ClassDestroy      ( Type->Class      );
      if( Type->Enum       ) f_EnumDestroy       ( Type->Enum       );
      if( Type->Declarator ) f_DeclaratorDestroy ( Type->Declarator );
      ckfree( (char*)Type );
   }
}

extern void f_TypeToString( Type_t Type, char *pc, int exact )
{
   *pc = 0;
   f_TypeStrcat( pc, Type, exact );
}

static void f_TypeStrcat( char *pc, Type_t Type, int exact )
{
   Boolean_t bType = False;   /* is explicit type defined ? */

   if( Type ) 
   {
      if( Type->fct_specifier == SN_INLINE   ) f_Strcat( pc, "inline" );
      if( Type->fct_specifier == SN_VIRTUAL  ) f_Strcat( pc, "virtual" );
      if( Type->s_unknown     )  f_Strcat( pc, "unknown" ), bType = True;
      if( Type->s_ellipsis    )  f_Strcat( pc, "..." ), bType = True;
      if( Type->s_const       )  f_Strcat( pc, "const" );
      if( Type->s_volatile    )  f_Strcat( pc, "volatile" );
/*    if( Type->s_signed      )  f_Strcat( pc, "signed" ); */
      if( Type->s_unsigned    )  f_Strcat( pc, "unsigned" );
      if( Type->s_short       )
         if( ! Type->s_int && ! Type->s_float && ! Type->s_double )
            f_Strcat( pc, "short" ), bType = True;
      if( Type->s_long        )
         if( ! Type->s_int && ! Type->s_float && ! Type->s_double )
            f_Strcat( pc, "long" ), bType = True;
      if( Type->s_char        )  f_Strcat( pc, "char" ), bType = True;
      if( Type->s_int         )
         /**/ if( Type->s_long  ) f_Strcat( pc, "long" ), bType = True;
         else if( Type->s_short ) f_Strcat( pc, "short" ), bType = True;
         else                     f_Strcat( pc, "int" ), bType = True;
      if( Type->s_float       )
         if( Type->s_long ) f_Strcat( pc, "double" ), bType = True;
         else               f_Strcat( pc, "float" ), bType = True;
      if( Type->s_double      )
         if( Type->s_short ) f_Strcat( pc, "float" ), bType = True;
         else                f_Strcat( pc, "double" ), bType = True;
	  if( Type->s_bool        )  f_Strcat( pc, "bool" ), bType = True;
      if( Type->s_void        )  f_Strcat( pc, "void" ), bType = True;
      if( Type->Name          )
      {
         f_Strcat( pc, Type->Name->pcName );
         bType = True;
      }
      if( Type->Class         )
      {
         if( Type->Class->Name )
            f_Strcat( pc, Type->Class->Name->pcName );
         bType = True;
      }
      if( Type->Enum          )
      {
         if( Type->Enum->Name )
            f_Strcat( pc, Type->Enum->Name->pcName );
         bType = True;
      }

      if( ! bType )
      {
         f_Strcat( pc, "int" ), bType = True;
      }

      if( Type->Declarator )
         f_DeclaratorStrcat( pc, Type->Declarator, exact );
   }
}

extern void f_TypeDelPointer( Type_t Type )
{
   Oper_t Oper;

   if( Type == 0 )
   {
      f_InternalError( 33 );
      return;
   }

   if( Type->s_unknown ) return;

   if( Type->Declarator )
   {
      if(( Oper = (Oper_t) d_ElemFirst( Type->Declarator->ListOper )))
      {
         if( Oper->type == POINTER_STAR
          || Oper->type == POINTER_AMPERSAND )
         {
            f_ListRemoveFirst( Type->Declarator->ListOper );
            f_OperDestroy( Oper );
         }
      }
   }
}

extern void f_TypeDelFunction( Type_t Type )
{
   Oper_t Oper;

   if( Type->s_unknown ) return;

   if( Type->Declarator )
   {
      if(( Oper = (Oper_t) d_ElemFirst( Type->Declarator->ListOper )))
      {
         if( Oper->type == FUNCTION )
         {
            f_ListRemoveFirst( Type->Declarator->ListOper );
            f_OperDestroy( Oper );
         }
      }
   }
}

extern void f_TypeAddPointer( Type_t Type )
{
   if( Type->s_unknown ) return;

   if( Type && Type->Declarator )
   {
      Oper_t Oper = f_OperCreate();
      Oper->type = POINTER_STAR;
      f_ListAddFirst( &Type->Declarator->ListOper, (Elem_t) Oper );
   }
}

extern Type_t f_TypeCreateUnknown( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_unknown = True;
   return Type;
}

extern Type_t f_TypeCreateInt( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_int = True;
   return Type;
}

extern Type_t f_TypeCreateLong( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_long = True;
   return Type;
}

extern Type_t f_TypeCreateChar( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_char = True;
   return Type;
}

extern Type_t f_TypeCreateShort( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_short = True;
   return Type;
}

extern Type_t f_TypeCreateFloat( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_float = True;
   return Type;
}

extern Type_t f_TypeCreateDouble( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_double = True;
   return Type;
}

extern Type_t f_TypeCreateBool( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_bool = True;
   return Type;
}

extern Type_t f_TypeCreateUnsigned( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_unsigned = True;
   return Type;
}

extern Type_t f_TypeCreateSigned( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_signed = True;
   return Type;
}

extern Type_t f_TypeCreateVoid( void )
{
   Type_t Type = f_TypeCreate();
   Type->s_void = True;
   return Type;
}

extern Type_t f_TypeCreateString( void )
{
   Type_t Type = f_TypeCreate();
   Declarator_t Declarator = f_DeclaratorCreate();
   Oper_t Oper = f_OperCreate();

/* printf( "TypeCreateString\n" ); */

   Type->s_char = True;
   Type->s_const = True;
   Type->Declarator = Declarator;
   Oper->type = POINTER_STAR;
   f_ListAddLast( &Type->Declarator->ListOper, (Elem_t) Oper );
   return Type;
}

extern Type_t f_TypeCreateName( Name_t Name )
{
   Type_t Type = f_TypeCreate();
   Type->Name = Name;
   return Type;
}

extern Type_t f_TypeFromString( char *type )
{
   Type_t Type;

/* printf( "f_TypeFromString: <%s>\n", type ); */

   TokenInit( type );
   Type = TypeName();

   return Type;
}

extern Type_t f_TypeFromDeclarationAndDeclarator( Declaration_t Declaration, Declarator_t Declarator )
{
   Type_t Type = f_TypeCreate();
   char *name;

   if( Declaration->Name )
   {
      name = Declaration->Name->pcName;
   }
   else if( Declaration->Class )
   {
      if( Declaration->Class->Name )
         name = Declaration->Class->Name->pcName;
      else
         name = 0;
   }
   else if( Declaration->Enum )
   {
      if( Declaration->Enum->Name )
         name = Declaration->Enum->Name->pcName;
      else
         name = 0;
   }
   else
   {
      name = 0;
   }

   Type->fct_specifier  = Declaration->fct_specifier;
   Type->s_ellipsis     = Declaration->s_ellipsis;
   Type->s_const        = Declaration->s_const;
   Type->s_volatile     = Declaration->s_volatile;
   Type->s_char         = Declaration->s_char;
   Type->s_short        = Declaration->s_short;
   Type->s_int          = Declaration->s_int;
   Type->s_long         = Declaration->s_long;
   Type->s_signed       = Declaration->s_signed;
   Type->s_unsigned     = Declaration->s_unsigned;
   Type->s_float        = Declaration->s_float;
   Type->s_double       = Declaration->s_double;
   Type->s_bool         = Declaration->s_bool;
   Type->s_void         = Declaration->s_void;
   Type->Name           = name ? f_NameCreate( name ) : 0;
   Type->Class          = 0;
   Type->Enum           = 0;
   Type->Declarator     = Declarator;

   return Type;
}

extern Type_t f_TypeFromDeclaration( Declaration_t Declaration )
{
   Type_t Type = f_TypeCreate();
   char *name;

   if( Declaration->Name )
   {
      name = Declaration->Name->pcName;
   }
   else if( Declaration->Class )
   {
      if( Declaration->Class->Name )
         name = Declaration->Class->Name->pcName;
      else
         name = 0;
   }
   else if( Declaration->Enum )
   {
      if( Declaration->Enum->Name )
         name = Declaration->Enum->Name->pcName;
      else
         name = 0;
   }
   else
   {
      name = 0;
   }

   Type->fct_specifier  = Declaration->fct_specifier;
   Type->s_ellipsis     = Declaration->s_ellipsis;
   Type->s_const        = Declaration->s_const;
   Type->s_volatile     = Declaration->s_volatile;
   Type->s_char         = Declaration->s_char;
   Type->s_short        = Declaration->s_short;
   Type->s_int          = Declaration->s_int;
   Type->s_long         = Declaration->s_long;
   Type->s_signed       = Declaration->s_signed;
   Type->s_unsigned     = Declaration->s_unsigned;
   Type->s_float        = Declaration->s_float;
   Type->s_double       = Declaration->s_double;
   Type->s_bool         = Declaration->s_bool;
   Type->s_void         = Declaration->s_void;
   Type->Name           = name ? f_NameCreate( name ) : 0;
   Type->Class          = 0;
   Type->Enum           = 0;
   Type->Declarator     = 0;

   return Type;
}
static Declaration_t f_TypeCastToDeclaration( Type_t Type )
{
   Declaration_t Declaration;

   Declaration = f_DeclarationCreate( DECLARATION_ARGUMENT );

   Declaration->fct_specifier  = Type->fct_specifier;
   Declaration->s_ellipsis     = Type->s_ellipsis;
   Declaration->s_const        = Type->s_const;
   Declaration->s_volatile     = Type->s_volatile;
   Declaration->s_char         = Type->s_char;
   Declaration->s_short        = Type->s_short;
   Declaration->s_int          = Type->s_int;
   Declaration->s_long         = Type->s_long;
   Declaration->s_signed       = Type->s_signed;
   Declaration->s_unsigned     = Type->s_unsigned;
   Declaration->s_float        = Type->s_float;
   Declaration->s_double       = Type->s_double;
   Declaration->s_bool         = Type->s_bool;
   Declaration->s_void         = Type->s_void;
   Declaration->Name           = Type->Name;
   Declaration->Class          = Type->Class;
   Declaration->Enum           = Type->Enum;

   if( Type->Declarator )
   {
      f_ListAddLast( &Declaration->ListDeclarator, (Elem_t) Type->Declarator );
   }

   Type->Name       = 0;
   Type->Class      = 0;
   Type->Enum       = 0;
   Type->Declarator = 0;

   f_TypeDestroy( Type );

   return Declaration;
}

static Type_t TypeName( void )
{
   Type_t Type;
   Declarator_t Declarator;
   int bFirst = True;

/* printf( "TypeName: {%s}\n", pcLex ); */

   if( Token( 0 ) == '(' )
   {
      TokenStep( 1 );
      Type = TypeName();
      TokenStep( 1 );
      return Type;
   }

   Type = f_TypeCreate();

   while( True )
   {
      int t = Token( 0 );

/*    printf( "token: %d %s {%s}\n", t, TokenIdent( 0 ), pcLex ); */

      switch( t )
      {
      case SN_CONST     : Type->s_const       = True      ; goto label_1;
      case SN_VOLATILE  : Type->s_volatile    = True      ; goto label_1;
      case SN_INLINE    : Type->fct_specifier = SN_INLINE ; goto label_1;
      case SN_VIRTUAL   : Type->fct_specifier = SN_VIRTUAL; goto label_1;
label_1:
         TokenStep( 1 );
         break;

      case SN_STRUCT    : TokenStep( 1 ); break;
      case SN_UNION     : TokenStep( 1 ); break;
      case SN_CLASS     : TokenStep( 1 ); break;
      case SN_ENUM      : TokenStep( 1 ); break;
      case SN_CHAR      : Type->s_char        = True   ; goto label;
      case SN_SHORT     : Type->s_short       = True   ; goto label;
      case SN_INT       : Type->s_int         = True   ; goto label;
      case SN_LONG      : Type->s_long        = True   ; goto label;
      case SN_FLOAT     : Type->s_float       = True   ; goto label;
      case SN_DOUBLE    : Type->s_double      = True   ; goto label;
      case SN_BOOL      : Type->s_bool        = True   ; goto label;
      case SN_VOID      : Type->s_void        = True   ; goto label;
      case SN_SIGNED    : Type->s_signed      = True   ; goto label;
      case SN_UNSIGNED  : Type->s_unsigned    = True   ; goto label;
label:
         bFirst = False;
         TokenStep( 1 );
         break;

      case SN_IDENTIFIER:
         if( bFirst )
         {
            Type->Name = CompleteClassName();
/*          printf( "After complete class name: %d\n", Token( 0 ));*/
            bFirst = False;
         }
         else
         {
            if(( Declarator = AbstractDeclarator()) == 0 )
            {
               Name_t Name = CompleteClassName();

               if( Type->Name == 0 )
               {
                  Type->Name = Name;
               }
               else
               {
                  f_NameDestroy( Name );
               }
               bFirst = False;
            }
            else
            {
               if( Type->Declarator == 0 )
               {
                  Type->Declarator = Declarator;
               }
               else
               {
                  f_DeclaratorDestroy( Declarator );
               }
               return Type;
            }
         }
         break;

      case '*':
      case '&':
      case SN_CLCL:
      case '[':
      case '(':
         if( bFirst )
         {
#ifdef TEST
            f_InternalError( 55 );
#endif
            f_TypeDestroy( Type );
            return 0;
         }
         if(( Declarator = AbstractDeclarator()) == 0 )
         {
            f_InternalError( 56 );
            f_TypeDestroy( Type );
            return 0;
         }
         else
         {
            if( Type->Declarator == 0 )
            {
               Type->Declarator = Declarator;
            }
            else
            {
               f_DeclaratorDestroy( Declarator );
            }
         }
         return Type;

      default:
         if( bFirst )
         {
            f_TypeDestroy( Type );
            return 0;
         }
         return Type;
      }
   }
}

static Declarator_t AbstractDeclarator( void )
{
   Oper_t OperPointer;
   Declarator_t Declarator;
   List_t ListOper;

   TokenSave();

/* printf( "AbstractDeclartor: %s {%s}\n", TokenIdent( 0 ), pcLex ); */

   switch( Token( 0 ))
   {
   case '*':
   case '&':
   case SN_IDENTIFIER:
   case SN_CLCL:
      if(( OperPointer = PtrOperator()) == 0 )
      {
         return 0;
      }
      if(( Declarator = AbstractDeclarator()) == 0 )  /* 14.02.97 rigo */
      {
         Declarator = f_DeclaratorCreate();
      }
      f_ListAddLast( &Declarator->ListOper, (Elem_t) OperPointer );
      return Declarator;

   case '[':
      Declarator = f_DeclaratorCreate();
      Declarator->ListOper = FunctionOrArrayList();
      return Declarator;

   case '(':
      TokenStep( 1 );
      if( OperPointer = PtrOperator())
      {
         f_OperDestroy( OperPointer );
         TokenRestore();
         TokenStep( 1 );
         if( Declarator = AbstractDeclarator())
         {
            if( Token( 0 ) == ')' )
            {
               TokenStep( 1 );
               ListOper = FunctionOrArrayList();
               f_ListConcat( &Declarator->ListOper, ListOper );
               return Declarator;
            }
            else
            {
               TokenRestore();
               f_DeclaratorDestroy( Declarator );
               return 0;
            }
         }
         else
         {
            TokenRestore();
            return 0;
         }
      }
      else if( Token( 0 ) == '[' )  /* 06.03.97 rigo: Void (*([]))() miatt */
      {
         if( Declarator = AbstractDeclarator())
         {
            if( Token( 0 ) == ')' )
            {
               TokenStep( 1 );
               ListOper = FunctionOrArrayList();
               f_ListConcat( &Declarator->ListOper, ListOper );
               return Declarator;
            }
            else
            {
               TokenRestore();
               f_DeclaratorDestroy( Declarator );
               return 0;
            }
         }
         else
         {
            TokenRestore();
            return 0;
         }
      }
      else
      {
         TokenRestore();
         Declarator = f_DeclaratorCreate();
         Declarator->ListOper = FunctionOrArrayList();
         return Declarator;
      }

   default:
      return 0;
   }
}


static Oper_t PtrOperator( void )
{
   Oper_t Oper = f_OperCreate();
   TokenSave();

/* printf( "PtrOperator: %s\n", TokenIdent( 0 )); */

   switch( Token( 0 ))
   {
   case '*':
      Oper->type = POINTER_STAR;
      TokenStep( 1 );
      CvQualifierList( Oper );
      return Oper;

   case '&':
      Oper->type = POINTER_AMPERSAND;
      TokenStep( 1 );
      CvQualifierList( Oper );
      return Oper;

   case SN_IDENTIFIER:
   case SN_CLCL:
      Oper->Name = CompleteClassName();
      if( Token( 0 ) != SN_CLCL )
      {
         f_OperDestroy( Oper );
         return 0;
      }
      TokenStep( 1 );
      switch( Token( 0 ))
      {
      case '*':
         Oper->type = POINTER_STAR;
         TokenStep( 1 );
         CvQualifierList( Oper );
         return Oper;

      default:
         TokenRestore();
         f_OperDestroy( Oper );
         return 0;
      }
      
   default:
      TokenRestore();
      f_OperDestroy( Oper );
      return 0;
   }
}

static List_t FunctionOrArrayList( void )
{
   List_t ListOper = 0;
   Oper_t Oper;

   while( True )
   {
      switch( Token( 0 ))
      {
      case '[':
         TokenStep( 2 );
         Oper = f_OperCreate();
         Oper->type = ARRAY;
         f_ListAddLast( &ListOper, (Elem_t) Oper );
         break;

      case '(':
         TokenStep( 1 );
         Oper = f_OperCreate();
         Oper->type = FUNCTION;
         Oper->ListDeclaration = ArgumentDeclarationList();
         TokenStep( 1 );
         f_ListAddLast( &ListOper, (Elem_t) Oper );
         break;

      default:
         return ListOper;
      }
   }
}

static void CvQualifierList( Oper_t Oper )
{
   while( True )
   {
      switch( Token( 0 ))
      {
      case SN_CONST   : Oper->s_const    = True; break;
      case SN_VOLATILE: Oper->s_volatile = True; break;
      default      : return;
      }
      TokenStep( 1 );
   }
}

static Name_t CompleteClassName( void )
{
   char ac[10000];   /* old: 1000 */
   TokenSave();
   
   ac[0] = 0;

   if( Token( 0 ) == SN_CLCL )
   {
      f_Strcat( ac, "::" );
      TokenStep( 1 );
   }

   if( Token( 0 ) == SN_IDENTIFIER )
   {
      f_Strcat( ac, TokenIdent( 0 ));
      TokenStep( 1 );
      if( Token( 0 ) == '<' )
      {
         TemplateArgList( ac );
      }
   }
   else
   {
      TokenRestore();
      return 0;
   }

   while( True )
   {
/*    TokenSave(); */

      if( Token( 0 ) == SN_CLCL
       && Token( 1 ) == SN_IDENTIFIER )
      {
         f_Strcat( ac, "::" );
         f_Strcat( ac, TokenIdent( 1 ));
         TokenStep( 2 );
      }
      else
      {
         return f_NameCreate( ac );
      }
   }
}

static List_t ArgumentDeclarationList( void )
{
   List_t ListDeclaration = 0;
   Declaration_t Declaration;
   Type_t Type;

   while( True )
   {
      switch( Token( 0 ))
      {
      case SN_ELLIPSIS:
         TokenStep( 1 );
         Declaration = f_DeclarationCreate( DECLARATION_ARGUMENT );
         Declaration->s_ellipsis = True;
         f_ListAddLast( &ListDeclaration, (Elem_t) Declaration );
         return ListDeclaration;

      case ',':
         TokenStep( 1 );
         break;

      case ')':
         return ListDeclaration;

      default:
         Type = TypeName();
         if( Type == 0 )
         {
            Type = f_TypeCreateInt();
            while( True )
            {
               if( Token( 0 ) == ')' ) break;
               if( Token( 0 ) == ',' ) break;
               if( Token( 0 ) == SN_ELLIPSIS ) break;
               if( Token( 0 ) == 0   ) break;
               TokenStep( 1 );
            }
         }
         Declaration = f_TypeCastToDeclaration( Type );
         f_ListAddLast( &ListDeclaration, (Elem_t) Declaration );
         break;
      }
   }
}

/*************************************************************
**
**
**    Token processing
**
**
*************************************************************/

static void TokenInit( char *pc )
{
   iTokenA = 0;
   iTokenT = 0;
   pcLex = pc;
   pcLexBeg = pc;
}

static int Token( int i )
{
   return ((iTokenT<=iTokenA+(i)?TokenRead(i),0:0), asToken[(iTokenA+(i))].iToken);
}

static void TokenStep( int i )
{
   iTokenA += i;
}

static char *TokenIdent( int i )
{
   static char ac[10000];  /* old: 1000 */
   struct _sToken *psToken = asToken + (iTokenA+(i));

   if( iTokenT <= iTokenA + i )
   {
      TokenRead( i );
   }

   memcpy( ac, psToken->pcToken, psToken->iLength );

   ac[ psToken->iLength ] = 0;

   return ac;
}

static void TokenRead( int i )
{
   while( iTokenT <= iTokenA + i )
   {
      struct _sToken *psToken = asToken + (iTokenT);

      psToken->iToken = TokenLex();
      psToken->pcToken = pcToken;
      psToken->iLength = iLength;

      iTokenT++;
   }
}

#define input() (iLength++,(yychar=*pcLex++))
#define unput() (--iLength,(yychar=*--pcLex))

#include "cpkeyw.h"

static int TokenLex( void )
{
   int yychar;

   while( 1 )
   {
      pcToken = pcLex;
      iLength = 0;

      input();

      switch( yychar )
      {
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

         {
            register int ihash = 0;
            register int j = 0;

            do
            {
               if( j++ < 6 && ihash < HASH_MAX )
               {
                  ihash += c_hash[ (int) yychar ];
               }
            }
            while( isalnum( input()) || yychar == '_' );

            unput();

            if( ihash < HASH_MAX &&
                LexKeyWordTab[ihash].leng == iLength &&
                strncmp( LexKeyWordTab[ihash].pcName, pcToken, iLength ) == 0 )
            {
               return LexKeyWordTab[ihash].wLexId; 
            }

            return SN_IDENTIFIER;
         }

      case ' ':
      case '\t':
      case '\r':
      case '\n':
         break;

      case ':':   /* :, :: */
         switch( input())
         {
         case ':':
            return SN_CLCL;
         default:
            unput();
            return ':';
         }

      case 0:
         unput();
         return 0;

      default:
         return yychar;
      }
   }
}

extern void f_Strcat( char *pc1, char *pc2 )
{
   register int len = strlen( pc1 );

   if( pc2[0] != 0 )
   {
      if( len > 0 )
      {
         register char c1;
         register char c2;

         c1 = pc1[len-1];
         c2 = pc2[    0];

         if(( isalnum( c1 ) || c1 == '_' || c1 == '$' ) &&
            ( c2 != '>' ))
         {
            strcat( pc1 + len, " " );
            len++;
         }
      }
      strcat( pc1 + len, pc2 );
   }
}

static void TemplateArgList( char *pname )
{
   int i = 1;

   TokenStep( 1 );
   strcat( pname, "<" );

   while( i > 0 )
   {
      switch( Token( 0 ))
      {
         case '>':
            i--;
            break;

         case '<':
            i++;
            break;

         case 0:
            f_InternalError( 71 );
            i = 0;
            break;
      }
      f_Strcat( pname, TokenIdent( 0 ));
      TokenStep( 1 );
   }

   return;
}

extern void f_TypeProcess( Type_t Type )
{
   if( Type )
   {
      if( Type->Declarator )
      {
         f_DeclaratorProcess( Type->Declarator );
      }
   }
}


