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

static int test = 0;
extern Name_t f_NameCreate( char *pcName );
extern void f_NameDestroy( Name_t Name );
extern Name_t f_Dname( void );
extern Name_t Name( void );
extern Name_t f_OperatorOrConversionFunctionName( void );
extern Name_t f_CompleteClassName( void );
extern Name_t f_QualifiedName( void );
extern Name_t f_ClassName( void );

static Boolean_t f_TemplateArgList( char *pname );

/*

dname:
   name
   class_name
   
name:
   identifier
   operator_function_name
   conversion_function_name
   ~ class_name
   qualified_name

qualified_name:
   class_name :: name

qualified_class_name:
   class_name
   class_name :: qualified_class_name

comlpete_class_name:
   qualified_class_name
   :: qualified_class_name

*/

extern Name_t f_NameCreate( char *pcName )
{
   Name_t Name = (Name_t) Malloc( sizeof( Name[0] ));
   memset((void *) Name, 0, sizeof( Name[0] ));
   Name->iCheck = NAME_CHECK;

   if( pcName == 0 )
   {
      fprintf( stderr, "Error by NameCreate\n" );
      fflush( stderr );
      Name->pcName = 0;
   }
   else
   {
      Name->pcName = SN_StrDup( pcName );
#ifdef TRACE
      printf( "NameCreate: %d %s Name: (%d)\n", Name->pcName, Name->pcName, Name );
#endif
   }

   return Name;
}

extern Name_t f_NameDuplicate( Name_t Name )
{
   Name_t NameDuplicate;

   if( Name == 0 )
   {
      return 0;
   }

   if( Name->iCheck != NAME_CHECK ) Abort();

   NameDuplicate = f_NameCreate( Name->pcName );

   return NameDuplicate;
}

extern void f_NameDestroy( Name_t Name )
{
   if( Name )
   {
      if( Name->iCheck != NAME_CHECK ) Abort();

      if( Name->pcName )
	  {
         ckfree( (char*)Name->pcName );
		 Name->pcName = NULL;
	  }
	  /* Mark it as unused!  Zsolt Koppany, 16-jan-98 */
	  Name->iCheck = 0;

      ckfree( (char*)Name );
   }
}

extern void f_NameCat( Name_t Name1, Name_t Name2 )
{
   char *pcName = Malloc( strlen( Name1->pcName )
                        + strlen( Name2->pcName )
                        + 1 );
   strcpy( pcName, Name1->pcName );
   strcat( pcName, Name2->pcName );
   ckfree( (char*)Name1->pcName );
   Name1->pcName = pcName;
   f_NameDestroy( Name2 );
}

extern Name_t f_Dname( void )
{
   Name_t Name1;
   Name_t Name2;
   int iva1;
   int iva2;
   Save();
   niveau++;

   if( test ) printf( "f_Dname: %s\n", ident( 0 ));

   Name1 = f_ClassName();
   iva1 = iva;

   Restore();

   Name2 = f_Name();
   iva2 = iva;

   if( iva2 >= iva1 )
   {
      f_NameDestroy( Name1 );
      niveau--;
      iva = iva2;
      return Name2;
   }
   else
   {
      f_NameDestroy( Name2 );
      niveau--;
      iva = iva1;
      return Name1;
   }
}

extern Name_t f_Name( void )
{
   Name_t Name;
   Name_t Name1;
   Save();
   niveau++;

   if( test ) printf( "f_Name: %s\n", ident( 0 ));

   switch( token( 0 ))
   {
   case SN_IDENTIFIER :
      if(( Name = f_QualifiedName()))
      {
         niveau--;
         if( test ) printf( "return OK\n" );
         return Name;
      }
      /* itt szandekosan nincs break ! (rigo) */

   case SN_ASM        :
   case SN_CATCH      :
   case SN_NAMESPACE  :
/* case SN_DELETE     : ez sajnos elrontja a deallocation expression-t  */
/* case SN_NEW        : ez sajnos elrontja az allocation expression-t   */
   case SN_OVERLOAD   :
   case SN_TEMPLATE   :
   case SN_THIS       :
   case SN_THROW      :
   case SN_TRY        :
   case SN_USING      :
      Name = f_NameCreate( ident( 0 ));
      step( 1 );
      niveau--;
      if( test ) printf( "return OK\n" );
      return Name;

   case SN_OPERATOR  :
      if(( Name = f_OperatorOrConversionFunctionName()))
      {
         niveau--;
         if( test ) printf( "return OK\n" );
         return Name;
      }
      else
      {
         Restore();
         niveau--;
         if( test ) printf( "return 0\n" );
         return 0;
      }

   case '~'       :
      step( 1 );
      if(( Name1 = f_ClassName()))
      {
         Name = f_NameCreate( "~" );
         f_NameCat( Name, Name1 );
         niveau--;
         if( test ) printf( "return OK\n" );
         return Name;
      }
      else
      {
         Restore();
         niveau--;
         if( test ) printf( "return 0\n" );
         return 0;
      }
   default        :
      Restore();
      niveau--;
      if( test ) printf( "return 0\n" );
      return 0;
   }
}

extern Name_t f_OperatorOrConversionFunctionName( void )
{
   Name_t Name;
   Name_t Name1;
   Type_t Type;
   char *name;
   Save();
   niveau++;

   if( token( 0 ) != SN_OPERATOR )
   {
      Restore();
      niveau--;
      return 0;
   }

   step( 1 );

   switch( token( 0 ))
   {
   case '@'         : name = "operator@"       ; goto label;
   case '+'         : name = "operator+"       ; goto label;
   case '-'         : name = "operator-"       ; goto label;
   case '*'         : name = "operator*"       ; goto label;
   case '/'         : name = "operator/"       ; goto label;
   case '%'         : name = "operator%"       ; goto label;
   case '^'         : name = "operator^"       ; goto label;
   case '&'         : name = "operator&"       ; goto label;
   case '|'         : name = "operator|"       ; goto label;
   case '~'         : name = "operator~"       ; goto label;
   case '!'         : name = "operator!"       ; goto label;
   case '<'         : name = "operator<"       ; goto label;
   case '>'         : name = "operator>"       ; goto label;
   case SN_LS          : name = "operator<<"      ; goto label;
   case SN_RS          : name = "operator>>"      ; goto label;
   case SN_ANDAND      : name = "operator&&"      ; goto label;
   case SN_OROR        : name = "operator||"      ; goto label;
   case SN_ARROW       : name = "operator->"      ; goto label;
   case SN_ARROWstar   : name = "operator->*"     ; goto label;
   case '.'         : name = "operator."       ; goto label;
   case SN_DOTstar     : name = "operator.*"      ; goto label;
   case SN_ICR         : name = "operator++"      ; goto label;
   case SN_DECR        : name = "operator--"      ; goto label;
   case SN_LE          : name = "operator<="      ; goto label;
   case SN_GE          : name = "operator>="      ; goto label;
   case SN_EQ          : name = "operator=="      ; goto label;
   case SN_NE          : name = "operator!="      ; goto label;
   case '='         : name = "operator="       ; goto label;
   case SN_MULTassign  : name = "operator*="      ; goto label;
   case SN_DIVassign   : name = "operator/="      ; goto label;
   case SN_MODassign   : name = "operator%="      ; goto label;
   case SN_PLUSassign  : name = "operator+="      ; goto label;
   case SN_MINUSassign : name = "operator-="      ; goto label;
   case SN_LSassign    : name = "operator<<="     ; goto label;
   case SN_RSassign    : name = "operator>>="     ; goto label;
   case SN_ANDassign   : name = "operator&="      ; goto label;
   case SN_ERassign    : name = "operator^="      ; goto label;
   case SN_ORassign    : name = "operator|="      ; goto label;
   case SN_NEW         : name = "operator new"    ; goto label;
   case SN_DELETE      : name = "operator delete" ; goto label;
   case ','         : name = "operator,"       ; goto label;
label:
      step( 1 );
      niveau--;
      return f_NameCreate( name );

   case '('         :
      if( token( 1 ) == ')' )
      {
         step( 2 );
         niveau--;
         return f_NameCreate( "operator()" );
      }
      else
      {
         Restore();
         niveau--;
         return 0;
      }
      
   case '['         :
      if( token( 1 ) == ']' )
      {
         step( 2 );
         niveau--;
         return f_NameCreate( "operator[]" );
      }
      else
      {
         Restore();
         niveau--;
         return 0;
      }
      
   default          :
      if(( Type = f_ConversionTypeName()))
      {
         char ac[10000];   /* old: 1000 */
         f_TypeToString( Type, ac, 1 );
         Name  = f_NameCreate( "operator" );
         Name1 = f_NameCreate( ac );
         f_NameCat( Name, Name1 );
         f_TypeDestroy( Type );
         niveau--;
         return Name;
      }
      else
      {
         Restore();
         niveau--;
         return 0;
      }
   }
}

extern Name_t f_CompleteClassName( void )
{
   Name_t Name;
   Save();
   Boolean_t bIsCLCL;
   niveau++;

   if( token( 0 ) == SN_CLCL )
   {
      bIsCLCL = True;
      step( 1 );
   }
   else
   {
      bIsCLCL = False;
   }

   if(( Name = f_QualifiedClassName()))
   {
      if( bIsCLCL )
      {
         char *pcName = Malloc( strlen( Name->pcName ) + 3 );
         strcpy( pcName, "::" );
         strcat( pcName, Name->pcName );
         ckfree( (char*)Name->pcName );
         Name->pcName = pcName;
      }
      niveau--;
      return Name;
   }
   else
   {
      Restore();
      niveau--;
      return 0;
   }
}
      
extern Name_t f_QualifiedClassName( void )
{
   Name_t Name;
   Name_t Name1;
   Save();
   niveau++;

   if(( Name = f_ClassName()))
   {
      if( token( 0 ) == SN_CLCL )
      {
         step( 1 );
         if( Name1 = f_QualifiedClassName())
         {
            char *pcName = Malloc( strlen( Name->pcName )
                                 + 2
                                 + strlen( Name1->pcName ) 
                                 + 1 );
            strcpy( pcName, Name->pcName );
            strcat( pcName, "::" );
            strcat( pcName, Name1->pcName );
            ckfree( (char*)Name->pcName );
            Name->pcName = pcName;
            f_NameDestroy( Name1 );
            niveau--;
            return Name;
         }
         else
         {
            step( -1 );
            niveau--;
            return Name;
         }
      }
      else
      {
         niveau--;
         return Name;
      }
   }
   else
   {
      Restore();
      niveau--;
      return 0;
   }
}

extern Name_t f_NamespaceName( void )
{
   char ac[10000];   /* old: 1000 */
   Save();
   
   ac[0] = 0;

   if( token( 0 ) == SN_CLCL )
   {
      strcat( ac, "::" );
      step( 1 );
   }

   if( token( 0 ) == SN_IDENTIFIER )
   {
      strcat( ac, ident( 0 ));
      step( 1 );
   }
   else
   {
      Restore();
      return 0;
   }

   while( True )
   {
/*    Save(); */

      if( token( 0 ) == SN_CLCL
       && token( 1 ) == SN_IDENTIFIER )
      {
         strcat( ac, "::" );
         strcat( ac, ident( 1 ));
         step( 2 );
      }
      else
      {
         return f_NameCreate( ac );
      }
   }
}

extern Name_t f_QualifiedName( void )
{
   Name_t Name;
   Name_t Name1;
   Save();
   niveau++;

   if( test ) printf( "f_QualifiedName: %s\n", ident( 0 ));

   if(( Name = f_ClassName()))
   {
      if( token( 0 ) != SN_CLCL )
      {
         f_NameDestroy( Name );
         Restore();
         niveau--;
         if( test ) printf( "f_QualifiedName: return 0\n" );
         return 0;
      }
      step( 1 );
      
      if(( Name1 = f_Name()))
      {
         f_NameCat( Name, f_NameCreate( "::" ));
         f_NameCat( Name, Name1 );
         niveau--;
         if( test ) printf( "f_QualifiedName: return OK\n" );
         return Name;
      }
      else
      {
         f_NameDestroy( Name );
         Restore();
         niveau--;
         if( test ) printf( "f_QualifiedName: return 0\n" );
         return 0;
      }
   }
   else
   {
      Restore();
      niveau--;
      if( test ) printf( "f_QualifiedName: return 0\n" );
      return 0;
   }
}

extern Name_t f_ClassName( void )
{
   char *pname;
   char ac[10000];   /* old: 1000 */
   Save();
   niveau++;

   if( token( 0 ) == SN_IDENTIFIER )
   {
      pname = ident( 0 );
      step( 1 );
      if( token( 0 ) == '<' )
      {
         strcpy( ac, pname );
         pname = ac;
         f_TemplateArgList( pname );   /* optionelle template_arg_list */
      }
      niveau--;
      return f_NameCreate( pname );
   }
   else
   {
      Restore();
      niveau--;
      return 0;
   }
}

extern Name_t f_TemplateArgumentList( void )
{
   int i;
   char *pcIdent;
   char aname[10000];   /* old: 1000 */
   char *pname = aname;

   niveau++;

   step( 1 );
   i = 1;

   strcpy( pname, "<" );

   while( True )
   {
      switch( token( 0 ))
      {
         case '<': i++; break;
         case '>': i--; break;
         case  0 :
            niveau--;
            goto end;
      }

      if( i == 0 )
      {
         break;
      }

      pcIdent = ident( 0 );

      f_Strcat( pname, pcIdent );
      step( 1 );
   }

end:
   strcat( pname, ">" );

   step( 1 );
   niveau--;
   return f_NameCreate( aname );
}

static Boolean_t f_TemplateArgList( char *pname )
{
   int iva_from;
   Type_t Type;
   Expr_t Expr;
   Save();
   niveau++;

   step( 1 );

   iva_from = iva;
   pname += strlen( pname );
   *pname++ = '<';
   *pname = 0;

   while( True )
   {
      if(( Type = f_TypeName( ",>" )))
      {
         f_TypeDestroy( Type );
      }
      else
      {
         if(( Expr = f_Expression()))
         {
            f_ExprDestroy( Expr );
         }
         else
         {
            niveau--;
            Restore();
            return False;
         }
      }
      
      switch( token( 0 ))
      {
         case '>':
            concat_token( pname, iva_from );
            strcat( pname, ">" );
            step( 1 );
            niveau--;
            return True;

         case ',':
            step( 1 );
            break;

         case 0:
            niveau--;
            Restore();
            return False;
      }
   }
}

extern Name_t f_NameDup( Name_t Name )
{
   Name_t NameDup = 0;

   if( Name )
   {
      NameDup = f_NameCreate( Name->pcName );
   }

   return NameDup;
}


