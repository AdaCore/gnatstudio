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

extern Class_t f_Class( void );
extern Class_t f_ClassCreate( void );
extern void f_ClassDestroy( Class_t Class );
extern Base_t f_BaseCreate( void );
extern void f_BaseDestroy( Base_t Base );
extern Member_t f_MemberCreate( void );
extern void f_MemberDestroy( Member_t Member );

static List_t f_BaseList( Class_t Class );
static Base_t f_Base( Class_t Class );
static List_t f_MemberList( Class_t Class );
static Member_t f_Member( Class_t Class, int *piAccess );

extern Class_t f_Class( void )
{
   Class_t Class;
   Save();

   niveau++;

   Class = f_ClassCreate();

   switch( token( 0 ))
   {
   case SN_CLASS:
      Class->iAccess = SN_PRIVATE;
      Class->iType   = SN_CLASS;
      break;
   case SN_STRUCT:
      Class->iAccess = SN_PUBLIC;
      Class->iType   = SN_STRUCT;
      break;
   case SN_UNION:
      Class->iAccess = SN_PUBLIC;
      Class->iType   = SN_UNION;
      break;
   default:
      f_ClassDestroy( Class );
      Restore();
      niveau--;
      return 0;
   }

   step( 1 );

   if( token( 0 ) == SN_IDENTIFIER )
   {
      /* a class name modifier atlepese */
      if( token( 1 ) == SN_IDENTIFIER &&
        ( token( 2 ) == ':' || token( 2 ) == LBRACE || token( 2 ) == '<' ))
      {
         printf( "class name modifier: %s %s (%s:%d.%d)\n"
               , ident( 0 )
               , ident( 1 )
               , filename_g
               , f_lineno( 0 )
               , f_charno( 0 )
               );
         step( 1 );
      }

      Class->Name = f_ClassName();
   }

   if( token( 0 ) == ':' )
   {
      step( 1 );
      Class->ListBase = f_BaseList( Class );
      f_StepTo( LBRACE, 0 );
   }

   if( token( 0 ) == LBRACE )
   {
      Class->ListMember = f_MemberList( Class );
   }

   niveau--;
   return Class;
}

static List_t f_BaseList( Class_t Class )
{
   List_t List;
   Base_t Base;
/* Save(); */
   niveau++;

   List = f_ListCreate();

   while( True )
   {
      if(( Base = f_Base( Class )))
      {
         f_ListAddLast( &List, (void*) Base );
      }
process:
      if( token( 0 ) == ',' )
      {
         step( 1 );
      }
      else if( token( 0 ) == LBRACE || token( 0 ) == 0 )
      {
         break;
      }
      else
      {
         f_StepTo( ',', '{', 0 );
         goto process;
      }
   }

   niveau--;
   return List;
}

static Base_t f_Base( Class_t Class )
{
   Base_t Base;
   niveau++;

   Base = f_BaseCreate();
   Base->iAccess = Class->iAccess;

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_VIRTUAL   :  Base->bVirtual   = True     ; step( 1 ); break;
      case SN_PRIVATE   :  Base->iAccess    = SN_PRIVATE  ; step( 1 ); break;
      case SN_PROTECTED :  Base->iAccess    = SN_PROTECTED; step( 1 ); break;
      case SN_PUBLIC    :  Base->iAccess    = SN_PUBLIC   ; step( 1 ); break;
      case SN_CLCL      :
      case SN_IDENTIFIER:
         if( token( 0 ) == SN_IDENTIFIER &&
           ( token( 1 ) == SN_IDENTIFIER ||
             token( 1 ) == SN_VIRTUAL    ||
             token( 1 ) == SN_PRIVATE    ||
             token( 1 ) == SN_PROTECTED  ||
             token( 1 ) == SN_PUBLIC     ))
         {  /* struct x : SN_PRIVATE y { ... } miatt */
            step( 1 );
         }
         else if(( Base->Name = f_CompleteClassName()))
         {
            niveau--;
            return Base;
         }
         else
         {
            f_BaseDestroy( Base );
            niveau--;
            return 0;
         }

      default:
         f_BaseDestroy( Base );
         niveau--;
         return 0;
      }
   }
}

static List_t f_MemberList( Class_t Class )
{
   List_t List;
   Member_t Member;
   int iAccess = Class->iAccess;
   niveau++;
   step( 1 );

   List = f_ListCreate();

   while( True )
   {
      if(( Member = f_Member( Class, &iAccess )))
      {
         f_ListAddLast( &List, (void*) Member );
      }

      if( token( 0 ) == RBRACE || token( 0 ) == 0 )
      {
         step( 1 );
         niveau--;
         return List;
      }
   }
}

static Member_t f_Member( Class_t Class, int *piAccess )
{
   Member_t Member;
   Declaration_t Declaration;
   niveau++;

   switch( token( 0 ))
   {
   case RBRACE      :
      niveau--;
      return 0;

   case SN_PRIVATE     :
   case SN_PUBLIC      :
   case SN_PROTECTED   :
      if( token( 1 ) == ':' )
      {
         *piAccess = token( 0 );
         step( 2 );
         niveau--;
         return 0;
      }
      else
      {
         step( 1 );
         niveau--;
         return 0;
      }

   default:
      if(( Declaration = f_MemberDeclaration()))
      {
         Member = f_MemberCreate();
         Member->Declaration = Declaration;
         Member->iAccess = *piAccess;
         niveau--;
         return Member;
      }
      else
      {
         int i = 0;
         while( True )
         {
            int t;

            switch( t = token( 0 ))
            {
            case LBRACE: i++; break;
            case RBRACE: i--; break;
            case  0 :
               niveau--;
               return 0;
            }

            if( i == 0 && t == ';' )
            {
               step( 1 );
               niveau--;
               return 0;
            }

            if( i < 0 )
            {
               niveau--;
               return 0;
            }

            step( 1 );
         }
      }
   }
}

extern Class_t f_ClassCreate( void )
{
   Class_t Class = (Class_t) Malloc( sizeof( Class[0] ));
   memset((void *) Class, 0, sizeof( Class[0] ));
   Class->iCheck = CLASS_CHECK;
   return Class;
}

extern void f_ClassDestroy( Class_t Class )
{
   if( Class )
   {
      if( Class->iCheck != CLASS_CHECK ) Abort();
      f_NameDestroy( Class->Name );
      f_ListDestroy( Class->ListBase, (void(*)()) f_BaseDestroy );
      f_ListDestroy( Class->ListMember, (void(*)()) f_MemberDestroy );
      ckfree( (char*)Class );
   }
}

extern Base_t f_BaseCreate( void )
{
   Base_t Base = (Base_t) Malloc( sizeof( Base[0] ));
   memset((void *) Base, 0, sizeof( Base[0] ));
   Base->iCheck = BASE_CHECK;
   return Base;
}

extern void f_BaseDestroy( Base_t Base )
{
   if( Base )
   {
      if( Base->iCheck != BASE_CHECK ) Abort();
      f_NameDestroy( Base->Name );
      ckfree( (char*)Base );
   }
}

extern Member_t f_MemberCreate( void )
{
   Member_t Member = (Member_t) Malloc( sizeof( Member[0] ));
   memset((void *) Member, 0, sizeof( Member[0] ));
   Member->iCheck = MEMBER_CHECK;
   return Member;
}

extern void f_MemberDestroy( Member_t Member )
{
   if( Member )
   {
      if( Member->iCheck != MEMBER_CHECK ) Abort();
      f_DeclarationDestroy( Member->Declaration );
      ckfree( (char*)Member );
   }
}

extern Class_t f_ClassPoorDup( Class_t Class )
{
   Class_t ClassDup = 0;

   if( Class )
   {
      ClassDup = f_ClassCreate();

      ClassDup->Name    = f_NameDup( Class->Name );
      ClassDup->iAccess = Class->iAccess;
      ClassDup->iType   = Class->iType;
   }

   return ClassDup;
}



