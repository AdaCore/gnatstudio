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

extern Enum_t f_Enum( void );
extern Enum_t f_EnumCreate( void );
extern void f_EnumDestroy( Enum_t Enum );
extern Enumerator_t f_EnumeratorCreate( void );
extern void f_EnumeratorDestroy( Enumerator_t Enumerator );

static List_t f_EnumeratorList( void );
static Enumerator_t f_Enumerator( void );

extern Enum_t f_Enum( void )
{
   Enum_t Enum;
   Save();
   niveau++;

   if( token( 0 ) != SN_ENUM )
   {
      niveau--;
      Restore();
      return 0;
   }

   Enum = f_EnumCreate();

   step( 1 );

   if( token( 0 ) != LBRACE ) /* SN_IDENTIFIER or SN_NEW or ... */
   {
      Enum->Name = f_NameCreate( ident( 0 ));
      step( 1 );
   }

   if( token( 0 ) == LBRACE )
   {
      step( 1 );
      Enum->ListEnumerator = f_EnumeratorList();
      f_StepTo( RBRACE, 0 );
      step( 1 );
   }

   niveau--;
   return Enum;
}

static List_t f_EnumeratorList( void )
{
   List_t List;
   Enumerator_t Enumerator;
   niveau++;
   step( 1 );

   List = f_ListCreate();

   while( True )
   {
      if(( Enumerator = f_Enumerator()))
      {
         f_ListAddLast( &List, (Elem_t) Enumerator );
      }
process:
      if( token( 0 ) == ',' )
      {
         step( 1 );
      }
      else if( token( 0 ) == RBRACE || token( 0 ) == 0 )
      {
         break;
      }
      else
      {
         f_StepTo( ',', '}', 0 );
         goto process;
      }
   }

   niveau--;
   return List;
}

static Enumerator_t f_Enumerator( void )
{
   Enumerator_t Enumerator;
   Save();
   niveau++;

   if( token( 0 ) == SN_IDENTIFIER )
   {
      Enumerator = f_EnumeratorCreate();

      Enumerator->Name = f_NameCreate( ident( 0 ));

      step( 1 );

      if( token( 0 ) == '=' )
      {
         step( 1 );
         Enumerator->Expr = f_ConstantExpression();
      }
      niveau--;
      return Enumerator;
   }
   else
   {
      Restore();
      niveau--;
      return 0;
   }
}

extern Enum_t f_EnumCreate( void )
{
   Enum_t Enum = (Enum_t) Malloc( sizeof( Enum[0] ));
   memset((void *) Enum, 0, sizeof( Enum[0] ));
   Enum->iCheck = ENUM_CHECK;
   return Enum;
}

extern void f_EnumDestroy( Enum_t Enum )
{
   if( Enum )
   {
      if( Enum->iCheck != ENUM_CHECK ) Abort();
      f_NameDestroy( Enum->Name );
      f_ListDestroy( Enum->ListEnumerator, (void(*)()) f_EnumeratorDestroy );
      ckfree( (char*)Enum );
   }
}

extern Enumerator_t f_EnumeratorCreate( void )
{
   Enumerator_t Enumerator = (Enumerator_t) Malloc( sizeof( Enumerator[0] ));
   memset((void *) Enumerator, 0, sizeof( Enumerator[0] ));
   Enumerator->iCheck = ENUMERATOR_CHECK;
   return Enumerator;
}

extern void f_EnumeratorDestroy( Enumerator_t Enumerator )
{
   if( Enumerator )
   {
      if( Enumerator->iCheck != ENUMERATOR_CHECK ) Abort();
      f_NameDestroy( Enumerator->Name );
      f_ExprDestroy( Enumerator->Expr );
      ckfree( (char*)Enumerator );
   }
}

extern Enum_t f_EnumPoorDup( Enum_t Enum )
{
   Enum_t EnumDup = 0;

   if( Enum )
   {
      EnumDup = f_EnumCreate();
      EnumDup->Name = f_NameDup( Enum->Name );
   }

   return EnumDup;
}



