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

static List_t f_InitList( char cTerminator );

extern Init_t f_Init( char cTerminator );
extern Init_t f_InitCreate( void );
extern void f_InitDestroy( Init_t Init );

extern Init_t f_Init( char cTerminator )
{
   Init_t Init;
   List_t ListInit;
   Expr_t Expr;
   Save();
   niveau++;

   if( token( 0 ) == '{' )
   {
      step( 1 );

      ListInit = f_InitList( '}' );

      if( token( 0 ) == '}' )
      {
         step( 1 );
      }

      Init = f_InitCreate();
      Init->ListInit = ListInit;
      niveau--;
      return Init;
   }
   else if(( Expr = f_AssignmentExpression()))
   {
      Init = f_InitCreate();
      Init->Expr = Expr;
      niveau--;
      return Init;
   }
   else
   {
      f_StepTo( ',', ';', cTerminator, 0 );
      Restore();
      niveau--;
      return 0;
   }
}

static List_t f_InitList( char cTerminator )
{
   Init_t Init;
   List_t List = 0;
/* Save(); */
   niveau++;

   while( True )
   {
      if(( Init = f_Init( cTerminator )))
      {
         f_ListAddLast( &List, (Elem_t) Init );
      }
      else
      {
         f_StepTo( ',', (char) cTerminator, ';', 0 );
      }

      if( token( 0 ) == ',' )
      {
         step( 1 );
      }

      if( token( 0 ) == cTerminator || token( 0 ) == ';' || token( 0 ) == 0 )
      {
         niveau--;
         return List;
      }
   }
}

extern Init_t f_InitCreate( void )
{
   Init_t Init = (Init_t) Malloc( sizeof( Init[0] ));
   memset((void*) Init, 0, sizeof( Init[0] ));
   Init->iCheck = INIT_CHECK;
   return Init;
}

extern void f_InitDestroy( Init_t Init )
{
   if( Init )
   {
      if( Init->iCheck != INIT_CHECK ) Abort();
      if( Init->Expr )
         f_ExprDestroy( Init->Expr );
      if( Init->ListInit )
         f_ListDestroy( Init->ListInit, (void(*)()) f_InitDestroy );
      ckfree( (char*)Init );
   }
}

extern Init_t f_NewInitializer( void )
{
   Init_t Init;
   Save();

   if( token( 0 ) == '(' )
   {
      step( 1 );
      Init = f_InitCreate();
      Init->ListInit = f_InitList( ')' );
      f_StepTo( ')', 0 );
      step( 1 );
      return Init;
   }
   else
   {
      Restore();
      return 0;
   }
}

extern void f_InitProcess( Init_t Init )
{
   Init_t Init2;

   if( Init->Expr ) f_ExprProcess( Init->Expr );

   for( Init2 = (Init_t) d_ElemFirst( Init->ListInit )
      ; Init2
      ; Init2 = Init2->InitNext
      )
   {
      f_InitProcess( Init2 );
   }
}


