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

extern Oper_t f_OperCreate( void )
{
   Oper_t Oper = (Oper_t) Malloc( sizeof( Oper[0] ));
   memset((void *) Oper, 0, sizeof( Oper[0] ));
   Oper->iCheck = OPER_CHECK;
   return Oper;
}

extern Oper_t f_OperDuplicate( Oper_t Oper )
{
   Oper_t OperDuplicate;

   if( Oper == 0 )
   {
      return 0;
   }

   if( Oper->iCheck != OPER_CHECK ) Abort();

   OperDuplicate = f_OperCreate();

   OperDuplicate->type            = Oper->type;
   OperDuplicate->s_const         = Oper->s_const;
   OperDuplicate->s_volatile      = Oper->s_volatile;
   OperDuplicate->Name            = f_NameDuplicate( Oper->Name );
   OperDuplicate->ListDeclaration = f_ListDuplicate( Oper->ListDeclaration
                           , (Elem_t(*)(Elem_t)) f_DeclarationDuplicate );

   return OperDuplicate;
}

extern void f_OperDestroy( Oper_t Oper )
{
   if( Oper )
   {
      if( Oper->iCheck != OPER_CHECK ) Abort();
      if( Oper->Name )
         f_NameDestroy( Oper->Name );
      if( Oper->ListDeclaration )
         f_ListDestroy( Oper->ListDeclaration
                      , (void(*)()) f_DeclarationDestroy );
      if( Oper->Expr )
         f_ExprDestroy( Oper->Expr );
      ckfree( (char*)Oper );
   }
}

extern void f_OperProcess( Oper_t Oper )
{
   Declaration_t Declaration;
   int record;

   if( Oper->Expr ) f_ExprProcess( Oper->Expr );

   for( Declaration = (Declaration_t) d_ElemFirst( Oper->ListDeclaration )
      ; Declaration
      ; Declaration = Declaration->DeclarationNext
      )
   {
      f_DeclarationProcess( Declaration, record = False );
   }
}


