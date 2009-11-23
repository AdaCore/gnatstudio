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

extern List_t f_ListCreate( void )
{
   List_t List = (List_t) Malloc( sizeof( List[0] ));
   memset((void *) List, 0, sizeof( List[0] ));
   List->iCheck = LIST_CHECK;
   return List;
}

extern List_t f_ListDuplicate( List_t List, Elem_t (*pfElemDuplicate)(Elem_t))
{
   List_t ListDuplicate;
   Elem_t Elem;
   Elem_t ElemDuplicate;

   if( List == 0 )
   {
      return 0;
   }

   if( List->iCheck != LIST_CHECK ) Abort();

   ListDuplicate = f_ListCreate();

   for( Elem = d_ElemFirst( List )
      ; Elem
      ; Elem = Elem->ElemNext
      )
   {
      ElemDuplicate = (*pfElemDuplicate)( Elem );
      f_ListAddLast( &ListDuplicate, ElemDuplicate );
   }

   return ListDuplicate;
}

extern void f_ListDestroy( List_t List, void (*pfDestroy)( Elem_t ))
{
   Elem_t Elem;
   Elem_t ElemNext;

   if( List )
   {
      if( List->iCheck != LIST_CHECK ) Abort();

      for( Elem = List->ElemFirst; Elem; Elem = ElemNext )
      {
         ElemNext = Elem->ElemNext;
         (*pfDestroy)( Elem );
      }
      ckfree( (char*)List );
   }
}

extern void f_ListAddLast( List_t *pList, Elem_t Elem )
{
   if( Elem == 0 )
   {
      return;
   }

   if((*pList) == 0 )
   {
      (*pList) = f_ListCreate();
   }

   if((*pList)->ElemLast == 0 )
   {
      (*pList)->ElemFirst = Elem;
      (*pList)->ElemLast  = Elem;
      Elem->ElemNext = 0;
   }
   else
   {
      (*pList)->ElemLast->ElemNext = Elem;
      (*pList)->ElemLast = Elem;
      Elem->ElemNext = 0;
   }
}

extern void f_ListAddFirst( List_t *pList, Elem_t Elem )
{
   if( Elem == 0 )
   {
      return;
   }

   if((*pList) == 0 )
   {
      (*pList) = f_ListCreate();
   }

   if((*pList)->ElemFirst == 0 )
   {
      (*pList)->ElemFirst = Elem;
      (*pList)->ElemLast  = Elem;
      Elem->ElemNext = 0;
   }
   else
   {
      Elem->ElemNext = (*pList)->ElemFirst;
      (*pList)->ElemFirst = Elem;
   }
}

extern Elem_t f_ListRemoveFirst( List_t List )
{
   Elem_t Elem;

   if( List == 0 || List->ElemFirst == 0 )
   {
      return 0;
   }
   else
   {
      Elem = List->ElemFirst;
      if(( List->ElemFirst = Elem->ElemNext ) == 0 )
      {
         List->ElemLast = 0;
      }
      Elem->ElemNext = 0;
      return Elem;
   }
}

extern void f_ListConcat( List_t *pList1, List_t List2 )
{
   if((*pList1) == 0 )
   {
      (*pList1) = List2;
   }
   else
   {
      if((*pList1)->ElemLast == 0 )
      {
         if( List2 )
         {
            (*pList1)->ElemFirst = List2->ElemFirst;
            (*pList1)->ElemLast  = List2->ElemLast ;
            ckfree( (char*)List2 );
         }
      }
      else
      {
         if( List2 && List2->ElemFirst )
         {
            (*pList1)->ElemLast->ElemNext = List2->ElemFirst;
            (*pList1)->ElemLast           = List2->ElemLast;
         }
         if( List2 )
         {
            ckfree( (char*)List2 );
         }
      }
   }
}

extern Elem_t f_ListLastElem( List_t List )
{
   if( List == 0 )
   {
      return 0;
   }
   else
   {
      return List->ElemLast;
   }
}



