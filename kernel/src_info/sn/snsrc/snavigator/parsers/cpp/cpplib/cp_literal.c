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

extern Boolean_t f_IsLiteral( int iToken )
{
   switch( iToken )
   {
   case SN_IDENTIFIER:
   case SN_ASM       :
   case SN_DO        :
   case SN_NEW       :
   case SN_CHAR      :
   case SN_IF        :
   case SN_INT       :
   case SN_CATCH     :
   case SN_ENUM      :
   case SN_TRY       :
   case SN_FOR       :
   case SN_CASE      :
   case SN_UNION     :
   case SN_LONG      :
   case SN_VOID      :
   case SN_NAMESPACE :
   case SN_CONTINUE  :
   case SN_AUTO      :
   case SN_CONST     :
   case SN_THROW     :
   case SN_GOTO      :
   case SN_THIS      :
   case SN_PUBLIC    :
   case SN_CLASS     :
   case SN_BREAK     :
   case SN_WHILE     :
   case SN_PRIVATE   :
   case SN_USING     :
   case SN_SWITCH    :
   case SN_TEMPLATE  :
   case SN_UNSIGNED  :
   case SN_PROTECTED :
   case SN_INLINE    :
   case SN_SHORT     :
   case SN_OPERATOR  :
   case SN_FLOAT     :
   case SN_ELSE      :
   case SN_STATIC    :
   case SN_VIRTUAL   :
   case SN_VOLATILE  :
   case SN_FRIEND    :
   case SN_RETURN    :
   case SN_SIGNED    :
   case SN_OVERLOAD  :
   case SN_DOUBLE    :
   case SN_BOOL      :
   case SN_STRUCT    :
   case SN_TYPEDEF   :
   case SN_EXTERN    :
   case SN_DEFAULT   :
   case SN_REGISTER  :
   case SN_SIZEOF    :
   case SN_DELETE    : return True;
   default        : return False;
   }
}

extern Boolean_t f_IsCppLiteral( int iToken )
{
   switch( iToken )
   {
   case SN_IDENTIFIER:
   case SN_ASM       :
   case SN_NEW       :
   case SN_CATCH     :
   case SN_TRY       :
   case SN_NAMESPACE :
   case SN_THROW     :
   case SN_THIS      :
   case SN_PUBLIC    :
   case SN_CLASS     :
   case SN_PRIVATE   :
   case SN_USING     :
   case SN_TEMPLATE  :
   case SN_PROTECTED :
   case SN_INLINE    :
   case SN_OPERATOR  :
   case SN_VIRTUAL   :
   case SN_VOLATILE  :
   case SN_FRIEND    :
   case SN_OVERLOAD  :
   case SN_DELETE    : return True;
   default        : return False;
   }
}


