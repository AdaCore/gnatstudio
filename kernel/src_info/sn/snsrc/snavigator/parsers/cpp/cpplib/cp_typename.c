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

extern Type_t f_TypeName( char *pcTerminator )
{
   Type_t Type = f_TypeCreate();
   int iTypeSpec = 0;
   Save();
   niveau++;

/*    printf( "TypeName\n" ); */

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         iTypeSpec++;
         if(( Type->Class = f_Class()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
            return 0;
         }
         break;

      case SN_ENUM       :
         iTypeSpec++;
         if(( Type->Enum = f_Enum()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
            return 0;
         }
         break;

      case SN_CHAR       : Type->s_char     = True; goto type;
      case SN_SHORT      : Type->s_short    = True; goto type;
      case SN_INT        : Type->s_int      = True; goto type;
      case SN_LONG       : Type->s_long     = True; goto type;
      case SN_SIGNED     : Type->s_signed   = True; goto type;
      case SN_UNSIGNED   : Type->s_unsigned = True; goto type;
      case SN_FLOAT      : Type->s_float    = True; goto type;
      case SN_DOUBLE     : Type->s_double   = True; goto type;
      case SN_BOOL       : Type->s_bool     = True; goto type;
      case SN_VOID       : Type->s_void     = True; goto type;
      case SN_CONST      : Type->s_const    = True; goto type;
      case SN_VOLATILE   : Type->s_volatile = True; goto type;
type:
         iTypeSpec++;
         step( 1 );
         break;

      default:
         if( iTypeSpec == 0 )
         {
            if(( Type->Name = f_CompleteClassName()))
            {
               iTypeSpec++;
               break;
            }
            else
            {
               f_TypeDestroy( Type );
               niveau--;
               Restore();
               return 0;
            }
         }
         else
         {
            if(( Type->Declarator = f_AbstractDeclarator()))
            {
               niveau--;
               return Type;
            }

            if( Type->Name == 0 )
            {
               if(( Type->Name = f_CompleteClassName()))
               {
                  iTypeSpec++;
                  break;
               }
            }

/* ha a kovetkezo token identifier, akkor nem ures abstract declaratorral van
** dolgunk, hanem atlependo identifier-ral ( peldaul: far )
*/
            if( token( 0 ) == SN_IDENTIFIER )   /* 22.02.97 rigo */
            {
               step( 1 );
               break;
            }
            
            niveau--;
            return Type;   /* empty abstract_declarator */
         }
      }
   }
}

extern Type_t f_NewTypeName( void )
{
   Type_t Type = f_TypeCreate();
   int iTypeSpec = 0;
   Save();
   niveau++;

/*    printf( "NewTypeName\n" ); */

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         iTypeSpec++;
         if(( Type->Class = f_Class()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
/*             printf( "NewTypeName: 0: bad Class\n" ); */
            return 0;
         }
         break;

      case SN_ENUM       :
         iTypeSpec++;
         if(( Type->Enum = f_Enum()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
/*             printf( "NewTypeName: 0: bad Enum\n" ); */
            return 0;
         }
         break;

      case SN_CHAR       : Type->s_char     = True; goto type;
      case SN_SHORT      : Type->s_short    = True; goto type;
      case SN_INT        : Type->s_int      = True; goto type;
      case SN_LONG       : Type->s_long     = True; goto type;
      case SN_SIGNED     : Type->s_signed   = True; goto type;
      case SN_UNSIGNED   : Type->s_unsigned = True; goto type;
      case SN_FLOAT      : Type->s_float    = True; goto type;
      case SN_DOUBLE     : Type->s_double   = True; goto type;
      case SN_BOOL       : Type->s_bool     = True; goto type;
      case SN_VOID       : Type->s_void     = True; goto type;
      case SN_CONST      : Type->s_const    = True; goto type;
      case SN_VOLATILE   : Type->s_volatile = True; goto type;
type:
         iTypeSpec++;
         step( 1 );
         break;

      default:
         if( iTypeSpec == 0 )
         {
            if(( Type->Name = f_CompleteClassName()))
            {
/*                printf( "NewTypeName: OK CompleteClassName: %s\n", ident( 0 )); */
               iTypeSpec++;
               break;
            }
            else
            {
               f_TypeDestroy( Type );
               niveau--;
               Restore();
/*                printf( "NewTypeName: 0: bad CompleteClassName\n" ); */
               return 0;
            }
         }
         else
         {
/*             printf( "NewTypeName: vor New declarator: %s\n", ident( 0 )); */
            if(( Type->Declarator = f_NewDeclarator()))
            {
/*                printf( "NewTypeName: OK: NewDeclarator: %s\n", ident( 0 )); */
               niveau--;
               return Type;
            }

            if( Type->Name == 0 )
            {
               if(( Type->Name = f_CompleteClassName()))
               {
                  iTypeSpec++;
                  break;
               }
            }

            niveau--;
/*             printf( "NewTypeName: OK: empty NewDeclarator: %s\n", ident( 0 )); */
            return Type;   /* empty new_declarator */
         }
      }
   }
}


extern Type_t f_ConversionTypeName( void )
{
   Type_t Type = f_TypeCreate();
   int iTypeSpec = 0;
   Save();
   niveau++;

/*    printf( "ConversionTypeName\n" ); */

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         iTypeSpec++;
         if(( Type->Class = f_Class()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
            return 0;
         }
         break;

      case SN_ENUM       :
         iTypeSpec++;
         if(( Type->Enum = f_Enum()) == 0 )
         {
            f_TypeDestroy( Type );
            niveau--;
            Restore();
            return 0;
         }
         break;

      case SN_CHAR       : Type->s_char     = True; goto type;
      case SN_SHORT      : Type->s_short    = True; goto type;
      case SN_INT        : Type->s_int      = True; goto type;
      case SN_LONG       : Type->s_long     = True; goto type;
      case SN_SIGNED     : Type->s_signed   = True; goto type;
      case SN_UNSIGNED   : Type->s_unsigned = True; goto type;
      case SN_FLOAT      : Type->s_float    = True; goto type;
      case SN_DOUBLE     : Type->s_double   = True; goto type;
      case SN_BOOL       : Type->s_bool     = True; goto type;
      case SN_VOID       : Type->s_void     = True; goto type;
      case SN_CONST      : Type->s_const    = True; goto type;
      case SN_VOLATILE   : Type->s_volatile = True; goto type;
type:
         iTypeSpec++;
         step( 1 );
         break;

      default:
         if( iTypeSpec == 0 )
         {
            if(( Type->Name = f_CompleteClassName()))
            {
               iTypeSpec++;
               break;
            }
            else
            {
               f_TypeDestroy( Type );
               niveau--;
               Restore();
               return 0;
            }
         }
         else
         {
            if(( Type->Declarator = f_ConversionDeclarator()))
            {
               niveau--;
               return Type;
            }

            if( Type->Name == 0 )
            {
               if(( Type->Name = f_CompleteClassName()))
               {
                  iTypeSpec++;
                  break;
               }
            }

            niveau--;
            return Type;   /* empty conversion_declarator */
         }
      }
   }
}



