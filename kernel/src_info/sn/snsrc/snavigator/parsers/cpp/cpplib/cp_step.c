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
#include <stdarg.h>
#include "crossrefP.h"

#ifdef TEST
main()
{
   f_StepTo( 'a', 0 );
   f_StepTo( 'a', 'b', 0 );
   f_StepTo( 'a', 'b', 'c', 0 );
}
#endif

extern void f_StepTo( int iToken, ... )
{
   int aiToken[10];
   int i = 0;
   va_list pvar;

   aiToken[i++] = iToken;
   va_start( pvar, iToken );

   while( True )
   {
      if(( aiToken[i++] = va_arg( pvar, int )) == 0 )
      {
         break;
      }
   }

/*    printf( "StepTo:" ); */
/*    for( i = 0; aiToken[i]; i++ ) */
/*    { */
/*       printf( " %c", aiToken[i] ); */
/*    } */
/*    printf( "\n" ); */

   va_end( pvar );

#ifndef TEST
   while( True )
   {
      iToken = token( 0 );

      for( i = 0; aiToken[i]; i++ )
      {
         if( aiToken[i] == iToken ) 
         {
            return;
         }
      }

      switch( iToken )
      {
      case 0  : return;
      case '(': step( 1 ); f_StepTo( ')', 0 ); step( 1 ); break;
      case '[': step( 1 ); f_StepTo( ']', 0 ); step( 1 ); break;
      case '{': step( 1 ); f_StepTo( '}', 0 ); step( 1 ); break;
      case '<': step( 1 ); f_StepTo( '>', 0 ); step( 1 ); break;
      default :                                step( 1 ); break;
      }
   }
#endif
}


