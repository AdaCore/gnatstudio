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

#define PRINT

#ifdef CORVEX
#undef stderr
#define stderr stdout
#endif

extern void f_SyntaxError( int iError )
{
#ifdef PRINT
   fprintf( stderr, "file: %s line: %d.%d syntax error: %d\n"
         , filename_g
         , f_lineno( 0 )
         , f_charno( 0 )
         , iError );
   fflush( stderr );
#endif
}

extern void f_InternalError( int iError )
{
#ifdef PRINT
   fprintf( stderr, "file: %s line: %d.%d internal error: %d\n"
         , filename_g
         , f_lineno( 0 )
         , f_charno( 0 )
         , iError );
   fflush( stderr );
#ifdef CORVEX
   exit( -1 );
#endif
#endif
}

extern void f_FatalError( int iError )
{
   fprintf( stderr, "file: %s line: %d.%d fatal error: %d\n"
         , filename_g
         , f_lineno( 0 )
         , f_charno( 0 )
         , iError );
   fflush( stderr );
   exit( -1 );
}

extern void Abort( void )
{
   fprintf( stderr, "file: %s line: %d.%d abort\n"
         , filename_g
         , f_lineno( 0 )
         , f_charno( 0 ));
   fflush( stderr );
   (*((int*)0)) = 0;
}


