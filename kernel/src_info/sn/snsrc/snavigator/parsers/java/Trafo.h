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

#ifndef yyTrafo
#define yyTrafo

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

#ifndef rbool
#define rbool char
#endif

#include "Tree.h"


extern void (* Trafo_Exit) ARGS ((void));

extern void to_types ARGS ((tTree yyP2, tString yyP1));
extern void to_names ARGS ((tTree yyP5, tString yyP4));
extern void to_files ARGS ((tTree yyP8, tString yyP7));
extern tTree get_objects ARGS ((tTree t, tTree o));
extern void Traverse ARGS ((tTree yyP16));

extern void BeginTrafo ARGS ((void));
extern void CloseTrafo ARGS ((void));

#endif

