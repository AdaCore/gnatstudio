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

#ifndef yydeftab
#define yydeftab

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#endif

#include "Tree.h"
#include "Position.h"

extern tTree nnoobject		;

extern void  init_deftab	ARGS ((void));

extern tTree IdentifyObjects	ARGS ((tIdent Ident, tTree Objects));
extern tTree IdentifyLocal	ARGS ((tIdent Ident, tTree Env));
extern tTree IdentifyWhole	ARGS ((tIdent Ident, tTree Env));
extern tTree IdentifyMethod	ARGS ((tIdent Ident, tTree Env, short no_of_args));
extern tTree IdentifyTail	ARGS ((tIdent Ident, tTree Env, tPosition Pos));
extern tTree IdentifyLocalKind	ARGS ((tIdent Ident, tTree Env, Tree_tKind Kind));
extern tTree IdentifyWholeKind	ARGS ((tIdent Ident, tTree Env, Tree_tKind Kind));
extern tTree IdentifyTailKind	ARGS ((tIdent Ident, tTree Env, tPosition Pos, Tree_tKind Kind));

extern tTree mEnv		ARGS ((tTree Objects, tTree Env, tTree Object));
extern void  WriteEnv		ARGS ((FILE *, tTree Env));

#endif

