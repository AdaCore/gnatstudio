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

#include "deftab.h"
#include "rMemory.h"
#include "Idents.h"

#define IsBefore(Pos1, Pos2) (Compare (Pos1, Pos2) < 0)

#ifndef Debug
#define WriteId(ident, object) object
#else
#define WriteId(ident, object) WriteResult (ident, object)
#endif

tTree nnoobject;

void init_deftab ARGS ((void))
{
   nnoobject = mnoobject ();
}

static tTree WriteResult
#if defined __STDC__ | defined __cplusplus
   (tIdent ident, tTree object)
#else
   (ident, object) tIdent ident; tTree object;
#endif
{
   (void) printf ("IDENTIFY ");
   WriteIdent (stdout, ident);
   if (object == nnoobject)
      (void) printf (" FAILS\n");
   else {
      tTree t = object->object.object;
      (void) printf (" AT ");
      switch (t->Kind) {
      case kclass	:
      case kinterface	: WritePosition (stdout, t->type_decl.pos); break;
      case kmethod	:
      case kconstructor	: WritePosition (stdout, t->function.pos); break;
      case kvariable	:
      case kparameter	: WritePosition (stdout, t->decl.pos); break;
      case klabeled_stmt: WritePosition (stdout, t->labeled_stmt.pos); break;
      default		: (void) printf ("?");
      }
      (void) printf ("\n");
   }
   return object;
}

/*
tTree IdentifyObjects
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Objects)
#else
   (Ident, Objects)
   register tIdent	Ident;
   register tTree	Objects;
#endif
{
   while (Objects != NoTree)
      if (Objects->object.ident == Ident) return WriteId (Ident, Objects);
      else Objects = Objects->object.next;
   return nnoobject;
}
*/

tTree IdentifyLocal
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env)
#else
   (Ident, Env)
   register tIdent	Ident;
   register tTree	Env;
#endif
{
   if (Env != NoTree) {
      if (Env->Kind == kenv) {
	 register tTree Object = Env->env.HashTable [Ident % Env->env.HashSize];
	 while (Object != NoTree)
	    if (Object->object.ident == Ident) return WriteId (Ident, Object);
	    else Object = Object->object.collision;
      } else {
	 return IdentifyWhole (Ident, Env->env2.env1);
      }
   }
   return nnoobject;
}

tTree IdentifyWhole
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env)
#else
   (Ident, Env)
   register tIdent	Ident;
   register tTree	Env;
#endif
{
   while (Env != NoTree) {
      register tTree Object;
      if (Env->Kind == kenv) {
	 Object = Env->env.HashTable [Ident % Env->env.HashSize];
	 while (Object != NoTree)
	    if (Object->object.ident == Ident) return WriteId (Ident, Object);
	    else Object = Object->object.collision;
	 Env = Env->env.env;
      } else {
	 Object = IdentifyWhole (Ident, Env->env2.env1);
	 if (Object != nnoobject) return WriteId (Ident, Object);
	 Env = Env->env2.env2;
	 /* return IdentifyWhole (Ident, Env->env2.env2); */
      }
   }
   return nnoobject;
}

tTree IdentifyMethod
#if defined __STDC__ | defined __cplusplus
   (register tIdent Ident, register tTree Env, short no_of_args)
#else
   (Ident, Env, no_of_args)
   register tIdent	Ident;
   register tTree	Env;
   	    short	no_of_args;
#endif
{
   while (Env != NoTree) {
      register tTree Object;
      if (Env->Kind == kenv) {
	 Object = Env->env.HashTable [Ident % Env->env.HashSize];
	 while (Object != NoTree)
	    if (Object->object.ident == Ident &&
	       Tree_IsType (Object->object.object, kfunction) &&
	       Object->object.object->function.decl_list->decl_list.no_of_args == no_of_args)
	       return WriteId (Ident, Object);
	    else
	       Object = Object->object.collision;
	 Env = Env->env.env;
      } else {
	 Object = IdentifyMethod (Ident, Env->env2.env1, no_of_args);
	 if (Object != nnoobject) return WriteId (Ident, Object);
	 Env = Env->env2.env2;
	 /* return IdentifyMethod (Ident, Env->env2.env2, no_of_args); */
      }
   }
   return nnoobject;
}

/*
tTree IdentifyTail
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env,
   tPosition		Pos)
#else
   (Ident, Env, Pos)
   register tIdent	Ident;
   register tTree	Env;
   tPosition		Pos;
#endif
{
   while (Env != NoTree) {
      register tTree Object = Env->env.objects;
      while (Object != NoTree)
	 if (Object->object.ident == Ident /* && IsBefore (Object->Decl.Pos, Pos) /* )
	    return Object;
	 else Object = Object->object.next;
      Env = Env->env.env;
   }
   return nnoobject;
}

tTree IdentifyLocalKind
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env,
   register Tree_tKind	Kind)
#else
   (Ident, Env, Kind)
   register tIdent	Ident;
   register tTree	Env;
   register Tree_tKind	Kind;
#endif
{
   if (Env != NoTree) {
      register tTree Object = Env->env.objects;
      while (Object != NoTree)
	 if (Object->object.ident == Ident && Tree_IsType (Object, Kind))
	    return Object;
	 else Object = Object->object.next;
   }
   return nnoobject;
}

tTree IdentifyWholeKind
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env,
   register Tree_tKind	Kind)
#else
   (Ident, Env, Kind)
   register tIdent	Ident;
   register tTree	Env;
   register Tree_tKind	Kind;
#endif
{
   while (Env != NoTree) {
      register tTree Object = Env->env.objects;
      while (Object != NoTree)
	 if (Object->object.ident == Ident && Tree_IsType (Object, Kind))
	    return Object;
	 else Object = Object->object.next;
      Env = Env->env.env;
   }
   return nnoobject;
}

tTree IdentifyTailKind
#if defined __STDC__ | defined __cplusplus
   (register tIdent	Ident,
   register tTree	Env,
   tPosition		Pos,
   register Tree_tKind	Kind)
#else
   (Ident, Env, Pos, Kind)
   register tIdent	Ident;
   register tTree	Env;
   tPosition		Pos;
   register Tree_tKind	Kind;
#endif
{
   while (Env != NoTree) {
      register tTree Object = Env->env.objects;
      while (Object != NoTree)
	 if (Object->object.ident == Ident && Tree_IsType (Object, Kind)
	    /* && IsBefore (Object->Decl.Pos, Pos) /* )
	    return Object;
	 else Object = Object->object.next;
      Env = Env->env.env;
   }
   return nnoobject;
}
*/

tTree mEnv
#if defined __STDC__ | defined __cplusplus
   (tTree Objects, tTree Env, tTree DclObject)
#else
   (Objects, Env, DclObject) tTree Objects; tTree Env, DclObject;
#endif
{
	    tTree	t;
   register tHashTable	HashTable;
   register int		HashSize = 0;
   register int		i;
   register tTree	Object;

   if ((DclObject->Kind == kcompound_stmt || DclObject->Kind == kfor_stmt) &&
	 Objects == NoTree)
      return Env;
   for (Object = Objects; Object != NoTree; Object = Object->object.next)
      HashSize ++;
   if (HashSize <= 1) HashSize = 4;
   HashTable = (tHashTable) Alloc (HashSize * sizeof (tTree));
   t = menv (Objects, Env, DclObject, HashTable, HashSize);
   for (i = HashSize; i > 0;) HashTable [-- i] = NoTree;
   while (Objects != NoTree) {
      register int Hash = Objects->object.ident % HashSize;
      Objects->object.collision = HashTable [Hash];
      HashTable [Hash] = Objects;
      Objects = Objects->object.next;
   }
   /* WriteEnv (stdout, t); */
   return t;
}

void WriteEnv		/* local only */
#if defined __STDC__ | defined __cplusplus
   (FILE * f, tTree Env)
#else
   (f, Env) FILE * f; tTree Env;
#endif
{
   if (Env != NoTree) {
      if (Env->Kind == kenv) {
	 register tTree Object = Env->env.objects;
	 (void) fprintf (f, "ENV %s (%p)", Tree_NodeName [Env->env.object->Kind], Env);
	 if (Env->env.env != NoTree)
	    if (Env->env.env->Kind == kenv)
	       (void) fprintf (f, " OUTER %s (%p)\n", Tree_NodeName [Env->env.env->env.object->Kind], Env->env.env);
	    else
	       (void) fprintf (f, " OUTER %s (%p)\n", Tree_NodeName [Env->env.env->env2.env1->env.object->Kind], Env->env.env);
	 else
	    (void) fprintf (f, "\n");
	 while (Object != NoTree) {
	    (void) fputc (' ', f);
	    WriteIdent (f, Object->object.ident);
	    (void) fprintf (f, "\t%s\n", Tree_NodeName [Object->object.object->Kind]);
	    Object = Object->object.next;
	 }
      } else {
	 (void) fprintf (f, "ENV2 %s (%p)", Tree_NodeName [Env->env2.env1->env.object->Kind], Env);
	 (void) fprintf (f, " INNER %s (%p)", Tree_NodeName [Env->env2.env1->env.object->Kind], Env->env2.env1);
	 (void) fprintf (f, " OUTER %s (%p)\n", Tree_NodeName [Env->env2.env2->env.object->Kind], Env->env2.env2);
	 WriteEnv (f, Env->env2.env1);
      }
   }
}

