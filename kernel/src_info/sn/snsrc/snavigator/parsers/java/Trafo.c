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

#include "Trafo.h"
#ifdef __cplusplus
extern "C" {
#include "General.h"
#include "rSystem.h"
}
#else
#include "General.h"
#include "rSystem.h"
#endif
#include <stdio.h>
#include "Tree.h"

#define yyInline
#ifndef NULL
#define NULL 0L
#endif
#ifndef rfalse
#define rfalse 0
#endif
#ifndef rtrue
#define rtrue 1
#endif

#ifdef yyInline
#define yyALLOC(tree, free, start, alloc, type, make, ptr, kind, init) \
  ptr = (free -= yyAlignedSize (sizeof (type))) >= start ? \
   (tree) free : alloc (sizeof (type)); \
  init (ptr, kind);
#else
#define yyALLOC(tree, free, start, alloc, type, make, ptr, kind, init) \
  ptr = make (kind);
#endif

/* line 5 "java.puma" */

#include "Reuse.h"
#include "StringM.h"
#include "Idents.h"
#include "Parser.h"
#include "deftab.h"
#include "sn.h"

#define null (char *) NULL

static	char *	p;
static	char *	cur_class_ptr	;
static	char *	cur_method_ptr	;
static	char *	cur_arg_types_ptr;
static	char	cur_arg_types [256];
static	char	ref_class [256], ref_sym [256], ref_arg_types [256];
static	int	acc		= PAF_REF_READ;
static	short	no_of_args	= -1;


#ifndef yyWrite
#define yyWrite(s) (void) fputs (s, yyf)
#endif
#ifndef yyWriteNl
#define yyWriteNl (void) fputc ('\n', yyf)
#endif

#include "yyTrafo.h"

static void yyExit ARGS ((void)) { rExit (1); }

void (* Trafo_Exit) ARGS ((void)) = yyExit;

#ifdef UNIX
static FILE * yyf = stdout;
#else
static FILE * yyf;
#endif

static void yyAbort
#ifdef __cplusplus
 (char * yyFunction)
#else
 (yyFunction) char * yyFunction;
#endif
{
 (void) fprintf (stderr, "Error: module Trafo, routine %s failed\n",
  yyFunction);
 Trafo_Exit ();
}

void to_types ARGS ((tTree yyP2, tString yyP1));
static void get_types ARGS ((tTree yyP3));
void to_names ARGS ((tTree yyP5, tString yyP4));
static void get_names ARGS ((tTree yyP6));
void to_files ARGS ((tTree yyP8, tString yyP7));
static void get_files ARGS ((tTree yyP9));
static void get_separator ARGS ((tTree yyP10));
tTree get_objects ARGS ((tTree t, tTree o));
static void use_object ARGS ((tTree yyP11));
static tString get_class_name ARGS ((tTree yyP12));
static tString get_class_name_2 ARGS ((tTree yyP13));
static tTree get_current_class ARGS ((tTree yyP14));
static void Tsuperclass ARGS ((tTree yyP15));
void Traverse ARGS ((tTree yyP16));
static void Tqualification ARGS ((tTree yyP17));
static void Tclass ARGS ((tTree yyP18));
static void Texception ARGS ((tTree yyP19));
static tTree Tidentify ARGS ((tTree yyP20));

void to_types
#if defined __STDC__ | defined __cplusplus
(register tTree yyP2, register tString yyP1)
#else
(yyP2, yyP1)
 register tTree yyP2;
 register tString yyP1;
#endif
{
/* line 27 "java.puma" */
  {
/* line 27 "java.puma" */
   p = yyP1;
/* line 27 "java.puma" */
   get_types (yyP2);
/* line 27 "java.puma" */
   * p = '\0';
  }
   return;

;
}

static void get_types
#if defined __STDC__ | defined __cplusplus
(register tTree yyP3)
#else
(yyP3)
 register tTree yyP3;
#endif
{
 yyRecursion:
  if (yyP3->Kind == kparameter) {
/* line 31 "java.puma" */
  {
/* line 32 "java.puma" */
   get_types (yyP3->parameter.type);
/* line 33 "java.puma" */
   get_types (yyP3->parameter.array);
/* line 34 "java.puma" */
   get_separator (yyP3->parameter.next);
/* line 35 "java.puma" */
   yyP3 = yyP3->parameter.next;
   goto yyRecursion;
  }

  }
  if (yyP3->Kind == ksimple_type) {
/* line 37 "java.puma" */
  {
/* line 38 "java.puma" */
   strcpy (p, types [yyP3->simple_type.type]);
/* line 38 "java.puma" */
   p = p + strlen (types [yyP3->simple_type.type]);
  }
   return;

  }
  if (yyP3->Kind == knamed_type) {
/* line 40 "java.puma" */
  {
/* line 41 "java.puma" */
   yyP3 = yyP3->named_type.qualified_symbol;
   goto yyRecursion;
  }

  }
  if (yyP3->Kind == karray_type) {
/* line 43 "java.puma" */
  {
/* line 44 "java.puma" */
   get_types (yyP3->array_type.type);
/* line 45 "java.puma" */
   strcpy (p, "[]");
/* line 45 "java.puma" */
   p = p + 2;
  }
   return;

  }
  if (yyP3->Kind == kqualification) {
/* line 47 "java.puma" */
  {
/* line 48 "java.puma" */
   get_types (yyP3->qualification.qualified_symbol);
/* line 49 "java.puma" */
   * p ++ = '.';
/* line 50 "java.puma" */
   GetString (yyP3->qualification.ident, p);
/* line 50 "java.puma" */
   p = p + LengthSt (GetStringRef (yyP3->qualification.ident));
  }
   return;

  }
  if (yyP3->Kind == kident) {
/* line 52 "java.puma" */
  {
/* line 53 "java.puma" */
   GetString (yyP3->ident.ident, p);
/* line 53 "java.puma" */
   p = p + LengthSt (GetStringRef (yyP3->ident.ident));
  }
   return;

  }
;
}

void to_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP5, register tString yyP4)
#else
(yyP5, yyP4)
 register tTree yyP5;
 register tString yyP4;
#endif
{
/* line 58 "java.puma" */
  {
/* line 58 "java.puma" */
   p = yyP4;
/* line 58 "java.puma" */
   get_names (yyP5);
/* line 58 "java.puma" */
   * p = '\0';
  }
   return;

;
}

static void get_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP6)
#else
(yyP6)
 register tTree yyP6;
#endif
{
 yyRecursion:
  if (yyP6->Kind == kparameter) {
/* line 62 "java.puma" */
  {
/* line 63 "java.puma" */
   GetString (yyP6->parameter.ident, p);
/* line 63 "java.puma" */
   p = p + LengthSt (GetStringRef (yyP6->parameter.ident));
/* line 64 "java.puma" */
   get_separator (yyP6->parameter.next);
/* line 65 "java.puma" */
   yyP6 = yyP6->parameter.next;
   goto yyRecursion;
  }

  }
;
}

void to_files
#if defined __STDC__ | defined __cplusplus
(register tTree yyP8, register tString yyP7)
#else
(yyP8, yyP7)
 register tTree yyP8;
 register tString yyP7;
#endif
{
/* line 70 "java.puma" */
  {
/* line 70 "java.puma" */
   p = yyP7;
/* line 70 "java.puma" */
   get_files (yyP8);
/* line 70 "java.puma" */
   * p = '\0';
  }
   return;

;
}

static void get_files
#if defined __STDC__ | defined __cplusplus
(register tTree yyP9)
#else
(yyP9)
 register tTree yyP9;
#endif
{
  if (yyP9->Kind == kqualification) {
/* line 74 "java.puma" */
  {
/* line 75 "java.puma" */
   get_files (yyP9->qualification.qualified_symbol);
/* line 76 "java.puma" */
   * p ++ = '/';
/* line 77 "java.puma" */
   GetString (yyP9->qualification.ident, p);
/* line 77 "java.puma" */
   p = p + LengthSt (GetStringRef (yyP9->qualification.ident));
  }
   return;

  }
  if (yyP9->Kind == kident) {
/* line 79 "java.puma" */
  {
/* line 80 "java.puma" */
   GetString (yyP9->ident.ident, p);
/* line 80 "java.puma" */
   p = p + LengthSt (GetStringRef (yyP9->ident.ident));
  }
   return;

  }
;
}

static void get_separator
#if defined __STDC__ | defined __cplusplus
(register tTree yyP10)
#else
(yyP10)
 register tTree yyP10;
#endif
{
  if (yyP10->Kind == kparameter) {
/* line 85 "java.puma" */
  {
/* line 85 "java.puma" */
   * p ++ = ',';
  }
   return;

  }
;
}

tTree get_objects
#if defined __STDC__ | defined __cplusplus
(register tTree t, register tTree o)
#else
(t, o)
 register tTree t;
 register tTree o;
#endif
{
 yyRecursion:

  switch (t->Kind) {
  case kimport_asterisk:
/* line 91 "java.puma" */
   t = t->import_asterisk.next;
   goto yyRecursion;

  case kimport:
/* line 94 "java.puma" */
   return get_objects (t->import.next, get_objects (t->import.qualified_symbol, o));

  case knoimport:
/* line 97 "java.puma" */
   return o;

  case kqualification:
/* line 100 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->qualification.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return yyV1;
  }
 }

  case kident:
/* line 103 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->ident.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return yyV1;
  }
 }

  case ktype_decl:
  case kclass:
  case kinterface:
/* line 106 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->type_decl.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return get_objects (t->type_decl.next, yyV1);
  }
 }

  case kfunction:
  case kmethod:
  case kconstructor:
/* line 109 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->function.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return get_objects (t->function.next, yyV1);
  }
 }

  case kvar_decl:
/* line 112 "java.puma" */
   return get_objects (t->var_decl.next, get_objects (t->var_decl.decl_list, o));

  case kfield:
  case kstatic_initializer:
  case kinitializer:
/* line 115 "java.puma" */
   t = t->field.next;
   goto yyRecursion;

  case knofield:
/* line 118 "java.puma" */
   return o;

  case kdecl:
  case kvariable:
  case kparameter:
/* line 121 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->decl.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return get_objects (t->decl.next, yyV1);
  }
 }

  case knodecl:
/* line 124 "java.puma" */
   return o;

  case kstatement:
/* line 127 "java.puma" */
   return get_objects (t->statement.next, get_objects (t->statement.statement, o));

  case knostatement:
/* line 130 "java.puma" */
   return o;

  case kvar_decl_stmt:
/* line 133 "java.puma" */
   t = t->var_decl_stmt.decl_list;
   goto yyRecursion;

  case ktype_decl_stmt:
/* line 136 "java.puma" */
   t = t->type_decl_stmt.type_decl;
   goto yyRecursion;

  case kif_stmt:
  case kif_else_stmt:
/* line 139 "java.puma" */
   t = t->if_stmt.then;
   goto yyRecursion;

  case kwhile_stmt:
/* line 145 "java.puma" */
   t = t->while_stmt.statement;
   goto yyRecursion;

  case kdo_stmt:
/* line 148 "java.puma" */
   t = t->do_stmt.statement;
   goto yyRecursion;

  case ksynchronized_stmt:
/* line 151 "java.puma" */
   t = t->synchronized_stmt.statement;
   goto yyRecursion;

  case klabeled_stmt:
/* line 154 "java.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = t;
    yyV1->object.ident = t->labeled_stmt.ident;
    yyV1->object.next = o;
    begintTree (yyV1->object.collision)
   return get_objects (t->labeled_stmt.statement, yyV1);
  }
 }

  case ktry_stmt:
/* line 157 "java.puma" */
   return get_objects (t->try_stmt.finally, get_objects (t->try_stmt.statement, o));

  case kswitch_stmt:
/* line 160 "java.puma" */
   t = t->switch_stmt.switch_list;
   goto yyRecursion;

  case kstatement_c:
  case kcompound_stmt:
  case kexpression_stmt:
  case kbreak_stmt:
  case kbreak_id_stmt:
  case kcontinue_stmt:
  case kcontinue_id_stmt:
  case kreturn_stmt:
  case kreturn_expr_stmt:
  case kfor_stmt:
  case kthrow_stmt:
  case knull_stmt:
/* line 163 "java.puma" */
   return o;

  case kswitch_:
/* line 166 "java.puma" */
   return get_objects (t->switch_.next, get_objects (t->switch_.statement_list, o));

  case knoswitch:
/* line 169 "java.puma" */
   return o;

  }

 yyAbort ("get_objects");
 return 0;
}

static void use_object
#if defined __STDC__ | defined __cplusplus
(register tTree yyP11)
#else
(yyP11)
 register tTree yyP11;
#endif
{
  if (yyP11->Kind == kident) {
  if (yyP11->ident.object->Kind == knoobject) {
/* line 177 "java.puma" */
  {
/* line 178 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, "?", GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
   return;

  }
  if (yyP11->ident.object->Kind == kobject) {

  switch (yyP11->ident.object->object.object->Kind) {
  case kparameter:
/* line 183 "java.puma" */
  {
/* line 185 "java.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
   return;

  case kvariable:
/* line 189 "java.puma" */
  {
/* line 191 "java.puma" */
   if (! (yyP11->ident.object->object.object->variable.env -> Kind == kenv2 && Tree_IsType (yyP11->ident.object->object.object->variable.env -> env2 . env1 -> env . object, ktype_decl))) goto yyL3;
  {
/* line 192 "java.puma" */
   put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, get_class_name (yyP11->ident.object->object.object->variable.env), GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
  }
   return;
yyL3:;

/* line 196 "java.puma" */
  {
/* line 198 "java.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
   return;

  case klabeled_stmt:
/* line 202 "java.puma" */
  {
/* line 204 "java.puma" */
   put_cross_ref (PAF_REF_TO_CONSTANT, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, PAF_REF_READ);
  }
   return;

  case kfunction:
  case kmethod:
  case kconstructor:
/* line 208 "java.puma" */
  {
/* line 210 "java.puma" */
   to_types (yyP11->ident.object->object.object->function.decl_list, ref_arg_types);
/* line 211 "java.puma" */
   put_cross_ref (PAF_REF_TO_MBR_FUNC, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, get_class_name (yyP11->ident.object->object.object->function.env), GetCStr (yyP11->ident.ident), ref_arg_types, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
   return;

  case ktype_decl:
  case kclass:
  case kinterface:
/* line 215 "java.puma" */
  {
/* line 217 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, acc);
  }
   return;

  case kident:
/* line 221 "java.puma" */
  {
/* line 225 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, PAF_REF_READ);
  }
   return;

  case kqualification:
/* line 221 "java.puma" */
  {
/* line 225 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP11->ident.ident), null, current_file, (int)  yyP11->ident.pos . Line, PAF_REF_READ);
  }
   return;

  }

  }
  }
  if (yyP11->Kind == kqualification) {
  if (yyP11->qualification.object->Kind == knoobject) {
  if (yyP11->qualification.qualified_symbol->qualified_symbol.object->Kind == kobject) {
  if (Tree_IsType (yyP11->qualification.qualified_symbol->qualified_symbol.object->object.object, ktype_decl)) {
/* line 229 "java.puma" */
  {
/* line 232 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, GetCStr (yyP11->qualification.qualified_symbol->qualified_symbol.object->object.object->type_decl.ident), GetCStr (yyP11->qualification.ident), null, current_file, (int)  yyP11->qualification.pos . Line, acc);
  }
   return;

  }
  if (Tree_IsType (yyP11->qualification.qualified_symbol->qualified_symbol.object->object.object, kdecl)) {
  if (yyP11->qualification.qualified_symbol->qualified_symbol.object->object.object->decl.type->Kind == knamed_type) {
/* line 237 "java.puma" */
  {
/* line 241 "java.puma" */
   to_types (yyP11->qualification.qualified_symbol->qualified_symbol.object->object.object->decl.type->named_type.qualified_symbol, ref_class);
/* line 242 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, ref_class, GetCStr (yyP11->qualification.ident), null, current_file, (int)  yyP11->qualification.pos . Line, acc);
  }
   return;

  }
  }
  }
/* line 247 "java.puma" */
  {
/* line 249 "java.puma" */
   to_types (yyP11->qualification.qualified_symbol, ref_class);
/* line 250 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, ref_class, GetCStr (yyP11->qualification.ident), null, current_file, (int)  yyP11->qualification.pos . Line, acc);
  }
   return;

  }
  if (yyP11->qualification.object->Kind == kobject) {
  if (yyP11->qualification.object->object.object->Kind == kvariable) {
/* line 255 "java.puma" */
  {
/* line 258 "java.puma" */
   put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, get_class_name (yyP11->qualification.object->object.object->variable.env), GetCStr (yyP11->qualification.ident), null, current_file, (int)  yyP11->qualification.pos . Line, acc);
  }
   return;

  }
  if (Tree_IsType (yyP11->qualification.object->object.object, kfunction)) {
/* line 262 "java.puma" */
  {
/* line 265 "java.puma" */
   to_types (yyP11->qualification.object->object.object->function.decl_list, ref_arg_types);
/* line 266 "java.puma" */
   put_cross_ref (PAF_REF_TO_MBR_FUNC, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, get_class_name (yyP11->qualification.object->object.object->function.env), GetCStr (yyP11->qualification.ident), ref_arg_types, current_file, (int)  yyP11->qualification.pos . Line, acc);
  }
   return;

  }
  }
  }
;
}

static tString get_class_name
#if defined __STDC__ | defined __cplusplus
(register tTree yyP12)
#else
(yyP12)
 register tTree yyP12;
#endif
{
 yyRecursion:
  if (yyP12 == NULL) {
/* line 273 "java.puma" */
   return null;

  }
  if (yyP12->Kind == kenv) {
  if (Tree_IsType (yyP12->env.object, ktype_decl)) {
/* line 276 "java.puma" */
   return GetCStr (yyP12->env.object->type_decl.ident);

  }
  if (yyP12->env.object->Kind == kanonymous) {
/* line 279 "java.puma" */
   return s_anonymous;

  }
  }
  if (yyP12->Kind == kenv2) {
/* line 282 "java.puma" */
   yyP12 = yyP12->env2.env1;
   goto yyRecursion;

  }
 yyAbort ("get_class_name");
 return 0;
}

static tString get_class_name_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP13)
#else
(yyP13)
 register tTree yyP13;
#endif
{
  if (Tree_IsType (yyP13, ktype_decl)) {
/* line 288 "java.puma" */
   return GetCStr (yyP13->type_decl.ident);

  }
  if (yyP13->Kind == kanonymous) {
/* line 291 "java.puma" */
   return s_anonymous;

  }
 yyAbort ("get_class_name_2");
 return 0;
}

static tTree get_current_class
#if defined __STDC__ | defined __cplusplus
(register tTree yyP14)
#else
(yyP14)
 register tTree yyP14;
#endif
{
 yyRecursion:
  if (yyP14->Kind == kenv) {

  switch (yyP14->env.object->Kind) {
  case kcompilation_unit:
/* line 297 "java.puma" */
   return NULL;

  case ktype_decl:
  case kclass:
  case kinterface:
/* line 300 "java.puma" */
   return yyP14->env.object;

  case kanonymous:
/* line 303 "java.puma" */
   return yyP14->env.object;

  case kfunction:
  case kmethod:
  case kconstructor:
/* line 306 "java.puma" */
   yyP14 = yyP14->env.object->function.env;
   goto yyRecursion;

  case kcompound_stmt:
/* line 306 "java.puma" */
   yyP14 = yyP14->env.object->compound_stmt.env;
   goto yyRecursion;

  case kfor_stmt:
/* line 306 "java.puma" */
   yyP14 = yyP14->env.object->for_stmt.env;
   goto yyRecursion;

  case kcatch:
/* line 306 "java.puma" */
   yyP14 = yyP14->env.object->catch.env;
   goto yyRecursion;

  }

  }
  if (yyP14->Kind == kenv2) {
/* line 312 "java.puma" */
   yyP14 = yyP14->env2.env1;
   goto yyRecursion;

  }
/* line 315 "java.puma" */
   return NULL;

}

static void Tsuperclass
#if defined __STDC__ | defined __cplusplus
(register tTree yyP15)
#else
(yyP15)
 register tTree yyP15;
#endif
{
 yyRecursion:
  if (yyP15->Kind == kclass) {
  if (yyP15->class.extends->Kind == knamed_type) {
/* line 321 "java.puma" */
  {
/* line 322 "java.puma" */
   cur_class_ptr = GetCStr (yyP15->class.ident);
/* line 323 "java.puma" */
   Tclass (yyP15->class.extends->named_type.qualified_symbol);
/* line 324 "java.puma" */

   if (yyP15->class.extends->named_type.qualified_symbol->qualified_symbol.object != nnoobject && yyP15->class.extends->named_type.qualified_symbol->qualified_symbol.object->object.object->Kind == kclass)
      yyP15->class.block->field_list.env->env2.env1->env.env = yyP15->class.extends->named_type.qualified_symbol->qualified_symbol.object->object.object->type_decl.block->field_list.env->env2.env1;

/* line 328 "java.puma" */
   yyP15 = yyP15->class.next;
   goto yyRecursion;
  }

  }
/* line 330 "java.puma" */
  {
/* line 331 "java.puma" */
   yyP15 = yyP15->class.next;
   goto yyRecursion;
  }

  }
  if (yyP15->Kind == kinterface) {
/* line 333 "java.puma" */
  {
/* line 334 "java.puma" */
   cur_class_ptr = GetCStr (yyP15->my_interface.ident);
/* line 335 "java.puma" */
   Tclass (yyP15->my_interface.extends);
/* line 336 "java.puma" */
   yyP15 = yyP15->my_interface.next;
   goto yyRecursion;
  }

  }
;
}

void Traverse
#if defined __STDC__ | defined __cplusplus
(register tTree yyP16)
#else
(yyP16)
 register tTree yyP16;
#endif
{
 yyRecursion:

  switch (yyP16->Kind) {
  case kcompilation_unit:
/* line 341 "java.puma" */
  {
/* line 342 "java.puma" */
   cur_class_ptr = null;
/* line 342 "java.puma" */
   cur_method_ptr = null;
/* line 342 "java.puma" */
   cur_arg_types_ptr = null;
/* line 345 "java.puma" */
   Tsuperclass (yyP16->compilation_unit.field_list);
/* line 346 "java.puma" */
   cur_class_ptr = null;
/* line 346 "java.puma" */
   cur_method_ptr = null;
/* line 346 "java.puma" */
   cur_arg_types_ptr = null;
/* line 347 "java.puma" */
   yyP16 = yyP16->compilation_unit.field_list;
   goto yyRecursion;
  }

  case kimport:
  case kimport_asterisk:
/* line 349 "java.puma" */
  {
/* line 350 "java.puma" */
   Tclass (yyP16->import.qualified_symbol);
/* line 351 "java.puma" */
   yyP16 = yyP16->import.next;
   goto yyRecursion;
  }

  case kclass:
/* line 353 "java.puma" */
 {
  tString prev_class;
  {
/* line 354 "java.puma" */

/* line 354 "java.puma" */
   prev_class = cur_class_ptr;
/* line 355 "java.puma" */
   cur_class_ptr = GetCStr (yyP16->class.ident);
/* line 356 "java.puma" */
   Tclass (yyP16->class.interfaces);
/* line 357 "java.puma" */
   Traverse (yyP16->class.block);
/* line 358 "java.puma" */
   cur_class_ptr = prev_class;
/* line 359 "java.puma" */
   yyP16 = yyP16->class.next;
   goto yyRecursion;
  }
 }

  case kinterface:
/* line 361 "java.puma" */
 {
  tString prev_class;
  {
/* line 362 "java.puma" */

/* line 362 "java.puma" */
   prev_class = cur_class_ptr;
/* line 363 "java.puma" */
   cur_class_ptr = GetCStr (yyP16->my_interface.ident);
/* line 364 "java.puma" */
   Traverse (yyP16->my_interface.block);
/* line 365 "java.puma" */
   cur_class_ptr = prev_class;
/* line 366 "java.puma" */
   yyP16 = yyP16->my_interface.next;
   goto yyRecursion;
  }
 }

  case kmethod:
/* line 368 "java.puma" */
  {
/* line 369 "java.puma" */
   cur_method_ptr = GetCStr (yyP16->method.ident);
/* line 370 "java.puma" */
   to_types (yyP16->method.decl_list, cur_arg_types);
/* line 370 "java.puma" */
   cur_arg_types_ptr = cur_arg_types;
/* line 371 "java.puma" */
   Traverse (yyP16->method.decl_list);
/* line 372 "java.puma" */
   Traverse (yyP16->method.type);
/* line 373 "java.puma" */
   Texception (yyP16->method.throws);
/* line 374 "java.puma" */
   Traverse (yyP16->method.block);
/* line 375 "java.puma" */
   yyP16 = yyP16->method.next;
   goto yyRecursion;
  }

  case kconstructor:
/* line 377 "java.puma" */
  {
/* line 378 "java.puma" */
   cur_method_ptr = GetCStr (yyP16->constructor.ident);
/* line 379 "java.puma" */
   to_types (yyP16->constructor.decl_list, cur_arg_types);
/* line 379 "java.puma" */
   cur_arg_types_ptr = cur_arg_types;
/* line 380 "java.puma" */
   Traverse (yyP16->constructor.decl_list);
/* line 381 "java.puma" */
   Texception (yyP16->constructor.throws);
/* line 382 "java.puma" */
   Traverse (yyP16->constructor.block);
/* line 383 "java.puma" */
   yyP16 = yyP16->constructor.next;
   goto yyRecursion;
  }

  case kvar_decl:
/* line 385 "java.puma" */
  {
/* line 386 "java.puma" */
   cur_method_ptr = null;
/* line 386 "java.puma" */
   cur_arg_types_ptr = null;
/* line 387 "java.puma" */
   Traverse (yyP16->var_decl.type);
/* line 388 "java.puma" */
   Traverse (yyP16->var_decl.decl_list);
/* line 389 "java.puma" */
   yyP16 = yyP16->var_decl.next;
   goto yyRecursion;
  }

  case kstatic_initializer:
/* line 391 "java.puma" */
  {
/* line 392 "java.puma" */
   cur_method_ptr = null;
/* line 392 "java.puma" */
   cur_arg_types_ptr = null;
/* line 393 "java.puma" */
   Traverse (yyP16->static_initializer.statement_c);
/* line 394 "java.puma" */
   yyP16 = yyP16->static_initializer.next;
   goto yyRecursion;
  }

  case kinitializer:
/* line 396 "java.puma" */
  {
/* line 397 "java.puma" */
   cur_method_ptr = null;
/* line 397 "java.puma" */
   cur_arg_types_ptr = null;
/* line 398 "java.puma" */
   Traverse (yyP16->initializer.statement_c);
/* line 399 "java.puma" */
   yyP16 = yyP16->initializer.next;
   goto yyRecursion;
  }

  case kparameter:
/* line 401 "java.puma" */
  {
/* line 402 "java.puma" */
   Traverse (yyP16->parameter.type);
/* line 403 "java.puma" */
   Traverse (yyP16->parameter.array);
/* line 404 "java.puma" */
   yyP16 = yyP16->parameter.next;
   goto yyRecursion;
  }

  case kvariable:
  if (yyP16->variable.expression->Kind == knoexpression) {
/* line 406 "java.puma" */
  {
/* line 407 "java.puma" */
   yyP16 = yyP16->variable.next;
   goto yyRecursion;
  }

  }
  if (yyP16->variable.env->Kind == kenv2) {
/* line 409 "java.puma" */
  {
/* line 410 "java.puma" */
   put_cross_ref (PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, get_class_name_2 (yyP16->variable.env->env2.env1 -> env . object), GetCStr (yyP16->variable.ident), null, current_file, (int)  yyP16->variable.pos . Line, PAF_REF_WRITE);
/* line 414 "java.puma" */
   Traverse (yyP16->variable.expression);
/* line 415 "java.puma" */
   yyP16 = yyP16->variable.next;
   goto yyRecursion;
  }

  }
/* line 417 "java.puma" */
  {
/* line 418 "java.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP16->variable.ident), null, current_file, (int)  yyP16->variable.pos . Line, PAF_REF_WRITE);
/* line 421 "java.puma" */
   Traverse (yyP16->variable.expression);
/* line 422 "java.puma" */
   yyP16 = yyP16->variable.next;
   goto yyRecursion;
  }

  case kcompound_stmt:
/* line 424 "java.puma" */
  {
/* line 425 "java.puma" */
   yyP16 = yyP16->compound_stmt.statement_list;
   goto yyRecursion;
  }

  case kexpression_stmt:
/* line 427 "java.puma" */
  {
/* line 428 "java.puma" */
   yyP16 = yyP16->expression_stmt.expression;
   goto yyRecursion;
  }

  case kvar_decl_stmt:
/* line 430 "java.puma" */
  {
/* line 431 "java.puma" */
   Traverse (yyP16->var_decl_stmt.type);
/* line 432 "java.puma" */
   yyP16 = yyP16->var_decl_stmt.decl_list;
   goto yyRecursion;
  }

  case ktype_decl_stmt:
/* line 434 "java.puma" */
  {
/* line 435 "java.puma" */
   yyP16 = yyP16->type_decl_stmt.type_decl;
   goto yyRecursion;
  }

  case kif_else_stmt:
/* line 437 "java.puma" */
  {
/* line 438 "java.puma" */
   Traverse (yyP16->if_else_stmt.expression);
/* line 439 "java.puma" */
   Traverse (yyP16->if_else_stmt.then);
/* line 440 "java.puma" */
   yyP16 = yyP16->if_else_stmt.Else;
   goto yyRecursion;
  }

  case kif_stmt:
/* line 442 "java.puma" */
  {
/* line 443 "java.puma" */
   Traverse (yyP16->if_stmt.expression);
/* line 444 "java.puma" */
   yyP16 = yyP16->if_stmt.then;
   goto yyRecursion;
  }

  case kwhile_stmt:
/* line 446 "java.puma" */
  {
/* line 447 "java.puma" */
   Traverse (yyP16->while_stmt.expression);
/* line 448 "java.puma" */
   yyP16 = yyP16->while_stmt.statement;
   goto yyRecursion;
  }

  case kdo_stmt:
/* line 450 "java.puma" */
  {
/* line 451 "java.puma" */
   Traverse (yyP16->do_stmt.statement);
/* line 452 "java.puma" */
   yyP16 = yyP16->do_stmt.expression;
   goto yyRecursion;
  }

  case kbreak_id_stmt:
/* line 454 "java.puma" */
  {
/* line 455 "java.puma" */
   yyP16 = yyP16->break_id_stmt.expression;
   goto yyRecursion;
  }

  case kcontinue_id_stmt:
/* line 457 "java.puma" */
  {
/* line 458 "java.puma" */
   yyP16 = yyP16->continue_id_stmt.expression;
   goto yyRecursion;
  }

  case kreturn_expr_stmt:
/* line 460 "java.puma" */
  {
/* line 461 "java.puma" */
   yyP16 = yyP16->return_expr_stmt.expression;
   goto yyRecursion;
  }

  case kfor_stmt:
/* line 463 "java.puma" */
  {
/* line 464 "java.puma" */
   Traverse (yyP16->for_stmt.for_init);
/* line 465 "java.puma" */
   Traverse (yyP16->for_stmt.expression);
/* line 466 "java.puma" */
   Traverse (yyP16->for_stmt.for_incr);
/* line 467 "java.puma" */
   yyP16 = yyP16->for_stmt.statement;
   goto yyRecursion;
  }

  case kthrow_stmt:
/* line 469 "java.puma" */
  {
/* line 470 "java.puma" */
   yyP16 = yyP16->throw_stmt.expression;
   goto yyRecursion;
  }

  case ksynchronized_stmt:
/* line 472 "java.puma" */
  {
/* line 473 "java.puma" */
   Traverse (yyP16->synchronized_stmt.expression);
/* line 474 "java.puma" */
   yyP16 = yyP16->synchronized_stmt.statement;
   goto yyRecursion;
  }

  case klabeled_stmt:
/* line 476 "java.puma" */
  {
/* line 477 "java.puma" */
   yyP16 = yyP16->labeled_stmt.statement;
   goto yyRecursion;
  }

  case ktry_stmt:
/* line 479 "java.puma" */
  {
/* line 480 "java.puma" */
   Traverse (yyP16->try_stmt.statement);
/* line 481 "java.puma" */
   Traverse (yyP16->try_stmt.catch_list);
/* line 482 "java.puma" */
   yyP16 = yyP16->try_stmt.finally;
   goto yyRecursion;
  }

  case kswitch_stmt:
/* line 484 "java.puma" */
  {
/* line 485 "java.puma" */
   Traverse (yyP16->switch_stmt.expression);
/* line 486 "java.puma" */
   yyP16 = yyP16->switch_stmt.switch_list;
   goto yyRecursion;
  }

  case kstatement:
/* line 488 "java.puma" */
  {
/* line 489 "java.puma" */
   Traverse (yyP16->statement.statement);
/* line 490 "java.puma" */
   yyP16 = yyP16->statement.next;
   goto yyRecursion;
  }

  case kswitch_:
/* line 492 "java.puma" */
  {
/* line 493 "java.puma" */
   Traverse (yyP16->switch_.expression_list);
/* line 494 "java.puma" */
   Traverse (yyP16->switch_.statement_list);
/* line 495 "java.puma" */
   yyP16 = yyP16->switch_.next;
   goto yyRecursion;
  }

  case kcatch:
  if (yyP16->catch.decl_list->Kind == kparameter) {
/* line 497 "java.puma" */
  {
/* line 498 "java.puma" */
   Texception (yyP16->catch.decl_list->parameter.type);
/* line 499 "java.puma" */
   Traverse (yyP16->catch.statement);
/* line 500 "java.puma" */
   yyP16 = yyP16->catch.next;
   goto yyRecursion;
  }

  }
  break;
  case knamed_type:
/* line 502 "java.puma" */
  {
/* line 503 "java.puma" */
   Tclass (yyP16->named_type.qualified_symbol);
  }
   return;

  case karray_type:
/* line 505 "java.puma" */
  {
/* line 506 "java.puma" */
   yyP16 = yyP16->array_type.type;
   goto yyRecursion;
  }

  case kexpression:
/* line 508 "java.puma" */
  {
/* line 509 "java.puma" */
   Traverse (yyP16->expression.expression);
/* line 510 "java.puma" */
   yyP16 = yyP16->expression.next;
   goto yyRecursion;
  }

  case kqualification:
/* line 512 "java.puma" */
 {
  tTree id_obj;
  {
/* line 513 "java.puma" */

/* line 513 "java.puma" */
   id_obj = Tidentify (yyP16);
/* line 514 "java.puma" */
   if (! (id_obj == nnoobject)) goto yyL37;
  {
/* line 515 "java.puma" */
   to_types (yyP16->qualification.qualified_symbol, ref_class);
/* line 516 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, ref_class, null, current_file, (int)  yyP16->qualification.qualified_symbol->qualified_symbol.pos . Line, acc);
/* line 519 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, ref_class, GetCStr (yyP16->qualification.ident), null, current_file, (int)  yyP16->qualification.pos . Line, acc);
  }
  }
   return;
 }
yyL37:;

/* line 524 "java.puma" */
  {
/* line 525 "java.puma" */
   Tqualification (yyP16);
  }
   return;

  case kident:
/* line 527 "java.puma" */
  {
/* line 528 "java.puma" */
  Tidentify (yyP16); 
/* line 529 "java.puma" */
   use_object (yyP16);
  }
   return;

  case kunary:
/* line 531 "java.puma" */
 {
  int acc_save;
  {
/* line 532 "java.puma" */

/* line 533 "java.puma" */

   switch (yyP16->unary.operator) {
   case post_decr	:
   case post_incr	:
   case pre_decr	:
   case pre_incr	:
      acc_save = acc; acc = PAF_REF_WRITE;
      Traverse (yyP16->unary.expression);
      acc = acc_save;
      break;
   default		:
      Traverse (yyP16->unary.expression);
   }

  }
   return;
 }

  case kbinary:
/* line 548 "java.puma" */
  {
/* line 549 "java.puma" */
   Traverse (yyP16->binary.lop);
/* line 550 "java.puma" */
   yyP16 = yyP16->binary.rop;
   goto yyRecursion;
  }

  case kassign:
/* line 552 "java.puma" */
  {
/* line 553 "java.puma" */
   acc = PAF_REF_WRITE;
/* line 554 "java.puma" */
   Traverse (yyP16->assign.lval);
/* line 555 "java.puma" */
   acc = PAF_REF_READ;
/* line 556 "java.puma" */
   yyP16 = yyP16->assign.rval;
   goto yyRecursion;
  }

  case kaggregate:
/* line 558 "java.puma" */
  {
/* line 559 "java.puma" */
   yyP16 = yyP16->aggregate.expression_list;
   goto yyRecursion;
  }

  case kcall:
/* line 561 "java.puma" */
 {
  int acc_save;
  {
/* line 562 "java.puma" */
   no_of_args = yyP16->call.expression_list->expression_list.no_of_args;
/* line 563 "java.puma" */
   Traverse (yyP16->call.expression);
/* line 564 "java.puma" */
   no_of_args = - 1;
/* line 565 "java.puma" */

/* line 565 "java.puma" */
   acc_save = acc;
/* line 565 "java.puma" */
   acc = PAF_REF_PASS;
/* line 566 "java.puma" */
   Traverse (yyP16->call.expression_list);
/* line 567 "java.puma" */
   acc = acc_save;
  }
   return;
 }

  case kselect:
  if (yyP16->select.expression->Kind == kthis) {
/* line 569 "java.puma" */
 {
  tTree class;
  tTree object;
  register tTree yyV1;
  {
/* line 570 "java.puma" */

/* line 570 "java.puma" */
   class = get_current_class (yyP16->select.env);
/* line 571 "java.puma" */

/* line 572 "java.puma" */
   if (! (class != NULL && class -> Kind == kclass)) goto yyL45;
  {
/* line 573 "java.puma" */
   object = IdentifyLocal (yyP16->select.ident, class->type_decl.block -> field_list . env);
/* line 574 "java.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqualification,MakeTree,yyV1,kqualification,Tree_InitHead)
    yyV1->qualification.pos = yyP16->select.pos;
    yyV1->qualification.env = yyP16->select.env;
    yyV1->qualification.object = object;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,yident,MakeTree,yyW1,kident,Tree_InitHead)
    yyV1->qualification.qualified_symbol = yyW1;
    begintPosition (yyW1->ident.pos)
    begintTree (yyW1->ident.env)
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,yobject,MakeTree,yyW2,kobject,Tree_InitHead)
    yyW1->ident.object = yyW2;
    yyW2->object.object = class;
    yyW2->object.ident = class->type_decl.ident;
    begintTree (yyW2->object.next)
    begintTree (yyW2->object.collision)
   }
    yyW1->ident.ident = class->type_decl.ident;
   }
    yyV1->qualification.ident = yyP16->select.ident;
   use_object (yyV1);
  }
  }
   return;
 }
yyL45:;

  }
  if (yyP16->select.expression->Kind == ksuper) {
/* line 578 "java.puma" */
 {
  tTree class;
  tTree object;
  register tTree yyV1;
  {
/* line 579 "java.puma" */

/* line 579 "java.puma" */
   class = get_current_class (yyP16->select.env);
/* line 580 "java.puma" */

/* line 581 "java.puma" */
   if (! (class != NULL && class -> Kind == kclass)) goto yyL46;
  {
/* line 582 "java.puma" */
   if (! (class -> class . extends -> Kind == knamed_type)) goto yyL46;
  {
/* line 583 "java.puma" */
   if (! (class -> class . extends -> named_type . qualified_symbol -> qualified_symbol . object != nnoobject)) goto yyL46;
  {
/* line 584 "java.puma" */
   class = class -> class . extends -> named_type . qualified_symbol -> qualified_symbol . object -> object . object;
/* line 585 "java.puma" */
   if (! ((Tree_IsType (class, ktype_decl)))) goto yyL46;
  {
/* line 586 "java.puma" */
   object = IdentifyLocal (yyP16->select.ident, class->type_decl.block -> field_list . env);
/* line 587 "java.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqualification,MakeTree,yyV1,kqualification,Tree_InitHead)
    yyV1->qualification.pos = yyP16->select.pos;
    yyV1->qualification.env = yyP16->select.env;
    yyV1->qualification.object = object;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,yident,MakeTree,yyW1,kident,Tree_InitHead)
    yyV1->qualification.qualified_symbol = yyW1;
    begintPosition (yyW1->ident.pos)
    begintTree (yyW1->ident.env)
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,yobject,MakeTree,yyW2,kobject,Tree_InitHead)
    yyW1->ident.object = yyW2;
    yyW2->object.object = class;
    yyW2->object.ident = class->type_decl.ident;
    begintTree (yyW2->object.next)
    begintTree (yyW2->object.collision)
   }
    yyW1->ident.ident = class->type_decl.ident;
   }
    yyV1->qualification.ident = yyP16->select.ident;
   use_object (yyV1);
  }
  }
  }
  }
  }
   return;
 }
yyL46:;

  }
/* line 591 "java.puma" */
 {
  short no_of_args_save;
  {
/* line 592 "java.puma" */

/* line 592 "java.puma" */
   no_of_args_save = no_of_args;
/* line 592 "java.puma" */
   no_of_args = - 1;
/* line 593 "java.puma" */
   Traverse (yyP16->select.expression);
/* line 594 "java.puma" */
   no_of_args = no_of_args_save;
/* line 595 "java.puma" */
   put_cross_ref (no_of_args >= 0 ? PAF_REF_TO_MBR_FUNC : PAF_REF_TO_MBR_VAR, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, "?", GetCStr (yyP16->select.ident), null, current_file, (int)  yyP16->select.pos . Line, acc);
  }
   return;
 }

  case kget_class_of_expr:
/* line 600 "java.puma" */
 {
  short no_of_args_save;
  {
/* line 601 "java.puma" */

/* line 601 "java.puma" */
   no_of_args_save = no_of_args;
/* line 601 "java.puma" */
   no_of_args = - 1;
/* line 602 "java.puma" */
   Traverse (yyP16->get_class_of_expr.expression);
/* line 603 "java.puma" */
   no_of_args = no_of_args_save;
  }
   return;
 }

  case kget_class:
/* line 605 "java.puma" */
  {
/* line 606 "java.puma" */
   yyP16 = yyP16->get_class.type;
   goto yyRecursion;
  }

  case ksubscript:
/* line 608 "java.puma" */
 {
  int acc_save;
  {
/* line 609 "java.puma" */
   Traverse (yyP16->subscript.base);
/* line 610 "java.puma" */

/* line 610 "java.puma" */
   acc_save = acc;
/* line 610 "java.puma" */
   acc = PAF_REF_READ;
/* line 611 "java.puma" */
   Traverse (yyP16->subscript.index);
/* line 612 "java.puma" */
   acc = acc_save;
  }
   return;
 }

  case ktype_compare:
/* line 614 "java.puma" */
  {
/* line 615 "java.puma" */
   Traverse (yyP16->type_compare.expression);
/* line 616 "java.puma" */
   yyP16 = yyP16->type_compare.type;
   goto yyRecursion;
  }

  case ktype_cast:
/* line 618 "java.puma" */
  {
/* line 619 "java.puma" */
   Traverse (yyP16->type_cast.type);
/* line 620 "java.puma" */
   yyP16 = yyP16->type_cast.expression;
   goto yyRecursion;
  }

  case knew:
/* line 622 "java.puma" */
  {
/* line 623 "java.puma" */
   Traverse (yyP16->new.type);
/* line 624 "java.puma" */
   Traverse (yyP16->new.expression_list);
/* line 625 "java.puma" */
   yyP16 = yyP16->new.expression;
   goto yyRecursion;
  }

  case kanonymous:
/* line 627 "java.puma" */
 {
  tString prev_class;
  {
/* line 628 "java.puma" */
   Traverse (yyP16->anonymous.type);
/* line 629 "java.puma" */
   Traverse (yyP16->anonymous.expression_list);
/* line 630 "java.puma" */

/* line 630 "java.puma" */
   prev_class = cur_class_ptr;
/* line 631 "java.puma" */
   cur_class_ptr = s_anonymous;
/* line 632 "java.puma" */
   Traverse (yyP16->anonymous.block);
/* line 633 "java.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  case kconditional:
/* line 635 "java.puma" */
  {
/* line 636 "java.puma" */
   Traverse (yyP16->conditional.condition);
/* line 637 "java.puma" */
   Traverse (yyP16->conditional.true_expr);
/* line 638 "java.puma" */
   yyP16 = yyP16->conditional.false_expr;
   goto yyRecursion;
  }

  }

;
}

static void Tqualification
#if defined __STDC__ | defined __cplusplus
(register tTree yyP17)
#else
(yyP17)
 register tTree yyP17;
#endif
{
  if (yyP17->Kind == kqualification) {
/* line 643 "java.puma" */
 {
  short no_of_args_save;
  {
/* line 644 "java.puma" */

/* line 644 "java.puma" */
   no_of_args_save = no_of_args;
/* line 644 "java.puma" */
   no_of_args = - 1;
/* line 645 "java.puma" */
   Tqualification (yyP17->qualification.qualified_symbol);
/* line 646 "java.puma" */
   no_of_args = no_of_args_save;
/* line 647 "java.puma" */
   use_object (yyP17);
  }
   return;
 }

  }
  if (yyP17->Kind == kident) {
/* line 649 "java.puma" */
  {
/* line 650 "java.puma" */
   use_object (yyP17);
  }
   return;

  }
;
}

static void Tclass
#if defined __STDC__ | defined __cplusplus
(register tTree yyP18)
#else
(yyP18)
 register tTree yyP18;
#endif
{
 yyRecursion:
  if (yyP18->Kind == kident) {
/* line 655 "java.puma" */
  {
/* line 656 "java.puma" */
   yyP18->ident.object = IdentifyWhole (yyP18->ident.ident, yyP18->ident.env);
/* line 657 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, GetCStr (yyP18->ident.ident), null, current_file, (int)  yyP18->ident.pos . Line, PAF_REF_READ);
  }
   return;

  }
  if (yyP18->Kind == kqualification) {
/* line 661 "java.puma" */
  {
/* line 662 "java.puma" */
   to_types (yyP18, ref_sym);
/* line 663 "java.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, ref_sym, null, current_file, (int)  yyP18->qualification.pos . Line, PAF_REF_READ);
  }
   return;

  }
  if (yyP18->Kind == ktype_name) {
/* line 667 "java.puma" */
  {
/* line 668 "java.puma" */
   Tclass (yyP18->type_name.named_type->named_type.qualified_symbol);
/* line 669 "java.puma" */
   yyP18 = yyP18->type_name.next;
   goto yyRecursion;
  }

  }
;
}

static void Texception
#if defined __STDC__ | defined __cplusplus
(register tTree yyP19)
#else
(yyP19)
 register tTree yyP19;
#endif
{
 yyRecursion:
  if (yyP19->Kind == knamed_type) {
/* line 677 "java.puma" */
  {
/* line 678 "java.puma" */
   yyP19 = yyP19->named_type.qualified_symbol;
   goto yyRecursion;
  }

  }
  if (yyP19->Kind == karray_type) {
/* line 680 "java.puma" */
  {
/* line 681 "java.puma" */
   yyP19 = yyP19->array_type.type;
   goto yyRecursion;
  }

  }
  if (Tree_IsType (yyP19, kqualified_symbol)) {
/* line 683 "java.puma" */
  {
/* line 684 "java.puma" */
   to_types (yyP19, ref_sym);
/* line 685 "java.puma" */
   put_cross_ref (PAF_REF_TO_EXCEPTION, PAF_MBR_FUNC_DEF, PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_method_ptr, cur_arg_types_ptr, null, ref_sym, null, current_file, (int)  yyP19->qualified_symbol.pos . Line, acc);
  }
   return;

  }
  if (yyP19->Kind == ktype_name) {
/* line 689 "java.puma" */
  {
/* line 690 "java.puma" */
   Texception (yyP19->type_name.named_type);
/* line 691 "java.puma" */
   yyP19 = yyP19->type_name.next;
   goto yyRecursion;
  }

  }
;
}

static tTree Tidentify
#if defined __STDC__ | defined __cplusplus
(register tTree yyP20)
#else
(yyP20)
 register tTree yyP20;
#endif
{
  if (yyP20->Kind == kqualification) {
/* line 696 "java.puma" */
 {
  short no_of_args_save;
  tTree id_obj;
  {
/* line 697 "java.puma" */

/* line 697 "java.puma" */
   no_of_args_save = no_of_args;
/* line 697 "java.puma" */
   no_of_args = - 1;
/* line 698 "java.puma" */

/* line 698 "java.puma" */
   id_obj = Tidentify (yyP20->qualification.qualified_symbol);
/* line 699 "java.puma" */
   no_of_args = no_of_args_save;
/* line 700 "java.puma" */

   if (yyP20->qualification.qualified_symbol->qualified_symbol.object != nnoobject) {
      tTree obj = yyP20->qualification.qualified_symbol->qualified_symbol.object->object.object;
      switch (obj->Kind) {
      case kvariable	:
      case kparameter	:
	 if (obj->decl.type->Kind != knamed_type) break;
	 obj = obj->decl.type->named_type.qualified_symbol;
	 if (obj->qualified_symbol.object == nnoobject) {
	    if (obj->Kind != kident) break;
	    obj->ident.object = IdentifyWhole (obj->ident.ident, obj->ident.env);
	    if (obj->ident.object == nnoobject) break;
	 }
	 obj = obj->qualified_symbol.object->object.object;
	 if (! Tree_IsType (obj, ktype_decl)) break;
      case kclass	:
      case kinterface	:
	 if (no_of_args >= 0)
	    yyP20->qualification.object = IdentifyMethod (yyP20->qualification.ident, obj->type_decl.block->field_list.env, no_of_args);
	 else
	    yyP20->qualification.object = IdentifyLocal (yyP20->qualification.ident, obj->type_decl.block->field_list.env);
      }
   }

  }
  {
   return id_obj;
  }
 }

  }
  if (yyP20->Kind == kident) {
/* line 726 "java.puma" */
  {
/* line 727 "java.puma" */
   if (! (no_of_args >= 0)) goto yyL2;
  {
/* line 728 "java.puma" */
   yyP20->ident.object = IdentifyMethod (yyP20->ident.ident, yyP20->ident.env, no_of_args);
  }
  }
   return yyP20->ident.object;
yyL2:;

/* line 731 "java.puma" */
  {
/* line 732 "java.puma" */
   yyP20->ident.object = IdentifyWhole (yyP20->ident.ident, yyP20->ident.env);
  }
   return yyP20->ident.object;

  }
/* line 735 "java.puma" */
   return nnoobject;

}

void BeginTrafo ARGS ((void))
{
}

void CloseTrafo ARGS ((void))
{
}

