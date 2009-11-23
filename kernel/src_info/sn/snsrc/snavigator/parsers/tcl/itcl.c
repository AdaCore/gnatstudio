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

#include "itcl.h"
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

/* line 5 "itcl.puma" */

#include "Reuse.h"
#include "Position.h"
#include "StringM.h"
#include "Idents.h"
#include "Parser.h"
#include "Eval.h"
#include "sn.h"

#define null		(char *) NULL
#define scope_type(cur_class) cur_class ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
#define default_attr	PAF_PUBLIC

extern	FILE *	cross_ref_fp	;
extern	int	report_local_vars;

static	tIdent
		iat_scope	,
		iclass		,
		icommon		,
		iconfigbody	,
		iconstructor	,
		idestructor	,
		idelete		,
		ihash_auto	,
		iinherit	,
		iisa		,
		iitcl		,
		iitcl_class	,
		iitcl_info	,
		iitk		,
		iiwidgets	,
		ilocal		,
		imethod		,
		iobject		,
		iobjects	,
		iprevious	,
		iprivate	,
		iprotected	,
		ipublic		,
		ithis		,
		ivirtual	;

static	char	buffer [1024]	;
static	char	args_buffer [1500];
static	int	length		;
static	rbool	need_pass_2	;
static	rbool	in_string	;
static	tPosition gpos		;
static	tTree	genv, predef	;
static	char *	p		;
static	char *	current_ident	;
static	char *	current_class	;
static	char *	cur_proc_ptr	= NULL;
static	char *	cur_class_ptr	= NULL;
static	char *	cur_arg_types_ptr = NULL;

static tIdent MakeID
#if defined __STDC__ | defined __cplusplus
(char * s)
#else
(s)
 char * s;
#endif
{
   return MakeIdent (s, strlen (s));
}


#ifndef yyWrite
#define yyWrite(s) (void) fputs (s, yyf)
#endif
#ifndef yyWriteNl
#define yyWriteNl (void) fputc ('\n', yyf)
#endif

#include "yyitcl.h"

static void yyExit ARGS ((void)) { rExit (1); }

void (* itcl_Exit) ARGS ((void)) = yyExit;

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
 (void) fprintf (stderr, "Error: module itcl, routine %s failed\n",
  yyFunction);
 itcl_Exit ();
}

void Interpret_itcl ARGS ((tTree yyP1));
static void get_objects ARGS ((tTree s));
static void get_param_objects ARGS ((tTree s));
static void add_variables ARGS ((tIdent yyP3, tTree yyP2, int attr));
static void include ARGS ((tTree yyP4));
static void add_global_vars ARGS ((tTree w));
static void add_variable_vars ARGS ((tTree w, int acc, int attr));
static void add_vars ARGS ((tTree yyP5, int acc));
static void add_foreach_vars ARGS ((tTree yyP6));
static void add_var ARGS ((tTree w, int acc, int attr));
static void add_var_2 ARGS ((tIdent i, tPosition pos, tTree w, tTree e, int acc, int attr));
static tTree IdentifyLocal ARGS ((tIdent i, tTree o));
static tTree IdentifyGlobal ARGS ((tIdent yyP7));
static tTree Identify ARGS ((tIdent yyP9, tTree yyP8));
static tTree IdentifyMember ARGS ((tIdent yyP11, tTree yyP10));
static tTree IdentifySuper ARGS ((tIdent yyP13, tTree yyP12));
static tTree IdentifyVariable ARGS ((tIdent yyP15, tTree yyP14));
static tTree IdentifyProcLocal ARGS ((tIdent i, tTree o));
static tTree IdentifyProcGlobal ARGS ((tIdent yyP16));
static tTree IdentifyProc ARGS ((tIdent yyP18, tTree yyP17));
static tTree IdentifyProcMember ARGS ((tIdent yyP20, tTree yyP19));
static tTree IdentifyProcSuper ARGS ((tIdent yyP22, tTree yyP21));
static tTree get_namespace ARGS ((tTree yyP23));
static void pass2 ARGS ((tTree t));
static void pass3 ARGS ((tTree yyP24));
tIdent make_one_word ARGS ((tTree yyP25));
static void make_one_word_2 ARGS ((tTree yyP26));
tTree Mword ARGS ((tTree yyP28, tTree yyP27));
tTree Mqualification ARGS ((tTree yyP29));
tTree Mstmt ARGS ((tTree yyP31, tTree yyP30));
static tTree Mstmt_1 ARGS ((tTree yyP33, tTree yyP32, int attr));
static tTree Mstmt_2 ARGS ((tTree yyP36, tIdent yyP35, tTree yyP34, int attr));
tTree Mblock_content ARGS ((tPosition yyP39, tPosition yyP38, tTree yyP37));
static tTree make_qualification ARGS ((tTree yyP40));
static tTree param_names ARGS ((tTree yyP41));
static tTree param_names_2 ARGS ((tTree yyP43, tTree yyP42));
static tTree param_names_3 ARGS ((tTree yyP45, tTree yyP44));
static tTree param_names_4 ARGS ((tTree yyP47, tTree yyP46));
static void get_end_pos ARGS ((tTree yyP48, tPosition * yyP49));
static void get_begin_pos ARGS ((tTree yyP50, tPosition * yyP51));
static void get_end_pos_2 ARGS ((tTree yyP52, tPosition * yyP53));
static rbool get_script ARGS ((tTree yyP54, tTree * yyP56, tPosition * yyP55));
static void itcl_info ARGS ((tTree yyP57));
static tTree get_env ARGS ((tTree yyP58));
static tTree get_env_2 ARGS ((tTree yyP59));
static tIdent get_ident ARGS ((tTree yyP60));
static void add_qualifications ARGS ((tIdent yyP62, tTree yyP61, int attr));
static void inherit_var ARGS ((tTree w));
static void inherit_var_2 ARGS ((tIdent yyP66, tPosition yyP65, tTree yyP64, tTree yyP63));
static void add_variable_quals ARGS ((tTree yyP67, int acc, int attr));
static void add_quals ARGS ((tTree yyP68, int acc));
static void add_foreach_quals ARGS ((tTree yyP69));
static void add_qual ARGS ((tTree w, int acc));
static void add_qual_2 ARGS ((tTree w, int acc));
static tTree use_qual ARGS ((tTree yyP70));
static tTree use_proc_qual ARGS ((tTree yyP71));
static tTree use_proc ARGS ((tTree yyP72));
static tTree use_word ARGS ((tTree yyP73));
static void use ARGS ((tTree yyP74, tPosition p, int acc));
static void use_2 ARGS ((tTree env, tPosition pos, int acc));
static void use_undef ARGS ((tIdent ident, tPosition pos));
static void use_proc_undef ARGS ((tIdent ident, tPosition pos));
static void dcl ARGS ((tTree yyP75, int attr));
static void dcl_2 ARGS ((tTree env, tPosition pos, int attr));
static tString current_namespace ARGS ((tTree yyP76));
static tString get_class ARGS ((tTree yyP77));
static tString get_class_name ARGS ((tTree yyP78));
static void to_names ARGS ((tTree yyP80, tString yyP79));
static void get_names ARGS ((tTree yyP81));
static void get_separator ARGS ((tTree yyP82));
static void relocate ARGS ((tTree yyP83, tTree new_env));
static tTree declare_object ARGS ((tTree yyP85, tTree yyP84));
static void use_proc_object ARGS ((tTree yyP87, tTree yyP86));
static void get_pos ARGS ((tTree yyP88, tPosition * yyP89));
static long add_attr ARGS ((tTree yyP90));

void Interpret_itcl
#if defined __STDC__ | defined __cplusplus
(register tTree yyP1)
#else
(yyP1)
 register tTree yyP1;
#endif
{
  if (yyP1->Kind == kprogram) {
  if (yyP1->program.stmts->stmts.env->Kind == kenv) {
/* line 187 "itcl.puma" */
  {
/* line 188 "itcl.puma" */
   yyP1->program.stmts->stmts.env->env.objects = genv;
/* line 189 "itcl.puma" */
   genv = yyP1->program.stmts->stmts.env;
/* line 190 "itcl.puma" */
   cur_class_ptr = null;
/* line 190 "itcl.puma" */
   cur_proc_ptr = null;
/* line 191 "itcl.puma" */
   get_objects (yyP1->program.stmts);
/* line 192 "itcl.puma" */
  if (need_pass_2) {
      cur_class_ptr = null; cur_proc_ptr = null;
      pass2 (yyP1->program.stmts);
   }

/* line 197 "itcl.puma" */
   cur_class_ptr = null;
/* line 197 "itcl.puma" */
   cur_proc_ptr = null;
/* line 198 "itcl.puma" */
   pass3 (yyP1->program.stmts);
  }
   return;

  }
  }
;
}

static void get_objects
#if defined __STDC__ | defined __cplusplus
(register tTree s)
#else
(s)
 register tTree s;
#endif
{
 yyRecursion:

  switch (s->Kind) {
  case kproc_c:
  case kproc:
  case kmethod:
  case kbody:
  case kconfigbody:
  case kconstructor:
  case kdestructor:
  if (s->proc_c.qualification->Kind == klocal_ident) {
/* line 205 "itcl.puma" */
 {
  register tTree yyV1;
  tString prev_proc;
  {
/* line 208 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->proc_c.qualification->local_ident.ident;
    yyV1->object.next = s->proc_c.env -> env . objects;
   s->proc_c.env -> env . objects = yyV1;
/* line 209 "itcl.puma" */
   dcl (s->proc_c.env -> env . objects, s->proc_c.attribute);
/* line 210 "itcl.puma" */
   get_objects (s->proc_c.next);
/* line 211 "itcl.puma" */

/* line 211 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 212 "itcl.puma" */
   cur_proc_ptr = GetCStr (s->proc_c.qualification->local_ident.ident);
/* line 213 "itcl.puma" */
   get_param_objects (s->proc_c.param_names);
/* line 214 "itcl.puma" */
   get_objects (s->proc_c.block);
/* line 215 "itcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc_c.qualification->Kind == kglobal_ident) {
/* line 217 "itcl.puma" */
 {
  register tTree yyV1;
  tString prev_proc;
  {
/* line 220 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->proc_c.qualification->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 221 "itcl.puma" */
   dcl (genv -> env . objects, s->proc_c.attribute);
/* line 222 "itcl.puma" */
   get_objects (s->proc_c.next);
/* line 223 "itcl.puma" */

/* line 223 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 224 "itcl.puma" */
   cur_proc_ptr = GetCStr (s->proc_c.qualification->global_ident.ident);
/* line 225 "itcl.puma" */
   get_param_objects (s->proc_c.param_names);
/* line 226 "itcl.puma" */
   get_objects (s->proc_c.block);
/* line 227 "itcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc_c.qualification->Kind == klocal_text) {
/* line 229 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 233 "itcl.puma" */
   get_objects (s->proc_c.next);
/* line 234 "itcl.puma" */

/* line 234 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 235 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (s->proc_c.qualification));
/* line 236 "itcl.puma" */
   get_param_objects (s->proc_c.param_names);
/* line 237 "itcl.puma" */
   get_objects (s->proc_c.block);
/* line 238 "itcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc_c.qualification->Kind == kglobal_text) {
/* line 229 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 233 "itcl.puma" */
   get_objects (s->proc_c.next);
/* line 234 "itcl.puma" */

/* line 234 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 235 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (s->proc_c.qualification));
/* line 236 "itcl.puma" */
   get_param_objects (s->proc_c.param_names);
/* line 237 "itcl.puma" */
   get_objects (s->proc_c.block);
/* line 238 "itcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
/* line 240 "itcl.puma" */
  {
/* line 241 "itcl.puma" */
   need_pass_2 = rtrue;
/* line 242 "itcl.puma" */
   s = s->proc_c.next;
   goto yyRecursion;
  }

  case knamespace:
  if (s->namespace.qualification->Kind == klocal_ident) {
/* line 244 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 247 "itcl.puma" */

/* line 247 "itcl.puma" */
   obj = IdentifyLocal (s->namespace.qualification->local_ident.ident, s->namespace.env -> env . objects);
/* line 248 "itcl.puma" */
  if (obj != NoTree && obj->object.object->Kind == knamespace) {
      relocate (s->namespace.block, obj->object.object->namespace.block->texts.env);
      use (obj, s->namespace.qualification->local_ident.pos, PAF_REF_READ);
   } else {
      s->namespace.env->env.objects = mobject (s, s->namespace.qualification->local_ident.ident, s->namespace.env->env.objects);
      dcl (s->namespace.env->env.objects, s->namespace.attribute);
   }

/* line 256 "itcl.puma" */
   get_objects (s->namespace.next);
/* line 257 "itcl.puma" */

/* line 257 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 258 "itcl.puma" */
   cur_class_ptr = GetCStr (s->namespace.qualification->local_ident.ident);
/* line 259 "itcl.puma" */
   get_objects (s->namespace.block);
/* line 260 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == kglobal_ident) {
/* line 262 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 265 "itcl.puma" */

/* line 265 "itcl.puma" */
   obj = IdentifyGlobal (s->namespace.qualification->global_ident.ident);
/* line 266 "itcl.puma" */
  if (obj != NoTree && obj->object.object->Kind == knamespace) {
      relocate (s->namespace.block, obj->object.object->namespace.block->texts.env);
      use (obj, s->namespace.qualification->global_ident.pos, PAF_REF_READ);
   } else {
      genv->env.objects = mobject (s, s->namespace.qualification->global_ident.ident, genv->env.objects);
      dcl (genv->env.objects, s->namespace.attribute);
   }

/* line 274 "itcl.puma" */
   get_objects (s->namespace.next);
/* line 275 "itcl.puma" */

/* line 275 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 276 "itcl.puma" */
   cur_class_ptr = GetCStr (s->namespace.qualification->global_ident.ident);
/* line 277 "itcl.puma" */
   get_objects (s->namespace.block);
/* line 278 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == klocal_text) {
/* line 280 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 282 "itcl.puma" */
   get_objects (s->namespace.next);
/* line 283 "itcl.puma" */

/* line 283 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 284 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->namespace.qualification));
/* line 285 "itcl.puma" */
   get_objects (s->namespace.block);
/* line 286 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == kglobal_text) {
/* line 280 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 282 "itcl.puma" */
   get_objects (s->namespace.next);
/* line 283 "itcl.puma" */

/* line 283 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 284 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->namespace.qualification));
/* line 285 "itcl.puma" */
   get_objects (s->namespace.block);
/* line 286 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
/* line 288 "itcl.puma" */
  {
/* line 289 "itcl.puma" */
   need_pass_2 = rtrue;
/* line 290 "itcl.puma" */
   s = s->namespace.next;
   goto yyRecursion;
  }

  case kclass:
  if (s->class.qualification->Kind == klocal_ident) {
/* line 292 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 298 "itcl.puma" */

/* line 298 "itcl.puma" */
   obj = IdentifyLocal (s->class.qualification->local_ident.ident, s->class.env -> env . objects);
/* line 299 "itcl.puma" */
  if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
      relocate (s->class.block, obj->object.object->class.block->texts.env);
      use (obj, s->class.qualification->local_ident.pos, PAF_REF_READ);
   } else {
      s->class.env->env.objects = mobject (s, s->class.qualification->local_ident.ident, s->class.env->env.objects);
      dcl (s->class.env->env.objects, s->class.attribute);
   }

/* line 307 "itcl.puma" */
   get_objects (s->class.next);
/* line 308 "itcl.puma" */

/* line 308 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 309 "itcl.puma" */
   cur_class_ptr = GetCStr (s->class.qualification->local_ident.ident);
/* line 310 "itcl.puma" */
   get_objects (s->class.block);
/* line 311 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->class.qualification->Kind == kglobal_ident) {
/* line 313 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 319 "itcl.puma" */

/* line 319 "itcl.puma" */
   obj = IdentifyGlobal (s->class.qualification->global_ident.ident);
/* line 320 "itcl.puma" */
  if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
      relocate (s->class.block, obj->object.object->class.block->texts.env);
      use (obj, s->class.qualification->global_ident.pos, PAF_REF_READ);
   } else {
      s->class.env->env.objects = mobject (s, s->class.qualification->global_ident.ident, s->class.env->env.objects);
      dcl (s->class.env->env.objects, s->class.attribute);
   }

/* line 328 "itcl.puma" */
   get_objects (s->class.next);
/* line 329 "itcl.puma" */

/* line 329 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 330 "itcl.puma" */
   cur_class_ptr = GetCStr (s->class.qualification->global_ident.ident);
/* line 331 "itcl.puma" */
   get_objects (s->class.block);
/* line 332 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->class.qualification->Kind == klocal_text) {
/* line 334 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 342 "itcl.puma" */
   get_objects (s->class.next);
/* line 343 "itcl.puma" */

/* line 343 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 344 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->class.qualification));
/* line 345 "itcl.puma" */
   get_objects (s->class.block);
/* line 346 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->class.qualification->Kind == kglobal_text) {
/* line 334 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 342 "itcl.puma" */
   get_objects (s->class.next);
/* line 343 "itcl.puma" */

/* line 343 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 344 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->class.qualification));
/* line 345 "itcl.puma" */
   get_objects (s->class.block);
/* line 346 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
/* line 348 "itcl.puma" */
  {
/* line 350 "itcl.puma" */
   need_pass_2 = rtrue;
/* line 351 "itcl.puma" */
   s = s->class.next;
   goto yyRecursion;
  }

  case kitcl_class:
  if (s->itcl_class.qualification->Kind == klocal_ident) {
/* line 292 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 298 "itcl.puma" */

/* line 298 "itcl.puma" */
   obj = IdentifyLocal (s->itcl_class.qualification->local_ident.ident, s->itcl_class.env -> env . objects);
/* line 299 "itcl.puma" */
  if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
      relocate (s->itcl_class.block, obj->object.object->class.block->texts.env);
      use (obj, s->itcl_class.qualification->local_ident.pos, PAF_REF_READ);
   } else {
      s->itcl_class.env->env.objects = mobject (s, s->itcl_class.qualification->local_ident.ident, s->itcl_class.env->env.objects);
      dcl (s->itcl_class.env->env.objects, s->itcl_class.attribute);
   }

/* line 307 "itcl.puma" */
   get_objects (s->itcl_class.next);
/* line 308 "itcl.puma" */

/* line 308 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 309 "itcl.puma" */
   cur_class_ptr = GetCStr (s->itcl_class.qualification->local_ident.ident);
/* line 310 "itcl.puma" */
   get_objects (s->itcl_class.block);
/* line 311 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->itcl_class.qualification->Kind == kglobal_ident) {
/* line 313 "itcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 319 "itcl.puma" */

/* line 319 "itcl.puma" */
   obj = IdentifyGlobal (s->itcl_class.qualification->global_ident.ident);
/* line 320 "itcl.puma" */
  if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
      relocate (s->itcl_class.block, obj->object.object->class.block->texts.env);
      use (obj, s->itcl_class.qualification->global_ident.pos, PAF_REF_READ);
   } else {
      s->itcl_class.env->env.objects = mobject (s, s->itcl_class.qualification->global_ident.ident, s->itcl_class.env->env.objects);
      dcl (s->itcl_class.env->env.objects, s->itcl_class.attribute);
   }

/* line 328 "itcl.puma" */
   get_objects (s->itcl_class.next);
/* line 329 "itcl.puma" */

/* line 329 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 330 "itcl.puma" */
   cur_class_ptr = GetCStr (s->itcl_class.qualification->global_ident.ident);
/* line 331 "itcl.puma" */
   get_objects (s->itcl_class.block);
/* line 332 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->itcl_class.qualification->Kind == klocal_text) {
/* line 334 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 342 "itcl.puma" */
   get_objects (s->itcl_class.next);
/* line 343 "itcl.puma" */

/* line 343 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 344 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->itcl_class.qualification));
/* line 345 "itcl.puma" */
   get_objects (s->itcl_class.block);
/* line 346 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->itcl_class.qualification->Kind == kglobal_text) {
/* line 334 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 342 "itcl.puma" */
   get_objects (s->itcl_class.next);
/* line 343 "itcl.puma" */

/* line 343 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 344 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->itcl_class.qualification));
/* line 345 "itcl.puma" */
   get_objects (s->itcl_class.block);
/* line 346 "itcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
/* line 348 "itcl.puma" */
  {
/* line 350 "itcl.puma" */
   need_pass_2 = rtrue;
/* line 351 "itcl.puma" */
   s = s->itcl_class.next;
   goto yyRecursion;
  }

  case kstmt:
  if (s->stmt.words->Kind == kone_word) {
  if (Tree_IsType (s->stmt.words->one_word.next, kword_c)) {
/* line 353 "itcl.puma" */
  {
/* line 355 "itcl.puma" */
   add_variables (s->stmt.words->one_word.ident, s->stmt.words->one_word.next, default_attr);
/* line 356 "itcl.puma" */
   get_objects (s->stmt.words);
/* line 357 "itcl.puma" */
   s = s->stmt.next;
   goto yyRecursion;
  }

  }
  }
/* line 359 "itcl.puma" */
  {
/* line 360 "itcl.puma" */
   get_objects (s->stmt.words);
/* line 361 "itcl.puma" */
   s = s->stmt.next;
   goto yyRecursion;
  }

  case kblock:
/* line 363 "itcl.puma" */
  {
/* line 364 "itcl.puma" */
   get_objects (s->block.stmts);
/* line 365 "itcl.puma" */
   s = s->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 367 "itcl.puma" */
  {
/* line 368 "itcl.puma" */
   get_objects (s->content.qualification);
/* line 369 "itcl.puma" */
   s = s->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 371 "itcl.puma" */
  {
/* line 372 "itcl.puma" */
   get_objects (s->block_content.stmts);
/* line 373 "itcl.puma" */
   s = s->block_content.next;
   goto yyRecursion;
  }

  case ktext:
  case kcharacter:
  case kident:
/* line 375 "itcl.puma" */
  {
/* line 376 "itcl.puma" */
   s = s->text.next;
   goto yyRecursion;
  }

  case kone_word:
/* line 378 "itcl.puma" */
  {
/* line 379 "itcl.puma" */
   s = s->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 381 "itcl.puma" */
  {
/* line 382 "itcl.puma" */
   get_objects (s->qual_word.qualification);
/* line 383 "itcl.puma" */
   s = s->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 385 "itcl.puma" */
  {
/* line 386 "itcl.puma" */
   get_objects (s->qual_words.qualifications);
/* line 387 "itcl.puma" */
   s = s->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 389 "itcl.puma" */
  {
/* line 390 "itcl.puma" */
   get_objects (s->one_qualification.qualification);
/* line 391 "itcl.puma" */
   s = s->one_qualification.next;
   goto yyRecursion;
  }

  case klocal_text:
/* line 393 "itcl.puma" */
  {
/* line 394 "itcl.puma" */
   s = s->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 396 "itcl.puma" */
  {
/* line 397 "itcl.puma" */
   s = s->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 399 "itcl.puma" */
  {
/* line 400 "itcl.puma" */
   s = s->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 402 "itcl.puma" */
  {
/* line 403 "itcl.puma" */
   get_objects (s->complex_qual.qualification);
/* line 404 "itcl.puma" */
   s = s->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 406 "itcl.puma" */
  {
/* line 407 "itcl.puma" */
   get_objects (s->subscription.qualification);
/* line 408 "itcl.puma" */
   s = s->subscription.index;
   goto yyRecursion;
  }

  }

;
}

static void get_param_objects
#if defined __STDC__ | defined __cplusplus
(register tTree s)
#else
(s)
 register tTree s;
#endif
{
 yyRecursion:
  if (s->Kind == kone_word) {
/* line 415 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 416 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->one_word.ident;
    yyV1->object.next = s->one_word.env -> env . objects;
   s->one_word.env -> env . objects = yyV1;
/* line 417 "itcl.puma" */
   dcl (s->one_word.env -> env . objects, PAF_PUBLIC);
/* line 418 "itcl.puma" */
   s = s->one_word.next;
   goto yyRecursion;
  }
 }

  }
  if (Tree_IsType (s, kword_c)) {
/* line 420 "itcl.puma" */
  {
/* line 421 "itcl.puma" */
   s = s->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_variables
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP3, register tTree yyP2, register int attr)
#else
(yyP3, yyP2, attr)
 register tIdent yyP3;
 register tTree yyP2;
 register int attr;
#endif
{
 yyRecursion:
  if (equaltIdent (yyP3, (iset))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 428 "itcl.puma" */
  {
/* line 429 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iset))) {
/* line 431 "itcl.puma" */
  {
/* line 432 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (iglobal))) {
/* line 434 "itcl.puma" */
  {
/* line 435 "itcl.puma" */
   add_global_vars (yyP2);
  }
   return;

  }
  if (equaltIdent (yyP3, (ivariable))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 437 "itcl.puma" */
  {
/* line 438 "itcl.puma" */
   add_variable_vars (yyP2, PAF_REF_READ, attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (ivariable))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
  if (yyP2->word_c.next->word_c.next->Kind == kqual_word) {
/* line 440 "itcl.puma" */
  {
/* line 443 "itcl.puma" */
   relocate (yyP2->word_c.next->word_c.next->qual_word.qualification, menv (mnoobject (), yyP2->word_c.next->word_c.next->qual_word.env, yyP2->word_c.next->word_c.next));
/* line 444 "itcl.puma" */
   add_variable_vars (yyP2, PAF_REF_WRITE, attr);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (ivariable))) {
/* line 446 "itcl.puma" */
  {
/* line 447 "itcl.puma" */
   add_variable_vars (yyP2, PAF_REF_WRITE, attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (icommon))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 449 "itcl.puma" */
  {
/* line 450 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (icommon))) {
/* line 452 "itcl.puma" */
  {
/* line 453 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (ipublic))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
/* line 455 "itcl.puma" */
  {
/* line 457 "itcl.puma" */
   if (! (yyP2->one_word.ident == ivariable || yyP2->one_word.ident == icommon)) goto yyL9;
  {
/* line 458 "itcl.puma" */
   yyP3 = yyP2->one_word.ident;
   yyP2 = yyP2->one_word.next;
   attr = PAF_PUBLIC;
   goto yyRecursion;
  }
  }
yyL9:;

  }
  }
  }
  if (equaltIdent (yyP3, (ipublic))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 460 "itcl.puma" */
  {
/* line 461 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, PAF_PUBLIC);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (ipublic))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
  if (yyP2->word_c.next->word_c.next->Kind == kqual_word) {
/* line 463 "itcl.puma" */
  {
/* line 466 "itcl.puma" */
   relocate (yyP2->word_c.next->word_c.next->qual_word.qualification, menv (mnoobject (), yyP2->word_c.next->word_c.next->qual_word.env, yyP2->word_c.next->word_c.next));
/* line 467 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PUBLIC);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (ipublic))) {
/* line 469 "itcl.puma" */
  {
/* line 470 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PUBLIC);
  }
   return;

  }
  if (equaltIdent (yyP3, (iprotected))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
/* line 472 "itcl.puma" */
  {
/* line 474 "itcl.puma" */
   if (! (yyP2->one_word.ident == ivariable || yyP2->one_word.ident == icommon)) goto yyL13;
  {
/* line 475 "itcl.puma" */
   yyP3 = yyP2->one_word.ident;
   yyP2 = yyP2->one_word.next;
   attr = PAF_PROTECTED;
   goto yyRecursion;
  }
  }
yyL13:;

  }
  }
  }
  if (equaltIdent (yyP3, (iprotected))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 477 "itcl.puma" */
  {
/* line 478 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, PAF_PROTECTED);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iprotected))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
  if (yyP2->word_c.next->word_c.next->Kind == kqual_word) {
/* line 480 "itcl.puma" */
  {
/* line 483 "itcl.puma" */
   relocate (yyP2->word_c.next->word_c.next->qual_word.qualification, menv (mnoobject (), yyP2->word_c.next->word_c.next->qual_word.env, yyP2->word_c.next->word_c.next));
/* line 484 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PROTECTED);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (iprotected))) {
/* line 486 "itcl.puma" */
  {
/* line 487 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PROTECTED);
  }
   return;

  }
  if (equaltIdent (yyP3, (iprivate))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
/* line 489 "itcl.puma" */
  {
/* line 491 "itcl.puma" */
   if (! (yyP2->one_word.ident == ivariable || yyP2->one_word.ident == icommon)) goto yyL17;
  {
/* line 492 "itcl.puma" */
   yyP3 = yyP2->one_word.ident;
   yyP2 = yyP2->one_word.next;
   attr = PAF_PRIVATE;
   goto yyRecursion;
  }
  }
yyL17:;

  }
  }
  }
  if (equaltIdent (yyP3, (iprivate))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 494 "itcl.puma" */
  {
/* line 495 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, PAF_PRIVATE);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iprivate))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
  if (yyP2->word_c.next->word_c.next->Kind == kqual_word) {
/* line 497 "itcl.puma" */
  {
/* line 500 "itcl.puma" */
   relocate (yyP2->word_c.next->word_c.next->qual_word.qualification, menv (mnoobject (), yyP2->word_c.next->word_c.next->qual_word.env, yyP2->word_c.next->word_c.next));
/* line 501 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PRIVATE);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (iprivate))) {
/* line 503 "itcl.puma" */
  {
/* line 504 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, PAF_PRIVATE);
  }
   return;

  }
  if (equaltIdent (yyP3, (iinherit))) {
/* line 514 "itcl.puma" */
  {
/* line 515 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (equaltIdent (yyP3, (iappend))) {
/* line 517 "itcl.puma" */
  {
/* line 518 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (ilappend))) {
/* line 520 "itcl.puma" */
  {
/* line 521 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (iarray))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iset))) {
/* line 523 "itcl.puma" */
  {
/* line 525 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (iarray))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 527 "itcl.puma" */
  {
/* line 528 "itcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (ibinary))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iscan))) {
/* line 530 "itcl.puma" */
  {
/* line 534 "itcl.puma" */
   add_vars (yyP2->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP3, (iscan))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
  if (Tree_IsType (yyP2->word_c.next->word_c.next, kword_c)) {
/* line 536 "itcl.puma" */
  {
/* line 539 "itcl.puma" */
   add_vars (yyP2->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (iforeach))) {
/* line 541 "itcl.puma" */
  {
/* line 542 "itcl.puma" */
   add_foreach_vars (yyP2);
  }
   return;

  }
  if (equaltIdent (yyP3, (ivwait))) {
/* line 544 "itcl.puma" */
  {
/* line 545 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (iincr))) {
/* line 547 "itcl.puma" */
  {
/* line 548 "itcl.puma" */
   add_var (yyP2, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (iparray))) {
/* line 550 "itcl.puma" */
  {
/* line 551 "itcl.puma" */
   add_var (yyP2, PAF_REF_READ, default_attr);
  }
   return;

  }
  if (equaltIdent (yyP3, (icatch))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 553 "itcl.puma" */
  {
/* line 554 "itcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (igets))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 556 "itcl.puma" */
  {
/* line 557 "itcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iexists))) {
/* line 559 "itcl.puma" */
  {
/* line 561 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (iinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iargs))) {
/* line 563 "itcl.puma" */
  {
/* line 565 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (iinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (ibody))) {
/* line 567 "itcl.puma" */
  {
/* line 569 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (iinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (idefault))) {
/* line 571 "itcl.puma" */
  {
/* line 575 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ, default_attr);
/* line 576 "itcl.puma" */
   add_var (yyP2->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP3, (itkinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (ivariable))) {
/* line 578 "itcl.puma" */
  {
/* line 580 "itcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ, default_attr);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (ifile))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (ilstat))) {
/* line 582 "itcl.puma" */
  {
/* line 585 "itcl.puma" */
   add_var (yyP2->one_word.next->word_c.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP3, (ifile))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (Tree_IsType (yyP2->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (istat))) {
/* line 587 "itcl.puma" */
  {
/* line 590 "itcl.puma" */
   add_var (yyP2->one_word.next->word_c.next, PAF_REF_WRITE, default_attr);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP3, (iunset))) {
/* line 592 "itcl.puma" */
  {
/* line 593 "itcl.puma" */
   add_vars (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (isource))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 603 "itcl.puma" */
  {
/* line 604 "itcl.puma" */
   include (yyP2);
  }
   return;

  }
  }
;
}

static void include
#if defined __STDC__ | defined __cplusplus
(register tTree yyP4)
#else
(yyP4)
 register tTree yyP4;
#endif
{
  if (yyP4->Kind == kone_word) {
/* line 611 "itcl.puma" */
  {
/* line 612 "itcl.puma" */
   put_symbol (PAF_INCLUDE_DEF, NULL, GetCStr (yyP4->one_word.ident), current_file, (int)  yyP4->one_word.pos . Line, (int)  yyP4->one_word.pos . Column - 1, (int)  yyP4->one_word.pos . Line, (int)  (yyP4->one_word.pos . Column + StLength (GetStringRef (yyP4->one_word.ident)) - 1), 0, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
  if (yyP4->Kind == kqual_word) {
/* line 617 "itcl.puma" */
   return;

  }
  if (yyP4->Kind == kqual_words) {
/* line 619 "itcl.puma" */
   return;

  }
;
}

static void add_global_vars
#if defined __STDC__ | defined __cplusplus
(register tTree w)
#else
(w)
 register tTree w;
#endif
{
 yyRecursion:
  if (w->Kind == kone_word) {
/* line 635 "itcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 636 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 637 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->one_word.ident;
    yyV2->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV2;
/* line 638 "itcl.puma" */
   w->one_word.env = genv;
/* line 639 "itcl.puma" */
   dcl (genv -> env . objects, PAF_PUBLIC);
/* line 640 "itcl.puma" */
   w = w->one_word.next;
   goto yyRecursion;
  }
 }

  }
  if (w->Kind == kqual_word) {
  if (w->qual_word.qualification->Kind == kglobal_ident) {
/* line 652 "itcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 654 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w->qual_word.qualification;
    yyV1->object.ident = w->qual_word.qualification->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 655 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w->qual_word.qualification;
    yyV2->object.ident = w->qual_word.qualification->global_ident.ident;
    yyV2->object.next = w->qual_word.qualification->global_ident.env -> env . objects;
   w->qual_word.qualification->global_ident.env -> env . objects = yyV2;
/* line 656 "itcl.puma" */
   w->qual_word.qualification->global_ident.env = genv;
/* line 657 "itcl.puma" */
   dcl (w->qual_word.qualification->global_ident.env -> env . objects, PAF_PUBLIC);
/* line 658 "itcl.puma" */
   w = w->qual_word.next;
   goto yyRecursion;
  }
 }

  }
/* line 660 "itcl.puma" */
  {
/* line 662 "itcl.puma" */
   w = w->qual_word.next;
   goto yyRecursion;
  }

  }
  if (w->Kind == kqual_words) {
/* line 660 "itcl.puma" */
  {
/* line 662 "itcl.puma" */
   w = w->qual_words.next;
   goto yyRecursion;
  }

  }
;
}

static void add_variable_vars
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc, register int attr)
#else
(w, acc, attr)
 register tTree w;
 register int acc;
 register int attr;
#endif
{
/* line 669 "itcl.puma" */
 tTree obj, e2; 
 yyRecursion:
  if (w->Kind == kone_word) {
  if (w->one_word.env->Kind == kenv) {
/* line 671 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 672 "itcl.puma" */
   if (! (! Tree_IsType (w->one_word.env->env.object, kproc_c))) goto yyL1;
  {
/* line 673 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV1;
/* line 674 "itcl.puma" */
   dcl (w->one_word.env -> env . objects, attr);
/* line 675 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (w->one_word.env->env.objects, w->one_word.pos, acc); 
  }
  }
   return;
 }
yyL1:;

  }
/* line 677 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 678 "itcl.puma" */
   e2 = get_namespace (w->one_word.env);
/* line 679 "itcl.puma" */
   obj = IdentifyLocal (w->one_word.ident, e2 -> env . objects);
/* line 680 "itcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  {
/* line 681 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = obj -> object . object;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV1;
/* line 682 "itcl.puma" */
   use (w->one_word.env -> env . objects, w->one_word.pos, acc);
  }
  }
   return;
 }
yyL2:;

/* line 684 "itcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 685 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = e2 -> env . objects;
   e2 -> env . objects = yyV1;
/* line 686 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->one_word.ident;
    yyV2->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV2;
/* line 687 "itcl.puma" */
   w->one_word.env = e2;
/* line 688 "itcl.puma" */
   dcl (e2 -> env . objects, attr);
/* line 689 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (e2->env.objects, w->one_word.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == kqual_word) {
/* line 691 "itcl.puma" */
  {
/* line 692 "itcl.puma" */
   w = w->qual_word.qualification;
   goto yyRecursion;
  }

  }
  if (w->Kind == kqual_words) {
/* line 694 "itcl.puma" */
   return;

  }
  if (w->Kind == kglobal_ident) {
/* line 696 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 697 "itcl.puma" */
   obj = IdentifyGlobal (w->global_ident.ident);
/* line 698 "itcl.puma" */
   if (! (obj != NULL)) goto yyL6;
  {
/* line 699 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = obj -> object . object;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = w->global_ident.env -> env . objects;
   w->global_ident.env -> env . objects = yyV1;
/* line 700 "itcl.puma" */
   use (w->global_ident.env -> env . objects, w->global_ident.pos, acc);
  }
  }
   return;
 }
yyL6:;

/* line 702 "itcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 703 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 704 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->global_ident.ident;
    yyV2->object.next = w->global_ident.env -> env . objects;
   w->global_ident.env -> env . objects = yyV2;
/* line 705 "itcl.puma" */
   w->global_ident.env = genv;
/* line 706 "itcl.puma" */
   dcl (genv -> env . objects, attr);
/* line 707 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_ident.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == kqualification) {
/* line 709 "itcl.puma" */
  {
/* line 711 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->Kind == kcomplex_qual) {
/* line 709 "itcl.puma" */
  {
/* line 711 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
;
}

static void add_vars
#if defined __STDC__ | defined __cplusplus
(register tTree yyP5, register int acc)
#else
(yyP5, acc)
 register tTree yyP5;
 register int acc;
#endif
{
 yyRecursion:
  if (Tree_IsType (yyP5, kword_c)) {
/* line 718 "itcl.puma" */
  {
/* line 719 "itcl.puma" */
   add_var (yyP5, acc, default_attr);
/* line 720 "itcl.puma" */
   yyP5 = yyP5->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_foreach_vars
#if defined __STDC__ | defined __cplusplus
(register tTree yyP6)
#else
(yyP6)
 register tTree yyP6;
#endif
{
 yyRecursion:
  if (yyP6->Kind == kqual_word) {
  if (Tree_IsType (yyP6->qual_word.next, kword_c)) {
  if (yyP6->qual_word.qualification->Kind == klocal_text) {
  if (yyP6->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 727 "itcl.puma" */
  {
/* line 729 "itcl.puma" */
   add_foreach_vars (yyP6->qual_word.qualification->local_text.texts->block.stmts);
/* line 730 "itcl.puma" */
   yyP6 = yyP6->qual_word.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  }
  }
  if (Tree_IsType (yyP6, kword_c)) {
  if (Tree_IsType (yyP6->word_c.next, kword_c)) {
/* line 732 "itcl.puma" */
  {
/* line 733 "itcl.puma" */
   add_var (yyP6, PAF_REF_WRITE, default_attr);
/* line 734 "itcl.puma" */
   yyP6 = yyP6->word_c.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  if (yyP6->Kind == kstmt) {
/* line 736 "itcl.puma" */
  {
/* line 737 "itcl.puma" */
   add_vars (yyP6->stmt.words, PAF_REF_WRITE);
/* line 738 "itcl.puma" */
   yyP6 = yyP6->stmt.next;
   goto yyRecursion;
  }

  }
;
}

static void add_var
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc, register int attr)
#else
(w, acc, attr)
 register tTree w;
 register int acc;
 register int attr;
#endif
{
  if (w->Kind == kone_word) {
/* line 745 "itcl.puma" */
  {
/* line 746 "itcl.puma" */
   add_var_2 (w->one_word.ident, w->one_word.pos, w, w->one_word.env, acc, attr);
  }
   return;

  }
  if (w->Kind == kqual_word) {
  if (w->qual_word.qualification->Kind == ksubscription) {
  if (w->qual_word.qualification->subscription.qualification->Kind == klocal_ident) {
/* line 748 "itcl.puma" */
  {
/* line 750 "itcl.puma" */
   add_var_2 (w->qual_word.qualification->subscription.qualification->local_ident.ident, w->qual_word.qualification->subscription.qualification->local_ident.pos, w->qual_word.qualification->subscription.qualification, w->qual_word.env, acc, attr);
  }
   return;

  }
/* line 755 "itcl.puma" */
  {
/* line 759 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kqualification) {
/* line 755 "itcl.puma" */
  {
/* line 759 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kcomplex_qual) {
/* line 755 "itcl.puma" */
  {
/* line 759 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kglobal_ident) {
/* line 755 "itcl.puma" */
  {
/* line 759 "itcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  }
  if (w->Kind == kobj_word) {
/* line 752 "itcl.puma" */
  {
/* line 753 "itcl.puma" */
   add_var_2 (w->obj_word.ident, w->obj_word.pos, w, w->obj_word.env, acc, attr);
  }
   return;

  }
;
}

static void add_var_2
#if defined __STDC__ | defined __cplusplus
(register tIdent i, tPosition pos, register tTree w, register tTree e, register int acc, register int attr)
#else
(i, pos, w, e, acc, attr)
 register tIdent i;
 tPosition pos;
 register tTree w;
 register tTree e;
 register int acc;
 register int attr;
#endif
{
/* line 767 "itcl.puma" */
 {
  tTree obj;
  {
/* line 768 "itcl.puma" */

/* line 768 "itcl.puma" */
   obj = IdentifyVariable (i, e);
/* line 769 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 770 "itcl.puma" */
   use (obj, pos, acc);
  }
  }
   return;
 }
yyL1:;

/* line 772 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 773 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = i;
    yyV1->object.next = e -> env . objects;
   e -> env . objects = yyV1;
/* line 774 "itcl.puma" */
   dcl (e -> env . objects, attr);
/* line 775 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (e->env.objects, pos, acc); 
  }
   return;
 }

;
}

static tTree IdentifyLocal
#if defined __STDC__ | defined __cplusplus
(register tIdent i, register tTree o)
#else
(i, o)
 register tIdent i;
 register tTree o;
#endif
{
 yyRecursion:
  if (o->Kind == kobject) {
/* line 782 "itcl.puma" */
  {
/* line 783 "itcl.puma" */
   if (! (o->object.ident == i && ! Tree_IsType (o->object.object, kproc_c))) goto yyL1;
  }
   return o;
yyL1:;

/* line 786 "itcl.puma" */
   o = o->object.next;
   goto yyRecursion;

  }
/* line 789 "itcl.puma" */
   return NULL;

}

static tTree IdentifyGlobal
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP7)
#else
(yyP7)
 register tIdent yyP7;
#endif
{
/* line 797 "itcl.puma" */
   return IdentifyLocal (yyP7, genv -> env . objects);

}

static tTree Identify
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP9, register tTree yyP8)
#else
(yyP9, yyP8)
 register tIdent yyP9;
 register tTree yyP8;
#endif
{
 yyRecursion:
/* line 803 "itcl.puma" */
 {
  tTree obj;
  {
/* line 804 "itcl.puma" */

/* line 804 "itcl.puma" */
   obj = IdentifyMember (yyP9, yyP8);
/* line 805 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

  if (yyP8->Kind == kenv) {
  if (yyP8->env.env == NULL) {
/* line 808 "itcl.puma" */
   return NULL;

  }
/* line 811 "itcl.puma" */
   yyP8 = yyP8->env.env;
   goto yyRecursion;

  }
  if (yyP8->Kind == kenv2) {
/* line 811 "itcl.puma" */
   yyP8 = yyP8->env2.env2;
   goto yyRecursion;

  }
/* line 815 "itcl.puma" */
   return NULL;

}

static tTree IdentifyMember
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP11, register tTree yyP10)
#else
(yyP11, yyP10)
 register tIdent yyP11;
 register tTree yyP10;
#endif
{
  if (yyP10->Kind == kenv) {
/* line 823 "itcl.puma" */
 {
  tTree obj;
  {
/* line 824 "itcl.puma" */

/* line 824 "itcl.puma" */
   obj = IdentifyLocal (yyP11, yyP10->env.objects);
/* line 825 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

/* line 828 "itcl.puma" */
   return IdentifySuper (yyP11, yyP10->env.env);

  }
/* line 831 "itcl.puma" */
   return NULL;

}

static tTree IdentifySuper
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP13, register tTree yyP12)
#else
(yyP13, yyP12)
 register tIdent yyP13;
 register tTree yyP12;
#endif
{
 yyRecursion:
  if (yyP12 == NULL) {
/* line 839 "itcl.puma" */
   return NULL;

  }
  if (yyP12->Kind == kenv2) {
/* line 842 "itcl.puma" */
 {
  tTree obj;
  {
/* line 843 "itcl.puma" */

/* line 843 "itcl.puma" */
   obj = IdentifyMember (yyP13, yyP12->env2.env1);
/* line 844 "itcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  }
  {
   return obj;
  }
 }
yyL2:;

/* line 847 "itcl.puma" */
   yyP12 = yyP12->env2.env2;
   goto yyRecursion;

  }
/* line 850 "itcl.puma" */
   return NULL;

}

static tTree IdentifyVariable
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP15, register tTree yyP14)
#else
(yyP15, yyP14)
 register tIdent yyP15;
 register tTree yyP14;
#endif
{
  if (yyP14->Kind == kenv) {
/* line 858 "itcl.puma" */
 {
  tTree obj;
  {
/* line 859 "itcl.puma" */

/* line 859 "itcl.puma" */
   obj = IdentifyLocal (yyP15, yyP14->env.objects);
/* line 860 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

  if (yyP14->env.env == NULL) {
/* line 863 "itcl.puma" */
   return NULL;

  }
  if (yyP14->env.env->Kind == kenv) {
  if (yyP14->env.env->env.object->Kind == kclass) {
/* line 866 "itcl.puma" */
 {
  tTree obj;
  {
/* line 868 "itcl.puma" */

/* line 868 "itcl.puma" */
   obj = IdentifyMember (yyP15, yyP14->env.env);
/* line 869 "itcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  }
  {
   return obj;
  }
 }
yyL3:;

  }
  if (yyP14->env.env->env.object->Kind == kitcl_class) {
/* line 866 "itcl.puma" */
 {
  tTree obj;
  {
/* line 868 "itcl.puma" */

/* line 868 "itcl.puma" */
   obj = IdentifyMember (yyP15, yyP14->env.env);
/* line 869 "itcl.puma" */
   if (! (obj != NULL)) goto yyL4;
  }
  {
   return obj;
  }
 }
yyL4:;

  }
  }
  }
/* line 872 "itcl.puma" */
   return NULL;

}

static tTree IdentifyProcLocal
#if defined __STDC__ | defined __cplusplus
(register tIdent i, register tTree o)
#else
(i, o)
 register tIdent i;
 register tTree o;
#endif
{
 yyRecursion:
  if (o->Kind == kobject) {
/* line 880 "itcl.puma" */
  {
/* line 881 "itcl.puma" */
   if (! (o->object.ident == i && Tree_IsType (o->object.object, kproc_c))) goto yyL1;
  }
   return o;
yyL1:;

/* line 884 "itcl.puma" */
   o = o->object.next;
   goto yyRecursion;

  }
/* line 887 "itcl.puma" */
   return NULL;

}

static tTree IdentifyProcGlobal
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP16)
#else
(yyP16)
 register tIdent yyP16;
#endif
{
/* line 895 "itcl.puma" */
   return IdentifyProcLocal (yyP16, genv -> env . objects);

}

static tTree IdentifyProc
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP18, register tTree yyP17)
#else
(yyP18, yyP17)
 register tIdent yyP18;
 register tTree yyP17;
#endif
{
 yyRecursion:
/* line 901 "itcl.puma" */
 {
  tTree obj;
  {
/* line 902 "itcl.puma" */

/* line 902 "itcl.puma" */
   obj = IdentifyProcMember (yyP18, yyP17);
/* line 903 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

  if (yyP17->Kind == kenv) {
  if (yyP17->env.env == NULL) {
/* line 906 "itcl.puma" */
   return NULL;

  }
/* line 909 "itcl.puma" */
   yyP17 = yyP17->env.env;
   goto yyRecursion;

  }
  if (yyP17->Kind == kenv2) {
/* line 909 "itcl.puma" */
   yyP17 = yyP17->env2.env2;
   goto yyRecursion;

  }
/* line 913 "itcl.puma" */
   return NULL;

}

static tTree IdentifyProcMember
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP20, register tTree yyP19)
#else
(yyP20, yyP19)
 register tIdent yyP20;
 register tTree yyP19;
#endif
{
  if (yyP19->Kind == kenv) {
/* line 921 "itcl.puma" */
 {
  tTree obj;
  {
/* line 922 "itcl.puma" */

/* line 922 "itcl.puma" */
   obj = IdentifyProcLocal (yyP20, yyP19->env.objects);
/* line 923 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

/* line 926 "itcl.puma" */
   return IdentifyProcSuper (yyP20, yyP19->env.env);

  }
/* line 929 "itcl.puma" */
   return NULL;

}

static tTree IdentifyProcSuper
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP22, register tTree yyP21)
#else
(yyP22, yyP21)
 register tIdent yyP22;
 register tTree yyP21;
#endif
{
 yyRecursion:
  if (yyP21 == NULL) {
/* line 937 "itcl.puma" */
   return NULL;

  }
  if (yyP21->Kind == kenv2) {
/* line 940 "itcl.puma" */
 {
  tTree obj;
  {
/* line 941 "itcl.puma" */

/* line 941 "itcl.puma" */
   obj = IdentifyProcMember (yyP22, yyP21->env2.env1);
/* line 942 "itcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  }
  {
   return obj;
  }
 }
yyL2:;

/* line 945 "itcl.puma" */
   yyP21 = yyP21->env2.env2;
   goto yyRecursion;

  }
/* line 948 "itcl.puma" */
   return NULL;

}

static tTree get_namespace
#if defined __STDC__ | defined __cplusplus
(register tTree yyP23)
#else
(yyP23)
 register tTree yyP23;
#endif
{
 yyRecursion:
  if (yyP23->Kind == kenv) {
  if (Tree_IsType (yyP23->env.object, knamespace_c)) {
/* line 956 "itcl.puma" */
   return yyP23;

  }
  if (yyP23->env.object->Kind == kprogram) {
/* line 956 "itcl.puma" */
   return yyP23;

  }
/* line 959 "itcl.puma" */
   yyP23 = yyP23->env.env;
   goto yyRecursion;

  }
 yyAbort ("get_namespace");
 return 0;
}

static void pass2
#if defined __STDC__ | defined __cplusplus
(register tTree t)
#else
(t)
 register tTree t;
#endif
{
 yyRecursion:
  if (t->Kind == kstmt) {
  if (t->stmt.words->Kind == kone_word) {
  if (Tree_IsType (t->stmt.words->one_word.next, kword_c)) {
/* line 965 "itcl.puma" */
  {
/* line 967 "itcl.puma" */
   add_qualifications (t->stmt.words->one_word.ident, t->stmt.words->one_word.next, default_attr);
/* line 968 "itcl.puma" */
   pass2 (t->stmt.words);
/* line 969 "itcl.puma" */
   t = t->stmt.next;
   goto yyRecursion;
  }

  }
  }
/* line 971 "itcl.puma" */
  {
/* line 972 "itcl.puma" */
   pass2 (t->stmt.words);
/* line 973 "itcl.puma" */
   t = t->stmt.next;
   goto yyRecursion;
  }

  }
  if (t->Kind == kconfigbody) {
/* line 975 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 976 "itcl.puma" */

/* line 976 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 977 "itcl.puma" */
   cur_proc_ptr = "";
/* line 978 "itcl.puma" */
   get_objects (t->configbody.block);
/* line 979 "itcl.puma" */
   pass2 (t->configbody.block);
/* line 980 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 981 "itcl.puma" */
   t = t->configbody.next;
   goto yyRecursion;
  }
 }

  }
  if (Tree_IsType (t, kproc_c)) {
  if (t->proc_c.qualification->Kind == klocal_ident) {
/* line 983 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 985 "itcl.puma" */

/* line 985 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 986 "itcl.puma" */
   cur_proc_ptr = GetCStr (t->proc_c.qualification->local_ident.ident);
/* line 987 "itcl.puma" */
   pass2 (t->proc_c.block);
/* line 988 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 989 "itcl.puma" */
   t = t->proc_c.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc_c.qualification->Kind == kglobal_ident) {
/* line 983 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 985 "itcl.puma" */

/* line 985 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 986 "itcl.puma" */
   cur_proc_ptr = GetCStr (t->proc_c.qualification->global_ident.ident);
/* line 987 "itcl.puma" */
   pass2 (t->proc_c.block);
/* line 988 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 989 "itcl.puma" */
   t = t->proc_c.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc_c.qualification->Kind == klocal_text) {
/* line 991 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 993 "itcl.puma" */

/* line 993 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 994 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (t->proc_c.qualification));
/* line 995 "itcl.puma" */
   pass2 (t->proc_c.block);
/* line 996 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 997 "itcl.puma" */
   t = t->proc_c.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc_c.qualification->Kind == kglobal_text) {
/* line 991 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 993 "itcl.puma" */

/* line 993 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 994 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (t->proc_c.qualification));
/* line 995 "itcl.puma" */
   pass2 (t->proc_c.block);
/* line 996 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 997 "itcl.puma" */
   t = t->proc_c.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc_c.block->texts.env->Kind == kenv) {
/* line 999 "itcl.puma" */
 {
  tTree e;
  tIdent ident;
  tString prev_proc;
  tString prev_class;
  {
/* line 1002 "itcl.puma" */

/* line 1002 "itcl.puma" */
   e = get_env (t->proc_c.qualification);
/* line 1003 "itcl.puma" */

/* line 1003 "itcl.puma" */
   ident = get_ident (t->proc_c.qualification);
/* line 1004 "itcl.puma" */
  if (e) {
      t->proc_c.env = e;
      t->proc_c.block->texts.env->env.env = e;
      e->env.objects = mobject (t, ident, e->env.objects);
      dcl (e->env.objects, t->proc_c.attribute);
   } else {
      t->proc_c.env->env.objects = mobject (t, ident, t->proc_c.env->env.objects);
      dcl (t->proc_c.env->env.objects, t->proc_c.attribute);
   }

/* line 1014 "itcl.puma" */

/* line 1014 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 1015 "itcl.puma" */
   cur_proc_ptr = GetCStr (ident);
/* line 1016 "itcl.puma" */

/* line 1016 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1017 "itcl.puma" */
   cur_class_ptr = get_class (t->proc_c.qualification);
/* line 1018 "itcl.puma" */
   get_param_objects (t->proc_c.param_names);
/* line 1019 "itcl.puma" */
   get_objects (t->proc_c.block);
/* line 1020 "itcl.puma" */
   pass2 (t->proc_c.block);
/* line 1021 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 1022 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1023 "itcl.puma" */
   t = t->proc_c.next;
   goto yyRecursion;
  }
 }

  }
  }
  if (t->Kind == knamespace) {
  if (t->namespace.qualification->Kind == klocal_ident) {
/* line 1025 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1027 "itcl.puma" */

/* line 1027 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1028 "itcl.puma" */
   cur_class_ptr = GetCStr (t->namespace.qualification->local_ident.ident);
/* line 1029 "itcl.puma" */
   pass2 (t->namespace.block);
/* line 1030 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1031 "itcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == kglobal_ident) {
/* line 1025 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1027 "itcl.puma" */

/* line 1027 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1028 "itcl.puma" */
   cur_class_ptr = GetCStr (t->namespace.qualification->global_ident.ident);
/* line 1029 "itcl.puma" */
   pass2 (t->namespace.block);
/* line 1030 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1031 "itcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == klocal_text) {
/* line 1033 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1035 "itcl.puma" */

/* line 1035 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1036 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (t->namespace.qualification));
/* line 1037 "itcl.puma" */
   pass2 (t->namespace.block);
/* line 1038 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1039 "itcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == kglobal_text) {
/* line 1033 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1035 "itcl.puma" */

/* line 1035 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1036 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (t->namespace.qualification));
/* line 1037 "itcl.puma" */
   pass2 (t->namespace.block);
/* line 1038 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1039 "itcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.block->texts.env->Kind == kenv) {
/* line 1041 "itcl.puma" */
 {
  tTree e;
  tIdent ident;
  tTree obj;
  tString prev_class;
  {
/* line 1044 "itcl.puma" */

/* line 1044 "itcl.puma" */
   e = get_env (t->namespace.qualification);
/* line 1045 "itcl.puma" */

/* line 1045 "itcl.puma" */
   ident = get_ident (t->namespace.qualification);
/* line 1046 "itcl.puma" */

/* line 1047 "itcl.puma" */
  if (e) {
      t->namespace.env = e;
      t->namespace.block->texts.env->env.env = e;
      obj = IdentifyLocal (ident, e->env.objects);
      if (obj != NoTree && obj->object.object->Kind == knamespace) {
	 relocate (t->namespace.block, obj->object.object->namespace.block->texts.env);
	 use (obj, t->namespace.qualification->qualification_c.pos, PAF_REF_READ);
      } else {
	 e->env.objects = mobject (t, ident, e->env.objects);
	 dcl (e->env.objects, t->namespace.attribute);
      }
   } else {
      t->namespace.env->env.objects = mobject (t, ident, t->namespace.env->env.objects);
      dcl (t->namespace.env->env.objects, t->namespace.attribute);
   }

/* line 1063 "itcl.puma" */

/* line 1063 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1064 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1065 "itcl.puma" */
   get_objects (t->namespace.block);
/* line 1066 "itcl.puma" */
   pass2 (t->namespace.block);
/* line 1067 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1068 "itcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  }
  if (t->Kind == kclass) {
  if (t->class.qualification->Kind == klocal_ident) {
/* line 1070 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1074 "itcl.puma" */

/* line 1074 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1075 "itcl.puma" */
   cur_class_ptr = GetCStr (t->class.qualification->local_ident.ident);
/* line 1076 "itcl.puma" */
   pass2 (t->class.block);
/* line 1077 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1078 "itcl.puma" */
   t = t->class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->class.qualification->Kind == kglobal_ident) {
/* line 1070 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1074 "itcl.puma" */

/* line 1074 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1075 "itcl.puma" */
   cur_class_ptr = GetCStr (t->class.qualification->global_ident.ident);
/* line 1076 "itcl.puma" */
   pass2 (t->class.block);
/* line 1077 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1078 "itcl.puma" */
   t = t->class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->class.qualification->Kind == klocal_text) {
/* line 1080 "itcl.puma" */
 {
  tIdent ident;
  tString prev_class;
  {
/* line 1084 "itcl.puma" */

/* line 1084 "itcl.puma" */
   ident = get_ident (t->class.qualification);
/* line 1085 "itcl.puma" */

/* line 1085 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1086 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1087 "itcl.puma" */
   pass2 (t->class.block);
/* line 1088 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1089 "itcl.puma" */
   t = t->class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->class.qualification->Kind == kglobal_text) {
/* line 1080 "itcl.puma" */
 {
  tIdent ident;
  tString prev_class;
  {
/* line 1084 "itcl.puma" */

/* line 1084 "itcl.puma" */
   ident = get_ident (t->class.qualification);
/* line 1085 "itcl.puma" */

/* line 1085 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1086 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1087 "itcl.puma" */
   pass2 (t->class.block);
/* line 1088 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1089 "itcl.puma" */
   t = t->class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->class.block->texts.env->Kind == kenv) {
/* line 1091 "itcl.puma" */
 {
  tTree e;
  tIdent ident;
  tTree obj;
  tString prev_class;
  {
/* line 1097 "itcl.puma" */

/* line 1097 "itcl.puma" */
   e = get_env (t->class.qualification);
/* line 1098 "itcl.puma" */

/* line 1098 "itcl.puma" */
   ident = get_ident (t->class.qualification);
/* line 1099 "itcl.puma" */

/* line 1100 "itcl.puma" */
  if (e) {
      t->class.env = e;
      t->class.block->texts.env->env.env = e;
      obj = IdentifyLocal (ident, e->env.objects);
      if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
	 relocate (t->class.block, obj->object.object->namespace_c.block->texts.env);
	 use (obj, t->class.qualification->qualification_c.pos, PAF_REF_READ);
      } else {
	 e->env.objects = mobject (t, ident, e->env.objects);
	 dcl (e->env.objects, t->class.attribute);
      }
   } else {
      t->class.env->env.objects = mobject (t, ident, t->class.env->env.objects);
      dcl (t->class.env->env.objects, t->class.attribute);
   }

/* line 1116 "itcl.puma" */

/* line 1116 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1117 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1118 "itcl.puma" */
   get_objects (t->class.block);
/* line 1119 "itcl.puma" */
   pass2 (t->class.block);
/* line 1120 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1121 "itcl.puma" */
   t = t->class.next;
   goto yyRecursion;
  }
 }

  }
  }
  if (t->Kind == kitcl_class) {
  if (t->itcl_class.qualification->Kind == klocal_ident) {
/* line 1070 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1074 "itcl.puma" */

/* line 1074 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1075 "itcl.puma" */
   cur_class_ptr = GetCStr (t->itcl_class.qualification->local_ident.ident);
/* line 1076 "itcl.puma" */
   pass2 (t->itcl_class.block);
/* line 1077 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1078 "itcl.puma" */
   t = t->itcl_class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->itcl_class.qualification->Kind == kglobal_ident) {
/* line 1070 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1074 "itcl.puma" */

/* line 1074 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1075 "itcl.puma" */
   cur_class_ptr = GetCStr (t->itcl_class.qualification->global_ident.ident);
/* line 1076 "itcl.puma" */
   pass2 (t->itcl_class.block);
/* line 1077 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1078 "itcl.puma" */
   t = t->itcl_class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->itcl_class.qualification->Kind == klocal_text) {
/* line 1080 "itcl.puma" */
 {
  tIdent ident;
  tString prev_class;
  {
/* line 1084 "itcl.puma" */

/* line 1084 "itcl.puma" */
   ident = get_ident (t->itcl_class.qualification);
/* line 1085 "itcl.puma" */

/* line 1085 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1086 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1087 "itcl.puma" */
   pass2 (t->itcl_class.block);
/* line 1088 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1089 "itcl.puma" */
   t = t->itcl_class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->itcl_class.qualification->Kind == kglobal_text) {
/* line 1080 "itcl.puma" */
 {
  tIdent ident;
  tString prev_class;
  {
/* line 1084 "itcl.puma" */

/* line 1084 "itcl.puma" */
   ident = get_ident (t->itcl_class.qualification);
/* line 1085 "itcl.puma" */

/* line 1085 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1086 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1087 "itcl.puma" */
   pass2 (t->itcl_class.block);
/* line 1088 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1089 "itcl.puma" */
   t = t->itcl_class.next;
   goto yyRecursion;
  }
 }

  }
  if (t->itcl_class.block->texts.env->Kind == kenv) {
/* line 1091 "itcl.puma" */
 {
  tTree e;
  tIdent ident;
  tTree obj;
  tString prev_class;
  {
/* line 1097 "itcl.puma" */

/* line 1097 "itcl.puma" */
   e = get_env (t->itcl_class.qualification);
/* line 1098 "itcl.puma" */

/* line 1098 "itcl.puma" */
   ident = get_ident (t->itcl_class.qualification);
/* line 1099 "itcl.puma" */

/* line 1100 "itcl.puma" */
  if (e) {
      t->itcl_class.env = e;
      t->itcl_class.block->texts.env->env.env = e;
      obj = IdentifyLocal (ident, e->env.objects);
      if (obj != NoTree && Tree_IsType (obj->object.object, knamespace_c)) {
	 relocate (t->itcl_class.block, obj->object.object->namespace_c.block->texts.env);
	 use (obj, t->itcl_class.qualification->qualification_c.pos, PAF_REF_READ);
      } else {
	 e->env.objects = mobject (t, ident, e->env.objects);
	 dcl (e->env.objects, t->itcl_class.attribute);
      }
   } else {
      t->itcl_class.env->env.objects = mobject (t, ident, t->itcl_class.env->env.objects);
      dcl (t->itcl_class.env->env.objects, t->itcl_class.attribute);
   }

/* line 1116 "itcl.puma" */

/* line 1116 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1117 "itcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 1118 "itcl.puma" */
   get_objects (t->itcl_class.block);
/* line 1119 "itcl.puma" */
   pass2 (t->itcl_class.block);
/* line 1120 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1121 "itcl.puma" */
   t = t->itcl_class.next;
   goto yyRecursion;
  }
 }

  }
  }

  switch (t->Kind) {
  case kstmt_c:
  case kstmt:
  case kproc_c:
  case kproc:
  case kmethod:
  case kbody:
  case kconstructor:
  case kdestructor:
  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
/* line 1123 "itcl.puma" */
  {
/* line 1124 "itcl.puma" */
   t = t->stmt_c.next;
   goto yyRecursion;
  }

  case kone_word:
/* line 1126 "itcl.puma" */
  {
/* line 1127 "itcl.puma" */
   t = t->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 1129 "itcl.puma" */
  {
/* line 1130 "itcl.puma" */
   pass2 (t->qual_word.qualification);
/* line 1131 "itcl.puma" */
   t = t->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 1133 "itcl.puma" */
  {
/* line 1134 "itcl.puma" */
   pass2 (t->qual_words.qualifications);
/* line 1135 "itcl.puma" */
   t = t->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 1137 "itcl.puma" */
  {
/* line 1138 "itcl.puma" */
   pass2 (t->one_qualification.qualification);
/* line 1139 "itcl.puma" */
   t = t->one_qualification.next;
   goto yyRecursion;
  }

  case kblock:
/* line 1141 "itcl.puma" */
  {
/* line 1142 "itcl.puma" */
   pass2 (t->block.stmts);
/* line 1143 "itcl.puma" */
   t = t->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 1145 "itcl.puma" */
  {
/* line 1146 "itcl.puma" */
   pass2 (t->content.qualification);
/* line 1147 "itcl.puma" */
   t = t->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 1149 "itcl.puma" */
  {
/* line 1150 "itcl.puma" */
   pass2 (t->block_content.stmts);
/* line 1151 "itcl.puma" */
   t = t->block_content.next;
   goto yyRecursion;
  }

  case ktext:
  case kcharacter:
  case kident:
/* line 1153 "itcl.puma" */
  {
/* line 1154 "itcl.puma" */
   t = t->text.next;
   goto yyRecursion;
  }

  case klocal_text:
/* line 1156 "itcl.puma" */
  {
/* line 1157 "itcl.puma" */
   t = t->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 1159 "itcl.puma" */
  {
/* line 1160 "itcl.puma" */
   t = t->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 1162 "itcl.puma" */
  {
/* line 1163 "itcl.puma" */
   t = t->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 1165 "itcl.puma" */
  {
/* line 1166 "itcl.puma" */
   pass2 (t->complex_qual.qualification);
/* line 1167 "itcl.puma" */
   t = t->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 1169 "itcl.puma" */
  {
/* line 1170 "itcl.puma" */
   pass2 (t->subscription.qualification);
/* line 1171 "itcl.puma" */
   t = t->subscription.index;
   goto yyRecursion;
  }

  }

;
}

static void pass3
#if defined __STDC__ | defined __cplusplus
(register tTree yyP24)
#else
(yyP24)
 register tTree yyP24;
#endif
{
 yyRecursion:

  switch (yyP24->Kind) {
  case kstmt:
  if (yyP24->stmt.words->Kind == kone_word) {
/* line 1178 "itcl.puma" */
  {
/* line 1180 "itcl.puma" */
   if (! (GetCStr (yyP24->stmt.words->one_word.ident) [0] == '#')) goto yyL1;
  {
/* line 1181 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }
  }
yyL1:;

  if (Tree_IsType (yyP24->stmt.words->one_word.next, kword_c)) {
  if (Tree_IsType (yyP24->stmt.words->one_word.next->word_c.next, kword_c)) {
 {
  tTree obj;
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (ilocal))) {
/* line 1183 "itcl.puma" */
  {
/* line 1187 "itcl.puma" */
   use_proc_undef (ilocal, yyP24->stmt.words->one_word.pos);
/* line 1188 "itcl.puma" */

/* line 1188 "itcl.puma" */
   obj = use_word (yyP24->stmt.words->one_word.next);
/* line 1189 "itcl.puma" */
   yyP24->stmt.words->one_word.next->word_c.next = declare_object (yyP24->stmt.words->one_word.next->word_c.next, obj);
/* line 1190 "itcl.puma" */
   pass3 (yyP24->stmt.words->one_word.next);
/* line 1191 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
 {
  tTree obj;
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (irename))) {
/* line 1193 "itcl.puma" */
  {
/* line 1197 "itcl.puma" */
   use_proc_undef (irename, yyP24->stmt.words->one_word.pos);
/* line 1198 "itcl.puma" */

/* line 1198 "itcl.puma" */
   obj = use_proc (yyP24->stmt.words->one_word.next);
/* line 1199 "itcl.puma" */
   pass3 (yyP24->stmt.words->one_word.next);
/* line 1200 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
  }
  }
  if (yyP24->stmt.words->one_word.next->Kind == kone_word) {
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (idelete))) {
/* line 1202 "itcl.puma" */
  {
/* line 1205 "itcl.puma" */
   use_proc_undef (idelete, yyP24->stmt.words->one_word.pos);
/* line 1206 "itcl.puma" */
   if (! (yyP24->stmt.words->one_word.next->one_word.ident == iobject || yyP24->stmt.words->one_word.next->one_word.ident == iclass || yyP24->stmt.words->one_word.next->one_word.ident == inamespace)) goto yyL4;
  {
/* line 1207 "itcl.puma" */
   add_vars (yyP24->stmt.words->one_word.next->one_word.next, PAF_REF_WRITE);
/* line 1208 "itcl.puma" */
   pass3 (yyP24->stmt.words->one_word.next->one_word.next);
/* line 1209 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }
  }
yyL4:;

  }
  if (equaltIdent (yyP24->stmt.words->one_word.next->one_word.ident, (iobjects))) {
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (iitcl_info))) {
/* line 1211 "itcl.puma" */
  {
/* line 1214 "itcl.puma" */
   use_proc_undef (iitcl_info, yyP24->stmt.words->one_word.pos);
/* line 1215 "itcl.puma" */
   itcl_info (yyP24->stmt.words->one_word.next->one_word.next);
/* line 1216 "itcl.puma" */
   pass3 (yyP24->stmt.words->one_word.next->one_word.next);
/* line 1217 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
  }
  }
 {
  register tTree yyV1;
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (ivirtual))) {
/* line 1219 "itcl.puma" */
  {
/* line 1221 "itcl.puma" */
   use_proc_undef (ivirtual, yyP24->stmt.words->one_word.pos);
/* line 1222 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ystmt,MakeTree,yyV1,kstmt,Tree_InitHead)
    yyV1->stmt.env = yyP24->stmt.env;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynostmt,MakeTree,yyW1,knostmt,Tree_InitHead)
    yyV1->stmt.next = yyW1;
    yyW1->nostmt.env = yyP24->stmt.env;
   }
    yyV1->stmt.words = yyP24->stmt.words->one_word.next;
   pass3 (yyV1);
/* line 1223 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
 {
  register tTree yyV1;
  if (equaltIdent (yyP24->stmt.words->one_word.ident, (iprevious))) {
/* line 1225 "itcl.puma" */
  {
/* line 1227 "itcl.puma" */
   use_proc_undef (iprevious, yyP24->stmt.words->one_word.pos);
/* line 1228 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ystmt,MakeTree,yyV1,kstmt,Tree_InitHead)
    yyV1->stmt.env = yyP24->stmt.env;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynostmt,MakeTree,yyW1,knostmt,Tree_InitHead)
    yyV1->stmt.next = yyW1;
    yyW1->nostmt.env = yyP24->stmt.env;
   }
    yyV1->stmt.words = yyP24->stmt.words->one_word.next;
   pass3 (yyV1);
/* line 1229 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
  }
  if (yyP24->stmt.words->Kind == kqual_word) {
  if (yyP24->stmt.words->qual_word.next->Kind == kone_word) {
  if (yyP24->stmt.words->qual_word.qualification->Kind == klocal_text) {
  if (yyP24->stmt.words->qual_word.qualification->local_text.texts->Kind == kcontent) {
  if (yyP24->stmt.words->qual_word.qualification->local_text.texts->content.next->Kind == knotext) {
  if (yyP24->stmt.words->qual_word.qualification->local_text.texts->content.qualification->Kind == klocal_ident) {
 {
  tTree obj;
  if (equaltIdent (yyP24->stmt.words->qual_word.qualification->local_text.texts->content.qualification->local_ident.ident, (ithis))) {
/* line 1231 "itcl.puma" */
  {
/* line 1236 "itcl.puma" */

/* line 1236 "itcl.puma" */
   obj = IdentifyMember (yyP24->stmt.words->qual_word.next->one_word.ident, get_namespace (yyP24->stmt.env));
/* line 1237 "itcl.puma" */
  if (obj) use (obj, yyP24->stmt.words->qual_word.next->one_word.pos, PAF_REF_READ); 
/* line 1238 "itcl.puma" */
   pass3 (yyP24->stmt.words->qual_word.next->one_word.next);
/* line 1239 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
  }
  }
  }
  }
  }
  }
  if (Tree_IsType (yyP24->stmt.words, kword_c)) {
  if (yyP24->stmt.words->word_c.next->Kind == kqual_word) {
  if (Tree_IsType (yyP24->stmt.words->word_c.next->qual_word.next, kword_c)) {
  if (yyP24->stmt.words->word_c.next->qual_word.qualification->Kind == kglobal_ident) {
 {
  tTree obj;
  if (equaltIdent (yyP24->stmt.words->word_c.next->qual_word.qualification->global_ident.ident, (NoIdent))) {
/* line 1241 "itcl.puma" */
  {
/* line 1245 "itcl.puma" */

/* line 1245 "itcl.puma" */
   obj = use_word (yyP24->stmt.words);
/* line 1246 "itcl.puma" */

   if (obj) switch (obj->object.object->Kind) {
   case kclass		:
   case kitcl_class	: use_proc_object (obj, yyP24->stmt.words->word_c.next->qual_word.next); break;
   }

/* line 1252 "itcl.puma" */
   pass3 (yyP24->stmt.words);
/* line 1253 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  }
 }
  }
  }
  }
  if (Tree_IsType (yyP24->stmt.words->word_c.next, kword_c)) {
/* line 1255 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1256 "itcl.puma" */

/* line 1256 "itcl.puma" */
   obj = use_word (yyP24->stmt.words);
/* line 1257 "itcl.puma" */
							
   if (obj) switch (obj->object.object->Kind) {
   case kclass		:
   case kitcl_class	: yyP24->stmt.words->word_c.next = declare_object (yyP24->stmt.words->word_c.next, obj); break;
   case kobj_word	: use_proc_object (obj, yyP24->stmt.words->word_c.next); break;
   }

/* line 1264 "itcl.puma" */
   pass3 (yyP24->stmt.words);
/* line 1265 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }
 }

  }
/* line 1267 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1268 "itcl.puma" */

/* line 1268 "itcl.puma" */
   obj = use_proc (yyP24->stmt.words);
/* line 1269 "itcl.puma" */
   pass3 (yyP24->stmt.words);
/* line 1270 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }
 }

  }
/* line 1272 "itcl.puma" */
  {
/* line 1273 "itcl.puma" */
   pass3 (yyP24->stmt.words);
/* line 1274 "itcl.puma" */
   yyP24 = yyP24->stmt.next;
   goto yyRecursion;
  }

  case kbody:
/* line 1276 "itcl.puma" */
 {
  tString prev_proc;
  tString prev_class;
  {
/* line 1277 "itcl.puma" */

/* line 1277 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 1278 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (yyP24->body.qualification));
/* line 1279 "itcl.puma" */

/* line 1279 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1280 "itcl.puma" */
   cur_class_ptr = get_class (yyP24->body.qualification);
/* line 1281 "itcl.puma" */
   pass3 (yyP24->body.qualification);
/* line 1283 "itcl.puma" */
   pass3 (yyP24->body.block);
/* line 1284 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 1285 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1286 "itcl.puma" */
   yyP24 = yyP24->body.next;
   goto yyRecursion;
  }
 }

  case kconfigbody:
/* line 1288 "itcl.puma" */
 {
  tTree obj;
  tString prev_proc;
  {
/* line 1289 "itcl.puma" */

/* line 1289 "itcl.puma" */
   obj = use_qual (yyP24->configbody.qualification);
/* line 1290 "itcl.puma" */

/* line 1290 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 1291 "itcl.puma" */
   cur_proc_ptr = "";
/* line 1292 "itcl.puma" */
   pass3 (yyP24->configbody.block);
/* line 1293 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 1294 "itcl.puma" */
   yyP24 = yyP24->configbody.next;
   goto yyRecursion;
  }
 }

  case kproc_c:
  case kproc:
  case kmethod:
  case kconstructor:
  case kdestructor:
/* line 1296 "itcl.puma" */
 {
  tString prev_proc;
  {
/* line 1297 "itcl.puma" */

/* line 1297 "itcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 1298 "itcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (yyP24->proc_c.qualification));
/* line 1299 "itcl.puma" */
   pass3 (yyP24->proc_c.qualification);
/* line 1301 "itcl.puma" */
   pass3 (yyP24->proc_c.block);
/* line 1302 "itcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 1303 "itcl.puma" */
   yyP24 = yyP24->proc_c.next;
   goto yyRecursion;
  }
 }

  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
/* line 1305 "itcl.puma" */
 {
  tString prev_class;
  {
/* line 1306 "itcl.puma" */

/* line 1306 "itcl.puma" */
   prev_class = cur_class_ptr;
/* line 1307 "itcl.puma" */
   cur_class_ptr = GetCStr (get_ident (yyP24->namespace_c.qualification));
/* line 1308 "itcl.puma" */
   pass3 (yyP24->namespace_c.qualification);
/* line 1309 "itcl.puma" */
   pass3 (yyP24->namespace_c.block);
/* line 1310 "itcl.puma" */
   cur_class_ptr = prev_class;
/* line 1311 "itcl.puma" */
   yyP24 = yyP24->namespace_c.next;
   goto yyRecursion;
  }
 }

  case kone_word:
/* line 1313 "itcl.puma" */
  {
/* line 1314 "itcl.puma" */
   yyP24 = yyP24->one_word.next;
   goto yyRecursion;
  }

  case kobj_word:
/* line 1316 "itcl.puma" */
  {
/* line 1317 "itcl.puma" */
   yyP24 = yyP24->obj_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 1319 "itcl.puma" */
  {
/* line 1320 "itcl.puma" */
   pass3 (yyP24->qual_word.qualification);
/* line 1321 "itcl.puma" */
   yyP24 = yyP24->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 1323 "itcl.puma" */
  {
/* line 1324 "itcl.puma" */
   pass3 (yyP24->qual_words.qualifications);
/* line 1325 "itcl.puma" */
   yyP24 = yyP24->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 1327 "itcl.puma" */
  {
/* line 1328 "itcl.puma" */
   pass3 (yyP24->one_qualification.qualification);
/* line 1329 "itcl.puma" */
   yyP24 = yyP24->one_qualification.next;
   goto yyRecursion;
  }

  case kcharacter:
/* line 1331 "itcl.puma" */
  {
/* line 1332 "itcl.puma" */
   yyP24 = yyP24->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 1334 "itcl.puma" */
  {
/* line 1335 "itcl.puma" */
   yyP24 = yyP24->ident.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 1337 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1338 "itcl.puma" */

/* line 1338 "itcl.puma" */
   obj = use_qual (yyP24->content.qualification);
/* line 1339 "itcl.puma" */
   pass3 (yyP24->content.qualification);
/* line 1340 "itcl.puma" */
   yyP24 = yyP24->content.next;
   goto yyRecursion;
  }
 }

  case kblock:
/* line 1342 "itcl.puma" */
 {
  rbool prev_in_string;
  {
/* line 1343 "itcl.puma" */

/* line 1343 "itcl.puma" */
   prev_in_string = in_string;
/* line 1344 "itcl.puma" */
   in_string = yyP24->block.type == '"';
/* line 1345 "itcl.puma" */
   pass3 (yyP24->block.stmts);
/* line 1346 "itcl.puma" */
   in_string = prev_in_string;
/* line 1347 "itcl.puma" */
   yyP24 = yyP24->block.next;
   goto yyRecursion;
  }
 }

  case kblock_content:
/* line 1349 "itcl.puma" */
  {
/* line 1350 "itcl.puma" */
   pass3 (yyP24->block_content.stmts);
/* line 1351 "itcl.puma" */
   yyP24 = yyP24->block_content.next;
   goto yyRecursion;
  }

  case kqualification:
/* line 1353 "itcl.puma" */
  {
/* line 1354 "itcl.puma" */
   yyP24 = yyP24->qualification.qualification;
   goto yyRecursion;
  }

  case klocal_text:
/* line 1356 "itcl.puma" */
  {
/* line 1357 "itcl.puma" */
   yyP24 = yyP24->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 1359 "itcl.puma" */
  {
/* line 1360 "itcl.puma" */
   yyP24 = yyP24->global_text.texts;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 1362 "itcl.puma" */
  {
/* line 1363 "itcl.puma" */
   pass3 (yyP24->complex_qual.qualification);
/* line 1364 "itcl.puma" */
   yyP24 = yyP24->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 1366 "itcl.puma" */
  {
/* line 1367 "itcl.puma" */
   pass3 (yyP24->subscription.qualification);
/* line 1368 "itcl.puma" */
   yyP24 = yyP24->subscription.index;
   goto yyRecursion;
  }

  }

;
}

tIdent make_one_word
#if defined __STDC__ | defined __cplusplus
(register tTree yyP25)
#else
(yyP25)
 register tTree yyP25;
#endif
{
  if (yyP25->Kind == kident) {
  if (yyP25->ident.next->Kind == knotext) {
/* line 1375 "itcl.puma" */
   return yyP25->ident.ident;

  }
  }
/* line 1378 "itcl.puma" */
  {
/* line 1379 "itcl.puma" */
   length = 0;
/* line 1380 "itcl.puma" */
   make_one_word_2 (yyP25);
  }
   return MakeIdent (buffer, length);

}

static void make_one_word_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP26)
#else
(yyP26)
 register tTree yyP26;
#endif
{
 yyRecursion:

  switch (yyP26->Kind) {
  case kcharacter:
/* line 1388 "itcl.puma" */
  {
/* line 1389 "itcl.puma" */
   buffer [length ++] = yyP26->character.text;
/* line 1390 "itcl.puma" */
   yyP26 = yyP26->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 1392 "itcl.puma" */
  {
/* line 1393 "itcl.puma" */
   GetString (yyP26->ident.ident, & buffer [length]);
/* line 1394 "itcl.puma" */
   length = length + StLength (GetStringRef (yyP26->ident.ident));
/* line 1395 "itcl.puma" */
   yyP26 = yyP26->ident.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 1397 "itcl.puma" */
  {
/* line 1398 "itcl.puma" */
   buffer [length ++] = '$';
/* line 1399 "itcl.puma" */
   make_one_word_2 (yyP26->content.qualification);
/* line 1400 "itcl.puma" */
   yyP26 = yyP26->content.next;
   goto yyRecursion;
  }

  case klocal_ident:
/* line 1402 "itcl.puma" */
  {
/* line 1403 "itcl.puma" */
   GetString (yyP26->local_ident.ident, & buffer [length]);
/* line 1404 "itcl.puma" */
   length = length + StLength (GetStringRef (yyP26->local_ident.ident));
  }
   return;

  case kglobal_ident:
/* line 1406 "itcl.puma" */
  {
/* line 1407 "itcl.puma" */
   strcpy (& buffer [length], "::");
/* line 1407 "itcl.puma" */
   length = length + 2;
/* line 1408 "itcl.puma" */
   GetString (yyP26->global_ident.ident, & buffer [length]);
/* line 1409 "itcl.puma" */
   length = length + StLength (GetStringRef (yyP26->global_ident.ident));
  }
   return;

  case klocal_text:
/* line 1411 "itcl.puma" */
  {
/* line 1412 "itcl.puma" */
   yyP26 = yyP26->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 1414 "itcl.puma" */
  {
/* line 1415 "itcl.puma" */
   strcpy (& buffer [length], "::");
/* line 1415 "itcl.puma" */
   length = length + 2;
/* line 1416 "itcl.puma" */
   yyP26 = yyP26->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 1418 "itcl.puma" */
  {
/* line 1419 "itcl.puma" */
   make_one_word_2 (yyP26->qualification.qualification);
/* line 1420 "itcl.puma" */
   strcpy (& buffer [length], "::");
/* line 1420 "itcl.puma" */
   length = length + 2;
/* line 1421 "itcl.puma" */
   GetString (yyP26->qualification.ident, & buffer [length]);
/* line 1422 "itcl.puma" */
   length = length + StLength (GetStringRef (yyP26->qualification.ident));
  }
   return;

  case kcomplex_qual:
/* line 1424 "itcl.puma" */
  {
/* line 1425 "itcl.puma" */
   make_one_word_2 (yyP26->complex_qual.qualification);
/* line 1426 "itcl.puma" */
   strcpy (& buffer [length], "::");
/* line 1426 "itcl.puma" */
   length = length + 2;
/* line 1427 "itcl.puma" */
   yyP26 = yyP26->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 1429 "itcl.puma" */
  {
/* line 1430 "itcl.puma" */
   make_one_word_2 (yyP26->subscription.qualification);
/* line 1431 "itcl.puma" */
   strcpy (& buffer [length], "(");
/* line 1431 "itcl.puma" */
   length = length + 1;
/* line 1432 "itcl.puma" */
   make_one_word_2 (yyP26->subscription.index);
/* line 1433 "itcl.puma" */
   strcpy (& buffer [length], ")");
/* line 1433 "itcl.puma" */
   length = length + 1;
  }
   return;

  case kblock:
  if (yyP26->block.next->Kind == knotext) {
  if (yyP26->block.stmts->Kind == knostmt) {
/* line 1435 "itcl.puma" */
   return;

  }
  }
  break;
  case knotext:
/* line 1437 "itcl.puma" */
   return;

  }

/* line 1439 "itcl.puma" */
  {
/* line 1440 "itcl.puma" */
   buffer [length ++] = '?';
  }
   return;

;
}

tTree Mword
#if defined __STDC__ | defined __cplusplus
(register tTree yyP28, register tTree yyP27)
#else
(yyP28, yyP27)
 register tTree yyP28;
 register tTree yyP27;
#endif
{
  if (yyP27->Kind == klocal_ident) {
/* line 1447 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP28;
    yyV1->one_word.ident = yyP27->local_ident.ident;
    yyV1->one_word.pos = yyP27->local_ident.pos;
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP27, kqualification_c)) {
/* line 1450 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_word,MakeTree,yyV1,kqual_word,Tree_InitHead)
    begintTree (yyV1->qual_word.env)
    yyV1->qual_word.next = yyP28;
    yyV1->qual_word.qualification = yyP27;
   return yyV1;
  }
 }

  }
  if (yyP27->Kind == kone_qualification) {
/* line 1453 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_words,MakeTree,yyV1,kqual_words,Tree_InitHead)
    begintTree (yyV1->qual_words.env)
    yyV1->qual_words.next = yyP28;
    yyV1->qual_words.qualifications = yyP27;
   return yyV1;
  }
 }

  }
  if (yyP27->Kind == kone_word) {
/* line 1456 "itcl.puma" */
  {
/* line 1457 "itcl.puma" */
   yyP27->one_word.next = yyP28;
  }
   return yyP27;

  }
 yyAbort ("Mword");
 return 0;
}

tTree Mqualification
#if defined __STDC__ | defined __cplusplus
(register tTree yyP29)
#else
(yyP29)
 register tTree yyP29;
#endif
{
  if (yyP29->Kind == kone_word) {
/* line 1465 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_qualification,MakeTree,yyV1,kone_qualification,Tree_InitHead)
    begintTree (yyV1->one_qualification.env)
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoqualification,MakeTree,yyW1,knoqualification,Tree_InitHead)
    yyV1->one_qualification.next = yyW1;
    begintTree (yyW1->noqualification.env)
   }
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW2,klocal_ident,Tree_InitHead)
    yyV1->one_qualification.qualification = yyW2;
    yyW2->local_ident.pos = yyP29->one_word.pos;
    begintTree (yyW2->local_ident.env)
    yyW2->local_ident.ident = yyP29->one_word.ident;
   }
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP29, kqualification_c)) {
/* line 1469 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_qualification,MakeTree,yyV1,kone_qualification,Tree_InitHead)
    begintTree (yyV1->one_qualification.env)
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoqualification,MakeTree,yyW1,knoqualification,Tree_InitHead)
    yyV1->one_qualification.next = yyW1;
    begintTree (yyW1->noqualification.env)
   }
    yyV1->one_qualification.qualification = yyP29;
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP29, kqualifications)) {
/* line 1472 "itcl.puma" */
   return yyP29;

  }
 yyAbort ("Mqualification");
 return 0;
}

tTree Mstmt
#if defined __STDC__ | defined __cplusplus
(register tTree yyP31, register tTree yyP30)
#else
(yyP31, yyP30)
 register tTree yyP31;
 register tTree yyP30;
#endif
{
/* line 1480 "itcl.puma" */
 {
  tTree stmt;
  {
/* line 1481 "itcl.puma" */

/* line 1481 "itcl.puma" */
   stmt = Mstmt_1 (yyP31, yyP30, default_attr);
/* line 1482 "itcl.puma" */
   if (! (stmt != NULL)) goto yyL1;
  }
  {
   return stmt;
  }
 }
yyL1:;

/* line 1485 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ystmt,MakeTree,yyV1,kstmt,Tree_InitHead)
    begintTree (yyV1->stmt.env)
    yyV1->stmt.next = yyP31;
    yyV1->stmt.words = yyP30;
   return yyV1;
  }
 }

}

static tTree Mstmt_1
#if defined __STDC__ | defined __cplusplus
(register tTree yyP33, register tTree yyP32, register int attr)
#else
(yyP33, yyP32, attr)
 register tTree yyP33;
 register tTree yyP32;
 register int attr;
#endif
{
 yyRecursion:
  if (yyP32->Kind == kone_word) {
  if (equaltIdent (yyP32->one_word.ident, (ipublic))) {
/* line 1493 "itcl.puma" */
   yyP32 = yyP32->one_word.next;
   attr = PAF_PUBLIC;
   goto yyRecursion;

  }
  if (equaltIdent (yyP32->one_word.ident, (iprotected))) {
/* line 1496 "itcl.puma" */
   yyP32 = yyP32->one_word.next;
   attr = PAF_PROTECTED;
   goto yyRecursion;

  }
  if (equaltIdent (yyP32->one_word.ident, (iprivate))) {
/* line 1499 "itcl.puma" */
   yyP32 = yyP32->one_word.next;
   attr = PAF_PRIVATE;
   goto yyRecursion;

  }
/* line 1502 "itcl.puma" */
  {
/* line 1512 "itcl.puma" */
   if (! (yyP32->one_word.ident == iproc || yyP32->one_word.ident == imethod || yyP32->one_word.ident == ibody || yyP32->one_word.ident == iconfigbody || yyP32->one_word.ident == iconstructor || yyP32->one_word.ident == idestructor || yyP32->one_word.ident == iclass || yyP32->one_word.ident == iitcl_class || yyP32->one_word.ident == inamespace || yyP32->one_word.ident == iat_scope)) goto yyL4;
  {
/* line 1513 "itcl.puma" */
   gpos = yyP32->one_word.pos;
  }
  }
   return Mstmt_2 (yyP33, yyP32->one_word.ident, yyP32->one_word.next, attr);
yyL4:;

  }
  if (yyP32->Kind == kqual_word) {
  if (yyP32->qual_word.qualification->Kind == kglobal_ident) {
/* line 1516 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP32->qual_word.next;
    yyV1->one_word.ident = yyP32->qual_word.qualification->global_ident.ident;
    yyV1->one_word.pos = yyP32->qual_word.qualification->global_ident.pos;
   yyP32 = yyV1;
   goto yyRecursion;
  }
 }

  }
  }
/* line 1519 "itcl.puma" */
   return NULL;

}

static tTree Mstmt_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP36, register tIdent yyP35, register tTree yyP34, register int attr)
#else
(yyP36, yyP35, yyP34, attr)
 register tTree yyP36;
 register tIdent yyP35;
 register tTree yyP34;
 register int attr;
#endif
{
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (iproc))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == knoword) {
/* line 1527 "itcl.puma" */
  {
/* line 1529 "itcl.puma" */
   get_end_pos (yyP34, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV2,kproc,Tree_InitHead)
    begintTree (yyV2->proc.env)
    yyV2->proc.next = yyP36;
    yyV2->proc.qualification = make_qualification (yyP34);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW1,knoword,Tree_InitHead)
    yyV2->proc.param_names = yyW1;
    begintTree (yyW1->noword.env)
   }
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW2,knoword,Tree_InitHead)
    yyV2->proc.parameter = yyW2;
    begintTree (yyW2->noword.env)
   }
   {register tTree yyW3;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW3,knotext,Tree_InitHead)
    yyV2->proc.block = yyW3;
    begintTree (yyW3->notext.env)
   }
    yyV2->proc.epos = yyV1;
    yyV2->proc.attribute = attr;
   return yyV2;
  }

  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (iproc))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == knoword) {
/* line 1533 "itcl.puma" */
  {
/* line 1536 "itcl.puma" */
   get_end_pos (yyP34->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV2,kproc,Tree_InitHead)
    begintTree (yyV2->proc.env)
    yyV2->proc.next = yyP36;
    yyV2->proc.qualification = make_qualification (yyP34);
    yyV2->proc.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV2->proc.parameter = yyP34->word_c.next;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->proc.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->proc.epos = yyV1;
    yyV2->proc.attribute = attr;
   return yyV2;
  }

  }
  }
  }
  }
 }
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP35, (iproc))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1540 "itcl.puma" */
  {
/* line 1544 "itcl.puma" */

/* line 1545 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV1,kproc,Tree_InitHead)
    begintTree (yyV1->proc.env)
    yyV1->proc.next = yyP36;
    yyV1->proc.qualification = make_qualification (yyP34);
    yyV1->proc.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV1->proc.parameter = yyP34->word_c.next;
    yyV1->proc.block = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->proc.epos = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->proc.attribute = attr;
   p = yyV1;
/* line 1547 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP34->word_c.next->word_c.next = yyV2;
  }
  {
   return p;
  }

  }
  }
  }
  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (iproc))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next->word_c.next, kword_c)) {
/* line 1550 "itcl.puma" */
  {
/* line 1554 "itcl.puma" */
   get_end_pos (yyP34->word_c.next->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV2,kproc,Tree_InitHead)
    begintTree (yyV2->proc.env)
    yyV2->proc.next = yyP36;
    yyV2->proc.qualification = make_qualification (yyP34);
    yyV2->proc.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV2->proc.parameter = yyP34->word_c.next;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->proc.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->proc.epos = yyV1;
    yyV2->proc.attribute = attr;
   return yyV2;
  }

  }
  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (imethod))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == knoword) {
/* line 1558 "itcl.puma" */
  {
/* line 1560 "itcl.puma" */
   itcl = itcl + 1;
/* line 1561 "itcl.puma" */
   get_end_pos (yyP34, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ymethod,MakeTree,yyV2,kmethod,Tree_InitHead)
    begintTree (yyV2->method.env)
    yyV2->method.next = yyP36;
    yyV2->method.qualification = make_qualification (yyP34);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW1,knoword,Tree_InitHead)
    yyV2->method.param_names = yyW1;
    begintTree (yyW1->noword.env)
   }
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW2,knoword,Tree_InitHead)
    yyV2->method.parameter = yyW2;
    begintTree (yyW2->noword.env)
   }
   {register tTree yyW3;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW3,knotext,Tree_InitHead)
    yyV2->method.block = yyW3;
    begintTree (yyW3->notext.env)
   }
    yyV2->method.epos = yyV1;
    yyV2->method.attribute = attr;
   return yyV2;
  }

  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (imethod))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == knoword) {
/* line 1565 "itcl.puma" */
  {
/* line 1568 "itcl.puma" */
   itcl = itcl + 1;
/* line 1569 "itcl.puma" */
   get_end_pos (yyP34->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ymethod,MakeTree,yyV2,kmethod,Tree_InitHead)
    begintTree (yyV2->method.env)
    yyV2->method.next = yyP36;
    yyV2->method.qualification = make_qualification (yyP34);
    yyV2->method.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV2->method.parameter = yyP34->word_c.next;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->method.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->method.epos = yyV1;
    yyV2->method.attribute = attr;
   return yyV2;
  }

  }
  }
  }
  }
 }
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP35, (imethod))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1573 "itcl.puma" */
  {
/* line 1577 "itcl.puma" */
   itcl = itcl + 1;
/* line 1578 "itcl.puma" */

/* line 1579 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ymethod,MakeTree,yyV1,kmethod,Tree_InitHead)
    begintTree (yyV1->method.env)
    yyV1->method.next = yyP36;
    yyV1->method.qualification = make_qualification (yyP34);
    yyV1->method.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV1->method.parameter = yyP34->word_c.next;
    yyV1->method.block = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->method.epos = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->method.attribute = attr;
   p = yyV1;
/* line 1581 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP34->word_c.next->word_c.next = yyV2;
  }
  {
   return p;
  }

  }
  }
  }
  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (imethod))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == kone_word) {
/* line 1584 "itcl.puma" */
  {
/* line 1588 "itcl.puma" */
   itcl = itcl + 1;
/* line 1589 "itcl.puma" */
   get_end_pos (yyP34->word_c.next->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ymethod,MakeTree,yyV2,kmethod,Tree_InitHead)
    begintTree (yyV2->method.env)
    yyV2->method.next = yyP36;
    yyV2->method.qualification = make_qualification (yyP34);
    yyV2->method.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV2->method.parameter = yyP34->word_c.next;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->method.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->method.epos = yyV1;
    yyV2->method.attribute = attr;
   return yyV2;
  }

  }
  }
  }
  }
 }
 {
  if (equaltIdent (yyP35, (inamespace))) {
  if (yyP34->Kind == kone_word) {
  if (Tree_IsType (yyP34->one_word.next, kword_c)) {
  if (yyP34->one_word.next->word_c.next->Kind == kqual_word) {
  if (yyP34->one_word.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->one_word.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
  if (equaltIdent (yyP34->one_word.ident, (ieval))) {
/* line 1593 "itcl.puma" */
  {
/* line 1597 "itcl.puma" */
   tcl80 = tcl80 + 1;
  }
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV1,knamespace,Tree_InitHead)
    begintTree (yyV1->namespace.env)
    yyV1->namespace.next = yyP36;
    yyV1->namespace.qualification = make_qualification (yyP34->one_word.next);
    yyV1->namespace.block = yyP34->one_word.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->namespace.epos = yyP34->one_word.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->namespace.attribute = attr;
   return yyV1;
  }

  }
  }
  }
  }
  }
  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (inamespace))) {
  if (yyP34->Kind == kone_word) {
  if (Tree_IsType (yyP34->one_word.next, kword_c)) {
  if (Tree_IsType (yyP34->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP34->one_word.ident, (ieval))) {
/* line 1600 "itcl.puma" */
  {
/* line 1604 "itcl.puma" */
   tcl80 = tcl80 + 1;
/* line 1605 "itcl.puma" */
   get_end_pos (yyP34->one_word.next->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV2,knamespace,Tree_InitHead)
    begintTree (yyV2->namespace.env)
    yyV2->namespace.next = yyP36;
    yyV2->namespace.qualification = make_qualification (yyP34->one_word.next);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->namespace.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->namespace.epos = yyV1;
    yyV2->namespace.attribute = attr;
   return yyV2;
  }

  }
  }
  }
  }
  }
 }
 {
  tTree yyV1;
  tPosition yyV2;
  if (equaltIdent (yyP35, (inamespace))) {
  if (Tree_IsType (yyP34, kword_c)) {
/* line 1608 "itcl.puma" */
  {
/* line 1610 "itcl.puma" */
   if (! (get_script (yyP34->word_c.next, & yyV1, & yyV2))) goto yyL11;
  {
/* line 1611 "itcl.puma" */
   itcl = itcl + 1;
  }
  }
  {
  register tTree yyV3;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV3,knamespace,Tree_InitHead)
    begintTree (yyV3->namespace.env)
    yyV3->namespace.next = yyP36;
    yyV3->namespace.qualification = make_qualification (yyP34);
    yyV3->namespace.block = yyV1;
    yyV3->namespace.epos = yyV2;
    yyV3->namespace.attribute = attr;
   return yyV3;
  }
yyL11:;

  }
  }
 }
 {
  tPosition yyV1;
  if (equaltIdent (yyP35, (iat_scope))) {
  if (Tree_IsType (yyP34, kword_c)) {
/* line 1614 "itcl.puma" */
  {
/* line 1616 "itcl.puma" */
   itcl = itcl + 1;
/* line 1617 "itcl.puma" */
   get_end_pos_2 (yyP34->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV2,knamespace,Tree_InitHead)
    begintTree (yyV2->namespace.env)
    yyV2->namespace.next = yyP36;
    yyV2->namespace.qualification = make_qualification (yyP34);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,yblock,MakeTree,yyW1,kblock,Tree_InitHead)
    yyV2->namespace.block = yyW1;
    begintTree (yyW1->block.env)
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW2,knotext,Tree_InitHead)
    yyW1->block.next = yyW2;
    begintTree (yyW2->notext.env)
   }
    yyW1->block.pos = gpos;
    yyW1->block.epos = yyV1;
   {register tTree yyW3;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ystmt,MakeTree,yyW3,kstmt,Tree_InitHead)
    yyW1->block.stmts = yyW3;
    begintTree (yyW3->stmt.env)
   {register tTree yyW4;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynostmt,MakeTree,yyW4,knostmt,Tree_InitHead)
    yyW3->stmt.next = yyW4;
    begintTree (yyW4->nostmt.env)
   }
    yyW3->stmt.words = yyP34->word_c.next;
   }
    yyW1->block.type = '@';
   }
    yyV2->namespace.epos = yyV1;
    yyV2->namespace.attribute = attr;
   return yyV2;
  }

  }
  }
 }
 {
  if (equaltIdent (yyP35, (iclass))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1621 "itcl.puma" */
  {
/* line 1624 "itcl.puma" */
   itcl = itcl + 1;
  }
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yclass,MakeTree,yyV1,kclass,Tree_InitHead)
    begintTree (yyV1->class.env)
    yyV1->class.next = yyP36;
    yyV1->class.qualification = make_qualification (yyP34);
    yyV1->class.block = yyP34->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->class.epos = yyP34->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->class.attribute = attr;
   return yyV1;
  }

  }
  }
  }
  }
  }
 }
 {
  if (equaltIdent (yyP35, (iitcl_class))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1627 "itcl.puma" */
  {
/* line 1630 "itcl.puma" */
   itcl15 = itcl15 + 1;
  }
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yitcl_class,MakeTree,yyV1,kitcl_class,Tree_InitHead)
    begintTree (yyV1->itcl_class.env)
    yyV1->itcl_class.next = yyP36;
    yyV1->itcl_class.qualification = make_qualification (yyP34);
    yyV1->itcl_class.block = yyP34->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->itcl_class.epos = yyP34->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->itcl_class.attribute = attr;
   return yyV1;
  }

  }
  }
  }
  }
  }
 }
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP35, (ibody))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1633 "itcl.puma" */
  {
/* line 1637 "itcl.puma" */
   itcl = itcl + 1;
/* line 1638 "itcl.puma" */

/* line 1639 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ybody,MakeTree,yyV1,kbody,Tree_InitHead)
    begintTree (yyV1->body.env)
    yyV1->body.next = yyP36;
    yyV1->body.qualification = make_qualification (yyP34);
    yyV1->body.param_names = ReverseTree (param_names (yyP34->word_c.next));
    yyV1->body.parameter = yyP34->word_c.next;
    yyV1->body.block = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->body.epos = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->body.attribute = attr;
   p = yyV1;
/* line 1641 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP34->word_c.next->word_c.next = yyV2;
  }
  {
   return p;
  }

  }
  }
  }
  }
  }
  }
 }
 {
  if (equaltIdent (yyP35, (iconfigbody))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1644 "itcl.puma" */
  {
/* line 1647 "itcl.puma" */
   itcl = itcl + 1;
  }
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yconfigbody,MakeTree,yyV1,kconfigbody,Tree_InitHead)
    begintTree (yyV1->configbody.env)
    yyV1->configbody.next = yyP36;
    yyV1->configbody.qualification = make_qualification (yyP34);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW1,knoword,Tree_InitHead)
    yyV1->configbody.param_names = yyW1;
    begintTree (yyW1->noword.env)
   }
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW2,knoword,Tree_InitHead)
    yyV1->configbody.parameter = yyW2;
    begintTree (yyW2->noword.env)
   }
    yyV1->configbody.block = yyP34->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->configbody.epos = yyP34->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->configbody.attribute = attr;
   return yyV1;
  }

  }
  }
  }
  }
  }
 }
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP35, (iconstructor))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (yyP34->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->qual_word.next->Kind == knoword) {
  if (yyP34->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1651 "itcl.puma" */
  {
/* line 1655 "itcl.puma" */
   itcl = itcl + 1;
/* line 1656 "itcl.puma" */

/* line 1657 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yconstructor,MakeTree,yyV1,kconstructor,Tree_InitHead)
    begintTree (yyV1->constructor.env)
    yyV1->constructor.next = yyP36;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW1,klocal_ident,Tree_InitHead)
    yyV1->constructor.qualification = yyW1;
    yyW1->local_ident.pos = gpos;
    begintTree (yyW1->local_ident.env)
    yyW1->local_ident.ident = iconstructor;
   }
    yyV1->constructor.param_names = ReverseTree (param_names (yyP34));
    yyV1->constructor.parameter = yyP34;
    yyV1->constructor.block = yyP34->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->constructor.epos = yyP34->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->constructor.attribute = attr;
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW2,knoword,Tree_InitHead)
    yyV1->constructor.init = yyW2;
    begintTree (yyW2->noword.env)
   }
   p = yyV1;
/* line 1659 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP34->word_c.next = yyV2;
  }
  {
   return p;
  }

  }
  }
  }
  }
  }
  }
 }
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP35, (iconstructor))) {
  if (Tree_IsType (yyP34, kword_c)) {
  if (Tree_IsType (yyP34->word_c.next, kword_c)) {
  if (yyP34->word_c.next->word_c.next->Kind == kqual_word) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1662 "itcl.puma" */
  {
/* line 1666 "itcl.puma" */
   itcl = itcl + 1;
/* line 1667 "itcl.puma" */

/* line 1668 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yconstructor,MakeTree,yyV1,kconstructor,Tree_InitHead)
    begintTree (yyV1->constructor.env)
    yyV1->constructor.next = yyP36;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW1,klocal_ident,Tree_InitHead)
    yyV1->constructor.qualification = yyW1;
    yyW1->local_ident.pos = gpos;
    begintTree (yyW1->local_ident.env)
    yyW1->local_ident.ident = iconstructor;
   }
    yyV1->constructor.param_names = ReverseTree (param_names (yyP34));
    yyV1->constructor.parameter = yyP34;
    yyV1->constructor.block = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->constructor.epos = yyP34->word_c.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->constructor.attribute = attr;
    yyV1->constructor.init = yyP34->word_c.next;
   p = yyV1;
/* line 1670 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP34->word_c.next = yyV2;
  }
  {
   return p;
  }

  }
  }
  }
  }
  }
  }
 }
 {
  if (equaltIdent (yyP35, (idestructor))) {
  if (yyP34->Kind == kqual_word) {
  if (yyP34->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1673 "itcl.puma" */
  {
/* line 1675 "itcl.puma" */
   itcl = itcl + 1;
  }
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ydestructor,MakeTree,yyV1,kdestructor,Tree_InitHead)
    begintTree (yyV1->destructor.env)
    yyV1->destructor.next = yyP36;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW1,klocal_ident,Tree_InitHead)
    yyV1->destructor.qualification = yyW1;
    yyW1->local_ident.pos = gpos;
    begintTree (yyW1->local_ident.env)
    yyW1->local_ident.ident = idestructor;
   }
   {register tTree yyW2;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW2,knoword,Tree_InitHead)
    yyV1->destructor.param_names = yyW2;
    begintTree (yyW2->noword.env)
   }
   {register tTree yyW3;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW3,knoword,Tree_InitHead)
    yyV1->destructor.parameter = yyW3;
    begintTree (yyW3->noword.env)
   }
    yyV1->destructor.block = yyP34->qual_word.qualification->local_text.texts;
    yyV1->destructor.epos = yyP34->qual_word.qualification->local_text.texts->block.epos;
    yyV1->destructor.attribute = attr;
   return yyV1;
  }

  }
  }
  }
  }
 }
/* line 1679 "itcl.puma" */
   return NULL;

}

tTree Mblock_content
#if defined __STDC__ | defined __cplusplus
(tPosition yyP39, tPosition yyP38, register tTree yyP37)
#else
(yyP39, yyP38, yyP37)
 tPosition yyP39;
 tPosition yyP38;
 register tTree yyP37;
#endif
{
  if (yyP37->Kind == kstmt) {
  if (yyP37->stmt.next->Kind == knostmt) {
  if (yyP37->stmt.words->Kind == kone_word) {
  if (yyP37->stmt.words->one_word.next->Kind == knoword) {
/* line 1687 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ycontent,MakeTree,yyV1,kcontent,Tree_InitHead)
    begintTree (yyV1->content.env)
    yyV1->content.next = NoTree;
    yyV1->content.pos = yyP39;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW1,klocal_ident,Tree_InitHead)
    yyV1->content.qualification = yyW1;
    yyW1->local_ident.pos = yyP37->stmt.words->one_word.pos;
    begintTree (yyW1->local_ident.env)
    yyW1->local_ident.ident = yyP37->stmt.words->one_word.ident;
   }
   return yyV1;
  }
 }

  }
  }
  if (yyP37->stmt.words->Kind == kqual_word) {
  if (yyP37->stmt.words->qual_word.next->Kind == knoword) {
/* line 1691 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ycontent,MakeTree,yyV1,kcontent,Tree_InitHead)
    begintTree (yyV1->content.env)
    yyV1->content.next = NoTree;
    yyV1->content.pos = yyP39;
    yyV1->content.qualification = yyP37->stmt.words->qual_word.qualification;
   return yyV1;
  }
 }

  }
  }
  }
  }
/* line 1695 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yblock_content,MakeTree,yyV1,kblock_content,Tree_InitHead)
    begintTree (yyV1->block_content.env)
    yyV1->block_content.next = NoTree;
    yyV1->block_content.pos = yyP39;
    yyV1->block_content.epos = yyP38;
    yyV1->block_content.stmts = yyP37;
   return yyV1;
  }
 }

}

static tTree make_qualification
#if defined __STDC__ | defined __cplusplus
(register tTree yyP40)
#else
(yyP40)
 register tTree yyP40;
#endif
{
 yyRecursion:
  if (yyP40->Kind == kone_word) {
/* line 1703 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ylocal_ident,MakeTree,yyV1,klocal_ident,Tree_InitHead)
    yyV1->local_ident.pos = yyP40->one_word.pos;
    begintTree (yyV1->local_ident.env)
    yyV1->local_ident.ident = yyP40->one_word.ident;
   return yyV1;
  }
 }

  }
  if (yyP40->Kind == kqual_word) {
/* line 1706 "itcl.puma" */
   return yyP40->qual_word.qualification;

  }
  if (yyP40->Kind == kqual_words) {
/* line 1709 "itcl.puma" */
   yyP40 = yyP40->qual_words.qualifications;
   goto yyRecursion;

  }
  if (yyP40->Kind == kone_qualification) {
  if (yyP40->one_qualification.next->Kind == knoqualification) {
/* line 1712 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_word,MakeTree,yyV1,kqual_word,Tree_InitHead)
    begintTree (yyV1->qual_word.env)
    begintTree (yyV1->qual_word.next)
    yyV1->qual_word.qualification = yyP40->one_qualification.qualification;
   yyP40 = yyV1;
   goto yyRecursion;
  }
 }

  }
/* line 1715 "itcl.puma" */
   yyP40 = yyP40->one_qualification.next;
   goto yyRecursion;

  }
 yyAbort ("make_qualification");
 return 0;
}

static tTree param_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP41)
#else
(yyP41)
 register tTree yyP41;
#endif
{
  if (yyP41->Kind == kone_word) {
/* line 1723 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW1,knoword,Tree_InitHead)
    yyV1->one_word.next = yyW1;
    begintTree (yyW1->noword.env)
   }
    yyV1->one_word.ident = yyP41->one_word.ident;
    yyV1->one_word.pos = yyP41->one_word.pos;
   return yyV1;
  }
 }

  }
  if (yyP41->Kind == kqual_word) {
  if (yyP41->qual_word.qualification->Kind == klocal_text) {
  if (yyP41->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1726 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV1,knoword,Tree_InitHead)
    begintTree (yyV1->noword.env)
   return param_names_2 (yyP41->qual_word.qualification->local_text.texts->block.stmts, yyV1);
  }
 }

  }
  }
/* line 1729 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynoword,MakeTree,yyW1,knoword,Tree_InitHead)
    yyV1->one_word.next = yyW1;
    begintTree (yyW1->noword.env)
   }
    yyV1->one_word.ident = make_one_word (yyP41->qual_word.qualification);
    yyV1->one_word.pos = yyP41->qual_word.qualification->qualification_c.pos;
   return yyV1;
  }
 }

  }
  if (yyP41->Kind == kqual_words) {
/* line 1732 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV1,knoword,Tree_InitHead)
    begintTree (yyV1->noword.env)
   return yyV1;
  }
 }

  }
 yyAbort ("param_names");
 return 0;
}

static tTree param_names_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP43, register tTree yyP42)
#else
(yyP43, yyP42)
 register tTree yyP43;
 register tTree yyP42;
#endif
{
  if (yyP43->Kind == kstmt) {
/* line 1740 "itcl.puma" */
   return param_names_2 (yyP43->stmt.next, param_names_3 (yyP43->stmt.words, yyP42));

  }
/* line 1743 "itcl.puma" */
   return yyP42;

}

static tTree param_names_3
#if defined __STDC__ | defined __cplusplus
(register tTree yyP45, register tTree yyP44)
#else
(yyP45, yyP44)
 register tTree yyP45;
 register tTree yyP44;
#endif
{
  if (yyP45->Kind == kone_word) {
/* line 1751 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP44;
    yyV1->one_word.ident = yyP45->one_word.ident;
    yyV1->one_word.pos = yyP45->one_word.pos;
   return param_names_3 (yyP45->one_word.next, yyV1);
  }
 }

  }
  if (yyP45->Kind == kqual_word) {
  if (yyP45->qual_word.qualification->Kind == klocal_text) {
  if (yyP45->qual_word.qualification->local_text.texts->Kind == kblock) {
  if (yyP45->qual_word.qualification->local_text.texts->block.stmts->Kind == kstmt) {
/* line 1754 "itcl.puma" */
   return param_names_3 (yyP45->qual_word.next, param_names_4 (yyP45->qual_word.qualification->local_text.texts->block.stmts->stmt.words, yyP44));

  }
  }
  }
/* line 1758 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP44;
    yyV1->one_word.ident = make_one_word (yyP45->qual_word.qualification);
    yyV1->one_word.pos = yyP45->qual_word.qualification->qualification_c.pos;
   return param_names_3 (yyP45->qual_word.next, yyV1);
  }
 }

  }
/* line 1761 "itcl.puma" */
   return yyP44;

}

static tTree param_names_4
#if defined __STDC__ | defined __cplusplus
(register tTree yyP47, register tTree yyP46)
#else
(yyP47, yyP46)
 register tTree yyP47;
 register tTree yyP46;
#endif
{
  if (yyP47->Kind == kone_word) {
/* line 1769 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP46;
    yyV1->one_word.ident = yyP47->one_word.ident;
    yyV1->one_word.pos = yyP47->one_word.pos;
   return yyV1;
  }
 }

  }
  if (yyP47->Kind == kqual_word) {
/* line 1772 "itcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP46;
    yyV1->one_word.ident = make_one_word (yyP47->qual_word.qualification);
    yyV1->one_word.pos = yyP47->qual_word.qualification->qualification_c.pos;
   return yyV1;
  }
 }

  }
/* line 1775 "itcl.puma" */
   return yyP46;

}

static void get_end_pos
#if defined __STDC__ | defined __cplusplus
(register tTree yyP48, tPosition * yyP49)
#else
(yyP48, yyP49)
 register tTree yyP48;
 tPosition * yyP49;
#endif
{
  if (yyP48->Kind == kone_word) {
/* line 1783 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1784 "itcl.puma" */

/* line 1784 "itcl.puma" */
   e = yyP48->one_word.pos;
/* line 1785 "itcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP48->one_word.ident)) - 1;
  }
   * yyP49 = e;
   return;
 }

  }
  if (yyP48->Kind == kqual_word) {
/* line 1787 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1788 "itcl.puma" */
   get_end_pos (yyP48->qual_word.qualification, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  }
  if (yyP48->Kind == kqual_words) {
/* line 1790 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1791 "itcl.puma" */
   get_end_pos (yyP48->qual_words.qualifications, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  }
  if (yyP48->Kind == kone_qualification) {
/* line 1793 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1794 "itcl.puma" */
   get_end_pos (yyP48->one_qualification.qualification, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  }
  if (Tree_IsType (yyP48, ktext)) {
  if (Tree_IsType (yyP48->text.next, ktext)) {
/* line 1796 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1797 "itcl.puma" */
   get_end_pos (yyP48->text.next, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  }
  }

  switch (yyP48->Kind) {
  case kcharacter:
/* line 1799 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1800 "itcl.puma" */

/* line 1800 "itcl.puma" */
   e = yyP48->character.pos;
  }
   * yyP49 = e;
   return;
 }

  case kident:
/* line 1802 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1803 "itcl.puma" */

/* line 1803 "itcl.puma" */
   e = yyP48->ident.pos;
/* line 1804 "itcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP48->ident.ident)) - 1;
  }
   * yyP49 = e;
   return;
 }

  case kblock:
/* line 1806 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1807 "itcl.puma" */

/* line 1807 "itcl.puma" */
   e = yyP48->block.epos;
  }
   * yyP49 = e;
   return;
 }

  case kcontent:
/* line 1809 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1810 "itcl.puma" */
   get_end_pos (yyP48->content.qualification, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  case kblock_content:
/* line 1812 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1813 "itcl.puma" */

/* line 1813 "itcl.puma" */
   e = yyP48->block_content.epos;
  }
   * yyP49 = e;
   return;
 }

  case klocal_ident:
/* line 1815 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1816 "itcl.puma" */

/* line 1816 "itcl.puma" */
   e = yyP48->local_ident.pos;
/* line 1817 "itcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP48->local_ident.ident)) - 1;
  }
   * yyP49 = e;
   return;
 }

  case kglobal_ident:
/* line 1819 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1820 "itcl.puma" */

/* line 1820 "itcl.puma" */
   e = yyP48->global_ident.pos;
/* line 1821 "itcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP48->global_ident.ident)) - 1;
  }
   * yyP49 = e;
   return;
 }

  case klocal_text:
/* line 1823 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1824 "itcl.puma" */
   get_end_pos (yyP48->local_text.texts, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  case kglobal_text:
/* line 1826 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1827 "itcl.puma" */
   get_end_pos (yyP48->global_text.texts, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  case kqualification:
/* line 1829 "itcl.puma" */
 {
  tPosition e;
  {
/* line 1830 "itcl.puma" */

/* line 1830 "itcl.puma" */
   e = yyP48->qualification.pos;
/* line 1831 "itcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP48->qualification.ident)) - 1;
  }
   * yyP49 = e;
   return;
 }

  case kcomplex_qual:
/* line 1833 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1834 "itcl.puma" */
   get_end_pos (yyP48->complex_qual.texts, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  case ksubscription:
/* line 1836 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1837 "itcl.puma" */
   get_end_pos (yyP48->subscription.qualification, & yyV1);
  }
   * yyP49 = yyV1;
   return;
 }

  }

;
}

static void get_begin_pos
#if defined __STDC__ | defined __cplusplus
(register tTree yyP50, tPosition * yyP51)
#else
(yyP50, yyP51)
 register tTree yyP50;
 tPosition * yyP51;
#endif
{
  if (yyP50->Kind == kqualification) {
/* line 1844 "itcl.puma" */
   * yyP51 = yyP50->qualification.qualification->qualification_c.pos;
   return;

  }
  if (yyP50->Kind == kcomplex_qual) {
  if (yyP50->complex_qual.qualification->Kind == kqualification) {
/* line 1845 "itcl.puma" */
   * yyP51 = yyP50->complex_qual.qualification->qualification.pos;
   return;

  }
  }
/* line 1846 "itcl.puma" */
   * yyP51 = yyP50->qualification_c.pos;
   return;

;
}

static void get_end_pos_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP52, tPosition * yyP53)
#else
(yyP52, yyP53)
 register tTree yyP52;
 tPosition * yyP53;
#endif
{
  if (Tree_IsType (yyP52, kword_c)) {
  if (yyP52->word_c.next->Kind == knoword) {
/* line 1852 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1853 "itcl.puma" */
   get_end_pos (yyP52, & yyV1);
  }
   * yyP53 = yyV1;
   return;
 }

  }
/* line 1855 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1856 "itcl.puma" */
   get_end_pos_2 (yyP52->word_c.next, & yyV1);
  }
   * yyP53 = yyV1;
   return;
 }

  }
;
}

static rbool get_script
#if defined __STDC__ | defined __cplusplus
(register tTree yyP54, register tTree * yyP56, tPosition * yyP55)
#else
(yyP54, yyP56, yyP55)
 register tTree yyP54;
 register tTree * yyP56;
 tPosition * yyP55;
#endif
{
  if (yyP54->Kind == kone_word) {
/* line 1863 "itcl.puma" */
 {
  tTree yyV1;
  tPosition yyV2;
  {
/* line 1864 "itcl.puma" */
   if (! (strncmp (GetCStr (yyP54->one_word.ident), "-l", 2) == 0)) goto yyL1;
  {
/* line 1865 "itcl.puma" */
   if (! (get_script (yyP54->one_word.next, & yyV1, & yyV2))) goto yyL1;
  }
  }
   * yyP56 = yyV1;
   * yyP55 = yyV2;
   return rtrue;
 }
yyL1:;

  if (Tree_IsType (yyP54->one_word.next, kword_c)) {
/* line 1867 "itcl.puma" */
 {
  tTree yyV1;
  tPosition yyV2;
  {
/* line 1868 "itcl.puma" */
   if (! (strncmp (GetCStr (yyP54->one_word.ident), "-e", 2) == 0)) goto yyL2;
  {
/* line 1869 "itcl.puma" */
   if (! (get_script (yyP54->one_word.next->word_c.next, & yyV1, & yyV2))) goto yyL2;
  }
  }
   * yyP56 = yyV1;
   * yyP55 = yyV2;
   return rtrue;
 }
yyL2:;

  }
/* line 1871 "itcl.puma" */
 {
  tTree yyV1;
  tPosition yyV2;
  {
/* line 1872 "itcl.puma" */
   if (! (strncmp (GetCStr (yyP54->one_word.ident), "--", 2) == 0)) goto yyL3;
  {
/* line 1873 "itcl.puma" */
   if (! (get_script (yyP54->one_word.next, & yyV1, & yyV2))) goto yyL3;
  }
  }
   * yyP56 = yyV1;
   * yyP55 = yyV2;
   return rtrue;
 }
yyL3:;

  }
  if (yyP54->Kind == kqual_word) {
  if (yyP54->qual_word.qualification->Kind == klocal_text) {
  if (yyP54->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1875 "itcl.puma" */
   * yyP56 = yyP54->qual_word.qualification->local_text.texts;
   * yyP55 = yyP54->qual_word.qualification->local_text.texts->block.epos;
   return rtrue;

  }
  }
  }
  return rfalse;
}

static void itcl_info
#if defined __STDC__ | defined __cplusplus
(register tTree yyP57)
#else
(yyP57)
 register tTree yyP57;
#endif
{
 yyRecursion:
  if (yyP57->Kind == kone_word) {
  if (yyP57->one_word.next->Kind == kone_word) {
/* line 1883 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1886 "itcl.puma" */
   if (! (strncmp (GetCStr (yyP57->one_word.ident), "-c", 2) == 0 || strncmp (GetCStr (yyP57->one_word.ident), "-i", 2) == 0)) goto yyL1;
  {
/* line 1887 "itcl.puma" */

/* line 1887 "itcl.puma" */
   obj = Identify (yyP57->one_word.next->one_word.ident, yyP57->one_word.env);
/* line 1888 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 1889 "itcl.puma" */
   use (obj, yyP57->one_word.next->one_word.pos, PAF_REF_READ);
/* line 1890 "itcl.puma" */
   yyP57 = yyP57->one_word.next->one_word.next;
   goto yyRecursion;
  }
  }
  }
 }
yyL1:;

  }
  }
  if (Tree_IsType (yyP57, kword_c)) {
/* line 1892 "itcl.puma" */
  {
/* line 1893 "itcl.puma" */
   yyP57 = yyP57->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static tTree get_env
#if defined __STDC__ | defined __cplusplus
(register tTree yyP58)
#else
(yyP58)
 register tTree yyP58;
#endif
{
  if (yyP58->Kind == kqualification) {
/* line 1900 "itcl.puma" */
   return get_env_2 (yyP58->qualification.qualification);

  }
  if (yyP58->Kind == kcomplex_qual) {
/* line 1900 "itcl.puma" */
   return get_env_2 (yyP58->complex_qual.qualification);

  }
  if (yyP58->Kind == ksubscription) {
/* line 1900 "itcl.puma" */
   return get_env_2 (yyP58->subscription.qualification);

  }
/* line 1900 "itcl.puma" */
   return get_env_2 (yyP58);

}

static tTree get_env_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP59)
#else
(yyP59)
 register tTree yyP59;
#endif
{
 yyRecursion:

  switch (yyP59->Kind) {
  case klocal_ident:
/* line 1911 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1912 "itcl.puma" */

/* line 1912 "itcl.puma" */
   obj = Identify (yyP59->local_ident.ident, yyP59->local_ident.env);
/* line 1913 "itcl.puma" */
   if (! (obj != NULL && Tree_IsType (obj -> object . object, knamespace_c))) goto yyL1;
  {
/* line 1914 "itcl.puma" */
   use (obj, yyP59->local_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj -> object . object -> namespace_c . block -> words . env;
  }
 }
yyL1:;

/* line 1917 "itcl.puma" */
  {
/* line 1918 "itcl.puma" */
   use_undef (yyP59->local_ident.ident, yyP59->local_ident.pos);
  }
   return NULL;

  case kglobal_ident:
/* line 1921 "itcl.puma" */
 {
  tTree obj;
  {
/* line 1922 "itcl.puma" */

/* line 1922 "itcl.puma" */
   obj = IdentifyGlobal (yyP59->global_ident.ident);
/* line 1923 "itcl.puma" */
   if (! (obj != NULL && Tree_IsType (obj -> object . object, knamespace_c))) goto yyL3;
  {
/* line 1924 "itcl.puma" */
   use (obj, yyP59->global_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj -> object . object -> namespace_c . block -> words . env;
  }
 }
yyL3:;

/* line 1927 "itcl.puma" */
  {
/* line 1928 "itcl.puma" */
   use_undef (yyP59->global_ident.ident, yyP59->global_ident.pos);
  }
   return NULL;

  case kqualification:
/* line 1931 "itcl.puma" */
 {
  tTree env;
  tTree obj;
  {
/* line 1932 "itcl.puma" */

/* line 1932 "itcl.puma" */
   env = get_env_2 (yyP59->qualification.qualification);
/* line 1933 "itcl.puma" */
   if (! (env != NULL)) goto yyL5;
  {
/* line 1934 "itcl.puma" */

/* line 1934 "itcl.puma" */
   obj = IdentifyMember (yyP59->qualification.ident, env);
/* line 1935 "itcl.puma" */
   if (! (obj != NULL && Tree_IsType (obj -> object . object, knamespace_c))) goto yyL5;
  {
/* line 1936 "itcl.puma" */
   use (obj, yyP59->qualification.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj -> object . object -> namespace_c . block -> words . env;
  }
 }
yyL5:;

/* line 1939 "itcl.puma" */
  {
/* line 1940 "itcl.puma" */
   use_undef (yyP59->qualification.ident, yyP59->qualification.pos);
  }
   return NULL;

  case kcomplex_qual:
/* line 1943 "itcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 1944 "itcl.puma" */

/* line 1944 "itcl.puma" */
   env = get_env_2 (yyP59->complex_qual.qualification);
/* line 1945 "itcl.puma" */
   if (! (env != NULL)) goto yyL7;
  {
/* line 1946 "itcl.puma" */

/* line 1946 "itcl.puma" */
   ident = make_one_word (yyP59->complex_qual.texts);
/* line 1947 "itcl.puma" */

/* line 1947 "itcl.puma" */
   obj = IdentifyMember (ident, env);
/* line 1948 "itcl.puma" */
   if (! (obj != NULL && Tree_IsType (obj -> object . object, knamespace_c))) goto yyL7;
  {
/* line 1949 "itcl.puma" */
   use (obj, yyP59->complex_qual.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj -> object . object -> namespace_c . block -> words . env;
  }
 }
yyL7:;

  break;
  case ksubscription:
/* line 1952 "itcl.puma" */
   yyP59 = yyP59->subscription.qualification;
   goto yyRecursion;

  case klocal_text:
/* line 1955 "itcl.puma" */
   return NULL;

  case kglobal_text:
/* line 1958 "itcl.puma" */
   return NULL;

  }

/* line 1961 "itcl.puma" */
   return NULL;

}

static tIdent get_ident
#if defined __STDC__ | defined __cplusplus
(register tTree yyP60)
#else
(yyP60)
 register tTree yyP60;
#endif
{
 yyRecursion:
  if (yyP60->Kind == kqualification) {
/* line 1969 "itcl.puma" */
   return yyP60->qualification.ident;

  }
  if (yyP60->Kind == kcomplex_qual) {
/* line 1972 "itcl.puma" */
   return make_one_word (yyP60->complex_qual.texts);

  }
  if (yyP60->Kind == ksubscription) {
/* line 1975 "itcl.puma" */
   yyP60 = yyP60->subscription.qualification;
   goto yyRecursion;

  }
  if (yyP60->Kind == klocal_ident) {
/* line 1978 "itcl.puma" */
   return yyP60->local_ident.ident;

  }
  if (yyP60->Kind == kglobal_ident) {
/* line 1981 "itcl.puma" */
   return yyP60->global_ident.ident;

  }
/* line 1984 "itcl.puma" */
   return NoIdent;

}

static void add_qualifications
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP62, register tTree yyP61, register int attr)
#else
(yyP62, yyP61, attr)
 register tIdent yyP62;
 register tTree yyP61;
 register int attr;
#endif
{
 yyRecursion:
  if (equaltIdent (yyP62, (iset))) {
  if (yyP61->word_c.next->Kind == knoword) {
/* line 1992 "itcl.puma" */
  {
/* line 1993 "itcl.puma" */
   add_qual (yyP61, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (iset))) {
/* line 1995 "itcl.puma" */
  {
/* line 1996 "itcl.puma" */
   add_qual (yyP61, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP62, (ivariable))) {
  if (yyP61->word_c.next->Kind == knoword) {
/* line 1998 "itcl.puma" */
  {
/* line 1999 "itcl.puma" */
   add_variable_quals (yyP61, PAF_REF_READ, PAF_PUBLIC);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (ivariable))) {
/* line 2001 "itcl.puma" */
  {
/* line 2002 "itcl.puma" */
   add_variable_quals (yyP61, PAF_REF_WRITE, PAF_PUBLIC);
  }
   return;

  }
  if (equaltIdent (yyP62, (icommon))) {
  if (yyP61->word_c.next->Kind == knoword) {
/* line 2004 "itcl.puma" */
  {
/* line 2005 "itcl.puma" */
   add_qual (yyP61, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (icommon))) {
/* line 2007 "itcl.puma" */
  {
/* line 2008 "itcl.puma" */
   add_qual (yyP61, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP62, (ipublic))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
/* line 2010 "itcl.puma" */
  {
/* line 2012 "itcl.puma" */
   yyP62 = yyP61->one_word.ident;
   yyP61 = yyP61->one_word.next;
   attr = PAF_PUBLIC;
   goto yyRecursion;
  }

  }
  }
  }
  if (equaltIdent (yyP62, (iprotected))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
/* line 2014 "itcl.puma" */
  {
/* line 2016 "itcl.puma" */
   yyP62 = yyP61->one_word.ident;
   yyP61 = yyP61->one_word.next;
   attr = PAF_PROTECTED;
   goto yyRecursion;
  }

  }
  }
  }
  if (equaltIdent (yyP62, (iprivate))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
/* line 2018 "itcl.puma" */
  {
/* line 2020 "itcl.puma" */
   yyP62 = yyP61->one_word.ident;
   yyP61 = yyP61->one_word.next;
   attr = PAF_PRIVATE;
   goto yyRecursion;
  }

  }
  }
  }
  if (equaltIdent (yyP62, (iinherit))) {
/* line 2027 "itcl.puma" */
  {
/* line 2028 "itcl.puma" */
   inherit_var (yyP61);
  }
   return;

  }
  if (equaltIdent (yyP62, (iappend))) {
/* line 2030 "itcl.puma" */
  {
/* line 2031 "itcl.puma" */
   add_qual (yyP61, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP62, (ilappend))) {
/* line 2033 "itcl.puma" */
  {
/* line 2034 "itcl.puma" */
   add_qual (yyP61, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP62, (iarray))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (iset))) {
/* line 2036 "itcl.puma" */
  {
/* line 2038 "itcl.puma" */
   add_qual (yyP61->one_word.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP62, (iarray))) {
  if (Tree_IsType (yyP61->word_c.next, kword_c)) {
/* line 2040 "itcl.puma" */
  {
/* line 2041 "itcl.puma" */
   add_qual (yyP61->word_c.next, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (ibinary))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (iscan))) {
/* line 2043 "itcl.puma" */
  {
/* line 2047 "itcl.puma" */
   add_quals (yyP61->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP62, (iscan))) {
  if (Tree_IsType (yyP61->word_c.next, kword_c)) {
  if (Tree_IsType (yyP61->word_c.next->word_c.next, kword_c)) {
/* line 2049 "itcl.puma" */
  {
/* line 2052 "itcl.puma" */
   add_quals (yyP61->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP62, (iforeach))) {
/* line 2054 "itcl.puma" */
  {
/* line 2055 "itcl.puma" */
   add_foreach_quals (yyP61);
  }
   return;

  }
  if (equaltIdent (yyP62, (ivwait))) {
/* line 2057 "itcl.puma" */
  {
/* line 2058 "itcl.puma" */
   add_qual (yyP61, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP62, (iincr))) {
/* line 2060 "itcl.puma" */
  {
/* line 2061 "itcl.puma" */
   add_qual (yyP61, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP62, (iparray))) {
/* line 2063 "itcl.puma" */
  {
/* line 2064 "itcl.puma" */
   add_qual (yyP61, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP62, (icatch))) {
  if (Tree_IsType (yyP61->word_c.next, kword_c)) {
/* line 2066 "itcl.puma" */
  {
/* line 2067 "itcl.puma" */
   add_qual (yyP61->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (igets))) {
  if (Tree_IsType (yyP61->word_c.next, kword_c)) {
/* line 2069 "itcl.puma" */
  {
/* line 2070 "itcl.puma" */
   add_qual (yyP61->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP62, (iinfo))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (iexists))) {
/* line 2072 "itcl.puma" */
  {
/* line 2074 "itcl.puma" */
   add_qual (yyP61->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP62, (iinfo))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (iargs))) {
/* line 2076 "itcl.puma" */
  {
/* line 2078 "itcl.puma" */
   add_qual (yyP61->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP62, (iinfo))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (ibody))) {
/* line 2080 "itcl.puma" */
  {
/* line 2082 "itcl.puma" */
   add_qual (yyP61->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP62, (iinfo))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (idefault))) {
/* line 2084 "itcl.puma" */
  {
/* line 2088 "itcl.puma" */
   add_qual (yyP61, PAF_REF_READ);
/* line 2089 "itcl.puma" */
   add_qual (yyP61->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP62, (itkinfo))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (ivariable))) {
/* line 2091 "itcl.puma" */
  {
/* line 2093 "itcl.puma" */
   add_qual (yyP61->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP62, (ifile))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (ilstat))) {
/* line 2095 "itcl.puma" */
  {
/* line 2098 "itcl.puma" */
   add_qual (yyP61->one_word.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP62, (ifile))) {
  if (yyP61->Kind == kone_word) {
  if (Tree_IsType (yyP61->one_word.next, kword_c)) {
  if (Tree_IsType (yyP61->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP61->one_word.ident, (istat))) {
/* line 2100 "itcl.puma" */
  {
/* line 2103 "itcl.puma" */
   add_qual (yyP61->one_word.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP62, (iunset))) {
/* line 2105 "itcl.puma" */
  {
/* line 2106 "itcl.puma" */
   add_quals (yyP61, PAF_REF_WRITE);
  }
   return;

  }
;
}

static void inherit_var
#if defined __STDC__ | defined __cplusplus
(register tTree w)
#else
(w)
 register tTree w;
#endif
{
 yyRecursion:
  if (w->Kind == kone_word) {
/* line 2121 "itcl.puma" */
  {
/* line 2122 "itcl.puma" */
   inherit_var (w->one_word.next);
/* line 2123 "itcl.puma" */
   inherit_var_2 (w->one_word.ident, w->one_word.pos, w->one_word.env, Identify (w->one_word.ident, w->one_word.env));
  }
   return;

  }
  if (w->Kind == kqual_word) {
/* line 2125 "itcl.puma" */
  {
/* line 2126 "itcl.puma" */
   inherit_var (w->qual_word.next);
/* line 2127 "itcl.puma" */
   w = w->qual_word.qualification;
   goto yyRecursion;
  }

  }
  if (w->Kind == kqual_words) {
/* line 2129 "itcl.puma" */
  {
/* line 2130 "itcl.puma" */
   w = w->qual_words.next;
   goto yyRecursion;
  }

  }
  if (w->Kind == kglobal_ident) {
/* line 2132 "itcl.puma" */
  {
/* line 2133 "itcl.puma" */
   inherit_var_2 (w->global_ident.ident, w->global_ident.pos, w->global_ident.env, IdentifyGlobal (w->global_ident.ident));
  }
   return;

  }
  if (w->Kind == kqualification) {
/* line 2135 "itcl.puma" */
 {
  tTree e2;
  {
/* line 2136 "itcl.puma" */

/* line 2136 "itcl.puma" */
   e2 = get_env (w);
/* line 2137 "itcl.puma" */
   if (! (e2 != NULL)) goto yyL5;
  {
/* line 2138 "itcl.puma" */
   inherit_var_2 (w->qualification.ident, w->qualification.pos, w->qualification.env, IdentifyMember (w->qualification.ident, e2));
  }
  }
   return;
 }
yyL5:;

/* line 2140 "itcl.puma" */
  {
/* line 2141 "itcl.puma" */
   inherit_var_2 (w->qualification.ident, w->qualification.pos, w->qualification.env, NULL);
  }
   return;

  }
;
}

static void inherit_var_2
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP66, tPosition yyP65, register tTree yyP64, register tTree yyP63)
#else
(yyP66, yyP65, yyP64, yyP63)
 register tIdent yyP66;
 tPosition yyP65;
 register tTree yyP64;
 register tTree yyP63;
#endif
{
  if (yyP63 == NULL) {
/* line 2148 "itcl.puma" */
  {
/* line 2149 "itcl.puma" */
   put_symbol (PAF_CLASS_INHERIT, cur_class_ptr, GetCStr (yyP66), current_file, (int)  yyP65 . Line, (int)  yyP65 . Column - 1, (int)  yyP65 . Line, (int)  (yyP65 . Column + StLength (GetStringRef (yyP66)) - 1), PAF_PUBLIC, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
  if (yyP63->Kind == kobject) {
  if (Tree_IsType (yyP63->object.object, knamespace_c)) {
/* line 2154 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 2156 "itcl.puma" */
   put_symbol (PAF_CLASS_INHERIT, cur_class_ptr, GetCStr (yyP66), current_file, (int)  yyP65 . Line, (int)  yyP65 . Column - 1, (int)  yyP65 . Line, (int)  (yyP65 . Column + StLength (GetStringRef (yyP66)) - 1), PAF_PUBLIC, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
/* line 2160 "itcl.puma" */
   use (yyP63, yyP65, PAF_REF_READ);
/* line 2161 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yenv2,MakeTree,yyV1,kenv2,Tree_InitHead)
    yyV1->env2.env1 = yyP63->object.object->namespace_c.block->texts.env;
    yyV1->env2.env2 = yyP64->env.env;
   yyP64->env.env = yyV1;
  }
   return;
 }

  }
  }
;
}

static void add_variable_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP67, register int acc, register int attr)
#else
(yyP67, acc, attr)
 register tTree yyP67;
 register int acc;
 register int attr;
#endif
{
/* line 2168 "itcl.puma" */
 tTree obj, e2; 
 yyRecursion:
  if (yyP67->Kind == kqual_word) {
/* line 2170 "itcl.puma" */
  {
/* line 2171 "itcl.puma" */
   yyP67 = yyP67->qual_word.qualification;
   goto yyRecursion;
  }

  }
  if (yyP67->Kind == kqual_words) {
/* line 2173 "itcl.puma" */
   return;

  }
  if (yyP67->Kind == ksubscription) {
/* line 2175 "itcl.puma" */
  {
/* line 2176 "itcl.puma" */
   yyP67 = yyP67->subscription.qualification;
   goto yyRecursion;
  }

  }
  if (Tree_IsType (yyP67, kqualification_c)) {
/* line 2178 "itcl.puma" */
 {
  tIdent ident;
  tTree obj;
  register tTree yyV1;
  {
/* line 2179 "itcl.puma" */
   e2 = get_env (yyP67);
/* line 2180 "itcl.puma" */
   if (! (e2 != NULL)) goto yyL4;
  {
/* line 2181 "itcl.puma" */

/* line 2181 "itcl.puma" */
   ident = get_ident (yyP67);
/* line 2182 "itcl.puma" */

/* line 2182 "itcl.puma" */
   obj = IdentifyMember (ident, e2 -> env . objects);
/* line 2183 "itcl.puma" */
   if (! (obj == NULL)) goto yyL4;
  {
/* line 2184 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = yyP67;
    yyV1->object.ident = ident;
    yyV1->object.next = e2 -> env . objects;
   e2 -> env . objects = yyV1;
/* line 2185 "itcl.puma" */
   yyP67->qualification_c.env = e2;
/* line 2186 "itcl.puma" */
   dcl (e2 -> env . objects, attr);
/* line 2187 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (e2->env.objects, yyP67->qualification_c.pos, acc); 
  }
  }
  }
   return;
 }
yyL4:;

/* line 2189 "itcl.puma" */
  {
/* line 2190 "itcl.puma" */
   if (! (e2 != NULL)) goto yyL5;
  {
/* line 2191 "itcl.puma" */
   use (obj, yyP67->qualification_c.pos, acc);
  }
  }
   return;
yyL5:;

  }
;
}

static void add_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP68, register int acc)
#else
(yyP68, acc)
 register tTree yyP68;
 register int acc;
#endif
{
 yyRecursion:
  if (Tree_IsType (yyP68, kword_c)) {
/* line 2198 "itcl.puma" */
  {
/* line 2199 "itcl.puma" */
   add_qual (yyP68, acc);
/* line 2200 "itcl.puma" */
   yyP68 = yyP68->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_foreach_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP69)
#else
(yyP69)
 register tTree yyP69;
#endif
{
 yyRecursion:
  if (yyP69->Kind == kqual_word) {
  if (Tree_IsType (yyP69->qual_word.next, kword_c)) {
  if (yyP69->qual_word.qualification->Kind == klocal_text) {
  if (yyP69->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 2207 "itcl.puma" */
  {
/* line 2209 "itcl.puma" */
   add_foreach_quals (yyP69->qual_word.qualification->local_text.texts->block.stmts);
/* line 2210 "itcl.puma" */
   yyP69 = yyP69->qual_word.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  }
  }
  if (Tree_IsType (yyP69, kword_c)) {
  if (Tree_IsType (yyP69->word_c.next, kword_c)) {
/* line 2212 "itcl.puma" */
  {
/* line 2213 "itcl.puma" */
   add_qual (yyP69, PAF_REF_WRITE);
/* line 2214 "itcl.puma" */
   yyP69 = yyP69->word_c.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  if (yyP69->Kind == kstmt) {
/* line 2216 "itcl.puma" */
  {
/* line 2217 "itcl.puma" */
   add_quals (yyP69->stmt.words, PAF_REF_WRITE);
/* line 2218 "itcl.puma" */
   yyP69 = yyP69->stmt.next;
   goto yyRecursion;
  }

  }
;
}

static void add_qual
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc)
#else
(w, acc)
 register tTree w;
 register int acc;
#endif
{
 yyRecursion:
  if (w->Kind == kqual_word) {
/* line 2225 "itcl.puma" */
  {
/* line 2226 "itcl.puma" */
   add_qual_2 (w->qual_word.qualification, acc);
  }
   return;

  }
  if (w->Kind == kqual_words) {
/* line 2228 "itcl.puma" */
  {
/* line 2229 "itcl.puma" */
   w = w->qual_words.qualifications;
   goto yyRecursion;
  }

  }
  if (w->Kind == kone_qualification) {
  if (w->one_qualification.next->Kind == knoqualification) {
/* line 2231 "itcl.puma" */
  {
/* line 2232 "itcl.puma" */
   add_qual_2 (w->one_qualification.qualification, acc);
  }
   return;

  }
/* line 2234 "itcl.puma" */
  {
/* line 2235 "itcl.puma" */
   w = w->one_qualification.next;
   goto yyRecursion;
  }

  }
;
}

static void add_qual_2
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc)
#else
(w, acc)
 register tTree w;
 register int acc;
#endif
{
/* line 2242 "itcl.puma" */
 tIdent id; tTree env; 
 yyRecursion:
  if (w->Kind == kglobal_ident) {
/* line 2244 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2245 "itcl.puma" */

/* line 2245 "itcl.puma" */
   obj = IdentifyGlobal (w->global_ident.ident);
/* line 2246 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 2247 "itcl.puma" */
   use (obj, w->global_ident.pos, acc);
  }
  }
   return;
 }
yyL1:;

/* line 2249 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 2250 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 2251 "itcl.puma" */
   dcl (genv -> env . objects, PAF_PUBLIC);
/* line 2252 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_ident.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == kglobal_text) {
/* line 2254 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2255 "itcl.puma" */
   id = make_one_word (w->global_text.texts);
/* line 2256 "itcl.puma" */

/* line 2256 "itcl.puma" */
   obj = IdentifyGlobal (id);
/* line 2257 "itcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 2258 "itcl.puma" */
   use (obj, w->global_text.pos, acc);
  }
  }
   return;
 }
yyL3:;

/* line 2260 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 2261 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = id;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 2262 "itcl.puma" */
   dcl (genv -> env . objects, PAF_PUBLIC);
/* line 2263 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_text.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == klocal_text) {
/* line 2265 "itcl.puma" */
   return;

  }
  if (w->Kind == ksubscription) {
  if (w->subscription.qualification->Kind == klocal_ident) {
/* line 2267 "itcl.puma" */
   return;

  }
  if (w->subscription.qualification->Kind == klocal_text) {
/* line 2269 "itcl.puma" */
   return;

  }
/* line 2271 "itcl.puma" */
  {
/* line 2272 "itcl.puma" */
   w = w->subscription.qualification;
   goto yyRecursion;
  }

  }
/* line 2274 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2275 "itcl.puma" */
   env = get_env (w);
/* line 2276 "itcl.puma" */
   if (! (env != NULL)) goto yyL9;
  {
/* line 2277 "itcl.puma" */
   id = get_ident (w);
/* line 2278 "itcl.puma" */

/* line 2278 "itcl.puma" */
   obj = IdentifyMember (id, env -> env . objects);
/* line 2279 "itcl.puma" */
   if (! (obj != NULL)) goto yyL9;
  {
/* line 2280 "itcl.puma" */
   use (obj, w->qualification_c.pos, acc);
  }
  }
  }
   return;
 }
yyL9:;

/* line 2282 "itcl.puma" */
 {
  register tTree yyV1;
  {
/* line 2283 "itcl.puma" */
   if (! (env != NULL)) goto yyL10;
  {
/* line 2284 "itcl.puma" */
   w->qualification_c.env = env;
/* line 2285 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = id;
    yyV1->object.next = env -> env . objects;
   env -> env . objects = yyV1;
/* line 2286 "itcl.puma" */
   dcl (env -> env . objects, PAF_PUBLIC);
/* line 2287 "itcl.puma" */
  if (acc == PAF_REF_WRITE) use (env->env.objects, w->qualification_c.pos, acc); 
  }
  }
   return;
 }
yyL10:;

;
}

static tTree use_qual
#if defined __STDC__ | defined __cplusplus
(register tTree yyP70)
#else
(yyP70)
 register tTree yyP70;
#endif
{
 yyRecursion:
  if (yyP70->Kind == kglobal_ident) {
/* line 2294 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2295 "itcl.puma" */

/* line 2295 "itcl.puma" */
   obj = IdentifyGlobal (yyP70->global_ident.ident);
/* line 2296 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 2297 "itcl.puma" */
   use (obj, yyP70->global_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj;
  }
 }
yyL1:;

/* line 2300 "itcl.puma" */
  {
/* line 2301 "itcl.puma" */
   use_undef (yyP70->global_ident.ident, yyP70->global_ident.pos);
  }
   return NULL;

  }
  if (yyP70->Kind == klocal_ident) {
/* line 2304 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2305 "itcl.puma" */

/* line 2305 "itcl.puma" */
   obj = Identify (yyP70->local_ident.ident, yyP70->local_ident.env);
/* line 2306 "itcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 2307 "itcl.puma" */
   use (obj, yyP70->local_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj;
  }
 }
yyL3:;

/* line 2310 "itcl.puma" */
  {
/* line 2311 "itcl.puma" */
   use_undef (yyP70->local_ident.ident, yyP70->local_ident.pos);
  }
   return NULL;

  }
  if (yyP70->Kind == ksubscription) {
/* line 2330 "itcl.puma" */
   yyP70 = yyP70->subscription.qualification;
   goto yyRecursion;

  }
/* line 2333 "itcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 2334 "itcl.puma" */

/* line 2334 "itcl.puma" */
   env = get_env (yyP70);
/* line 2335 "itcl.puma" */
   if (! (env != NULL)) goto yyL6;
  {
/* line 2336 "itcl.puma" */

/* line 2336 "itcl.puma" */
   ident = get_ident (yyP70);
/* line 2337 "itcl.puma" */

/* line 2337 "itcl.puma" */
   obj = IdentifyMember (ident, env);
/* line 2338 "itcl.puma" */
   if (! (obj != NULL)) goto yyL6;
  {
/* line 2339 "itcl.puma" */
   use (obj, yyP70->qualification_c.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj;
  }
 }
yyL6:;

/* line 2342 "itcl.puma" */
 {
  tTree env;
  tIdent ident;
  {
/* line 2343 "itcl.puma" */

/* line 2343 "itcl.puma" */
   env = get_env (yyP70);
/* line 2344 "itcl.puma" */
   if (! (env != NULL)) goto yyL7;
  {
/* line 2345 "itcl.puma" */

/* line 2345 "itcl.puma" */
   ident = get_ident (yyP70);
/* line 2346 "itcl.puma" */
   use_undef (ident, yyP70->qualification_c.pos);
  }
  }
  {
   return NULL;
  }
 }
yyL7:;

/* line 2349 "itcl.puma" */
   return NULL;

}

static tTree use_proc_qual
#if defined __STDC__ | defined __cplusplus
(register tTree yyP71)
#else
(yyP71)
 register tTree yyP71;
#endif
{
  if (yyP71->Kind == kglobal_ident) {
/* line 2357 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2358 "itcl.puma" */

/* line 2358 "itcl.puma" */
   obj = IdentifyProcGlobal (yyP71->global_ident.ident);
/* line 2359 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 2360 "itcl.puma" */
   use (obj, yyP71->global_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj;
  }
 }
yyL1:;

/* line 2363 "itcl.puma" */
  {
/* line 2364 "itcl.puma" */
   use_proc_undef (yyP71->global_ident.ident, yyP71->global_ident.pos);
  }
   return NULL;

  }
  if (yyP71->Kind == klocal_ident) {
/* line 2367 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2368 "itcl.puma" */

/* line 2368 "itcl.puma" */
   obj = IdentifyProc (yyP71->local_ident.ident, yyP71->local_ident.env);
/* line 2369 "itcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 2370 "itcl.puma" */
   use (obj, yyP71->local_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj;
  }
 }
yyL3:;

/* line 2373 "itcl.puma" */
  {
/* line 2374 "itcl.puma" */
   use_proc_undef (yyP71->local_ident.ident, yyP71->local_ident.pos);
  }
   return NULL;

  }
/* line 2377 "itcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 2378 "itcl.puma" */

/* line 2378 "itcl.puma" */
   env = get_env (yyP71);
/* line 2379 "itcl.puma" */
   if (! (env != NULL)) goto yyL5;
  {
/* line 2380 "itcl.puma" */

/* line 2380 "itcl.puma" */
   ident = get_ident (yyP71);
/* line 2381 "itcl.puma" */

/* line 2381 "itcl.puma" */
   obj = IdentifyProcMember (ident, env);
/* line 2382 "itcl.puma" */
   if (! (obj != NULL)) goto yyL5;
  {
/* line 2383 "itcl.puma" */
   use (obj, yyP71->qualification_c.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj;
  }
 }
yyL5:;

/* line 2386 "itcl.puma" */
 {
  tTree env;
  tIdent ident;
  {
/* line 2387 "itcl.puma" */

/* line 2387 "itcl.puma" */
   env = get_env (yyP71);
/* line 2388 "itcl.puma" */
   if (! (env != NULL)) goto yyL6;
  {
/* line 2389 "itcl.puma" */

/* line 2389 "itcl.puma" */
   ident = get_ident (yyP71);
/* line 2390 "itcl.puma" */
   use_proc_undef (ident, yyP71->qualification_c.pos);
  }
  }
  {
   return NULL;
  }
 }
yyL6:;

/* line 2393 "itcl.puma" */
   return NULL;

}

static tTree use_proc
#if defined __STDC__ | defined __cplusplus
(register tTree yyP72)
#else
(yyP72)
 register tTree yyP72;
#endif
{
 yyRecursion:
  if (yyP72->Kind == kone_word) {
/* line 2401 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2402 "itcl.puma" */

/* line 2402 "itcl.puma" */
   obj = IdentifyProc (yyP72->one_word.ident, yyP72->one_word.env);
/* line 2403 "itcl.puma" */
  if (obj) {
      use (obj, yyP72->one_word.pos, PAF_REF_READ);
   } else {
      use_proc_undef (yyP72->one_word.ident, yyP72->one_word.pos);
   }

  }
  {
   return obj;
  }
 }

  }
  if (yyP72->Kind == kqual_word) {
/* line 2411 "itcl.puma" */
   return use_proc_qual (yyP72->qual_word.qualification);

  }
  if (yyP72->Kind == kqual_words) {
/* line 2414 "itcl.puma" */
   yyP72 = yyP72->qual_words.qualifications;
   goto yyRecursion;

  }
  if (yyP72->Kind == kone_qualification) {
  if (yyP72->one_qualification.next->Kind == knoqualification) {
/* line 2417 "itcl.puma" */
   return use_proc_qual (yyP72->one_qualification.qualification);

  }
/* line 2420 "itcl.puma" */
   yyP72 = yyP72->one_qualification.next;
   goto yyRecursion;

  }
 yyAbort ("use_proc");
 return 0;
}

static tTree use_word
#if defined __STDC__ | defined __cplusplus
(register tTree yyP73)
#else
(yyP73)
 register tTree yyP73;
#endif
{
 yyRecursion:
  if (yyP73->Kind == kone_word) {
/* line 2428 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2429 "itcl.puma" */

/* line 2429 "itcl.puma" */
   obj = Identify (yyP73->one_word.ident, yyP73->one_word.env);
/* line 2430 "itcl.puma" */
  if (obj) switch (obj->object.object->Kind) {
   case kproc		:
   case kmethod		:
   case kconstructor	:
   case kdestructor	:
   case knamespace	:
   case kclass		:
   case kitcl_class	:
   case kobj_word	: use (obj, yyP73->one_word.pos, PAF_REF_READ);
   } else {
      use_undef (yyP73->one_word.ident, yyP73->one_word.pos);
   }

  }
  {
   return obj;
  }
 }

  }
  if (yyP73->Kind == kqual_word) {
/* line 2445 "itcl.puma" */
   return use_qual (yyP73->qual_word.qualification);

  }
  if (yyP73->Kind == kqual_words) {
/* line 2448 "itcl.puma" */
   yyP73 = yyP73->qual_words.qualifications;
   goto yyRecursion;

  }
  if (yyP73->Kind == kone_qualification) {
  if (yyP73->one_qualification.next->Kind == knoqualification) {
/* line 2451 "itcl.puma" */
   return use_qual (yyP73->one_qualification.qualification);

  }
/* line 2454 "itcl.puma" */
   yyP73 = yyP73->one_qualification.next;
   goto yyRecursion;

  }
 yyAbort ("use_word");
 return 0;
}

static void use
#if defined __STDC__ | defined __cplusplus
(register tTree yyP74, tPosition p, register int acc)
#else
(yyP74, p, acc)
 register tTree yyP74;
 tPosition p;
 register int acc;
#endif
{
 yyRecursion:

  switch (yyP74->Kind) {
  case kobject:
/* line 2462 "itcl.puma" */
  {
/* line 2463 "itcl.puma" */
   if (! ((cross_ref_fp))) goto yyL1;
  {
/* line 2464 "itcl.puma" */
   current_ident = GetCStr (yyP74->object.ident);
/* line 2465 "itcl.puma" */
   yyP74 = yyP74->object.object;
   goto yyRecursion;
  }
  }
yyL1:;

  break;
  case kone_word:
/* line 2467 "itcl.puma" */
  {
/* line 2472 "itcl.puma" */
   use_2 (yyP74->one_word.env, p, acc);
  }
   return;

  case kobj_word:
/* line 2467 "itcl.puma" */
  {
/* line 2472 "itcl.puma" */
   use_2 (yyP74->obj_word.env, p, acc);
  }
   return;

  case klocal_ident:
/* line 2467 "itcl.puma" */
  {
/* line 2472 "itcl.puma" */
   use_2 (yyP74->local_ident.env, p, acc);
  }
   return;

  case kglobal_ident:
/* line 2467 "itcl.puma" */
  {
/* line 2472 "itcl.puma" */
   use_2 (yyP74->global_ident.env, p, acc);
  }
   return;

  case kqualification_c:
  case klocal_text:
  case kglobal_text:
  case kqualification:
  case kcomplex_qual:
  case ksubscription:
/* line 2467 "itcl.puma" */
  {
/* line 2472 "itcl.puma" */
   use_2 (yyP74->qualification_c.env, p, acc);
  }
   return;

  case kproc_c:
  case kproc:
  case kmethod:
  case kbody:
  case kconfigbody:
  case kconstructor:
  case kdestructor:
  if (yyP74->proc_c.env->Kind == kenv) {
  if (Tree_IsType (yyP74->proc_c.env->env.object, knamespace_c)) {
/* line 2474 "itcl.puma" */
  {
/* line 2476 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_MBR_FUNC, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, cur_arg_types_ptr, current_namespace (yyP74->proc_c.env), current_ident, NULL, current_file, (int)  p . Line, PAF_REF_READ);
  }
   return;

  }
  }
/* line 2481 "itcl.puma" */
  {
/* line 2483 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_FUNCTION, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, cur_arg_types_ptr, NULL, current_ident, NULL, current_file, (int)  p . Line, PAF_REF_READ);
  }
   return;

  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
/* line 2487 "itcl.puma" */
  {
/* line 2488 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_CLASS, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, cur_arg_types_ptr, NULL, current_ident, NULL, current_file, (int)  p . Line, PAF_REF_READ);
  }
   return;

  }

;
}

static void use_2
#if defined __STDC__ | defined __cplusplus
(register tTree env, tPosition pos, register int acc)
#else
(env, pos, acc)
 register tTree env;
 tPosition pos;
 register int acc;
#endif
{
  if (Tree_IsType (env->env.object, kproc_c)) {
/* line 2497 "itcl.puma" */
  {
/* line 2499 "itcl.puma" */
   if (! ((report_local_vars))) goto yyL1;
  {
/* line 2500 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
  }
   return;
yyL1:;

  }
  if (env->env.object->Kind == kqual_word) {
/* line 2497 "itcl.puma" */
  {
/* line 2499 "itcl.puma" */
   if (! ((report_local_vars))) goto yyL2;
  {
/* line 2500 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
  }
   return;
yyL2:;

  }
  if (env->env.object->Kind == kprogram) {
/* line 2504 "itcl.puma" */
  {
/* line 2505 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_GLOB_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
   return;

  }
  if (Tree_IsType (env->env.object, knamespace_c)) {
/* line 2509 "itcl.puma" */
  {
/* line 2510 "itcl.puma" */
   put_cross_ref (PAF_REF_TO_MBR_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, NULL, current_namespace (env), current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
   return;

  }
;
}

static void use_undef
#if defined __STDC__ | defined __cplusplus
(register tIdent ident, tPosition pos)
#else
(ident, pos)
 register tIdent ident;
 tPosition pos;
#endif
{
/* line 2519 "itcl.puma" */
 {
  char c;
  {
/* line 2520 "itcl.puma" */
   if (! (ident > last_keyword)) goto yyL1;
  {
/* line 2521 "itcl.puma" */

/* line 2521 "itcl.puma" */
   c = * GetCStr (ident);
/* line 2522 "itcl.puma" */
   if (! ('A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || c == '_')) goto yyL1;
  {
/* line 2523 "itcl.puma" */
   put_cross_ref (PAF_REF_UNDEFINED, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, GetCStr (ident), NULL, current_file, (int)  pos . Line, PAF_REF_READ);
  }
  }
  }
   return;
 }
yyL1:;

;
}

static void use_proc_undef
#if defined __STDC__ | defined __cplusplus
(register tIdent ident, tPosition pos)
#else
(ident, pos)
 register tIdent ident;
 tPosition pos;
#endif
{
/* line 2532 "itcl.puma" */
  {
/* line 2533 "itcl.puma" */
   if (! (! in_string)) goto yyL1;
  {
/* line 2534 "itcl.puma" */
   use_undef (ident, pos);
  }
  }
   return;
yyL1:;

;
}

static void dcl
#if defined __STDC__ | defined __cplusplus
(register tTree yyP75, register int attr)
#else
(yyP75, attr)
 register tTree yyP75;
 register int attr;
#endif
{
 yyRecursion:

  switch (yyP75->Kind) {
  case kobject:
/* line 2546 "itcl.puma" */
  {
/* line 2547 "itcl.puma" */
   if (! (yyP75->object.ident != NoIdent)) goto yyL1;
  {
/* line 2548 "itcl.puma" */
   current_ident = GetCStr (yyP75->object.ident);
/* line 2551 "itcl.puma" */
   if (! (* current_ident != '-')) goto yyL1;
  {
/* line 2552 "itcl.puma" */
   yyP75 = yyP75->object.object;
   goto yyRecursion;
  }
  }
  }
yyL1:;

  break;
  case kone_word:
/* line 2554 "itcl.puma" */
  {
/* line 2559 "itcl.puma" */
   dcl_2 (yyP75->one_word.env, yyP75->one_word.pos, attr);
  }
   return;

  case kobj_word:
/* line 2554 "itcl.puma" */
  {
/* line 2559 "itcl.puma" */
   dcl_2 (yyP75->obj_word.env, yyP75->obj_word.pos, attr);
  }
   return;

  case klocal_ident:
/* line 2554 "itcl.puma" */
  {
/* line 2559 "itcl.puma" */
   dcl_2 (yyP75->local_ident.env, yyP75->local_ident.pos, attr);
  }
   return;

  case kglobal_ident:
/* line 2554 "itcl.puma" */
  {
/* line 2559 "itcl.puma" */
   dcl_2 (yyP75->global_ident.env, yyP75->global_ident.pos, attr);
  }
   return;

  case kqualification_c:
  case klocal_text:
  case kglobal_text:
  case kqualification:
  case kcomplex_qual:
  case ksubscription:
/* line 2554 "itcl.puma" */
  {
/* line 2559 "itcl.puma" */
   dcl_2 (yyP75->qualification_c.env, yyP75->qualification_c.pos, attr);
  }
   return;

  case kbody:
/* line 2561 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 2563 "itcl.puma" */
   to_names (yyP75->body.param_names, args_buffer);
/* line 2564 "itcl.puma" */
   current_class = get_class (yyP75->body.qualification);
/* line 2565 "itcl.puma" */
   get_begin_pos (yyP75->body.qualification, & yyV1);
/* line 2566 "itcl.puma" */
   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file, (int)  yyP75->body.qualification->qualification_c.pos . Line, (int)  yyP75->body.qualification->qualification_c.pos . Column - 1, (int)  yyP75->body.epos . Line, (int)  yyP75->body.epos . Column, attr, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->body.qualification->qualification_c.pos . Line, (int)  (yyP75->body.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  case kproc_c:
  case kproc:
  case kmethod:
  case kconfigbody:
  case kconstructor:
  case kdestructor:
  if (yyP75->proc_c.env->Kind == kenv) {
  if (Tree_IsType (yyP75->proc_c.env->env.object, knamespace_c)) {
  if (yyP75->proc_c.block->Kind == knotext) {
/* line 2585 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 2589 "itcl.puma" */
   to_names (yyP75->proc_c.param_names, args_buffer);
/* line 2590 "itcl.puma" */
   current_class = current_namespace (yyP75->proc_c.env);
/* line 2591 "itcl.puma" */
   get_begin_pos (yyP75->proc_c.qualification, & yyV1);
/* line 2592 "itcl.puma" */
   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  yyP75->proc_c.qualification->qualification_c.pos . Column - 1, (int)  yyP75->proc_c.epos . Line, (int)  yyP75->proc_c.epos . Column, attr | add_attr (yyP75), NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  (yyP75->proc_c.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  }
/* line 2598 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 2601 "itcl.puma" */
   to_names (yyP75->proc_c.param_names, args_buffer);
/* line 2602 "itcl.puma" */
   current_class = current_namespace (yyP75->proc_c.env);
/* line 2603 "itcl.puma" */
   get_begin_pos (yyP75->proc_c.qualification, & yyV1);
/* line 2604 "itcl.puma" */
   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  yyP75->proc_c.qualification->qualification_c.pos . Column - 1, (int)  yyP75->proc_c.epos . Line, (int)  yyP75->proc_c.epos . Column, attr, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  (yyP75->proc_c.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
/* line 2609 "itcl.puma" */
   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  yyP75->proc_c.qualification->qualification_c.pos . Column - 1, (int)  yyP75->proc_c.epos . Line, (int)  yyP75->proc_c.epos . Column, attr | add_attr (yyP75), NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  (yyP75->proc_c.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  }
  }
/* line 2615 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 2617 "itcl.puma" */
   to_names (yyP75->proc_c.param_names, args_buffer);
/* line 2618 "itcl.puma" */
   get_begin_pos (yyP75->proc_c.qualification, & yyV1);
/* line 2619 "itcl.puma" */
   put_symbol (PAF_FUNC_DEF, NULL, current_ident, current_file, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  yyP75->proc_c.qualification->qualification_c.pos . Column - 1, (int)  yyP75->proc_c.epos . Line, (int)  yyP75->proc_c.epos . Column, attr, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->proc_c.qualification->qualification_c.pos . Line, (int)  (yyP75->proc_c.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
/* line 2631 "itcl.puma" */
 {
  tPosition yyV1;
  {
/* line 2633 "itcl.puma" */
   get_begin_pos (yyP75->namespace_c.qualification, & yyV1);
/* line 2634 "itcl.puma" */
   put_symbol (PAF_CLASS_DEF, current_namespace (yyP75->namespace_c.env), current_ident, current_file, (int)  yyP75->namespace_c.qualification->qualification_c.pos . Line, (int)  yyP75->namespace_c.qualification->qualification_c.pos . Column - 1, (int)  yyP75->namespace_c.epos . Line, (int)  yyP75->namespace_c.epos . Column, 0, NULL, NULL, NULL, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP75->namespace_c.qualification->qualification_c.pos . Line, (int)  (yyP75->namespace_c.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  }

;
}

static void dcl_2
#if defined __STDC__ | defined __cplusplus
(register tTree env, tPosition pos, register int attr)
#else
(env, pos, attr)
 register tTree env;
 tPosition pos;
 register int attr;
#endif
{
  if (Tree_IsType (env->env.object, kproc_c)) {
/* line 2645 "itcl.puma" */
  {
/* line 2647 "itcl.puma" */
   if (! ((report_local_vars))) goto yyL1;
  {
/* line 2648 "itcl.puma" */
   put_symbol (PAF_LOCAL_VAR_DEF, NULL, current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), attr, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
  }
   return;
yyL1:;

  }
  if (env->env.object->Kind == kqual_word) {
/* line 2645 "itcl.puma" */
  {
/* line 2647 "itcl.puma" */
   if (! ((report_local_vars))) goto yyL2;
  {
/* line 2648 "itcl.puma" */
   put_symbol (PAF_LOCAL_VAR_DEF, NULL, current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), attr, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
  }
   return;
yyL2:;

  }
  if (env->env.object->Kind == kprogram) {
/* line 2653 "itcl.puma" */
  {
/* line 2654 "itcl.puma" */
   put_symbol (PAF_GLOB_VAR_DEF, NULL, current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), attr, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
  if (Tree_IsType (env->env.object, knamespace_c)) {
/* line 2659 "itcl.puma" */
  {
/* line 2660 "itcl.puma" */
   put_symbol (PAF_MBR_VAR_DEF, current_namespace (env), current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), attr, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
;
}

static tString current_namespace
#if defined __STDC__ | defined __cplusplus
(register tTree yyP76)
#else
(yyP76)
 register tTree yyP76;
#endif
{
 yyRecursion:
  if (yyP76->Kind == kenv) {
  if (yyP76->env.object->Kind == kprogram) {
/* line 2670 "itcl.puma" */
   return NULL;

  }
  if (Tree_IsType (yyP76->env.object, knamespace_c)) {
/* line 2673 "itcl.puma" */
   return GetCStr (get_ident (yyP76->env.object->namespace_c.qualification));

  }
/* line 2676 "itcl.puma" */
   yyP76 = yyP76->env.env;
   goto yyRecursion;

  }
 yyAbort ("current_namespace");
 return 0;
}

static tString get_class
#if defined __STDC__ | defined __cplusplus
(register tTree yyP77)
#else
(yyP77)
 register tTree yyP77;
#endif
{
  if (yyP77->Kind == kqualification) {
/* line 2684 "itcl.puma" */
   return get_class_name (yyP77->qualification.qualification);

  }
  if (yyP77->Kind == kcomplex_qual) {
/* line 2687 "itcl.puma" */
   return get_class_name (yyP77->complex_qual.qualification);

  }
/* line 2690 "itcl.puma" */
   return NULL;

}

static tString get_class_name
#if defined __STDC__ | defined __cplusplus
(register tTree yyP78)
#else
(yyP78)
 register tTree yyP78;
#endif
{
  if (yyP78->Kind == klocal_ident) {
/* line 2698 "itcl.puma" */
   return GetCStr (yyP78->local_ident.ident);

  }
  if (yyP78->Kind == kglobal_ident) {
/* line 2698 "itcl.puma" */
   return GetCStr (yyP78->global_ident.ident);

  }
  if (yyP78->Kind == kqualification) {
/* line 2698 "itcl.puma" */
   return GetCStr (yyP78->qualification.ident);

  }
/* line 2703 "itcl.puma" */
   return NULL;

}

static void to_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP80, register tString yyP79)
#else
(yyP80, yyP79)
 register tTree yyP80;
 register tString yyP79;
#endif
{
/* line 2711 "itcl.puma" */
  {
/* line 2711 "itcl.puma" */
   p = yyP79;
/* line 2711 "itcl.puma" */
   get_names (yyP80);
/* line 2711 "itcl.puma" */
   * p = '\0';
  }
   return;

;
}

static void get_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP81)
#else
(yyP81)
 register tTree yyP81;
#endif
{
 yyRecursion:
  if (yyP81->Kind == kone_word) {
/* line 2715 "itcl.puma" */
  {
/* line 2716 "itcl.puma" */
   GetString (yyP81->one_word.ident, p);
/* line 2716 "itcl.puma" */
   p = p + LengthSt (GetStringRef (yyP81->one_word.ident));
/* line 2717 "itcl.puma" */
   get_separator (yyP81->one_word.next);
/* line 2718 "itcl.puma" */
   yyP81 = yyP81->one_word.next;
   goto yyRecursion;
  }

  }
;
}

static void get_separator
#if defined __STDC__ | defined __cplusplus
(register tTree yyP82)
#else
(yyP82)
 register tTree yyP82;
#endif
{
  if (yyP82->Kind == kone_word) {
/* line 2725 "itcl.puma" */
  {
/* line 2725 "itcl.puma" */
   * p ++ = ',';
  }
   return;

  }
;
}

static void relocate
#if defined __STDC__ | defined __cplusplus
(register tTree yyP83, register tTree new_env)
#else
(yyP83, new_env)
 register tTree yyP83;
 register tTree new_env;
#endif
{
 yyRecursion:

  switch (yyP83->Kind) {
  case kstmt:
/* line 2731 "itcl.puma" */
  {
/* line 2732 "itcl.puma" */
   yyP83->stmt.env = new_env;
/* line 2733 "itcl.puma" */
   relocate (yyP83->stmt.words, new_env);
/* line 2734 "itcl.puma" */
   yyP83 = yyP83->stmt.next;
   goto yyRecursion;
  }

  case kproc_c:
  case kproc:
  case kmethod:
  case kbody:
  case kconfigbody:
  case kconstructor:
  case kdestructor:
  if (yyP83->proc_c.block->texts.env->Kind == kenv) {
/* line 2736 "itcl.puma" */
  {
/* line 2739 "itcl.puma" */
   yyP83->proc_c.env = new_env;
/* line 2740 "itcl.puma" */
   yyP83->proc_c.block->texts.env->env.env = new_env;
/* line 2741 "itcl.puma" */
   relocate (yyP83->proc_c.qualification, new_env);
/* line 2742 "itcl.puma" */
   yyP83 = yyP83->proc_c.next;
   goto yyRecursion;
  }

  }
  break;
  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
  if (yyP83->namespace_c.block->texts.env->Kind == kenv) {
/* line 2744 "itcl.puma" */
  {
/* line 2746 "itcl.puma" */
   yyP83->namespace_c.env = new_env;
/* line 2747 "itcl.puma" */
   yyP83->namespace_c.block->texts.env->env.env = new_env;
/* line 2748 "itcl.puma" */
   relocate (yyP83->namespace_c.qualification, new_env);
/* line 2749 "itcl.puma" */
   yyP83 = yyP83->namespace_c.next;
   goto yyRecursion;
  }

  }
  break;
  case kone_word:
/* line 2751 "itcl.puma" */
  {
/* line 2752 "itcl.puma" */
   yyP83->one_word.env = new_env;
/* line 2753 "itcl.puma" */
   yyP83 = yyP83->one_word.next;
   goto yyRecursion;
  }

  case kobj_word:
/* line 2755 "itcl.puma" */
  {
/* line 2756 "itcl.puma" */
   yyP83->obj_word.env = new_env;
/* line 2757 "itcl.puma" */
   yyP83 = yyP83->obj_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 2759 "itcl.puma" */
  {
/* line 2760 "itcl.puma" */
   yyP83->qual_word.env = new_env;
/* line 2761 "itcl.puma" */
   relocate (yyP83->qual_word.qualification, new_env);
/* line 2762 "itcl.puma" */
   yyP83 = yyP83->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 2764 "itcl.puma" */
  {
/* line 2765 "itcl.puma" */
   yyP83->qual_words.env = new_env;
/* line 2766 "itcl.puma" */
   relocate (yyP83->qual_words.qualifications, new_env);
/* line 2767 "itcl.puma" */
   yyP83 = yyP83->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 2769 "itcl.puma" */
  {
/* line 2770 "itcl.puma" */
   yyP83->one_qualification.env = new_env;
/* line 2771 "itcl.puma" */
   relocate (yyP83->one_qualification.qualification, new_env);
/* line 2772 "itcl.puma" */
   yyP83 = yyP83->one_qualification.next;
   goto yyRecursion;
  }

  case kcharacter:
/* line 2774 "itcl.puma" */
  {
/* line 2775 "itcl.puma" */
   yyP83->character.env = new_env;
/* line 2776 "itcl.puma" */
   yyP83 = yyP83->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 2778 "itcl.puma" */
  {
/* line 2779 "itcl.puma" */
   yyP83->ident.env = new_env;
/* line 2780 "itcl.puma" */
   yyP83 = yyP83->ident.next;
   goto yyRecursion;
  }

  case kblock:
/* line 2782 "itcl.puma" */
  {
/* line 2783 "itcl.puma" */
   yyP83->block.env = new_env;
/* line 2784 "itcl.puma" */
   relocate (yyP83->block.stmts, new_env);
/* line 2785 "itcl.puma" */
   yyP83 = yyP83->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 2787 "itcl.puma" */
  {
/* line 2788 "itcl.puma" */
   yyP83->content.env = new_env;
/* line 2789 "itcl.puma" */
   relocate (yyP83->content.qualification, new_env);
/* line 2790 "itcl.puma" */
   yyP83 = yyP83->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 2792 "itcl.puma" */
  {
/* line 2793 "itcl.puma" */
   yyP83->block_content.env = new_env;
/* line 2794 "itcl.puma" */
   relocate (yyP83->block_content.stmts, new_env);
/* line 2795 "itcl.puma" */
   yyP83 = yyP83->block_content.next;
   goto yyRecursion;
  }

  case klocal_ident:
/* line 2797 "itcl.puma" */
  {
/* line 2798 "itcl.puma" */
   yyP83->local_ident.env = new_env;
  }
   return;

  case kglobal_ident:
/* line 2800 "itcl.puma" */
  {
/* line 2801 "itcl.puma" */
   yyP83->global_ident.env = new_env;
  }
   return;

  case klocal_text:
/* line 2803 "itcl.puma" */
  {
/* line 2804 "itcl.puma" */
   yyP83->local_text.env = new_env;
/* line 2805 "itcl.puma" */
   yyP83 = yyP83->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 2807 "itcl.puma" */
  {
/* line 2808 "itcl.puma" */
   yyP83->global_text.env = new_env;
/* line 2809 "itcl.puma" */
   yyP83 = yyP83->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 2811 "itcl.puma" */
  {
/* line 2812 "itcl.puma" */
   yyP83->qualification.env = new_env;
/* line 2813 "itcl.puma" */
   yyP83 = yyP83->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 2815 "itcl.puma" */
  {
/* line 2816 "itcl.puma" */
   yyP83->complex_qual.env = new_env;
/* line 2817 "itcl.puma" */
   relocate (yyP83->complex_qual.qualification, new_env);
/* line 2818 "itcl.puma" */
   yyP83 = yyP83->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 2820 "itcl.puma" */
  {
/* line 2821 "itcl.puma" */
   yyP83->subscription.env = new_env;
/* line 2822 "itcl.puma" */
   relocate (yyP83->subscription.qualification, new_env);
/* line 2823 "itcl.puma" */
   yyP83 = yyP83->subscription.index;
   goto yyRecursion;
  }

  }

;
}

static tTree declare_object
#if defined __STDC__ | defined __cplusplus
(register tTree yyP85, register tTree yyP84)
#else
(yyP85, yyP84)
 register tTree yyP85;
 register tTree yyP84;
#endif
{
  if (yyP84 == NULL) {
/* line 2830 "itcl.puma" */
   return yyP85;

  }
  if (yyP85->Kind == kone_word) {
/* line 2833 "itcl.puma" */
 {
  tTree w;
  register tTree yyV1;
  {
/* line 2834 "itcl.puma" */
   if (! (yyP85->one_word.ident != ihash_auto)) goto yyL2;
  {
/* line 2835 "itcl.puma" */

/* line 2836 "itcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobj_word,MakeTree,yyV1,kobj_word,Tree_InitHead)
    yyV1->obj_word.env = yyP85->one_word.env;
    yyV1->obj_word.next = yyP85->one_word.next;
    yyV1->obj_word.ident = yyP85->one_word.ident;
    yyV1->obj_word.pos = yyP85->one_word.pos;
    yyV1->obj_word.object = yyP84;
   w = yyV1;
/* line 2837 "itcl.puma" */
   add_var (w, PAF_REF_READ, PAF_PUBLIC);
  }
  }
  {
   return w;
  }
 }
yyL2:;

  }
/* line 2840 "itcl.puma" */
   return yyP85;

}

static void use_proc_object
#if defined __STDC__ | defined __cplusplus
(register tTree yyP87, register tTree yyP86)
#else
(yyP87, yyP86)
 register tTree yyP87;
 register tTree yyP86;
#endif
{
  if (yyP86->Kind == kone_word) {
  if (yyP86->one_word.next->Kind == kone_word) {
 {
  tTree obj;
  if (equaltIdent (yyP86->one_word.ident, (iisa))) {
/* line 2848 "itcl.puma" */
  {
/* line 2850 "itcl.puma" */

/* line 2850 "itcl.puma" */
   obj = IdentifyProc (yyP86->one_word.next->one_word.ident, yyP86->one_word.env);
/* line 2851 "itcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 2852 "itcl.puma" */
   use (obj, yyP86->one_word.next->one_word.pos, PAF_REF_READ);
  }
  }
   return;
yyL1:;

  }
 }
  }
  }
  if (yyP87->object.object->Kind == kobj_word) {
  if (yyP87->object.object->obj_word.object->Kind == kobject) {
  if (Tree_IsType (yyP87->object.object->obj_word.object->object.object, knamespace_c)) {
  if (yyP86->Kind == kone_word) {
/* line 2854 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2856 "itcl.puma" */

/* line 2856 "itcl.puma" */
   obj = IdentifyProcMember (yyP86->one_word.ident, yyP87->object.object->obj_word.object->object.object->namespace_c.block->texts.env);
/* line 2857 "itcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  {
/* line 2858 "itcl.puma" */
   use (obj, yyP86->one_word.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL2:;

  }
  }
  }
  }
  if (Tree_IsType (yyP87->object.object, knamespace_c)) {
  if (yyP86->Kind == kone_word) {
/* line 2860 "itcl.puma" */
 {
  tTree obj;
  {
/* line 2862 "itcl.puma" */

/* line 2862 "itcl.puma" */
   obj = IdentifyProcMember (yyP86->one_word.ident, yyP87->object.object->namespace_c.block->texts.env);
/* line 2863 "itcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 2864 "itcl.puma" */
   use (obj, yyP86->one_word.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL3:;

  }
  }
;
}

static void get_pos
#if defined __STDC__ | defined __cplusplus
(register tTree yyP88, tPosition * yyP89)
#else
(yyP88, yyP89)
 register tTree yyP88;
 tPosition * yyP89;
#endif
{
  if (yyP88->Kind == kone_word) {
/* line 2871 "itcl.puma" */
   * yyP89 = yyP88->one_word.pos;
   return;

  }
/* line 2872 "itcl.puma" */
   * yyP89 = NoPosition;
   return;

;
}

static long add_attr
#if defined __STDC__ | defined __cplusplus
(register tTree yyP90)
#else
(yyP90)
 register tTree yyP90;
#endif
{
  if (yyP90->Kind == kconstructor) {
/* line 2878 "itcl.puma" */
   return PAF_CONSTRUCTOR;

  }
  if (yyP90->Kind == kdestructor) {
/* line 2879 "itcl.puma" */
   return PAF_DESTRUCTOR;

  }
/* line 2880 "itcl.puma" */
   return 0;

}

void Beginitcl ARGS ((void))
{
/* line 74 "itcl.puma" */

   yyf		= stdout;
   need_pass_2	= rfalse;
   in_string	= rfalse;

   iappend	= MakeID ("append");
   iarray	= MakeID ("array");
   ibreak	= MakeID ("break");
   icatch	= MakeID ("catch");
   icontinue	= MakeID ("continue");
   idefault	= MakeID ("default");
   ifor		= MakeID ("for");
   iforeach	= MakeID ("foreach");
   iglobal	= MakeID ("global");
   iif		= MakeID ("if");
   iincr	= MakeID ("incr");
   iinfo	= MakeID ("info");
   ilappend	= MakeID ("lappend");
   ireturn	= MakeID ("return");
   iset		= MakeID ("set");
   iunset	= MakeID ("unset");
   iwhile	= MakeID ("while");

   last_keyword	= MakeID ("concat");
   last_keyword	= MakeID ("eval");
   last_keyword	= MakeID ("expr");
   last_keyword	= MakeID ("format");
   last_keyword	= MakeID ("glob");
   last_keyword	= MakeID ("join");
   last_keyword	= MakeID ("lindex");
   last_keyword	= MakeID ("linsert");
   last_keyword	= MakeID ("list");
   last_keyword	= MakeID ("llength");
   last_keyword	= MakeID ("lrange");
   last_keyword	= MakeID ("lreplace");
   last_keyword	= MakeID ("lsearch");
   last_keyword	= MakeID ("lsort");
   last_keyword	= MakeID ("regexp");
   last_keyword	= MakeID ("regsub");
   last_keyword	= MakeID ("split");
   last_keyword	= MakeID ("string");
   last_keyword	= MakeID ("switch");
   last_keyword	= MakeID ("uplevel");
   last_keyword	= MakeID ("upvar");
   last_keyword	= MakeID ("winfo");

   iargs	= MakeID ("args");
   ibinary	= MakeID ("binary");
   ibody	= MakeID ("body");
   icatch	= MakeID ("catch");
   ieval	= MakeID ("eval");
   iexists	= MakeID ("exists");
   ifile	= MakeID ("file");
   iforeach	= MakeID ("foreach");
   igets	= MakeID ("gets");
   iglobal	= MakeID ("global");
   ilstat	= MakeID ("lstat");
   inamespace	= MakeID ("namespace");
   iparray	= MakeID ("parray");
   iproc	= MakeID ("proc");
   irename	= MakeID ("rename");
   iscan	= MakeID ("scan");
   isource	= MakeID ("source");
   istat	= MakeID ("stat");
   itkinfo	= MakeID ("tkinfo");
   ivariable	= MakeID ("variable");
   ivwait	= MakeID ("vwait");

   iat_scope	= MakeID ("@scope");
   iclass	= MakeID ("class");
   icommon	= MakeID ("common");
   iconfigbody	= MakeID ("configbody");
   iconstructor	= MakeID ("constructor");
   idelete	= MakeID ("delete");
   idestructor	= MakeID ("destructor");
   ihash_auto	= MakeID ("#auto");
   iinherit	= MakeID ("inherit");
   iisa		= MakeID ("isa");
   iitcl	= MakeID ("itcl");
   iitcl_class	= MakeID ("itcl_class");
   iitcl_info	= MakeID ("itcl_info");
   iitk		= MakeID ("itk");
   iiwidgets	= MakeID ("iwidgets");
   ilocal	= MakeID ("local");
   imethod	= MakeID ("method");
   iobject	= MakeID ("object");
   iobjects	= MakeID ("objects");
   iprevious	= MakeID ("previous");
   iprivate	= MakeID ("private");
   iprotected	= MakeID ("protected");
   ipublic	= MakeID ("public");
   ithis	= MakeID ("this");
   ivirtual	= MakeID ("virtual");

   genv		= mnoobject ();
   predef	= mnostmt ();
   predef	= mclass (predef, mlocal_ident (NoPosition, iitcl),
		     mnotext (), NoPosition, PAF_PUBLIC);
   genv		= mobject (predef, iitcl, genv);
   predef	= mclass (predef, mlocal_ident (NoPosition, iitk),
		     mnotext (), NoPosition, PAF_PUBLIC);
   genv		= mobject (predef, iitk, genv);
   predef	= mclass (predef, mlocal_ident (NoPosition, iiwidgets),
		     mnotext (), NoPosition, PAF_PUBLIC);
   genv		= mobject (predef, iiwidgets, genv);
   predef	= mprogram (predef);
   Eval (predef);

}

void Closeitcl ARGS ((void))
{
}

