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

/* line 5 "tcl.puma" */

#include "Reuse.h"
#include "Position.h"
#include "StringM.h"
#include "Idents.h"
#include "Parser.h"
#include "sn.h"

#define null	(char *) NULL
#define scope_type(cur_class) cur_class ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF

extern	FILE *	cross_ref_fp	;
extern	int	report_local_vars;

static	char	buffer [1024]	;
static	char	args_buffer [1500];
static	int	length		;
static	rbool	need_pass_2	;
static	rbool	in_string	;
static	tTree	genv		;
static	char *	p		;
static	char *	current_ident	;
static	char *	current_class	;
static	char *	cur_proc_ptr	= NULL;
static	char *	cur_class_ptr	= NULL;
static	char *	cur_arg_types_ptr = NULL;


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

void Interpret_tcl ARGS ((tTree yyP1));
static void get_objects ARGS ((tTree s));
static void get_param_objects ARGS ((tTree s));
static void add_variables ARGS ((tIdent yyP3, tTree yyP2));
static void include ARGS ((tTree yyP4));
static void add_global_vars ARGS ((tTree w));
static void add_variable_vars ARGS ((tTree w, int acc));
static void add_variable_vars_2 ARGS ((tTree yyP5));
static void add_vars ARGS ((tTree yyP6, int acc));
static void add_foreach_vars ARGS ((tTree yyP7));
static void add_var ARGS ((tTree w, int acc));
static void add_var_2 ARGS ((tIdent i, tPosition pos, tTree w, tTree e, int acc));
static tTree IdentifyLocal ARGS ((tIdent i, tTree o));
static tTree IdentifyGlobal ARGS ((tIdent yyP8));
static tTree Identify ARGS ((tIdent yyP10, tTree yyP9));
static tTree IdentifyProcLocal ARGS ((tIdent i, tTree o));
static tTree IdentifyProcGlobal ARGS ((tIdent yyP11));
static tTree IdentifyProc ARGS ((tIdent yyP13, tTree yyP12));
static tTree get_namespace ARGS ((tTree yyP14));
static void pass2 ARGS ((tTree t));
static void pass3 ARGS ((tTree yyP15));
static tIdent make_one_word ARGS ((tTree yyP16));
static void make_one_word_2 ARGS ((tTree yyP17));
static tTree Mword ARGS ((tTree yyP19, tTree yyP18));
static tTree Mqualification ARGS ((tTree yyP20));
static tTree Mstmt ARGS ((tTree yyP22, tTree yyP21));
static tTree Mstmt_2 ARGS ((tTree yyP25, tIdent yyP24, tTree yyP23));
static tTree Mblock_content ARGS ((tPosition yyP28, tPosition yyP27, tTree yyP26));
static tTree make_qualification ARGS ((tTree yyP29));
static tTree param_names ARGS ((tTree yyP30));
static tTree param_names_2 ARGS ((tTree yyP32, tTree yyP31));
static tTree param_names_3 ARGS ((tTree yyP34, tTree yyP33));
static tTree param_names_4 ARGS ((tTree yyP36, tTree yyP35));
static void get_end_pos ARGS ((tTree yyP37, tPosition * yyP38));
static void get_begin_pos ARGS ((tTree yyP39, tPosition * yyP40));
static tTree get_env ARGS ((tTree yyP41));
static tTree get_env_2 ARGS ((tTree yyP42));
static tIdent get_ident ARGS ((tTree yyP43));
static void add_qualifications ARGS ((tIdent yyP45, tTree yyP44));
static void add_variable_quals ARGS ((tTree yyP46, int acc));
static void add_variable_quals_2 ARGS ((tTree yyP47));
static void add_quals ARGS ((tTree yyP48, int acc));
static void add_foreach_quals ARGS ((tTree yyP49));
static void add_qual ARGS ((tTree w, int acc));
static void add_qual_2 ARGS ((tTree w, int acc));
static void use_qual ARGS ((tTree yyP50));
static void use_proc_qual ARGS ((tTree yyP51));
static void use_proc ARGS ((tTree yyP52));
static void use ARGS ((tTree yyP53, tPosition p, int acc));
static void use_2 ARGS ((tTree env, tPosition pos, int acc));
static void use_undef ARGS ((tIdent ident, tPosition pos));
static void use_proc_undef ARGS ((tIdent ident, tPosition pos));
static void dcl ARGS ((tTree yyP54));
static void dcl_2 ARGS ((tTree env, tPosition pos));
static tString current_namespace ARGS ((tTree yyP55));
static tString get_class ARGS ((tTree yyP56));
static tString get_class_name ARGS ((tTree yyP57));
static void to_names ARGS ((tTree yyP59, tString yyP58));
static void get_names ARGS ((tTree yyP60));
static void get_separator ARGS ((tTree yyP61));
static void relocate ARGS ((tTree yyP62, tTree new_env));

void Interpret_tcl
#if defined __STDC__ | defined __cplusplus
(register tTree yyP1)
#else
(yyP1)
 register tTree yyP1;
#endif
{
  if (yyP1->Kind == kprogram) {
/* line 44 "tcl.puma" */
  {
/* line 45 "tcl.puma" */
   genv = yyP1->program.stmts->stmts.env;
/* line 46 "tcl.puma" */
   cur_class_ptr = null;
/* line 46 "tcl.puma" */
   cur_proc_ptr = null;
/* line 47 "tcl.puma" */
   get_objects (yyP1->program.stmts);
/* line 48 "tcl.puma" */
  if (need_pass_2) {
      cur_class_ptr = null; cur_proc_ptr = null;
      pass2 (yyP1->program.stmts);
   }

/* line 53 "tcl.puma" */
   if (! ((cross_ref_fp))) goto yyL1;
  {
/* line 54 "tcl.puma" */
   cur_class_ptr = null;
/* line 54 "tcl.puma" */
   cur_proc_ptr = null;
/* line 55 "tcl.puma" */
   pass3 (yyP1->program.stmts);
  }
  }
   return;
yyL1:;

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
  case kproc:
  if (s->proc.qualification->Kind == klocal_ident) {
/* line 62 "tcl.puma" */
 {
  register tTree yyV1;
  tString prev_proc;
  {
/* line 65 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->proc.qualification->local_ident.ident;
    yyV1->object.next = s->proc.env -> env . objects;
   s->proc.env -> env . objects = yyV1;
/* line 66 "tcl.puma" */
   dcl (s->proc.env -> env . objects);
/* line 67 "tcl.puma" */
   get_objects (s->proc.next);
/* line 68 "tcl.puma" */

/* line 68 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 69 "tcl.puma" */
   cur_proc_ptr = GetCStr (s->proc.qualification->local_ident.ident);
/* line 70 "tcl.puma" */
   get_param_objects (s->proc.param_names);
/* line 71 "tcl.puma" */
   get_objects (s->proc.block);
/* line 72 "tcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc.qualification->Kind == kglobal_ident) {
/* line 74 "tcl.puma" */
 {
  register tTree yyV1;
  tString prev_proc;
  {
/* line 77 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->proc.qualification->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 78 "tcl.puma" */
   dcl (genv -> env . objects);
/* line 79 "tcl.puma" */
   get_objects (s->proc.next);
/* line 80 "tcl.puma" */

/* line 80 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 81 "tcl.puma" */
   cur_proc_ptr = GetCStr (s->proc.qualification->global_ident.ident);
/* line 82 "tcl.puma" */
   get_param_objects (s->proc.param_names);
/* line 83 "tcl.puma" */
   get_objects (s->proc.block);
/* line 84 "tcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc.qualification->Kind == klocal_text) {
/* line 86 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 90 "tcl.puma" */
   get_objects (s->proc.next);
/* line 91 "tcl.puma" */

/* line 91 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 92 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (s->proc.qualification));
/* line 93 "tcl.puma" */
   get_param_objects (s->proc.param_names);
/* line 94 "tcl.puma" */
   get_objects (s->proc.block);
/* line 95 "tcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
  if (s->proc.qualification->Kind == kglobal_text) {
/* line 86 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 90 "tcl.puma" */
   get_objects (s->proc.next);
/* line 91 "tcl.puma" */

/* line 91 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 92 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (s->proc.qualification));
/* line 93 "tcl.puma" */
   get_param_objects (s->proc.param_names);
/* line 94 "tcl.puma" */
   get_objects (s->proc.block);
/* line 95 "tcl.puma" */
   cur_proc_ptr = prev_proc;
  }
   return;
 }

  }
/* line 97 "tcl.puma" */
  {
/* line 99 "tcl.puma" */
   need_pass_2 = rtrue;
/* line 100 "tcl.puma" */
   s = s->proc.next;
   goto yyRecursion;
  }

  case knamespace:
  if (s->namespace.qualification->Kind == klocal_ident) {
/* line 102 "tcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 104 "tcl.puma" */

/* line 104 "tcl.puma" */
   obj = IdentifyLocal (s->namespace.qualification->local_ident.ident, s->namespace.env -> env . objects);
/* line 105 "tcl.puma" */
  if (obj != NoTree && obj->object.object->Kind == knamespace) {
      relocate (s->namespace.block, obj->object.object->namespace.block->texts.env);
      use (obj, s->namespace.qualification->local_ident.pos, PAF_REF_READ);
   } else {
      s->namespace.env->env.objects = mobject (s, s->namespace.qualification->local_ident.ident, s->namespace.env->env.objects);
      dcl (s->namespace.env->env.objects);
   }

/* line 113 "tcl.puma" */
   get_objects (s->namespace.next);
/* line 114 "tcl.puma" */

/* line 114 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 115 "tcl.puma" */
   cur_class_ptr = GetCStr (s->namespace.qualification->local_ident.ident);
/* line 116 "tcl.puma" */
   get_objects (s->namespace.block);
/* line 117 "tcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == kglobal_ident) {
/* line 119 "tcl.puma" */
 {
  tTree obj;
  tString prev_class;
  {
/* line 121 "tcl.puma" */

/* line 121 "tcl.puma" */
   obj = IdentifyGlobal (s->namespace.qualification->global_ident.ident);
/* line 122 "tcl.puma" */
  if (obj != NoTree && obj->object.object->Kind == knamespace) {
      relocate (s->namespace.block, obj->object.object->namespace.block->texts.env);
      use (obj, s->namespace.qualification->global_ident.pos, PAF_REF_READ);
   } else {
      genv->env.objects = mobject (s, s->namespace.qualification->global_ident.ident, genv->env.objects);
      dcl (genv->env.objects);
   }

/* line 130 "tcl.puma" */
   get_objects (s->namespace.next);
/* line 131 "tcl.puma" */

/* line 131 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 132 "tcl.puma" */
   cur_class_ptr = GetCStr (s->namespace.qualification->global_ident.ident);
/* line 133 "tcl.puma" */
   get_objects (s->namespace.block);
/* line 134 "tcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == klocal_text) {
/* line 136 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 138 "tcl.puma" */
   get_objects (s->namespace.next);
/* line 139 "tcl.puma" */

/* line 139 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 140 "tcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->namespace.qualification));
/* line 141 "tcl.puma" */
   get_objects (s->namespace.block);
/* line 142 "tcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
  if (s->namespace.qualification->Kind == kglobal_text) {
/* line 136 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 138 "tcl.puma" */
   get_objects (s->namespace.next);
/* line 139 "tcl.puma" */

/* line 139 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 140 "tcl.puma" */
   cur_class_ptr = GetCStr (get_ident (s->namespace.qualification));
/* line 141 "tcl.puma" */
   get_objects (s->namespace.block);
/* line 142 "tcl.puma" */
   cur_class_ptr = prev_class;
  }
   return;
 }

  }
/* line 144 "tcl.puma" */
  {
/* line 145 "tcl.puma" */
   need_pass_2 = rtrue;
/* line 146 "tcl.puma" */
   s = s->namespace.next;
   goto yyRecursion;
  }

  case kstmt:
  if (s->stmt.words->Kind == kone_word) {
  if (Tree_IsType (s->stmt.words->one_word.next, kword_c)) {
/* line 148 "tcl.puma" */
  {
/* line 150 "tcl.puma" */
   add_variables (s->stmt.words->one_word.ident, s->stmt.words->one_word.next);
/* line 151 "tcl.puma" */
   get_objects (s->stmt.words);
/* line 152 "tcl.puma" */
   s = s->stmt.next;
   goto yyRecursion;
  }

  }
  }
/* line 154 "tcl.puma" */
  {
/* line 155 "tcl.puma" */
   get_objects (s->stmt.words);
/* line 156 "tcl.puma" */
   s = s->stmt.next;
   goto yyRecursion;
  }

  case kblock:
/* line 158 "tcl.puma" */
  {
/* line 159 "tcl.puma" */
   get_objects (s->block.stmts);
/* line 160 "tcl.puma" */
   s = s->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 162 "tcl.puma" */
  {
/* line 163 "tcl.puma" */
   get_objects (s->content.qualification);
/* line 164 "tcl.puma" */
   s = s->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 166 "tcl.puma" */
  {
/* line 167 "tcl.puma" */
   get_objects (s->block_content.stmts);
/* line 168 "tcl.puma" */
   s = s->block_content.next;
   goto yyRecursion;
  }

  case ktext:
  case kcharacter:
  case kident:
/* line 170 "tcl.puma" */
  {
/* line 171 "tcl.puma" */
   s = s->text.next;
   goto yyRecursion;
  }

  case kone_word:
/* line 173 "tcl.puma" */
  {
/* line 174 "tcl.puma" */
   s = s->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 176 "tcl.puma" */
  {
/* line 177 "tcl.puma" */
   get_objects (s->qual_word.qualification);
/* line 178 "tcl.puma" */
   s = s->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 180 "tcl.puma" */
  {
/* line 181 "tcl.puma" */
   get_objects (s->qual_words.qualifications);
/* line 182 "tcl.puma" */
   s = s->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 184 "tcl.puma" */
  {
/* line 185 "tcl.puma" */
   get_objects (s->one_qualification.qualification);
/* line 186 "tcl.puma" */
   s = s->one_qualification.next;
   goto yyRecursion;
  }

  case klocal_text:
/* line 188 "tcl.puma" */
  {
/* line 189 "tcl.puma" */
   s = s->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 191 "tcl.puma" */
  {
/* line 192 "tcl.puma" */
   s = s->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 194 "tcl.puma" */
  {
/* line 195 "tcl.puma" */
   s = s->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 197 "tcl.puma" */
  {
/* line 198 "tcl.puma" */
   get_objects (s->complex_qual.qualification);
/* line 199 "tcl.puma" */
   s = s->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 201 "tcl.puma" */
  {
/* line 202 "tcl.puma" */
   get_objects (s->subscription.qualification);
/* line 203 "tcl.puma" */
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
/* line 210 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 211 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = s;
    yyV1->object.ident = s->one_word.ident;
    yyV1->object.next = s->one_word.env -> env . objects;
   s->one_word.env -> env . objects = yyV1;
/* line 212 "tcl.puma" */
   dcl (s->one_word.env -> env . objects);
/* line 213 "tcl.puma" */
   s = s->one_word.next;
   goto yyRecursion;
  }
 }

  }
  if (Tree_IsType (s, kword_c)) {
/* line 215 "tcl.puma" */
  {
/* line 216 "tcl.puma" */
   s = s->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_variables
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP3, register tTree yyP2)
#else
(yyP3, yyP2)
 register tIdent yyP3;
 register tTree yyP2;
#endif
{
  if (equaltIdent (yyP3, (iset))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 223 "tcl.puma" */
  {
/* line 224 "tcl.puma" */
   add_var (yyP2, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iset))) {
/* line 226 "tcl.puma" */
  {
/* line 227 "tcl.puma" */
   add_var (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (iglobal))) {
/* line 229 "tcl.puma" */
  {
/* line 230 "tcl.puma" */
   add_global_vars (yyP2);
  }
   return;

  }
  if (equaltIdent (yyP3, (ivariable))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 232 "tcl.puma" */
  {
/* line 233 "tcl.puma" */
   add_variable_vars (yyP2, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (ivariable))) {
/* line 235 "tcl.puma" */
  {
/* line 236 "tcl.puma" */
   add_variable_vars (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (iappend))) {
/* line 238 "tcl.puma" */
  {
/* line 239 "tcl.puma" */
   add_var (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (ilappend))) {
/* line 241 "tcl.puma" */
  {
/* line 242 "tcl.puma" */
   add_var (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (iarray))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iset))) {
/* line 244 "tcl.puma" */
  {
/* line 246 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP3, (iarray))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 248 "tcl.puma" */
  {
/* line 249 "tcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_READ);
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
/* line 251 "tcl.puma" */
  {
/* line 255 "tcl.puma" */
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
/* line 257 "tcl.puma" */
  {
/* line 260 "tcl.puma" */
   add_vars (yyP2->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP3, (iforeach))) {
/* line 262 "tcl.puma" */
  {
/* line 263 "tcl.puma" */
   add_foreach_vars (yyP2);
  }
   return;

  }
  if (equaltIdent (yyP3, (ivwait))) {
/* line 265 "tcl.puma" */
  {
/* line 266 "tcl.puma" */
   add_var (yyP2, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP3, (iincr))) {
/* line 268 "tcl.puma" */
  {
/* line 269 "tcl.puma" */
   add_var (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (iparray))) {
/* line 271 "tcl.puma" */
  {
/* line 272 "tcl.puma" */
   add_var (yyP2, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP3, (icatch))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 274 "tcl.puma" */
  {
/* line 275 "tcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (igets))) {
  if (Tree_IsType (yyP2->word_c.next, kword_c)) {
/* line 277 "tcl.puma" */
  {
/* line 278 "tcl.puma" */
   add_var (yyP2->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP3, (iinfo))) {
  if (yyP2->Kind == kone_word) {
  if (Tree_IsType (yyP2->one_word.next, kword_c)) {
  if (equaltIdent (yyP2->one_word.ident, (iexists))) {
/* line 280 "tcl.puma" */
  {
/* line 282 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ);
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
/* line 284 "tcl.puma" */
  {
/* line 286 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ);
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
/* line 288 "tcl.puma" */
  {
/* line 290 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ);
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
/* line 292 "tcl.puma" */
  {
/* line 296 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ);
/* line 297 "tcl.puma" */
   add_var (yyP2->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
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
/* line 299 "tcl.puma" */
  {
/* line 301 "tcl.puma" */
   add_var (yyP2->one_word.next, PAF_REF_READ);
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
/* line 303 "tcl.puma" */
  {
/* line 306 "tcl.puma" */
   add_var (yyP2->one_word.next->word_c.next, PAF_REF_WRITE);
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
/* line 308 "tcl.puma" */
  {
/* line 311 "tcl.puma" */
   add_var (yyP2->one_word.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP3, (iunset))) {
/* line 313 "tcl.puma" */
  {
/* line 314 "tcl.puma" */
   add_vars (yyP2, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP3, (isource))) {
  if (yyP2->word_c.next->Kind == knoword) {
/* line 321 "tcl.puma" */
  {
/* line 322 "tcl.puma" */
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
/* line 329 "tcl.puma" */
  {
/* line 330 "tcl.puma" */
   put_symbol (PAF_INCLUDE_DEF, NULL, GetCStr (yyP4->one_word.ident), current_file, (int)  yyP4->one_word.pos . Line, (int)  yyP4->one_word.pos . Column - 1, (int)  yyP4->one_word.pos . Line, (int)  (yyP4->one_word.pos . Column + StLength (GetStringRef (yyP4->one_word.ident)) - 1), 0, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
  if (yyP4->Kind == kqual_word) {
/* line 335 "tcl.puma" */
   return;

  }
  if (yyP4->Kind == kqual_words) {
/* line 337 "tcl.puma" */
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
/* line 353 "tcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 354 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 355 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->one_word.ident;
    yyV2->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV2;
/* line 356 "tcl.puma" */
   w->one_word.env = genv;
/* line 357 "tcl.puma" */
   dcl (genv -> env . objects);
/* line 358 "tcl.puma" */
   w = w->one_word.next;
   goto yyRecursion;
  }
 }

  }
  if (w->Kind == kqual_word) {
  if (w->qual_word.qualification->Kind == kglobal_ident) {
/* line 370 "tcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 372 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w->qual_word.qualification;
    yyV1->object.ident = w->qual_word.qualification->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 373 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w->qual_word.qualification;
    yyV2->object.ident = w->qual_word.qualification->global_ident.ident;
    yyV2->object.next = w->qual_word.qualification->global_ident.env -> env . objects;
   w->qual_word.qualification->global_ident.env -> env . objects = yyV2;
/* line 374 "tcl.puma" */
   w->qual_word.qualification->global_ident.env = genv;
/* line 375 "tcl.puma" */
   dcl (w->qual_word.qualification->global_ident.env -> env . objects);
/* line 376 "tcl.puma" */
   w = w->qual_word.next;
   goto yyRecursion;
  }
 }

  }
/* line 378 "tcl.puma" */
  {
/* line 380 "tcl.puma" */
   w = w->qual_word.next;
   goto yyRecursion;
  }

  }
  if (w->Kind == kqual_words) {
/* line 378 "tcl.puma" */
  {
/* line 380 "tcl.puma" */
   w = w->qual_words.next;
   goto yyRecursion;
  }

  }
;
}

static void add_variable_vars
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc)
#else
(w, acc)
 register tTree w;
 register int acc;
#endif
{
/* line 387 "tcl.puma" */
 tTree obj, e2; 
  if (w->Kind == kone_word) {
  if (w->one_word.env->Kind == kenv) {
/* line 389 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 390 "tcl.puma" */
   if (! (w->one_word.env->env.object -> Kind != kproc)) goto yyL1;
  {
/* line 391 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV1;
/* line 392 "tcl.puma" */
   dcl (w->one_word.env -> env . objects);
/* line 393 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (w->one_word.env->env.objects, w->one_word.pos, acc); 
/* line 394 "tcl.puma" */
   add_variable_vars_2 (w->one_word.next);
  }
  }
   return;
 }
yyL1:;

  }
/* line 396 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 397 "tcl.puma" */
   e2 = get_namespace (w->one_word.env);
/* line 398 "tcl.puma" */
   obj = IdentifyLocal (w->one_word.ident, e2 -> env . objects);
/* line 399 "tcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  {
/* line 400 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = obj -> object . object;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV1;
/* line 401 "tcl.puma" */
   use (w->one_word.env -> env . objects, w->one_word.pos, acc);
/* line 402 "tcl.puma" */
   add_variable_vars_2 (w->one_word.next);
  }
  }
   return;
 }
yyL2:;

/* line 404 "tcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 405 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->one_word.ident;
    yyV1->object.next = e2 -> env . objects;
   e2 -> env . objects = yyV1;
/* line 406 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->one_word.ident;
    yyV2->object.next = w->one_word.env -> env . objects;
   w->one_word.env -> env . objects = yyV2;
/* line 407 "tcl.puma" */
   w->one_word.env = e2;
/* line 408 "tcl.puma" */
   dcl (e2 -> env . objects);
/* line 409 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (e2->env.objects, w->one_word.pos, acc); 
/* line 410 "tcl.puma" */
   add_variable_vars_2 (w->one_word.next);
  }
   return;
 }

  }
  if (w->Kind == kqual_word) {
/* line 412 "tcl.puma" */
  {
/* line 413 "tcl.puma" */
   add_variable_vars (w->qual_word.qualification, acc);
/* line 414 "tcl.puma" */
   add_variable_vars_2 (w->qual_word.next);
  }
   return;

  }
  if (w->Kind == kqual_words) {
/* line 416 "tcl.puma" */
  {
/* line 417 "tcl.puma" */
   add_variable_vars_2 (w->qual_words.next);
  }
   return;

  }
  if (w->Kind == kglobal_ident) {
/* line 419 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 420 "tcl.puma" */
   obj = IdentifyGlobal (w->global_ident.ident);
/* line 421 "tcl.puma" */
   if (! (obj != NULL)) goto yyL6;
  {
/* line 422 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = obj -> object . object;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = w->global_ident.env -> env . objects;
   w->global_ident.env -> env . objects = yyV1;
/* line 423 "tcl.puma" */
   use (w->global_ident.env -> env . objects, w->global_ident.pos, acc);
  }
  }
   return;
 }
yyL6:;

/* line 425 "tcl.puma" */
 {
  register tTree yyV1;
  register tTree yyV2;
  {
/* line 426 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 427 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV2,kobject,Tree_InitHead)
    yyV2->object.object = w;
    yyV2->object.ident = w->global_ident.ident;
    yyV2->object.next = w->global_ident.env -> env . objects;
   w->global_ident.env -> env . objects = yyV2;
/* line 428 "tcl.puma" */
   w->global_ident.env = genv;
/* line 429 "tcl.puma" */
   dcl (genv -> env . objects);
/* line 430 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_ident.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == kqualification) {
/* line 432 "tcl.puma" */
  {
/* line 434 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->Kind == kcomplex_qual) {
/* line 432 "tcl.puma" */
  {
/* line 434 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
;
}

static void add_variable_vars_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP5)
#else
(yyP5)
 register tTree yyP5;
#endif
{
  if (Tree_IsType (yyP5, kword_c)) {
  if (Tree_IsType (yyP5->word_c.next, kword_c)) {
/* line 441 "tcl.puma" */
  {
/* line 442 "tcl.puma" */
   add_variable_vars (yyP5->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
/* line 444 "tcl.puma" */
  {
/* line 445 "tcl.puma" */
   add_variable_vars (yyP5->word_c.next, PAF_REF_READ);
  }
   return;

  }
;
}

static void add_vars
#if defined __STDC__ | defined __cplusplus
(register tTree yyP6, register int acc)
#else
(yyP6, acc)
 register tTree yyP6;
 register int acc;
#endif
{
 yyRecursion:
  if (Tree_IsType (yyP6, kword_c)) {
/* line 452 "tcl.puma" */
  {
/* line 453 "tcl.puma" */
   add_var (yyP6, acc);
/* line 454 "tcl.puma" */
   yyP6 = yyP6->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_foreach_vars
#if defined __STDC__ | defined __cplusplus
(register tTree yyP7)
#else
(yyP7)
 register tTree yyP7;
#endif
{
 yyRecursion:
  if (yyP7->Kind == kqual_word) {
  if (Tree_IsType (yyP7->qual_word.next, kword_c)) {
  if (yyP7->qual_word.qualification->Kind == klocal_text) {
  if (yyP7->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 461 "tcl.puma" */
  {
/* line 463 "tcl.puma" */
   add_foreach_vars (yyP7->qual_word.qualification->local_text.texts->block.stmts);
/* line 464 "tcl.puma" */
   yyP7 = yyP7->qual_word.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  }
  }
  if (Tree_IsType (yyP7, kword_c)) {
  if (Tree_IsType (yyP7->word_c.next, kword_c)) {
/* line 466 "tcl.puma" */
  {
/* line 467 "tcl.puma" */
   add_var (yyP7, PAF_REF_WRITE);
/* line 468 "tcl.puma" */
   yyP7 = yyP7->word_c.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  if (yyP7->Kind == kstmt) {
/* line 470 "tcl.puma" */
  {
/* line 471 "tcl.puma" */
   add_vars (yyP7->stmt.words, PAF_REF_WRITE);
/* line 472 "tcl.puma" */
   yyP7 = yyP7->stmt.next;
   goto yyRecursion;
  }

  }
;
}

static void add_var
#if defined __STDC__ | defined __cplusplus
(register tTree w, register int acc)
#else
(w, acc)
 register tTree w;
 register int acc;
#endif
{
  if (w->Kind == kone_word) {
/* line 479 "tcl.puma" */
  {
/* line 480 "tcl.puma" */
   add_var_2 (w->one_word.ident, w->one_word.pos, w, w->one_word.env, acc);
  }
   return;

  }
  if (w->Kind == kqual_word) {
  if (w->qual_word.qualification->Kind == ksubscription) {
  if (w->qual_word.qualification->subscription.qualification->Kind == klocal_ident) {
/* line 482 "tcl.puma" */
  {
/* line 484 "tcl.puma" */
   add_var_2 (w->qual_word.qualification->subscription.qualification->local_ident.ident, w->qual_word.qualification->subscription.qualification->local_ident.pos, w->qual_word.qualification->subscription.qualification, w->qual_word.env, acc);
  }
   return;

  }
/* line 486 "tcl.puma" */
  {
/* line 490 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kqualification) {
/* line 486 "tcl.puma" */
  {
/* line 490 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kcomplex_qual) {
/* line 486 "tcl.puma" */
  {
/* line 490 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  if (w->qual_word.qualification->Kind == kglobal_ident) {
/* line 486 "tcl.puma" */
  {
/* line 490 "tcl.puma" */
   need_pass_2 = rtrue;
  }
   return;

  }
  }
;
}

static void add_var_2
#if defined __STDC__ | defined __cplusplus
(register tIdent i, tPosition pos, register tTree w, register tTree e, register int acc)
#else
(i, pos, w, e, acc)
 register tIdent i;
 tPosition pos;
 register tTree w;
 register tTree e;
 register int acc;
#endif
{
  if (e->Kind == kenv) {
  if (e->env.object->Kind == kproc) {
/* line 498 "tcl.puma" */
 {
  tTree obj;
  {
/* line 500 "tcl.puma" */

/* line 500 "tcl.puma" */
   obj = IdentifyLocal (i, e -> env . objects);
/* line 501 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 502 "tcl.puma" */
   use (obj, pos, acc);
  }
  }
   return;
 }
yyL1:;

  }
  if (e->env.object->Kind == kprogram) {
/* line 498 "tcl.puma" */
 {
  tTree obj;
  {
/* line 500 "tcl.puma" */

/* line 500 "tcl.puma" */
   obj = IdentifyLocal (i, e -> env . objects);
/* line 501 "tcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  {
/* line 502 "tcl.puma" */
   use (obj, pos, acc);
  }
  }
   return;
 }
yyL2:;

  }
  if (e->env.object->Kind == knamespace) {
/* line 504 "tcl.puma" */
 {
  tTree obj;
  {
/* line 505 "tcl.puma" */

/* line 505 "tcl.puma" */
   obj = Identify (i, e);
/* line 506 "tcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 507 "tcl.puma" */
   use (obj, pos, acc);
  }
  }
   return;
 }
yyL3:;

  }
  }
/* line 509 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 510 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = i;
    yyV1->object.next = e -> env . objects;
   e -> env . objects = yyV1;
/* line 511 "tcl.puma" */
   dcl (e -> env . objects);
/* line 512 "tcl.puma" */
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
/* line 519 "tcl.puma" */
  {
/* line 520 "tcl.puma" */
   if (! (o->object.ident == i && o->object.object -> Kind != kproc)) goto yyL1;
  }
   return o;
yyL1:;

/* line 523 "tcl.puma" */
   o = o->object.next;
   goto yyRecursion;

  }
/* line 526 "tcl.puma" */
   return NULL;

}

static tTree IdentifyGlobal
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP8)
#else
(yyP8)
 register tIdent yyP8;
#endif
{
/* line 534 "tcl.puma" */
  {
/* line 534 "tcl.puma" */
   if (! (genv != NULL)) goto yyL1;
  }
   return IdentifyLocal (yyP8, genv -> env . objects);
yyL1:;

/* line 535 "tcl.puma" */
   return NULL;

}

static tTree Identify
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP10, register tTree yyP9)
#else
(yyP10, yyP9)
 register tIdent yyP10;
 register tTree yyP9;
#endif
{
  if (yyP9->Kind == kenv) {
/* line 541 "tcl.puma" */
 {
  tTree obj;
  {
/* line 542 "tcl.puma" */

/* line 542 "tcl.puma" */
   obj = IdentifyLocal (yyP10, yyP9->env.objects);
/* line 543 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

  }
/* line 546 "tcl.puma" */
  {
/* line 547 "tcl.puma" */
   if (! (yyP9 != genv)) goto yyL2;
  }
   return IdentifyGlobal (yyP10);
yyL2:;

/* line 550 "tcl.puma" */
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
/* line 558 "tcl.puma" */
  {
/* line 559 "tcl.puma" */
   if (! (o->object.ident == i && o->object.object -> Kind == kproc)) goto yyL1;
  }
   return o;
yyL1:;

/* line 562 "tcl.puma" */
   o = o->object.next;
   goto yyRecursion;

  }
/* line 565 "tcl.puma" */
   return NULL;

}

static tTree IdentifyProcGlobal
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP11)
#else
(yyP11)
 register tIdent yyP11;
#endif
{
/* line 573 "tcl.puma" */
  {
/* line 573 "tcl.puma" */
   if (! (genv != NULL)) goto yyL1;
  }
   return IdentifyProcLocal (yyP11, genv -> env . objects);
yyL1:;

/* line 574 "tcl.puma" */
   return NULL;

}

static tTree IdentifyProc
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP13, register tTree yyP12)
#else
(yyP13, yyP12)
 register tIdent yyP13;
 register tTree yyP12;
#endif
{
  if (yyP12->Kind == kenv) {
/* line 580 "tcl.puma" */
 {
  tTree obj;
  {
/* line 581 "tcl.puma" */

/* line 581 "tcl.puma" */
   obj = IdentifyProcLocal (yyP13, yyP12->env.objects);
/* line 582 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  }
  {
   return obj;
  }
 }
yyL1:;

  }
/* line 585 "tcl.puma" */
  {
/* line 586 "tcl.puma" */
   if (! (yyP12 != genv)) goto yyL2;
  }
   return IdentifyProcGlobal (yyP13);
yyL2:;

/* line 589 "tcl.puma" */
   return NULL;

}

static tTree get_namespace
#if defined __STDC__ | defined __cplusplus
(register tTree yyP14)
#else
(yyP14)
 register tTree yyP14;
#endif
{
 yyRecursion:
  if (yyP14->Kind == kenv) {
  if (yyP14->env.object->Kind == knamespace) {
/* line 597 "tcl.puma" */
   return yyP14;

  }
  if (yyP14->env.object->Kind == kprogram) {
/* line 597 "tcl.puma" */
   return yyP14;

  }
/* line 600 "tcl.puma" */
   yyP14 = yyP14->env.env;
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
/* line 606 "tcl.puma" */
  {
/* line 608 "tcl.puma" */
   add_qualifications (t->stmt.words->one_word.ident, t->stmt.words->one_word.next);
/* line 609 "tcl.puma" */
   pass2 (t->stmt.words);
/* line 610 "tcl.puma" */
   t = t->stmt.next;
   goto yyRecursion;
  }

  }
  }
/* line 612 "tcl.puma" */
  {
/* line 613 "tcl.puma" */
   pass2 (t->stmt.words);
/* line 614 "tcl.puma" */
   t = t->stmt.next;
   goto yyRecursion;
  }

  }
  if (t->Kind == kproc) {
  if (t->proc.qualification->Kind == klocal_ident) {
/* line 616 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 618 "tcl.puma" */

/* line 618 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 619 "tcl.puma" */
   cur_proc_ptr = GetCStr (t->proc.qualification->local_ident.ident);
/* line 620 "tcl.puma" */
   pass2 (t->proc.block);
/* line 621 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 622 "tcl.puma" */
   t = t->proc.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc.qualification->Kind == kglobal_ident) {
/* line 616 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 618 "tcl.puma" */

/* line 618 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 619 "tcl.puma" */
   cur_proc_ptr = GetCStr (t->proc.qualification->global_ident.ident);
/* line 620 "tcl.puma" */
   pass2 (t->proc.block);
/* line 621 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 622 "tcl.puma" */
   t = t->proc.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc.qualification->Kind == klocal_text) {
/* line 624 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 626 "tcl.puma" */

/* line 626 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 627 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (t->proc.qualification));
/* line 628 "tcl.puma" */
   pass2 (t->proc.block);
/* line 629 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 630 "tcl.puma" */
   t = t->proc.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc.qualification->Kind == kglobal_text) {
/* line 624 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 626 "tcl.puma" */

/* line 626 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 627 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (t->proc.qualification));
/* line 628 "tcl.puma" */
   pass2 (t->proc.block);
/* line 629 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 630 "tcl.puma" */
   t = t->proc.next;
   goto yyRecursion;
  }
 }

  }
  if (t->proc.block->texts.env->Kind == kenv) {
/* line 632 "tcl.puma" */
 {
  tTree e;
  tIdent ident;
  tString prev_proc;
  tString prev_class;
  {
/* line 635 "tcl.puma" */

/* line 635 "tcl.puma" */
   e = get_env (t->proc.qualification);
/* line 636 "tcl.puma" */

/* line 636 "tcl.puma" */
   ident = get_ident (t->proc.qualification);
/* line 637 "tcl.puma" */
  if (e) {
      t->proc.env = e;
      t->proc.block->texts.env->env.env = e;
      e->env.objects = mobject (t, ident, e->env.objects);
      dcl (e->env.objects);
   } else {
      t->proc.env->env.objects = mobject (t, ident, t->proc.env->env.objects);
      dcl (t->proc.env->env.objects);
   }

/* line 647 "tcl.puma" */

/* line 647 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 648 "tcl.puma" */
   cur_proc_ptr = GetCStr (ident);
/* line 649 "tcl.puma" */

/* line 649 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 650 "tcl.puma" */
   cur_class_ptr = get_class (t->proc.qualification);
/* line 651 "tcl.puma" */
   get_param_objects (t->proc.param_names);
/* line 652 "tcl.puma" */
   get_objects (t->proc.block);
/* line 653 "tcl.puma" */
   pass2 (t->proc.block);
/* line 654 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 655 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 656 "tcl.puma" */
   t = t->proc.next;
   goto yyRecursion;
  }
 }

  }
  }
  if (t->Kind == knamespace) {
  if (t->namespace.qualification->Kind == klocal_ident) {
/* line 658 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 660 "tcl.puma" */

/* line 660 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 661 "tcl.puma" */
   cur_class_ptr = GetCStr (t->namespace.qualification->local_ident.ident);
/* line 662 "tcl.puma" */
   pass2 (t->namespace.block);
/* line 663 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 664 "tcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == kglobal_ident) {
/* line 658 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 660 "tcl.puma" */

/* line 660 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 661 "tcl.puma" */
   cur_class_ptr = GetCStr (t->namespace.qualification->global_ident.ident);
/* line 662 "tcl.puma" */
   pass2 (t->namespace.block);
/* line 663 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 664 "tcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == klocal_text) {
/* line 666 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 668 "tcl.puma" */

/* line 668 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 669 "tcl.puma" */
   cur_class_ptr = GetCStr (get_ident (t->namespace.qualification));
/* line 670 "tcl.puma" */
   pass2 (t->namespace.block);
/* line 671 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 672 "tcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.qualification->Kind == kglobal_text) {
/* line 666 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 668 "tcl.puma" */

/* line 668 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 669 "tcl.puma" */
   cur_class_ptr = GetCStr (get_ident (t->namespace.qualification));
/* line 670 "tcl.puma" */
   pass2 (t->namespace.block);
/* line 671 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 672 "tcl.puma" */
   t = t->namespace.next;
   goto yyRecursion;
  }
 }

  }
  if (t->namespace.block->texts.env->Kind == kenv) {
/* line 674 "tcl.puma" */
 {
  tTree e;
  tIdent ident;
  tTree obj;
  tString prev_class;
  {
/* line 676 "tcl.puma" */

/* line 676 "tcl.puma" */
   e = get_env (t->namespace.qualification);
/* line 677 "tcl.puma" */

/* line 677 "tcl.puma" */
   ident = get_ident (t->namespace.qualification);
/* line 678 "tcl.puma" */

/* line 679 "tcl.puma" */
  if (e) {
      t->namespace.env = e;
      t->namespace.block->texts.env->env.env = e;
      obj = IdentifyLocal (ident, e->env.objects);
      if (obj != NoTree && obj->object.object->Kind == knamespace) {
	 relocate (t->namespace.block, obj->object.object->namespace.block->texts.env);
	 use (obj, t->namespace.qualification->qualification_c.pos, PAF_REF_READ);
      } else {
	 e->env.objects = mobject (t, ident, e->env.objects);
	 dcl (e->env.objects);
      }
   } else {
      t->namespace.env->env.objects = mobject (t, ident, t->namespace.env->env.objects);
      dcl (t->namespace.env->env.objects);
   }

/* line 695 "tcl.puma" */

/* line 695 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 696 "tcl.puma" */
   cur_class_ptr = GetCStr (ident);
/* line 697 "tcl.puma" */
   get_objects (t->namespace.block);
/* line 698 "tcl.puma" */
   pass2 (t->namespace.block);
/* line 699 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 700 "tcl.puma" */
   t = t->namespace.next;
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
  case kconfigbody:
  case kconstructor:
  case kdestructor:
  case knamespace_c:
  case knamespace:
  case kclass:
  case kitcl_class:
/* line 702 "tcl.puma" */
  {
/* line 703 "tcl.puma" */
   t = t->stmt_c.next;
   goto yyRecursion;
  }

  case kone_word:
/* line 705 "tcl.puma" */
  {
/* line 706 "tcl.puma" */
   t = t->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 708 "tcl.puma" */
  {
/* line 709 "tcl.puma" */
   pass2 (t->qual_word.qualification);
/* line 710 "tcl.puma" */
   t = t->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 712 "tcl.puma" */
  {
/* line 713 "tcl.puma" */
   pass2 (t->qual_words.qualifications);
/* line 714 "tcl.puma" */
   t = t->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 716 "tcl.puma" */
  {
/* line 717 "tcl.puma" */
   pass2 (t->one_qualification.qualification);
/* line 718 "tcl.puma" */
   t = t->one_qualification.next;
   goto yyRecursion;
  }

  case kblock:
/* line 720 "tcl.puma" */
  {
/* line 721 "tcl.puma" */
   pass2 (t->block.stmts);
/* line 722 "tcl.puma" */
   t = t->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 724 "tcl.puma" */
  {
/* line 725 "tcl.puma" */
   pass2 (t->content.qualification);
/* line 726 "tcl.puma" */
   t = t->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 728 "tcl.puma" */
  {
/* line 729 "tcl.puma" */
   pass2 (t->block_content.stmts);
/* line 730 "tcl.puma" */
   t = t->block_content.next;
   goto yyRecursion;
  }

  case ktext:
  case kcharacter:
  case kident:
/* line 732 "tcl.puma" */
  {
/* line 733 "tcl.puma" */
   t = t->text.next;
   goto yyRecursion;
  }

  case klocal_text:
/* line 735 "tcl.puma" */
  {
/* line 736 "tcl.puma" */
   t = t->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 738 "tcl.puma" */
  {
/* line 739 "tcl.puma" */
   t = t->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 741 "tcl.puma" */
  {
/* line 742 "tcl.puma" */
   t = t->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 744 "tcl.puma" */
  {
/* line 745 "tcl.puma" */
   pass2 (t->complex_qual.qualification);
/* line 746 "tcl.puma" */
   t = t->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 748 "tcl.puma" */
  {
/* line 749 "tcl.puma" */
   pass2 (t->subscription.qualification);
/* line 750 "tcl.puma" */
   t = t->subscription.index;
   goto yyRecursion;
  }

  }

;
}

static void pass3
#if defined __STDC__ | defined __cplusplus
(register tTree yyP15)
#else
(yyP15)
 register tTree yyP15;
#endif
{
 yyRecursion:

  switch (yyP15->Kind) {
  case kstmt:
  if (yyP15->stmt.words->Kind == kone_word) {
/* line 757 "tcl.puma" */
  {
/* line 759 "tcl.puma" */
   if (! (GetCStr (yyP15->stmt.words->one_word.ident) [0] == '#')) goto yyL1;
  {
/* line 760 "tcl.puma" */
   yyP15 = yyP15->stmt.next;
   goto yyRecursion;
  }
  }
yyL1:;

  if (Tree_IsType (yyP15->stmt.words->one_word.next, kword_c)) {
  if (Tree_IsType (yyP15->stmt.words->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP15->stmt.words->one_word.ident, (irename))) {
/* line 762 "tcl.puma" */
  {
/* line 766 "tcl.puma" */
   use_proc_undef (irename, yyP15->stmt.words->one_word.pos);
/* line 767 "tcl.puma" */
   use_proc (yyP15->stmt.words->one_word.next);
/* line 768 "tcl.puma" */
   pass3 (yyP15->stmt.words->one_word.next);
/* line 769 "tcl.puma" */
   yyP15 = yyP15->stmt.next;
   goto yyRecursion;
  }

  }
  }
  }
  }
/* line 771 "tcl.puma" */
  {
/* line 772 "tcl.puma" */
   use_proc (yyP15->stmt.words);
/* line 773 "tcl.puma" */
   pass3 (yyP15->stmt.words);
/* line 774 "tcl.puma" */
   yyP15 = yyP15->stmt.next;
   goto yyRecursion;
  }

  case kproc:
  if (yyP15->proc.qualification->Kind == kqualification) {
/* line 776 "tcl.puma" */
 {
  tString prev_proc;
  tString prev_class;
  {
/* line 777 "tcl.puma" */

/* line 777 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 778 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (yyP15->proc.qualification));
/* line 779 "tcl.puma" */

/* line 779 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 780 "tcl.puma" */
   cur_class_ptr = get_class (yyP15->proc.qualification);
/* line 781 "tcl.puma" */
   pass3 (yyP15->proc.qualification);
/* line 783 "tcl.puma" */
   pass3 (yyP15->proc.block);
/* line 784 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 785 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 786 "tcl.puma" */
   yyP15 = yyP15->proc.next;
   goto yyRecursion;
  }
 }

  }
/* line 788 "tcl.puma" */
 {
  tString prev_proc;
  {
/* line 789 "tcl.puma" */

/* line 789 "tcl.puma" */
   prev_proc = cur_proc_ptr;
/* line 790 "tcl.puma" */
   cur_proc_ptr = GetCStr (get_ident (yyP15->proc.qualification));
/* line 791 "tcl.puma" */
   pass3 (yyP15->proc.qualification);
/* line 793 "tcl.puma" */
   pass3 (yyP15->proc.block);
/* line 794 "tcl.puma" */
   cur_proc_ptr = prev_proc;
/* line 795 "tcl.puma" */
   yyP15 = yyP15->proc.next;
   goto yyRecursion;
  }
 }

  case knamespace:
/* line 797 "tcl.puma" */
 {
  tString prev_class;
  {
/* line 798 "tcl.puma" */

/* line 798 "tcl.puma" */
   prev_class = cur_class_ptr;
/* line 799 "tcl.puma" */
   cur_class_ptr = GetCStr (get_ident (yyP15->namespace.qualification));
/* line 800 "tcl.puma" */
   pass3 (yyP15->namespace.qualification);
/* line 801 "tcl.puma" */
   pass3 (yyP15->namespace.block);
/* line 802 "tcl.puma" */
   cur_class_ptr = prev_class;
/* line 803 "tcl.puma" */
   yyP15 = yyP15->namespace.next;
   goto yyRecursion;
  }
 }

  case kone_word:
/* line 805 "tcl.puma" */
  {
/* line 806 "tcl.puma" */
   yyP15 = yyP15->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 808 "tcl.puma" */
  {
/* line 809 "tcl.puma" */
   pass3 (yyP15->qual_word.qualification);
/* line 810 "tcl.puma" */
   yyP15 = yyP15->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 812 "tcl.puma" */
  {
/* line 813 "tcl.puma" */
   pass3 (yyP15->qual_words.qualifications);
/* line 814 "tcl.puma" */
   yyP15 = yyP15->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 816 "tcl.puma" */
  {
/* line 817 "tcl.puma" */
   pass3 (yyP15->one_qualification.qualification);
/* line 818 "tcl.puma" */
   yyP15 = yyP15->one_qualification.next;
   goto yyRecursion;
  }

  case kcharacter:
/* line 820 "tcl.puma" */
  {
/* line 821 "tcl.puma" */
   yyP15 = yyP15->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 823 "tcl.puma" */
  {
/* line 824 "tcl.puma" */
   yyP15 = yyP15->ident.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 826 "tcl.puma" */
  {
/* line 827 "tcl.puma" */
   use_qual (yyP15->content.qualification);
/* line 828 "tcl.puma" */
   pass3 (yyP15->content.qualification);
/* line 829 "tcl.puma" */
   yyP15 = yyP15->content.next;
   goto yyRecursion;
  }

  case kblock:
/* line 831 "tcl.puma" */
 {
  rbool prev_in_string;
  {
/* line 832 "tcl.puma" */

/* line 832 "tcl.puma" */
   prev_in_string = in_string;
/* line 833 "tcl.puma" */
   in_string = yyP15->block.type == '"';
/* line 834 "tcl.puma" */
   pass3 (yyP15->block.stmts);
/* line 835 "tcl.puma" */
   in_string = prev_in_string;
/* line 836 "tcl.puma" */
   yyP15 = yyP15->block.next;
   goto yyRecursion;
  }
 }

  case kblock_content:
/* line 838 "tcl.puma" */
  {
/* line 839 "tcl.puma" */
   pass3 (yyP15->block_content.stmts);
/* line 840 "tcl.puma" */
   yyP15 = yyP15->block_content.next;
   goto yyRecursion;
  }

  case kqualification:
/* line 842 "tcl.puma" */
  {
/* line 843 "tcl.puma" */
   yyP15 = yyP15->qualification.qualification;
   goto yyRecursion;
  }

  case klocal_text:
/* line 845 "tcl.puma" */
  {
/* line 846 "tcl.puma" */
   yyP15 = yyP15->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 848 "tcl.puma" */
  {
/* line 849 "tcl.puma" */
   yyP15 = yyP15->global_text.texts;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 851 "tcl.puma" */
  {
/* line 852 "tcl.puma" */
   pass3 (yyP15->complex_qual.qualification);
/* line 853 "tcl.puma" */
   yyP15 = yyP15->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 855 "tcl.puma" */
  {
/* line 856 "tcl.puma" */
   pass3 (yyP15->subscription.qualification);
/* line 857 "tcl.puma" */
   yyP15 = yyP15->subscription.index;
   goto yyRecursion;
  }

  }

;
}

static tIdent make_one_word
#if defined __STDC__ | defined __cplusplus
(register tTree yyP16)
#else
(yyP16)
 register tTree yyP16;
#endif
{
  if (yyP16->Kind == kident) {
  if (yyP16->ident.next->Kind == knotext) {
/* line 864 "tcl.puma" */
   return yyP16->ident.ident;

  }
  }
/* line 867 "tcl.puma" */
  {
/* line 868 "tcl.puma" */
   length = 0;
/* line 869 "tcl.puma" */
   make_one_word_2 (yyP16);
  }
   return MakeIdent (buffer, length);

}

static void make_one_word_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP17)
#else
(yyP17)
 register tTree yyP17;
#endif
{
 yyRecursion:

  switch (yyP17->Kind) {
  case kcharacter:
/* line 877 "tcl.puma" */
  {
/* line 878 "tcl.puma" */
   buffer [length ++] = yyP17->character.text;
/* line 879 "tcl.puma" */
   yyP17 = yyP17->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 881 "tcl.puma" */
  {
/* line 882 "tcl.puma" */
   GetString (yyP17->ident.ident, & buffer [length]);
/* line 883 "tcl.puma" */
   length = length + StLength (GetStringRef (yyP17->ident.ident));
/* line 884 "tcl.puma" */
   yyP17 = yyP17->ident.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 886 "tcl.puma" */
  {
/* line 887 "tcl.puma" */
   buffer [length ++] = '$';
/* line 888 "tcl.puma" */
   make_one_word_2 (yyP17->content.qualification);
/* line 889 "tcl.puma" */
   yyP17 = yyP17->content.next;
   goto yyRecursion;
  }

  case klocal_ident:
/* line 891 "tcl.puma" */
  {
/* line 892 "tcl.puma" */
   GetString (yyP17->local_ident.ident, & buffer [length]);
/* line 893 "tcl.puma" */
   length = length + StLength (GetStringRef (yyP17->local_ident.ident));
  }
   return;

  case kglobal_ident:
/* line 895 "tcl.puma" */
  {
/* line 896 "tcl.puma" */
   strcpy (& buffer [length], "::");
/* line 896 "tcl.puma" */
   length = length + 2;
/* line 897 "tcl.puma" */
   GetString (yyP17->global_ident.ident, & buffer [length]);
/* line 898 "tcl.puma" */
   length = length + StLength (GetStringRef (yyP17->global_ident.ident));
  }
   return;

  case klocal_text:
/* line 900 "tcl.puma" */
  {
/* line 901 "tcl.puma" */
   yyP17 = yyP17->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 903 "tcl.puma" */
  {
/* line 904 "tcl.puma" */
   strcpy (& buffer [length], "::");
/* line 904 "tcl.puma" */
   length = length + 2;
/* line 905 "tcl.puma" */
   yyP17 = yyP17->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 907 "tcl.puma" */
  {
/* line 908 "tcl.puma" */
   make_one_word_2 (yyP17->qualification.qualification);
/* line 909 "tcl.puma" */
   strcpy (& buffer [length], "::");
/* line 909 "tcl.puma" */
   length = length + 2;
/* line 910 "tcl.puma" */
   GetString (yyP17->qualification.ident, & buffer [length]);
/* line 911 "tcl.puma" */
   length = length + StLength (GetStringRef (yyP17->qualification.ident));
  }
   return;

  case kcomplex_qual:
/* line 913 "tcl.puma" */
  {
/* line 914 "tcl.puma" */
   make_one_word_2 (yyP17->complex_qual.qualification);
/* line 915 "tcl.puma" */
   strcpy (& buffer [length], "::");
/* line 915 "tcl.puma" */
   length = length + 2;
/* line 916 "tcl.puma" */
   yyP17 = yyP17->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 918 "tcl.puma" */
  {
/* line 919 "tcl.puma" */
   make_one_word_2 (yyP17->subscription.qualification);
/* line 920 "tcl.puma" */
   strcpy (& buffer [length], "(");
/* line 920 "tcl.puma" */
   length = length + 1;
/* line 921 "tcl.puma" */
   make_one_word_2 (yyP17->subscription.index);
/* line 922 "tcl.puma" */
   strcpy (& buffer [length], ")");
/* line 922 "tcl.puma" */
   length = length + 1;
  }
   return;

  case kblock:
  if (yyP17->block.next->Kind == knotext) {
  if (yyP17->block.stmts->Kind == knostmt) {
/* line 924 "tcl.puma" */
   return;

  }
  }
  break;
  case knotext:
/* line 926 "tcl.puma" */
   return;

  }

/* line 928 "tcl.puma" */
  {
/* line 929 "tcl.puma" */
   buffer [length ++] = '?';
  }
   return;

;
}

static tTree Mword
#if defined __STDC__ | defined __cplusplus
(register tTree yyP19, register tTree yyP18)
#else
(yyP19, yyP18)
 register tTree yyP19;
 register tTree yyP18;
#endif
{
  if (yyP18->Kind == klocal_ident) {
/* line 936 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP19;
    yyV1->one_word.ident = yyP18->local_ident.ident;
    yyV1->one_word.pos = yyP18->local_ident.pos;
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP18, kqualification_c)) {
/* line 939 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_word,MakeTree,yyV1,kqual_word,Tree_InitHead)
    begintTree (yyV1->qual_word.env)
    yyV1->qual_word.next = yyP19;
    yyV1->qual_word.qualification = yyP18;
   return yyV1;
  }
 }

  }
  if (yyP18->Kind == kone_qualification) {
/* line 942 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_words,MakeTree,yyV1,kqual_words,Tree_InitHead)
    begintTree (yyV1->qual_words.env)
    yyV1->qual_words.next = yyP19;
    yyV1->qual_words.qualifications = yyP18;
   return yyV1;
  }
 }

  }
  if (yyP18->Kind == kone_word) {
/* line 945 "tcl.puma" */
  {
/* line 946 "tcl.puma" */
   yyP18->one_word.next = yyP19;
  }
   return yyP18;

  }
 yyAbort ("Mword");
 return 0;
}

static tTree Mqualification
#if defined __STDC__ | defined __cplusplus
(register tTree yyP20)
#else
(yyP20)
 register tTree yyP20;
#endif
{
  if (yyP20->Kind == kone_word) {
/* line 954 "tcl.puma" */
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
    yyW2->local_ident.pos = yyP20->one_word.pos;
    begintTree (yyW2->local_ident.env)
    yyW2->local_ident.ident = yyP20->one_word.ident;
   }
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP20, kqualification_c)) {
/* line 958 "tcl.puma" */
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
    yyV1->one_qualification.qualification = yyP20;
   return yyV1;
  }
 }

  }
  if (Tree_IsType (yyP20, kqualifications)) {
/* line 961 "tcl.puma" */
   return yyP20;

  }
 yyAbort ("Mqualification");
 return 0;
}

static tTree Mstmt
#if defined __STDC__ | defined __cplusplus
(register tTree yyP22, register tTree yyP21)
#else
(yyP22, yyP21)
 register tTree yyP22;
 register tTree yyP21;
#endif
{
  if (yyP21->Kind == kone_word) {
/* line 969 "tcl.puma" */
 {
  tTree stmt;
  {
/* line 970 "tcl.puma" */
   if (! (yyP21->one_word.ident == iproc || yyP21->one_word.ident == inamespace)) goto yyL1;
  {
/* line 971 "tcl.puma" */

/* line 971 "tcl.puma" */
   stmt = Mstmt_2 (yyP22, yyP21->one_word.ident, yyP21->one_word.next);
/* line 972 "tcl.puma" */
   if (! (stmt != NULL)) goto yyL1;
  }
  }
  {
   return stmt;
  }
 }
yyL1:;

  }
/* line 975 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ystmt,MakeTree,yyV1,kstmt,Tree_InitHead)
    begintTree (yyV1->stmt.env)
    yyV1->stmt.next = yyP22;
    yyV1->stmt.words = yyP21;
   return yyV1;
  }
 }

}

static tTree Mstmt_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP25, register tIdent yyP24, register tTree yyP23)
#else
(yyP25, yyP24, yyP23)
 register tTree yyP25;
 register tIdent yyP24;
 register tTree yyP23;
#endif
{
 {
  tTree p;
  register tTree yyV1;
  register tTree yyV2;
  if (equaltIdent (yyP24, (iproc))) {
  if (Tree_IsType (yyP23, kword_c)) {
  if (Tree_IsType (yyP23->word_c.next, kword_c)) {
  if (yyP23->word_c.next->word_c.next->Kind == kqual_word) {
  if (yyP23->word_c.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP23->word_c.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 983 "tcl.puma" */
  {
/* line 987 "tcl.puma" */

/* line 988 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV1,kproc,Tree_InitHead)
    begintTree (yyV1->proc.env)
    yyV1->proc.next = yyP25;
    yyV1->proc.qualification = make_qualification (yyP23);
    yyV1->proc.param_names = ReverseTree (param_names (yyP23->word_c.next));
    yyV1->proc.parameter = yyP23->word_c.next;
    yyV1->proc.block = yyP23->word_c.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->proc.epos = yyP23->word_c.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->proc.attribute = PAF_PUBLIC;
   p = yyV1;
/* line 990 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV2,knoword,Tree_InitHead)
    begintTree (yyV2->noword.env)
   yyP23->word_c.next->word_c.next = yyV2;
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
  if (equaltIdent (yyP24, (iproc))) {
  if (Tree_IsType (yyP23, kword_c)) {
  if (Tree_IsType (yyP23->word_c.next, kword_c)) {
  if (Tree_IsType (yyP23->word_c.next->word_c.next, kword_c)) {
/* line 993 "tcl.puma" */
  {
/* line 997 "tcl.puma" */
   get_end_pos (yyP23->word_c.next->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yproc,MakeTree,yyV2,kproc,Tree_InitHead)
    begintTree (yyV2->proc.env)
    yyV2->proc.next = yyP25;
    yyV2->proc.qualification = make_qualification (yyP23);
    yyV2->proc.param_names = ReverseTree (param_names (yyP23->word_c.next));
    yyV2->proc.parameter = yyP23->word_c.next;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->proc.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->proc.epos = yyV1;
    yyV2->proc.attribute = PAF_PUBLIC;
   return yyV2;
  }

  }
  }
  }
  }
 }
 {
  if (equaltIdent (yyP24, (inamespace))) {
  if (yyP23->Kind == kone_word) {
  if (Tree_IsType (yyP23->one_word.next, kword_c)) {
  if (yyP23->one_word.next->word_c.next->Kind == kqual_word) {
  if (yyP23->one_word.next->word_c.next->qual_word.qualification->Kind == klocal_text) {
  if (yyP23->one_word.next->word_c.next->qual_word.qualification->local_text.texts->Kind == kblock) {
  if (equaltIdent (yyP23->one_word.ident, (ieval))) {
/* line 1001 "tcl.puma" */
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV1,knamespace,Tree_InitHead)
    begintTree (yyV1->namespace.env)
    yyV1->namespace.next = yyP25;
    yyV1->namespace.qualification = make_qualification (yyP23->one_word.next);
    yyV1->namespace.block = yyP23->one_word.next->word_c.next->qual_word.qualification->local_text.texts;
    yyV1->namespace.epos = yyP23->one_word.next->word_c.next->qual_word.qualification->local_text.texts->block.epos;
    yyV1->namespace.attribute = PAF_PUBLIC;
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
  if (equaltIdent (yyP24, (inamespace))) {
  if (yyP23->Kind == kone_word) {
  if (Tree_IsType (yyP23->one_word.next, kword_c)) {
  if (Tree_IsType (yyP23->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP23->one_word.ident, (ieval))) {
/* line 1007 "tcl.puma" */
  {
/* line 1011 "tcl.puma" */
   get_end_pos (yyP23->one_word.next->word_c.next, & yyV1);
  }
  {
  register tTree yyV2;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynamespace,MakeTree,yyV2,knamespace,Tree_InitHead)
    begintTree (yyV2->namespace.env)
    yyV2->namespace.next = yyP25;
    yyV2->namespace.qualification = make_qualification (yyP23->one_word.next);
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ynotext,MakeTree,yyW1,knotext,Tree_InitHead)
    yyV2->namespace.block = yyW1;
    begintTree (yyW1->notext.env)
   }
    yyV2->namespace.epos = yyV1;
    yyV2->namespace.attribute = PAF_PUBLIC;
   return yyV2;
  }

  }
  }
  }
  }
  }
 }
/* line 1014 "tcl.puma" */
   return NULL;

}

static tTree Mblock_content
#if defined __STDC__ | defined __cplusplus
(tPosition yyP28, tPosition yyP27, register tTree yyP26)
#else
(yyP28, yyP27, yyP26)
 tPosition yyP28;
 tPosition yyP27;
 register tTree yyP26;
#endif
{
  if (yyP26->Kind == kstmt) {
  if (yyP26->stmt.next->Kind == knostmt) {
  if (yyP26->stmt.words->Kind == kone_word) {
  if (yyP26->stmt.words->one_word.next->Kind == knoword) {
/* line 1022 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ycontent,MakeTree,yyV1,kcontent,Tree_InitHead)
    begintTree (yyV1->content.env)
    yyV1->content.next = NoTree;
    yyV1->content.pos = yyP28;
   {register tTree yyW1;
    yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
     Tree_Alloc,ylocal_ident,MakeTree,yyW1,klocal_ident,Tree_InitHead)
    yyV1->content.qualification = yyW1;
    yyW1->local_ident.pos = yyP26->stmt.words->one_word.pos;
    begintTree (yyW1->local_ident.env)
    yyW1->local_ident.ident = yyP26->stmt.words->one_word.ident;
   }
   return yyV1;
  }
 }

  }
  }
  if (yyP26->stmt.words->Kind == kqual_word) {
  if (yyP26->stmt.words->qual_word.next->Kind == knoword) {
/* line 1026 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ycontent,MakeTree,yyV1,kcontent,Tree_InitHead)
    begintTree (yyV1->content.env)
    yyV1->content.next = NoTree;
    yyV1->content.pos = yyP28;
    yyV1->content.qualification = yyP26->stmt.words->qual_word.qualification;
   return yyV1;
  }
 }

  }
  }
  }
  }
/* line 1030 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yblock_content,MakeTree,yyV1,kblock_content,Tree_InitHead)
    begintTree (yyV1->block_content.env)
    yyV1->block_content.next = NoTree;
    yyV1->block_content.pos = yyP28;
    yyV1->block_content.epos = yyP27;
    yyV1->block_content.stmts = yyP26;
   return yyV1;
  }
 }

}

static tTree make_qualification
#if defined __STDC__ | defined __cplusplus
(register tTree yyP29)
#else
(yyP29)
 register tTree yyP29;
#endif
{
 yyRecursion:
  if (yyP29->Kind == kone_word) {
/* line 1038 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ylocal_ident,MakeTree,yyV1,klocal_ident,Tree_InitHead)
    yyV1->local_ident.pos = yyP29->one_word.pos;
    begintTree (yyV1->local_ident.env)
    yyV1->local_ident.ident = yyP29->one_word.ident;
   return yyV1;
  }
 }

  }
  if (yyP29->Kind == kqual_word) {
/* line 1041 "tcl.puma" */
   return yyP29->qual_word.qualification;

  }
  if (yyP29->Kind == kqual_words) {
/* line 1044 "tcl.puma" */
   yyP29 = yyP29->qual_words.qualifications;
   goto yyRecursion;

  }
  if (yyP29->Kind == kone_qualification) {
  if (yyP29->one_qualification.next->Kind == knoqualification) {
/* line 1047 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yqual_word,MakeTree,yyV1,kqual_word,Tree_InitHead)
    begintTree (yyV1->qual_word.env)
    begintTree (yyV1->qual_word.next)
    yyV1->qual_word.qualification = yyP29->one_qualification.qualification;
   yyP29 = yyV1;
   goto yyRecursion;
  }
 }

  }
/* line 1050 "tcl.puma" */
   yyP29 = yyP29->one_qualification.next;
   goto yyRecursion;

  }
 yyAbort ("make_qualification");
 return 0;
}

static tTree param_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP30)
#else
(yyP30)
 register tTree yyP30;
#endif
{
  if (yyP30->Kind == kone_word) {
/* line 1058 "tcl.puma" */
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
    yyV1->one_word.ident = yyP30->one_word.ident;
    yyV1->one_word.pos = yyP30->one_word.pos;
   return yyV1;
  }
 }

  }
  if (yyP30->Kind == kqual_word) {
  if (yyP30->qual_word.qualification->Kind == klocal_text) {
  if (yyP30->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1061 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,ynoword,MakeTree,yyV1,knoword,Tree_InitHead)
    begintTree (yyV1->noword.env)
   return param_names_2 (yyP30->qual_word.qualification->local_text.texts->block.stmts, yyV1);
  }
 }

  }
  }
/* line 1064 "tcl.puma" */
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
    yyV1->one_word.ident = make_one_word (yyP30->qual_word.qualification);
    yyV1->one_word.pos = yyP30->qual_word.qualification->qualification_c.pos;
   return yyV1;
  }
 }

  }
  if (yyP30->Kind == kqual_words) {
/* line 1067 "tcl.puma" */
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
(register tTree yyP32, register tTree yyP31)
#else
(yyP32, yyP31)
 register tTree yyP32;
 register tTree yyP31;
#endif
{
  if (yyP32->Kind == kstmt) {
/* line 1075 "tcl.puma" */
   return param_names_2 (yyP32->stmt.next, param_names_3 (yyP32->stmt.words, yyP31));

  }
/* line 1078 "tcl.puma" */
   return yyP31;

}

static tTree param_names_3
#if defined __STDC__ | defined __cplusplus
(register tTree yyP34, register tTree yyP33)
#else
(yyP34, yyP33)
 register tTree yyP34;
 register tTree yyP33;
#endif
{
  if (yyP34->Kind == kone_word) {
/* line 1086 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP33;
    yyV1->one_word.ident = yyP34->one_word.ident;
    yyV1->one_word.pos = yyP34->one_word.pos;
   return param_names_3 (yyP34->one_word.next, yyV1);
  }
 }

  }
  if (yyP34->Kind == kqual_word) {
  if (yyP34->qual_word.qualification->Kind == klocal_text) {
  if (yyP34->qual_word.qualification->local_text.texts->Kind == kblock) {
  if (yyP34->qual_word.qualification->local_text.texts->block.stmts->Kind == kstmt) {
/* line 1089 "tcl.puma" */
   return param_names_3 (yyP34->qual_word.next, param_names_4 (yyP34->qual_word.qualification->local_text.texts->block.stmts->stmt.words, yyP33));

  }
  }
  }
/* line 1093 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP33;
    yyV1->one_word.ident = make_one_word (yyP34->qual_word.qualification);
    yyV1->one_word.pos = yyP34->qual_word.qualification->qualification_c.pos;
   return param_names_3 (yyP34->qual_word.next, yyV1);
  }
 }

  }
/* line 1096 "tcl.puma" */
   return yyP33;

}

static tTree param_names_4
#if defined __STDC__ | defined __cplusplus
(register tTree yyP36, register tTree yyP35)
#else
(yyP36, yyP35)
 register tTree yyP36;
 register tTree yyP35;
#endif
{
  if (yyP36->Kind == kone_word) {
/* line 1104 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP35;
    yyV1->one_word.ident = yyP36->one_word.ident;
    yyV1->one_word.pos = yyP36->one_word.pos;
   return yyV1;
  }
 }

  }
  if (yyP36->Kind == kqual_word) {
/* line 1107 "tcl.puma" */
 {
  {
  register tTree yyV1;
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yone_word,MakeTree,yyV1,kone_word,Tree_InitHead)
    begintTree (yyV1->one_word.env)
    yyV1->one_word.next = yyP35;
    yyV1->one_word.ident = make_one_word (yyP36->qual_word.qualification);
    yyV1->one_word.pos = yyP36->qual_word.qualification->qualification_c.pos;
   return yyV1;
  }
 }

  }
/* line 1110 "tcl.puma" */
   return yyP35;

}

static void get_end_pos
#if defined __STDC__ | defined __cplusplus
(register tTree yyP37, tPosition * yyP38)
#else
(yyP37, yyP38)
 register tTree yyP37;
 tPosition * yyP38;
#endif
{
  if (yyP37->Kind == kone_word) {
/* line 1118 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1119 "tcl.puma" */

/* line 1119 "tcl.puma" */
   e = yyP37->one_word.pos;
/* line 1120 "tcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP37->one_word.ident)) - 1;
  }
   * yyP38 = e;
   return;
 }

  }
  if (yyP37->Kind == kqual_word) {
/* line 1122 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1123 "tcl.puma" */
   get_end_pos (yyP37->qual_word.qualification, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  }
  if (yyP37->Kind == kqual_words) {
/* line 1125 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1126 "tcl.puma" */
   get_end_pos (yyP37->qual_words.qualifications, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  }
  if (yyP37->Kind == kone_qualification) {
/* line 1128 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1129 "tcl.puma" */
   get_end_pos (yyP37->one_qualification.qualification, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  }
  if (Tree_IsType (yyP37, ktext)) {
  if (Tree_IsType (yyP37->text.next, ktext)) {
/* line 1131 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1132 "tcl.puma" */
   get_end_pos (yyP37->text.next, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  }
  }

  switch (yyP37->Kind) {
  case kcharacter:
/* line 1134 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1135 "tcl.puma" */

/* line 1135 "tcl.puma" */
   e = yyP37->character.pos;
  }
   * yyP38 = e;
   return;
 }

  case kident:
/* line 1137 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1138 "tcl.puma" */

/* line 1138 "tcl.puma" */
   e = yyP37->ident.pos;
/* line 1139 "tcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP37->ident.ident)) - 1;
  }
   * yyP38 = e;
   return;
 }

  case kblock:
/* line 1141 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1142 "tcl.puma" */

/* line 1142 "tcl.puma" */
   e = yyP37->block.epos;
  }
   * yyP38 = e;
   return;
 }

  case kcontent:
/* line 1144 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1145 "tcl.puma" */
   get_end_pos (yyP37->content.qualification, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  case kblock_content:
/* line 1147 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1148 "tcl.puma" */

/* line 1148 "tcl.puma" */
   e = yyP37->block_content.epos;
  }
   * yyP38 = e;
   return;
 }

  case klocal_ident:
/* line 1150 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1151 "tcl.puma" */

/* line 1151 "tcl.puma" */
   e = yyP37->local_ident.pos;
/* line 1152 "tcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP37->local_ident.ident)) - 1;
  }
   * yyP38 = e;
   return;
 }

  case kglobal_ident:
/* line 1154 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1155 "tcl.puma" */

/* line 1155 "tcl.puma" */
   e = yyP37->global_ident.pos;
/* line 1156 "tcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP37->global_ident.ident)) - 1;
  }
   * yyP38 = e;
   return;
 }

  case klocal_text:
/* line 1158 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1159 "tcl.puma" */
   get_end_pos (yyP37->local_text.texts, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  case kglobal_text:
/* line 1161 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1162 "tcl.puma" */
   get_end_pos (yyP37->global_text.texts, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  case kqualification:
/* line 1164 "tcl.puma" */
 {
  tPosition e;
  {
/* line 1165 "tcl.puma" */

/* line 1165 "tcl.puma" */
   e = yyP37->qualification.pos;
/* line 1166 "tcl.puma" */
   e . Column = e . Column + StLength (GetStringRef (yyP37->qualification.ident)) - 1;
  }
   * yyP38 = e;
   return;
 }

  case kcomplex_qual:
/* line 1168 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1169 "tcl.puma" */
   get_end_pos (yyP37->complex_qual.texts, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  case ksubscription:
/* line 1171 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1172 "tcl.puma" */
   get_end_pos (yyP37->subscription.qualification, & yyV1);
  }
   * yyP38 = yyV1;
   return;
 }

  }

;
}

static void get_begin_pos
#if defined __STDC__ | defined __cplusplus
(register tTree yyP39, tPosition * yyP40)
#else
(yyP39, yyP40)
 register tTree yyP39;
 tPosition * yyP40;
#endif
{
  if (yyP39->Kind == kqualification) {
/* line 1179 "tcl.puma" */
   * yyP40 = yyP39->qualification.qualification->qualification_c.pos;
   return;

  }
  if (yyP39->Kind == kcomplex_qual) {
  if (yyP39->complex_qual.qualification->Kind == kqualification) {
/* line 1180 "tcl.puma" */
   * yyP40 = yyP39->complex_qual.qualification->qualification.pos;
   return;

  }
  }
/* line 1181 "tcl.puma" */
   * yyP40 = yyP39->qualification_c.pos;
   return;

;
}

static tTree get_env
#if defined __STDC__ | defined __cplusplus
(register tTree yyP41)
#else
(yyP41)
 register tTree yyP41;
#endif
{
  if (yyP41->Kind == kqualification) {
/* line 1187 "tcl.puma" */
   return get_env_2 (yyP41->qualification.qualification);

  }
  if (yyP41->Kind == kcomplex_qual) {
/* line 1187 "tcl.puma" */
   return get_env_2 (yyP41->complex_qual.qualification);

  }
  if (yyP41->Kind == ksubscription) {
/* line 1187 "tcl.puma" */
   return get_env_2 (yyP41->subscription.qualification);

  }
/* line 1187 "tcl.puma" */
   return get_env_2 (yyP41);

}

static tTree get_env_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP42)
#else
(yyP42)
 register tTree yyP42;
#endif
{
 yyRecursion:

  switch (yyP42->Kind) {
  case klocal_ident:
/* line 1198 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1199 "tcl.puma" */

/* line 1199 "tcl.puma" */
   obj = IdentifyLocal (yyP42->local_ident.ident, yyP42->local_ident.env -> env . objects);
/* line 1200 "tcl.puma" */
   if (! (obj != NULL && obj -> object . object -> Kind == knamespace)) goto yyL1;
  {
/* line 1201 "tcl.puma" */
   use (obj, yyP42->local_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj -> object . object -> namespace . block -> words . env;
  }
 }
yyL1:;

/* line 1204 "tcl.puma" */
  {
/* line 1205 "tcl.puma" */
   use_undef (yyP42->local_ident.ident, yyP42->local_ident.pos);
  }
   return NULL;

  case kglobal_ident:
/* line 1208 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1209 "tcl.puma" */

/* line 1209 "tcl.puma" */
   obj = IdentifyGlobal (yyP42->global_ident.ident);
/* line 1210 "tcl.puma" */
   if (! (obj != NULL && obj -> object . object -> Kind == knamespace)) goto yyL3;
  {
/* line 1211 "tcl.puma" */
   use (obj, yyP42->global_ident.pos, PAF_REF_READ);
  }
  }
  {
   return obj -> object . object -> namespace . block -> words . env;
  }
 }
yyL3:;

/* line 1214 "tcl.puma" */
  {
/* line 1215 "tcl.puma" */
   use_undef (yyP42->global_ident.ident, yyP42->global_ident.pos);
  }
   return NULL;

  case kqualification:
/* line 1218 "tcl.puma" */
 {
  tTree env;
  tTree obj;
  {
/* line 1219 "tcl.puma" */

/* line 1219 "tcl.puma" */
   env = get_env_2 (yyP42->qualification.qualification);
/* line 1220 "tcl.puma" */
   if (! (env != NULL)) goto yyL5;
  {
/* line 1221 "tcl.puma" */

/* line 1221 "tcl.puma" */
   obj = IdentifyLocal (yyP42->qualification.ident, env -> env . objects);
/* line 1222 "tcl.puma" */
   if (! (obj != NULL && obj -> object . object -> Kind == knamespace)) goto yyL5;
  {
/* line 1223 "tcl.puma" */
   use (obj, yyP42->qualification.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj -> object . object -> namespace . block -> words . env;
  }
 }
yyL5:;

/* line 1226 "tcl.puma" */
  {
/* line 1227 "tcl.puma" */
   use_undef (yyP42->qualification.ident, yyP42->qualification.pos);
  }
   return NULL;

  case kcomplex_qual:
/* line 1230 "tcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 1231 "tcl.puma" */

/* line 1231 "tcl.puma" */
   env = get_env_2 (yyP42->complex_qual.qualification);
/* line 1232 "tcl.puma" */
   if (! (env != NULL)) goto yyL7;
  {
/* line 1233 "tcl.puma" */

/* line 1233 "tcl.puma" */
   ident = make_one_word (yyP42->complex_qual.texts);
/* line 1234 "tcl.puma" */

/* line 1234 "tcl.puma" */
   obj = IdentifyLocal (ident, env -> env . objects);
/* line 1235 "tcl.puma" */
   if (! (obj != NULL && obj -> object . object -> Kind == knamespace)) goto yyL7;
  {
/* line 1236 "tcl.puma" */
   use (obj, yyP42->complex_qual.pos, PAF_REF_READ);
  }
  }
  }
  {
   return obj -> object . object -> namespace . block -> words . env;
  }
 }
yyL7:;

  break;
  case ksubscription:
/* line 1239 "tcl.puma" */
   yyP42 = yyP42->subscription.qualification;
   goto yyRecursion;

  case klocal_text:
/* line 1242 "tcl.puma" */
   return NULL;

  case kglobal_text:
/* line 1245 "tcl.puma" */
   return NULL;

  }

/* line 1248 "tcl.puma" */
   return NULL;

}

static tIdent get_ident
#if defined __STDC__ | defined __cplusplus
(register tTree yyP43)
#else
(yyP43)
 register tTree yyP43;
#endif
{
 yyRecursion:
  if (yyP43->Kind == kqualification) {
/* line 1256 "tcl.puma" */
   return yyP43->qualification.ident;

  }
  if (yyP43->Kind == kcomplex_qual) {
/* line 1259 "tcl.puma" */
   return make_one_word (yyP43->complex_qual.texts);

  }
  if (yyP43->Kind == ksubscription) {
/* line 1262 "tcl.puma" */
   yyP43 = yyP43->subscription.qualification;
   goto yyRecursion;

  }
  if (yyP43->Kind == klocal_ident) {
/* line 1265 "tcl.puma" */
   return yyP43->local_ident.ident;

  }
  if (yyP43->Kind == kglobal_ident) {
/* line 1268 "tcl.puma" */
   return yyP43->global_ident.ident;

  }
/* line 1271 "tcl.puma" */
   return NoIdent;

}

static void add_qualifications
#if defined __STDC__ | defined __cplusplus
(register tIdent yyP45, register tTree yyP44)
#else
(yyP45, yyP44)
 register tIdent yyP45;
 register tTree yyP44;
#endif
{
  if (equaltIdent (yyP45, (iset))) {
  if (yyP44->word_c.next->Kind == knoword) {
/* line 1279 "tcl.puma" */
  {
/* line 1280 "tcl.puma" */
   add_qual (yyP44, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP45, (iset))) {
/* line 1282 "tcl.puma" */
  {
/* line 1283 "tcl.puma" */
   add_qual (yyP44, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP45, (ivariable))) {
  if (yyP44->word_c.next->Kind == knoword) {
/* line 1285 "tcl.puma" */
  {
/* line 1286 "tcl.puma" */
   add_variable_quals (yyP44, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP45, (ivariable))) {
/* line 1288 "tcl.puma" */
  {
/* line 1289 "tcl.puma" */
   add_variable_quals (yyP44, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP45, (iappend))) {
/* line 1291 "tcl.puma" */
  {
/* line 1292 "tcl.puma" */
   add_qual (yyP44, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP45, (ilappend))) {
/* line 1294 "tcl.puma" */
  {
/* line 1295 "tcl.puma" */
   add_qual (yyP44, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP45, (iarray))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (iset))) {
/* line 1297 "tcl.puma" */
  {
/* line 1299 "tcl.puma" */
   add_qual (yyP44->one_word.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP45, (iarray))) {
  if (Tree_IsType (yyP44->word_c.next, kword_c)) {
/* line 1301 "tcl.puma" */
  {
/* line 1302 "tcl.puma" */
   add_qual (yyP44->word_c.next, PAF_REF_READ);
  }
   return;

  }
  }
  if (equaltIdent (yyP45, (ibinary))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (iscan))) {
/* line 1304 "tcl.puma" */
  {
/* line 1308 "tcl.puma" */
   add_quals (yyP44->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP45, (iscan))) {
  if (Tree_IsType (yyP44->word_c.next, kword_c)) {
  if (Tree_IsType (yyP44->word_c.next->word_c.next, kword_c)) {
/* line 1310 "tcl.puma" */
  {
/* line 1313 "tcl.puma" */
   add_quals (yyP44->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  if (equaltIdent (yyP45, (iforeach))) {
/* line 1315 "tcl.puma" */
  {
/* line 1316 "tcl.puma" */
   add_foreach_quals (yyP44);
  }
   return;

  }
  if (equaltIdent (yyP45, (ivwait))) {
/* line 1318 "tcl.puma" */
  {
/* line 1319 "tcl.puma" */
   add_qual (yyP44, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP45, (iincr))) {
/* line 1321 "tcl.puma" */
  {
/* line 1322 "tcl.puma" */
   add_qual (yyP44, PAF_REF_WRITE);
  }
   return;

  }
  if (equaltIdent (yyP45, (iparray))) {
/* line 1324 "tcl.puma" */
  {
/* line 1325 "tcl.puma" */
   add_qual (yyP44, PAF_REF_READ);
  }
   return;

  }
  if (equaltIdent (yyP45, (icatch))) {
  if (Tree_IsType (yyP44->word_c.next, kword_c)) {
/* line 1327 "tcl.puma" */
  {
/* line 1328 "tcl.puma" */
   add_qual (yyP44->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP45, (igets))) {
  if (Tree_IsType (yyP44->word_c.next, kword_c)) {
/* line 1330 "tcl.puma" */
  {
/* line 1331 "tcl.puma" */
   add_qual (yyP44->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  if (equaltIdent (yyP45, (iinfo))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (iexists))) {
/* line 1333 "tcl.puma" */
  {
/* line 1335 "tcl.puma" */
   add_qual (yyP44->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP45, (iinfo))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (iargs))) {
/* line 1337 "tcl.puma" */
  {
/* line 1339 "tcl.puma" */
   add_qual (yyP44->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP45, (iinfo))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (ibody))) {
/* line 1341 "tcl.puma" */
  {
/* line 1343 "tcl.puma" */
   add_qual (yyP44->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP45, (iinfo))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (idefault))) {
/* line 1345 "tcl.puma" */
  {
/* line 1349 "tcl.puma" */
   add_qual (yyP44, PAF_REF_READ);
/* line 1350 "tcl.puma" */
   add_qual (yyP44->one_word.next->word_c.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  }
  if (equaltIdent (yyP45, (itkinfo))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (ivariable))) {
/* line 1352 "tcl.puma" */
  {
/* line 1354 "tcl.puma" */
   add_qual (yyP44->one_word.next, PAF_REF_READ);
  }
   return;

  }
  }
  }
  }
  if (equaltIdent (yyP45, (ifile))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (ilstat))) {
/* line 1356 "tcl.puma" */
  {
/* line 1359 "tcl.puma" */
   add_qual (yyP44->one_word.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP45, (ifile))) {
  if (yyP44->Kind == kone_word) {
  if (Tree_IsType (yyP44->one_word.next, kword_c)) {
  if (Tree_IsType (yyP44->one_word.next->word_c.next, kword_c)) {
  if (equaltIdent (yyP44->one_word.ident, (istat))) {
/* line 1361 "tcl.puma" */
  {
/* line 1364 "tcl.puma" */
   add_qual (yyP44->one_word.next->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
  }
  }
  }
  }
  if (equaltIdent (yyP45, (iunset))) {
/* line 1366 "tcl.puma" */
  {
/* line 1367 "tcl.puma" */
   add_quals (yyP44, PAF_REF_WRITE);
  }
   return;

  }
;
}

static void add_variable_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP46, register int acc)
#else
(yyP46, acc)
 register tTree yyP46;
 register int acc;
#endif
{
/* line 1379 "tcl.puma" */
 tTree obj, e2; 
 yyRecursion:
  if (yyP46->Kind == kqual_word) {
/* line 1381 "tcl.puma" */
  {
/* line 1382 "tcl.puma" */
   add_variable_quals (yyP46->qual_word.qualification, acc);
/* line 1383 "tcl.puma" */
   add_variable_quals_2 (yyP46->qual_word.next);
  }
   return;

  }
  if (yyP46->Kind == kqual_words) {
/* line 1385 "tcl.puma" */
  {
/* line 1386 "tcl.puma" */
   add_variable_quals_2 (yyP46->qual_words.next);
  }
   return;

  }
  if (yyP46->Kind == ksubscription) {
/* line 1388 "tcl.puma" */
  {
/* line 1389 "tcl.puma" */
   yyP46 = yyP46->subscription.qualification;
   goto yyRecursion;
  }

  }
  if (Tree_IsType (yyP46, kqualification_c)) {
/* line 1391 "tcl.puma" */
 {
  tIdent ident;
  tTree obj;
  register tTree yyV1;
  {
/* line 1392 "tcl.puma" */
   e2 = get_env (yyP46);
/* line 1393 "tcl.puma" */
   if (! (e2 != NULL)) goto yyL4;
  {
/* line 1394 "tcl.puma" */

/* line 1394 "tcl.puma" */
   ident = get_ident (yyP46);
/* line 1395 "tcl.puma" */

/* line 1395 "tcl.puma" */
   obj = IdentifyLocal (ident, e2 -> env . objects);
/* line 1396 "tcl.puma" */
   if (! (obj == NULL)) goto yyL4;
  {
/* line 1397 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = yyP46;
    yyV1->object.ident = ident;
    yyV1->object.next = e2 -> env . objects;
   e2 -> env . objects = yyV1;
/* line 1398 "tcl.puma" */
   yyP46->qualification_c.env = e2;
/* line 1399 "tcl.puma" */
   dcl (e2 -> env . objects);
/* line 1400 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (e2->env.objects, yyP46->qualification_c.pos, acc); 
  }
  }
  }
   return;
 }
yyL4:;

/* line 1402 "tcl.puma" */
  {
/* line 1403 "tcl.puma" */
   if (! (e2 != NULL)) goto yyL5;
  {
/* line 1404 "tcl.puma" */
   use (obj, yyP46->qualification_c.pos, acc);
  }
  }
   return;
yyL5:;

  }
;
}

static void add_variable_quals_2
#if defined __STDC__ | defined __cplusplus
(register tTree yyP47)
#else
(yyP47)
 register tTree yyP47;
#endif
{
  if (Tree_IsType (yyP47, kword_c)) {
  if (Tree_IsType (yyP47->word_c.next, kword_c)) {
/* line 1411 "tcl.puma" */
  {
/* line 1412 "tcl.puma" */
   add_variable_quals (yyP47->word_c.next, PAF_REF_WRITE);
  }
   return;

  }
/* line 1414 "tcl.puma" */
  {
/* line 1415 "tcl.puma" */
   add_variable_quals (yyP47->word_c.next, PAF_REF_READ);
  }
   return;

  }
;
}

static void add_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP48, register int acc)
#else
(yyP48, acc)
 register tTree yyP48;
 register int acc;
#endif
{
 yyRecursion:
  if (Tree_IsType (yyP48, kword_c)) {
/* line 1422 "tcl.puma" */
  {
/* line 1423 "tcl.puma" */
   add_qual (yyP48, acc);
/* line 1424 "tcl.puma" */
   yyP48 = yyP48->word_c.next;
   goto yyRecursion;
  }

  }
;
}

static void add_foreach_quals
#if defined __STDC__ | defined __cplusplus
(register tTree yyP49)
#else
(yyP49)
 register tTree yyP49;
#endif
{
 yyRecursion:
  if (yyP49->Kind == kqual_word) {
  if (Tree_IsType (yyP49->qual_word.next, kword_c)) {
  if (yyP49->qual_word.qualification->Kind == klocal_text) {
  if (yyP49->qual_word.qualification->local_text.texts->Kind == kblock) {
/* line 1431 "tcl.puma" */
  {
/* line 1433 "tcl.puma" */
   add_foreach_quals (yyP49->qual_word.qualification->local_text.texts->block.stmts);
/* line 1434 "tcl.puma" */
   yyP49 = yyP49->qual_word.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  }
  }
  if (Tree_IsType (yyP49, kword_c)) {
  if (Tree_IsType (yyP49->word_c.next, kword_c)) {
/* line 1436 "tcl.puma" */
  {
/* line 1437 "tcl.puma" */
   add_qual (yyP49, PAF_REF_WRITE);
/* line 1438 "tcl.puma" */
   yyP49 = yyP49->word_c.next->word_c.next;
   goto yyRecursion;
  }

  }
  }
  if (yyP49->Kind == kstmt) {
/* line 1440 "tcl.puma" */
  {
/* line 1441 "tcl.puma" */
   add_quals (yyP49->stmt.words, PAF_REF_WRITE);
/* line 1442 "tcl.puma" */
   yyP49 = yyP49->stmt.next;
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
/* line 1449 "tcl.puma" */
  {
/* line 1450 "tcl.puma" */
   add_qual_2 (w->qual_word.qualification, acc);
  }
   return;

  }
  if (w->Kind == kqual_words) {
/* line 1452 "tcl.puma" */
  {
/* line 1453 "tcl.puma" */
   w = w->qual_words.qualifications;
   goto yyRecursion;
  }

  }
  if (w->Kind == kone_qualification) {
  if (w->one_qualification.next->Kind == knoqualification) {
/* line 1455 "tcl.puma" */
  {
/* line 1456 "tcl.puma" */
   add_qual_2 (w->one_qualification.qualification, acc);
  }
   return;

  }
/* line 1458 "tcl.puma" */
  {
/* line 1459 "tcl.puma" */
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
/* line 1466 "tcl.puma" */
 tIdent id; tTree env; 
 yyRecursion:
  if (w->Kind == kglobal_ident) {
/* line 1468 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1469 "tcl.puma" */

/* line 1469 "tcl.puma" */
   obj = IdentifyGlobal (w->global_ident.ident);
/* line 1470 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 1471 "tcl.puma" */
   use (obj, w->global_ident.pos, acc);
  }
  }
   return;
 }
yyL1:;

/* line 1473 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 1474 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = w->global_ident.ident;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 1475 "tcl.puma" */
   dcl (genv -> env . objects);
/* line 1476 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_ident.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == kglobal_text) {
/* line 1478 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1479 "tcl.puma" */
   id = make_one_word (w->global_text.texts);
/* line 1480 "tcl.puma" */

/* line 1480 "tcl.puma" */
   obj = IdentifyGlobal (id);
/* line 1481 "tcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 1482 "tcl.puma" */
   use (obj, w->global_text.pos, acc);
  }
  }
   return;
 }
yyL3:;

/* line 1484 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 1485 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = id;
    yyV1->object.next = genv -> env . objects;
   genv -> env . objects = yyV1;
/* line 1486 "tcl.puma" */
   dcl (genv -> env . objects);
/* line 1487 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (genv->env.objects, w->global_text.pos, acc); 
  }
   return;
 }

  }
  if (w->Kind == klocal_text) {
/* line 1489 "tcl.puma" */
   return;

  }
  if (w->Kind == ksubscription) {
  if (w->subscription.qualification->Kind == klocal_ident) {
/* line 1491 "tcl.puma" */
   return;

  }
  if (w->subscription.qualification->Kind == klocal_text) {
/* line 1493 "tcl.puma" */
   return;

  }
/* line 1495 "tcl.puma" */
  {
/* line 1496 "tcl.puma" */
   w = w->subscription.qualification;
   goto yyRecursion;
  }

  }
/* line 1498 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1499 "tcl.puma" */
   env = get_env (w);
/* line 1500 "tcl.puma" */
   if (! (env != NULL)) goto yyL9;
  {
/* line 1501 "tcl.puma" */
   id = get_ident (w);
/* line 1502 "tcl.puma" */

/* line 1502 "tcl.puma" */
   obj = IdentifyLocal (id, env -> env . objects);
/* line 1503 "tcl.puma" */
   if (! (obj != NULL)) goto yyL9;
  {
/* line 1504 "tcl.puma" */
   use (obj, w->qualification_c.pos, acc);
  }
  }
  }
   return;
 }
yyL9:;

/* line 1506 "tcl.puma" */
 {
  register tTree yyV1;
  {
/* line 1507 "tcl.puma" */
   if (! (env != NULL)) goto yyL10;
  {
/* line 1508 "tcl.puma" */
   w->qualification_c.env = env;
/* line 1509 "tcl.puma" */
   yyALLOC (tTree,Tree_PoolFreePtr,Tree_PoolStartPtr,
    Tree_Alloc,yobject,MakeTree,yyV1,kobject,Tree_InitHead)
    yyV1->object.object = w;
    yyV1->object.ident = id;
    yyV1->object.next = env -> env . objects;
   env -> env . objects = yyV1;
/* line 1510 "tcl.puma" */
   dcl (env -> env . objects);
/* line 1511 "tcl.puma" */
  if (acc == PAF_REF_WRITE) use (env->env.objects, w->qualification_c.pos, acc); 
  }
  }
   return;
 }
yyL10:;

;
}

static void use_qual
#if defined __STDC__ | defined __cplusplus
(register tTree yyP50)
#else
(yyP50)
 register tTree yyP50;
#endif
{
 yyRecursion:
  if (yyP50->Kind == kglobal_ident) {
/* line 1518 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1519 "tcl.puma" */

/* line 1519 "tcl.puma" */
   obj = IdentifyGlobal (yyP50->global_ident.ident);
/* line 1520 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 1521 "tcl.puma" */
   use (obj, yyP50->global_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL1:;

/* line 1523 "tcl.puma" */
  {
/* line 1524 "tcl.puma" */
   use_undef (yyP50->global_ident.ident, yyP50->global_ident.pos);
  }
   return;

  }
  if (yyP50->Kind == klocal_ident) {
  if (yyP50->local_ident.env->Kind == kenv) {
  if (yyP50->local_ident.env->env.object->Kind == kproc) {
/* line 1526 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1528 "tcl.puma" */

/* line 1528 "tcl.puma" */
   obj = IdentifyLocal (yyP50->local_ident.ident, yyP50->local_ident.env -> env . objects);
/* line 1529 "tcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 1530 "tcl.puma" */
   use (obj, yyP50->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL3:;

  }
  if (yyP50->local_ident.env->env.object->Kind == kprogram) {
/* line 1526 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1528 "tcl.puma" */

/* line 1528 "tcl.puma" */
   obj = IdentifyLocal (yyP50->local_ident.ident, yyP50->local_ident.env -> env . objects);
/* line 1529 "tcl.puma" */
   if (! (obj != NULL)) goto yyL4;
  {
/* line 1530 "tcl.puma" */
   use (obj, yyP50->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL4:;

  }
  if (yyP50->local_ident.env->env.object->Kind == knamespace) {
/* line 1532 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1533 "tcl.puma" */

/* line 1533 "tcl.puma" */
   obj = Identify (yyP50->local_ident.ident, yyP50->local_ident.env);
/* line 1534 "tcl.puma" */
   if (! (obj != NULL)) goto yyL5;
  {
/* line 1535 "tcl.puma" */
   use (obj, yyP50->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL5:;

  }
  }
/* line 1537 "tcl.puma" */
  {
/* line 1538 "tcl.puma" */
   use_undef (yyP50->local_ident.ident, yyP50->local_ident.pos);
  }
   return;

  }
  if (yyP50->Kind == ksubscription) {
/* line 1554 "tcl.puma" */
  {
/* line 1555 "tcl.puma" */
   yyP50 = yyP50->subscription.qualification;
   goto yyRecursion;
  }

  }
/* line 1557 "tcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 1558 "tcl.puma" */

/* line 1558 "tcl.puma" */
   env = get_env (yyP50);
/* line 1559 "tcl.puma" */
   if (! (env != NULL)) goto yyL8;
  {
/* line 1560 "tcl.puma" */

/* line 1560 "tcl.puma" */
   ident = get_ident (yyP50);
/* line 1561 "tcl.puma" */

/* line 1561 "tcl.puma" */
   obj = IdentifyLocal (ident, env -> env . objects);
/* line 1562 "tcl.puma" */
   if (! (obj != NULL)) goto yyL8;
  {
/* line 1563 "tcl.puma" */
   use (obj, yyP50->qualification_c.pos, PAF_REF_READ);
  }
  }
  }
   return;
 }
yyL8:;

/* line 1565 "tcl.puma" */
 {
  tTree env;
  tIdent ident;
  {
/* line 1566 "tcl.puma" */

/* line 1566 "tcl.puma" */
   env = get_env (yyP50);
/* line 1567 "tcl.puma" */
   if (! (env != NULL)) goto yyL9;
  {
/* line 1568 "tcl.puma" */

/* line 1568 "tcl.puma" */
   ident = get_ident (yyP50);
/* line 1569 "tcl.puma" */
   use_undef (ident, yyP50->qualification_c.pos);
  }
  }
   return;
 }
yyL9:;

;
}

static void use_proc_qual
#if defined __STDC__ | defined __cplusplus
(register tTree yyP51)
#else
(yyP51)
 register tTree yyP51;
#endif
{
  if (yyP51->Kind == kglobal_ident) {
/* line 1576 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1577 "tcl.puma" */

/* line 1577 "tcl.puma" */
   obj = IdentifyProcGlobal (yyP51->global_ident.ident);
/* line 1578 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 1579 "tcl.puma" */
   use (obj, yyP51->global_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL1:;

/* line 1581 "tcl.puma" */
  {
/* line 1582 "tcl.puma" */
   use_proc_undef (yyP51->global_ident.ident, yyP51->global_ident.pos);
  }
   return;

  }
  if (yyP51->Kind == klocal_ident) {
  if (yyP51->local_ident.env->Kind == kenv) {
  if (yyP51->local_ident.env->env.object->Kind == kproc) {
/* line 1584 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1586 "tcl.puma" */

/* line 1586 "tcl.puma" */
   obj = IdentifyProcLocal (yyP51->local_ident.ident, yyP51->local_ident.env -> env . objects);
/* line 1587 "tcl.puma" */
   if (! (obj != NULL)) goto yyL3;
  {
/* line 1588 "tcl.puma" */
   use (obj, yyP51->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL3:;

  }
  if (yyP51->local_ident.env->env.object->Kind == kprogram) {
/* line 1584 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1586 "tcl.puma" */

/* line 1586 "tcl.puma" */
   obj = IdentifyProcLocal (yyP51->local_ident.ident, yyP51->local_ident.env -> env . objects);
/* line 1587 "tcl.puma" */
   if (! (obj != NULL)) goto yyL4;
  {
/* line 1588 "tcl.puma" */
   use (obj, yyP51->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL4:;

  }
  if (yyP51->local_ident.env->env.object->Kind == knamespace) {
/* line 1590 "tcl.puma" */
 {
  tTree obj;
  {
/* line 1591 "tcl.puma" */

/* line 1591 "tcl.puma" */
   obj = IdentifyProc (yyP51->local_ident.ident, yyP51->local_ident.env);
/* line 1592 "tcl.puma" */
   if (! (obj != NULL)) goto yyL5;
  {
/* line 1593 "tcl.puma" */
   use (obj, yyP51->local_ident.pos, PAF_REF_READ);
  }
  }
   return;
 }
yyL5:;

  }
  }
/* line 1595 "tcl.puma" */
  {
/* line 1596 "tcl.puma" */
   use_proc_undef (yyP51->local_ident.ident, yyP51->local_ident.pos);
  }
   return;

  }
/* line 1598 "tcl.puma" */
 {
  tTree env;
  tIdent ident;
  tTree obj;
  {
/* line 1599 "tcl.puma" */

/* line 1599 "tcl.puma" */
   env = get_env (yyP51);
/* line 1600 "tcl.puma" */
   if (! (env != NULL)) goto yyL7;
  {
/* line 1601 "tcl.puma" */

/* line 1601 "tcl.puma" */
   ident = get_ident (yyP51);
/* line 1602 "tcl.puma" */

/* line 1602 "tcl.puma" */
   obj = IdentifyProcLocal (ident, env -> env . objects);
/* line 1603 "tcl.puma" */
   if (! (obj != NULL)) goto yyL7;
  {
/* line 1604 "tcl.puma" */
   use (obj, yyP51->qualification_c.pos, PAF_REF_READ);
  }
  }
  }
   return;
 }
yyL7:;

/* line 1606 "tcl.puma" */
 {
  tTree env;
  tIdent ident;
  {
/* line 1607 "tcl.puma" */

/* line 1607 "tcl.puma" */
   env = get_env (yyP51);
/* line 1608 "tcl.puma" */
   if (! (env != NULL)) goto yyL8;
  {
/* line 1609 "tcl.puma" */

/* line 1609 "tcl.puma" */
   ident = get_ident (yyP51);
/* line 1610 "tcl.puma" */
   use_proc_undef (ident, yyP51->qualification_c.pos);
  }
  }
   return;
 }
yyL8:;

;
}

static void use_proc
#if defined __STDC__ | defined __cplusplus
(register tTree yyP52)
#else
(yyP52)
 register tTree yyP52;
#endif
{
/* line 1617 "tcl.puma" */
 tTree obj; 
 yyRecursion:
  if (yyP52->Kind == kone_word) {
/* line 1619 "tcl.puma" */
  {
/* line 1620 "tcl.puma" */
   obj = IdentifyProc (yyP52->one_word.ident, get_namespace (yyP52->one_word.env));
/* line 1621 "tcl.puma" */
   if (! (obj != NULL)) goto yyL1;
  {
/* line 1622 "tcl.puma" */
   use (obj, yyP52->one_word.pos, PAF_REF_READ);
  }
  }
   return;
yyL1:;

/* line 1624 "tcl.puma" */
  {
/* line 1625 "tcl.puma" */
   obj = Identify (yyP52->one_word.ident, get_namespace (yyP52->one_word.env));
/* line 1626 "tcl.puma" */
   if (! (obj != NULL)) goto yyL2;
  {
/* line 1627 "tcl.puma" */
   use (obj, yyP52->one_word.pos, PAF_REF_READ);
  }
  }
   return;
yyL2:;

/* line 1629 "tcl.puma" */
  {
/* line 1630 "tcl.puma" */
   use_proc_undef (yyP52->one_word.ident, yyP52->one_word.pos);
  }
   return;

  }
  if (yyP52->Kind == kqual_word) {
/* line 1632 "tcl.puma" */
  {
/* line 1633 "tcl.puma" */
   use_proc_qual (yyP52->qual_word.qualification);
  }
   return;

  }
  if (yyP52->Kind == kqual_words) {
/* line 1635 "tcl.puma" */
  {
/* line 1636 "tcl.puma" */
   yyP52 = yyP52->qual_words.qualifications;
   goto yyRecursion;
  }

  }
  if (yyP52->Kind == kone_qualification) {
  if (yyP52->one_qualification.next->Kind == knoqualification) {
/* line 1638 "tcl.puma" */
  {
/* line 1639 "tcl.puma" */
   use_proc_qual (yyP52->one_qualification.qualification);
  }
   return;

  }
/* line 1641 "tcl.puma" */
  {
/* line 1642 "tcl.puma" */
   yyP52 = yyP52->one_qualification.next;
   goto yyRecursion;
  }

  }
;
}

static void use
#if defined __STDC__ | defined __cplusplus
(register tTree yyP53, tPosition p, register int acc)
#else
(yyP53, p, acc)
 register tTree yyP53;
 tPosition p;
 register int acc;
#endif
{
 yyRecursion:

  switch (yyP53->Kind) {
  case kobject:
/* line 1649 "tcl.puma" */
  {
/* line 1650 "tcl.puma" */
   if (! ((cross_ref_fp))) goto yyL1;
  {
/* line 1651 "tcl.puma" */
   current_ident = GetCStr (yyP53->object.ident);
/* line 1652 "tcl.puma" */
   yyP53 = yyP53->object.object;
   goto yyRecursion;
  }
  }
yyL1:;

  break;
  case kone_word:
/* line 1654 "tcl.puma" */
  {
/* line 1658 "tcl.puma" */
   use_2 (yyP53->one_word.env, p, acc);
  }
   return;

  case klocal_ident:
/* line 1654 "tcl.puma" */
  {
/* line 1658 "tcl.puma" */
   use_2 (yyP53->local_ident.env, p, acc);
  }
   return;

  case kglobal_ident:
/* line 1654 "tcl.puma" */
  {
/* line 1658 "tcl.puma" */
   use_2 (yyP53->global_ident.env, p, acc);
  }
   return;

  case kqualification_c:
  case klocal_text:
  case kglobal_text:
  case kqualification:
  case kcomplex_qual:
  case ksubscription:
/* line 1654 "tcl.puma" */
  {
/* line 1658 "tcl.puma" */
   use_2 (yyP53->qualification_c.env, p, acc);
  }
   return;

  case kproc:
  if (yyP53->proc.env->Kind == kenv) {
  if (yyP53->proc.env->env.object->Kind == knamespace) {
/* line 1660 "tcl.puma" */
  {
/* line 1662 "tcl.puma" */
   put_cross_ref (PAF_REF_TO_MBR_FUNC, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, cur_arg_types_ptr, current_namespace (yyP53->proc.env), current_ident, NULL, current_file, (int)  p . Line, PAF_REF_READ);
  }
   return;

  }
  }
/* line 1667 "tcl.puma" */
  {
/* line 1669 "tcl.puma" */
   put_cross_ref (PAF_REF_TO_FUNCTION, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, cur_arg_types_ptr, NULL, current_ident, NULL, current_file, (int)  p . Line, PAF_REF_READ);
  }
   return;

  case knamespace:
/* line 1673 "tcl.puma" */
  {
/* line 1674 "tcl.puma" */
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
  if (env->env.object->Kind == kproc) {
/* line 1683 "tcl.puma" */
  {
/* line 1684 "tcl.puma" */
   if (! ((report_local_vars))) goto yyL1;
  {
/* line 1685 "tcl.puma" */
   put_cross_ref (PAF_REF_TO_LOCAL_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_LOCAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
  }
   return;
yyL1:;

  }
  if (env->env.object->Kind == kprogram) {
/* line 1689 "tcl.puma" */
  {
/* line 1690 "tcl.puma" */
   put_cross_ref (PAF_REF_TO_GLOB_VAR, scope_type (cur_class_ptr), PAF_REF_SCOPE_GLOBAL, cur_class_ptr, cur_proc_ptr, NULL, NULL, current_ident, NULL, current_file, (int)  pos . Line, acc);
  }
   return;

  }
  if (env->env.object->Kind == knamespace) {
/* line 1694 "tcl.puma" */
  {
/* line 1695 "tcl.puma" */
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
/* line 1704 "tcl.puma" */
 {
  char c;
  {
/* line 1705 "tcl.puma" */
   if (! (ident > last_keyword)) goto yyL1;
  {
/* line 1706 "tcl.puma" */

/* line 1706 "tcl.puma" */
   c = * GetCStr (ident);
/* line 1707 "tcl.puma" */
   if (! ('A' <= c && c <= 'Z' || 'a' <= c && c <= 'z' || c == '_')) goto yyL1;
  {
/* line 1708 "tcl.puma" */
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
/* line 1717 "tcl.puma" */
  {
/* line 1718 "tcl.puma" */
   if (! (! in_string)) goto yyL1;
  {
/* line 1719 "tcl.puma" */
   use_undef (ident, pos);
  }
  }
   return;
yyL1:;

;
}

static void dcl
#if defined __STDC__ | defined __cplusplus
(register tTree yyP54)
#else
(yyP54)
 register tTree yyP54;
#endif
{
 yyRecursion:

  switch (yyP54->Kind) {
  case kobject:
/* line 1726 "tcl.puma" */
  {
/* line 1727 "tcl.puma" */
   if (! (yyP54->object.ident != NoIdent)) goto yyL1;
  {
/* line 1728 "tcl.puma" */
   current_ident = GetCStr (yyP54->object.ident);
/* line 1731 "tcl.puma" */
   if (! (* current_ident != '-')) goto yyL1;
  {
/* line 1732 "tcl.puma" */
   yyP54 = yyP54->object.object;
   goto yyRecursion;
  }
  }
  }
yyL1:;

  break;
  case kone_word:
/* line 1734 "tcl.puma" */
  {
/* line 1738 "tcl.puma" */
   dcl_2 (yyP54->one_word.env, yyP54->one_word.pos);
  }
   return;

  case klocal_ident:
/* line 1734 "tcl.puma" */
  {
/* line 1738 "tcl.puma" */
   dcl_2 (yyP54->local_ident.env, yyP54->local_ident.pos);
  }
   return;

  case kglobal_ident:
/* line 1734 "tcl.puma" */
  {
/* line 1738 "tcl.puma" */
   dcl_2 (yyP54->global_ident.env, yyP54->global_ident.pos);
  }
   return;

  case kqualification_c:
  case klocal_text:
  case kglobal_text:
  case kqualification:
  case kcomplex_qual:
  case ksubscription:
/* line 1734 "tcl.puma" */
  {
/* line 1738 "tcl.puma" */
   dcl_2 (yyP54->qualification_c.env, yyP54->qualification_c.pos);
  }
   return;

  case kproc:
  if (yyP54->proc.env->Kind == kenv) {
  if (yyP54->proc.env->env.object->Kind == knamespace) {
/* line 1740 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1742 "tcl.puma" */
   to_names (yyP54->proc.param_names, args_buffer);
/* line 1743 "tcl.puma" */
   current_class = current_namespace (yyP54->proc.env);
/* line 1744 "tcl.puma" */
   get_begin_pos (yyP54->proc.qualification, & yyV1);
/* line 1745 "tcl.puma" */
   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  yyP54->proc.qualification->qualification_c.pos . Column - 1, (int)  yyP54->proc.epos . Line, (int)  yyP54->proc.epos . Column, PAF_PUBLIC, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  (yyP54->proc.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
/* line 1750 "tcl.puma" */
   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  yyP54->proc.qualification->qualification_c.pos . Column - 1, (int)  yyP54->proc.epos . Line, (int)  yyP54->proc.epos . Column, PAF_PUBLIC, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  (yyP54->proc.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  }
  }
/* line 1756 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1758 "tcl.puma" */
   to_names (yyP54->proc.param_names, args_buffer);
/* line 1759 "tcl.puma" */
   get_begin_pos (yyP54->proc.qualification, & yyV1);
/* line 1760 "tcl.puma" */
   put_symbol (PAF_FUNC_DEF, NULL, current_ident, current_file, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  yyP54->proc.qualification->qualification_c.pos . Column - 1, (int)  yyP54->proc.epos . Line, (int)  yyP54->proc.epos . Column, PAF_PUBLIC, NULL, NULL, args_buffer, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP54->proc.qualification->qualification_c.pos . Line, (int)  (yyP54->proc.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  case knamespace:
/* line 1772 "tcl.puma" */
 {
  tPosition yyV1;
  {
/* line 1774 "tcl.puma" */
   get_begin_pos (yyP54->namespace.qualification, & yyV1);
/* line 1775 "tcl.puma" */
   put_symbol (PAF_CLASS_DEF, current_namespace (yyP54->namespace.env), current_ident, current_file, (int)  yyP54->namespace.qualification->qualification_c.pos . Line, (int)  yyP54->namespace.qualification->qualification_c.pos . Column - 1, (int)  yyP54->namespace.epos . Line, (int)  yyP54->namespace.epos . Column, PAF_PUBLIC, NULL, NULL, NULL, NULL, (int)  yyV1 . Line, (int)  yyV1 . Column - 1, (int)  yyP54->namespace.qualification->qualification_c.pos . Line, (int)  (yyP54->namespace.qualification->qualification_c.pos . Column + strlen (current_ident) - 1));
  }
   return;
 }

  }

;
}

static void dcl_2
#if defined __STDC__ | defined __cplusplus
(register tTree env, tPosition pos)
#else
(env, pos)
 register tTree env;
 tPosition pos;
#endif
{
  if (env->env.object->Kind == kproc) {
/* line 1786 "tcl.puma" */
  {
/* line 1787 "tcl.puma" */
   if (! ((report_local_vars))) goto yyL1;
  {
/* line 1788 "tcl.puma" */
   put_symbol (PAF_LOCAL_VAR_DEF, NULL, current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), PAF_PUBLIC, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
  }
   return;
yyL1:;

  }
  if (env->env.object->Kind == kprogram) {
/* line 1793 "tcl.puma" */
  {
/* line 1794 "tcl.puma" */
   put_symbol (PAF_GLOB_VAR_DEF, NULL, current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), PAF_PUBLIC, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
  if (env->env.object->Kind == knamespace) {
/* line 1799 "tcl.puma" */
  {
/* line 1800 "tcl.puma" */
   put_symbol (PAF_MBR_VAR_DEF, current_namespace (env), current_ident, current_file, (int)  pos . Line, (int)  pos . Column - 1, (int)  pos . Line, (int)  (pos . Column + strlen (current_ident) - 1), PAF_PUBLIC, NULL, NULL, NULL, NULL, 0, 0, 0, 0);
  }
   return;

  }
;
}

static tString current_namespace
#if defined __STDC__ | defined __cplusplus
(register tTree yyP55)
#else
(yyP55)
 register tTree yyP55;
#endif
{
 yyRecursion:
  if (yyP55->Kind == kenv) {
  if (yyP55->env.object->Kind == kprogram) {
/* line 1810 "tcl.puma" */
   return NULL;

  }
  if (yyP55->env.object->Kind == knamespace) {
/* line 1813 "tcl.puma" */
   return GetCStr (get_ident (yyP55->env.object->namespace.qualification));

  }
/* line 1816 "tcl.puma" */
   yyP55 = yyP55->env.env;
   goto yyRecursion;

  }
 yyAbort ("current_namespace");
 return 0;
}

static tString get_class
#if defined __STDC__ | defined __cplusplus
(register tTree yyP56)
#else
(yyP56)
 register tTree yyP56;
#endif
{
  if (yyP56->Kind == kqualification) {
/* line 1824 "tcl.puma" */
   return get_class_name (yyP56->qualification.qualification);

  }
  if (yyP56->Kind == kcomplex_qual) {
/* line 1827 "tcl.puma" */
   return get_class_name (yyP56->complex_qual.qualification);

  }
/* line 1830 "tcl.puma" */
   return NULL;

}

static tString get_class_name
#if defined __STDC__ | defined __cplusplus
(register tTree yyP57)
#else
(yyP57)
 register tTree yyP57;
#endif
{
  if (yyP57->Kind == klocal_ident) {
/* line 1838 "tcl.puma" */
   return GetCStr (yyP57->local_ident.ident);

  }
  if (yyP57->Kind == kglobal_ident) {
/* line 1838 "tcl.puma" */
   return GetCStr (yyP57->global_ident.ident);

  }
  if (yyP57->Kind == kqualification) {
/* line 1838 "tcl.puma" */
   return GetCStr (yyP57->qualification.ident);

  }
/* line 1843 "tcl.puma" */
   return NULL;

}

static void to_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP59, register tString yyP58)
#else
(yyP59, yyP58)
 register tTree yyP59;
 register tString yyP58;
#endif
{
/* line 1851 "tcl.puma" */
  {
/* line 1851 "tcl.puma" */
   p = yyP58;
/* line 1851 "tcl.puma" */
   get_names (yyP59);
/* line 1851 "tcl.puma" */
   * p = '\0';
  }
   return;

;
}

static void get_names
#if defined __STDC__ | defined __cplusplus
(register tTree yyP60)
#else
(yyP60)
 register tTree yyP60;
#endif
{
 yyRecursion:
  if (yyP60->Kind == kone_word) {
/* line 1855 "tcl.puma" */
  {
/* line 1856 "tcl.puma" */
   GetString (yyP60->one_word.ident, p);
/* line 1856 "tcl.puma" */
   p = p + LengthSt (GetStringRef (yyP60->one_word.ident));
/* line 1857 "tcl.puma" */
   get_separator (yyP60->one_word.next);
/* line 1858 "tcl.puma" */
   yyP60 = yyP60->one_word.next;
   goto yyRecursion;
  }

  }
;
}

static void get_separator
#if defined __STDC__ | defined __cplusplus
(register tTree yyP61)
#else
(yyP61)
 register tTree yyP61;
#endif
{
  if (yyP61->Kind == kone_word) {
/* line 1865 "tcl.puma" */
  {
/* line 1865 "tcl.puma" */
   * p ++ = ',';
  }
   return;

  }
;
}

static void relocate
#if defined __STDC__ | defined __cplusplus
(register tTree yyP62, register tTree new_env)
#else
(yyP62, new_env)
 register tTree yyP62;
 register tTree new_env;
#endif
{
 yyRecursion:

  switch (yyP62->Kind) {
  case kstmt:
/* line 1871 "tcl.puma" */
  {
/* line 1872 "tcl.puma" */
   yyP62->stmt.env = new_env;
/* line 1873 "tcl.puma" */
   relocate (yyP62->stmt.words, new_env);
/* line 1874 "tcl.puma" */
   yyP62 = yyP62->stmt.next;
   goto yyRecursion;
  }

  case kproc:
  if (yyP62->proc.block->texts.env->Kind == kenv) {
/* line 1876 "tcl.puma" */
  {
/* line 1879 "tcl.puma" */
   yyP62->proc.env = new_env;
/* line 1880 "tcl.puma" */
   yyP62->proc.block->texts.env->env.env = new_env;
/* line 1881 "tcl.puma" */
   relocate (yyP62->proc.qualification, new_env);
/* line 1882 "tcl.puma" */
   yyP62 = yyP62->proc.next;
   goto yyRecursion;
  }

  }
  break;
  case knamespace:
  if (yyP62->namespace.block->texts.env->Kind == kenv) {
/* line 1884 "tcl.puma" */
  {
/* line 1886 "tcl.puma" */
   yyP62->namespace.env = new_env;
/* line 1887 "tcl.puma" */
   yyP62->namespace.block->texts.env->env.env = new_env;
/* line 1888 "tcl.puma" */
   relocate (yyP62->namespace.qualification, new_env);
/* line 1889 "tcl.puma" */
   yyP62 = yyP62->namespace.next;
   goto yyRecursion;
  }

  }
  break;
  case kone_word:
/* line 1891 "tcl.puma" */
  {
/* line 1892 "tcl.puma" */
   yyP62->one_word.env = new_env;
/* line 1893 "tcl.puma" */
   yyP62 = yyP62->one_word.next;
   goto yyRecursion;
  }

  case kqual_word:
/* line 1895 "tcl.puma" */
  {
/* line 1896 "tcl.puma" */
   yyP62->qual_word.env = new_env;
/* line 1897 "tcl.puma" */
   relocate (yyP62->qual_word.qualification, new_env);
/* line 1898 "tcl.puma" */
   yyP62 = yyP62->qual_word.next;
   goto yyRecursion;
  }

  case kqual_words:
/* line 1900 "tcl.puma" */
  {
/* line 1901 "tcl.puma" */
   yyP62->qual_words.env = new_env;
/* line 1902 "tcl.puma" */
   relocate (yyP62->qual_words.qualifications, new_env);
/* line 1903 "tcl.puma" */
   yyP62 = yyP62->qual_words.next;
   goto yyRecursion;
  }

  case kone_qualification:
/* line 1905 "tcl.puma" */
  {
/* line 1906 "tcl.puma" */
   yyP62->one_qualification.env = new_env;
/* line 1907 "tcl.puma" */
   relocate (yyP62->one_qualification.qualification, new_env);
/* line 1908 "tcl.puma" */
   yyP62 = yyP62->one_qualification.next;
   goto yyRecursion;
  }

  case kcharacter:
/* line 1910 "tcl.puma" */
  {
/* line 1911 "tcl.puma" */
   yyP62->character.env = new_env;
/* line 1912 "tcl.puma" */
   yyP62 = yyP62->character.next;
   goto yyRecursion;
  }

  case kident:
/* line 1914 "tcl.puma" */
  {
/* line 1915 "tcl.puma" */
   yyP62->ident.env = new_env;
/* line 1916 "tcl.puma" */
   yyP62 = yyP62->ident.next;
   goto yyRecursion;
  }

  case kblock:
/* line 1918 "tcl.puma" */
  {
/* line 1919 "tcl.puma" */
   yyP62->block.env = new_env;
/* line 1920 "tcl.puma" */
   relocate (yyP62->block.stmts, new_env);
/* line 1921 "tcl.puma" */
   yyP62 = yyP62->block.next;
   goto yyRecursion;
  }

  case kcontent:
/* line 1923 "tcl.puma" */
  {
/* line 1924 "tcl.puma" */
   yyP62->content.env = new_env;
/* line 1925 "tcl.puma" */
   relocate (yyP62->content.qualification, new_env);
/* line 1926 "tcl.puma" */
   yyP62 = yyP62->content.next;
   goto yyRecursion;
  }

  case kblock_content:
/* line 1928 "tcl.puma" */
  {
/* line 1929 "tcl.puma" */
   yyP62->block_content.env = new_env;
/* line 1930 "tcl.puma" */
   relocate (yyP62->block_content.stmts, new_env);
/* line 1931 "tcl.puma" */
   yyP62 = yyP62->block_content.next;
   goto yyRecursion;
  }

  case klocal_ident:
/* line 1933 "tcl.puma" */
  {
/* line 1934 "tcl.puma" */
   yyP62->local_ident.env = new_env;
  }
   return;

  case kglobal_ident:
/* line 1936 "tcl.puma" */
  {
/* line 1937 "tcl.puma" */
   yyP62->global_ident.env = new_env;
  }
   return;

  case klocal_text:
/* line 1939 "tcl.puma" */
  {
/* line 1940 "tcl.puma" */
   yyP62->local_text.env = new_env;
/* line 1941 "tcl.puma" */
   yyP62 = yyP62->local_text.texts;
   goto yyRecursion;
  }

  case kglobal_text:
/* line 1943 "tcl.puma" */
  {
/* line 1944 "tcl.puma" */
   yyP62->global_text.env = new_env;
/* line 1945 "tcl.puma" */
   yyP62 = yyP62->global_text.texts;
   goto yyRecursion;
  }

  case kqualification:
/* line 1947 "tcl.puma" */
  {
/* line 1948 "tcl.puma" */
   yyP62->qualification.env = new_env;
/* line 1949 "tcl.puma" */
   yyP62 = yyP62->qualification.qualification;
   goto yyRecursion;
  }

  case kcomplex_qual:
/* line 1951 "tcl.puma" */
  {
/* line 1952 "tcl.puma" */
   yyP62->complex_qual.env = new_env;
/* line 1953 "tcl.puma" */
   relocate (yyP62->complex_qual.qualification, new_env);
/* line 1954 "tcl.puma" */
   yyP62 = yyP62->complex_qual.texts;
   goto yyRecursion;
  }

  case ksubscription:
/* line 1956 "tcl.puma" */
  {
/* line 1957 "tcl.puma" */
   yyP62->subscription.env = new_env;
/* line 1958 "tcl.puma" */
   relocate (yyP62->subscription.qualification, new_env);
/* line 1959 "tcl.puma" */
   yyP62 = yyP62->subscription.index;
   goto yyRecursion;
  }

  }

;
}

void BeginTrafo ARGS ((void))
{
/* line 33 "tcl.puma" */

   yyf		= stdout;
   need_pass_2	= rfalse;
   in_string	= rfalse;
   genv		= NoTree;

}

void CloseTrafo ARGS ((void))
{
}

