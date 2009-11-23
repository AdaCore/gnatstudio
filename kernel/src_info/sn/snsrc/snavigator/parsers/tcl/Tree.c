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

#include "Tree.h"
#ifdef __cplusplus
extern "C" {
#include "Reuse.h"
#include "rSystem.h"
#include "General.h"
#include "rMemory.h"
#include "DynArray.h"
#include "Idents.h"
#include "Position.h"
}
#else
#include "Reuse.h"
#include "rSystem.h"
#include "General.h"
#include "rMemory.h"
#include "DynArray.h"
#include "Idents.h"
#include "Position.h"
#endif

char Tree_module_does_not_match_evaluator_module_7188957;
char generate_Tree_module_without_option_0;

/* line 20 "tcl.ast" */

tIdent
   ibreak, icatch, icontinue, ifor, iforeach, iglobal, iif, ireturn, iwhile,
   ibinary, iarray, iforeach, icatch, ifile, istat, ilstat, iappend, iset,
   iincr, iscan, igets, iinfo, ivwait, iparray, iexists, itkinfo, idefault,
   ilappend, iproc, iglobal, ivariable, inamespace, ieval, isource, iunset,
   iargs, ibody, irename;

tIdent last_keyword;

#ifdef __cplusplus
#ifndef yyALLOC
#define yyALLOC(size1, size2) yyALLOCi (size1, size2)
#endif
inline tTree yyALLOCi (unsigned long yysize1, unsigned long yysize2)
{ return Tree_PoolFreePtr >= Tree_PoolStartPtr + yysize1 ?
  (tTree) (Tree_PoolFreePtr -= yysize1) : Tree_Alloc (yysize2); }
inline tTree yyALLOCk (unsigned long yysize)
{ return yyALLOC (yysize, yysize); }
inline tTree yyALLOCt (unsigned long yysize)
{ return yyALLOC (yyAlignedSize (yysize), yysize); }
#else
#define yyALLOCk(size) yyALLOC (size, size)
#define yyALLOCt(size) yyALLOC (yyAlignedSize (size), size)
#ifndef yyALLOC
#define yyALLOC(size1, size2) (Tree_PoolFreePtr -= (long) size1) >= \
     Tree_PoolStartPtr ? (tTree) Tree_PoolFreePtr : Tree_Alloc (size2)
#endif
#endif
#ifndef yyFREE
#define yyFREE(ptr, size) 
#endif
#ifdef getchar
#undef getchar
#endif
#ifdef putchar
#undef putchar
#endif
#include "yyTree.h"

static void yyExit ARGS ((void)) { rExit (1); }

void (* Tree_Exit) ARGS ((void)) = yyExit;

tTree TreeRoot;
unsigned long Tree_HeapUsed = 0;

yytTree_BlockPtr Tree_BlockList	= (yytTree_BlockPtr) NoTree;
char * Tree_PoolFreePtr = (char *) & Tree_BlockList;
char * Tree_PoolStartPtr = (char *) & Tree_BlockList;
const unsigned short Tree_NodeSize [50] = { 0,
 yyAlignedSize (sizeof (yprogram)),
 yyAlignedSize (sizeof (ystmts)),
 yyAlignedSize (sizeof (ynostmt)),
 yyAlignedSize (sizeof (ystmt_c)),
 yyAlignedSize (sizeof (ystmt)),
 yyAlignedSize (sizeof (yproc_c)),
 yyAlignedSize (sizeof (yproc)),
 yyAlignedSize (sizeof (ymethod)),
 yyAlignedSize (sizeof (ybody)),
 yyAlignedSize (sizeof (yconfigbody)),
 yyAlignedSize (sizeof (yconstructor)),
 yyAlignedSize (sizeof (ydestructor)),
 yyAlignedSize (sizeof (ynamespace_c)),
 yyAlignedSize (sizeof (ynamespace)),
 yyAlignedSize (sizeof (yclass)),
 yyAlignedSize (sizeof (yitcl_class)),
 yyAlignedSize (sizeof (ywords)),
 yyAlignedSize (sizeof (ynoword)),
 yyAlignedSize (sizeof (yword_c)),
 yyAlignedSize (sizeof (yone_word)),
 yyAlignedSize (sizeof (yobj_word)),
 yyAlignedSize (sizeof (yqual_word)),
 yyAlignedSize (sizeof (yqual_words)),
 yyAlignedSize (sizeof (yqualifications)),
 yyAlignedSize (sizeof (ynoqualification)),
 yyAlignedSize (sizeof (yone_qualification)),
 yyAlignedSize (sizeof (ytexts)),
 yyAlignedSize (sizeof (ynotext)),
 yyAlignedSize (sizeof (ytext)),
 yyAlignedSize (sizeof (ycharacter)),
 yyAlignedSize (sizeof (yident)),
 yyAlignedSize (sizeof (yblock)),
 yyAlignedSize (sizeof (ycontent)),
 yyAlignedSize (sizeof (yblock_content)),
 yyAlignedSize (sizeof (yqualification_c)),
 yyAlignedSize (sizeof (ylocal_ident)),
 yyAlignedSize (sizeof (yglobal_ident)),
 yyAlignedSize (sizeof (ylocal_text)),
 yyAlignedSize (sizeof (yglobal_text)),
 yyAlignedSize (sizeof (yqualification)),
 yyAlignedSize (sizeof (ycomplex_qual)),
 yyAlignedSize (sizeof (ysubscription)),
 yyAlignedSize (sizeof (yenvs)),
 yyAlignedSize (sizeof (yenv)),
 yyAlignedSize (sizeof (yenv2)),
 yyAlignedSize (sizeof (ynoenv)),
 yyAlignedSize (sizeof (yobjects)),
 yyAlignedSize (sizeof (yobject)),
 yyAlignedSize (sizeof (ynoobject)),
};
static const Tree_tKind yyTypeRange [50] = { 0,
 kprogram,
 kitcl_class,
 knostmt,
 kitcl_class,
 kstmt,
 kdestructor,
 kproc,
 kmethod,
 kbody,
 kconfigbody,
 kconstructor,
 kdestructor,
 kitcl_class,
 knamespace,
 kclass,
 kitcl_class,
 kqual_words,
 knoword,
 kqual_words,
 kone_word,
 kobj_word,
 kqual_word,
 kqual_words,
 kone_qualification,
 knoqualification,
 kone_qualification,
 kblock_content,
 knotext,
 kblock_content,
 kcharacter,
 kident,
 kblock,
 kcontent,
 kblock_content,
 ksubscription,
 klocal_ident,
 kglobal_ident,
 klocal_text,
 kglobal_text,
 kqualification,
 kcomplex_qual,
 ksubscription,
 knoenv,
 kenv,
 kenv2,
 knoenv,
 knoobject,
 kobject,
 knoobject,
};
const char * const Tree_NodeName [50] = {
 "NoTree",
 "program",
 "stmts",
 "nostmt",
 "stmt_c",
 "stmt",
 "proc_c",
 "proc",
 "method",
 "body",
 "configbody",
 "constructor",
 "destructor",
 "namespace_c",
 "namespace",
 "class",
 "itcl_class",
 "words",
 "noword",
 "word_c",
 "one_word",
 "obj_word",
 "qual_word",
 "qual_words",
 "qualifications",
 "noqualification",
 "one_qualification",
 "texts",
 "notext",
 "text",
 "character",
 "ident",
 "block",
 "content",
 "block_content",
 "qualification_c",
 "local_ident",
 "global_ident",
 "local_text",
 "global_text",
 "qualification",
 "complex_qual",
 "subscription",
 "envs",
 "env",
 "env2",
 "noenv",
 "objects",
 "object",
 "noobject",
};

tTree Tree_Alloc
#if defined __STDC__ | defined __cplusplus
 (unsigned short yySize)
#else
 (yySize) unsigned short yySize;
#endif
{
 register yytTree_BlockPtr yyBlockPtr = Tree_BlockList;
 Tree_BlockList = (yytTree_BlockPtr) Alloc ((unsigned long) sizeof (yytTree_Block));
 if ((tTree) Tree_BlockList == NoTree) {
  (void) fprintf (stderr, "Tree: out of memory\n"); Tree_Exit ();
 }
 Tree_BlockList->yySuccessor = yyBlockPtr;
 Tree_PoolStartPtr = (char *) Tree_BlockList;
 Tree_HeapUsed += sizeof (yytTree_Block);
 return (tTree) (Tree_PoolFreePtr = Tree_PoolStartPtr +
  (yyBlockSize - yyAlignedSize (yySize)));
}

tTree MakeTree
#if defined __STDC__ | defined __cplusplus
 (Tree_tKind yyKind)
#else
 (yyKind) Tree_tKind yyKind;
#endif
{
 register tTree yyt = yyALLOCk (Tree_NodeSize [yyKind]);
 Tree_InitHead (yyt, yyKind)
 return yyt;
}

rbool Tree_IsType
#if defined __STDC__ | defined __cplusplus
 (register tTree yyt, register Tree_tKind yyKind)
#else
 (yyt, yyKind) register tTree yyt; register Tree_tKind yyKind;
#endif
{
 return yyt != NoTree && yyKind <= yyt->Kind &&
  yyt->Kind <= yyTypeRange [yyKind];
}


tTree mprogram
#if defined __STDC__ | defined __cplusplus
(tTree pstmts)
#else
(pstmts)
tTree pstmts;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yprogram));
 Tree_InitHead (yyt, kprogram)
 yyt->program.stmts = pstmts;
 return yyt;
}

tTree mstmts
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystmts));
 Tree_InitHead (yyt, kstmts)
 begintTree(yyt->stmts.env)
 return yyt;
}

tTree mnostmt
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynostmt));
 Tree_InitHead (yyt, knostmt)
 begintTree(yyt->nostmt.env)
 return yyt;
}

tTree mstmt_c
#if defined __STDC__ | defined __cplusplus
(tTree pnext)
#else
(pnext)
tTree pnext;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystmt_c));
 Tree_InitHead (yyt, kstmt_c)
 begintTree(yyt->stmt_c.env)
 yyt->stmt_c.next = pnext;
 return yyt;
}

tTree mstmt
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pwords)
#else
(pnext, pwords)
tTree pnext;
tTree pwords;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystmt));
 Tree_InitHead (yyt, kstmt)
 begintTree(yyt->stmt.env)
 yyt->stmt.next = pnext;
 yyt->stmt.words = pwords;
 return yyt;
}

tTree mproc_c
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yproc_c));
 Tree_InitHead (yyt, kproc_c)
 begintTree(yyt->proc_c.env)
 yyt->proc_c.next = pnext;
 yyt->proc_c.qualification = pqualification;
 yyt->proc_c.param_names = pparam_names;
 yyt->proc_c.parameter = pparameter;
 yyt->proc_c.block = pblock;
 yyt->proc_c.epos = pepos;
 yyt->proc_c.attribute = pattribute;
 return yyt;
}

tTree mproc
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yproc));
 Tree_InitHead (yyt, kproc)
 begintTree(yyt->proc.env)
 yyt->proc.next = pnext;
 yyt->proc.qualification = pqualification;
 yyt->proc.param_names = pparam_names;
 yyt->proc.parameter = pparameter;
 yyt->proc.block = pblock;
 yyt->proc.epos = pepos;
 yyt->proc.attribute = pattribute;
 return yyt;
}

tTree mmethod
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ymethod));
 Tree_InitHead (yyt, kmethod)
 begintTree(yyt->method.env)
 yyt->method.next = pnext;
 yyt->method.qualification = pqualification;
 yyt->method.param_names = pparam_names;
 yyt->method.parameter = pparameter;
 yyt->method.block = pblock;
 yyt->method.epos = pepos;
 yyt->method.attribute = pattribute;
 return yyt;
}

tTree mbody
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ybody));
 Tree_InitHead (yyt, kbody)
 begintTree(yyt->body.env)
 yyt->body.next = pnext;
 yyt->body.qualification = pqualification;
 yyt->body.param_names = pparam_names;
 yyt->body.parameter = pparameter;
 yyt->body.block = pblock;
 yyt->body.epos = pepos;
 yyt->body.attribute = pattribute;
 return yyt;
}

tTree mconfigbody
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yconfigbody));
 Tree_InitHead (yyt, kconfigbody)
 begintTree(yyt->configbody.env)
 yyt->configbody.next = pnext;
 yyt->configbody.qualification = pqualification;
 yyt->configbody.param_names = pparam_names;
 yyt->configbody.parameter = pparameter;
 yyt->configbody.block = pblock;
 yyt->configbody.epos = pepos;
 yyt->configbody.attribute = pattribute;
 return yyt;
}

tTree mconstructor
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute, tTree pinit)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute, pinit)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
tTree pinit;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yconstructor));
 Tree_InitHead (yyt, kconstructor)
 begintTree(yyt->constructor.env)
 yyt->constructor.next = pnext;
 yyt->constructor.qualification = pqualification;
 yyt->constructor.param_names = pparam_names;
 yyt->constructor.parameter = pparameter;
 yyt->constructor.block = pblock;
 yyt->constructor.epos = pepos;
 yyt->constructor.attribute = pattribute;
 yyt->constructor.init = pinit;
 return yyt;
}

tTree mdestructor
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pparam_names, pparameter, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pparam_names;
tTree pparameter;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ydestructor));
 Tree_InitHead (yyt, kdestructor)
 begintTree(yyt->destructor.env)
 yyt->destructor.next = pnext;
 yyt->destructor.qualification = pqualification;
 yyt->destructor.param_names = pparam_names;
 yyt->destructor.parameter = pparameter;
 yyt->destructor.block = pblock;
 yyt->destructor.epos = pepos;
 yyt->destructor.attribute = pattribute;
 return yyt;
}

tTree mnamespace_c
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynamespace_c));
 Tree_InitHead (yyt, knamespace_c)
 begintTree(yyt->namespace_c.env)
 yyt->namespace_c.next = pnext;
 yyt->namespace_c.qualification = pqualification;
 yyt->namespace_c.block = pblock;
 yyt->namespace_c.epos = pepos;
 yyt->namespace_c.attribute = pattribute;
 return yyt;
}

tTree mnamespace
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynamespace));
 Tree_InitHead (yyt, knamespace)
 begintTree(yyt->namespace.env)
 yyt->namespace.next = pnext;
 yyt->namespace.qualification = pqualification;
 yyt->namespace.block = pblock;
 yyt->namespace.epos = pepos;
 yyt->namespace.attribute = pattribute;
 return yyt;
}

tTree mclass
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yclass));
 Tree_InitHead (yyt, kclass)
 begintTree(yyt->class.env)
 yyt->class.next = pnext;
 yyt->class.qualification = pqualification;
 yyt->class.block = pblock;
 yyt->class.epos = pepos;
 yyt->class.attribute = pattribute;
 return yyt;
}

tTree mitcl_class
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute)
#else
(pnext, pqualification, pblock, pepos, pattribute)
tTree pnext;
tTree pqualification;
tTree pblock;
tPosition pepos;
int pattribute;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yitcl_class));
 Tree_InitHead (yyt, kitcl_class)
 begintTree(yyt->itcl_class.env)
 yyt->itcl_class.next = pnext;
 yyt->itcl_class.qualification = pqualification;
 yyt->itcl_class.block = pblock;
 yyt->itcl_class.epos = pepos;
 yyt->itcl_class.attribute = pattribute;
 return yyt;
}

tTree mwords
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ywords));
 Tree_InitHead (yyt, kwords)
 begintTree(yyt->words.env)
 return yyt;
}

tTree mnoword
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoword));
 Tree_InitHead (yyt, knoword)
 begintTree(yyt->noword.env)
 return yyt;
}

tTree mword_c
#if defined __STDC__ | defined __cplusplus
(tTree pnext)
#else
(pnext)
tTree pnext;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yword_c));
 Tree_InitHead (yyt, kword_c)
 begintTree(yyt->word_c.env)
 yyt->word_c.next = pnext;
 return yyt;
}

tTree mone_word
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tIdent pident, tPosition ppos)
#else
(pnext, pident, ppos)
tTree pnext;
tIdent pident;
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yone_word));
 Tree_InitHead (yyt, kone_word)
 begintTree(yyt->one_word.env)
 yyt->one_word.next = pnext;
 yyt->one_word.ident = pident;
 yyt->one_word.pos = ppos;
 return yyt;
}

tTree mobj_word
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tIdent pident, tPosition ppos, tTree pobject)
#else
(pnext, pident, ppos, pobject)
tTree pnext;
tIdent pident;
tPosition ppos;
tTree pobject;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yobj_word));
 Tree_InitHead (yyt, kobj_word)
 begintTree(yyt->obj_word.env)
 yyt->obj_word.next = pnext;
 yyt->obj_word.ident = pident;
 yyt->obj_word.pos = ppos;
 yyt->obj_word.object = pobject;
 return yyt;
}

tTree mqual_word
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification)
#else
(pnext, pqualification)
tTree pnext;
tTree pqualification;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqual_word));
 Tree_InitHead (yyt, kqual_word)
 begintTree(yyt->qual_word.env)
 yyt->qual_word.next = pnext;
 yyt->qual_word.qualification = pqualification;
 return yyt;
}

tTree mqual_words
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualifications)
#else
(pnext, pqualifications)
tTree pnext;
tTree pqualifications;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqual_words));
 Tree_InitHead (yyt, kqual_words)
 begintTree(yyt->qual_words.env)
 yyt->qual_words.next = pnext;
 yyt->qual_words.qualifications = pqualifications;
 return yyt;
}

tTree mqualifications
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqualifications));
 Tree_InitHead (yyt, kqualifications)
 begintTree(yyt->qualifications.env)
 return yyt;
}

tTree mnoqualification
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoqualification));
 Tree_InitHead (yyt, knoqualification)
 begintTree(yyt->noqualification.env)
 return yyt;
}

tTree mone_qualification
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualification)
#else
(pnext, pqualification)
tTree pnext;
tTree pqualification;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yone_qualification));
 Tree_InitHead (yyt, kone_qualification)
 begintTree(yyt->one_qualification.env)
 yyt->one_qualification.next = pnext;
 yyt->one_qualification.qualification = pqualification;
 return yyt;
}

tTree mtexts
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytexts));
 Tree_InitHead (yyt, ktexts)
 begintTree(yyt->texts.env)
 return yyt;
}

tTree mnotext
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynotext));
 Tree_InitHead (yyt, knotext)
 begintTree(yyt->notext.env)
 return yyt;
}

tTree mtext
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos)
#else
(pnext, ppos)
tTree pnext;
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytext));
 Tree_InitHead (yyt, ktext)
 begintTree(yyt->text.env)
 yyt->text.next = pnext;
 yyt->text.pos = ppos;
 return yyt;
}

tTree mcharacter
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos, char ptext)
#else
(pnext, ppos, ptext)
tTree pnext;
tPosition ppos;
char ptext;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycharacter));
 Tree_InitHead (yyt, kcharacter)
 begintTree(yyt->character.env)
 yyt->character.next = pnext;
 yyt->character.pos = ppos;
 yyt->character.text = ptext;
 return yyt;
}

tTree mident
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos, tIdent pident)
#else
(pnext, ppos, pident)
tTree pnext;
tPosition ppos;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yident));
 Tree_InitHead (yyt, kident)
 begintTree(yyt->ident.env)
 yyt->ident.next = pnext;
 yyt->ident.pos = ppos;
 yyt->ident.ident = pident;
 return yyt;
}

tTree mblock
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos, tPosition pepos, tTree pstmts, char ptype)
#else
(pnext, ppos, pepos, pstmts, ptype)
tTree pnext;
tPosition ppos;
tPosition pepos;
tTree pstmts;
char ptype;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yblock));
 Tree_InitHead (yyt, kblock)
 begintTree(yyt->block.env)
 yyt->block.next = pnext;
 yyt->block.pos = ppos;
 yyt->block.epos = pepos;
 yyt->block.stmts = pstmts;
 yyt->block.type = ptype;
 return yyt;
}

tTree mcontent
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos, tTree pqualification)
#else
(pnext, ppos, pqualification)
tTree pnext;
tPosition ppos;
tTree pqualification;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycontent));
 Tree_InitHead (yyt, kcontent)
 begintTree(yyt->content.env)
 yyt->content.next = pnext;
 yyt->content.pos = ppos;
 yyt->content.qualification = pqualification;
 return yyt;
}

tTree mblock_content
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tPosition ppos, tPosition pepos, tTree pstmts)
#else
(pnext, ppos, pepos, pstmts)
tTree pnext;
tPosition ppos;
tPosition pepos;
tTree pstmts;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yblock_content));
 Tree_InitHead (yyt, kblock_content)
 begintTree(yyt->block_content.env)
 yyt->block_content.next = pnext;
 yyt->block_content.pos = ppos;
 yyt->block_content.epos = pepos;
 yyt->block_content.stmts = pstmts;
 return yyt;
}

tTree mqualification_c
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqualification_c));
 Tree_InitHead (yyt, kqualification_c)
 yyt->qualification_c.pos = ppos;
 begintTree(yyt->qualification_c.env)
 return yyt;
}

tTree mlocal_ident
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tIdent pident)
#else
(ppos, pident)
tPosition ppos;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ylocal_ident));
 Tree_InitHead (yyt, klocal_ident)
 yyt->local_ident.pos = ppos;
 begintTree(yyt->local_ident.env)
 yyt->local_ident.ident = pident;
 return yyt;
}

tTree mglobal_ident
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tIdent pident)
#else
(ppos, pident)
tPosition ppos;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yglobal_ident));
 Tree_InitHead (yyt, kglobal_ident)
 yyt->global_ident.pos = ppos;
 begintTree(yyt->global_ident.env)
 yyt->global_ident.ident = pident;
 return yyt;
}

tTree mlocal_text
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptexts)
#else
(ppos, ptexts)
tPosition ppos;
tTree ptexts;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ylocal_text));
 Tree_InitHead (yyt, klocal_text)
 yyt->local_text.pos = ppos;
 begintTree(yyt->local_text.env)
 yyt->local_text.texts = ptexts;
 return yyt;
}

tTree mglobal_text
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptexts)
#else
(ppos, ptexts)
tPosition ppos;
tTree ptexts;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yglobal_text));
 Tree_InitHead (yyt, kglobal_text)
 yyt->global_text.pos = ppos;
 begintTree(yyt->global_text.env)
 yyt->global_text.texts = ptexts;
 return yyt;
}

tTree mqualification
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pqualification, tIdent pident)
#else
(ppos, pqualification, pident)
tPosition ppos;
tTree pqualification;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqualification));
 Tree_InitHead (yyt, kqualification)
 yyt->qualification.pos = ppos;
 begintTree(yyt->qualification.env)
 yyt->qualification.qualification = pqualification;
 yyt->qualification.ident = pident;
 return yyt;
}

tTree mcomplex_qual
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pqualification, tTree ptexts)
#else
(ppos, pqualification, ptexts)
tPosition ppos;
tTree pqualification;
tTree ptexts;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycomplex_qual));
 Tree_InitHead (yyt, kcomplex_qual)
 yyt->complex_qual.pos = ppos;
 begintTree(yyt->complex_qual.env)
 yyt->complex_qual.qualification = pqualification;
 yyt->complex_qual.texts = ptexts;
 return yyt;
}

tTree msubscription
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pqualification, tTree pindex)
#else
(ppos, pqualification, pindex)
tPosition ppos;
tTree pqualification;
tTree pindex;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ysubscription));
 Tree_InitHead (yyt, ksubscription)
 yyt->subscription.pos = ppos;
 begintTree(yyt->subscription.env)
 yyt->subscription.qualification = pqualification;
 yyt->subscription.index = pindex;
 return yyt;
}

tTree menvs
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yenvs));
 Tree_InitHead (yyt, kenvs)
 return yyt;
}

tTree menv
#if defined __STDC__ | defined __cplusplus
(tTree pobjects, tTree penv, tTree pobject)
#else
(pobjects, penv, pobject)
tTree pobjects;
tTree penv;
tTree pobject;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yenv));
 Tree_InitHead (yyt, kenv)
 yyt->env.objects = pobjects;
 yyt->env.env = penv;
 yyt->env.object = pobject;
 begintHashTable(yyt->env.HashTable)
 beginint(yyt->env.HashSize)
 return yyt;
}

tTree menv2
#if defined __STDC__ | defined __cplusplus
(tTree penv1, tTree penv2)
#else
(penv1, penv2)
tTree penv1;
tTree penv2;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yenv2));
 Tree_InitHead (yyt, kenv2)
 yyt->env2.env1 = penv1;
 yyt->env2.env2 = penv2;
 return yyt;
}

tTree mnoenv
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoenv));
 Tree_InitHead (yyt, knoenv)
 return yyt;
}

tTree mobjects
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yobjects));
 Tree_InitHead (yyt, kobjects)
 return yyt;
}

tTree mobject
#if defined __STDC__ | defined __cplusplus
(tTree pobject, tIdent pident, tTree pnext)
#else
(pobject, pident, pnext)
tTree pobject;
tIdent pident;
tTree pnext;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yobject));
 Tree_InitHead (yyt, kobject)
 yyt->object.object = pobject;
 yyt->object.ident = pident;
 yyt->object.next = pnext;
 return yyt;
}

tTree mnoobject
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoobject));
 Tree_InitHead (yyt, knoobject)
 return yyt;
}

static FILE * yyf;

static void yyMark
#if defined __STDC__ | defined __cplusplus
 (register tTree yyt)
#else
 (yyt) register tTree yyt;
#endif
{
 for (;;) {
  if (yyt == NoTree || ++ yyt->yyHead.yyMark > 1) return;

  switch (yyt->Kind) {
case kprogram:
yyt = yyt->program.stmts; break;
case kstmt_c:
yyt = yyt->stmt_c.next; break;
case kstmt:
yyMark (yyt->stmt.words);
yyt = yyt->stmt.next; break;
case kproc_c:
yyMark (yyt->proc_c.qualification);
yyMark (yyt->proc_c.param_names);
yyMark (yyt->proc_c.parameter);
yyMark (yyt->proc_c.block);
yyt = yyt->proc_c.next; break;
case kproc:
yyMark (yyt->proc.qualification);
yyMark (yyt->proc.param_names);
yyMark (yyt->proc.parameter);
yyMark (yyt->proc.block);
yyt = yyt->proc.next; break;
case kmethod:
yyMark (yyt->method.qualification);
yyMark (yyt->method.param_names);
yyMark (yyt->method.parameter);
yyMark (yyt->method.block);
yyt = yyt->method.next; break;
case kbody:
yyMark (yyt->body.qualification);
yyMark (yyt->body.param_names);
yyMark (yyt->body.parameter);
yyMark (yyt->body.block);
yyt = yyt->body.next; break;
case kconfigbody:
yyMark (yyt->configbody.qualification);
yyMark (yyt->configbody.param_names);
yyMark (yyt->configbody.parameter);
yyMark (yyt->configbody.block);
yyt = yyt->configbody.next; break;
case kconstructor:
yyMark (yyt->constructor.qualification);
yyMark (yyt->constructor.param_names);
yyMark (yyt->constructor.parameter);
yyMark (yyt->constructor.block);
yyMark (yyt->constructor.init);
yyt = yyt->constructor.next; break;
case kdestructor:
yyMark (yyt->destructor.qualification);
yyMark (yyt->destructor.param_names);
yyMark (yyt->destructor.parameter);
yyMark (yyt->destructor.block);
yyt = yyt->destructor.next; break;
case knamespace_c:
yyMark (yyt->namespace_c.qualification);
yyMark (yyt->namespace_c.block);
yyt = yyt->namespace_c.next; break;
case knamespace:
yyMark (yyt->namespace.qualification);
yyMark (yyt->namespace.block);
yyt = yyt->namespace.next; break;
case kclass:
yyMark (yyt->class.qualification);
yyMark (yyt->class.block);
yyt = yyt->class.next; break;
case kitcl_class:
yyMark (yyt->itcl_class.qualification);
yyMark (yyt->itcl_class.block);
yyt = yyt->itcl_class.next; break;
case kword_c:
yyt = yyt->word_c.next; break;
case kone_word:
yyt = yyt->one_word.next; break;
case kobj_word:
yyt = yyt->obj_word.next; break;
case kqual_word:
yyMark (yyt->qual_word.qualification);
yyt = yyt->qual_word.next; break;
case kqual_words:
yyMark (yyt->qual_words.qualifications);
yyt = yyt->qual_words.next; break;
case kone_qualification:
yyMark (yyt->one_qualification.qualification);
yyt = yyt->one_qualification.next; break;
case ktext:
yyt = yyt->text.next; break;
case kcharacter:
yyt = yyt->character.next; break;
case kident:
yyt = yyt->ident.next; break;
case kblock:
yyMark (yyt->block.stmts);
yyt = yyt->block.next; break;
case kcontent:
yyMark (yyt->content.qualification);
yyt = yyt->content.next; break;
case kblock_content:
yyMark (yyt->block_content.stmts);
yyt = yyt->block_content.next; break;
case klocal_text:
yyt = yyt->local_text.texts; break;
case kglobal_text:
yyt = yyt->global_text.texts; break;
case kqualification:
yyt = yyt->qualification.qualification; break;
case kcomplex_qual:
yyMark (yyt->complex_qual.qualification);
yyt = yyt->complex_qual.texts; break;
case ksubscription:
yyMark (yyt->subscription.qualification);
yyt = yyt->subscription.index; break;
case kenv:
yyMark (yyt->env.objects);
yyt = yyt->env.env; break;
case kenv2:
yyMark (yyt->env2.env1);
yyt = yyt->env2.env2; break;
case kobject:
yyt = yyt->object.next; break;
  default: return;
  }
 }
}

static void yyWriteTree ARGS ((tTree yyt));

static void yyWriteNl ARGS ((void)) { (void) putc ('\n', yyf); }

static void yyWriteSelector
#if defined __STDC__ | defined __cplusplus
 (char * yys)
#else
 (yys) char * yys;
#endif
{
 register int yyi = 16 - strlen (yys);
 (void) fputs (yys, yyf);
 while (yyi -- > 0) (void) putc (' ', yyf);
 (void) fputs (" = ", yyf);
}

static void yyWriteHex
#if defined __STDC__ | defined __cplusplus
 (unsigned char * yyx, int yysize)
#else
 (yyx, yysize) unsigned char * yyx; int yysize;
#endif
{ register int yyi; for (yyi = 0; yyi < yysize; yyi ++)
   (void) fprintf (yyf, "%02x ", yyx [yyi]); }

static void yyWriteAdr
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 if (yyt == NoTree) (void) fputs ("NoTree", yyf);
 else (void) fprintf (yyf, "%08lx *", (unsigned long) yyt);
 yyWriteNl ();
}

static void yWriteNodeprogram
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("stmts");
 yyWriteAdr (yyt->program.stmts);
}

static void yWriteNodestmts
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->stmts.env) yyWriteNl ();
}

static void yWriteNodenostmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestmts (yyt);
}

static void yWriteNodestmt_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestmts (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->stmt_c.next);
}

static void yWriteNodestmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestmt_c (yyt);
 yyWriteSelector ("words");
 yyWriteAdr (yyt->stmt.words);
}

static void yWriteNodeproc_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestmt_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->proc_c.qualification);
 yyWriteSelector ("param_names");
 yyWriteAdr (yyt->proc_c.param_names);
 yyWriteSelector ("parameter");
 yyWriteAdr (yyt->proc_c.parameter);
 yyWriteSelector ("block");
 yyWriteAdr (yyt->proc_c.block);
 yyWriteSelector ("epos");
 writetPosition (yyt->proc_c.epos) yyWriteNl ();
 yyWriteSelector ("attribute");
 writeint (yyt->proc_c.attribute) yyWriteNl ();
}

static void yWriteNodeproc
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
}

static void yWriteNodemethod
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
}

static void yWriteNodebody
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
}

static void yWriteNodeconfigbody
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
}

static void yWriteNodeconstructor
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
 yyWriteSelector ("init");
 yyWriteAdr (yyt->constructor.init);
}

static void yWriteNodedestructor
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeproc_c (yyt);
}

static void yWriteNodenamespace_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestmt_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->namespace_c.qualification);
 yyWriteSelector ("block");
 yyWriteAdr (yyt->namespace_c.block);
 yyWriteSelector ("epos");
 writetPosition (yyt->namespace_c.epos) yyWriteNl ();
 yyWriteSelector ("attribute");
 writeint (yyt->namespace_c.attribute) yyWriteNl ();
}

static void yWriteNodenamespace
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodenamespace_c (yyt);
}

static void yWriteNodeclass
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodenamespace_c (yyt);
}

static void yWriteNodeitcl_class
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodenamespace_c (yyt);
}

static void yWriteNodewords
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->words.env) yyWriteNl ();
}

static void yWriteNodenoword
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodewords (yyt);
}

static void yWriteNodeword_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodewords (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->word_c.next);
}

static void yWriteNodeone_word
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeword_c (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->one_word.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->one_word.pos) yyWriteNl ();
}

static void yWriteNodeobj_word
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeword_c (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->obj_word.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->obj_word.pos) yyWriteNl ();
 yyWriteSelector ("object");
 writetTree (yyt->obj_word.object) yyWriteNl ();
}

static void yWriteNodequal_word
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeword_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->qual_word.qualification);
}

static void yWriteNodequal_words
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeword_c (yyt);
 yyWriteSelector ("qualifications");
 yyWriteAdr (yyt->qual_words.qualifications);
}

static void yWriteNodequalifications
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->qualifications.env) yyWriteNl ();
}

static void yWriteNodenoqualification
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalifications (yyt);
}

static void yWriteNodeone_qualification
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalifications (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->one_qualification.next);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->one_qualification.qualification);
}

static void yWriteNodetexts
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->texts.env) yyWriteNl ();
}

static void yWriteNodenotext
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetexts (yyt);
}

static void yWriteNodetext
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetexts (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->text.next);
 yyWriteSelector ("pos");
 writetPosition (yyt->text.pos) yyWriteNl ();
}

static void yWriteNodecharacter
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetext (yyt);
 yyWriteSelector ("text");
 writechar (yyt->character.text) yyWriteNl ();
}

static void yWriteNodeident
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetext (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->ident.ident) yyWriteNl ();
}

static void yWriteNodeblock
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetext (yyt);
 yyWriteSelector ("epos");
 writetPosition (yyt->block.epos) yyWriteNl ();
 yyWriteSelector ("stmts");
 yyWriteAdr (yyt->block.stmts);
 yyWriteSelector ("type");
 writechar (yyt->block.type) yyWriteNl ();
}

static void yWriteNodecontent
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetext (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->content.qualification);
}

static void yWriteNodeblock_content
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetext (yyt);
 yyWriteSelector ("epos");
 writetPosition (yyt->block_content.epos) yyWriteNl ();
 yyWriteSelector ("stmts");
 yyWriteAdr (yyt->block_content.stmts);
}

static void yWriteNodequalification_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("pos");
 writetPosition (yyt->qualification_c.pos) yyWriteNl ();
 yyWriteSelector ("env");
 writetTree (yyt->qualification_c.env) yyWriteNl ();
}

static void yWriteNodelocal_ident
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->local_ident.ident) yyWriteNl ();
}

static void yWriteNodeglobal_ident
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->global_ident.ident) yyWriteNl ();
}

static void yWriteNodelocal_text
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("texts");
 yyWriteAdr (yyt->local_text.texts);
}

static void yWriteNodeglobal_text
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("texts");
 yyWriteAdr (yyt->global_text.texts);
}

static void yWriteNodequalification
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->qualification.qualification);
 yyWriteSelector ("ident");
 writetIdent (yyt->qualification.ident) yyWriteNl ();
}

static void yWriteNodecomplex_qual
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->complex_qual.qualification);
 yyWriteSelector ("texts");
 yyWriteAdr (yyt->complex_qual.texts);
}

static void yWriteNodesubscription
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalification_c (yyt);
 yyWriteSelector ("qualification");
 yyWriteAdr (yyt->subscription.qualification);
 yyWriteSelector ("index");
 yyWriteAdr (yyt->subscription.index);
}

static void yWriteNodeenv
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("objects");
 yyWriteAdr (yyt->env.objects);
 yyWriteSelector ("env");
 yyWriteAdr (yyt->env.env);
 yyWriteSelector ("object");
 writetTree (yyt->env.object) yyWriteNl ();
 yyWriteSelector ("HashTable");
 writetHashTable (yyt->env.HashTable) yyWriteNl ();
 yyWriteSelector ("HashSize");
 writeint (yyt->env.HashSize) yyWriteNl ();
}

static void yWriteNodeenv2
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env1");
 yyWriteAdr (yyt->env2.env1);
 yyWriteSelector ("env2");
 yyWriteAdr (yyt->env2.env2);
}

static void yWriteNodeobject
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("object");
 writetTree (yyt->object.object) yyWriteNl ();
 yyWriteSelector ("ident");
 writetIdent (yyt->object.ident) yyWriteNl ();
 yyWriteSelector ("next");
 yyWriteAdr (yyt->object.next);
}

void WriteTreeNode
#if defined __STDC__ | defined __cplusplus
 (FILE * yyyf, tTree yyt)
#else
 (yyyf, yyt) FILE * yyyf; tTree yyt;
#endif
{
 yyf = yyyf;
 if (yyt == NoTree) { (void) fputs ("NoTree\n", yyf); return; }

 (void) fputs (Tree_NodeName [yyt->Kind], yyf); (void) fputc ('\n', yyf);
 writeNodeHead (yyt)
 switch (yyt->Kind) {
case kprogram:
 yWriteNodeprogram (yyt); break;
case kstmts:
 yWriteNodestmts (yyt); break;
case knostmt:
 yWriteNodenostmt (yyt); break;
case kstmt_c:
 yWriteNodestmt_c (yyt); break;
case kstmt:
 yWriteNodestmt (yyt); break;
case kproc_c:
 yWriteNodeproc_c (yyt); break;
case kproc:
 yWriteNodeproc (yyt); break;
case kmethod:
 yWriteNodemethod (yyt); break;
case kbody:
 yWriteNodebody (yyt); break;
case kconfigbody:
 yWriteNodeconfigbody (yyt); break;
case kconstructor:
 yWriteNodeconstructor (yyt); break;
case kdestructor:
 yWriteNodedestructor (yyt); break;
case knamespace_c:
 yWriteNodenamespace_c (yyt); break;
case knamespace:
 yWriteNodenamespace (yyt); break;
case kclass:
 yWriteNodeclass (yyt); break;
case kitcl_class:
 yWriteNodeitcl_class (yyt); break;
case kwords:
 yWriteNodewords (yyt); break;
case knoword:
 yWriteNodenoword (yyt); break;
case kword_c:
 yWriteNodeword_c (yyt); break;
case kone_word:
 yWriteNodeone_word (yyt); break;
case kobj_word:
 yWriteNodeobj_word (yyt); break;
case kqual_word:
 yWriteNodequal_word (yyt); break;
case kqual_words:
 yWriteNodequal_words (yyt); break;
case kqualifications:
 yWriteNodequalifications (yyt); break;
case knoqualification:
 yWriteNodenoqualification (yyt); break;
case kone_qualification:
 yWriteNodeone_qualification (yyt); break;
case ktexts:
 yWriteNodetexts (yyt); break;
case knotext:
 yWriteNodenotext (yyt); break;
case ktext:
 yWriteNodetext (yyt); break;
case kcharacter:
 yWriteNodecharacter (yyt); break;
case kident:
 yWriteNodeident (yyt); break;
case kblock:
 yWriteNodeblock (yyt); break;
case kcontent:
 yWriteNodecontent (yyt); break;
case kblock_content:
 yWriteNodeblock_content (yyt); break;
case kqualification_c:
 yWriteNodequalification_c (yyt); break;
case klocal_ident:
 yWriteNodelocal_ident (yyt); break;
case kglobal_ident:
 yWriteNodeglobal_ident (yyt); break;
case klocal_text:
 yWriteNodelocal_text (yyt); break;
case kglobal_text:
 yWriteNodeglobal_text (yyt); break;
case kqualification:
 yWriteNodequalification (yyt); break;
case kcomplex_qual:
 yWriteNodecomplex_qual (yyt); break;
case ksubscription:
 yWriteNodesubscription (yyt); break;
case kenv:
 yWriteNodeenv (yyt); break;
case kenv2:
 yWriteNodeenv2 (yyt); break;
case kobject:
 yWriteNodeobject (yyt); break;
 default: ;
 }
}

#define yyNil	(unsigned char) 0374
#define yyNoLabel	(unsigned char) 0375
#define yyLabelDef	(unsigned char) 0376
#define yyLabelUse	(unsigned char) 0377

void ReleaseTreeModule ARGS ((void))
{
 yytTree_BlockPtr yyBlockPtr;
 while (Tree_BlockList != (yytTree_BlockPtr) NoTree) {
  yyBlockPtr = Tree_BlockList;
  Tree_BlockList = Tree_BlockList->yySuccessor;
  Free ((unsigned long) sizeof (yytTree_Block), (char *) yyBlockPtr);
 }
 Tree_PoolFreePtr = (char *) & Tree_BlockList;
 Tree_PoolStartPtr = (char *) & Tree_BlockList;
 Tree_HeapUsed = 0;
}

static Tree_tProcTree yyProc;

static void yyTraverseTreeTD
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 for (;;) {
  if (yyt == NoTree || yyt->yyHead.yyMark == 0) return;
  yyProc (yyt);
  yyt->yyHead.yyMark = 0;

  switch (yyt->Kind) {
case kprogram:
yyt = yyt->program.stmts; break;
case kstmt_c:
yyt = yyt->stmt_c.next; break;
case kstmt:
yyTraverseTreeTD (yyt->stmt.words);
yyt = yyt->stmt.next; break;
case kproc_c:
yyTraverseTreeTD (yyt->proc_c.qualification);
yyTraverseTreeTD (yyt->proc_c.param_names);
yyTraverseTreeTD (yyt->proc_c.parameter);
yyTraverseTreeTD (yyt->proc_c.block);
yyt = yyt->proc_c.next; break;
case kproc:
yyTraverseTreeTD (yyt->proc.qualification);
yyTraverseTreeTD (yyt->proc.param_names);
yyTraverseTreeTD (yyt->proc.parameter);
yyTraverseTreeTD (yyt->proc.block);
yyt = yyt->proc.next; break;
case kmethod:
yyTraverseTreeTD (yyt->method.qualification);
yyTraverseTreeTD (yyt->method.param_names);
yyTraverseTreeTD (yyt->method.parameter);
yyTraverseTreeTD (yyt->method.block);
yyt = yyt->method.next; break;
case kbody:
yyTraverseTreeTD (yyt->body.qualification);
yyTraverseTreeTD (yyt->body.param_names);
yyTraverseTreeTD (yyt->body.parameter);
yyTraverseTreeTD (yyt->body.block);
yyt = yyt->body.next; break;
case kconfigbody:
yyTraverseTreeTD (yyt->configbody.qualification);
yyTraverseTreeTD (yyt->configbody.param_names);
yyTraverseTreeTD (yyt->configbody.parameter);
yyTraverseTreeTD (yyt->configbody.block);
yyt = yyt->configbody.next; break;
case kconstructor:
yyTraverseTreeTD (yyt->constructor.qualification);
yyTraverseTreeTD (yyt->constructor.param_names);
yyTraverseTreeTD (yyt->constructor.parameter);
yyTraverseTreeTD (yyt->constructor.block);
yyTraverseTreeTD (yyt->constructor.init);
yyt = yyt->constructor.next; break;
case kdestructor:
yyTraverseTreeTD (yyt->destructor.qualification);
yyTraverseTreeTD (yyt->destructor.param_names);
yyTraverseTreeTD (yyt->destructor.parameter);
yyTraverseTreeTD (yyt->destructor.block);
yyt = yyt->destructor.next; break;
case knamespace_c:
yyTraverseTreeTD (yyt->namespace_c.qualification);
yyTraverseTreeTD (yyt->namespace_c.block);
yyt = yyt->namespace_c.next; break;
case knamespace:
yyTraverseTreeTD (yyt->namespace.qualification);
yyTraverseTreeTD (yyt->namespace.block);
yyt = yyt->namespace.next; break;
case kclass:
yyTraverseTreeTD (yyt->class.qualification);
yyTraverseTreeTD (yyt->class.block);
yyt = yyt->class.next; break;
case kitcl_class:
yyTraverseTreeTD (yyt->itcl_class.qualification);
yyTraverseTreeTD (yyt->itcl_class.block);
yyt = yyt->itcl_class.next; break;
case kword_c:
yyt = yyt->word_c.next; break;
case kone_word:
yyt = yyt->one_word.next; break;
case kobj_word:
yyt = yyt->obj_word.next; break;
case kqual_word:
yyTraverseTreeTD (yyt->qual_word.qualification);
yyt = yyt->qual_word.next; break;
case kqual_words:
yyTraverseTreeTD (yyt->qual_words.qualifications);
yyt = yyt->qual_words.next; break;
case kone_qualification:
yyTraverseTreeTD (yyt->one_qualification.qualification);
yyt = yyt->one_qualification.next; break;
case ktext:
yyt = yyt->text.next; break;
case kcharacter:
yyt = yyt->character.next; break;
case kident:
yyt = yyt->ident.next; break;
case kblock:
yyTraverseTreeTD (yyt->block.stmts);
yyt = yyt->block.next; break;
case kcontent:
yyTraverseTreeTD (yyt->content.qualification);
yyt = yyt->content.next; break;
case kblock_content:
yyTraverseTreeTD (yyt->block_content.stmts);
yyt = yyt->block_content.next; break;
case klocal_text:
yyt = yyt->local_text.texts; break;
case kglobal_text:
yyt = yyt->global_text.texts; break;
case kqualification:
yyt = yyt->qualification.qualification; break;
case kcomplex_qual:
yyTraverseTreeTD (yyt->complex_qual.qualification);
yyt = yyt->complex_qual.texts; break;
case ksubscription:
yyTraverseTreeTD (yyt->subscription.qualification);
yyt = yyt->subscription.index; break;
case kenv:
yyTraverseTreeTD (yyt->env.objects);
yyt = yyt->env.env; break;
case kenv2:
yyTraverseTreeTD (yyt->env2.env1);
yyt = yyt->env2.env2; break;
case kobject:
yyt = yyt->object.next; break;
  default: return;
  }
 }
}

void TraverseTreeTD
#if defined __STDC__ | defined __cplusplus
 (tTree yyt, Tree_tProcTree yyyProc)
#else
 (yyt, yyyProc) tTree yyt; Tree_tProcTree yyyProc;
#endif
{
 yyMark (yyt);
 yyProc = yyyProc;
 yyTraverseTreeTD (yyt);
}

tTree ReverseTree
#if defined __STDC__ | defined __cplusplus
 (tTree yyOld)
#else
 (yyOld) tTree yyOld;
#endif
{
 register tTree yyNew, yyNext, yyTail;
 yyNew = yyOld;
 yyTail = yyOld;
 for (;;) {
  switch (yyOld->Kind) {
case kstmt_c: yyNext = yyOld->stmt_c.next;
 yyOld->stmt_c.next = yyNew; break;
case kstmt: yyNext = yyOld->stmt.next;
 yyOld->stmt.next = yyNew; break;
case kproc_c: yyNext = yyOld->proc_c.next;
 yyOld->proc_c.next = yyNew; break;
case kproc: yyNext = yyOld->proc.next;
 yyOld->proc.next = yyNew; break;
case kmethod: yyNext = yyOld->method.next;
 yyOld->method.next = yyNew; break;
case kbody: yyNext = yyOld->body.next;
 yyOld->body.next = yyNew; break;
case kconfigbody: yyNext = yyOld->configbody.next;
 yyOld->configbody.next = yyNew; break;
case kconstructor: yyNext = yyOld->constructor.next;
 yyOld->constructor.next = yyNew; break;
case kdestructor: yyNext = yyOld->destructor.next;
 yyOld->destructor.next = yyNew; break;
case knamespace_c: yyNext = yyOld->namespace_c.next;
 yyOld->namespace_c.next = yyNew; break;
case knamespace: yyNext = yyOld->namespace.next;
 yyOld->namespace.next = yyNew; break;
case kclass: yyNext = yyOld->class.next;
 yyOld->class.next = yyNew; break;
case kitcl_class: yyNext = yyOld->itcl_class.next;
 yyOld->itcl_class.next = yyNew; break;
case kword_c: yyNext = yyOld->word_c.next;
 yyOld->word_c.next = yyNew; break;
case kone_word: yyNext = yyOld->one_word.next;
 yyOld->one_word.next = yyNew; break;
case kobj_word: yyNext = yyOld->obj_word.next;
 yyOld->obj_word.next = yyNew; break;
case kqual_word: yyNext = yyOld->qual_word.next;
 yyOld->qual_word.next = yyNew; break;
case kqual_words: yyNext = yyOld->qual_words.next;
 yyOld->qual_words.next = yyNew; break;
case kone_qualification: yyNext = yyOld->one_qualification.next;
 yyOld->one_qualification.next = yyNew; break;
case ktext: yyNext = yyOld->text.next;
 yyOld->text.next = yyNew; break;
case kcharacter: yyNext = yyOld->character.next;
 yyOld->character.next = yyNew; break;
case kident: yyNext = yyOld->ident.next;
 yyOld->ident.next = yyNew; break;
case kblock: yyNext = yyOld->block.next;
 yyOld->block.next = yyNew; break;
case kcontent: yyNext = yyOld->content.next;
 yyOld->content.next = yyNew; break;
case kblock_content: yyNext = yyOld->block_content.next;
 yyOld->block_content.next = yyNew; break;
case kenv2: yyNext = yyOld->env2.env2;
 yyOld->env2.env2 = yyNew; break;
case kobject: yyNext = yyOld->object.next;
 yyOld->object.next = yyNew; break;
  default: goto yyExit;
  }
  yyNew = yyOld;
  yyOld = yyNext;
 }
yyExit:
 switch (yyTail->Kind) {
case kstmt_c: yyTail->stmt_c.next = yyOld; break;
case kstmt: yyTail->stmt.next = yyOld; break;
case kproc_c: yyTail->proc_c.next = yyOld; break;
case kproc: yyTail->proc.next = yyOld; break;
case kmethod: yyTail->method.next = yyOld; break;
case kbody: yyTail->body.next = yyOld; break;
case kconfigbody: yyTail->configbody.next = yyOld; break;
case kconstructor: yyTail->constructor.next = yyOld; break;
case kdestructor: yyTail->destructor.next = yyOld; break;
case knamespace_c: yyTail->namespace_c.next = yyOld; break;
case knamespace: yyTail->namespace.next = yyOld; break;
case kclass: yyTail->class.next = yyOld; break;
case kitcl_class: yyTail->itcl_class.next = yyOld; break;
case kword_c: yyTail->word_c.next = yyOld; break;
case kone_word: yyTail->one_word.next = yyOld; break;
case kobj_word: yyTail->obj_word.next = yyOld; break;
case kqual_word: yyTail->qual_word.next = yyOld; break;
case kqual_words: yyTail->qual_words.next = yyOld; break;
case kone_qualification: yyTail->one_qualification.next = yyOld; break;
case ktext: yyTail->text.next = yyOld; break;
case kcharacter: yyTail->character.next = yyOld; break;
case kident: yyTail->ident.next = yyOld; break;
case kblock: yyTail->block.next = yyOld; break;
case kcontent: yyTail->content.next = yyOld; break;
case kblock_content: yyTail->block_content.next = yyOld; break;
case kenv2: yyTail->env2.env2 = yyOld; break;
case kobject: yyTail->object.next = yyOld; break;
 default: ;
 }
 return yyNew;
}

void ForallTree
#if defined __STDC__ | defined __cplusplus
 (tTree yyt, Tree_tProcTree yyProc)
#else
 (yyt, yyProc) tTree yyt; Tree_tProcTree yyProc;
#endif
{
 for (;;) {
  tTree yyyt;
  if ((yyyt = yyt) == NoTree) return;
  switch (yyt->Kind) {
case kstmt_c: yyt = yyt->stmt_c.next; break;
case kstmt: yyt = yyt->stmt.next; break;
case kproc_c: yyt = yyt->proc_c.next; break;
case kproc: yyt = yyt->proc.next; break;
case kmethod: yyt = yyt->method.next; break;
case kbody: yyt = yyt->body.next; break;
case kconfigbody: yyt = yyt->configbody.next; break;
case kconstructor: yyt = yyt->constructor.next; break;
case kdestructor: yyt = yyt->destructor.next; break;
case knamespace_c: yyt = yyt->namespace_c.next; break;
case knamespace: yyt = yyt->namespace.next; break;
case kclass: yyt = yyt->class.next; break;
case kitcl_class: yyt = yyt->itcl_class.next; break;
case kword_c: yyt = yyt->word_c.next; break;
case kone_word: yyt = yyt->one_word.next; break;
case kobj_word: yyt = yyt->obj_word.next; break;
case kqual_word: yyt = yyt->qual_word.next; break;
case kqual_words: yyt = yyt->qual_words.next; break;
case kone_qualification: yyt = yyt->one_qualification.next; break;
case ktext: yyt = yyt->text.next; break;
case kcharacter: yyt = yyt->character.next; break;
case kident: yyt = yyt->ident.next; break;
case kblock: yyt = yyt->block.next; break;
case kcontent: yyt = yyt->content.next; break;
case kblock_content: yyt = yyt->block_content.next; break;
case kenv2: yyt = yyt->env2.env2; break;
case kobject: yyt = yyt->object.next; break;
  default: return;
  }
  yyProc (yyyt);
 }
}

static rbool yyResult;

static void yyCheckTree ARGS ((tTree yyt));

rbool CheckTree
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyMark (yyt);
 yyResult = rtrue;
 yyCheckTree (yyt);
 return yyResult;
}

static void yyCheckChild2
#if defined __STDC__ | defined __cplusplus
 (tTree yyParent, tTree yyyChild, Tree_tKind yyType, char * yySelector)
#else
 (yyParent, yyyChild, yyType, yySelector)
 tTree yyParent, yyyChild;
 Tree_tKind yyType;
 char * yySelector;
#endif
{
 if (! Tree_IsType (yyyChild, yyType)) {
  yyResult = rfalse;
  (void) fputs ("CheckTree: parent = ", stderr);
  WriteTreeNode (stderr, yyParent);
  (void) fprintf (stderr, "\nselector: %s child = ", yySelector);
  WriteTreeNode (stderr, yyyChild);
  (void) fputc ('\n', stderr);
 }
}

static void yyCheckChild
#if defined __STDC__ | defined __cplusplus
 (tTree yyParent, tTree yyyChild, Tree_tKind yyType, char * yySelector)
#else
 (yyParent, yyyChild, yyType, yySelector)
 tTree yyParent, yyyChild;
 Tree_tKind yyType;
 char * yySelector;
#endif
{
 yyCheckChild2 (yyParent, yyyChild, yyType, yySelector);
 yyCheckTree (yyyChild);
}

static void yyCheckTree
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 for (;;) {
  if (yyt == NoTree) { yyResult = rfalse; return; }
  if (yyt->yyHead.yyMark == 0) return;
  yyt->yyHead.yyMark = 0;

  switch (yyt->Kind) {
case kprogram:
yyCheckChild2 (yyt, yyt->program.stmts,
kstmts, "stmts");
yyt = yyt->program.stmts; break;
case kstmt_c:
yyCheckChild2 (yyt, yyt->stmt_c.next,
kstmts, "next");
yyt = yyt->stmt_c.next; break;
case kstmt:
yyCheckChild (yyt, yyt->stmt.words,
kwords, "words");
yyCheckChild2 (yyt, yyt->stmt.next,
kstmts, "next");
yyt = yyt->stmt.next; break;
case kproc_c:
yyCheckChild (yyt, yyt->proc_c.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->proc_c.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->proc_c.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->proc_c.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->proc_c.next,
kstmts, "next");
yyt = yyt->proc_c.next; break;
case kproc:
yyCheckChild (yyt, yyt->proc.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->proc.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->proc.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->proc.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->proc.next,
kstmts, "next");
yyt = yyt->proc.next; break;
case kmethod:
yyCheckChild (yyt, yyt->method.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->method.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->method.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->method.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->method.next,
kstmts, "next");
yyt = yyt->method.next; break;
case kbody:
yyCheckChild (yyt, yyt->body.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->body.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->body.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->body.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->body.next,
kstmts, "next");
yyt = yyt->body.next; break;
case kconfigbody:
yyCheckChild (yyt, yyt->configbody.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->configbody.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->configbody.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->configbody.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->configbody.next,
kstmts, "next");
yyt = yyt->configbody.next; break;
case kconstructor:
yyCheckChild (yyt, yyt->constructor.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->constructor.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->constructor.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->constructor.block,
ktexts, "block");
yyCheckChild (yyt, yyt->constructor.init,
kwords, "init");
yyCheckChild2 (yyt, yyt->constructor.next,
kstmts, "next");
yyt = yyt->constructor.next; break;
case kdestructor:
yyCheckChild (yyt, yyt->destructor.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->destructor.param_names,
kwords, "param_names");
yyCheckChild (yyt, yyt->destructor.parameter,
kwords, "parameter");
yyCheckChild (yyt, yyt->destructor.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->destructor.next,
kstmts, "next");
yyt = yyt->destructor.next; break;
case knamespace_c:
yyCheckChild (yyt, yyt->namespace_c.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->namespace_c.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->namespace_c.next,
kstmts, "next");
yyt = yyt->namespace_c.next; break;
case knamespace:
yyCheckChild (yyt, yyt->namespace.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->namespace.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->namespace.next,
kstmts, "next");
yyt = yyt->namespace.next; break;
case kclass:
yyCheckChild (yyt, yyt->class.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->class.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->class.next,
kstmts, "next");
yyt = yyt->class.next; break;
case kitcl_class:
yyCheckChild (yyt, yyt->itcl_class.qualification,
kqualification_c, "qualification");
yyCheckChild (yyt, yyt->itcl_class.block,
ktexts, "block");
yyCheckChild2 (yyt, yyt->itcl_class.next,
kstmts, "next");
yyt = yyt->itcl_class.next; break;
case kword_c:
yyCheckChild2 (yyt, yyt->word_c.next,
kwords, "next");
yyt = yyt->word_c.next; break;
case kone_word:
yyCheckChild2 (yyt, yyt->one_word.next,
kwords, "next");
yyt = yyt->one_word.next; break;
case kobj_word:
yyCheckChild2 (yyt, yyt->obj_word.next,
kwords, "next");
yyt = yyt->obj_word.next; break;
case kqual_word:
yyCheckChild (yyt, yyt->qual_word.qualification,
kqualification_c, "qualification");
yyCheckChild2 (yyt, yyt->qual_word.next,
kwords, "next");
yyt = yyt->qual_word.next; break;
case kqual_words:
yyCheckChild (yyt, yyt->qual_words.qualifications,
kqualifications, "qualifications");
yyCheckChild2 (yyt, yyt->qual_words.next,
kwords, "next");
yyt = yyt->qual_words.next; break;
case kone_qualification:
yyCheckChild (yyt, yyt->one_qualification.qualification,
kqualification_c, "qualification");
yyCheckChild2 (yyt, yyt->one_qualification.next,
kqualifications, "next");
yyt = yyt->one_qualification.next; break;
case ktext:
yyCheckChild2 (yyt, yyt->text.next,
ktexts, "next");
yyt = yyt->text.next; break;
case kcharacter:
yyCheckChild2 (yyt, yyt->character.next,
ktexts, "next");
yyt = yyt->character.next; break;
case kident:
yyCheckChild2 (yyt, yyt->ident.next,
ktexts, "next");
yyt = yyt->ident.next; break;
case kblock:
yyCheckChild (yyt, yyt->block.stmts,
kstmts, "stmts");
yyCheckChild2 (yyt, yyt->block.next,
ktexts, "next");
yyt = yyt->block.next; break;
case kcontent:
yyCheckChild (yyt, yyt->content.qualification,
kqualification_c, "qualification");
yyCheckChild2 (yyt, yyt->content.next,
ktexts, "next");
yyt = yyt->content.next; break;
case kblock_content:
yyCheckChild (yyt, yyt->block_content.stmts,
kstmts, "stmts");
yyCheckChild2 (yyt, yyt->block_content.next,
ktexts, "next");
yyt = yyt->block_content.next; break;
case klocal_text:
yyCheckChild2 (yyt, yyt->local_text.texts,
ktexts, "texts");
yyt = yyt->local_text.texts; break;
case kglobal_text:
yyCheckChild2 (yyt, yyt->global_text.texts,
ktexts, "texts");
yyt = yyt->global_text.texts; break;
case kqualification:
yyCheckChild2 (yyt, yyt->qualification.qualification,
kqualification_c, "qualification");
yyt = yyt->qualification.qualification; break;
case kcomplex_qual:
yyCheckChild (yyt, yyt->complex_qual.qualification,
kqualification_c, "qualification");
yyCheckChild2 (yyt, yyt->complex_qual.texts,
ktexts, "texts");
yyt = yyt->complex_qual.texts; break;
case ksubscription:
yyCheckChild (yyt, yyt->subscription.qualification,
kqualification_c, "qualification");
yyCheckChild2 (yyt, yyt->subscription.index,
kqualification_c, "index");
yyt = yyt->subscription.index; break;
case kenv:
yyCheckChild (yyt, yyt->env.objects,
kobjects, "objects");
yyCheckChild2 (yyt, yyt->env.env,
kenvs, "env");
yyt = yyt->env.env; break;
case kenv2:
yyCheckChild (yyt, yyt->env2.env1,
kenvs, "env1");
yyCheckChild2 (yyt, yyt->env2.env2,
kenvs, "env2");
yyt = yyt->env2.env2; break;
case kobject:
yyCheckChild2 (yyt, yyt->object.next,
kobjects, "next");
yyt = yyt->object.next; break;
  default: return;
  }
 }
}

#ifdef __cplusplus
extern "C" {
#include "Position.h"
};
#else
#include "Position.h"
#endif

static long yyLine, yyCurLine;
static tTree yyTheTree, yyNode;
static void (* yySearch3) ARGS ((tTree yyt, tPosition yyp));

static void yySearch4
#if defined __STDC__ | defined __cplusplus
 (tTree yyt, tPosition yyp)
#else
 (yyt, yyp) tTree yyt; tPosition yyp;
#endif
{
 if ((unsigned short) yyLine <= yyp.Line && yyp.Line < (unsigned short) yyCurLine)
  { yyNode = yyt; yyCurLine = yyp.Line; }
}

static void yySearch2
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 switch (yyt->Kind) {
case kproc_c:
yySearch3 (yyt, yyt->proc_c.epos);
break;
case kproc:
yySearch3 (yyt, yyt->proc.epos);
break;
case kmethod:
yySearch3 (yyt, yyt->method.epos);
break;
case kbody:
yySearch3 (yyt, yyt->body.epos);
break;
case kconfigbody:
yySearch3 (yyt, yyt->configbody.epos);
break;
case kconstructor:
yySearch3 (yyt, yyt->constructor.epos);
break;
case kdestructor:
yySearch3 (yyt, yyt->destructor.epos);
break;
case knamespace_c:
yySearch3 (yyt, yyt->namespace_c.epos);
break;
case knamespace:
yySearch3 (yyt, yyt->namespace.epos);
break;
case kclass:
yySearch3 (yyt, yyt->class.epos);
break;
case kitcl_class:
yySearch3 (yyt, yyt->itcl_class.epos);
break;
case kone_word:
yySearch3 (yyt, yyt->one_word.pos);
break;
case kobj_word:
yySearch3 (yyt, yyt->obj_word.pos);
break;
case ktext:
yySearch3 (yyt, yyt->text.pos);
break;
case kcharacter:
yySearch3 (yyt, yyt->character.pos);
break;
case kident:
yySearch3 (yyt, yyt->ident.pos);
break;
case kblock:
yySearch3 (yyt, yyt->block.pos);
yySearch3 (yyt, yyt->block.epos);
break;
case kcontent:
yySearch3 (yyt, yyt->content.pos);
break;
case kblock_content:
yySearch3 (yyt, yyt->block_content.pos);
yySearch3 (yyt, yyt->block_content.epos);
break;
case kqualification_c:
yySearch3 (yyt, yyt->qualification_c.pos);
break;
case klocal_ident:
yySearch3 (yyt, yyt->local_ident.pos);
break;
case kglobal_ident:
yySearch3 (yyt, yyt->global_ident.pos);
break;
case klocal_text:
yySearch3 (yyt, yyt->local_text.pos);
break;
case kglobal_text:
yySearch3 (yyt, yyt->global_text.pos);
break;
case kqualification:
yySearch3 (yyt, yyt->qualification.pos);
break;
case kcomplex_qual:
yySearch3 (yyt, yyt->complex_qual.pos);
break;
case ksubscription:
yySearch3 (yyt, yyt->subscription.pos);
break;
 default: ;
 }
}

#ifdef DRAWTREE
#include <tcl.h>
#include <tk.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
extern int isatty ARGS ((int fd));
}
#else
extern int isatty ARGS ((int fd));
#endif

static Tcl_Interp * yygInterp;

static void yySearch5
#if defined __STDC__ | defined __cplusplus
 (tTree yyt, tPosition yyp)
#else
 (yyt, yyp) tTree yyt; tPosition yyp;
#endif
{
 if (yyt->yyHead.yyx != 65535 && (unsigned short) yyLine <= yyp.Line && yyp.Line < (unsigned short) yyCurLine)
  { yyNode = yyt; yyCurLine = yyp.Line; }
}

static int yySearch
#if defined __STDC__ | defined __cplusplus
 (ClientData yyclass, Tcl_Interp * yyinterp, int yyargc, char * yyargv [])
#else
 (yyclass, yyinterp, yyargc, yyargv)
 ClientData	yyclass;
 Tcl_Interp *	yyinterp;
 int		yyargc;
 char *	yyargv [];
#endif
{
 tTree yyTheTree = (tTree) atol (yyargv [1]);
 yyLine = atol (yyargv [2]);
 yyCurLine = 1000000;
 yyNode = yyTheTree;
 yySearch3 = yySearch5;
 TraverseTreeTD (yyTheTree, yySearch2);
 (void) sprintf (yyinterp->result, "%lu", (unsigned long) yyNode);
 return TCL_OK;
}

static rbool yyphase1;
static int yyDepth = 6;
static int yymaxx, yymaxy;

static int yySetY
#if defined __STDC__ | defined __cplusplus
 (tTree yyp, tTree yyt, int yyy, int yyk)
#else
 (yyp, yyt, yyy, yyk) tTree yyp, yyt; int yyy, yyk;
#endif
{
 int yy, yymax = ++ yyy;
 if (yyphase1) {
  if (yyt == NoTree || (yyt->yyHead.yyMark == 0 &&
   (yyt->yyHead.yyy == 65535 || yyt->yyHead.yyy >= (unsigned short) yyy))) return yymax;
  yyt->yyHead.yyparent = yyp;
 } else {
  if (yyt == NoTree || yyt->yyHead.yyy == 65535 ||
  yyt->yyHead.yyMark == 0 || yyt->yyHead.yyparent != yyp) return yymax;
 }
 yyt->yyHead.yyMark = 0;
 yyt->yyHead.yyy = 65535;
 yyk ++;
 switch (yyt->Kind) {
case kprogram:
yy = yySetY (yyt, yyt->program.stmts, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kstmt_c:
yymax = yySetY (yyt, yyt->stmt_c.next, yymax, yyk - 1);
break;
case kstmt:
yy = yySetY (yyt, yyt->stmt.words, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->stmt.next, yymax - 1, yyk - 1);
break;
case kproc_c:
yy = yySetY (yyt, yyt->proc_c.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc_c.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc_c.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc_c.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->proc_c.next, yymax - 1, yyk - 1);
break;
case kproc:
yy = yySetY (yyt, yyt->proc.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->proc.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->proc.next, yymax - 1, yyk - 1);
break;
case kmethod:
yy = yySetY (yyt, yyt->method.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->method.next, yymax - 1, yyk - 1);
break;
case kbody:
yy = yySetY (yyt, yyt->body.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->body.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->body.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->body.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->body.next, yymax - 1, yyk - 1);
break;
case kconfigbody:
yy = yySetY (yyt, yyt->configbody.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->configbody.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->configbody.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->configbody.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->configbody.next, yymax - 1, yyk - 1);
break;
case kconstructor:
yy = yySetY (yyt, yyt->constructor.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.block, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.init, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->constructor.next, yymax - 1, yyk - 1);
break;
case kdestructor:
yy = yySetY (yyt, yyt->destructor.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->destructor.param_names, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->destructor.parameter, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->destructor.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->destructor.next, yymax - 1, yyk - 1);
break;
case knamespace_c:
yy = yySetY (yyt, yyt->namespace_c.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->namespace_c.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->namespace_c.next, yymax - 1, yyk - 1);
break;
case knamespace:
yy = yySetY (yyt, yyt->namespace.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->namespace.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->namespace.next, yymax - 1, yyk - 1);
break;
case kclass:
yy = yySetY (yyt, yyt->class.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->class.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->class.next, yymax - 1, yyk - 1);
break;
case kitcl_class:
yy = yySetY (yyt, yyt->itcl_class.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->itcl_class.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->itcl_class.next, yymax - 1, yyk - 1);
break;
case kword_c:
yymax = yySetY (yyt, yyt->word_c.next, yymax, yyk - 1);
break;
case kone_word:
yymax = yySetY (yyt, yyt->one_word.next, yymax, yyk - 1);
break;
case kobj_word:
yymax = yySetY (yyt, yyt->obj_word.next, yymax, yyk - 1);
break;
case kqual_word:
yy = yySetY (yyt, yyt->qual_word.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->qual_word.next, yymax - 1, yyk - 1);
break;
case kqual_words:
yy = yySetY (yyt, yyt->qual_words.qualifications, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->qual_words.next, yymax - 1, yyk - 1);
break;
case kone_qualification:
yy = yySetY (yyt, yyt->one_qualification.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->one_qualification.next, yymax - 1, yyk - 1);
break;
case ktext:
yymax = yySetY (yyt, yyt->text.next, yymax, yyk - 1);
break;
case kcharacter:
yymax = yySetY (yyt, yyt->character.next, yymax, yyk - 1);
break;
case kident:
yymax = yySetY (yyt, yyt->ident.next, yymax, yyk - 1);
break;
case kblock:
yy = yySetY (yyt, yyt->block.stmts, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->block.next, yymax - 1, yyk - 1);
break;
case kcontent:
yy = yySetY (yyt, yyt->content.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->content.next, yymax - 1, yyk - 1);
break;
case kblock_content:
yy = yySetY (yyt, yyt->block_content.stmts, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->block_content.next, yymax - 1, yyk - 1);
break;
case klocal_text:
yy = yySetY (yyt, yyt->local_text.texts, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kglobal_text:
yy = yySetY (yyt, yyt->global_text.texts, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kqualification:
yy = yySetY (yyt, yyt->qualification.qualification, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kcomplex_qual:
yy = yySetY (yyt, yyt->complex_qual.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->complex_qual.texts, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ksubscription:
yy = yySetY (yyt, yyt->subscription.qualification, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->subscription.index, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kenv:
yy = yySetY (yyt, yyt->env.objects, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->env.env, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kenv2:
yy = yySetY (yyt, yyt->env2.env1, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->env2.env2, yymax - 1, yyk - 1);
break;
case kobject:
yymax = yySetY (yyt, yyt->object.next, yymax, yyk - 1);
break;
 default: ;
 }
 yyt->yyHead.yyy = yyy;
 yymaxy = Max (yymaxy, yyy);
 return yyk <= yyDepth ? yymax : yyy;
}

typedef struct { unsigned short yyfirst, yylast; } yytFirstLast;

static int yySetX
#if defined __STDC__ | defined __cplusplus
 (tTree yyp, tTree yyt, int yyx, int yyk, yytFirstLast * yyout)
#else
 (yyp, yyt, yyx, yyk, yyout) tTree yyp, yyt; int yyx, yyk; yytFirstLast * yyout;
#endif
{
 int yyxin = yyx, yyw;
 yytFirstLast yyFirstLast;
 if (yyt == NoTree || yyt->yyHead.yyMark == 0 ||
  yyt->yyHead.yyparent != yyp) return yyx;
 yyt->yyHead.yyMark = 0;
 yyFirstLast.yyfirst = 65535;
 yyk ++;
 switch (yyt->Kind) {
case kprogram:
yyx = yySetX (yyt, yyt->program.stmts, yyx, yyk, & yyFirstLast);
break;
case kstmt_c:
yyw = yySetX (yyt, yyt->stmt_c.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kstmt:
yyw = yySetX (yyt, yyt->stmt.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->stmt.words, yyx, yyk, & yyFirstLast);
goto yyList;
case kproc_c:
yyw = yySetX (yyt, yyt->proc_c.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc_c.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc_c.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc_c.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc_c.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kproc:
yyw = yySetX (yyt, yyt->proc.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->proc.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kmethod:
yyw = yySetX (yyt, yyt->method.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kbody:
yyw = yySetX (yyt, yyt->body.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->body.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->body.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->body.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->body.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kconfigbody:
yyw = yySetX (yyt, yyt->configbody.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->configbody.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->configbody.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->configbody.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->configbody.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kconstructor:
yyw = yySetX (yyt, yyt->constructor.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.block, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.init, yyx, yyk, & yyFirstLast);
goto yyList;
case kdestructor:
yyw = yySetX (yyt, yyt->destructor.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->destructor.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->destructor.param_names, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->destructor.parameter, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->destructor.block, yyx, yyk, & yyFirstLast);
goto yyList;
case knamespace_c:
yyw = yySetX (yyt, yyt->namespace_c.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->namespace_c.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->namespace_c.block, yyx, yyk, & yyFirstLast);
goto yyList;
case knamespace:
yyw = yySetX (yyt, yyt->namespace.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->namespace.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->namespace.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kclass:
yyw = yySetX (yyt, yyt->class.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->class.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->class.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kitcl_class:
yyw = yySetX (yyt, yyt->itcl_class.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->itcl_class.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->itcl_class.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kword_c:
yyw = yySetX (yyt, yyt->word_c.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kone_word:
yyw = yySetX (yyt, yyt->one_word.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kobj_word:
yyw = yySetX (yyt, yyt->obj_word.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kqual_word:
yyw = yySetX (yyt, yyt->qual_word.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->qual_word.qualification, yyx, yyk, & yyFirstLast);
goto yyList;
case kqual_words:
yyw = yySetX (yyt, yyt->qual_words.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->qual_words.qualifications, yyx, yyk, & yyFirstLast);
goto yyList;
case kone_qualification:
yyw = yySetX (yyt, yyt->one_qualification.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->one_qualification.qualification, yyx, yyk, & yyFirstLast);
goto yyList;
case ktext:
yyw = yySetX (yyt, yyt->text.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kcharacter:
yyw = yySetX (yyt, yyt->character.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kident:
yyw = yySetX (yyt, yyt->ident.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kblock:
yyw = yySetX (yyt, yyt->block.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->block.stmts, yyx, yyk, & yyFirstLast);
goto yyList;
case kcontent:
yyw = yySetX (yyt, yyt->content.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->content.qualification, yyx, yyk, & yyFirstLast);
goto yyList;
case kblock_content:
yyw = yySetX (yyt, yyt->block_content.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->block_content.stmts, yyx, yyk, & yyFirstLast);
goto yyList;
case klocal_text:
yyx = yySetX (yyt, yyt->local_text.texts, yyx, yyk, & yyFirstLast);
break;
case kglobal_text:
yyx = yySetX (yyt, yyt->global_text.texts, yyx, yyk, & yyFirstLast);
break;
case kqualification:
yyx = yySetX (yyt, yyt->qualification.qualification, yyx, yyk, & yyFirstLast);
break;
case kcomplex_qual:
yyx = yySetX (yyt, yyt->complex_qual.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->complex_qual.texts, yyx, yyk, & yyFirstLast);
break;
case ksubscription:
yyx = yySetX (yyt, yyt->subscription.qualification, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->subscription.index, yyx, yyk, & yyFirstLast);
break;
case kenv:
yyx = yySetX (yyt, yyt->env.objects, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->env.env, yyx, yyk, & yyFirstLast);
break;
case kenv2:
yyw = yySetX (yyt, yyt->env2.env2, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->env2.env1, yyx, yyk, & yyFirstLast);
goto yyList;
case kobject:
yyw = yySetX (yyt, yyt->object.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
 default: ;
 }
 if (yyk <= yyDepth) {
  yyt->yyHead.yyx = yyFirstLast.yyfirst == 65535 ? yyx :
   (yyFirstLast.yyfirst + yyFirstLast.yylast) / 2;
  yymaxx = Max ((unsigned short) yymaxx, yyt->yyHead.yyx);
  if (yyt->yyHead.yyparent == yyp) {
   if (yyout->yyfirst == 65535) yyout->yyfirst = yyt->yyHead.yyx;
   yyout->yylast = yyt->yyHead.yyx;
  }
  yyxin ++;
  return Max (yyx, yyxin);
 } else {
  yyt->yyHead.yyx = 65535;
  return yyxin;
 }
yyList:
 if (yyk <= yyDepth) {
  yyt->yyHead.yyx = yyxin;
  yymaxx = Max (yymaxx, yyxin);
  if (yyt->yyHead.yyparent == yyp) {
   if (yyout->yyfirst == 65535) yyout->yyfirst = yyt->yyHead.yyx;
   yyout->yylast = yyt->yyHead.yyx;
  }
  return Max (yyx, yyw);
 } else {
  yyt->yyHead.yyx = 65535;
  return yyxin;
 }
}

static void yyDrawEdge
#if defined __STDC__ | defined __cplusplus
 (tTree yyfrom, tTree yyto)
#else
 (yyfrom, yyto) tTree yyfrom, yyto;
#endif
{
 char yyCoord [32];
 if (yyto == NoTree) return;
 if (yyto->yyHead.yyx == 65535)
  (void) sprintf (yyCoord, "%d %d %d %d", yyfrom->yyHead.yyx, yyfrom->yyHead.yyy,
   yyfrom->yyHead.yyx + 1, yyfrom->yyHead.yyy);
 else
  (void) sprintf (yyCoord, "%d %d %d %d", yyfrom->yyHead.yyx, yyfrom->yyHead.yyy,
   yyto->yyHead.yyx, yyto->yyHead.yyy);
 (void) Tcl_VarEval (yygInterp, "draw_edge ", yyCoord, NULL);
}

static void yyDrawNode
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 char yyCoord [32];
 if (yyt->yyHead.yyx == 65535) return;
 (void) sprintf (yyCoord, "%d %d %lu ", yyt->yyHead.yyx, yyt->yyHead.yyy, (unsigned long) yyt);
 (void) Tcl_VarEval (yygInterp, "draw_node ", yyCoord,
  Tree_NodeName [yyt->Kind], NULL);
 switch (yyt->Kind) {
case kprogram:
yyDrawEdge (yyt, yyt->program.stmts);
break;
case kstmt_c:
yyDrawEdge (yyt, yyt->stmt_c.next);
break;
case kstmt:
yyDrawEdge (yyt, yyt->stmt.next);
yyDrawEdge (yyt, yyt->stmt.words);
break;
case kproc_c:
yyDrawEdge (yyt, yyt->proc_c.next);
yyDrawEdge (yyt, yyt->proc_c.qualification);
yyDrawEdge (yyt, yyt->proc_c.param_names);
yyDrawEdge (yyt, yyt->proc_c.parameter);
yyDrawEdge (yyt, yyt->proc_c.block);
break;
case kproc:
yyDrawEdge (yyt, yyt->proc.next);
yyDrawEdge (yyt, yyt->proc.qualification);
yyDrawEdge (yyt, yyt->proc.param_names);
yyDrawEdge (yyt, yyt->proc.parameter);
yyDrawEdge (yyt, yyt->proc.block);
break;
case kmethod:
yyDrawEdge (yyt, yyt->method.next);
yyDrawEdge (yyt, yyt->method.qualification);
yyDrawEdge (yyt, yyt->method.param_names);
yyDrawEdge (yyt, yyt->method.parameter);
yyDrawEdge (yyt, yyt->method.block);
break;
case kbody:
yyDrawEdge (yyt, yyt->body.next);
yyDrawEdge (yyt, yyt->body.qualification);
yyDrawEdge (yyt, yyt->body.param_names);
yyDrawEdge (yyt, yyt->body.parameter);
yyDrawEdge (yyt, yyt->body.block);
break;
case kconfigbody:
yyDrawEdge (yyt, yyt->configbody.next);
yyDrawEdge (yyt, yyt->configbody.qualification);
yyDrawEdge (yyt, yyt->configbody.param_names);
yyDrawEdge (yyt, yyt->configbody.parameter);
yyDrawEdge (yyt, yyt->configbody.block);
break;
case kconstructor:
yyDrawEdge (yyt, yyt->constructor.next);
yyDrawEdge (yyt, yyt->constructor.qualification);
yyDrawEdge (yyt, yyt->constructor.param_names);
yyDrawEdge (yyt, yyt->constructor.parameter);
yyDrawEdge (yyt, yyt->constructor.block);
yyDrawEdge (yyt, yyt->constructor.init);
break;
case kdestructor:
yyDrawEdge (yyt, yyt->destructor.next);
yyDrawEdge (yyt, yyt->destructor.qualification);
yyDrawEdge (yyt, yyt->destructor.param_names);
yyDrawEdge (yyt, yyt->destructor.parameter);
yyDrawEdge (yyt, yyt->destructor.block);
break;
case knamespace_c:
yyDrawEdge (yyt, yyt->namespace_c.next);
yyDrawEdge (yyt, yyt->namespace_c.qualification);
yyDrawEdge (yyt, yyt->namespace_c.block);
break;
case knamespace:
yyDrawEdge (yyt, yyt->namespace.next);
yyDrawEdge (yyt, yyt->namespace.qualification);
yyDrawEdge (yyt, yyt->namespace.block);
break;
case kclass:
yyDrawEdge (yyt, yyt->class.next);
yyDrawEdge (yyt, yyt->class.qualification);
yyDrawEdge (yyt, yyt->class.block);
break;
case kitcl_class:
yyDrawEdge (yyt, yyt->itcl_class.next);
yyDrawEdge (yyt, yyt->itcl_class.qualification);
yyDrawEdge (yyt, yyt->itcl_class.block);
break;
case kword_c:
yyDrawEdge (yyt, yyt->word_c.next);
break;
case kone_word:
yyDrawEdge (yyt, yyt->one_word.next);
break;
case kobj_word:
yyDrawEdge (yyt, yyt->obj_word.next);
break;
case kqual_word:
yyDrawEdge (yyt, yyt->qual_word.next);
yyDrawEdge (yyt, yyt->qual_word.qualification);
break;
case kqual_words:
yyDrawEdge (yyt, yyt->qual_words.next);
yyDrawEdge (yyt, yyt->qual_words.qualifications);
break;
case kone_qualification:
yyDrawEdge (yyt, yyt->one_qualification.next);
yyDrawEdge (yyt, yyt->one_qualification.qualification);
break;
case ktext:
yyDrawEdge (yyt, yyt->text.next);
break;
case kcharacter:
yyDrawEdge (yyt, yyt->character.next);
break;
case kident:
yyDrawEdge (yyt, yyt->ident.next);
break;
case kblock:
yyDrawEdge (yyt, yyt->block.next);
yyDrawEdge (yyt, yyt->block.stmts);
break;
case kcontent:
yyDrawEdge (yyt, yyt->content.next);
yyDrawEdge (yyt, yyt->content.qualification);
break;
case kblock_content:
yyDrawEdge (yyt, yyt->block_content.next);
yyDrawEdge (yyt, yyt->block_content.stmts);
break;
case klocal_text:
yyDrawEdge (yyt, yyt->local_text.texts);
break;
case kglobal_text:
yyDrawEdge (yyt, yyt->global_text.texts);
break;
case kqualification:
yyDrawEdge (yyt, yyt->qualification.qualification);
break;
case kcomplex_qual:
yyDrawEdge (yyt, yyt->complex_qual.qualification);
yyDrawEdge (yyt, yyt->complex_qual.texts);
break;
case ksubscription:
yyDrawEdge (yyt, yyt->subscription.qualification);
yyDrawEdge (yyt, yyt->subscription.index);
break;
case kenv:
yyDrawEdge (yyt, yyt->env.objects);
yyDrawEdge (yyt, yyt->env.env);
break;
case kenv2:
yyDrawEdge (yyt, yyt->env2.env1);
yyDrawEdge (yyt, yyt->env2.env2);
break;
case kobject:
yyDrawEdge (yyt, yyt->object.next);
break;
 default: ;
 }
}

static int yyPutAttr
#if defined __STDC__ | defined __cplusplus
 (ClientData yyclass, Tcl_Interp * yyinterp, int yyargc, char * yyargv [])
#else
 (yyclass, yyinterp, yyargc, yyargv)
 ClientData	yyclass;
 Tcl_Interp *	yyinterp;
 int		yyargc;
 char *	yyargv [];
#endif
{
 FILE * yyf = fopen ("yyNode", "w");
 WriteTreeNode (yyf, (tTree) atol (yyargv [1]));
 (void) fclose (yyf);
 return TCL_OK;
}

static int yyDrawTree
#if defined __STDC__ | defined __cplusplus
 (ClientData yyclass, Tcl_Interp * yyinterp, int yyargc, char * yyargv [])
#else
 (yyclass, yyinterp, yyargc, yyargv)
 ClientData	yyclass;
 Tcl_Interp *	yyinterp;
 int		yyargc;
 char *	yyargv [];
#endif
{
 yyDepth = atoi (yyargv [2]);
 DrawTree ((tTree) atol (yyargv [1]));
 return TCL_OK;
}

#ifndef DrawAttr
#define DrawAttr(x, y)
#endif

static int yyCallAttr
#if defined __STDC__ | defined __cplusplus
 (ClientData yyclass, Tcl_Interp * yyinterp, int yyargc, char * yyargv [])
#else
 (yyclass, yyinterp, yyargc, yyargv)
 ClientData	yyclass;
 Tcl_Interp *	yyinterp;
 int		yyargc;
 char *	yyargv [];
#endif
{
DrawAttr ((tTree) atol (yyargv [1]), yyargv [2]);
 return TCL_OK;
}

void SetDepthTree
#if defined __STDC__ | defined __cplusplus
 (int yyyDepth)
#else
 (yyyDepth) int yyyDepth;
#endif
{
 yyDepth = yyyDepth;
}

void DrawTree
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 int yyCode;
 char yyString [128];
 Tcl_Interp * yyInterp;
 yytFirstLast yyDummy;
 yyDummy.yyfirst = 65535;

 yygInterp = yyInterp = Tcl_CreateInterp ();
 Tcl_SetVar (yyInterp, "tcl_interactive", isatty (0) ? "1" : "0",
  TCL_GLOBAL_ONLY);
 yyCode = Tcl_Init (yyInterp);
 if (yyCode != TCL_OK) {
  (void) fprintf (stderr, "%s\n", yyInterp->result); goto yyReturn; }
 yyCode = Tk_Init (yyInterp);
 if (yyCode != TCL_OK) {
  (void) fprintf (stderr, "%s\n", yyInterp->result); goto yyReturn; }
 Tcl_StaticPackage (yyInterp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

 Tcl_CreateCommand (yyInterp, "put_attr", yyPutAttr, (ClientData) NULL, NULL);
 Tcl_CreateCommand (yyInterp, "search_pos", yySearch, (ClientData) NULL, NULL);
 Tcl_CreateCommand (yyInterp, "draw_subtree", yyDrawTree, (ClientData) NULL, NULL);
 Tcl_CreateCommand (yyInterp, "call_attr", yyCallAttr, (ClientData) NULL, NULL);
 yyCode = Tcl_EvalFile (yyInterp, "Tree.tcl");
 if (yyCode != TCL_OK) {
  char path [256], * p = getenv ("CT_DIR");
  if (p) {
   (void) strcpy (path, p);
   (void) strcat (path, "/lib/cg/Tree.tcl");
   yyCode = Tcl_EvalFile (yyInterp, path);
  }
  if (yyCode != TCL_OK) {
   yyCode = Tcl_EvalFile (yyInterp, "/home/grosch/lib.t/cg/Tree.tcl");
   if (yyCode != TCL_OK) {
    (void) fprintf (stderr, "%s\n", yyInterp->result); goto yyReturn; }
  }
 }
 yymaxx = yymaxy = 0;
 yyMark (yyt); yyphase1 = rtrue ; (void) yySetY ((tTree) & yyt, yyt, 0, 0);
 yyMark (yyt); yyphase1 = rfalse; (void) yySetY ((tTree) & yyt, yyt, 0, 0);
 yyMark (yyt); (void) yySetX ((tTree) & yyt, yyt, 0, 0, & yyDummy);
 yymaxx = Max (yymaxx, 16);
 yymaxy = Max (yymaxy, 16);
 (void) sprintf (yyString, "%d %d ", yymaxx, yymaxy);
 (void) Tcl_VarEval (yyInterp, "draw_tree ", yyString, NULL);
 TraverseTreeTD (yyt, yyDrawNode);
 Tk_MainLoop ();
yyReturn: Tcl_DeleteInterp (yyInterp);
}
#endif

void BeginTree ARGS ((void))
{
}

void CloseTree ARGS ((void))
{
}

