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

#ifndef yyTree
#define yyTree

extern char Tree_module_does_not_match_evaluator_module_7188957;
extern char generate_Tree_module_without_option_0;

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#define const
#endif

#include <stdio.h>

#ifndef rbool
#define rbool char
#endif
#define NoTree (tTree) 0L
#define kprogram 1
#define kstmts 2
#define knostmt 3
#define kstmt_c 4
#define kstmt 5
#define kproc_c 6
#define kproc 7
#define kmethod 8
#define kbody 9
#define kconfigbody 10
#define kconstructor 11
#define kdestructor 12
#define knamespace_c 13
#define knamespace 14
#define kclass 15
#define kitcl_class 16
#define kwords 17
#define knoword 18
#define kword_c 19
#define kone_word 20
#define kobj_word 21
#define kqual_word 22
#define kqual_words 23
#define kqualifications 24
#define knoqualification 25
#define kone_qualification 26
#define ktexts 27
#define knotext 28
#define ktext 29
#define kcharacter 30
#define kident 31
#define kblock 32
#define kcontent 33
#define kblock_content 34
#define kqualification_c 35
#define klocal_ident 36
#define kglobal_ident 37
#define klocal_text 38
#define kglobal_text 39
#define kqualification 40
#define kcomplex_qual 41
#define ksubscription 42
#define kenvs 43
#define kenv 44
#define kenv2 45
#define knoenv 46
#define kobjects 47
#define kobject 48
#define knoobject 49
#define yyBlockSize 20480

typedef unsigned char Tree_tKind;
typedef unsigned short Tree_tMark;
typedef unsigned short Tree_tLabel;
typedef union Tree_Node * tTree;
typedef void (* Tree_tProcTree) ARGS ((tTree));
typedef tTree * yytTreePtr;
typedef struct { tTree yyOld, yyNew; } yytTreeOldToNew;

typedef struct yysTree_Block {
 char yyBlock [yyBlockSize];
 struct yysTree_Block * yySuccessor;
} yytTree_Block, * yytTree_BlockPtr;

/* line 5 "tcl.ast" */

#include "Idents.h"
#include "StringM.h"
#include "Position.h"

extern	tIdent
   ibreak, icatch, icontinue, ifor, iforeach, iglobal, iif, ireturn, iwhile,
   ibinary, iarray, iforeach, icatch, ifile, istat, ilstat, iappend, iset,
   iincr, iscan, igets, iinfo, ivwait, iparray, iexists, itkinfo, idefault,
   ilappend, iproc, iglobal, ivariable, inamespace, ieval, isource, iunset,
   iargs, ibody, irename;

extern tIdent last_keyword;

/* line 99 "tcl.ast" */

typedef tTree * tHashTable;


#define Tree_InitHead(ptr, kind) ptr->Kind = kind; \
   ptr->yyHead.yyMark = 0; \
   beginNodeHead (ptr)
#ifndef Tree_NodeHead
#define Tree_NodeHead unsigned short yyx, yyy; tTree yyparent;
#endif
typedef struct { Tree_tKind yyKind; Tree_tMark yyMark;
 Tree_NodeHead } Tree_tNodeHead;
typedef struct { Tree_tNodeHead yyHead;
tTree stmts; } yprogram;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ystmts;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynostmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; } ystmt_c;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree words; } ystmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } yproc_c;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } yproc;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } ymethod;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } ybody;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } yconfigbody;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; tTree init; } yconstructor;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree param_names; tTree parameter; tTree block; tPosition epos; int attribute; } ydestructor;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree block; tPosition epos; int attribute; } ynamespace_c;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree block; tPosition epos; int attribute; } ynamespace;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree block; tPosition epos; int attribute; } yclass;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; tTree block; tPosition epos; int attribute; } yitcl_class;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ywords;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynoword;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; } yword_c;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tIdent ident; tPosition pos; } yone_word;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tIdent ident; tPosition pos; tTree object; } yobj_word;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; } yqual_word;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualifications; } yqual_words;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } yqualifications;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynoqualification;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualification; } yone_qualification;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ytexts;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynotext;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; } ytext;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; char text; } ycharacter;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; tIdent ident; } yident;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; tPosition epos; tTree stmts; char type; } yblock;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; tTree qualification; } ycontent;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tPosition pos; tPosition epos; tTree stmts; } yblock_content;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; } yqualification_c;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tIdent ident; } ylocal_ident;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tIdent ident; } yglobal_ident;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree texts; } ylocal_text;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree texts; } yglobal_text;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree qualification; tIdent ident; } yqualification;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree qualification; tTree texts; } ycomplex_qual;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree qualification; tTree index; } ysubscription;
typedef struct { Tree_tNodeHead yyHead;
} yenvs;
typedef struct { Tree_tNodeHead yyHead;
tTree objects; tTree env; tTree object; tHashTable HashTable; int HashSize; } yenv;
typedef struct { Tree_tNodeHead yyHead;
tTree env1; tTree env2; } yenv2;
typedef struct { Tree_tNodeHead yyHead;
} ynoenv;
typedef struct { Tree_tNodeHead yyHead;
} yobjects;
typedef struct { Tree_tNodeHead yyHead;
tTree object; tIdent ident; tTree next; } yobject;
typedef struct { Tree_tNodeHead yyHead;
} ynoobject;

union Tree_Node {
 Tree_tKind Kind;
 Tree_tNodeHead yyHead;
 yprogram program;
 ystmts stmts;
 ynostmt nostmt;
 ystmt_c stmt_c;
 ystmt stmt;
 yproc_c proc_c;
 yproc proc;
 ymethod method;
 ybody body;
 yconfigbody configbody;
 yconstructor constructor;
 ydestructor destructor;
 ynamespace_c namespace_c;
 ynamespace namespace;
 yclass class;
 yitcl_class itcl_class;
 ywords words;
 ynoword noword;
 yword_c word_c;
 yone_word one_word;
 yobj_word obj_word;
 yqual_word qual_word;
 yqual_words qual_words;
 yqualifications qualifications;
 ynoqualification noqualification;
 yone_qualification one_qualification;
 ytexts texts;
 ynotext notext;
 ytext text;
 ycharacter character;
 yident ident;
 yblock block;
 ycontent content;
 yblock_content block_content;
 yqualification_c qualification_c;
 ylocal_ident local_ident;
 yglobal_ident global_ident;
 ylocal_text local_text;
 yglobal_text global_text;
 yqualification qualification;
 ycomplex_qual complex_qual;
 ysubscription subscription;
 yenvs envs;
 yenv env;
 yenv2 env2;
 ynoenv noenv;
 yobjects objects;
 yobject object;
 ynoobject noobject;
};

extern const unsigned short Tree_NodeSize [50];
extern const char * const Tree_NodeName [];

extern tTree TreeRoot;
extern unsigned long Tree_HeapUsed;
extern yytTree_BlockPtr Tree_BlockList;
extern char * Tree_PoolFreePtr, * Tree_PoolStartPtr;

extern void (* Tree_Exit) ARGS ((void));
extern tTree Tree_Alloc ARGS ((unsigned short yySize));
extern tTree MakeTree ARGS ((Tree_tKind yyKind));
extern rbool Tree_IsType ARGS ((register tTree yyt,
 register Tree_tKind yyKind));

extern tTree mprogram
 ARGS ((tTree pstmts));
extern tTree mstmts
 ARGS ((void));
extern tTree mnostmt
 ARGS ((void));
extern tTree mstmt_c
 ARGS ((tTree pnext));
extern tTree mstmt
 ARGS ((tTree pnext, tTree pwords));
extern tTree mproc_c
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mproc
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mmethod
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mbody
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mconfigbody
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mconstructor
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute, tTree pinit));
extern tTree mdestructor
 ARGS ((tTree pnext, tTree pqualification, tTree pparam_names, tTree pparameter, tTree pblock, tPosition pepos, int pattribute));
extern tTree mnamespace_c
 ARGS ((tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute));
extern tTree mnamespace
 ARGS ((tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute));
extern tTree mclass
 ARGS ((tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute));
extern tTree mitcl_class
 ARGS ((tTree pnext, tTree pqualification, tTree pblock, tPosition pepos, int pattribute));
extern tTree mwords
 ARGS ((void));
extern tTree mnoword
 ARGS ((void));
extern tTree mword_c
 ARGS ((tTree pnext));
extern tTree mone_word
 ARGS ((tTree pnext, tIdent pident, tPosition ppos));
extern tTree mobj_word
 ARGS ((tTree pnext, tIdent pident, tPosition ppos, tTree pobject));
extern tTree mqual_word
 ARGS ((tTree pnext, tTree pqualification));
extern tTree mqual_words
 ARGS ((tTree pnext, tTree pqualifications));
extern tTree mqualifications
 ARGS ((void));
extern tTree mnoqualification
 ARGS ((void));
extern tTree mone_qualification
 ARGS ((tTree pnext, tTree pqualification));
extern tTree mtexts
 ARGS ((void));
extern tTree mnotext
 ARGS ((void));
extern tTree mtext
 ARGS ((tTree pnext, tPosition ppos));
extern tTree mcharacter
 ARGS ((tTree pnext, tPosition ppos, char ptext));
extern tTree mident
 ARGS ((tTree pnext, tPosition ppos, tIdent pident));
extern tTree mblock
 ARGS ((tTree pnext, tPosition ppos, tPosition pepos, tTree pstmts, char ptype));
extern tTree mcontent
 ARGS ((tTree pnext, tPosition ppos, tTree pqualification));
extern tTree mblock_content
 ARGS ((tTree pnext, tPosition ppos, tPosition pepos, tTree pstmts));
extern tTree mqualification_c
 ARGS ((tPosition ppos));
extern tTree mlocal_ident
 ARGS ((tPosition ppos, tIdent pident));
extern tTree mglobal_ident
 ARGS ((tPosition ppos, tIdent pident));
extern tTree mlocal_text
 ARGS ((tPosition ppos, tTree ptexts));
extern tTree mglobal_text
 ARGS ((tPosition ppos, tTree ptexts));
extern tTree mqualification
 ARGS ((tPosition ppos, tTree pqualification, tIdent pident));
extern tTree mcomplex_qual
 ARGS ((tPosition ppos, tTree pqualification, tTree ptexts));
extern tTree msubscription
 ARGS ((tPosition ppos, tTree pqualification, tTree pindex));
extern tTree menvs
 ARGS ((void));
extern tTree menv
 ARGS ((tTree pobjects, tTree penv, tTree pobject));
extern tTree menv2
 ARGS ((tTree penv1, tTree penv2));
extern tTree mnoenv
 ARGS ((void));
extern tTree mobjects
 ARGS ((void));
extern tTree mobject
 ARGS ((tTree pobject, tIdent pident, tTree pnext));
extern tTree mnoobject
 ARGS ((void));

extern void ReleaseTree ARGS ((tTree yyt));
extern void ReleaseTreeModule ARGS ((void));
extern void WriteTreeNode ARGS ((FILE * yyyf, tTree yyt));
extern void WriteTree ARGS ((FILE * yyyf, tTree yyt));
extern tTree ReadTree ARGS ((FILE * yyyf));
extern void PutTree ARGS ((FILE * yyyf, tTree yyt));
extern tTree GetTree ARGS ((FILE * yyyf));
extern void TraverseTreeTD ARGS ((tTree yyt, Tree_tProcTree yyyProc));
extern void TraverseTreeBU ARGS ((tTree yyt, Tree_tProcTree yyProc));
extern tTree ReverseTree ARGS ((tTree yyt));
extern void ForallTree ARGS ((tTree yyt, Tree_tProcTree yyProc));
extern tTree CopyTree ARGS ((tTree yyt));
extern rbool CheckTree ARGS ((tTree yyt));
extern void QueryTree ARGS ((tTree yyt));
extern void DrawTree ARGS ((tTree yyt));
extern void SetDepthTree ARGS ((int yyDepth));
extern rbool IsEqualTree ARGS ((tTree yyt1, tTree yyt2));
extern void InitTree ARGS ((register tTree yyt));
extern void BeginTree ARGS ((void));
extern void CloseTree ARGS ((void));

#endif

