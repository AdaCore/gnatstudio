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
#include "StringM.h"
#include "Idents.h"
#include "Position.h"
}
#else
#include "Reuse.h"
#include "rSystem.h"
#include "General.h"
#include "rMemory.h"
#include "DynArray.h"
#include "StringM.h"
#include "Idents.h"
#include "Position.h"
#endif

char Tree_module_does_not_match_evaluator_module_30586066;
char generate_Tree_module_without_option_0;

/* line 66 "java.ast" */

#define writetoperator(a) (void) fputs (operators [a], yyf);

static char * operators [] = { 0,
   "and",
   "and_and",
   "assignment",
   "complement",
   "divide",
   "equal",
   "greater",
   "greater_equal",
   "less",
   "less_equal",
   "lshift",
   "minus",
   "modulo",
   "negate",
   "not",
   "not_equal",
   "or",
   "or_or",
   "plus",
   "post_decr",
   "post_incr",
   "pre_decr",
   "pre_incr",
   "rshift",
   "times",
   "us_rshift",
   "xor",
};

#define writettype(a) (void) fputs (types [a], yyf);

char * types [] = { 0,
   "boolean",
   "byte",
   "char",
   "double",
   "float",
   "int",
   "long",
   "short",
   "void",
};

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
const unsigned short Tree_NodeSize [99] = { 0,
 yyAlignedSize (sizeof (ycompilation_unit)),
 yyAlignedSize (sizeof (yimport_list)),
 yyAlignedSize (sizeof (ynoimport)),
 yyAlignedSize (sizeof (yimport)),
 yyAlignedSize (sizeof (yimport_asterisk)),
 yyAlignedSize (sizeof (yfield_list)),
 yyAlignedSize (sizeof (ynofield)),
 yyAlignedSize (sizeof (yfield)),
 yyAlignedSize (sizeof (yfunction)),
 yyAlignedSize (sizeof (ymethod)),
 yyAlignedSize (sizeof (yconstructor)),
 yyAlignedSize (sizeof (yvar_decl)),
 yyAlignedSize (sizeof (ystatic_initializer)),
 yyAlignedSize (sizeof (yinitializer)),
 yyAlignedSize (sizeof (ytype_decl)),
 yyAlignedSize (sizeof (yclass)),
 yyAlignedSize (sizeof (yinterface)),
 yyAlignedSize (sizeof (ydecl_list)),
 yyAlignedSize (sizeof (ynodecl)),
 yyAlignedSize (sizeof (ydecl)),
 yyAlignedSize (sizeof (yvariable)),
 yyAlignedSize (sizeof (yparameter)),
 yyAlignedSize (sizeof (ystatement_list)),
 yyAlignedSize (sizeof (ynostatement)),
 yyAlignedSize (sizeof (ystatement)),
 yyAlignedSize (sizeof (ystatement_c)),
 yyAlignedSize (sizeof (ycompound_stmt)),
 yyAlignedSize (sizeof (yexpression_stmt)),
 yyAlignedSize (sizeof (yvar_decl_stmt)),
 yyAlignedSize (sizeof (ytype_decl_stmt)),
 yyAlignedSize (sizeof (yif_stmt)),
 yyAlignedSize (sizeof (yif_else_stmt)),
 yyAlignedSize (sizeof (ywhile_stmt)),
 yyAlignedSize (sizeof (ydo_stmt)),
 yyAlignedSize (sizeof (ybreak_stmt)),
 yyAlignedSize (sizeof (ybreak_id_stmt)),
 yyAlignedSize (sizeof (ycontinue_stmt)),
 yyAlignedSize (sizeof (ycontinue_id_stmt)),
 yyAlignedSize (sizeof (yreturn_stmt)),
 yyAlignedSize (sizeof (yreturn_expr_stmt)),
 yyAlignedSize (sizeof (yfor_stmt)),
 yyAlignedSize (sizeof (ythrow_stmt)),
 yyAlignedSize (sizeof (ysynchronized_stmt)),
 yyAlignedSize (sizeof (ylabeled_stmt)),
 yyAlignedSize (sizeof (ytry_stmt)),
 yyAlignedSize (sizeof (yswitch_stmt)),
 yyAlignedSize (sizeof (ynull_stmt)),
 yyAlignedSize (sizeof (yswitch_list)),
 yyAlignedSize (sizeof (ynoswitch)),
 yyAlignedSize (sizeof (yswitch_)),
 yyAlignedSize (sizeof (ycatch_list)),
 yyAlignedSize (sizeof (ynocatch)),
 yyAlignedSize (sizeof (ycatch)),
 yyAlignedSize (sizeof (ytype_name_list)),
 yyAlignedSize (sizeof (ynotype_name)),
 yyAlignedSize (sizeof (ytype_name)),
 yyAlignedSize (sizeof (ytype)),
 yyAlignedSize (sizeof (ysimple_type)),
 yyAlignedSize (sizeof (ynamed_type)),
 yyAlignedSize (sizeof (yarray_type)),
 yyAlignedSize (sizeof (ynotype)),
 yyAlignedSize (sizeof (yexpression_list)),
 yyAlignedSize (sizeof (ynoexpression_l)),
 yyAlignedSize (sizeof (yexpression)),
 yyAlignedSize (sizeof (yexpression_c)),
 yyAlignedSize (sizeof (yqualified_symbol)),
 yyAlignedSize (sizeof (yqualification)),
 yyAlignedSize (sizeof (yident)),
 yyAlignedSize (sizeof (ynoexpression)),
 yyAlignedSize (sizeof (yunary)),
 yyAlignedSize (sizeof (ybinary)),
 yyAlignedSize (sizeof (yassign)),
 yyAlignedSize (sizeof (yaggregate)),
 yyAlignedSize (sizeof (ycall)),
 yyAlignedSize (sizeof (yselect)),
 yyAlignedSize (sizeof (yget_class_of_expr)),
 yyAlignedSize (sizeof (yget_class)),
 yyAlignedSize (sizeof (ysubscript)),
 yyAlignedSize (sizeof (ytype_compare)),
 yyAlignedSize (sizeof (ytype_cast)),
 yyAlignedSize (sizeof (ynew)),
 yyAlignedSize (sizeof (yanonymous)),
 yyAlignedSize (sizeof (yconditional)),
 yyAlignedSize (sizeof (ysuper)),
 yyAlignedSize (sizeof (ythis)),
 yyAlignedSize (sizeof (ynull)),
 yyAlignedSize (sizeof (yint_literal)),
 yyAlignedSize (sizeof (ylong_literal)),
 yyAlignedSize (sizeof (ybool_literal)),
 yyAlignedSize (sizeof (yfloat_literal)),
 yyAlignedSize (sizeof (ychar_literal)),
 yyAlignedSize (sizeof (ystring_literal)),
 yyAlignedSize (sizeof (yenvs)),
 yyAlignedSize (sizeof (yenv)),
 yyAlignedSize (sizeof (yenv2)),
 yyAlignedSize (sizeof (yobjects)),
 yyAlignedSize (sizeof (yobject)),
 yyAlignedSize (sizeof (ynoobject)),
};
static const Tree_tKind yyTypeRange [99] = { 0,
 kcompilation_unit,
 kimport_asterisk,
 knoimport,
 kimport_asterisk,
 kimport_asterisk,
 kinterface,
 knofield,
 kinterface,
 kconstructor,
 kmethod,
 kconstructor,
 kvar_decl,
 kstatic_initializer,
 kinitializer,
 kinterface,
 kclass,
 kinterface,
 kparameter,
 knodecl,
 kparameter,
 kvariable,
 kparameter,
 kstatement,
 knostatement,
 kstatement,
 knull_stmt,
 kcompound_stmt,
 kexpression_stmt,
 kvar_decl_stmt,
 ktype_decl_stmt,
 kif_else_stmt,
 kif_else_stmt,
 kwhile_stmt,
 kdo_stmt,
 kbreak_id_stmt,
 kbreak_id_stmt,
 kcontinue_id_stmt,
 kcontinue_id_stmt,
 kreturn_expr_stmt,
 kreturn_expr_stmt,
 kfor_stmt,
 kthrow_stmt,
 ksynchronized_stmt,
 klabeled_stmt,
 ktry_stmt,
 kswitch_stmt,
 knull_stmt,
 kswitch_,
 knoswitch,
 kswitch_,
 kcatch,
 knocatch,
 kcatch,
 ktype_name,
 knotype_name,
 ktype_name,
 knotype,
 ksimple_type,
 knamed_type,
 karray_type,
 knotype,
 kexpression,
 knoexpression_l,
 kexpression,
 kstring_literal,
 knoexpression,
 kqualification,
 kident,
 knoexpression,
 kunary,
 kbinary,
 kassign,
 kaggregate,
 kcall,
 kselect,
 kget_class_of_expr,
 kget_class,
 ksubscript,
 ktype_compare,
 ktype_cast,
 knew,
 kanonymous,
 kconditional,
 ksuper,
 kthis,
 knull,
 kint_literal,
 klong_literal,
 kbool_literal,
 kfloat_literal,
 kchar_literal,
 kstring_literal,
 kenv2,
 kenv,
 kenv2,
 knoobject,
 kobject,
 knoobject,
};
const char * const Tree_NodeName [99] = {
 "NoTree",
 "compilation_unit",
 "import_list",
 "noimport",
 "import",
 "import_asterisk",
 "field_list",
 "nofield",
 "field",
 "function",
 "method",
 "constructor",
 "var_decl",
 "static_initializer",
 "initializer",
 "type_decl",
 "class",
 "interface",
 "decl_list",
 "nodecl",
 "decl",
 "variable",
 "parameter",
 "statement_list",
 "nostatement",
 "statement",
 "statement_c",
 "compound_stmt",
 "expression_stmt",
 "var_decl_stmt",
 "type_decl_stmt",
 "if_stmt",
 "if_else_stmt",
 "while_stmt",
 "do_stmt",
 "break_stmt",
 "break_id_stmt",
 "continue_stmt",
 "continue_id_stmt",
 "return_stmt",
 "return_expr_stmt",
 "for_stmt",
 "throw_stmt",
 "synchronized_stmt",
 "labeled_stmt",
 "try_stmt",
 "switch_stmt",
 "null_stmt",
 "switch_list",
 "noswitch",
 "switch_",
 "catch_list",
 "nocatch",
 "catch",
 "type_name_list",
 "notype_name",
 "type_name",
 "type",
 "simple_type",
 "named_type",
 "array_type",
 "notype",
 "expression_list",
 "noexpression_l",
 "expression",
 "expression_c",
 "qualified_symbol",
 "qualification",
 "ident",
 "noexpression",
 "unary",
 "binary",
 "assign",
 "aggregate",
 "call",
 "select",
 "get_class_of_expr",
 "get_class",
 "subscript",
 "type_compare",
 "type_cast",
 "new",
 "anonymous",
 "conditional",
 "super",
 "this",
 "null",
 "int_literal",
 "long_literal",
 "bool_literal",
 "float_literal",
 "char_literal",
 "string_literal",
 "envs",
 "env",
 "env2",
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


tTree mcompilation_unit
#if defined __STDC__ | defined __cplusplus
(tTree ppackage, tTree pimport_list, tTree pfield_list)
#else
(ppackage, pimport_list, pfield_list)
tTree ppackage;
tTree pimport_list;
tTree pfield_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycompilation_unit));
 Tree_InitHead (yyt, kcompilation_unit)
 yyt->compilation_unit.package = ppackage;
 yyt->compilation_unit.import_list = pimport_list;
 yyt->compilation_unit.field_list = pfield_list;
 return yyt;
}

tTree mimport_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yimport_list));
 Tree_InitHead (yyt, kimport_list)
 begintTree(yyt->import_list.env)
 return yyt;
}

tTree mnoimport
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoimport));
 Tree_InitHead (yyt, knoimport)
 begintTree(yyt->noimport.env)
 return yyt;
}

tTree mimport
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualified_symbol)
#else
(pnext, pqualified_symbol)
tTree pnext;
tTree pqualified_symbol;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yimport));
 Tree_InitHead (yyt, kimport)
 begintTree(yyt->import.env)
 yyt->import.next = pnext;
 yyt->import.qualified_symbol = pqualified_symbol;
 return yyt;
}

tTree mimport_asterisk
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pqualified_symbol)
#else
(pnext, pqualified_symbol)
tTree pnext;
tTree pqualified_symbol;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yimport_asterisk));
 Tree_InitHead (yyt, kimport_asterisk)
 begintTree(yyt->import_asterisk.env)
 yyt->import_asterisk.next = pnext;
 yyt->import_asterisk.qualified_symbol = pqualified_symbol;
 return yyt;
}

tTree mfield_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yfield_list));
 Tree_InitHead (yyt, kfield_list)
 begintTree(yyt->field_list.env)
 return yyt;
}

tTree mnofield
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynofield));
 Tree_InitHead (yyt, knofield)
 begintTree(yyt->nofield.env)
 return yyt;
}

tTree mfield
#if defined __STDC__ | defined __cplusplus
(tTree pnext)
#else
(pnext)
tTree pnext;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yfield));
 Tree_InitHead (yyt, kfield)
 begintTree(yyt->field.env)
 yyt->field.next = pnext;
 return yyt;
}

tTree mfunction
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock)
#else
(pnext, pmodifiers, pident, ppos, pdecl_list, pthrows, pblock)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pdecl_list;
tTree pthrows;
tTree pblock;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yfunction));
 Tree_InitHead (yyt, kfunction)
 begintTree(yyt->function.env)
 yyt->function.next = pnext;
 yyt->function.modifiers = pmodifiers;
 yyt->function.ident = pident;
 yyt->function.pos = ppos;
 yyt->function.decl_list = pdecl_list;
 yyt->function.throws = pthrows;
 yyt->function.block = pblock;
 return yyt;
}

tTree mmethod
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock, tTree ptype, tTree parray)
#else
(pnext, pmodifiers, pident, ppos, pdecl_list, pthrows, pblock, ptype, parray)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pdecl_list;
tTree pthrows;
tTree pblock;
tTree ptype;
tTree parray;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ymethod));
 Tree_InitHead (yyt, kmethod)
 begintTree(yyt->method.env)
 yyt->method.next = pnext;
 yyt->method.modifiers = pmodifiers;
 yyt->method.ident = pident;
 yyt->method.pos = ppos;
 yyt->method.decl_list = pdecl_list;
 yyt->method.throws = pthrows;
 yyt->method.block = pblock;
 yyt->method.type = ptype;
 yyt->method.array = parray;
 return yyt;
}

tTree mconstructor
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock)
#else
(pnext, pmodifiers, pident, ppos, pdecl_list, pthrows, pblock)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pdecl_list;
tTree pthrows;
tTree pblock;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yconstructor));
 Tree_InitHead (yyt, kconstructor)
 begintTree(yyt->constructor.env)
 yyt->constructor.next = pnext;
 yyt->constructor.modifiers = pmodifiers;
 yyt->constructor.ident = pident;
 yyt->constructor.pos = ppos;
 yyt->constructor.decl_list = pdecl_list;
 yyt->constructor.throws = pthrows;
 yyt->constructor.block = pblock;
 return yyt;
}

tTree mvar_decl
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tTree ptype, tTree pdecl_list)
#else
(pnext, pmodifiers, ptype, pdecl_list)
tTree pnext;
long pmodifiers;
tTree ptype;
tTree pdecl_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yvar_decl));
 Tree_InitHead (yyt, kvar_decl)
 begintTree(yyt->var_decl.env)
 yyt->var_decl.next = pnext;
 yyt->var_decl.modifiers = pmodifiers;
 yyt->var_decl.type = ptype;
 yyt->var_decl.decl_list = pdecl_list;
 return yyt;
}

tTree mstatic_initializer
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pstatement_c)
#else
(pnext, pstatement_c)
tTree pnext;
tTree pstatement_c;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystatic_initializer));
 Tree_InitHead (yyt, kstatic_initializer)
 begintTree(yyt->static_initializer.env)
 yyt->static_initializer.next = pnext;
 yyt->static_initializer.statement_c = pstatement_c;
 return yyt;
}

tTree minitializer
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pstatement_c)
#else
(pnext, pstatement_c)
tTree pnext;
tTree pstatement_c;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yinitializer));
 Tree_InitHead (yyt, kinitializer)
 begintTree(yyt->initializer.env)
 yyt->initializer.next = pnext;
 yyt->initializer.statement_c = pstatement_c;
 return yyt;
}

tTree mtype_decl
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock)
#else
(pnext, pmodifiers, pident, ppos, pblock)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pblock;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_decl));
 Tree_InitHead (yyt, ktype_decl)
 begintTree(yyt->type_decl.env)
 yyt->type_decl.next = pnext;
 yyt->type_decl.modifiers = pmodifiers;
 yyt->type_decl.ident = pident;
 yyt->type_decl.pos = ppos;
 yyt->type_decl.block = pblock;
 return yyt;
}

tTree mclass
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock, tTree pextends, tTree pinterfaces)
#else
(pnext, pmodifiers, pident, ppos, pblock, pextends, pinterfaces)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pblock;
tTree pextends;
tTree pinterfaces;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yclass));
 Tree_InitHead (yyt, kclass)
 begintTree(yyt->class.env)
 yyt->class.next = pnext;
 yyt->class.modifiers = pmodifiers;
 yyt->class.ident = pident;
 yyt->class.pos = ppos;
 yyt->class.block = pblock;
 yyt->class.extends = pextends;
 yyt->class.interfaces = pinterfaces;
 return yyt;
}

tTree minterface
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock, tTree pextends)
#else
(pnext, pmodifiers, pident, ppos, pblock, pextends)
tTree pnext;
long pmodifiers;
tIdent pident;
tPosition ppos;
tTree pblock;
tTree pextends;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yinterface));
 Tree_InitHead (yyt, kinterface)
 begintTree(yyt->my_interface.env)
 yyt->my_interface.next = pnext;
 yyt->my_interface.modifiers = pmodifiers;
 yyt->my_interface.ident = pident;
 yyt->my_interface.pos = ppos;
 yyt->my_interface.block = pblock;
 yyt->my_interface.extends = pextends;
 return yyt;
}

tTree mdecl_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ydecl_list));
 Tree_InitHead (yyt, kdecl_list)
 begintTree(yyt->decl_list.env)
 beginshort(yyt->decl_list.no_of_args)
 return yyt;
}

tTree mnodecl
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynodecl));
 Tree_InitHead (yyt, knodecl)
 begintTree(yyt->nodecl.env)
 beginshort(yyt->nodecl.no_of_args)
 return yyt;
}

tTree mdecl
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray)
#else
(pnext, pmodifiers, ptype, pident, ppos, parray)
tTree pnext;
long pmodifiers;
tTree ptype;
tIdent pident;
tPosition ppos;
tTree parray;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ydecl));
 Tree_InitHead (yyt, kdecl)
 begintTree(yyt->decl.env)
 beginshort(yyt->decl.no_of_args)
 yyt->decl.next = pnext;
 yyt->decl.modifiers = pmodifiers;
 yyt->decl.type = ptype;
 yyt->decl.ident = pident;
 yyt->decl.pos = ppos;
 yyt->decl.array = parray;
 return yyt;
}

tTree mvariable
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray, tTree pexpression)
#else
(pnext, pmodifiers, ptype, pident, ppos, parray, pexpression)
tTree pnext;
long pmodifiers;
tTree ptype;
tIdent pident;
tPosition ppos;
tTree parray;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yvariable));
 Tree_InitHead (yyt, kvariable)
 begintTree(yyt->variable.env)
 beginshort(yyt->variable.no_of_args)
 yyt->variable.next = pnext;
 yyt->variable.modifiers = pmodifiers;
 yyt->variable.type = ptype;
 yyt->variable.ident = pident;
 yyt->variable.pos = ppos;
 yyt->variable.array = parray;
 yyt->variable.expression = pexpression;
 return yyt;
}

tTree mparameter
#if defined __STDC__ | defined __cplusplus
(tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray)
#else
(pnext, pmodifiers, ptype, pident, ppos, parray)
tTree pnext;
long pmodifiers;
tTree ptype;
tIdent pident;
tPosition ppos;
tTree parray;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yparameter));
 Tree_InitHead (yyt, kparameter)
 begintTree(yyt->parameter.env)
 beginshort(yyt->parameter.no_of_args)
 yyt->parameter.next = pnext;
 yyt->parameter.modifiers = pmodifiers;
 yyt->parameter.type = ptype;
 yyt->parameter.ident = pident;
 yyt->parameter.pos = ppos;
 yyt->parameter.array = parray;
 return yyt;
}

tTree mstatement_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystatement_list));
 Tree_InitHead (yyt, kstatement_list)
 begintTree(yyt->statement_list.env)
 return yyt;
}

tTree mnostatement
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynostatement));
 Tree_InitHead (yyt, knostatement)
 begintTree(yyt->nostatement.env)
 return yyt;
}

tTree mstatement
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pstatement)
#else
(pnext, pstatement)
tTree pnext;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystatement));
 Tree_InitHead (yyt, kstatement)
 begintTree(yyt->statement.env)
 yyt->statement.next = pnext;
 yyt->statement.statement = pstatement;
 return yyt;
}

tTree mstatement_c
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystatement_c));
 Tree_InitHead (yyt, kstatement_c)
 begintTree(yyt->statement_c.env)
 return yyt;
}

tTree mcompound_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pstatement_list)
#else
(pstatement_list)
tTree pstatement_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycompound_stmt));
 Tree_InitHead (yyt, kcompound_stmt)
 begintTree(yyt->compound_stmt.env)
 yyt->compound_stmt.statement_list = pstatement_list;
 return yyt;
}

tTree mexpression_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression)
#else
(pexpression)
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yexpression_stmt));
 Tree_InitHead (yyt, kexpression_stmt)
 begintTree(yyt->expression_stmt.env)
 yyt->expression_stmt.expression = pexpression;
 return yyt;
}

tTree mvar_decl_stmt
#if defined __STDC__ | defined __cplusplus
(tTree ptype, tTree pdecl_list)
#else
(ptype, pdecl_list)
tTree ptype;
tTree pdecl_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yvar_decl_stmt));
 Tree_InitHead (yyt, kvar_decl_stmt)
 begintTree(yyt->var_decl_stmt.env)
 yyt->var_decl_stmt.type = ptype;
 yyt->var_decl_stmt.decl_list = pdecl_list;
 return yyt;
}

tTree mtype_decl_stmt
#if defined __STDC__ | defined __cplusplus
(tTree ptype_decl)
#else
(ptype_decl)
tTree ptype_decl;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_decl_stmt));
 Tree_InitHead (yyt, ktype_decl_stmt)
 begintTree(yyt->type_decl_stmt.env)
 yyt->type_decl_stmt.type_decl = ptype_decl;
 return yyt;
}

tTree mif_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression, tTree pthen)
#else
(pexpression, pthen)
tTree pexpression;
tTree pthen;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yif_stmt));
 Tree_InitHead (yyt, kif_stmt)
 begintTree(yyt->if_stmt.env)
 yyt->if_stmt.expression = pexpression;
 yyt->if_stmt.then = pthen;
 return yyt;
}

tTree mif_else_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression, tTree pthen, tTree pElse)
#else
(pexpression, pthen, pElse)
tTree pexpression;
tTree pthen;
tTree pElse;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yif_else_stmt));
 Tree_InitHead (yyt, kif_else_stmt)
 begintTree(yyt->if_else_stmt.env)
 yyt->if_else_stmt.expression = pexpression;
 yyt->if_else_stmt.then = pthen;
 yyt->if_else_stmt.Else = pElse;
 return yyt;
}

tTree mwhile_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression, tTree pstatement)
#else
(pexpression, pstatement)
tTree pexpression;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ywhile_stmt));
 Tree_InitHead (yyt, kwhile_stmt)
 begintTree(yyt->while_stmt.env)
 yyt->while_stmt.expression = pexpression;
 yyt->while_stmt.statement = pstatement;
 return yyt;
}

tTree mdo_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pstatement, tTree pexpression)
#else
(pstatement, pexpression)
tTree pstatement;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ydo_stmt));
 Tree_InitHead (yyt, kdo_stmt)
 begintTree(yyt->do_stmt.env)
 yyt->do_stmt.statement = pstatement;
 yyt->do_stmt.expression = pexpression;
 return yyt;
}

tTree mbreak_stmt
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ybreak_stmt));
 Tree_InitHead (yyt, kbreak_stmt)
 begintTree(yyt->break_stmt.env)
 return yyt;
}

tTree mbreak_id_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression)
#else
(pexpression)
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ybreak_id_stmt));
 Tree_InitHead (yyt, kbreak_id_stmt)
 begintTree(yyt->break_id_stmt.env)
 yyt->break_id_stmt.expression = pexpression;
 return yyt;
}

tTree mcontinue_stmt
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycontinue_stmt));
 Tree_InitHead (yyt, kcontinue_stmt)
 begintTree(yyt->continue_stmt.env)
 return yyt;
}

tTree mcontinue_id_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression)
#else
(pexpression)
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycontinue_id_stmt));
 Tree_InitHead (yyt, kcontinue_id_stmt)
 begintTree(yyt->continue_id_stmt.env)
 yyt->continue_id_stmt.expression = pexpression;
 return yyt;
}

tTree mreturn_stmt
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yreturn_stmt));
 Tree_InitHead (yyt, kreturn_stmt)
 begintTree(yyt->return_stmt.env)
 return yyt;
}

tTree mreturn_expr_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression)
#else
(pexpression)
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yreturn_expr_stmt));
 Tree_InitHead (yyt, kreturn_expr_stmt)
 begintTree(yyt->return_expr_stmt.env)
 yyt->return_expr_stmt.expression = pexpression;
 return yyt;
}

tTree mfor_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pfor_init, tTree pexpression, tTree pfor_incr, tTree pstatement)
#else
(pfor_init, pexpression, pfor_incr, pstatement)
tTree pfor_init;
tTree pexpression;
tTree pfor_incr;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yfor_stmt));
 Tree_InitHead (yyt, kfor_stmt)
 begintTree(yyt->for_stmt.env)
 yyt->for_stmt.for_init = pfor_init;
 yyt->for_stmt.expression = pexpression;
 yyt->for_stmt.for_incr = pfor_incr;
 yyt->for_stmt.statement = pstatement;
 return yyt;
}

tTree mthrow_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression)
#else
(pexpression)
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ythrow_stmt));
 Tree_InitHead (yyt, kthrow_stmt)
 begintTree(yyt->throw_stmt.env)
 yyt->throw_stmt.expression = pexpression;
 return yyt;
}

tTree msynchronized_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression, tTree pstatement)
#else
(pexpression, pstatement)
tTree pexpression;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ysynchronized_stmt));
 Tree_InitHead (yyt, ksynchronized_stmt)
 begintTree(yyt->synchronized_stmt.env)
 yyt->synchronized_stmt.expression = pexpression;
 yyt->synchronized_stmt.statement = pstatement;
 return yyt;
}

tTree mlabeled_stmt
#if defined __STDC__ | defined __cplusplus
(tIdent pident, tPosition ppos, tTree pstatement)
#else
(pident, ppos, pstatement)
tIdent pident;
tPosition ppos;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ylabeled_stmt));
 Tree_InitHead (yyt, klabeled_stmt)
 begintTree(yyt->labeled_stmt.env)
 yyt->labeled_stmt.ident = pident;
 yyt->labeled_stmt.pos = ppos;
 yyt->labeled_stmt.statement = pstatement;
 return yyt;
}

tTree mtry_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pstatement, tTree pcatch_list, tTree pfinally)
#else
(pstatement, pcatch_list, pfinally)
tTree pstatement;
tTree pcatch_list;
tTree pfinally;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytry_stmt));
 Tree_InitHead (yyt, ktry_stmt)
 begintTree(yyt->try_stmt.env)
 yyt->try_stmt.statement = pstatement;
 yyt->try_stmt.catch_list = pcatch_list;
 yyt->try_stmt.finally = pfinally;
 return yyt;
}

tTree mswitch_stmt
#if defined __STDC__ | defined __cplusplus
(tTree pexpression, tTree pswitch_list)
#else
(pexpression, pswitch_list)
tTree pexpression;
tTree pswitch_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yswitch_stmt));
 Tree_InitHead (yyt, kswitch_stmt)
 begintTree(yyt->switch_stmt.env)
 yyt->switch_stmt.expression = pexpression;
 yyt->switch_stmt.switch_list = pswitch_list;
 return yyt;
}

tTree mnull_stmt
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynull_stmt));
 Tree_InitHead (yyt, knull_stmt)
 begintTree(yyt->null_stmt.env)
 return yyt;
}

tTree mswitch_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yswitch_list));
 Tree_InitHead (yyt, kswitch_list)
 begintTree(yyt->switch_list.env)
 return yyt;
}

tTree mnoswitch
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoswitch));
 Tree_InitHead (yyt, knoswitch)
 begintTree(yyt->noswitch.env)
 return yyt;
}

tTree mswitch_
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pexpression_list, tTree pstatement_list)
#else
(pnext, pexpression_list, pstatement_list)
tTree pnext;
tTree pexpression_list;
tTree pstatement_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yswitch_));
 Tree_InitHead (yyt, kswitch_)
 begintTree(yyt->switch_.env)
 yyt->switch_.next = pnext;
 yyt->switch_.expression_list = pexpression_list;
 yyt->switch_.statement_list = pstatement_list;
 return yyt;
}

tTree mcatch_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycatch_list));
 Tree_InitHead (yyt, kcatch_list)
 begintTree(yyt->catch_list.env)
 return yyt;
}

tTree mnocatch
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynocatch));
 Tree_InitHead (yyt, knocatch)
 begintTree(yyt->nocatch.env)
 return yyt;
}

tTree mcatch
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pdecl_list, tTree pstatement)
#else
(pnext, pdecl_list, pstatement)
tTree pnext;
tTree pdecl_list;
tTree pstatement;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycatch));
 Tree_InitHead (yyt, kcatch)
 begintTree(yyt->catch.env)
 yyt->catch.next = pnext;
 yyt->catch.decl_list = pdecl_list;
 yyt->catch.statement = pstatement;
 return yyt;
}

tTree mtype_name_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_name_list));
 Tree_InitHead (yyt, ktype_name_list)
 begintTree(yyt->type_name_list.env)
 return yyt;
}

tTree mnotype_name
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynotype_name));
 Tree_InitHead (yyt, knotype_name)
 begintTree(yyt->notype_name.env)
 return yyt;
}

tTree mtype_name
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pnamed_type)
#else
(pnext, pnamed_type)
tTree pnext;
tTree pnamed_type;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_name));
 Tree_InitHead (yyt, ktype_name)
 begintTree(yyt->type_name.env)
 yyt->type_name.next = pnext;
 yyt->type_name.named_type = pnamed_type;
 return yyt;
}

tTree mtype
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype));
 Tree_InitHead (yyt, ktype)
 begintTree(yyt->type.env)
 return yyt;
}

tTree msimple_type
#if defined __STDC__ | defined __cplusplus
(ttype ptype)
#else
(ptype)
ttype ptype;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ysimple_type));
 Tree_InitHead (yyt, ksimple_type)
 begintTree(yyt->simple_type.env)
 yyt->simple_type.type = ptype;
 return yyt;
}

tTree mnamed_type
#if defined __STDC__ | defined __cplusplus
(tTree pqualified_symbol)
#else
(pqualified_symbol)
tTree pqualified_symbol;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynamed_type));
 Tree_InitHead (yyt, knamed_type)
 begintTree(yyt->named_type.env)
 yyt->named_type.qualified_symbol = pqualified_symbol;
 return yyt;
}

tTree marray_type
#if defined __STDC__ | defined __cplusplus
(tTree ptype)
#else
(ptype)
tTree ptype;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yarray_type));
 Tree_InitHead (yyt, karray_type)
 begintTree(yyt->array_type.env)
 yyt->array_type.type = ptype;
 return yyt;
}

tTree mnotype
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynotype));
 Tree_InitHead (yyt, knotype)
 begintTree(yyt->notype.env)
 return yyt;
}

tTree mexpression_list
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yexpression_list));
 Tree_InitHead (yyt, kexpression_list)
 begintTree(yyt->expression_list.env)
 beginshort(yyt->expression_list.no_of_args)
 return yyt;
}

tTree mnoexpression_l
#if defined __STDC__ | defined __cplusplus
(void)
#else
()
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoexpression_l));
 Tree_InitHead (yyt, knoexpression_l)
 begintTree(yyt->noexpression_l.env)
 beginshort(yyt->noexpression_l.no_of_args)
 return yyt;
}

tTree mexpression
#if defined __STDC__ | defined __cplusplus
(tTree pnext, tTree pexpression)
#else
(pnext, pexpression)
tTree pnext;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yexpression));
 Tree_InitHead (yyt, kexpression)
 begintTree(yyt->expression.env)
 beginshort(yyt->expression.no_of_args)
 yyt->expression.next = pnext;
 yyt->expression.expression = pexpression;
 return yyt;
}

tTree mexpression_c
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yexpression_c));
 Tree_InitHead (yyt, kexpression_c)
 yyt->expression_c.pos = ppos;
 begintTree(yyt->expression_c.env)
 return yyt;
}

tTree mqualified_symbol
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqualified_symbol));
 Tree_InitHead (yyt, kqualified_symbol)
 yyt->qualified_symbol.pos = ppos;
 begintTree(yyt->qualified_symbol.env)
 begintTree(yyt->qualified_symbol.object)
 return yyt;
}

tTree mqualification
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pqualified_symbol, tIdent pident)
#else
(ppos, pqualified_symbol, pident)
tPosition ppos;
tTree pqualified_symbol;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yqualification));
 Tree_InitHead (yyt, kqualification)
 yyt->qualification.pos = ppos;
 begintTree(yyt->qualification.env)
 begintTree(yyt->qualification.object)
 yyt->qualification.qualified_symbol = pqualified_symbol;
 yyt->qualification.ident = pident;
 return yyt;
}

tTree mident
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tIdent pident)
#else
(ppos, pident)
tPosition ppos;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yident));
 Tree_InitHead (yyt, kident)
 yyt->ident.pos = ppos;
 begintTree(yyt->ident.env)
 begintTree(yyt->ident.object)
 yyt->ident.ident = pident;
 return yyt;
}

tTree mnoexpression
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynoexpression));
 Tree_InitHead (yyt, knoexpression)
 yyt->noexpression.pos = ppos;
 begintTree(yyt->noexpression.env)
 begintTree(yyt->noexpression.object)
 return yyt;
}

tTree munary
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression, toperator poperator)
#else
(ppos, pexpression, poperator)
tPosition ppos;
tTree pexpression;
toperator poperator;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yunary));
 Tree_InitHead (yyt, kunary)
 yyt->unary.pos = ppos;
 begintTree(yyt->unary.env)
 yyt->unary.expression = pexpression;
 yyt->unary.operator = poperator;
 return yyt;
}

tTree mbinary
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree plop, tTree prop, toperator poperator)
#else
(ppos, plop, prop, poperator)
tPosition ppos;
tTree plop;
tTree prop;
toperator poperator;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ybinary));
 Tree_InitHead (yyt, kbinary)
 yyt->binary.pos = ppos;
 begintTree(yyt->binary.env)
 yyt->binary.lop = plop;
 yyt->binary.rop = prop;
 yyt->binary.operator = poperator;
 return yyt;
}

tTree massign
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree plval, tTree prval, toperator poperator)
#else
(ppos, plval, prval, poperator)
tPosition ppos;
tTree plval;
tTree prval;
toperator poperator;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yassign));
 Tree_InitHead (yyt, kassign)
 yyt->assign.pos = ppos;
 begintTree(yyt->assign.env)
 yyt->assign.lval = plval;
 yyt->assign.rval = prval;
 yyt->assign.operator = poperator;
 return yyt;
}

tTree maggregate
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression_list)
#else
(ppos, pexpression_list)
tPosition ppos;
tTree pexpression_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yaggregate));
 Tree_InitHead (yyt, kaggregate)
 yyt->aggregate.pos = ppos;
 begintTree(yyt->aggregate.env)
 yyt->aggregate.expression_list = pexpression_list;
 return yyt;
}

tTree mcall
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression, tTree pexpression_list)
#else
(ppos, pexpression, pexpression_list)
tPosition ppos;
tTree pexpression;
tTree pexpression_list;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ycall));
 Tree_InitHead (yyt, kcall)
 yyt->call.pos = ppos;
 begintTree(yyt->call.env)
 yyt->call.expression = pexpression;
 yyt->call.expression_list = pexpression_list;
 return yyt;
}

tTree mselect
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression, tIdent pident)
#else
(ppos, pexpression, pident)
tPosition ppos;
tTree pexpression;
tIdent pident;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yselect));
 Tree_InitHead (yyt, kselect)
 yyt->select.pos = ppos;
 begintTree(yyt->select.env)
 yyt->select.expression = pexpression;
 yyt->select.ident = pident;
 return yyt;
}

tTree mget_class_of_expr
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression)
#else
(ppos, pexpression)
tPosition ppos;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yget_class_of_expr));
 Tree_InitHead (yyt, kget_class_of_expr)
 yyt->get_class_of_expr.pos = ppos;
 begintTree(yyt->get_class_of_expr.env)
 yyt->get_class_of_expr.expression = pexpression;
 return yyt;
}

tTree mget_class
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptype)
#else
(ppos, ptype)
tPosition ppos;
tTree ptype;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yget_class));
 Tree_InitHead (yyt, kget_class)
 yyt->get_class.pos = ppos;
 begintTree(yyt->get_class.env)
 yyt->get_class.type = ptype;
 return yyt;
}

tTree msubscript
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pbase, tTree pindex)
#else
(ppos, pbase, pindex)
tPosition ppos;
tTree pbase;
tTree pindex;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ysubscript));
 Tree_InitHead (yyt, ksubscript)
 yyt->subscript.pos = ppos;
 begintTree(yyt->subscript.env)
 yyt->subscript.base = pbase;
 yyt->subscript.index = pindex;
 return yyt;
}

tTree mtype_compare
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pexpression, tTree ptype)
#else
(ppos, pexpression, ptype)
tPosition ppos;
tTree pexpression;
tTree ptype;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_compare));
 Tree_InitHead (yyt, ktype_compare)
 yyt->type_compare.pos = ppos;
 begintTree(yyt->type_compare.env)
 yyt->type_compare.expression = pexpression;
 yyt->type_compare.type = ptype;
 return yyt;
}

tTree mtype_cast
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptype, tTree pdims, tTree pexpression)
#else
(ppos, ptype, pdims, pexpression)
tPosition ppos;
tTree ptype;
tTree pdims;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ytype_cast));
 Tree_InitHead (yyt, ktype_cast)
 yyt->type_cast.pos = ppos;
 begintTree(yyt->type_cast.env)
 yyt->type_cast.type = ptype;
 yyt->type_cast.dims = pdims;
 yyt->type_cast.expression = pexpression;
 return yyt;
}

tTree mnew
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptype, tTree pdims, tTree pexpression_list, tTree pexpression)
#else
(ppos, ptype, pdims, pexpression_list, pexpression)
tPosition ppos;
tTree ptype;
tTree pdims;
tTree pexpression_list;
tTree pexpression;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynew));
 Tree_InitHead (yyt, knew)
 yyt->new.pos = ppos;
 begintTree(yyt->new.env)
 yyt->new.type = ptype;
 yyt->new.dims = pdims;
 yyt->new.expression_list = pexpression_list;
 yyt->new.expression = pexpression;
 return yyt;
}

tTree manonymous
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree ptype, tTree pexpression_list, tTree pblock)
#else
(ppos, ptype, pexpression_list, pblock)
tPosition ppos;
tTree ptype;
tTree pexpression_list;
tTree pblock;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yanonymous));
 Tree_InitHead (yyt, kanonymous)
 yyt->anonymous.pos = ppos;
 begintTree(yyt->anonymous.env)
 yyt->anonymous.type = ptype;
 yyt->anonymous.expression_list = pexpression_list;
 yyt->anonymous.block = pblock;
 return yyt;
}

tTree mconditional
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tTree pcondition, tTree ptrue_expr, tTree pfalse_expr)
#else
(ppos, pcondition, ptrue_expr, pfalse_expr)
tPosition ppos;
tTree pcondition;
tTree ptrue_expr;
tTree pfalse_expr;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yconditional));
 Tree_InitHead (yyt, kconditional)
 yyt->conditional.pos = ppos;
 begintTree(yyt->conditional.env)
 yyt->conditional.condition = pcondition;
 yyt->conditional.true_expr = ptrue_expr;
 yyt->conditional.false_expr = pfalse_expr;
 return yyt;
}

tTree msuper
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ysuper));
 Tree_InitHead (yyt, ksuper)
 yyt->super.pos = ppos;
 begintTree(yyt->super.env)
 return yyt;
}

tTree mthis
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ythis));
 Tree_InitHead (yyt, kthis)
 yyt->this.pos = ppos;
 begintTree(yyt->this.env)
 return yyt;
}

tTree mnull
#if defined __STDC__ | defined __cplusplus
(tPosition ppos)
#else
(ppos)
tPosition ppos;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ynull));
 Tree_InitHead (yyt, knull)
 yyt->null.pos = ppos;
 begintTree(yyt->null.env)
 return yyt;
}

tTree mint_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, long pvalue)
#else
(ppos, pvalue)
tPosition ppos;
long pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yint_literal));
 Tree_InitHead (yyt, kint_literal)
 yyt->int_literal.pos = ppos;
 begintTree(yyt->int_literal.env)
 yyt->int_literal.value = pvalue;
 return yyt;
}

tTree mlong_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, long pvalue)
#else
(ppos, pvalue)
tPosition ppos;
long pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ylong_literal));
 Tree_InitHead (yyt, klong_literal)
 yyt->long_literal.pos = ppos;
 begintTree(yyt->long_literal.env)
 yyt->long_literal.value = pvalue;
 return yyt;
}

tTree mbool_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, rbool pvalue)
#else
(ppos, pvalue)
tPosition ppos;
rbool pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ybool_literal));
 Tree_InitHead (yyt, kbool_literal)
 yyt->bool_literal.pos = ppos;
 begintTree(yyt->bool_literal.env)
 yyt->bool_literal.value = pvalue;
 return yyt;
}

tTree mfloat_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tStringRef pvalue)
#else
(ppos, pvalue)
tPosition ppos;
tStringRef pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yfloat_literal));
 Tree_InitHead (yyt, kfloat_literal)
 yyt->float_literal.pos = ppos;
 begintTree(yyt->float_literal.env)
 yyt->float_literal.value = pvalue;
 return yyt;
}

tTree mchar_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tStringRef pvalue)
#else
(ppos, pvalue)
tPosition ppos;
tStringRef pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ychar_literal));
 Tree_InitHead (yyt, kchar_literal)
 yyt->char_literal.pos = ppos;
 begintTree(yyt->char_literal.env)
 yyt->char_literal.value = pvalue;
 return yyt;
}

tTree mstring_literal
#if defined __STDC__ | defined __cplusplus
(tPosition ppos, tStringRef pvalue)
#else
(ppos, pvalue)
tPosition ppos;
tStringRef pvalue;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (ystring_literal));
 Tree_InitHead (yyt, kstring_literal)
 yyt->string_literal.pos = ppos;
 begintTree(yyt->string_literal.env)
 yyt->string_literal.value = pvalue;
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
(tTree pobjects, tTree penv, tTree pobject, tHashTable pHashTable, int pHashSize)
#else
(pobjects, penv, pobject, pHashTable, pHashSize)
tTree pobjects;
tTree penv;
tTree pobject;
tHashTable pHashTable;
int pHashSize;
#endif
{
 register tTree yyt = yyALLOCt (sizeof (yenv));
 Tree_InitHead (yyt, kenv)
 yyt->env.objects = pobjects;
 yyt->env.env = penv;
 yyt->env.object = pobject;
 yyt->env.HashTable = pHashTable;
 yyt->env.HashSize = pHashSize;
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
 begintTree(yyt->object.collision)
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
case kcompilation_unit:
yyMark (yyt->compilation_unit.package);
yyMark (yyt->compilation_unit.import_list);
yyt = yyt->compilation_unit.field_list; break;
case kimport:
yyMark (yyt->import.qualified_symbol);
yyt = yyt->import.next; break;
case kimport_asterisk:
yyMark (yyt->import_asterisk.qualified_symbol);
yyt = yyt->import_asterisk.next; break;
case kfield:
yyt = yyt->field.next; break;
case kfunction:
yyMark (yyt->function.decl_list);
yyMark (yyt->function.throws);
yyMark (yyt->function.block);
yyt = yyt->function.next; break;
case kmethod:
yyMark (yyt->method.decl_list);
yyMark (yyt->method.throws);
yyMark (yyt->method.block);
yyMark (yyt->method.type);
yyMark (yyt->method.array);
yyt = yyt->method.next; break;
case kconstructor:
yyMark (yyt->constructor.decl_list);
yyMark (yyt->constructor.throws);
yyMark (yyt->constructor.block);
yyt = yyt->constructor.next; break;
case kvar_decl:
yyMark (yyt->var_decl.type);
yyMark (yyt->var_decl.decl_list);
yyt = yyt->var_decl.next; break;
case kstatic_initializer:
yyMark (yyt->static_initializer.statement_c);
yyt = yyt->static_initializer.next; break;
case kinitializer:
yyMark (yyt->initializer.statement_c);
yyt = yyt->initializer.next; break;
case ktype_decl:
yyMark (yyt->type_decl.block);
yyt = yyt->type_decl.next; break;
case kclass:
yyMark (yyt->class.block);
yyMark (yyt->class.extends);
yyMark (yyt->class.interfaces);
yyt = yyt->class.next; break;
case kinterface:
yyMark (yyt->my_interface.block);
yyMark (yyt->my_interface.extends);
yyt = yyt->my_interface.next; break;
case kdecl:
yyMark (yyt->decl.type);
yyMark (yyt->decl.array);
yyt = yyt->decl.next; break;
case kvariable:
yyMark (yyt->variable.type);
yyMark (yyt->variable.array);
yyMark (yyt->variable.expression);
yyt = yyt->variable.next; break;
case kparameter:
yyMark (yyt->parameter.type);
yyMark (yyt->parameter.array);
yyt = yyt->parameter.next; break;
case kstatement:
yyMark (yyt->statement.statement);
yyt = yyt->statement.next; break;
case kcompound_stmt:
yyt = yyt->compound_stmt.statement_list; break;
case kexpression_stmt:
yyt = yyt->expression_stmt.expression; break;
case kvar_decl_stmt:
yyMark (yyt->var_decl_stmt.type);
yyt = yyt->var_decl_stmt.decl_list; break;
case ktype_decl_stmt:
yyt = yyt->type_decl_stmt.type_decl; break;
case kif_stmt:
yyMark (yyt->if_stmt.expression);
yyt = yyt->if_stmt.then; break;
case kif_else_stmt:
yyMark (yyt->if_else_stmt.expression);
yyMark (yyt->if_else_stmt.then);
yyt = yyt->if_else_stmt.Else; break;
case kwhile_stmt:
yyMark (yyt->while_stmt.expression);
yyt = yyt->while_stmt.statement; break;
case kdo_stmt:
yyMark (yyt->do_stmt.statement);
yyt = yyt->do_stmt.expression; break;
case kbreak_id_stmt:
yyt = yyt->break_id_stmt.expression; break;
case kcontinue_id_stmt:
yyt = yyt->continue_id_stmt.expression; break;
case kreturn_expr_stmt:
yyt = yyt->return_expr_stmt.expression; break;
case kfor_stmt:
yyMark (yyt->for_stmt.for_init);
yyMark (yyt->for_stmt.expression);
yyMark (yyt->for_stmt.for_incr);
yyt = yyt->for_stmt.statement; break;
case kthrow_stmt:
yyt = yyt->throw_stmt.expression; break;
case ksynchronized_stmt:
yyMark (yyt->synchronized_stmt.expression);
yyt = yyt->synchronized_stmt.statement; break;
case klabeled_stmt:
yyt = yyt->labeled_stmt.statement; break;
case ktry_stmt:
yyMark (yyt->try_stmt.statement);
yyMark (yyt->try_stmt.catch_list);
yyt = yyt->try_stmt.finally; break;
case kswitch_stmt:
yyMark (yyt->switch_stmt.expression);
yyt = yyt->switch_stmt.switch_list; break;
case kswitch_:
yyMark (yyt->switch_.expression_list);
yyMark (yyt->switch_.statement_list);
yyt = yyt->switch_.next; break;
case kcatch:
yyMark (yyt->catch.decl_list);
yyMark (yyt->catch.statement);
yyt = yyt->catch.next; break;
case ktype_name:
yyMark (yyt->type_name.named_type);
yyt = yyt->type_name.next; break;
case knamed_type:
yyt = yyt->named_type.qualified_symbol; break;
case karray_type:
yyt = yyt->array_type.type; break;
case kexpression:
yyMark (yyt->expression.expression);
yyt = yyt->expression.next; break;
case kqualification:
yyt = yyt->qualification.qualified_symbol; break;
case kunary:
yyt = yyt->unary.expression; break;
case kbinary:
yyMark (yyt->binary.lop);
yyt = yyt->binary.rop; break;
case kassign:
yyMark (yyt->assign.lval);
yyt = yyt->assign.rval; break;
case kaggregate:
yyt = yyt->aggregate.expression_list; break;
case kcall:
yyMark (yyt->call.expression);
yyt = yyt->call.expression_list; break;
case kselect:
yyt = yyt->select.expression; break;
case kget_class_of_expr:
yyt = yyt->get_class_of_expr.expression; break;
case kget_class:
yyt = yyt->get_class.type; break;
case ksubscript:
yyMark (yyt->subscript.base);
yyt = yyt->subscript.index; break;
case ktype_compare:
yyMark (yyt->type_compare.expression);
yyt = yyt->type_compare.type; break;
case ktype_cast:
yyMark (yyt->type_cast.type);
yyMark (yyt->type_cast.dims);
yyt = yyt->type_cast.expression; break;
case knew:
yyMark (yyt->new.type);
yyMark (yyt->new.dims);
yyMark (yyt->new.expression_list);
yyt = yyt->new.expression; break;
case kanonymous:
yyMark (yyt->anonymous.type);
yyMark (yyt->anonymous.expression_list);
yyt = yyt->anonymous.block; break;
case kconditional:
yyMark (yyt->conditional.condition);
yyMark (yyt->conditional.true_expr);
yyt = yyt->conditional.false_expr; break;
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

static void yWriteNodecompilation_unit
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("package");
 yyWriteAdr (yyt->compilation_unit.package);
 yyWriteSelector ("import_list");
 yyWriteAdr (yyt->compilation_unit.import_list);
 yyWriteSelector ("field_list");
 yyWriteAdr (yyt->compilation_unit.field_list);
}

static void yWriteNodeimport_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->import_list.env) yyWriteNl ();
}

static void yWriteNodenoimport
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeimport_list (yyt);
}

static void yWriteNodeimport
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeimport_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->import.next);
 yyWriteSelector ("qualified_symbol");
 yyWriteAdr (yyt->import.qualified_symbol);
}

static void yWriteNodeimport_asterisk
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeimport (yyt);
}

static void yWriteNodefield_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->field_list.env) yyWriteNl ();
}

static void yWriteNodenofield
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield_list (yyt);
}

static void yWriteNodefield
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->field.next);
}

static void yWriteNodefunction
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield (yyt);
 yyWriteSelector ("modifiers");
 writelong (yyt->function.modifiers) yyWriteNl ();
 yyWriteSelector ("ident");
 writetIdent (yyt->function.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->function.pos) yyWriteNl ();
 yyWriteSelector ("decl_list");
 yyWriteAdr (yyt->function.decl_list);
 yyWriteSelector ("throws");
 yyWriteAdr (yyt->function.throws);
 yyWriteSelector ("block");
 yyWriteAdr (yyt->function.block);
}

static void yWriteNodemethod
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefunction (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->method.type);
 yyWriteSelector ("array");
 yyWriteAdr (yyt->method.array);
}

static void yWriteNodeconstructor
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefunction (yyt);
}

static void yWriteNodevar_decl
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield (yyt);
 yyWriteSelector ("modifiers");
 writelong (yyt->var_decl.modifiers) yyWriteNl ();
 yyWriteSelector ("type");
 yyWriteAdr (yyt->var_decl.type);
 yyWriteSelector ("decl_list");
 yyWriteAdr (yyt->var_decl.decl_list);
}

static void yWriteNodestatic_initializer
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield (yyt);
 yyWriteSelector ("statement_c");
 yyWriteAdr (yyt->static_initializer.statement_c);
}

static void yWriteNodeinitializer
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield (yyt);
 yyWriteSelector ("statement_c");
 yyWriteAdr (yyt->initializer.statement_c);
}

static void yWriteNodetype_decl
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodefield (yyt);
 yyWriteSelector ("modifiers");
 writelong (yyt->type_decl.modifiers) yyWriteNl ();
 yyWriteSelector ("ident");
 writetIdent (yyt->type_decl.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->type_decl.pos) yyWriteNl ();
 yyWriteSelector ("block");
 yyWriteAdr (yyt->type_decl.block);
}

static void yWriteNodeclass
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype_decl (yyt);
 yyWriteSelector ("extends");
 yyWriteAdr (yyt->class.extends);
 yyWriteSelector ("interfaces");
 yyWriteAdr (yyt->class.interfaces);
}

static void yWriteNodeinterface
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype_decl (yyt);
 yyWriteSelector ("extends");
 yyWriteAdr (yyt->my_interface.extends);
}

static void yWriteNodedecl_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->decl_list.env) yyWriteNl ();
 yyWriteSelector ("no_of_args");
 writeshort (yyt->decl_list.no_of_args) yyWriteNl ();
}

static void yWriteNodenodecl
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodedecl_list (yyt);
}

static void yWriteNodedecl
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodedecl_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->decl.next);
 yyWriteSelector ("modifiers");
 writelong (yyt->decl.modifiers) yyWriteNl ();
 yyWriteSelector ("type");
 yyWriteAdr (yyt->decl.type);
 yyWriteSelector ("ident");
 writetIdent (yyt->decl.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->decl.pos) yyWriteNl ();
 yyWriteSelector ("array");
 yyWriteAdr (yyt->decl.array);
}

static void yWriteNodevariable
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodedecl (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->variable.expression);
}

static void yWriteNodeparameter
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodedecl (yyt);
}

static void yWriteNodestatement_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->statement_list.env) yyWriteNl ();
}

static void yWriteNodenostatement
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_list (yyt);
}

static void yWriteNodestatement
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->statement.next);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->statement.statement);
}

static void yWriteNodestatement_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->statement_c.env) yyWriteNl ();
}

static void yWriteNodecompound_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("statement_list");
 yyWriteAdr (yyt->compound_stmt.statement_list);
}

static void yWriteNodeexpression_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->expression_stmt.expression);
}

static void yWriteNodevar_decl_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->var_decl_stmt.type);
 yyWriteSelector ("decl_list");
 yyWriteAdr (yyt->var_decl_stmt.decl_list);
}

static void yWriteNodetype_decl_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("type_decl");
 yyWriteAdr (yyt->type_decl_stmt.type_decl);
}

static void yWriteNodeif_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->if_stmt.expression);
 yyWriteSelector ("then");
 yyWriteAdr (yyt->if_stmt.then);
}

static void yWriteNodeif_else_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeif_stmt (yyt);
 yyWriteSelector ("Else");
 yyWriteAdr (yyt->if_else_stmt.Else);
}

static void yWriteNodewhile_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->while_stmt.expression);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->while_stmt.statement);
}

static void yWriteNodedo_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->do_stmt.statement);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->do_stmt.expression);
}

static void yWriteNodebreak_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
}

static void yWriteNodebreak_id_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodebreak_stmt (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->break_id_stmt.expression);
}

static void yWriteNodecontinue_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
}

static void yWriteNodecontinue_id_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodecontinue_stmt (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->continue_id_stmt.expression);
}

static void yWriteNodereturn_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
}

static void yWriteNodereturn_expr_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodereturn_stmt (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->return_expr_stmt.expression);
}

static void yWriteNodefor_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("for_init");
 yyWriteAdr (yyt->for_stmt.for_init);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->for_stmt.expression);
 yyWriteSelector ("for_incr");
 yyWriteAdr (yyt->for_stmt.for_incr);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->for_stmt.statement);
}

static void yWriteNodethrow_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->throw_stmt.expression);
}

static void yWriteNodesynchronized_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->synchronized_stmt.expression);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->synchronized_stmt.statement);
}

static void yWriteNodelabeled_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->labeled_stmt.ident) yyWriteNl ();
 yyWriteSelector ("pos");
 writetPosition (yyt->labeled_stmt.pos) yyWriteNl ();
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->labeled_stmt.statement);
}

static void yWriteNodetry_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->try_stmt.statement);
 yyWriteSelector ("catch_list");
 yyWriteAdr (yyt->try_stmt.catch_list);
 yyWriteSelector ("finally");
 yyWriteAdr (yyt->try_stmt.finally);
}

static void yWriteNodeswitch_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->switch_stmt.expression);
 yyWriteSelector ("switch_list");
 yyWriteAdr (yyt->switch_stmt.switch_list);
}

static void yWriteNodenull_stmt
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodestatement_c (yyt);
}

static void yWriteNodeswitch_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->switch_list.env) yyWriteNl ();
}

static void yWriteNodenoswitch
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeswitch_list (yyt);
}

static void yWriteNodeswitch_
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeswitch_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->switch_.next);
 yyWriteSelector ("expression_list");
 yyWriteAdr (yyt->switch_.expression_list);
 yyWriteSelector ("statement_list");
 yyWriteAdr (yyt->switch_.statement_list);
}

static void yWriteNodecatch_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->catch_list.env) yyWriteNl ();
}

static void yWriteNodenocatch
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodecatch_list (yyt);
}

static void yWriteNodecatch
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodecatch_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->catch.next);
 yyWriteSelector ("decl_list");
 yyWriteAdr (yyt->catch.decl_list);
 yyWriteSelector ("statement");
 yyWriteAdr (yyt->catch.statement);
}

static void yWriteNodetype_name_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->type_name_list.env) yyWriteNl ();
}

static void yWriteNodenotype_name
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype_name_list (yyt);
}

static void yWriteNodetype_name
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype_name_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->type_name.next);
 yyWriteSelector ("named_type");
 yyWriteAdr (yyt->type_name.named_type);
}

static void yWriteNodetype
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->type.env) yyWriteNl ();
}

static void yWriteNodesimple_type
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype (yyt);
 yyWriteSelector ("type");
 writettype (yyt->simple_type.type) yyWriteNl ();
}

static void yWriteNodenamed_type
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype (yyt);
 yyWriteSelector ("qualified_symbol");
 yyWriteAdr (yyt->named_type.qualified_symbol);
}

static void yWriteNodearray_type
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->array_type.type);
}

static void yWriteNodenotype
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodetype (yyt);
}

static void yWriteNodeexpression_list
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("env");
 writetTree (yyt->expression_list.env) yyWriteNl ();
 yyWriteSelector ("no_of_args");
 writeshort (yyt->expression_list.no_of_args) yyWriteNl ();
}

static void yWriteNodenoexpression_l
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_list (yyt);
}

static void yWriteNodeexpression
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_list (yyt);
 yyWriteSelector ("next");
 yyWriteAdr (yyt->expression.next);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->expression.expression);
}

static void yWriteNodeexpression_c
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yyWriteSelector ("pos");
 writetPosition (yyt->expression_c.pos) yyWriteNl ();
 yyWriteSelector ("env");
 writetTree (yyt->expression_c.env) yyWriteNl ();
}

static void yWriteNodequalified_symbol
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("object");
 writetTree (yyt->qualified_symbol.object) yyWriteNl ();
}

static void yWriteNodequalification
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalified_symbol (yyt);
 yyWriteSelector ("qualified_symbol");
 yyWriteAdr (yyt->qualification.qualified_symbol);
 yyWriteSelector ("ident");
 writetIdent (yyt->qualification.ident) yyWriteNl ();
}

static void yWriteNodeident
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalified_symbol (yyt);
 yyWriteSelector ("ident");
 writetIdent (yyt->ident.ident) yyWriteNl ();
}

static void yWriteNodenoexpression
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodequalified_symbol (yyt);
}

static void yWriteNodeunary
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->unary.expression);
 yyWriteSelector ("operator");
 writetoperator (yyt->unary.operator) yyWriteNl ();
}

static void yWriteNodebinary
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("lop");
 yyWriteAdr (yyt->binary.lop);
 yyWriteSelector ("rop");
 yyWriteAdr (yyt->binary.rop);
 yyWriteSelector ("operator");
 writetoperator (yyt->binary.operator) yyWriteNl ();
}

static void yWriteNodeassign
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("lval");
 yyWriteAdr (yyt->assign.lval);
 yyWriteSelector ("rval");
 yyWriteAdr (yyt->assign.rval);
 yyWriteSelector ("operator");
 writetoperator (yyt->assign.operator) yyWriteNl ();
}

static void yWriteNodeaggregate
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression_list");
 yyWriteAdr (yyt->aggregate.expression_list);
}

static void yWriteNodecall
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->call.expression);
 yyWriteSelector ("expression_list");
 yyWriteAdr (yyt->call.expression_list);
}

static void yWriteNodeselect
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->select.expression);
 yyWriteSelector ("ident");
 writetIdent (yyt->select.ident) yyWriteNl ();
}

static void yWriteNodeget_class_of_expr
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->get_class_of_expr.expression);
}

static void yWriteNodeget_class
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->get_class.type);
}

static void yWriteNodesubscript
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("base");
 yyWriteAdr (yyt->subscript.base);
 yyWriteSelector ("index");
 yyWriteAdr (yyt->subscript.index);
}

static void yWriteNodetype_compare
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->type_compare.expression);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->type_compare.type);
}

static void yWriteNodetype_cast
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->type_cast.type);
 yyWriteSelector ("dims");
 yyWriteAdr (yyt->type_cast.dims);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->type_cast.expression);
}

static void yWriteNodenew
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->new.type);
 yyWriteSelector ("dims");
 yyWriteAdr (yyt->new.dims);
 yyWriteSelector ("expression_list");
 yyWriteAdr (yyt->new.expression_list);
 yyWriteSelector ("expression");
 yyWriteAdr (yyt->new.expression);
}

static void yWriteNodeanonymous
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("type");
 yyWriteAdr (yyt->anonymous.type);
 yyWriteSelector ("expression_list");
 yyWriteAdr (yyt->anonymous.expression_list);
 yyWriteSelector ("block");
 yyWriteAdr (yyt->anonymous.block);
}

static void yWriteNodeconditional
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("condition");
 yyWriteAdr (yyt->conditional.condition);
 yyWriteSelector ("true_expr");
 yyWriteAdr (yyt->conditional.true_expr);
 yyWriteSelector ("false_expr");
 yyWriteAdr (yyt->conditional.false_expr);
}

static void yWriteNodesuper
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
}

static void yWriteNodethis
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
}

static void yWriteNodenull
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
}

static void yWriteNodeint_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writelong (yyt->int_literal.value) yyWriteNl ();
}

static void yWriteNodelong_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writelong (yyt->long_literal.value) yyWriteNl ();
}

static void yWriteNodebool_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writerbool (yyt->bool_literal.value) yyWriteNl ();
}

static void yWriteNodefloat_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writetStringRef (yyt->float_literal.value) yyWriteNl ();
}

static void yWriteNodechar_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writetStringRef (yyt->char_literal.value) yyWriteNl ();
}

static void yWriteNodestring_literal
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{
 yWriteNodeexpression_c (yyt);
 yyWriteSelector ("value");
 writetStringRef (yyt->string_literal.value) yyWriteNl ();
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
 yyWriteSelector ("collision");
 writetTree (yyt->object.collision) yyWriteNl ();
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
case kcompilation_unit:
 yWriteNodecompilation_unit (yyt); break;
case kimport_list:
 yWriteNodeimport_list (yyt); break;
case knoimport:
 yWriteNodenoimport (yyt); break;
case kimport:
 yWriteNodeimport (yyt); break;
case kimport_asterisk:
 yWriteNodeimport_asterisk (yyt); break;
case kfield_list:
 yWriteNodefield_list (yyt); break;
case knofield:
 yWriteNodenofield (yyt); break;
case kfield:
 yWriteNodefield (yyt); break;
case kfunction:
 yWriteNodefunction (yyt); break;
case kmethod:
 yWriteNodemethod (yyt); break;
case kconstructor:
 yWriteNodeconstructor (yyt); break;
case kvar_decl:
 yWriteNodevar_decl (yyt); break;
case kstatic_initializer:
 yWriteNodestatic_initializer (yyt); break;
case kinitializer:
 yWriteNodeinitializer (yyt); break;
case ktype_decl:
 yWriteNodetype_decl (yyt); break;
case kclass:
 yWriteNodeclass (yyt); break;
case kinterface:
 yWriteNodeinterface (yyt); break;
case kdecl_list:
 yWriteNodedecl_list (yyt); break;
case knodecl:
 yWriteNodenodecl (yyt); break;
case kdecl:
 yWriteNodedecl (yyt); break;
case kvariable:
 yWriteNodevariable (yyt); break;
case kparameter:
 yWriteNodeparameter (yyt); break;
case kstatement_list:
 yWriteNodestatement_list (yyt); break;
case knostatement:
 yWriteNodenostatement (yyt); break;
case kstatement:
 yWriteNodestatement (yyt); break;
case kstatement_c:
 yWriteNodestatement_c (yyt); break;
case kcompound_stmt:
 yWriteNodecompound_stmt (yyt); break;
case kexpression_stmt:
 yWriteNodeexpression_stmt (yyt); break;
case kvar_decl_stmt:
 yWriteNodevar_decl_stmt (yyt); break;
case ktype_decl_stmt:
 yWriteNodetype_decl_stmt (yyt); break;
case kif_stmt:
 yWriteNodeif_stmt (yyt); break;
case kif_else_stmt:
 yWriteNodeif_else_stmt (yyt); break;
case kwhile_stmt:
 yWriteNodewhile_stmt (yyt); break;
case kdo_stmt:
 yWriteNodedo_stmt (yyt); break;
case kbreak_stmt:
 yWriteNodebreak_stmt (yyt); break;
case kbreak_id_stmt:
 yWriteNodebreak_id_stmt (yyt); break;
case kcontinue_stmt:
 yWriteNodecontinue_stmt (yyt); break;
case kcontinue_id_stmt:
 yWriteNodecontinue_id_stmt (yyt); break;
case kreturn_stmt:
 yWriteNodereturn_stmt (yyt); break;
case kreturn_expr_stmt:
 yWriteNodereturn_expr_stmt (yyt); break;
case kfor_stmt:
 yWriteNodefor_stmt (yyt); break;
case kthrow_stmt:
 yWriteNodethrow_stmt (yyt); break;
case ksynchronized_stmt:
 yWriteNodesynchronized_stmt (yyt); break;
case klabeled_stmt:
 yWriteNodelabeled_stmt (yyt); break;
case ktry_stmt:
 yWriteNodetry_stmt (yyt); break;
case kswitch_stmt:
 yWriteNodeswitch_stmt (yyt); break;
case knull_stmt:
 yWriteNodenull_stmt (yyt); break;
case kswitch_list:
 yWriteNodeswitch_list (yyt); break;
case knoswitch:
 yWriteNodenoswitch (yyt); break;
case kswitch_:
 yWriteNodeswitch_ (yyt); break;
case kcatch_list:
 yWriteNodecatch_list (yyt); break;
case knocatch:
 yWriteNodenocatch (yyt); break;
case kcatch:
 yWriteNodecatch (yyt); break;
case ktype_name_list:
 yWriteNodetype_name_list (yyt); break;
case knotype_name:
 yWriteNodenotype_name (yyt); break;
case ktype_name:
 yWriteNodetype_name (yyt); break;
case ktype:
 yWriteNodetype (yyt); break;
case ksimple_type:
 yWriteNodesimple_type (yyt); break;
case knamed_type:
 yWriteNodenamed_type (yyt); break;
case karray_type:
 yWriteNodearray_type (yyt); break;
case knotype:
 yWriteNodenotype (yyt); break;
case kexpression_list:
 yWriteNodeexpression_list (yyt); break;
case knoexpression_l:
 yWriteNodenoexpression_l (yyt); break;
case kexpression:
 yWriteNodeexpression (yyt); break;
case kexpression_c:
 yWriteNodeexpression_c (yyt); break;
case kqualified_symbol:
 yWriteNodequalified_symbol (yyt); break;
case kqualification:
 yWriteNodequalification (yyt); break;
case kident:
 yWriteNodeident (yyt); break;
case knoexpression:
 yWriteNodenoexpression (yyt); break;
case kunary:
 yWriteNodeunary (yyt); break;
case kbinary:
 yWriteNodebinary (yyt); break;
case kassign:
 yWriteNodeassign (yyt); break;
case kaggregate:
 yWriteNodeaggregate (yyt); break;
case kcall:
 yWriteNodecall (yyt); break;
case kselect:
 yWriteNodeselect (yyt); break;
case kget_class_of_expr:
 yWriteNodeget_class_of_expr (yyt); break;
case kget_class:
 yWriteNodeget_class (yyt); break;
case ksubscript:
 yWriteNodesubscript (yyt); break;
case ktype_compare:
 yWriteNodetype_compare (yyt); break;
case ktype_cast:
 yWriteNodetype_cast (yyt); break;
case knew:
 yWriteNodenew (yyt); break;
case kanonymous:
 yWriteNodeanonymous (yyt); break;
case kconditional:
 yWriteNodeconditional (yyt); break;
case ksuper:
 yWriteNodesuper (yyt); break;
case kthis:
 yWriteNodethis (yyt); break;
case knull:
 yWriteNodenull (yyt); break;
case kint_literal:
 yWriteNodeint_literal (yyt); break;
case klong_literal:
 yWriteNodelong_literal (yyt); break;
case kbool_literal:
 yWriteNodebool_literal (yyt); break;
case kfloat_literal:
 yWriteNodefloat_literal (yyt); break;
case kchar_literal:
 yWriteNodechar_literal (yyt); break;
case kstring_literal:
 yWriteNodestring_literal (yyt); break;
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
case kcompilation_unit:
yyTraverseTreeTD (yyt->compilation_unit.package);
yyTraverseTreeTD (yyt->compilation_unit.import_list);
yyt = yyt->compilation_unit.field_list; break;
case kimport:
yyTraverseTreeTD (yyt->import.qualified_symbol);
yyt = yyt->import.next; break;
case kimport_asterisk:
yyTraverseTreeTD (yyt->import_asterisk.qualified_symbol);
yyt = yyt->import_asterisk.next; break;
case kfield:
yyt = yyt->field.next; break;
case kfunction:
yyTraverseTreeTD (yyt->function.decl_list);
yyTraverseTreeTD (yyt->function.throws);
yyTraverseTreeTD (yyt->function.block);
yyt = yyt->function.next; break;
case kmethod:
yyTraverseTreeTD (yyt->method.decl_list);
yyTraverseTreeTD (yyt->method.throws);
yyTraverseTreeTD (yyt->method.block);
yyTraverseTreeTD (yyt->method.type);
yyTraverseTreeTD (yyt->method.array);
yyt = yyt->method.next; break;
case kconstructor:
yyTraverseTreeTD (yyt->constructor.decl_list);
yyTraverseTreeTD (yyt->constructor.throws);
yyTraverseTreeTD (yyt->constructor.block);
yyt = yyt->constructor.next; break;
case kvar_decl:
yyTraverseTreeTD (yyt->var_decl.type);
yyTraverseTreeTD (yyt->var_decl.decl_list);
yyt = yyt->var_decl.next; break;
case kstatic_initializer:
yyTraverseTreeTD (yyt->static_initializer.statement_c);
yyt = yyt->static_initializer.next; break;
case kinitializer:
yyTraverseTreeTD (yyt->initializer.statement_c);
yyt = yyt->initializer.next; break;
case ktype_decl:
yyTraverseTreeTD (yyt->type_decl.block);
yyt = yyt->type_decl.next; break;
case kclass:
yyTraverseTreeTD (yyt->class.block);
yyTraverseTreeTD (yyt->class.extends);
yyTraverseTreeTD (yyt->class.interfaces);
yyt = yyt->class.next; break;
case kinterface:
yyTraverseTreeTD (yyt->my_interface.block);
yyTraverseTreeTD (yyt->my_interface.extends);
yyt = yyt->my_interface.next; break;
case kdecl:
yyTraverseTreeTD (yyt->decl.type);
yyTraverseTreeTD (yyt->decl.array);
yyt = yyt->decl.next; break;
case kvariable:
yyTraverseTreeTD (yyt->variable.type);
yyTraverseTreeTD (yyt->variable.array);
yyTraverseTreeTD (yyt->variable.expression);
yyt = yyt->variable.next; break;
case kparameter:
yyTraverseTreeTD (yyt->parameter.type);
yyTraverseTreeTD (yyt->parameter.array);
yyt = yyt->parameter.next; break;
case kstatement:
yyTraverseTreeTD (yyt->statement.statement);
yyt = yyt->statement.next; break;
case kcompound_stmt:
yyt = yyt->compound_stmt.statement_list; break;
case kexpression_stmt:
yyt = yyt->expression_stmt.expression; break;
case kvar_decl_stmt:
yyTraverseTreeTD (yyt->var_decl_stmt.type);
yyt = yyt->var_decl_stmt.decl_list; break;
case ktype_decl_stmt:
yyt = yyt->type_decl_stmt.type_decl; break;
case kif_stmt:
yyTraverseTreeTD (yyt->if_stmt.expression);
yyt = yyt->if_stmt.then; break;
case kif_else_stmt:
yyTraverseTreeTD (yyt->if_else_stmt.expression);
yyTraverseTreeTD (yyt->if_else_stmt.then);
yyt = yyt->if_else_stmt.Else; break;
case kwhile_stmt:
yyTraverseTreeTD (yyt->while_stmt.expression);
yyt = yyt->while_stmt.statement; break;
case kdo_stmt:
yyTraverseTreeTD (yyt->do_stmt.statement);
yyt = yyt->do_stmt.expression; break;
case kbreak_id_stmt:
yyt = yyt->break_id_stmt.expression; break;
case kcontinue_id_stmt:
yyt = yyt->continue_id_stmt.expression; break;
case kreturn_expr_stmt:
yyt = yyt->return_expr_stmt.expression; break;
case kfor_stmt:
yyTraverseTreeTD (yyt->for_stmt.for_init);
yyTraverseTreeTD (yyt->for_stmt.expression);
yyTraverseTreeTD (yyt->for_stmt.for_incr);
yyt = yyt->for_stmt.statement; break;
case kthrow_stmt:
yyt = yyt->throw_stmt.expression; break;
case ksynchronized_stmt:
yyTraverseTreeTD (yyt->synchronized_stmt.expression);
yyt = yyt->synchronized_stmt.statement; break;
case klabeled_stmt:
yyt = yyt->labeled_stmt.statement; break;
case ktry_stmt:
yyTraverseTreeTD (yyt->try_stmt.statement);
yyTraverseTreeTD (yyt->try_stmt.catch_list);
yyt = yyt->try_stmt.finally; break;
case kswitch_stmt:
yyTraverseTreeTD (yyt->switch_stmt.expression);
yyt = yyt->switch_stmt.switch_list; break;
case kswitch_:
yyTraverseTreeTD (yyt->switch_.expression_list);
yyTraverseTreeTD (yyt->switch_.statement_list);
yyt = yyt->switch_.next; break;
case kcatch:
yyTraverseTreeTD (yyt->catch.decl_list);
yyTraverseTreeTD (yyt->catch.statement);
yyt = yyt->catch.next; break;
case ktype_name:
yyTraverseTreeTD (yyt->type_name.named_type);
yyt = yyt->type_name.next; break;
case knamed_type:
yyt = yyt->named_type.qualified_symbol; break;
case karray_type:
yyt = yyt->array_type.type; break;
case kexpression:
yyTraverseTreeTD (yyt->expression.expression);
yyt = yyt->expression.next; break;
case kqualification:
yyt = yyt->qualification.qualified_symbol; break;
case kunary:
yyt = yyt->unary.expression; break;
case kbinary:
yyTraverseTreeTD (yyt->binary.lop);
yyt = yyt->binary.rop; break;
case kassign:
yyTraverseTreeTD (yyt->assign.lval);
yyt = yyt->assign.rval; break;
case kaggregate:
yyt = yyt->aggregate.expression_list; break;
case kcall:
yyTraverseTreeTD (yyt->call.expression);
yyt = yyt->call.expression_list; break;
case kselect:
yyt = yyt->select.expression; break;
case kget_class_of_expr:
yyt = yyt->get_class_of_expr.expression; break;
case kget_class:
yyt = yyt->get_class.type; break;
case ksubscript:
yyTraverseTreeTD (yyt->subscript.base);
yyt = yyt->subscript.index; break;
case ktype_compare:
yyTraverseTreeTD (yyt->type_compare.expression);
yyt = yyt->type_compare.type; break;
case ktype_cast:
yyTraverseTreeTD (yyt->type_cast.type);
yyTraverseTreeTD (yyt->type_cast.dims);
yyt = yyt->type_cast.expression; break;
case knew:
yyTraverseTreeTD (yyt->new.type);
yyTraverseTreeTD (yyt->new.dims);
yyTraverseTreeTD (yyt->new.expression_list);
yyt = yyt->new.expression; break;
case kanonymous:
yyTraverseTreeTD (yyt->anonymous.type);
yyTraverseTreeTD (yyt->anonymous.expression_list);
yyt = yyt->anonymous.block; break;
case kconditional:
yyTraverseTreeTD (yyt->conditional.condition);
yyTraverseTreeTD (yyt->conditional.true_expr);
yyt = yyt->conditional.false_expr; break;
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
case kimport: yyNext = yyOld->import.next;
 yyOld->import.next = yyNew; break;
case kimport_asterisk: yyNext = yyOld->import_asterisk.next;
 yyOld->import_asterisk.next = yyNew; break;
case kfield: yyNext = yyOld->field.next;
 yyOld->field.next = yyNew; break;
case kfunction: yyNext = yyOld->function.next;
 yyOld->function.next = yyNew; break;
case kmethod: yyNext = yyOld->method.next;
 yyOld->method.next = yyNew; break;
case kconstructor: yyNext = yyOld->constructor.next;
 yyOld->constructor.next = yyNew; break;
case kvar_decl: yyNext = yyOld->var_decl.next;
 yyOld->var_decl.next = yyNew; break;
case kstatic_initializer: yyNext = yyOld->static_initializer.next;
 yyOld->static_initializer.next = yyNew; break;
case kinitializer: yyNext = yyOld->initializer.next;
 yyOld->initializer.next = yyNew; break;
case ktype_decl: yyNext = yyOld->type_decl.next;
 yyOld->type_decl.next = yyNew; break;
case kclass: yyNext = yyOld->class.next;
 yyOld->class.next = yyNew; break;
case kinterface: yyNext = yyOld->my_interface.next;
 yyOld->my_interface.next = yyNew; break;
case kdecl: yyNext = yyOld->decl.next;
 yyOld->decl.next = yyNew; break;
case kvariable: yyNext = yyOld->variable.next;
 yyOld->variable.next = yyNew; break;
case kparameter: yyNext = yyOld->parameter.next;
 yyOld->parameter.next = yyNew; break;
case kstatement: yyNext = yyOld->statement.next;
 yyOld->statement.next = yyNew; break;
case kswitch_: yyNext = yyOld->switch_.next;
 yyOld->switch_.next = yyNew; break;
case kcatch: yyNext = yyOld->catch.next;
 yyOld->catch.next = yyNew; break;
case ktype_name: yyNext = yyOld->type_name.next;
 yyOld->type_name.next = yyNew; break;
case kexpression: yyNext = yyOld->expression.next;
 yyOld->expression.next = yyNew; break;
case kobject: yyNext = yyOld->object.next;
 yyOld->object.next = yyNew; break;
  default: goto yyExit;
  }
  yyNew = yyOld;
  yyOld = yyNext;
 }
yyExit:
 switch (yyTail->Kind) {
case kimport: yyTail->import.next = yyOld; break;
case kimport_asterisk: yyTail->import_asterisk.next = yyOld; break;
case kfield: yyTail->field.next = yyOld; break;
case kfunction: yyTail->function.next = yyOld; break;
case kmethod: yyTail->method.next = yyOld; break;
case kconstructor: yyTail->constructor.next = yyOld; break;
case kvar_decl: yyTail->var_decl.next = yyOld; break;
case kstatic_initializer: yyTail->static_initializer.next = yyOld; break;
case kinitializer: yyTail->initializer.next = yyOld; break;
case ktype_decl: yyTail->type_decl.next = yyOld; break;
case kclass: yyTail->class.next = yyOld; break;
case kinterface: yyTail->my_interface.next = yyOld; break;
case kdecl: yyTail->decl.next = yyOld; break;
case kvariable: yyTail->variable.next = yyOld; break;
case kparameter: yyTail->parameter.next = yyOld; break;
case kstatement: yyTail->statement.next = yyOld; break;
case kswitch_: yyTail->switch_.next = yyOld; break;
case kcatch: yyTail->catch.next = yyOld; break;
case ktype_name: yyTail->type_name.next = yyOld; break;
case kexpression: yyTail->expression.next = yyOld; break;
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
case kimport: yyt = yyt->import.next; break;
case kimport_asterisk: yyt = yyt->import_asterisk.next; break;
case kfield: yyt = yyt->field.next; break;
case kfunction: yyt = yyt->function.next; break;
case kmethod: yyt = yyt->method.next; break;
case kconstructor: yyt = yyt->constructor.next; break;
case kvar_decl: yyt = yyt->var_decl.next; break;
case kstatic_initializer: yyt = yyt->static_initializer.next; break;
case kinitializer: yyt = yyt->initializer.next; break;
case ktype_decl: yyt = yyt->type_decl.next; break;
case kclass: yyt = yyt->class.next; break;
case kinterface: yyt = yyt->my_interface.next; break;
case kdecl: yyt = yyt->decl.next; break;
case kvariable: yyt = yyt->variable.next; break;
case kparameter: yyt = yyt->parameter.next; break;
case kstatement: yyt = yyt->statement.next; break;
case kswitch_: yyt = yyt->switch_.next; break;
case kcatch: yyt = yyt->catch.next; break;
case ktype_name: yyt = yyt->type_name.next; break;
case kexpression: yyt = yyt->expression.next; break;
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
case kcompilation_unit:
yyCheckChild (yyt, yyt->compilation_unit.package,
kqualified_symbol, "package");
yyCheckChild (yyt, yyt->compilation_unit.import_list,
kimport_list, "import_list");
yyCheckChild2 (yyt, yyt->compilation_unit.field_list,
kfield_list, "field_list");
yyt = yyt->compilation_unit.field_list; break;
case kimport:
yyCheckChild (yyt, yyt->import.qualified_symbol,
kqualified_symbol, "qualified_symbol");
yyCheckChild2 (yyt, yyt->import.next,
kimport_list, "next");
yyt = yyt->import.next; break;
case kimport_asterisk:
yyCheckChild (yyt, yyt->import_asterisk.qualified_symbol,
kqualified_symbol, "qualified_symbol");
yyCheckChild2 (yyt, yyt->import_asterisk.next,
kimport_list, "next");
yyt = yyt->import_asterisk.next; break;
case kfield:
yyCheckChild2 (yyt, yyt->field.next,
kfield_list, "next");
yyt = yyt->field.next; break;
case kfunction:
yyCheckChild (yyt, yyt->function.decl_list,
kdecl_list, "decl_list");
yyCheckChild (yyt, yyt->function.throws,
ktype_name_list, "throws");
yyCheckChild (yyt, yyt->function.block,
kstatement_c, "block");
yyCheckChild2 (yyt, yyt->function.next,
kfield_list, "next");
yyt = yyt->function.next; break;
case kmethod:
yyCheckChild (yyt, yyt->method.decl_list,
kdecl_list, "decl_list");
yyCheckChild (yyt, yyt->method.throws,
ktype_name_list, "throws");
yyCheckChild (yyt, yyt->method.block,
kstatement_c, "block");
yyCheckChild (yyt, yyt->method.type,
ktype, "type");
yyCheckChild (yyt, yyt->method.array,
ktype, "array");
yyCheckChild2 (yyt, yyt->method.next,
kfield_list, "next");
yyt = yyt->method.next; break;
case kconstructor:
yyCheckChild (yyt, yyt->constructor.decl_list,
kdecl_list, "decl_list");
yyCheckChild (yyt, yyt->constructor.throws,
ktype_name_list, "throws");
yyCheckChild (yyt, yyt->constructor.block,
kstatement_c, "block");
yyCheckChild2 (yyt, yyt->constructor.next,
kfield_list, "next");
yyt = yyt->constructor.next; break;
case kvar_decl:
yyCheckChild (yyt, yyt->var_decl.type,
ktype, "type");
yyCheckChild (yyt, yyt->var_decl.decl_list,
kdecl_list, "decl_list");
yyCheckChild2 (yyt, yyt->var_decl.next,
kfield_list, "next");
yyt = yyt->var_decl.next; break;
case kstatic_initializer:
yyCheckChild (yyt, yyt->static_initializer.statement_c,
kstatement_c, "statement_c");
yyCheckChild2 (yyt, yyt->static_initializer.next,
kfield_list, "next");
yyt = yyt->static_initializer.next; break;
case kinitializer:
yyCheckChild (yyt, yyt->initializer.statement_c,
kstatement_c, "statement_c");
yyCheckChild2 (yyt, yyt->initializer.next,
kfield_list, "next");
yyt = yyt->initializer.next; break;
case ktype_decl:
yyCheckChild (yyt, yyt->type_decl.block,
kfield_list, "block");
yyCheckChild2 (yyt, yyt->type_decl.next,
kfield_list, "next");
yyt = yyt->type_decl.next; break;
case kclass:
yyCheckChild (yyt, yyt->class.block,
kfield_list, "block");
yyCheckChild (yyt, yyt->class.extends,
ktype, "extends");
yyCheckChild (yyt, yyt->class.interfaces,
ktype_name_list, "interfaces");
yyCheckChild2 (yyt, yyt->class.next,
kfield_list, "next");
yyt = yyt->class.next; break;
case kinterface:
yyCheckChild (yyt, yyt->my_interface.block,
kfield_list, "block");
yyCheckChild (yyt, yyt->my_interface.extends,
ktype_name_list, "extends");
yyCheckChild2 (yyt, yyt->my_interface.next,
kfield_list, "next");
yyt = yyt->my_interface.next; break;
case kdecl:
yyCheckChild (yyt, yyt->decl.type,
ktype, "type");
yyCheckChild (yyt, yyt->decl.array,
ktype, "array");
yyCheckChild2 (yyt, yyt->decl.next,
kdecl_list, "next");
yyt = yyt->decl.next; break;
case kvariable:
yyCheckChild (yyt, yyt->variable.type,
ktype, "type");
yyCheckChild (yyt, yyt->variable.array,
ktype, "array");
yyCheckChild (yyt, yyt->variable.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->variable.next,
kdecl_list, "next");
yyt = yyt->variable.next; break;
case kparameter:
yyCheckChild (yyt, yyt->parameter.type,
ktype, "type");
yyCheckChild (yyt, yyt->parameter.array,
ktype, "array");
yyCheckChild2 (yyt, yyt->parameter.next,
kdecl_list, "next");
yyt = yyt->parameter.next; break;
case kstatement:
yyCheckChild (yyt, yyt->statement.statement,
kstatement_c, "statement");
yyCheckChild2 (yyt, yyt->statement.next,
kstatement_list, "next");
yyt = yyt->statement.next; break;
case kcompound_stmt:
yyCheckChild2 (yyt, yyt->compound_stmt.statement_list,
kstatement_list, "statement_list");
yyt = yyt->compound_stmt.statement_list; break;
case kexpression_stmt:
yyCheckChild2 (yyt, yyt->expression_stmt.expression,
kexpression_c, "expression");
yyt = yyt->expression_stmt.expression; break;
case kvar_decl_stmt:
yyCheckChild (yyt, yyt->var_decl_stmt.type,
ktype, "type");
yyCheckChild2 (yyt, yyt->var_decl_stmt.decl_list,
kdecl_list, "decl_list");
yyt = yyt->var_decl_stmt.decl_list; break;
case ktype_decl_stmt:
yyCheckChild2 (yyt, yyt->type_decl_stmt.type_decl,
ktype_decl, "type_decl");
yyt = yyt->type_decl_stmt.type_decl; break;
case kif_stmt:
yyCheckChild (yyt, yyt->if_stmt.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->if_stmt.then,
kstatement_c, "then");
yyt = yyt->if_stmt.then; break;
case kif_else_stmt:
yyCheckChild (yyt, yyt->if_else_stmt.expression,
kexpression_c, "expression");
yyCheckChild (yyt, yyt->if_else_stmt.then,
kstatement_c, "then");
yyCheckChild2 (yyt, yyt->if_else_stmt.Else,
kstatement_c, "Else");
yyt = yyt->if_else_stmt.Else; break;
case kwhile_stmt:
yyCheckChild (yyt, yyt->while_stmt.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->while_stmt.statement,
kstatement_c, "statement");
yyt = yyt->while_stmt.statement; break;
case kdo_stmt:
yyCheckChild (yyt, yyt->do_stmt.statement,
kstatement_c, "statement");
yyCheckChild2 (yyt, yyt->do_stmt.expression,
kexpression_c, "expression");
yyt = yyt->do_stmt.expression; break;
case kbreak_id_stmt:
yyCheckChild2 (yyt, yyt->break_id_stmt.expression,
kexpression_c, "expression");
yyt = yyt->break_id_stmt.expression; break;
case kcontinue_id_stmt:
yyCheckChild2 (yyt, yyt->continue_id_stmt.expression,
kexpression_c, "expression");
yyt = yyt->continue_id_stmt.expression; break;
case kreturn_expr_stmt:
yyCheckChild2 (yyt, yyt->return_expr_stmt.expression,
kexpression_c, "expression");
yyt = yyt->return_expr_stmt.expression; break;
case kfor_stmt:
yyCheckChild (yyt, yyt->for_stmt.for_init,
kstatement_list, "for_init");
yyCheckChild (yyt, yyt->for_stmt.expression,
kexpression_c, "expression");
yyCheckChild (yyt, yyt->for_stmt.for_incr,
kstatement_list, "for_incr");
yyCheckChild2 (yyt, yyt->for_stmt.statement,
kstatement_c, "statement");
yyt = yyt->for_stmt.statement; break;
case kthrow_stmt:
yyCheckChild2 (yyt, yyt->throw_stmt.expression,
kexpression_c, "expression");
yyt = yyt->throw_stmt.expression; break;
case ksynchronized_stmt:
yyCheckChild (yyt, yyt->synchronized_stmt.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->synchronized_stmt.statement,
kstatement_c, "statement");
yyt = yyt->synchronized_stmt.statement; break;
case klabeled_stmt:
yyCheckChild2 (yyt, yyt->labeled_stmt.statement,
kstatement_c, "statement");
yyt = yyt->labeled_stmt.statement; break;
case ktry_stmt:
yyCheckChild (yyt, yyt->try_stmt.statement,
kstatement_c, "statement");
yyCheckChild (yyt, yyt->try_stmt.catch_list,
kcatch_list, "catch_list");
yyCheckChild2 (yyt, yyt->try_stmt.finally,
kstatement_c, "finally");
yyt = yyt->try_stmt.finally; break;
case kswitch_stmt:
yyCheckChild (yyt, yyt->switch_stmt.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->switch_stmt.switch_list,
kswitch_list, "switch_list");
yyt = yyt->switch_stmt.switch_list; break;
case kswitch_:
yyCheckChild (yyt, yyt->switch_.expression_list,
kexpression_list, "expression_list");
yyCheckChild (yyt, yyt->switch_.statement_list,
kstatement_list, "statement_list");
yyCheckChild2 (yyt, yyt->switch_.next,
kswitch_list, "next");
yyt = yyt->switch_.next; break;
case kcatch:
yyCheckChild (yyt, yyt->catch.decl_list,
kdecl_list, "decl_list");
yyCheckChild (yyt, yyt->catch.statement,
kstatement_c, "statement");
yyCheckChild2 (yyt, yyt->catch.next,
kcatch_list, "next");
yyt = yyt->catch.next; break;
case ktype_name:
yyCheckChild (yyt, yyt->type_name.named_type,
knamed_type, "named_type");
yyCheckChild2 (yyt, yyt->type_name.next,
ktype_name_list, "next");
yyt = yyt->type_name.next; break;
case knamed_type:
yyCheckChild2 (yyt, yyt->named_type.qualified_symbol,
kqualified_symbol, "qualified_symbol");
yyt = yyt->named_type.qualified_symbol; break;
case karray_type:
yyCheckChild2 (yyt, yyt->array_type.type,
ktype, "type");
yyt = yyt->array_type.type; break;
case kexpression:
yyCheckChild (yyt, yyt->expression.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->expression.next,
kexpression_list, "next");
yyt = yyt->expression.next; break;
case kqualification:
yyCheckChild2 (yyt, yyt->qualification.qualified_symbol,
kqualified_symbol, "qualified_symbol");
yyt = yyt->qualification.qualified_symbol; break;
case kunary:
yyCheckChild2 (yyt, yyt->unary.expression,
kexpression_c, "expression");
yyt = yyt->unary.expression; break;
case kbinary:
yyCheckChild (yyt, yyt->binary.lop,
kexpression_c, "lop");
yyCheckChild2 (yyt, yyt->binary.rop,
kexpression_c, "rop");
yyt = yyt->binary.rop; break;
case kassign:
yyCheckChild (yyt, yyt->assign.lval,
kexpression_c, "lval");
yyCheckChild2 (yyt, yyt->assign.rval,
kexpression_c, "rval");
yyt = yyt->assign.rval; break;
case kaggregate:
yyCheckChild2 (yyt, yyt->aggregate.expression_list,
kexpression_list, "expression_list");
yyt = yyt->aggregate.expression_list; break;
case kcall:
yyCheckChild (yyt, yyt->call.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->call.expression_list,
kexpression_list, "expression_list");
yyt = yyt->call.expression_list; break;
case kselect:
yyCheckChild2 (yyt, yyt->select.expression,
kexpression_c, "expression");
yyt = yyt->select.expression; break;
case kget_class_of_expr:
yyCheckChild2 (yyt, yyt->get_class_of_expr.expression,
kexpression_c, "expression");
yyt = yyt->get_class_of_expr.expression; break;
case kget_class:
yyCheckChild2 (yyt, yyt->get_class.type,
ktype, "type");
yyt = yyt->get_class.type; break;
case ksubscript:
yyCheckChild (yyt, yyt->subscript.base,
kexpression_c, "base");
yyCheckChild2 (yyt, yyt->subscript.index,
kexpression_c, "index");
yyt = yyt->subscript.index; break;
case ktype_compare:
yyCheckChild (yyt, yyt->type_compare.expression,
kexpression_c, "expression");
yyCheckChild2 (yyt, yyt->type_compare.type,
ktype, "type");
yyt = yyt->type_compare.type; break;
case ktype_cast:
yyCheckChild (yyt, yyt->type_cast.type,
ktype, "type");
yyCheckChild (yyt, yyt->type_cast.dims,
ktype, "dims");
yyCheckChild2 (yyt, yyt->type_cast.expression,
kexpression_c, "expression");
yyt = yyt->type_cast.expression; break;
case knew:
yyCheckChild (yyt, yyt->new.type,
ktype, "type");
yyCheckChild (yyt, yyt->new.dims,
ktype, "dims");
yyCheckChild (yyt, yyt->new.expression_list,
kexpression_list, "expression_list");
yyCheckChild2 (yyt, yyt->new.expression,
kexpression_c, "expression");
yyt = yyt->new.expression; break;
case kanonymous:
yyCheckChild (yyt, yyt->anonymous.type,
ktype, "type");
yyCheckChild (yyt, yyt->anonymous.expression_list,
kexpression_list, "expression_list");
yyCheckChild2 (yyt, yyt->anonymous.block,
kfield_list, "block");
yyt = yyt->anonymous.block; break;
case kconditional:
yyCheckChild (yyt, yyt->conditional.condition,
kexpression_c, "condition");
yyCheckChild (yyt, yyt->conditional.true_expr,
kexpression_c, "true_expr");
yyCheckChild2 (yyt, yyt->conditional.false_expr,
kexpression_c, "false_expr");
yyt = yyt->conditional.false_expr; break;
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
case kfunction:
yySearch3 (yyt, yyt->function.pos);
break;
case kmethod:
yySearch3 (yyt, yyt->method.pos);
break;
case kconstructor:
yySearch3 (yyt, yyt->constructor.pos);
break;
case ktype_decl:
yySearch3 (yyt, yyt->type_decl.pos);
break;
case kclass:
yySearch3 (yyt, yyt->class.pos);
break;
case kinterface:
yySearch3 (yyt, yyt->my_interface.pos);
break;
case kdecl:
yySearch3 (yyt, yyt->decl.pos);
break;
case kvariable:
yySearch3 (yyt, yyt->variable.pos);
break;
case kparameter:
yySearch3 (yyt, yyt->parameter.pos);
break;
case klabeled_stmt:
yySearch3 (yyt, yyt->labeled_stmt.pos);
break;
case kexpression_c:
yySearch3 (yyt, yyt->expression_c.pos);
break;
case kqualified_symbol:
yySearch3 (yyt, yyt->qualified_symbol.pos);
break;
case kqualification:
yySearch3 (yyt, yyt->qualification.pos);
break;
case kident:
yySearch3 (yyt, yyt->ident.pos);
break;
case knoexpression:
yySearch3 (yyt, yyt->noexpression.pos);
break;
case kunary:
yySearch3 (yyt, yyt->unary.pos);
break;
case kbinary:
yySearch3 (yyt, yyt->binary.pos);
break;
case kassign:
yySearch3 (yyt, yyt->assign.pos);
break;
case kaggregate:
yySearch3 (yyt, yyt->aggregate.pos);
break;
case kcall:
yySearch3 (yyt, yyt->call.pos);
break;
case kselect:
yySearch3 (yyt, yyt->select.pos);
break;
case kget_class_of_expr:
yySearch3 (yyt, yyt->get_class_of_expr.pos);
break;
case kget_class:
yySearch3 (yyt, yyt->get_class.pos);
break;
case ksubscript:
yySearch3 (yyt, yyt->subscript.pos);
break;
case ktype_compare:
yySearch3 (yyt, yyt->type_compare.pos);
break;
case ktype_cast:
yySearch3 (yyt, yyt->type_cast.pos);
break;
case knew:
yySearch3 (yyt, yyt->new.pos);
break;
case kanonymous:
yySearch3 (yyt, yyt->anonymous.pos);
break;
case kconditional:
yySearch3 (yyt, yyt->conditional.pos);
break;
case ksuper:
yySearch3 (yyt, yyt->super.pos);
break;
case kthis:
yySearch3 (yyt, yyt->this.pos);
break;
case knull:
yySearch3 (yyt, yyt->null.pos);
break;
case kint_literal:
yySearch3 (yyt, yyt->int_literal.pos);
break;
case klong_literal:
yySearch3 (yyt, yyt->long_literal.pos);
break;
case kbool_literal:
yySearch3 (yyt, yyt->bool_literal.pos);
break;
case kfloat_literal:
yySearch3 (yyt, yyt->float_literal.pos);
break;
case kchar_literal:
yySearch3 (yyt, yyt->char_literal.pos);
break;
case kstring_literal:
yySearch3 (yyt, yyt->string_literal.pos);
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
 tTree yyTheTree = (tTree) atoi (yyargv [1]);
 yyLine = atoi (yyargv [2]);
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
case kcompilation_unit:
yy = yySetY (yyt, yyt->compilation_unit.package, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->compilation_unit.import_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->compilation_unit.field_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kimport:
yy = yySetY (yyt, yyt->import.qualified_symbol, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->import.next, yymax - 1, yyk - 1);
break;
case kimport_asterisk:
yy = yySetY (yyt, yyt->import_asterisk.qualified_symbol, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->import_asterisk.next, yymax - 1, yyk - 1);
break;
case kfield:
yymax = yySetY (yyt, yyt->field.next, yymax, yyk - 1);
break;
case kfunction:
yy = yySetY (yyt, yyt->function.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->function.throws, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->function.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->function.next, yymax - 1, yyk - 1);
break;
case kmethod:
yy = yySetY (yyt, yyt->method.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.throws, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.block, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->method.array, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->method.next, yymax - 1, yyk - 1);
break;
case kconstructor:
yy = yySetY (yyt, yyt->constructor.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.throws, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->constructor.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->constructor.next, yymax - 1, yyk - 1);
break;
case kvar_decl:
yy = yySetY (yyt, yyt->var_decl.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->var_decl.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->var_decl.next, yymax - 1, yyk - 1);
break;
case kstatic_initializer:
yy = yySetY (yyt, yyt->static_initializer.statement_c, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->static_initializer.next, yymax - 1, yyk - 1);
break;
case kinitializer:
yy = yySetY (yyt, yyt->initializer.statement_c, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->initializer.next, yymax - 1, yyk - 1);
break;
case ktype_decl:
yy = yySetY (yyt, yyt->type_decl.block, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->type_decl.next, yymax - 1, yyk - 1);
break;
case kclass:
yy = yySetY (yyt, yyt->class.block, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->class.extends, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->class.interfaces, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->class.next, yymax - 1, yyk - 1);
break;
case kinterface:
yy = yySetY (yyt, yyt->my_interface.block, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->my_interface.extends, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->my_interface.next, yymax - 1, yyk - 1);
break;
case kdecl:
yy = yySetY (yyt, yyt->decl.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->decl.array, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->decl.next, yymax - 1, yyk - 1);
break;
case kvariable:
yy = yySetY (yyt, yyt->variable.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->variable.array, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->variable.expression, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->variable.next, yymax - 1, yyk - 1);
break;
case kparameter:
yy = yySetY (yyt, yyt->parameter.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->parameter.array, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->parameter.next, yymax - 1, yyk - 1);
break;
case kstatement:
yy = yySetY (yyt, yyt->statement.statement, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->statement.next, yymax - 1, yyk - 1);
break;
case kcompound_stmt:
yy = yySetY (yyt, yyt->compound_stmt.statement_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kexpression_stmt:
yy = yySetY (yyt, yyt->expression_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kvar_decl_stmt:
yy = yySetY (yyt, yyt->var_decl_stmt.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->var_decl_stmt.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ktype_decl_stmt:
yy = yySetY (yyt, yyt->type_decl_stmt.type_decl, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kif_stmt:
yy = yySetY (yyt, yyt->if_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->if_stmt.then, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kif_else_stmt:
yy = yySetY (yyt, yyt->if_else_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->if_else_stmt.then, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->if_else_stmt.Else, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kwhile_stmt:
yy = yySetY (yyt, yyt->while_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->while_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kdo_stmt:
yy = yySetY (yyt, yyt->do_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->do_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kbreak_id_stmt:
yy = yySetY (yyt, yyt->break_id_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kcontinue_id_stmt:
yy = yySetY (yyt, yyt->continue_id_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kreturn_expr_stmt:
yy = yySetY (yyt, yyt->return_expr_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kfor_stmt:
yy = yySetY (yyt, yyt->for_stmt.for_init, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->for_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->for_stmt.for_incr, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->for_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kthrow_stmt:
yy = yySetY (yyt, yyt->throw_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ksynchronized_stmt:
yy = yySetY (yyt, yyt->synchronized_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->synchronized_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
break;
case klabeled_stmt:
yy = yySetY (yyt, yyt->labeled_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ktry_stmt:
yy = yySetY (yyt, yyt->try_stmt.statement, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->try_stmt.catch_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->try_stmt.finally, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kswitch_stmt:
yy = yySetY (yyt, yyt->switch_stmt.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->switch_stmt.switch_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kswitch_:
yy = yySetY (yyt, yyt->switch_.expression_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->switch_.statement_list, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->switch_.next, yymax - 1, yyk - 1);
break;
case kcatch:
yy = yySetY (yyt, yyt->catch.decl_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->catch.statement, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->catch.next, yymax - 1, yyk - 1);
break;
case ktype_name:
yy = yySetY (yyt, yyt->type_name.named_type, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->type_name.next, yymax - 1, yyk - 1);
break;
case knamed_type:
yy = yySetY (yyt, yyt->named_type.qualified_symbol, yyy, yyk);
yymax = Max (yymax, yy);
break;
case karray_type:
yy = yySetY (yyt, yyt->array_type.type, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kexpression:
yy = yySetY (yyt, yyt->expression.expression, yyy, yyk);
yymax = Max (yymax, yy);
yymax = yySetY (yyt, yyt->expression.next, yymax - 1, yyk - 1);
break;
case kqualification:
yy = yySetY (yyt, yyt->qualification.qualified_symbol, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kunary:
yy = yySetY (yyt, yyt->unary.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kbinary:
yy = yySetY (yyt, yyt->binary.lop, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->binary.rop, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kassign:
yy = yySetY (yyt, yyt->assign.lval, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->assign.rval, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kaggregate:
yy = yySetY (yyt, yyt->aggregate.expression_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kcall:
yy = yySetY (yyt, yyt->call.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->call.expression_list, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kselect:
yy = yySetY (yyt, yyt->select.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kget_class_of_expr:
yy = yySetY (yyt, yyt->get_class_of_expr.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kget_class:
yy = yySetY (yyt, yyt->get_class.type, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ksubscript:
yy = yySetY (yyt, yyt->subscript.base, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->subscript.index, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ktype_compare:
yy = yySetY (yyt, yyt->type_compare.expression, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->type_compare.type, yyy, yyk);
yymax = Max (yymax, yy);
break;
case ktype_cast:
yy = yySetY (yyt, yyt->type_cast.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->type_cast.dims, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->type_cast.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case knew:
yy = yySetY (yyt, yyt->new.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->new.dims, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->new.expression_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->new.expression, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kanonymous:
yy = yySetY (yyt, yyt->anonymous.type, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->anonymous.expression_list, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->anonymous.block, yyy, yyk);
yymax = Max (yymax, yy);
break;
case kconditional:
yy = yySetY (yyt, yyt->conditional.condition, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->conditional.true_expr, yyy, yyk);
yymax = Max (yymax, yy);
yy = yySetY (yyt, yyt->conditional.false_expr, yyy, yyk);
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
yy = yySetY (yyt, yyt->env2.env2, yyy, yyk);
yymax = Max (yymax, yy);
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
case kcompilation_unit:
yyx = yySetX (yyt, yyt->compilation_unit.package, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->compilation_unit.import_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->compilation_unit.field_list, yyx, yyk, & yyFirstLast);
break;
case kimport:
yyw = yySetX (yyt, yyt->import.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->import.qualified_symbol, yyx, yyk, & yyFirstLast);
goto yyList;
case kimport_asterisk:
yyw = yySetX (yyt, yyt->import_asterisk.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->import_asterisk.qualified_symbol, yyx, yyk, & yyFirstLast);
goto yyList;
case kfield:
yyw = yySetX (yyt, yyt->field.next, yyx ++, yyk - 1, & yyFirstLast);
goto yyList;
case kfunction:
yyw = yySetX (yyt, yyt->function.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->function.decl_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->function.throws, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->function.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kmethod:
yyw = yySetX (yyt, yyt->method.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.decl_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.throws, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.block, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->method.array, yyx, yyk, & yyFirstLast);
goto yyList;
case kconstructor:
yyw = yySetX (yyt, yyt->constructor.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.decl_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.throws, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->constructor.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kvar_decl:
yyw = yySetX (yyt, yyt->var_decl.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->var_decl.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->var_decl.decl_list, yyx, yyk, & yyFirstLast);
goto yyList;
case kstatic_initializer:
yyw = yySetX (yyt, yyt->static_initializer.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->static_initializer.statement_c, yyx, yyk, & yyFirstLast);
goto yyList;
case kinitializer:
yyw = yySetX (yyt, yyt->initializer.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->initializer.statement_c, yyx, yyk, & yyFirstLast);
goto yyList;
case ktype_decl:
yyw = yySetX (yyt, yyt->type_decl.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->type_decl.block, yyx, yyk, & yyFirstLast);
goto yyList;
case kclass:
yyw = yySetX (yyt, yyt->class.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->class.block, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->class.extends, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->class.interfaces, yyx, yyk, & yyFirstLast);
goto yyList;
case kinterface:
yyw = yySetX (yyt, yyt->my_interface.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->my_interface.block, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->my_interface.extends, yyx, yyk, & yyFirstLast);
goto yyList;
case kdecl:
yyw = yySetX (yyt, yyt->decl.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->decl.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->decl.array, yyx, yyk, & yyFirstLast);
goto yyList;
case kvariable:
yyw = yySetX (yyt, yyt->variable.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->variable.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->variable.array, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->variable.expression, yyx, yyk, & yyFirstLast);
goto yyList;
case kparameter:
yyw = yySetX (yyt, yyt->parameter.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->parameter.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->parameter.array, yyx, yyk, & yyFirstLast);
goto yyList;
case kstatement:
yyw = yySetX (yyt, yyt->statement.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->statement.statement, yyx, yyk, & yyFirstLast);
goto yyList;
case kcompound_stmt:
yyx = yySetX (yyt, yyt->compound_stmt.statement_list, yyx, yyk, & yyFirstLast);
break;
case kexpression_stmt:
yyx = yySetX (yyt, yyt->expression_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case kvar_decl_stmt:
yyx = yySetX (yyt, yyt->var_decl_stmt.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->var_decl_stmt.decl_list, yyx, yyk, & yyFirstLast);
break;
case ktype_decl_stmt:
yyx = yySetX (yyt, yyt->type_decl_stmt.type_decl, yyx, yyk, & yyFirstLast);
break;
case kif_stmt:
yyx = yySetX (yyt, yyt->if_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->if_stmt.then, yyx, yyk, & yyFirstLast);
break;
case kif_else_stmt:
yyx = yySetX (yyt, yyt->if_else_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->if_else_stmt.then, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->if_else_stmt.Else, yyx, yyk, & yyFirstLast);
break;
case kwhile_stmt:
yyx = yySetX (yyt, yyt->while_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->while_stmt.statement, yyx, yyk, & yyFirstLast);
break;
case kdo_stmt:
yyx = yySetX (yyt, yyt->do_stmt.statement, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->do_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case kbreak_id_stmt:
yyx = yySetX (yyt, yyt->break_id_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case kcontinue_id_stmt:
yyx = yySetX (yyt, yyt->continue_id_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case kreturn_expr_stmt:
yyx = yySetX (yyt, yyt->return_expr_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case kfor_stmt:
yyx = yySetX (yyt, yyt->for_stmt.for_init, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->for_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->for_stmt.for_incr, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->for_stmt.statement, yyx, yyk, & yyFirstLast);
break;
case kthrow_stmt:
yyx = yySetX (yyt, yyt->throw_stmt.expression, yyx, yyk, & yyFirstLast);
break;
case ksynchronized_stmt:
yyx = yySetX (yyt, yyt->synchronized_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->synchronized_stmt.statement, yyx, yyk, & yyFirstLast);
break;
case klabeled_stmt:
yyx = yySetX (yyt, yyt->labeled_stmt.statement, yyx, yyk, & yyFirstLast);
break;
case ktry_stmt:
yyx = yySetX (yyt, yyt->try_stmt.statement, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->try_stmt.catch_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->try_stmt.finally, yyx, yyk, & yyFirstLast);
break;
case kswitch_stmt:
yyx = yySetX (yyt, yyt->switch_stmt.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->switch_stmt.switch_list, yyx, yyk, & yyFirstLast);
break;
case kswitch_:
yyw = yySetX (yyt, yyt->switch_.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->switch_.expression_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->switch_.statement_list, yyx, yyk, & yyFirstLast);
goto yyList;
case kcatch:
yyw = yySetX (yyt, yyt->catch.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->catch.decl_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->catch.statement, yyx, yyk, & yyFirstLast);
goto yyList;
case ktype_name:
yyw = yySetX (yyt, yyt->type_name.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->type_name.named_type, yyx, yyk, & yyFirstLast);
goto yyList;
case knamed_type:
yyx = yySetX (yyt, yyt->named_type.qualified_symbol, yyx, yyk, & yyFirstLast);
break;
case karray_type:
yyx = yySetX (yyt, yyt->array_type.type, yyx, yyk, & yyFirstLast);
break;
case kexpression:
yyw = yySetX (yyt, yyt->expression.next, yyx ++, yyk - 1, & yyFirstLast);
yyx = yySetX (yyt, yyt->expression.expression, yyx, yyk, & yyFirstLast);
goto yyList;
case kqualification:
yyx = yySetX (yyt, yyt->qualification.qualified_symbol, yyx, yyk, & yyFirstLast);
break;
case kunary:
yyx = yySetX (yyt, yyt->unary.expression, yyx, yyk, & yyFirstLast);
break;
case kbinary:
yyx = yySetX (yyt, yyt->binary.lop, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->binary.rop, yyx, yyk, & yyFirstLast);
break;
case kassign:
yyx = yySetX (yyt, yyt->assign.lval, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->assign.rval, yyx, yyk, & yyFirstLast);
break;
case kaggregate:
yyx = yySetX (yyt, yyt->aggregate.expression_list, yyx, yyk, & yyFirstLast);
break;
case kcall:
yyx = yySetX (yyt, yyt->call.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->call.expression_list, yyx, yyk, & yyFirstLast);
break;
case kselect:
yyx = yySetX (yyt, yyt->select.expression, yyx, yyk, & yyFirstLast);
break;
case kget_class_of_expr:
yyx = yySetX (yyt, yyt->get_class_of_expr.expression, yyx, yyk, & yyFirstLast);
break;
case kget_class:
yyx = yySetX (yyt, yyt->get_class.type, yyx, yyk, & yyFirstLast);
break;
case ksubscript:
yyx = yySetX (yyt, yyt->subscript.base, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->subscript.index, yyx, yyk, & yyFirstLast);
break;
case ktype_compare:
yyx = yySetX (yyt, yyt->type_compare.expression, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->type_compare.type, yyx, yyk, & yyFirstLast);
break;
case ktype_cast:
yyx = yySetX (yyt, yyt->type_cast.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->type_cast.dims, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->type_cast.expression, yyx, yyk, & yyFirstLast);
break;
case knew:
yyx = yySetX (yyt, yyt->new.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->new.dims, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->new.expression_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->new.expression, yyx, yyk, & yyFirstLast);
break;
case kanonymous:
yyx = yySetX (yyt, yyt->anonymous.type, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->anonymous.expression_list, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->anonymous.block, yyx, yyk, & yyFirstLast);
break;
case kconditional:
yyx = yySetX (yyt, yyt->conditional.condition, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->conditional.true_expr, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->conditional.false_expr, yyx, yyk, & yyFirstLast);
break;
case kenv:
yyx = yySetX (yyt, yyt->env.objects, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->env.env, yyx, yyk, & yyFirstLast);
break;
case kenv2:
yyx = yySetX (yyt, yyt->env2.env1, yyx, yyk, & yyFirstLast);
yyx = yySetX (yyt, yyt->env2.env2, yyx, yyk, & yyFirstLast);
break;
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
case kcompilation_unit:
yyDrawEdge (yyt, yyt->compilation_unit.package);
yyDrawEdge (yyt, yyt->compilation_unit.import_list);
yyDrawEdge (yyt, yyt->compilation_unit.field_list);
break;
case kimport:
yyDrawEdge (yyt, yyt->import.next);
yyDrawEdge (yyt, yyt->import.qualified_symbol);
break;
case kimport_asterisk:
yyDrawEdge (yyt, yyt->import_asterisk.next);
yyDrawEdge (yyt, yyt->import_asterisk.qualified_symbol);
break;
case kfield:
yyDrawEdge (yyt, yyt->field.next);
break;
case kfunction:
yyDrawEdge (yyt, yyt->function.next);
yyDrawEdge (yyt, yyt->function.decl_list);
yyDrawEdge (yyt, yyt->function.throws);
yyDrawEdge (yyt, yyt->function.block);
break;
case kmethod:
yyDrawEdge (yyt, yyt->method.next);
yyDrawEdge (yyt, yyt->method.decl_list);
yyDrawEdge (yyt, yyt->method.throws);
yyDrawEdge (yyt, yyt->method.block);
yyDrawEdge (yyt, yyt->method.type);
yyDrawEdge (yyt, yyt->method.array);
break;
case kconstructor:
yyDrawEdge (yyt, yyt->constructor.next);
yyDrawEdge (yyt, yyt->constructor.decl_list);
yyDrawEdge (yyt, yyt->constructor.throws);
yyDrawEdge (yyt, yyt->constructor.block);
break;
case kvar_decl:
yyDrawEdge (yyt, yyt->var_decl.next);
yyDrawEdge (yyt, yyt->var_decl.type);
yyDrawEdge (yyt, yyt->var_decl.decl_list);
break;
case kstatic_initializer:
yyDrawEdge (yyt, yyt->static_initializer.next);
yyDrawEdge (yyt, yyt->static_initializer.statement_c);
break;
case kinitializer:
yyDrawEdge (yyt, yyt->initializer.next);
yyDrawEdge (yyt, yyt->initializer.statement_c);
break;
case ktype_decl:
yyDrawEdge (yyt, yyt->type_decl.next);
yyDrawEdge (yyt, yyt->type_decl.block);
break;
case kclass:
yyDrawEdge (yyt, yyt->class.next);
yyDrawEdge (yyt, yyt->class.block);
yyDrawEdge (yyt, yyt->class.extends);
yyDrawEdge (yyt, yyt->class.interfaces);
break;
case kinterface:
yyDrawEdge (yyt, yyt->my_interface.next);
yyDrawEdge (yyt, yyt->my_interface.block);
yyDrawEdge (yyt, yyt->my_interface.extends);
break;
case kdecl:
yyDrawEdge (yyt, yyt->decl.next);
yyDrawEdge (yyt, yyt->decl.type);
yyDrawEdge (yyt, yyt->decl.array);
break;
case kvariable:
yyDrawEdge (yyt, yyt->variable.next);
yyDrawEdge (yyt, yyt->variable.type);
yyDrawEdge (yyt, yyt->variable.array);
yyDrawEdge (yyt, yyt->variable.expression);
break;
case kparameter:
yyDrawEdge (yyt, yyt->parameter.next);
yyDrawEdge (yyt, yyt->parameter.type);
yyDrawEdge (yyt, yyt->parameter.array);
break;
case kstatement:
yyDrawEdge (yyt, yyt->statement.next);
yyDrawEdge (yyt, yyt->statement.statement);
break;
case kcompound_stmt:
yyDrawEdge (yyt, yyt->compound_stmt.statement_list);
break;
case kexpression_stmt:
yyDrawEdge (yyt, yyt->expression_stmt.expression);
break;
case kvar_decl_stmt:
yyDrawEdge (yyt, yyt->var_decl_stmt.type);
yyDrawEdge (yyt, yyt->var_decl_stmt.decl_list);
break;
case ktype_decl_stmt:
yyDrawEdge (yyt, yyt->type_decl_stmt.type_decl);
break;
case kif_stmt:
yyDrawEdge (yyt, yyt->if_stmt.expression);
yyDrawEdge (yyt, yyt->if_stmt.then);
break;
case kif_else_stmt:
yyDrawEdge (yyt, yyt->if_else_stmt.expression);
yyDrawEdge (yyt, yyt->if_else_stmt.then);
yyDrawEdge (yyt, yyt->if_else_stmt.Else);
break;
case kwhile_stmt:
yyDrawEdge (yyt, yyt->while_stmt.expression);
yyDrawEdge (yyt, yyt->while_stmt.statement);
break;
case kdo_stmt:
yyDrawEdge (yyt, yyt->do_stmt.statement);
yyDrawEdge (yyt, yyt->do_stmt.expression);
break;
case kbreak_id_stmt:
yyDrawEdge (yyt, yyt->break_id_stmt.expression);
break;
case kcontinue_id_stmt:
yyDrawEdge (yyt, yyt->continue_id_stmt.expression);
break;
case kreturn_expr_stmt:
yyDrawEdge (yyt, yyt->return_expr_stmt.expression);
break;
case kfor_stmt:
yyDrawEdge (yyt, yyt->for_stmt.for_init);
yyDrawEdge (yyt, yyt->for_stmt.expression);
yyDrawEdge (yyt, yyt->for_stmt.for_incr);
yyDrawEdge (yyt, yyt->for_stmt.statement);
break;
case kthrow_stmt:
yyDrawEdge (yyt, yyt->throw_stmt.expression);
break;
case ksynchronized_stmt:
yyDrawEdge (yyt, yyt->synchronized_stmt.expression);
yyDrawEdge (yyt, yyt->synchronized_stmt.statement);
break;
case klabeled_stmt:
yyDrawEdge (yyt, yyt->labeled_stmt.statement);
break;
case ktry_stmt:
yyDrawEdge (yyt, yyt->try_stmt.statement);
yyDrawEdge (yyt, yyt->try_stmt.catch_list);
yyDrawEdge (yyt, yyt->try_stmt.finally);
break;
case kswitch_stmt:
yyDrawEdge (yyt, yyt->switch_stmt.expression);
yyDrawEdge (yyt, yyt->switch_stmt.switch_list);
break;
case kswitch_:
yyDrawEdge (yyt, yyt->switch_.next);
yyDrawEdge (yyt, yyt->switch_.expression_list);
yyDrawEdge (yyt, yyt->switch_.statement_list);
break;
case kcatch:
yyDrawEdge (yyt, yyt->catch.next);
yyDrawEdge (yyt, yyt->catch.decl_list);
yyDrawEdge (yyt, yyt->catch.statement);
break;
case ktype_name:
yyDrawEdge (yyt, yyt->type_name.next);
yyDrawEdge (yyt, yyt->type_name.named_type);
break;
case knamed_type:
yyDrawEdge (yyt, yyt->named_type.qualified_symbol);
break;
case karray_type:
yyDrawEdge (yyt, yyt->array_type.type);
break;
case kexpression:
yyDrawEdge (yyt, yyt->expression.next);
yyDrawEdge (yyt, yyt->expression.expression);
break;
case kqualification:
yyDrawEdge (yyt, yyt->qualification.qualified_symbol);
break;
case kunary:
yyDrawEdge (yyt, yyt->unary.expression);
break;
case kbinary:
yyDrawEdge (yyt, yyt->binary.lop);
yyDrawEdge (yyt, yyt->binary.rop);
break;
case kassign:
yyDrawEdge (yyt, yyt->assign.lval);
yyDrawEdge (yyt, yyt->assign.rval);
break;
case kaggregate:
yyDrawEdge (yyt, yyt->aggregate.expression_list);
break;
case kcall:
yyDrawEdge (yyt, yyt->call.expression);
yyDrawEdge (yyt, yyt->call.expression_list);
break;
case kselect:
yyDrawEdge (yyt, yyt->select.expression);
break;
case kget_class_of_expr:
yyDrawEdge (yyt, yyt->get_class_of_expr.expression);
break;
case kget_class:
yyDrawEdge (yyt, yyt->get_class.type);
break;
case ksubscript:
yyDrawEdge (yyt, yyt->subscript.base);
yyDrawEdge (yyt, yyt->subscript.index);
break;
case ktype_compare:
yyDrawEdge (yyt, yyt->type_compare.expression);
yyDrawEdge (yyt, yyt->type_compare.type);
break;
case ktype_cast:
yyDrawEdge (yyt, yyt->type_cast.type);
yyDrawEdge (yyt, yyt->type_cast.dims);
yyDrawEdge (yyt, yyt->type_cast.expression);
break;
case knew:
yyDrawEdge (yyt, yyt->new.type);
yyDrawEdge (yyt, yyt->new.dims);
yyDrawEdge (yyt, yyt->new.expression_list);
yyDrawEdge (yyt, yyt->new.expression);
break;
case kanonymous:
yyDrawEdge (yyt, yyt->anonymous.type);
yyDrawEdge (yyt, yyt->anonymous.expression_list);
yyDrawEdge (yyt, yyt->anonymous.block);
break;
case kconditional:
yyDrawEdge (yyt, yyt->conditional.condition);
yyDrawEdge (yyt, yyt->conditional.true_expr);
yyDrawEdge (yyt, yyt->conditional.false_expr);
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
 WriteTreeNode (yyf, (tTree) atoi (yyargv [1]));
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
 DrawTree ((tTree) atoi (yyargv [1]));
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
DrawAttr (atol (yyargv [1]), yyargv [2]);
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

