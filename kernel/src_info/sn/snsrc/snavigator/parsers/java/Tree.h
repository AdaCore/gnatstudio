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

extern char Tree_module_does_not_match_evaluator_module_30586066;
extern char generate_Tree_module_without_option_0;

#if defined __STDC__ | defined __cplusplus
#define ARGS(parameters)	parameters
#else
#define ARGS(parameters)	()
#define const
#endif

#include <stdio.h>
/* line 3 "java.ast" */

#include "Position.h"
#include "StringM.h"
#include "Idents.h"

#define t_boolean	1
#define t_byte		2
#define t_char		3
#define t_double	4
#define t_float	5
#define t_int		6
#define t_long		7
#define t_short	8
#define t_void		9

#define a_PUBLIC	0x0001
#define a_PROTECTED	0x0002
#define a_PRIVATE	0x0004
#define a_STATIC	0x0008
#define a_ABSTRACT	0x0010
#define a_FINAL	0x0020
#define a_NATIVE	0x0040
#define a_SYNCHRONIZED	0x0080
#define a_TRANSIENT	0x0100
#define a_VOLATILE	0x0200
#define a_THREADSAFE	0x0400
#define a_CONST	0x0800

extern char * types [];

typedef char ttype;

#define and		 1
#define and_and	 2
#define assignment	 3
#define complement	 4
#define divide		 5
#define equal		 6
#define greater	 7
#define greater_equal	 8
#define less		 9
#define less_equal	10
#define lshift		11
#define minus		12
#define modulo		13
#define negate		14
#define not		15
#define not_equal	16
#define or		17
#define or_or		18
#define plus		19
#define post_decr	20
#define post_incr	21
#define pre_decr	22
#define pre_incr	23
#define rshift		24
#define times		25
#define us_rshift	26
#define xor		27

typedef char toperator;


#ifndef rbool
#define rbool char
#endif
#define NoTree (tTree) 0L
#define kcompilation_unit 1
#define kimport_list 2
#define knoimport 3
#define kimport 4
#define kimport_asterisk 5
#define kfield_list 6
#define knofield 7
#define kfield 8
#define kfunction 9
#define kmethod 10
#define kconstructor 11
#define kvar_decl 12
#define kstatic_initializer 13
#define kinitializer 14
#define ktype_decl 15
#define kclass 16
#define kinterface 17
#define kdecl_list 18
#define knodecl 19
#define kdecl 20
#define kvariable 21
#define kparameter 22
#define kstatement_list 23
#define knostatement 24
#define kstatement 25
#define kstatement_c 26
#define kcompound_stmt 27
#define kexpression_stmt 28
#define kvar_decl_stmt 29
#define ktype_decl_stmt 30
#define kif_stmt 31
#define kif_else_stmt 32
#define kwhile_stmt 33
#define kdo_stmt 34
#define kbreak_stmt 35
#define kbreak_id_stmt 36
#define kcontinue_stmt 37
#define kcontinue_id_stmt 38
#define kreturn_stmt 39
#define kreturn_expr_stmt 40
#define kfor_stmt 41
#define kthrow_stmt 42
#define ksynchronized_stmt 43
#define klabeled_stmt 44
#define ktry_stmt 45
#define kswitch_stmt 46
#define knull_stmt 47
#define kswitch_list 48
#define knoswitch 49
#define kswitch_ 50
#define kcatch_list 51
#define knocatch 52
#define kcatch 53
#define ktype_name_list 54
#define knotype_name 55
#define ktype_name 56
#define ktype 57
#define ksimple_type 58
#define knamed_type 59
#define karray_type 60
#define knotype 61
#define kexpression_list 62
#define knoexpression_l 63
#define kexpression 64
#define kexpression_c 65
#define kqualified_symbol 66
#define kqualification 67
#define kident 68
#define knoexpression 69
#define kunary 70
#define kbinary 71
#define kassign 72
#define kaggregate 73
#define kcall 74
#define kselect 75
#define kget_class_of_expr 76
#define kget_class 77
#define ksubscript 78
#define ktype_compare 79
#define ktype_cast 80
#define knew 81
#define kanonymous 82
#define kconditional 83
#define ksuper 84
#define kthis 85
#define knull 86
#define kint_literal 87
#define klong_literal 88
#define kbool_literal 89
#define kfloat_literal 90
#define kchar_literal 91
#define kstring_literal 92
#define kenvs 93
#define kenv 94
#define kenv2 95
#define kobjects 96
#define kobject 97
#define knoobject 98
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

/* line 241 "java.ast" */

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
tTree package; tTree import_list; tTree field_list; } ycompilation_unit;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } yimport_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynoimport;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualified_symbol; } yimport;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree qualified_symbol; } yimport_asterisk;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } yfield_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynofield;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; } yfield;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree decl_list; tTree throws; tTree block; } yfunction;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree decl_list; tTree throws; tTree block; tTree type; tTree array; } ymethod;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree decl_list; tTree throws; tTree block; } yconstructor;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tTree type; tTree decl_list; } yvar_decl;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree statement_c; } ystatic_initializer;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree statement_c; } yinitializer;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree block; } ytype_decl;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree block; tTree extends; tTree interfaces; } yclass;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; long modifiers; tIdent ident; tPosition pos; tTree block; tTree extends; } yinterface;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; } ydecl_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; } ynodecl;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; tTree next; long modifiers; tTree type; tIdent ident; tPosition pos; tTree array; } ydecl;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; tTree next; long modifiers; tTree type; tIdent ident; tPosition pos; tTree array; tTree expression; } yvariable;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; tTree next; long modifiers; tTree type; tIdent ident; tPosition pos; tTree array; } yparameter;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ystatement_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynostatement;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree statement; } ystatement;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ystatement_c;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree statement_list; } ycompound_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; } yexpression_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree type; tTree decl_list; } yvar_decl_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree type_decl; } ytype_decl_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; tTree then; } yif_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; tTree then; tTree Else; } yif_else_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; tTree statement; } ywhile_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree statement; tTree expression; } ydo_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ybreak_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; } ybreak_id_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ycontinue_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; } ycontinue_id_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } yreturn_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; } yreturn_expr_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree for_init; tTree expression; tTree for_incr; tTree statement; } yfor_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; } ythrow_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; tTree statement; } ysynchronized_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tIdent ident; tPosition pos; tTree statement; } ylabeled_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree statement; tTree catch_list; tTree finally; } ytry_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree expression; tTree switch_list; } yswitch_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynull_stmt;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } yswitch_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynoswitch;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree expression_list; tTree statement_list; } yswitch_;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ycatch_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynocatch;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree decl_list; tTree statement; } ycatch;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ytype_name_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynotype_name;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree next; tTree named_type; } ytype_name;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ytype;
typedef struct { Tree_tNodeHead yyHead;
tTree env; ttype type; } ysimple_type;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree qualified_symbol; } ynamed_type;
typedef struct { Tree_tNodeHead yyHead;
tTree env; tTree type; } yarray_type;
typedef struct { Tree_tNodeHead yyHead;
tTree env; } ynotype;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; } yexpression_list;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; } ynoexpression_l;
typedef struct { Tree_tNodeHead yyHead;
tTree env; short no_of_args; tTree next; tTree expression; } yexpression;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; } yexpression_c;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree object; } yqualified_symbol;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree object; tTree qualified_symbol; tIdent ident; } yqualification;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree object; tIdent ident; } yident;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree object; } ynoexpression;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression; toperator operator; } yunary;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree lop; tTree rop; toperator operator; } ybinary;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree lval; tTree rval; toperator operator; } yassign;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression_list; } yaggregate;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression; tTree expression_list; } ycall;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression; tIdent ident; } yselect;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression; } yget_class_of_expr;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree type; } yget_class;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree base; tTree index; } ysubscript;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree expression; tTree type; } ytype_compare;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree type; tTree dims; tTree expression; } ytype_cast;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree type; tTree dims; tTree expression_list; tTree expression; } ynew;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree type; tTree expression_list; tTree block; } yanonymous;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tTree condition; tTree true_expr; tTree false_expr; } yconditional;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; } ysuper;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; } ythis;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; } ynull;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; long value; } yint_literal;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; long value; } ylong_literal;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; rbool value; } ybool_literal;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tStringRef value; } yfloat_literal;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tStringRef value; } ychar_literal;
typedef struct { Tree_tNodeHead yyHead;
tPosition pos; tTree env; tStringRef value; } ystring_literal;
typedef struct { Tree_tNodeHead yyHead;
} yenvs;
typedef struct { Tree_tNodeHead yyHead;
tTree objects; tTree env; tTree object; tHashTable HashTable; int HashSize; } yenv;
typedef struct { Tree_tNodeHead yyHead;
tTree env1; tTree env2; } yenv2;
typedef struct { Tree_tNodeHead yyHead;
} yobjects;
typedef struct { Tree_tNodeHead yyHead;
tTree object; tIdent ident; tTree next; tTree collision; } yobject;
typedef struct { Tree_tNodeHead yyHead;
} ynoobject;

union Tree_Node {
 Tree_tKind Kind;
 Tree_tNodeHead yyHead;
 ycompilation_unit compilation_unit;
 yimport_list import_list;
 ynoimport noimport;
 yimport import;
 yimport_asterisk import_asterisk;
 yfield_list field_list;
 ynofield nofield;
 yfield field;
 yfunction function;
 ymethod method;
 yconstructor constructor;
 yvar_decl var_decl;
 ystatic_initializer static_initializer;
 yinitializer initializer;
 ytype_decl type_decl;
 yclass class;
 yinterface my_interface;
 ydecl_list decl_list;
 ynodecl nodecl;
 ydecl decl;
 yvariable variable;
 yparameter parameter;
 ystatement_list statement_list;
 ynostatement nostatement;
 ystatement statement;
 ystatement_c statement_c;
 ycompound_stmt compound_stmt;
 yexpression_stmt expression_stmt;
 yvar_decl_stmt var_decl_stmt;
 ytype_decl_stmt type_decl_stmt;
 yif_stmt if_stmt;
 yif_else_stmt if_else_stmt;
 ywhile_stmt while_stmt;
 ydo_stmt do_stmt;
 ybreak_stmt break_stmt;
 ybreak_id_stmt break_id_stmt;
 ycontinue_stmt continue_stmt;
 ycontinue_id_stmt continue_id_stmt;
 yreturn_stmt return_stmt;
 yreturn_expr_stmt return_expr_stmt;
 yfor_stmt for_stmt;
 ythrow_stmt throw_stmt;
 ysynchronized_stmt synchronized_stmt;
 ylabeled_stmt labeled_stmt;
 ytry_stmt try_stmt;
 yswitch_stmt switch_stmt;
 ynull_stmt null_stmt;
 yswitch_list switch_list;
 ynoswitch noswitch;
 yswitch_ switch_;
 ycatch_list catch_list;
 ynocatch nocatch;
 ycatch catch;
 ytype_name_list type_name_list;
 ynotype_name notype_name;
 ytype_name type_name;
 ytype type;
 ysimple_type simple_type;
 ynamed_type named_type;
 yarray_type array_type;
 ynotype notype;
 yexpression_list expression_list;
 ynoexpression_l noexpression_l;
 yexpression expression;
 yexpression_c expression_c;
 yqualified_symbol qualified_symbol;
 yqualification qualification;
 yident ident;
 ynoexpression noexpression;
 yunary unary;
 ybinary binary;
 yassign assign;
 yaggregate aggregate;
 ycall call;
 yselect select;
 yget_class_of_expr get_class_of_expr;
 yget_class get_class;
 ysubscript subscript;
 ytype_compare type_compare;
 ytype_cast type_cast;
 ynew new;
 yanonymous anonymous;
 yconditional conditional;
 ysuper super;
 ythis this;
 ynull null;
 yint_literal int_literal;
 ylong_literal long_literal;
 ybool_literal bool_literal;
 yfloat_literal float_literal;
 ychar_literal char_literal;
 ystring_literal string_literal;
 yenvs envs;
 yenv env;
 yenv2 env2;
 yobjects objects;
 yobject object;
 ynoobject noobject;
};

extern const unsigned short Tree_NodeSize [99];
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

extern tTree mcompilation_unit
 ARGS ((tTree ppackage, tTree pimport_list, tTree pfield_list));
extern tTree mimport_list
 ARGS ((void));
extern tTree mnoimport
 ARGS ((void));
extern tTree mimport
 ARGS ((tTree pnext, tTree pqualified_symbol));
extern tTree mimport_asterisk
 ARGS ((tTree pnext, tTree pqualified_symbol));
extern tTree mfield_list
 ARGS ((void));
extern tTree mnofield
 ARGS ((void));
extern tTree mfield
 ARGS ((tTree pnext));
extern tTree mfunction
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock));
extern tTree mmethod
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock, tTree ptype, tTree parray));
extern tTree mconstructor
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pdecl_list, tTree pthrows, tTree pblock));
extern tTree mvar_decl
 ARGS ((tTree pnext, long pmodifiers, tTree ptype, tTree pdecl_list));
extern tTree mstatic_initializer
 ARGS ((tTree pnext, tTree pstatement_c));
extern tTree minitializer
 ARGS ((tTree pnext, tTree pstatement_c));
extern tTree mtype_decl
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock));
extern tTree mclass
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock, tTree pextends, tTree pinterfaces));
extern tTree minterface
 ARGS ((tTree pnext, long pmodifiers, tIdent pident, tPosition ppos, tTree pblock, tTree pextends));
extern tTree mdecl_list
 ARGS ((void));
extern tTree mnodecl
 ARGS ((void));
extern tTree mdecl
 ARGS ((tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray));
extern tTree mvariable
 ARGS ((tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray, tTree pexpression));
extern tTree mparameter
 ARGS ((tTree pnext, long pmodifiers, tTree ptype, tIdent pident, tPosition ppos, tTree parray));
extern tTree mstatement_list
 ARGS ((void));
extern tTree mnostatement
 ARGS ((void));
extern tTree mstatement
 ARGS ((tTree pnext, tTree pstatement));
extern tTree mstatement_c
 ARGS ((void));
extern tTree mcompound_stmt
 ARGS ((tTree pstatement_list));
extern tTree mexpression_stmt
 ARGS ((tTree pexpression));
extern tTree mvar_decl_stmt
 ARGS ((tTree ptype, tTree pdecl_list));
extern tTree mtype_decl_stmt
 ARGS ((tTree ptype_decl));
extern tTree mif_stmt
 ARGS ((tTree pexpression, tTree pthen));
extern tTree mif_else_stmt
 ARGS ((tTree pexpression, tTree pthen, tTree pElse));
extern tTree mwhile_stmt
 ARGS ((tTree pexpression, tTree pstatement));
extern tTree mdo_stmt
 ARGS ((tTree pstatement, tTree pexpression));
extern tTree mbreak_stmt
 ARGS ((void));
extern tTree mbreak_id_stmt
 ARGS ((tTree pexpression));
extern tTree mcontinue_stmt
 ARGS ((void));
extern tTree mcontinue_id_stmt
 ARGS ((tTree pexpression));
extern tTree mreturn_stmt
 ARGS ((void));
extern tTree mreturn_expr_stmt
 ARGS ((tTree pexpression));
extern tTree mfor_stmt
 ARGS ((tTree pfor_init, tTree pexpression, tTree pfor_incr, tTree pstatement));
extern tTree mthrow_stmt
 ARGS ((tTree pexpression));
extern tTree msynchronized_stmt
 ARGS ((tTree pexpression, tTree pstatement));
extern tTree mlabeled_stmt
 ARGS ((tIdent pident, tPosition ppos, tTree pstatement));
extern tTree mtry_stmt
 ARGS ((tTree pstatement, tTree pcatch_list, tTree pfinally));
extern tTree mswitch_stmt
 ARGS ((tTree pexpression, tTree pswitch_list));
extern tTree mnull_stmt
 ARGS ((void));
extern tTree mswitch_list
 ARGS ((void));
extern tTree mnoswitch
 ARGS ((void));
extern tTree mswitch_
 ARGS ((tTree pnext, tTree pexpression_list, tTree pstatement_list));
extern tTree mcatch_list
 ARGS ((void));
extern tTree mnocatch
 ARGS ((void));
extern tTree mcatch
 ARGS ((tTree pnext, tTree pdecl_list, tTree pstatement));
extern tTree mtype_name_list
 ARGS ((void));
extern tTree mnotype_name
 ARGS ((void));
extern tTree mtype_name
 ARGS ((tTree pnext, tTree pnamed_type));
extern tTree mtype
 ARGS ((void));
extern tTree msimple_type
 ARGS ((ttype ptype));
extern tTree mnamed_type
 ARGS ((tTree pqualified_symbol));
extern tTree marray_type
 ARGS ((tTree ptype));
extern tTree mnotype
 ARGS ((void));
extern tTree mexpression_list
 ARGS ((void));
extern tTree mnoexpression_l
 ARGS ((void));
extern tTree mexpression
 ARGS ((tTree pnext, tTree pexpression));
extern tTree mexpression_c
 ARGS ((tPosition ppos));
extern tTree mqualified_symbol
 ARGS ((tPosition ppos));
extern tTree mqualification
 ARGS ((tPosition ppos, tTree pqualified_symbol, tIdent pident));
extern tTree mident
 ARGS ((tPosition ppos, tIdent pident));
extern tTree mnoexpression
 ARGS ((tPosition ppos));
extern tTree munary
 ARGS ((tPosition ppos, tTree pexpression, toperator poperator));
extern tTree mbinary
 ARGS ((tPosition ppos, tTree plop, tTree prop, toperator poperator));
extern tTree massign
 ARGS ((tPosition ppos, tTree plval, tTree prval, toperator poperator));
extern tTree maggregate
 ARGS ((tPosition ppos, tTree pexpression_list));
extern tTree mcall
 ARGS ((tPosition ppos, tTree pexpression, tTree pexpression_list));
extern tTree mselect
 ARGS ((tPosition ppos, tTree pexpression, tIdent pident));
extern tTree mget_class_of_expr
 ARGS ((tPosition ppos, tTree pexpression));
extern tTree mget_class
 ARGS ((tPosition ppos, tTree ptype));
extern tTree msubscript
 ARGS ((tPosition ppos, tTree pbase, tTree pindex));
extern tTree mtype_compare
 ARGS ((tPosition ppos, tTree pexpression, tTree ptype));
extern tTree mtype_cast
 ARGS ((tPosition ppos, tTree ptype, tTree pdims, tTree pexpression));
extern tTree mnew
 ARGS ((tPosition ppos, tTree ptype, tTree pdims, tTree pexpression_list, tTree pexpression));
extern tTree manonymous
 ARGS ((tPosition ppos, tTree ptype, tTree pexpression_list, tTree pblock));
extern tTree mconditional
 ARGS ((tPosition ppos, tTree pcondition, tTree ptrue_expr, tTree pfalse_expr));
extern tTree msuper
 ARGS ((tPosition ppos));
extern tTree mthis
 ARGS ((tPosition ppos));
extern tTree mnull
 ARGS ((tPosition ppos));
extern tTree mint_literal
 ARGS ((tPosition ppos, long pvalue));
extern tTree mlong_literal
 ARGS ((tPosition ppos, long pvalue));
extern tTree mbool_literal
 ARGS ((tPosition ppos, rbool pvalue));
extern tTree mfloat_literal
 ARGS ((tPosition ppos, tStringRef pvalue));
extern tTree mchar_literal
 ARGS ((tPosition ppos, tStringRef pvalue));
extern tTree mstring_literal
 ARGS ((tPosition ppos, tStringRef pvalue));
extern tTree menvs
 ARGS ((void));
extern tTree menv
 ARGS ((tTree pobjects, tTree penv, tTree pobject, tHashTable pHashTable, int pHashSize));
extern tTree menv2
 ARGS ((tTree penv1, tTree penv2));
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

