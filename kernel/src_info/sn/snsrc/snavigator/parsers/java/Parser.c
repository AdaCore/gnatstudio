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

/* $Id$ */

#include "Parser.h"

#ifdef __cplusplus
extern "C" {
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#include <string.h>
}
#else
#include "Reuse.h"
#include "rMemory.h"
#include "General.h"
#include "DynArray.h"
#ifndef NO_RECOVER
#include "Sets.h"
#endif
#if ! defined NO_RECOVER | defined YYReParse
#include "Errors.h"
#endif
#if defined YYDEBUG | defined lex_interface
#include "Position.h"
#endif
#include <stdio.h>
#endif

#ifdef lex_interface
#define GetToken	yylex
     extern int yylex ARGS ((void));
#ifndef AttributeDef
#include "Position.h"
	   typedef struct { tPosition Position; } tScanAttribute;
	   tScanAttribute	Attribute = {{ 0, 0 }};
#endif
#ifndef ErrorAttributeDef
#define ErrorAttribute(Token, RepairAttribute)
#endif
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) * yyAttrStackPtr = yylval
#endif
#else
#include "Scanner.h"
#ifndef yyGetAttribute
#define yyGetAttribute(yyAttrStackPtr, a) (yyAttrStackPtr)->Scan = a
#endif
#endif

typedef unsigned short	yyStateRange	;
typedef unsigned short	yySymbolRange	;
typedef struct { yyStateRange Check, Next; } yytComb;
typedef enum {
yyNT0_intern	= 105,
yyNTGoal	= 106,
yyNTType	= 107,
yyNTPrimitiveType	= 108,
yyNTReferenceType	= 109,
yyNTClassOrInterfaceType	= 110,
yyNTClassType	= 111,
yyNTInterfaceType	= 112,
yyNTArrayType	= 113,
yyNTName	= 114,
yyNTCompilationUnit	= 115,
yyNTImportDeclarations	= 116,
yyNTTypeDeclarations	= 117,
yyNTPackageDeclaration	= 118,
yyNTImportDeclaration	= 119,
yyNTTypeDeclaration	= 120,
yyNTModifiers	= 121,
yyNTModifier	= 122,
yyNTClassDeclaration	= 123,
yyNTxx_ClassDeclaration_1_3	= 124,
yyNTxx_ClassDeclaration_2_4	= 125,
yyNTSuper	= 126,
yyNTInterfaces	= 127,
yyNTInterfaceTypeList	= 128,
yyNTClassBody	= 129,
yyNTClassBodyDeclarations	= 130,
yyNTClassBodyDeclaration	= 131,
yyNTget_type_1	= 132,
yyNTget_type_2	= 133,
yyNTFieldDeclaration	= 134,
yyNTVariableDeclarators	= 135,
yyNTVariableDeclarator	= 136,
yyNTVariableDeclaratorId	= 137,
yyNTLocalVariableDeclaration	= 138,
yyNTxx_LocalVariableDeclaration_1_2	= 139,
yyNTxx_LocalVariableDeclaration_2_3	= 140,
yyNTLocalVariableDeclarators	= 141,
yyNTLocalVariableDeclarator	= 142,
yyNTMethodDeclaration	= 143,
yyNTMethodHeader	= 144,
yyNTMethodDeclarator	= 145,
yyNTMethodBody	= 146,
yyNTFormalParameterListOpt	= 147,
yyNTFormalParameterList	= 148,
yyNTFormalParameter	= 149,
yyNTThrowsOpt	= 150,
yyNTClassTypeList	= 151,
yyNTClassInitializer	= 152,
yyNTConstructorDeclaration	= 153,
yyNTConstructorDeclarator	= 154,
yyNTConstructorBody	= 155,
yyNTExplicitConstructorInvocation	= 156,
yyNTInterfaceDeclaration	= 157,
yyNTxx_InterfaceDeclaration_1_3	= 158,
yyNTxx_InterfaceDeclaration_2_4	= 159,
yyNTExtendsInterfacesOpt	= 160,
yyNTExtendsInterfaces	= 161,
yyNTInterfaceBody	= 162,
yyNTInterfaceMemberDeclarations	= 163,
yyNTInterfaceMemberDeclaration	= 164,
yyNTVariableInitializerOpt	= 165,
yyNTVariableInitializer	= 166,
yyNTArrayInitializer	= 167,
yyNTVariableInitializersOpt	= 168,
yyNTVariableInitializers	= 169,
yyNTCommaOpt	= 170,
yyNTArrayInitializerOpt	= 171,
yyNTBlock	= 172,
yyNTBlockStatements	= 173,
yyNTBlockStatement	= 174,
yyNTStatement	= 175,
yyNTStatementNoShortIf	= 176,
yyNTStatementWithoutTrailingSubstatement	= 177,
yyNTStatementExpression	= 178,
yyNTSwitchBlock	= 179,
yyNTSwitchBlockStatementGroups	= 180,
yyNTSwitchBlockStatementGroup	= 181,
yyNTSwitchLabels	= 182,
yyNTSwitchLabel	= 183,
yyNTForInit	= 184,
yyNTExpressionOpt	= 185,
yyNTForUpdate	= 186,
yyNTStatementExpressionList	= 187,
yyNTCatchesOpt	= 188,
yyNTCatches	= 189,
yyNTCatchClause	= 190,
yyNTFinally	= 191,
yyNTPrimary	= 192,
yyNTxx_Primary_13_6	= 193,
yyNTPrimaryNoNewArray	= 194,
yyNTClassInstanceCreationExpression	= 195,
yyNTFieldAccess	= 196,
yyNTMethodInvocation	= 197,
yyNTArrayAccess	= 198,
yyNTDimExprs	= 199,
yyNTDimExpr	= 200,
yyNTDims	= 201,
yyNTArgumentListOpt	= 202,
yyNTArgumentList	= 203,
yyNTcExpression	= 204,
yyNTExpression	= 205,
yyNTAssignment	= 206,
yyNTLeftHandSide	= 207,
yyNTAssignmentOperator	= 208
} yytNonterminal;
typedef struct { short yyMode; rbool yyActions, yyMessages; } yytControl;

static	yytControl	yyControl	= { 0, rtrue, rtrue };
	rbool		Parser_Debug	= rfalse;

#define yyFirstTerminal	0
#define yyLastTerminal	104
#define yySetSize	105
#define yyFirstSymbol	0
#define yyLastSymbol	208
#define yyTTableMax	3769
#define yyNTableMax	513
#define yyStartState	1
#define yyFirstReadState	1
#define yyLastReadState	407
#define yyFirstReadReduceState	408
#define yyLastReadReduceState	492
#define yyFirstReduceState	493
#define yyLastReduceState	836
#define yyLastState	836
#define yyLastStopState	493
#define YYTDefault
#define YYNDefault

#define yyFirstFinalState	yyFirstReadReduceState

#define ErrorMessages(Messages) yyControl.yyMessages = Messages
#define SemActions(Actions)	 yyControl.yyActions = Actions

#ifdef YYGetLook

#define GetLookahead(k)	yyGetLookahead ((k) - 1, yyTerminal)
#define GetAttribute(k, a)	xxGetAttribute ((k) - 1, yyTerminal, a)

static int yyGetLookahead	ARGS ((int yyk, yySymbolRange yyToken));
static void xxGetAttribute	ARGS ((int yyk, yySymbolRange yyToken,
				tScanAttribute * yyAttribute));

#endif

/* line 19 "Parser.lrk" */


#include "stdlib.h"
#include "StringM.h"
#include "Tree.h"
#include "Eval.h"
#include "Trafo.h"
#include "deftab.h"
#include "sn.h"

#define NIL (char *) NULL

extern	FILE *	cross_ref_fp;

	FILE *	yyin;
	FILE *	hlfp;
	int	hl;
	char *	current_file;
	tIdent	i_anonymous;

static	tString	current_ident, current_class;
static	char	current_string		[256];
static	char	ret_buffer		[1024];
static	char	arg_types_buffer	[1024];
static	char	args_buffer		[1024];
static	unsigned long current_attr;
static	tTree	current_type;

#define SHARE

#ifdef SHARE

static	tTree
   snocatch		,
   snodecl		,
   snoexpression	,
   snoexpression_l	,
   snofield		,
   snoimport		,
   snostatement		,
   snoswitch		,
   snotype		,
   snotype_name		;

#define dnocatch		snocatch
#define dnodecl		snodecl
#define dnoexpression		snoexpression
#define dnoexpression_l	snoexpression_l
#define dnofield		snofield
#define dnoimport		snoimport
#define dnostatement		snostatement
#define dnoswitch		snoswitch
#define dnotype		snotype
#define dnotype_name		snotype_name

#else

#define dnocatch		mnocatch ()
#define dnodecl		mnodecl ()
#define dnoexpression		mnoexpression (NoPosition)
#define dnoexpression_l	mnoexpression_l ()
#define dnofield		mnofield ()
#define dnoimport		mnoimport ()
#define dnostatement		mnostatement ()
#define dnoswitch		mnoswitch ()
#define dnotype		mnotype ()
#define dnotype_name		mnotype_name ()

#endif

static	rbool	is_initialized	= rfalse;
static	rbool	report		= rfalse;
static	rbool	graphic		= rfalse;

static void get_options ARGS ((void))
{
   char * arg	= getenv ("PAF_JAVA");

   if (arg == NULL) return;

   while (* arg) {
      if (* arg == '-') {
	 switch (* ++ arg) {
	 case 'f': report = rtrue; break;
	 case 'g': graphic = rtrue; break;

	 case 'h':
	    (void) printf ("\nsyntax of PAF_JAVA:\n\n");
	    (void) printf ("   [-f] [-h]\n\n");
	    (void) printf (" f : report error messages (default: do not report)\n");
	    (void) printf (" h : help\n\n");
	    break;
	 }
      }
      arg ++;
   }
}

void start_parser (fname, parse_cplpl, highl_fp, highlight)
   char	*	fname;
   int		parse_cplpl;
   FILE	*	highl_fp;
   int		highlight;
{
   current_file = fname;
   hlfp = highl_fp;
   hl = highlight;
   if (! is_initialized) { get_options (); is_initialized = rtrue; }
   StoreMessages	(rtrue);
   InitStringMemory	();
   InitIdents		();
   init_deftab		();
   if (fname) Attribute.Position.FileName = MakeIdent (fname, strlen (fname));
   else       Attribute.Position.FileName = NoIdent;
   BeginFile		("");
   (void) Parser	();
   /* (void) CheckTree	(TreeRoot); */
   if (cross_ref_fp != NULL) {
      Eval		(TreeRoot);
      Traverse		(TreeRoot);
   }
#ifdef DRAWTREE
   if (graphic) DrawTree (TreeRoot);
#endif
   ReleaseTreeModule	();
   CloseStringMemory	();
   if (report) WriteMessages (stderr);
}

static	int	class_stack_ptr	= 0;
static	char *	class_stack	[32] = { NIL };

#define push(ident)  { class_stack [++ class_stack_ptr] = current_class; \
			current_class = GetCStr (ident); }
#define pop()	      { current_class = class_stack [class_stack_ptr --]; }
#define top()	        class_stack [class_stack_ptr]

static void get_bpos (tree, l, c)
   tTree	tree;
   int *	l;
   int *	c;
{
   while (tree->Kind == kqualification)
      tree = tree->qualification.qualified_symbol;
   * l = tree->ident.pos.Line;
   * c = tree->ident.pos.Column - 1;
}

static void put_extends (tree)
   tTree	tree;
{
   while (tree->Kind == ktype_name) {
      tTree symbol = tree->type_name.named_type->named_type.qualified_symbol;
      to_types (symbol, current_string);
      if (symbol->Kind == kident)
	 put_symbol (PAF_CLASS_INHERIT, current_class, current_string, current_file,
	 (int) symbol->ident.pos.Line,
	 (int) symbol->ident.pos.Column - 1,
	 (int) symbol->ident.pos.Line,
	 (int) (symbol->ident.pos.Column + LengthSt (GetStringRef (symbol->ident.ident)) - 1),
	 (unsigned long) PAF_PUBLIC, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
      else {
	 int l, c;
         get_bpos (symbol->qualification.qualified_symbol, & l, & c);
	 put_symbol (PAF_CLASS_INHERIT, current_class, current_string, current_file,
	 l, c, (int) symbol->qualification.pos.Line,
	 (int) (symbol->qualification.pos.Column + LengthSt (GetStringRef (symbol->qualification.ident)) - 1),
	 (unsigned long) PAF_PUBLIC, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
      }
      tree = tree->type_name.next;
   }
}

typedef struct { tTree tree; } zzType;
typedef struct { tTree tree; } zzPrimitiveType;
typedef struct { tTree tree; } zzReferenceType;
typedef struct { tTree tree; tPosition bpos; tPosition epos; } zzClassOrInterfaceType;
typedef struct { tTree tree; tPosition bpos; tPosition epos; } zzClassType;
typedef struct { tTree tree; } zzInterfaceType;
typedef struct { tTree tree; } zzArrayType;
typedef struct { tTree tree; tPosition bpos; tPosition epos; } zzName;
typedef struct { tTree tree; } zzImportDeclarations;
typedef struct { tTree tree; } zzTypeDeclarations;
typedef struct { tTree tree; } zzPackageDeclaration;
typedef struct { tTree tree; } zzImportDeclaration;
typedef struct { tTree tree; } zzTypeDeclaration;
typedef struct { long attr; } zzModifiers;
typedef struct { long attr; } zzModifier;
typedef struct { tTree tree; } zzClassDeclaration;
typedef struct { tTree tree; } zzSuper;
typedef struct { tTree tree; } zzInterfaces;
typedef struct { tTree tree; } zzInterfaceTypeList;
typedef struct { tTree tree; tPosition epos; } zzClassBody;
typedef struct { tTree tree; } zzClassBodyDeclarations;
typedef struct { tTree tree; } zzClassBodyDeclaration;
typedef struct { tTree tree; } zzFieldDeclaration;
typedef struct { tTree tree; } zzVariableDeclarators;
typedef struct { tTree tree; } zzVariableDeclarator;
typedef struct { tTree tree; tIdent ident; tPosition Position; } zzVariableDeclaratorId;
typedef struct { tTree tree; } zzLocalVariableDeclaration;
typedef struct { tTree tree; } zzLocalVariableDeclarators;
typedef struct { tTree tree; } zzLocalVariableDeclarator;
typedef struct { tTree tree; } zzMethodDeclaration;
typedef struct { tTree tree; tIdent ident; tPosition Position; tTree array; long attr; tTree type; tTree throws; } zzMethodHeader;
typedef struct { tTree tree; tIdent ident; tPosition Position; tTree array; } zzMethodDeclarator;
typedef struct { tTree tree; tPosition epos; } zzMethodBody;
typedef struct { tTree tree; } zzFormalParameterListOpt;
typedef struct { tTree tree; } zzFormalParameterList;
typedef struct { tTree tree; } zzFormalParameter;
typedef struct { tTree tree; } zzThrowsOpt;
typedef struct { tTree tree; } zzClassTypeList;
typedef struct { tTree tree; } zzClassInitializer;
typedef struct { tTree tree; } zzConstructorDeclaration;
typedef struct { tTree tree; tIdent ident; tPosition Position; } zzConstructorDeclarator;
typedef struct { tTree tree; tPosition epos; } zzConstructorBody;
typedef struct { tTree tree; } zzExplicitConstructorInvocation;
typedef struct { tTree tree; } zzInterfaceDeclaration;
typedef struct { tTree tree; } zzExtendsInterfacesOpt;
typedef struct { tTree tree; } zzExtendsInterfaces;
typedef struct { tTree tree; tPosition epos; } zzInterfaceBody;
typedef struct { tTree tree; } zzInterfaceMemberDeclarations;
typedef struct { tTree tree; } zzInterfaceMemberDeclaration;
typedef struct { tTree tree; } zzVariableInitializerOpt;
typedef struct { tTree tree; } zzVariableInitializer;
typedef struct { tTree tree; } zzVariableInitializersOpt;
typedef struct { tTree tree; } zzVariableInitializers;
typedef struct { tTree tree; } zzArrayInitializerOpt;
typedef struct { tTree tree; tPosition epos; } zzBlock;
typedef struct { tTree tree; } zzBlockStatements;
typedef struct { tTree tree; } zzBlockStatement;
typedef struct { tTree tree; } zzStatementNoShortIf;
typedef struct { tTree tree; } zzStatementWithoutTrailingSubstatement;
typedef struct { tTree tree; } zzStatementExpression;
typedef struct { tTree tree; } zzSwitchBlock;
typedef struct { tTree tree; } zzSwitchBlockStatementGroups;
typedef struct { tTree tree; } zzSwitchBlockStatementGroup;
typedef struct { tTree tree; } zzSwitchLabels;
typedef struct { tTree tree; } zzSwitchLabel;
typedef struct { tTree tree; } zzForInit;
typedef struct { tTree tree; } zzExpressionOpt;
typedef struct { tTree tree; } zzForUpdate;
typedef struct { tTree tree; } zzStatementExpressionList;
typedef struct { tTree tree; } zzCatchesOpt;
typedef struct { tTree tree; } zzCatches;
typedef struct { tTree tree; } zzCatchClause;
typedef struct { tTree tree; } zzFinally;
typedef struct { tTree tree; } zzPrimary;
typedef struct { tTree tree; } zzDimExprs;
typedef struct { tTree tree; } zzDimExpr;
typedef struct { tTree tree; } zzDims;
typedef struct { tTree tree; } zzArgumentListOpt;
typedef struct { tTree tree; } zzArgumentList;
typedef struct { tTree tree; } zzcExpression;
typedef struct { tTree tree; } zzExpression;
typedef struct { tTree tree; } zzLeftHandSide;
typedef struct { int operator; tPosition Position; } zzAssignmentOperator;

typedef union {
tScanAttribute Scan;
zzType Type;
zzPrimitiveType PrimitiveType;
zzReferenceType ReferenceType;
zzClassOrInterfaceType ClassOrInterfaceType;
zzClassType ClassType;
zzInterfaceType InterfaceType;
zzArrayType ArrayType;
zzName Name;
zzImportDeclarations ImportDeclarations;
zzTypeDeclarations TypeDeclarations;
zzPackageDeclaration PackageDeclaration;
zzImportDeclaration ImportDeclaration;
zzTypeDeclaration TypeDeclaration;
zzModifiers Modifiers;
zzModifier Modifier;
zzClassDeclaration ClassDeclaration;
zzSuper Super;
zzInterfaces Interfaces;
zzInterfaceTypeList InterfaceTypeList;
zzClassBody ClassBody;
zzClassBodyDeclarations ClassBodyDeclarations;
zzClassBodyDeclaration ClassBodyDeclaration;
zzFieldDeclaration FieldDeclaration;
zzVariableDeclarators VariableDeclarators;
zzVariableDeclarator VariableDeclarator;
zzVariableDeclaratorId VariableDeclaratorId;
zzLocalVariableDeclaration LocalVariableDeclaration;
zzLocalVariableDeclarators LocalVariableDeclarators;
zzLocalVariableDeclarator LocalVariableDeclarator;
zzMethodDeclaration MethodDeclaration;
zzMethodHeader MethodHeader;
zzMethodDeclarator MethodDeclarator;
zzMethodBody MethodBody;
zzFormalParameterListOpt FormalParameterListOpt;
zzFormalParameterList FormalParameterList;
zzFormalParameter FormalParameter;
zzThrowsOpt ThrowsOpt;
zzClassTypeList ClassTypeList;
zzClassInitializer ClassInitializer;
zzConstructorDeclaration ConstructorDeclaration;
zzConstructorDeclarator ConstructorDeclarator;
zzConstructorBody ConstructorBody;
zzExplicitConstructorInvocation ExplicitConstructorInvocation;
zzInterfaceDeclaration InterfaceDeclaration;
zzExtendsInterfacesOpt ExtendsInterfacesOpt;
zzExtendsInterfaces ExtendsInterfaces;
zzInterfaceBody InterfaceBody;
zzInterfaceMemberDeclarations InterfaceMemberDeclarations;
zzInterfaceMemberDeclaration InterfaceMemberDeclaration;
zzVariableInitializerOpt VariableInitializerOpt;
zzVariableInitializer VariableInitializer;
zzVariableInitializersOpt VariableInitializersOpt;
zzVariableInitializers VariableInitializers;
zzArrayInitializerOpt ArrayInitializerOpt;
zzBlock Block;
zzBlockStatements BlockStatements;
zzBlockStatement BlockStatement;
zzStatementNoShortIf StatementNoShortIf;
zzStatementWithoutTrailingSubstatement StatementWithoutTrailingSubstatement;
zzStatementExpression StatementExpression;
zzSwitchBlock SwitchBlock;
zzSwitchBlockStatementGroups SwitchBlockStatementGroups;
zzSwitchBlockStatementGroup SwitchBlockStatementGroup;
zzSwitchLabels SwitchLabels;
zzSwitchLabel SwitchLabel;
zzForInit ForInit;
zzExpressionOpt ExpressionOpt;
zzForUpdate ForUpdate;
zzStatementExpressionList StatementExpressionList;
zzCatchesOpt CatchesOpt;
zzCatches Catches;
zzCatchClause CatchClause;
zzFinally Finally;
zzPrimary Primary;
zzDimExprs DimExprs;
zzDimExpr DimExpr;
zzDims Dims;
zzArgumentListOpt ArgumentListOpt;
zzArgumentList ArgumentList;
zzcExpression cExpression;
zzExpression Expression;
zzLeftHandSide LeftHandSide;
zzAssignmentOperator AssignmentOperator;
} tParsAttribute;


#if defined lex_interface & ! defined yylvalDef
     tParsAttribute yylval;
#endif

#ifndef yyInitStackSize
#define yyInitStackSize	100
#endif
#ifndef MY_ERROR
#define MY_ERROR
#endif
#define yyNoState		0
#define yystandard		1
#define yytrial		2
#define yybuffer		4
#define yyreparse		8
#define yyS			yySynAttribute
#define yyA			yyAttrStackPtr
#define YYACCEPT		goto yyAccept
#define YYABORT		goto yyAbort

#ifdef YYDEC_TABLE
#define yyDecrement(x)
#define yySetNT(x)
#else
#define yyDecrement(x)		yyStateStackPtr -= x; yyAttrStackPtr -= x;
#define yySetNT(x)		yyNonterminal = x;
#endif

#ifdef YYNDefault
#define yytNComb yytComb
#else
#define yytNComb yyStateRange
#endif

#if defined YYDEBUG | defined YYDCRP
static	long		yyCount		= 0;
static	FILE *		yyTrace		;

static	void		yyPrintState	ARGS ((yyStateRange yyState));
static	void		yyNl		ARGS ((void));

static	char *		yyRule		[] = { 0,
""
};
#endif
	char *		Parser_TokenName	[yyLastTerminal + 2] = {
"_EOF_",
"Identifier",
"IntegerLiteral",
"LongLiteral",
"FloatingPointLiteral",
"DoubleLiteral",
"CharacterLiteral",
"StringLiteral",
"package",
".",
";",
"import",
"*",
"class",
"extends",
"implements",
",",
"{",
"byte",
"short",
"int",
"long",
"char",
"float",
"double",
"boolean",
"[",
"]",
"void",
"=",
"true",
"false",
"null",
"this",
"(",
")",
"new",
"super",
"}",
"++",
"--",
"+",
"-",
"~",
"!",
"/",
"%",
"<<",
">>",
">>>",
"<",
">",
"<=",
">=",
"instanceof",
"==",
"!=",
"&",
"^",
"|",
"&&",
"||",
"?",
":",
"*=",
"/=",
"%=",
"+=",
"-=",
"<<=",
">>=",
">>>=",
"&=",
"^=",
"|=",
"public",
"protected",
"private",
"static",
"abstract",
"final",
"native",
"synchronized",
"transient",
"volatile",
"throws",
"interface",
"switch",
"case",
"default",
"do",
"while",
"break",
"continue",
"return",
"throw",
"try",
"catch",
"finally",
"if",
"else",
"for",
"CAST",
"UNARY_OP",
"POST_OP",
""
};
static	yytComb		yyTComb		[yyTTableMax + 1] = {
{   3,  493}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 114,  440}, {  39,  420}, 
{ 242,  669}, {  10,   14}, {  10,  424}, { 242,  669}, { 320,  440}, 
{ 133,  223}, {  26,  420}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, {  24,  420}, { 133,  224}, { 242,  669}, { 325,  774}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{  53,   50}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 319,   75}, { 319,   76}, { 133,  225}, { 319,   77}, { 319,   78}, 
{   5,    7}, {   5,  423}, {  53,  409}, {  53,  410}, {  53,  411}, 
{  53,  412}, {  53,  413}, {  53,  414}, {  53,  415}, {  53,  416}, 
{  22,  543}, {  22,  543}, {  53,  408}, {  22,  543}, { 325,  774}, 
{ 325,  774}, { 325,  774}, { 325,  774}, { 325,  774}, { 325,  774}, 
{ 325,  774}, { 325,  774}, { 325,  774}, { 325,  774}, { 325,  774}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{  69,  497}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 162,  251}, { 242,  699}, { 242,  669}, 
{ 242,  669}, { 242,  669}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, { 192,  590}, 
{  21,   26}, { 354,  645}, {  31,  420}, {  21,  608}, { 354,  645}, 
{ 192,  279}, { 192,  590}, { 162,  252}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, {  15,  542}, {  15,  542}, { 354,  645}, 
{  15,  542}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, {  34,  613}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, {  20,   24}, {  20,  544}, {  34,  613}, {  20,  544}, 
{  33,   39}, {  33,  609}, {  88,  152}, {  34,  613}, {  34,  613}, 
{  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, 
{  34,  613}, {  88,  463}, {  44,  560}, {  34,  613}, {  75,  409}, 
{  75,  410}, {  75,  411}, {  75,  412}, {  75,  413}, {  75,  414}, 
{  75,  415}, {  75,  416}, {  25,   31}, {  34,  613}, {  25,  546}, 
{ 369,  472}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 255,  323}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, { 354,  645}, 
{ 354,  645}, { 354,  645}, { 354,  645}, { 401,  144}, {  50,  514}, 
{ 354,  645}, { 354,  651}, { 354,  645}, { 349,  514}, { 193,  280}, 
{ 255,  781}, { 349,  514}, {  50,  514}, { 193,  281}, { 193,  446}, 
{  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, 
{  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, {  34,  613}, 
{ 349,  514}, {  34,  613}, { 125,  211}, { 349,  514}, {  50,  514}, 
{ 369,  366}, { 369,  367}, { 141,  572}, { 349,  514}, {  18,   23}, 
{ 125,  212}, { 125,  785}, {  50,   61}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, {  17,   22}, { 321,  773}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  374}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, 
{ 349,  514}, { 349,  514}, { 349,  514}, { 349,  514}, { 223,  718}, 
{  37,   42}, {  37,  547}, { 223,  718}, { 321,  773}, { 321,  773}, 
{ 321,  773}, { 321,  773}, { 321,  773}, { 321,  773}, { 321,  773}, 
{ 321,  773}, { 321,  773}, { 321,  773}, { 321,  773}, {  89,  153}, 
{ 199,  439}, { 223,  753}, {  23,  607}, { 401,  146}, { 223,  769}, 
{  23,  607}, { 199,  195}, {  16,  606}, {  89,  465}, { 223,  302}, 
{  16,  606}, { 401,  147}, {  62,  514}, { 401,  148}, { 223,  718}, 
{ 223,  718}, { 223,  718}, { 223,  718}, { 388,  684}, { 104,  481}, 
{ 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, 
{ 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, 
{ 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, { 223,  718}, 
{ 223,  718}, { 223,  718}, { 223,  718}, {  95,  571}, { 223,  769}, 
{ 223,  769}, { 223,  769}, { 223,  769}, { 223,  769}, { 223,  769}, 
{ 223,  769}, { 223,  769}, { 223,  769}, { 223,  769}, { 223,  769}, 
{ 149,  162}, { 112,  108}, {  97,  508}, { 149,  785}, { 104,  482}, 
{ 104,  483}, { 104,  484}, { 104,  485}, { 104,  486}, { 104,  487}, 
{ 104,  488}, { 104,  489}, { 104,  490}, { 104,  491}, { 104,  492}, 
{ 246,  690}, { 202,  587}, { 149,  237}, { 388,  684}, { 388,  684}, 
{ 149,  822}, { 246,  319}, { 195,  440}, { 307,  570}, {  62,  117}, 
{ 149,  164}, { 202,  196}, {  97,  163}, { 368,  471}, { 307,  320}, 
{ 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, { 118,  204}, 
{ 202,  587}, { 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, 
{ 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, 
{ 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, 
{ 149,  785}, { 149,  785}, { 149,  785}, { 149,  785}, { 118,  205}, 
{ 149,  822}, { 149,  822}, { 149,  822}, { 149,  822}, { 149,  822}, 
{ 149,  822}, { 149,  822}, { 149,  822}, { 149,  822}, { 149,  822}, 
{ 149,  822}, { 256,  717}, { 219,  297}, {  30,  508}, { 256,  717}, 
{ 159,  689}, { 108,  567}, { 210,  290}, {  30,  508}, {  30,  508}, 
{  30,  508}, { 219,  290}, { 108,  567}, { 368,  366}, { 368,  367}, 
{ 161,  249}, { 210,  291}, { 160,  440}, { 256,  752}, {  30,  508}, 
{ 111,  589}, { 256,  768}, { 108,  567}, { 159,   69}, {  48,  420}, 
{ 108,  567}, { 256,  324}, {  30,  508}, { 157,  240}, { 157,  698}, 
{ 108,  194}, { 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, 
{ 161,  250}, { 111,  198}, { 256,  717}, { 256,  717}, { 256,  717}, 
{ 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, 
{ 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, 
{ 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, { 256,  717}, 
{  40,   11}, { 256,  768}, { 256,  768}, { 256,  768}, { 256,  768}, 
{ 256,  768}, { 256,  768}, { 256,  768}, { 256,  768}, { 256,  768}, 
{ 256,  768}, { 256,  768}, { 136,  162}, { 136,  785}, { 105,  589}, 
{ 136,  785}, { 165,  256}, { 247,  619}, { 154,  785}, { 136,  785}, 
{ 159,  244}, { 116,  203}, { 124,  208}, { 247,  619}, { 154,  785}, 
{  40,  452}, {  42,  420}, { 106,  420}, { 395,  319}, { 136,  227}, 
{ 136,  785}, { 124,  209}, {  11,   15}, { 247,  196}, { 154,  237}, 
{ 154,  785}, { 247,  197}, { 136,  212}, { 136,  785}, {   7,  421}, 
{ 116,  584}, { 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, 
{ 136,  785}, { 154,  785}, { 395,  695}, { 136,  785}, { 136,  785}, 
{ 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, 
{ 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, 
{ 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, { 136,  785}, 
{ 136,  785}, { 136,  785}, { 303,  718}, { 303,  718}, { 286,  588}, 
{ 303,  718}, { 154,  785}, { 331,  360}, {  40,   12}, { 303,  718}, 
{   8,  420}, { 214,  294}, {  60,  561}, { 226,  303}, { 297,  456}, 
{ 207,  288}, { 279,  420}, { 105,  106}, {  12,   16}, { 303,  753}, 
{ 303,  718}, {   2,  420}, {  56,  108}, { 286,  588}, { 201,  440}, 
{ 230,  440}, {  45,  495}, { 303,  333}, { 303,  718}, { 331,  628}, 
{ 228,  305}, { 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, 
{ 303,  718}, { 297,  628}, { 119,  118}, { 303,  718}, { 303,  718}, 
{ 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, 
{ 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, 
{ 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, { 303,  718}, 
{ 303,  718}, { 303,  718}, { 305,  717}, { 305,  717}, { 121,  207}, 
{ 305,  717}, { 137,  228}, {  96,  161}, { 135,  161}, { 305,  717}, 
{ 134,  226}, {  76,  133}, { 318,  692}, { 371,  390}, {  19,  425}, 
{ 372,  462}, { 355,  379}, {  90,  467}, { 245,  318}, { 305,  752}, 
{ 305,  717}, {  49,  453}, { 312,  346}, { 153,  466}, { 403,  405}, 
{ 109,  438}, { 381,  451}, { 305,  337}, { 305,  717}, { 382,  450}, 
{ 248,  569}, { 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, 
{ 305,  717}, {  98,  459}, {  54,  444}, { 305,  717}, { 305,  717}, 
{ 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, 
{ 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, 
{ 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, { 305,  717}, 
{ 305,  717}, { 305,  717}, { 215,  714}, { 215,  714}, { 100,  461}, 
{ 215,  714}, { 156,  469}, { 152,  464}, { 393,  399}, { 215,  714}, 
{ 155,  468}, {  14,   19}, { 107,  193}, { 373,   51}, {  27,   34}, 
{ 197,  285}, { 191,  193}, { 316,   51}, {  32,   38}, { 215,  749}, 
{ 215,  714}, {  41,   38}, {  36,   34}, {  92,   51}, { 364,   38}, 
{ 309,  343}, { 308,   51}, { 299,  290}, { 215,  714}, { 138,  229}, 
{ 270,   57}, { 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, 
{ 215,  714}, { 130,  217}, { 102,  166}, { 215,  714}, { 215,  714}, 
{ 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, 
{ 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, 
{ 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, { 215,  714}, 
{ 215,  714}, { 215,  714}, { 225,  724}, { 225,  724}, { 296,  476}, 
{ 225,  724}, { 196,  441}, { 208,  477}, {  59,  418}, { 225,  724}, 
{ 306,  342}, { 304,  341}, { 163,  418}, {  57,  417}, {  58,  419}, 
{ 298,  477}, { 253,  321}, { 290,  478}, { 198,  443}, { 225,  759}, 
{ 225,  724}, { 257,  325}, { 244,  408}, { 146,  234}, { 350,  375}, 
{ 240,  315}, { 238,  313}, {  87,  151}, { 225,  724}, { 351,  376}, 
{ 352,  377}, { 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, 
{ 225,  724}, {  85,  143}, { 206,  287}, { 225,  724}, { 225,  724}, 
{ 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, 
{ 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, 
{ 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, { 225,  724}, 
{ 225,  724}, { 225,  724}, {  67,  708}, {  67,  708}, { 147,  235}, 
{  67,  708}, { 132,  222}, {  93,  158}, {  94,  159}, {  67,  708}, 
{ 148,  236}, { 281,  328}, { 145,  142}, { 131,  509}, { 243,  317}, 
{ 348,  373}, { 311,  345}, { 347,  372}, { 310,  344}, {  67,  743}, 
{  67,  708}, { 239,  314}, { 356,  380}, { 358,  381}, { 359,  382}, 
{ 362,  383}, { 232,  309}, { 231,  308}, {  67,  708}, { 340,  365}, 
{ 363,  384}, {  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, 
{  67,  708}, { 301,  339}, { 406,  407}, {  67,  708}, {  67,  708}, 
{  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, 
{  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, 
{  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, {  67,  708}, 
{  67,  708}, {  67,  708}, { 220,  630}, { 220,  630}, {   9,  516}, 
{ 220,  630}, { 396,  401}, { 115,  445}, { 254,  322}, { 220,  630}, 
{ 394,  400}, { 213,  293}, { 405,  694}, {  61,  583}, {   9,  422}, 
{ 392,  398}, { 328,  780}, {   9,   11}, { 332,  361}, { 220,  298}, 
{ 220,  630}, { 391,  397}, { 292,  335}, { 128,  215}, { 284,  442}, 
{ 237,  121}, { 343,  470}, { 338,  455}, { 220,  630}, { 387,  473}, 
{  51,  457}, { 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, 
{ 220,  630}, {  99,  458}, { 283,  447}, { 220,  630}, { 220,  630}, 
{ 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, 
{ 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, 
{ 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, { 220,  630}, 
{ 220,  630}, { 220,  630}, {  46,  507}, { 282,  448}, { 330,  454}, 
{ 360,  629}, { 144,  233}, {  43,   50}, { 278,  326}, { 386,  474}, 
{ 367,  475}, {  46,  507}, { 203,  113}, {  46,  507}, { 150,  238}, 
{ 241,  316}, {  43,  437}, {  46,  507}, { 353,  378}, { 370,  389}, 
{ 402,  404}, {   0,    0}, {   0,    0}, {  43,   51}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  46,   58}, {  46,  507}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   9,   12}, {  46,  507}, {   0,    0}, {   0,    0}, {  46,  507}, 
{  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, {  43,  436}, 
{   0,    0}, {  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, 
{  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, 
{  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, 
{  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, {  46,  507}, 
{  70,  710}, {  70,  710}, {   0,    0}, {  70,  710}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  70,  710}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  70,  745}, {  70,  710}, {  43,   52}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  70,  710}, {   0,    0}, {   0,    0}, {  70,  710}, 
{  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, {   0,    0}, 
{   0,    0}, {  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, 
{  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, 
{  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, 
{  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, {  70,  710}, 
{ 224,  720}, { 224,  720}, {   0,    0}, { 224,  720}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 224,  720}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 224,  755}, { 224,  720}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 224,  720}, {   0,    0}, {   0,    0}, { 224,  720}, 
{ 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, {   0,    0}, 
{   0,    0}, { 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, 
{ 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, 
{ 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, 
{ 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, { 224,  720}, 
{  68,  709}, {  68,  709}, {   0,    0}, {  68,  709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  68,  709}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  68,  744}, {  68,  709}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  68,  709}, {   0,    0}, {   0,    0}, {  68,  709}, 
{  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, {   0,    0}, 
{   0,    0}, {  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, 
{  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, 
{  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, 
{  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, {  68,  709}, 
{ 385,  716}, { 385,  716}, {   0,    0}, { 385,  716}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 385,  716}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 385,  751}, { 385,  716}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 385,  716}, {   0,    0}, {   0,    0}, { 385,  716}, 
{ 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, {   0,    0}, 
{   0,    0}, { 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, 
{ 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, 
{ 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, 
{ 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, { 385,  716}, 
{ 342,  731}, { 342,  731}, {   0,    0}, { 342,  731}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 342,  731}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 342,  766}, { 342,  731}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 342,  731}, {   0,    0}, {   0,    0}, { 342,  731}, 
{ 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, {   0,    0}, 
{   0,    0}, { 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, 
{ 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, 
{ 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, 
{ 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, { 342,  731}, 
{  47,  508}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  47,  508}, 
{   0,    0}, {  47,  508}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  47,  508}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  47,   59}, {  47,  508}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  47,  508}, 
{   0,    0}, {   0,    0}, {  47,  508}, {  47,  508}, {  47,  508}, 
{  47,  508}, {  47,  508}, {   0,    0}, {   0,    0}, {  47,  508}, 
{  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, 
{  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, 
{  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, {  47,  508}, 
{  47,  508}, {  47,  508}, {  47,  508}, {  73,  713}, {  73,  713}, 
{   0,    0}, {  73,  713}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  73,  713}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  73,  748}, {  73,  713}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  73,  713}, 
{   0,    0}, {   0,    0}, {  73,  713}, {  73,  713}, {  73,  713}, 
{  73,  713}, {  73,  713}, {   0,    0}, {   0,    0}, {  73,  713}, 
{  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, 
{  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, 
{  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, {  73,  713}, 
{  73,  713}, {  73,  713}, {  73,  713}, { 341,  730}, { 341,  730}, 
{   0,    0}, { 341,  730}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 341,  730}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 341,  765}, { 341,  730}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 341,  730}, 
{   0,    0}, {   0,    0}, { 341,  730}, { 341,  730}, { 341,  730}, 
{ 341,  730}, { 341,  730}, {   0,    0}, {   0,    0}, { 341,  730}, 
{ 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, 
{ 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, 
{ 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, { 341,  730}, 
{ 341,  730}, { 341,  730}, { 341,  730}, {  72,  712}, {  72,  712}, 
{   0,    0}, {  72,  712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  72,  712}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  72,  747}, {  72,  712}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  72,  712}, 
{   0,    0}, {   0,    0}, {  72,  712}, {  72,  712}, {  72,  712}, 
{  72,  712}, {  72,  712}, {   0,    0}, {   0,    0}, {  72,  712}, 
{  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, 
{  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, 
{  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, {  72,  712}, 
{  72,  712}, {  72,  712}, {  72,  712}, {  66,  707}, {  66,  707}, 
{   0,    0}, {  66,  707}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  66,  707}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  66,  742}, {  66,  707}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  66,  707}, 
{   0,    0}, {   0,    0}, {  66,  707}, {  66,  707}, {  66,  707}, 
{  66,  707}, {  66,  707}, {   0,    0}, {   0,    0}, {  66,  707}, 
{  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, 
{  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, 
{  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, {  66,  707}, 
{  66,  707}, {  66,  707}, {  66,  707}, { 205,  726}, { 205,  726}, 
{   0,    0}, { 205,  726}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 205,  726}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 205,  761}, { 205,  726}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 205,  726}, 
{   0,    0}, {   0,    0}, { 205,  726}, { 205,  726}, { 205,  726}, 
{ 205,  726}, { 205,  726}, {   0,    0}, {   0,    0}, { 205,  726}, 
{ 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, 
{ 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, 
{ 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, { 205,  726}, 
{ 205,  726}, { 205,  726}, { 205,  726}, {  63,  704}, {  63,  704}, 
{   0,    0}, {  63,  704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  63,  704}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  63,  739}, {  63,  704}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  63,  704}, 
{   0,    0}, {   0,    0}, {  63,  704}, {  63,  704}, {  63,  704}, 
{  63,  704}, {  63,  704}, {   0,    0}, {   0,    0}, {  63,  704}, 
{  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, 
{  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, 
{  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, {  63,  704}, 
{  63,  704}, {  63,  704}, {  63,  704}, { 249,  721}, { 249,  721}, 
{   0,    0}, { 249,  721}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 249,  721}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 249,  756}, { 249,  721}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 249,  721}, 
{   0,    0}, {   0,    0}, { 249,  721}, { 249,  721}, { 249,  721}, 
{ 249,  721}, { 249,  721}, {   0,    0}, {   0,    0}, { 249,  721}, 
{ 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, 
{ 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, 
{ 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, { 249,  721}, 
{ 249,  721}, { 249,  721}, { 249,  721}, { 250,  725}, { 250,  725}, 
{   0,    0}, { 250,  725}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 250,  725}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 250,  760}, { 250,  725}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 250,  725}, 
{   0,    0}, {   0,    0}, { 250,  725}, { 250,  725}, { 250,  725}, 
{ 250,  725}, { 250,  725}, {   0,    0}, {   0,    0}, { 250,  725}, 
{ 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, 
{ 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, 
{ 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, { 250,  725}, 
{ 250,  725}, { 250,  725}, { 250,  725}, {  65,  706}, {  65,  706}, 
{   0,    0}, {  65,  706}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  65,  706}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  65,  741}, {  65,  706}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  65,  706}, 
{   0,    0}, {   0,    0}, {  65,  706}, {  65,  706}, {  65,  706}, 
{  65,  706}, {  65,  706}, {   0,    0}, {   0,    0}, {  65,  706}, 
{  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, 
{  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, 
{  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, {  65,  706}, 
{  65,  706}, {  65,  706}, {  65,  706}, { 251,  719}, { 251,  719}, 
{   0,    0}, { 251,  719}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 251,  719}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 251,  754}, { 251,  719}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 251,  719}, 
{   0,    0}, {   0,    0}, { 251,  719}, { 251,  719}, { 251,  719}, 
{ 251,  719}, { 251,  719}, {   0,    0}, {   0,    0}, { 251,  719}, 
{ 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, 
{ 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, 
{ 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, { 251,  719}, 
{ 251,  719}, { 251,  719}, { 251,  719}, { 252,  723}, { 252,  723}, 
{   0,    0}, { 252,  723}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 252,  723}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 252,  758}, { 252,  723}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 252,  723}, 
{   0,    0}, {   0,    0}, { 252,  723}, { 252,  723}, { 252,  723}, 
{ 252,  723}, { 252,  723}, {   0,    0}, {   0,    0}, { 252,  723}, 
{ 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, 
{ 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, 
{ 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, { 252,  723}, 
{ 252,  723}, { 252,  723}, { 252,  723}, {  64,  705}, {  64,  705}, 
{   0,    0}, {  64,  705}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  64,  705}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  64,  740}, {  64,  705}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  64,  705}, 
{   0,    0}, {   0,    0}, {  64,  705}, {  64,  705}, {  64,  705}, 
{  64,  705}, {  64,  705}, {   0,    0}, {   0,    0}, {  64,  705}, 
{  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, 
{  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, 
{  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, {  64,  705}, 
{  64,  705}, {  64,  705}, {  64,  705}, { 204,  722}, { 204,  722}, 
{   0,    0}, { 204,  722}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 204,  722}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 204,  757}, { 204,  722}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 204,  722}, 
{   0,    0}, {   0,    0}, { 204,  722}, { 204,  722}, { 204,  722}, 
{ 204,  722}, { 204,  722}, {   0,    0}, {   0,    0}, { 204,  722}, 
{ 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, 
{ 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, 
{ 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, { 204,  722}, 
{ 204,  722}, { 204,  722}, { 204,  722}, {  71,  711}, {  71,  711}, 
{   0,    0}, {  71,  711}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  71,  711}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  71,  746}, {  71,  711}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {  71,  711}, 
{   0,    0}, {   0,    0}, {  71,  711}, {  71,  711}, {  71,  711}, 
{  71,  711}, {  71,  711}, {   0,    0}, {   0,    0}, {  71,  711}, 
{  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, 
{  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, 
{  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, {  71,  711}, 
{  71,  711}, {  71,  711}, {  71,  711}, { 339,  715}, { 339,  767}, 
{   0,    0}, { 339,  715}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 339,  767}, { 339,  738}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 339,  750}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 339,  767}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 339,  715}, { 339,  715}, 
{ 339,  715}, { 339,  715}, {   0,    0}, {   0,    0}, { 339,  715}, 
{ 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, 
{ 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, 
{ 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, { 339,  715}, 
{ 339,  715}, { 339,  715}, { 380,  728}, { 380,  771}, {   0,    0}, 
{ 380,  728}, {   0,    0}, {   0,    0}, {   0,    0}, { 380,  771}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 380,  763}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 380,  771}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 380,  728}, { 380,  728}, { 380,  728}, 
{ 380,  728}, {   0,    0}, {   0,    0}, { 380,  728}, { 380,  728}, 
{ 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, 
{ 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, 
{ 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, { 380,  728}, 
{ 380,  728}, { 322,  727}, { 322,  770}, {   0,    0}, { 322,  727}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 322,  770}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 322,  762}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 322,  770}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, 
{   0,    0}, {   0,    0}, { 322,  727}, { 322,  727}, { 322,  727}, 
{ 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, 
{ 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, 
{ 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, { 322,  727}, 
{ 365,  729}, { 365,  772}, {   0,    0}, { 365,  729}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 365,  772}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 365,  764}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 365,  772}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, {   0,    0}, 
{   0,    0}, { 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, 
{ 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, 
{ 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, 
{ 365,  729}, { 365,  729}, { 365,  729}, { 365,  729}, { 336,  797}, 
{   0,    0}, { 336,  797}, {   0,    0}, { 126,  214}, { 126,  784}, 
{ 336,  797}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 126,  784}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 336,  797}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 126,  784}, {   0,    0}, {   0,    0}, { 336,  797}, 
{   0,    0}, {   0,    0}, { 336,  797}, {   0,    0}, { 126,  784}, 
{ 336,  797}, { 336,  797}, { 126,  784}, {   0,    0}, { 336,  797}, 
{ 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, 
{ 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, 
{ 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, { 336,  797}, 
{ 336,  797}, { 336,  797}, { 336,  797}, { 261,  799}, {   0,    0}, 
{ 261,  799}, {   0,    0}, { 126,  784}, { 288,  718}, { 261,  799}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 288,  718}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 261,  799}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 288,  718}, {   0,    0}, {   0,    0}, { 261,  799}, {   0,    0}, 
{   0,    0}, { 261,  799}, { 288,  333}, { 288,  718}, { 261,  799}, 
{ 261,  799}, { 288,  718}, {   0,    0}, { 261,  799}, { 261,  799}, 
{ 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, 
{ 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, 
{ 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, { 261,  799}, 
{ 261,  799}, { 261,  799}, { 334,  795}, {   0,    0}, { 334,  795}, 
{   0,    0}, { 288,  718}, { 294,  717}, { 334,  795}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 294,  717}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 334,  795}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 294,  717}, 
{   0,    0}, {   0,    0}, { 334,  795}, {   0,    0}, {   0,    0}, 
{ 334,  795}, { 294,  337}, { 294,  717}, { 334,  795}, { 334,  795}, 
{ 294,  717}, {   0,    0}, { 334,  795}, { 334,  795}, { 334,  795}, 
{ 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, 
{ 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, 
{ 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, { 334,  795}, 
{ 334,  795}, { 258,  798}, {   0,    0}, { 258,  798}, {   0,    0}, 
{ 294,  717}, { 335,  727}, { 258,  798}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 335,  727}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 258,  798}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 335,  727}, {   0,    0}, 
{   0,    0}, { 258,  798}, {   0,    0}, {   0,    0}, { 258,  798}, 
{   0,    0}, { 335,  727}, { 258,  798}, { 258,  798}, { 335,  727}, 
{   0,    0}, { 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, 
{ 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, 
{ 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, 
{ 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, { 258,  798}, 
{ 262,  800}, {   0,    0}, { 262,  800}, {   0,    0}, { 335,  727}, 
{ 127,  819}, { 262,  800}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 127,  819}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 262,  800}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 127,  819}, {   0,    0}, {   0,    0}, 
{ 262,  800}, {   0,    0}, {   0,    0}, { 262,  800}, {   0,    0}, 
{ 127,  819}, { 262,  800}, { 262,  800}, { 127,  819}, {   0,    0}, 
{ 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, 
{ 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, 
{ 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, 
{ 262,  800}, { 262,  800}, { 262,  800}, { 262,  800}, { 295,  796}, 
{   0,    0}, { 295,  796}, {   0,    0}, { 127,  819}, { 384,  728}, 
{ 295,  796}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 384,  728}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 295,  796}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 384,  728}, {   0,    0}, {   0,    0}, { 295,  796}, 
{   0,    0}, {   0,    0}, { 295,  796}, {   0,    0}, { 384,  728}, 
{ 295,  796}, { 295,  796}, { 384,  728}, {   0,    0}, { 295,  796}, 
{ 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, 
{ 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, 
{ 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, { 295,  796}, 
{ 295,  796}, { 295,  796}, { 295,  796}, { 289,  794}, {  83,  535}, 
{ 289,  794}, {   0,    0}, { 384,  728}, { 361,  715}, { 289,  794}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 361,  715}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 289,  794}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 361,  715}, {   0,    0}, {   0,    0}, { 289,  794}, {   0,    0}, 
{   0,    0}, { 289,  794}, {   0,    0}, { 361,  715}, { 289,  794}, 
{ 289,  794}, { 361,  715}, {   0,    0}, { 289,  794}, { 289,  794}, 
{ 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, 
{ 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, 
{ 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, { 289,  794}, 
{ 289,  794}, { 289,  794}, { 169,  675}, {  13,   17}, { 169,  787}, 
{   0,    0}, { 361,  715}, {   0,    0}, { 169,  675}, {   0,    0}, 
{   0,    0}, {  83,  535}, {  83,  535}, {  83,  535}, {  83,  535}, 
{  83,  535}, {  83,  535}, {  83,  535}, {  83,  535}, {  83,  535}, 
{  83,  535}, {   0,    0}, {  83,  535}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 169,  675}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, 
{   0,    0}, {   0,    0}, { 169,  787}, { 169,  787}, { 169,  787}, 
{ 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, 
{ 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, 
{ 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, { 169,  787}, 
{ 168,  674}, {  84,  537}, { 168,  786}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 168,  674}, {   0,    0}, {   0,    0}, {   0,    0}, 
{  13,  426}, {  13,  427}, {  13,  428}, {  13,  429}, {  13,  430}, 
{  13,  431}, {  13,  432}, {  13,  433}, {  13,  434}, {  13,  435}, 
{   0,    0}, {  13,   18}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 168,  674}, {   0,    0}, {   0,    0}, {   0,    0}, { 168,  786}, 
{ 168,  786}, { 168,  786}, { 168,  786}, {   0,    0}, {   0,    0}, 
{ 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, 
{ 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, 
{ 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, { 168,  786}, 
{ 168,  786}, { 168,  786}, { 168,  786}, { 140,  673}, {   0,    0}, 
{ 140,  789}, {   0,    0}, {   0,    0}, {   0,    0}, { 140,  673}, 
{   0,    0}, {   0,    0}, {   0,    0}, {  84,  537}, {  84,  537}, 
{  84,  537}, {  84,  537}, {  84,  537}, {  84,  537}, {  84,  537}, 
{  84,  537}, {  84,  537}, {  84,  537}, {   0,    0}, {  84,  537}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 140,  673}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 140,  789}, { 140,  789}, { 140,  789}, 
{ 140,  789}, {   0,    0}, {   0,    0}, { 140,  789}, { 140,  789}, 
{ 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, 
{ 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, 
{ 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, { 140,  789}, 
{ 140,  789}, { 139,  672}, {   0,    0}, { 139,  788}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 139,  672}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 139,  672}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, {   0,    0}, 
{   0,    0}, { 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, 
{ 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, 
{ 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, 
{ 139,  788}, { 139,  788}, { 139,  788}, { 139,  788}, { 280,  713}, 
{   0,    0}, {   0,    0}, { 280,  713}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 280,  748}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 280,  327}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 280,  713}, 
{ 280,  713}, { 280,  713}, { 280,  713}, {   0,    0}, {   0,    0}, 
{ 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, 
{ 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, 
{ 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, { 280,  713}, 
{ 280,  713}, { 280,  713}, { 280,  713}, {  38,  551}, {   0,    0}, 
{ 101,  165}, {   0,    0}, {   0,    0}, { 101,  784}, {   0,    0}, 
{   0,    0}, {   0,    0}, {  38,  551}, {   0,    0}, {   0,    0}, 
{  38,  551}, {   0,    0}, {   0,    0}, {   0,    0}, {  38,  551}, 
{  38,  551}, {  38,  551}, {  38,  551}, {  38,  551}, {  38,  551}, 
{  38,  551}, {  38,  551}, {  38,  551}, {   0,    0}, {   0,    0}, 
{  38,  551}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, {   0,    0}, 
{  38,  551}, { 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, 
{ 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, 
{ 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, 
{ 101,  784}, { 101,  784}, { 101,  784}, { 101,  784}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 260,  802}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 260,  802}, {  38,  551}, {  38,  551}, {  38,  551}, 
{  38,  551}, {  38,  551}, {  38,  551}, {  38,  551}, {  38,  551}, 
{  38,  551}, {  38,  551}, { 260,  802}, {  38,  551}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 260,  802}, {   0,    0}, {   0,    0}, { 260,  802}, {   0,    0}, 
{   0,    0}, { 260,  802}, { 260,  802}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 260,  802}, { 260,  802}, { 260,  802}, 
{ 260,  802}, { 260,  802}, { 260,  802}, { 260,  802}, { 260,  802}, 
{ 260,  802}, { 260,  802}, { 260,  802}, { 260,  802}, { 260,  802}, 
{ 260,  802}, { 260,  802}, { 260,  802}, { 260,  802}, { 259,  801}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 357,  818}, 
{ 259,  801}, {   0,    0}, {   0,    0}, {   0,    0}, {   1,  522}, 
{ 357,  818}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 259,  801}, {   1,    2}, {   0,    0}, {   1,  522}, 
{   1,  522}, { 357,  818}, {   1,  522}, {   0,    0}, { 259,  801}, 
{   0,    0}, { 103,  167}, { 259,  801}, {   0,    0}, { 357,  818}, 
{ 259,  801}, { 259,  801}, { 357,  818}, { 357,  479}, { 357,  480}, 
{   0,    0}, { 259,  801}, { 259,  801}, { 259,  801}, { 259,  801}, 
{ 259,  801}, { 259,  801}, { 259,  801}, { 259,  801}, { 259,  801}, 
{ 259,  801}, { 259,  801}, { 259,  801}, { 259,  801}, { 259,  801}, 
{ 259,  801}, { 259,  801}, { 259,  801}, { 103,  168}, { 103,  169}, 
{ 103,  170}, { 103,  171}, { 357,  818}, {   0,    0}, { 103,  172}, 
{ 103,  173}, { 103,  174}, { 103,  175}, { 103,  176}, { 103,  177}, 
{ 103,  178}, { 103,  179}, { 103,  180}, { 103,  181}, { 103,  182}, 
{ 103,  183}, { 103,  184}, { 103,  185}, { 103,  186}, { 103,  187}, 
{ 103,  188}, { 103,  189}, { 263,  803}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 263,  803}, {   1,  522}, 
{   1,  522}, {   1,  522}, {   1,  522}, {   1,  522}, {   1,  522}, 
{   1,  522}, {   1,  522}, {   1,  522}, {   1,  522}, { 263,  803}, 
{   1,  522}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 263,  803}, {   0,    0}, {   0,    0}, 
{ 263,  803}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 263,  803}, 
{ 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, 
{ 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, 
{ 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, { 263,  803}, 
{ 263,  803}, { 264,  804}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 276,  816}, { 264,  804}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 276,  816}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 264,  804}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 276,  816}, {   0,    0}, 
{   0,    0}, { 264,  804}, {   0,    0}, {   0,    0}, { 264,  804}, 
{   0,    0}, { 276,  816}, {   0,    0}, {   0,    0}, { 276,  816}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 264,  804}, { 264,  804}, 
{ 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, 
{ 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, 
{ 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, { 264,  804}, 
{ 265,  805}, { 276,  816}, { 276,  816}, { 276,  816}, { 276,  816}, 
{   0,    0}, { 265,  805}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 265,  805}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 265,  805}, {   0,    0}, {   0,    0}, { 265,  805}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 265,  805}, { 265,  805}, { 265,  805}, 
{ 265,  805}, { 265,  805}, { 265,  805}, { 265,  805}, { 265,  805}, 
{ 265,  805}, { 265,  805}, { 265,  805}, { 265,  805}, { 265,  805}, 
{ 265,  805}, { 265,  805}, { 265,  805}, { 265,  805}, {  52,  533}, 
{   0,    0}, { 291,   63}, { 291,   64}, { 291,   65}, { 291,   66}, 
{ 291,   67}, { 291,   68}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  52,  533}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {  52,  533}, {  52,  533}, {  52,  533}, {  52,  533}, 
{  52,  533}, {  52,  533}, {  52,  533}, {  52,  533}, {   0,    0}, 
{   0,    0}, {  52,  533}, { 291,  119}, {   0,    0}, { 291,   70}, 
{ 291,   71}, { 291,   72}, { 291,   73}, { 291,   74}, {   0,    0}, 
{ 291,  120}, { 291,  134}, {   0,    0}, { 291,  122}, { 291,  123}, 
{ 291,   79}, { 291,   80}, { 291,   81}, { 291,   82}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   6,  519}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   6,  519}, {   6,    8}, {   0,    0}, 
{   6,  519}, { 267,  807}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 267,  807}, {  52,  533}, {  52,  533}, 
{  52,  533}, {  52,  533}, {  52,  533}, {  52,  533}, {  52,  533}, 
{  52,  533}, {  52,  533}, {  52,  533}, { 267,  807}, {  52,  533}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 267,  807}, {   0,    0}, {   0,    0}, { 267,  807}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 267,  807}, { 267,  807}, { 267,  807}, { 267,  807}, 
{ 267,  807}, { 267,  807}, { 267,  807}, { 267,  807}, { 267,  807}, 
{ 267,  807}, { 267,  807}, { 267,  807}, { 267,  807}, { 267,  807}, 
{ 266,  806}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 266,  806}, {   6,  519}, {   6,  519}, {   6,  519}, 
{   6,  519}, {   6,  519}, {   6,  519}, {   6,  519}, {   6,  519}, 
{   6,  519}, {   6,  519}, { 266,  806}, {   6,  519}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 266,  806}, {   0,    0}, {   0,    0}, { 266,  806}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 266,  806}, { 266,  806}, { 266,  806}, { 266,  806}, { 266,  806}, 
{ 266,  806}, { 266,  806}, { 266,  806}, { 266,  806}, { 266,  806}, 
{ 266,  806}, { 266,  806}, { 266,  806}, { 266,  806}, { 269,  809}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, { 383,  729}, 
{ 269,  809}, {   0,    0}, {   0,    0}, {   0,    0}, { 277,  817}, 
{ 383,  729}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 277,  817}, { 269,  809}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 383,  729}, {   0,    0}, {   0,    0}, { 269,  809}, 
{   0,    0}, { 277,  817}, { 269,  809}, {   0,    0}, { 383,  729}, 
{   0,    0}, {   0,    0}, { 383,  729}, {   0,    0}, { 277,  817}, 
{   0,    0}, {   0,    0}, { 277,  817}, {   0,    0}, { 269,  809}, 
{ 269,  809}, { 269,  809}, { 269,  809}, { 269,  809}, { 269,  809}, 
{ 269,  809}, { 269,  809}, { 269,  809}, { 269,  809}, { 269,  809}, 
{ 269,  809}, { 269,  809}, { 269,  809}, { 268,  808}, {   0,    0}, 
{ 329,   62}, {   0,    0}, { 383,  729}, {   0,    0}, { 268,  808}, 
{ 277,  817}, { 277,  817}, { 277,  817}, { 271,  811}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 329,   11}, {   0,    0}, { 271,  811}, 
{ 268,  808}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 268,  808}, {   0,    0}, 
{ 271,  811}, { 268,  808}, { 329,   69}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 271,  811}, {   0,    0}, 
{   0,    0}, { 271,  811}, { 329,  449}, { 268,  808}, { 268,  808}, 
{ 268,  808}, { 268,  808}, { 268,  808}, { 268,  808}, { 268,  808}, 
{ 268,  808}, { 268,  808}, { 268,  808}, { 268,  808}, { 268,  808}, 
{ 268,  808}, { 268,  808}, {   4,  517}, { 271,  811}, { 271,  811}, 
{ 271,  811}, { 271,  811}, { 271,  811}, { 271,  811}, { 271,  811}, 
{ 271,  811}, { 271,  811}, {   4,  517}, {   4,  517}, {   0,    0}, 
{   4,  517}, {   0,    0}, { 272,  812}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 272,  812}, { 329,  426}, 
{ 329,  427}, { 329,  428}, { 329,  429}, { 329,  430}, { 329,   83}, 
{ 329,  432}, { 329,   84}, { 329,  434}, { 329,  435}, { 272,  812}, 
{ 329,   12}, {   0,    0}, { 398,  349}, {   0,    0}, {   0,    0}, 
{ 329,   87}, {   0,    0}, { 272,  812}, {   0,    0}, {   0,    0}, 
{ 272,  812}, { 398,  460}, {   0,    0}, { 329,   93}, {   0,    0}, 
{ 329,   94}, {   0,    0}, {   0,    0}, { 398,   51}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 272,  812}, { 272,  812}, { 272,  812}, 
{ 272,  812}, { 272,  812}, { 272,  812}, { 272,  812}, { 272,  812}, 
{ 272,  812}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   4,  517}, {   4,  517}, {   4,  517}, 
{   4,  517}, {   4,  517}, {   4,  517}, {   4,  517}, {   4,  517}, 
{   4,  517}, {   4,  517}, {   0,    0}, {   4,  517}, {   0,    0}, 
{   0,    0}, { 273,  813}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 273,  813}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 273,  813}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 273,  813}, {   0,    0}, { 398,  145}, { 273,  813}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 398,   85}, {   0,    0}, 
{   0,    0}, { 398,   86}, { 398,  350}, { 398,   88}, { 398,   89}, 
{ 398,   90}, { 398,   91}, { 398,   92}, {   0,    0}, {   0,    0}, 
{ 398,  351}, {   0,    0}, { 398,  352}, { 273,  813}, { 273,  813}, 
{ 273,  813}, { 273,  813}, { 273,  813}, { 273,  813}, { 273,  813}, 
{ 274,  814}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{ 275,  815}, { 274,  814}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, { 275,  815}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 274,  814}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, { 275,  815}, {   0,    0}, {   0,    0}, 
{ 274,  814}, {   0,    0}, {   0,    0}, { 274,  814}, {   0,    0}, 
{ 275,  815}, {   0,    0}, {   0,    0}, { 275,  815}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, { 274,  814}, { 274,  814}, 
{ 274,  814}, { 274,  814}, { 274,  814}, { 274,  814}, { 275,  815}, 
{ 275,  815}, { 275,  815}, { 275,  815}, { 275,  815}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytNComb	yyNComb		[yyNTableMax - yyLastTerminal] = {
{   0,    0}, {   0,    0}, { 329,   95}, { 329,   96}, { 329,  496}, 
{ 329,  506}, {  48,   60}, {   0,    0}, { 329,   46}, { 329,   97}, 
{  43,   44}, {   1,    3}, { 319,  149}, {  40,   44}, { 106,  591}, 
{   0,    0}, { 329,   13}, { 329,  528}, { 329,  637}, { 211,  154}, 
{   1,  494}, {  48,  529}, {  26,  610}, {   1,    4}, {  43,   53}, 
{  43,  528}, {  43,  556}, {  40,   48}, {  40,  528}, {  40,  617}, 
{ 188,  135}, {  56,  109}, {  56,  564}, { 329,   98}, {  43,  552}, 
{   0,    0}, { 188,  136}, {  43,  554}, { 279,  509}, { 279,  592}, 
{  40,  615}, {  56,  111}, { 279,   30}, { 181,  270}, { 181,  810}, 
{ 181,  506}, {  43,  555}, {  43,   54}, { 181,   46}, { 181,   47}, 
{  40,   49}, { 193,  282}, { 329,  638}, {  74,  124}, { 106,  192}, 
{  43,  558}, {  43,  559}, {  43,   55}, {  31,  548}, {  74,  125}, 
{  43,  557}, { 360,  627}, { 285,  626}, {  40,  618}, { 285,  330}, 
{ 285,  331}, { 285,  624}, { 329,  656}, { 193,  283}, { 329,  635}, 
{  40,  614}, {  26,   33}, { 329,  639}, { 329,  100}, {  31,   37}, 
{  43,  593}, { 319,  697}, { 236,   95}, { 236,   96}, { 236,  496}, 
{ 236,  506}, { 374,  646}, { 374,  652}, { 236,   46}, { 236,   97}, 
{   9,  520}, {   9,   13}, {   9,  528}, {   9,  526}, { 218,  732}, 
{ 319,  101}, {   0,    0}, { 319,  102}, { 319,  677}, { 319,  823}, 
{ 319,  676}, { 319,  824}, { 211,  126}, { 390,  696}, { 211,  102}, 
{ 360,  621}, { 211,  823}, { 319,  103}, { 211,  824}, { 319,  671}, 
{ 319,  104}, { 390,  396}, { 390,  395}, { 236,  691}, { 211,  127}, 
{ 211,  253}, { 130,  218}, { 211,  129}, { 130,  219}, { 188,  137}, 
{ 401,  656}, { 188,  138}, { 220,  735}, { 401,  650}, { 218,  299}, 
{ 401,  645}, { 401,  100}, {   9,  527}, { 343,  368}, { 343,  682}, 
{ 343,  369}, { 188,  277}, { 157,  241}, { 157,  242}, { 157,  700}, 
{ 368,  683}, { 368,  387}, { 368,  685}, { 328,  359}, { 328,  255}, 
{ 143,  232}, { 328,  782}, { 398,  647}, { 398,  402}, { 398,  354}, 
{ 194,  284}, { 194,  116}, { 194,  585}, { 131,  220}, { 131,  775}, 
{ 131,  221}, { 220,  776}, { 220,  300}, { 236,  696}, { 120,  130}, 
{  74,  128}, { 120,  131}, { 120,  206}, {  54,  576}, { 236,  312}, 
{ 300,  631}, {  42,  510}, { 236,  246}, {  42,  549}, { 300,  736}, 
{  42,   30}, { 244,  141}, { 244,   45}, { 244,  496}, { 317,  641}, 
{ 317,  353}, { 397,  649}, { 397,  654}, { 404,  648}, { 404,  653}, 
{ 407,  650}, { 407,  655}, {  51,   99}, {  51,  634}, { 369,  388}, 
{ 230,  307}, { 230,  573}, { 112,  199}, { 203,  114}, {  54,  581}, 
{ 318,  355}, { 195,  565}, { 195,  110}, {   6,    9}, { 369,  686}, 
{   6,  518}, { 320,  247}, { 112,  200}, {  90,  155}, { 235,  311}, 
{ 234,  310}, { 320,  574}, { 376,  392}, { 375,  391}, { 366,  386}, 
{ 158,  243}, { 227,  304}, { 229,  306}, { 313,  347}, { 217,  296}, 
{ 318,  693}, { 216,  820}, { 142,  231}, { 379,  394}, { 346,  371}, 
{ 197,  620}, { 323,  783}, { 151,  239}, { 314,  643}, { 344,  649}, 
{ 166,  257}, { 399,  403}, {  91,  156}, { 190,  821}, { 189,  278}, 
{  61,  115}, {  53,  105}, { 287,  332}, { 164,  254}, { 315,  348}, 
{ 203,  586}, { 113,  201}, { 400,  644}, { 324,  356}, { 117,  640}, 
{ 184,  273}, { 183,  272}, { 187,  276}, { 182,  271}, { 123,  789}, 
{ 209,  289}, {  82,  793}, {  81,  792}, {  80,  791}, {  79,  790}, 
{  78,  140}, {  77,  139}, { 177,  266}, { 180,  269}, { 167,  258}, 
{ 378,  642}, { 326,  357}, { 122,  788}, { 186,  275}, { 179,  268}, 
{ 170,  259}, { 185,  274}, { 171,  260}, { 172,  261}, { 291,  334}, 
{ 293,  336}, { 215,  295}, { 173,  262}, { 174,  263}, { 176,  265}, 
{ 175,  264}, { 389,  648}, { 178,  267}, { 233,  646}, {  86,  150}, 
{ 327,  358}, {  75,  132}, { 345,  370}, { 159,  245}, { 333,  362}, 
{ 222,  301}, { 212,  292}, { 302,  340}, { 377,  393}, { 337,  363}, 
{ 405,  406}, { 297,  338}, { 107,  595}, { 160,  248}, { 373,  702}, 
{ 282,  329}, {  24,  545}, {  27,  604}, {  39,  611}, { 191,  596}, 
{ 316,  703}, { 299,  733}, {  32,  540}, {  41,  541}, {  36,  605}, 
{   4,    6}, { 114,  202}, { 124,  210}, {  13,  529}, { 125,  213}, 
{ 361,  364}, { 105,  191}, {  21,   27}, {  44,   56}, { 141,  230}, 
{ 247,  575}, { 219,  734}, {  95,  160}, {  16,   21}, { 331,  625}, 
{  22,   28}, {  52,  594}, { 104,  190}, {  15,   20}, {  25,   32}, 
{  34,   40}, {  20,   25}, {   8,   10}, {  38,   43}, {  60,  112}, 
{ 339,  364}, { 111,  577}, {  23,   29}, { 242,  701}, {   2,    5}, 
{  29,   36}, { 201,  286}, {  28,   35}, {  35,   41}, {  55,  107}, 
{ 129,  216}, { 221,  737}, { 110,  566}, { 200,  578}, { 241,  670}, 
{ 308,  668}, { 309,  659}, { 364,  385}, {  92,  157}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
{   0,    0}, {   0,    0}, {   0,    0}, {   0,    0}, 
};
static	yytComb *	yyTBasePtr	[yyLastReadState + 1] = { 0,
& yyTComb [3014], & yyTComb [ 540], & yyTComb [   0], & yyTComb [3527], 
& yyTComb [  41], & yyTComb [3297], & yyTComb [ 493], & yyTComb [ 529], 
& yyTComb [ 799], & yyTComb [   2], & yyTComb [ 486], & yyTComb [ 537], 
& yyTComb [2605], & yyTComb [ 629], & yyTComb [ 113], & yyTComb [ 278], 
& yyTComb [ 236], & yyTComb [ 228], & yyTComb [ 579], & yyTComb [ 132], 
& yyTComb [  96], & yyTComb [  46], & yyTComb [ 273], & yyTComb [  25], 
& yyTComb [ 157], & yyTComb [  15], & yyTComb [ 627], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [ 393], & yyTComb [ 111], & yyTComb [ 631], 
& yyTComb [ 134], & yyTComb [ 135], & yyTComb [   0], & yyTComb [ 635], 
& yyTComb [ 254], & yyTComb [2882], & yyTComb [   8], & yyTComb [ 442], 
& yyTComb [ 634], & yyTComb [ 480], & yyTComb [ 856], & yyTComb [ 161], 
& yyTComb [ 545], & yyTComb [ 851], & yyTComb [1189], & yyTComb [ 423], 
& yyTComb [ 586], & yyTComb [ 198], & yyTComb [ 787], & yyTComb [3243], 
& yyTComb [  34], & yyTComb [ 602], & yyTComb [   0], & yyTComb [ 541], 
& yyTComb [ 671], & yyTComb [ 672], & yyTComb [ 666], & yyTComb [ 531], 
& yyTComb [ 773], & yyTComb [ 296], & yyTComb [1519], & yyTComb [1849], 
& yyTComb [1684], & yyTComb [1409], & yyTComb [ 733], & yyTComb [1016], 
& yyTComb [  84], & yyTComb [ 906], & yyTComb [1959], & yyTComb [1354], 
& yyTComb [1244], & yyTComb [   0], & yyTComb [ 146], & yyTComb [ 577], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [2551], & yyTComb [2658], 
& yyTComb [ 687], & yyTComb [   0], & yyTComb [ 678], & yyTComb [ 151], 
& yyTComb [ 283], & yyTComb [ 582], & yyTComb [   0], & yyTComb [ 636], 
& yyTComb [ 713], & yyTComb [ 714], & yyTComb [ 322], & yyTComb [ 573], 
& yyTComb [ 336], & yyTComb [ 601], & yyTComb [ 793], & yyTComb [ 624], 
& yyTComb [2876], & yyTComb [ 641], & yyTComb [3019], & yyTComb [ 275], 
& yyTComb [ 452], & yyTComb [ 481], & yyTComb [ 625], & yyTComb [ 396], 
& yyTComb [ 590], & yyTComb [   0], & yyTComb [ 410], & yyTComb [ 335], 
& yyTComb [   0], & yyTComb [   7], & yyTComb [ 767], & yyTComb [ 460], 
& yyTComb [   0], & yyTComb [ 356], & yyTComb [ 548], & yyTComb [   0], 
& yyTComb [ 570], & yyTComb [   0], & yyTComb [   0], & yyTComb [ 451], 
& yyTComb [ 196], & yyTComb [2234], & yyTComb [2450], & yyTComb [ 783], 
& yyTComb [   0], & yyTComb [ 640], & yyTComb [ 719], & yyTComb [ 712], 
& yyTComb [  14], & yyTComb [ 576], & yyTComb [ 574], & yyTComb [ 458], 
& yyTComb [ 572], & yyTComb [ 633], & yyTComb [2766], & yyTComb [2713], 
& yyTComb [ 226], & yyTComb [   0], & yyTComb [   0], & yyTComb [ 793], 
& yyTComb [ 718], & yyTComb [ 674], & yyTComb [ 710], & yyTComb [ 716], 
& yyTComb [ 326], & yyTComb [ 773], & yyTComb [   0], & yyTComb [ 627], 
& yyTComb [ 588], & yyTComb [ 463], & yyTComb [ 630], & yyTComb [ 626], 
& yyTComb [ 331], & yyTComb [   0], & yyTComb [ 395], & yyTComb [ 416], 
& yyTComb [ 402], & yyTComb [  84], & yyTComb [ 670], & yyTComb [   0], 
& yyTComb [ 470], & yyTComb [   0], & yyTComb [   0], & yyTComb [2660], 
& yyTComb [2607], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [ 629], & yyTComb [  99], 
& yyTComb [ 171], & yyTComb [   0], & yyTComb [ 356], & yyTComb [ 664], 
& yyTComb [ 628], & yyTComb [ 676], & yyTComb [ 275], & yyTComb [   0], 
& yyTComb [ 543], & yyTComb [ 335], & yyTComb [ 782], & yyTComb [1904], 
& yyTComb [1464], & yyTComb [ 688], & yyTComb [ 534], & yyTComb [ 665], 
& yyTComb [   0], & yyTComb [ 381], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [ 771], & yyTComb [ 530], & yyTComb [ 623], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [ 385], & yyTComb [ 788], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [ 260], & yyTComb [ 961], 
& yyTComb [ 678], & yyTComb [ 532], & yyTComb [   0], & yyTComb [ 549], 
& yyTComb [   0], & yyTComb [ 544], & yyTComb [ 732], & yyTComb [ 731], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [ 783], & yyTComb [ 677], & yyTComb [ 726], & yyTComb [ 676], 
& yyTComb [ 767], & yyTComb [   0], & yyTComb [ 719], & yyTComb [ 679], 
& yyTComb [ 583], & yyTComb [ 340], & yyTComb [ 462], & yyTComb [ 595], 
& yyTComb [1574], & yyTComb [1629], & yyTComb [1739], & yyTComb [1794], 
& yyTComb [ 674], & yyTComb [ 768], & yyTComb [ 170], & yyTComb [ 392], 
& yyTComb [ 679], & yyTComb [2391], & yyTComb [2994], & yyTComb [2940], 
& yyTComb [2283], & yyTComb [2445], & yyTComb [3072], & yyTComb [3126], 
& yyTComb [3180], & yyTComb [3355], & yyTComb [3301], & yyTComb [3463], 
& yyTComb [3409], & yyTComb [ 634], & yyTComb [3473], & yyTComb [3532], 
& yyTComb [3606], & yyTComb [3660], & yyTComb [3665], & yyTComb [3131], 
& yyTComb [3419], & yyTComb [ 795], & yyTComb [ 535], & yyTComb [2820], 
& yyTComb [ 717], & yyTComb [ 815], & yyTComb [ 794], & yyTComb [ 784], 
& yyTComb [   0], & yyTComb [ 508], & yyTComb [   0], & yyTComb [2288], 
& yyTComb [2553], & yyTComb [ 675], & yyTComb [3244], & yyTComb [ 782], 
& yyTComb [   0], & yyTComb [2342], & yyTComb [2499], & yyTComb [ 662], 
& yyTComb [ 518], & yyTComb [ 673], & yyTComb [ 631], & yyTComb [   0], 
& yyTComb [ 741], & yyTComb [   0], & yyTComb [ 513], & yyTComb [ 669], 
& yyTComb [ 568], & yyTComb [ 668], & yyTComb [ 348], & yyTComb [ 639], 
& yyTComb [ 638], & yyTComb [ 723], & yyTComb [ 721], & yyTComb [ 587], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [ 630], 
& yyTComb [   0], & yyTComb [ 577], & yyTComb [   9], & yyTComb [  13], 
& yyTComb [ 209], & yyTComb [2122], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [ 776], 
& yyTComb [3474], & yyTComb [ 816], & yyTComb [ 511], & yyTComb [ 778], 
& yyTComb [   0], & yyTComb [2337], & yyTComb [2396], & yyTComb [2229], 
& yyTComb [   0], & yyTComb [ 784], & yyTComb [2014], & yyTComb [ 734], 
& yyTComb [1299], & yyTComb [1126], & yyTComb [ 783], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [ 722], & yyTComb [ 720], 
& yyTComb [ 194], & yyTComb [ 675], & yyTComb [ 680], & yyTComb [ 681], 
& yyTComb [ 768], & yyTComb [ 101], & yyTComb [ 581], & yyTComb [ 727], 
& yyTComb [2999], & yyTComb [ 728], & yyTComb [ 729], & yyTComb [ 817], 
& yyTComb [2558], & yyTComb [ 730], & yyTComb [ 735], & yyTComb [ 637], 
& yyTComb [2176], & yyTComb [   0], & yyTComb [ 797], & yyTComb [ 325], 
& yyTComb [ 137], & yyTComb [ 769], & yyTComb [ 578], & yyTComb [ 580], 
& yyTComb [ 626], & yyTComb [   0], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [   0], & yyTComb [2068], 
& yyTComb [ 591], & yyTComb [ 594], & yyTComb [3414], & yyTComb [2504], 
& yyTComb [1071], & yyTComb [ 796], & yyTComb [ 786], & yyTComb [ 265], 
& yyTComb [   0], & yyTComb [   0], & yyTComb [ 781], & yyTComb [ 775], 
& yyTComb [ 628], & yyTComb [ 770], & yyTComb [ 467], & yyTComb [ 766], 
& yyTComb [   0], & yyTComb [3561], & yyTComb [   0], & yyTComb [   0], 
& yyTComb [ 197], & yyTComb [ 770], & yyTComb [ 589], & yyTComb [   0], 
& yyTComb [ 772], & yyTComb [ 742], & yyTComb [   0], 
};
static	yytNComb *	yyNBasePtr	[yyLastReadState + 1] = { 0,
& yyNComb [ -95], & yyNComb [ 200], & yyNComb [-105], & yyNComb [ 169], 
& yyNComb [-105], & yyNComb [  66], & yyNComb [-105], & yyNComb [ 193], 
& yyNComb [ -35], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 166], & yyNComb [-105], & yyNComb [ 179], & yyNComb [ 140], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [ 180], 
& yyNComb [ 132], & yyNComb [ 175], & yyNComb [ 153], & yyNComb [ 165], 
& yyNComb [ 177], & yyNComb [ -90], & yyNComb [ 115], & yyNComb [ 191], 
& yyNComb [ 155], & yyNComb [-105], & yyNComb [ -54], & yyNComb [ 153], 
& yyNComb [-105], & yyNComb [ 142], & yyNComb [ 191], & yyNComb [ 122], 
& yyNComb [-105], & yyNComb [ 178], & yyNComb [ 166], & yyNComb [ -94], 
& yyNComb [ 154], & yyNComb [  46], & yyNComb [ -97], & yyNComb [ 161], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-101], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [  -1], & yyNComb [ 129], 
& yyNComb [  62], & yyNComb [   7], & yyNComb [ 169], & yyNComb [-104], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [ 176], 
& yyNComb [  68], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [ -55], & yyNComb [ 150], & yyNComb [-105], 
& yyNComb [  32], & yyNComb [  31], & yyNComb [  30], & yyNComb [  29], 
& yyNComb [  28], & yyNComb [  27], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [  84], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [ -17], & yyNComb [   7], & yyNComb [ 156], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [ 158], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [  94], 
& yyNComb [ 141], & yyNComb [ -97], & yyNComb [ 117], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [ 157], & yyNComb [ 161], & yyNComb [  42], 
& yyNComb [ 114], & yyNComb [ 149], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  49], & yyNComb [-105], & yyNComb [-105], & yyNComb [  41], 
& yyNComb [-105], & yyNComb [  38], & yyNComb [  25], & yyNComb [  86], 
& yyNComb [  88], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 112], & yyNComb [ -88], & yyNComb [ -56], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 154], & yyNComb [  -3], & yyNComb [ -70], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [   2], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ -61], & yyNComb [ -10], & yyNComb [  79], & yyNComb [ 132], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [  16], 
& yyNComb [-105], & yyNComb [   5], & yyNComb [  35], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [  41], & yyNComb [  43], & yyNComb [  44], 
& yyNComb [  48], & yyNComb [  49], & yyNComb [  51], & yyNComb [  50], 
& yyNComb [  33], & yyNComb [  53], & yyNComb [  40], & yyNComb [  34], 
& yyNComb [ -65], & yyNComb [  24], & yyNComb [  22], & yyNComb [  21], 
& yyNComb [  42], & yyNComb [  39], & yyNComb [  23], & yyNComb [ -78], 
& yyNComb [   9], & yyNComb [   8], & yyNComb [ 124], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [  -7], & yyNComb [  45], & yyNComb [-105], 
& yyNComb [  39], & yyNComb [-105], & yyNComb [-105], & yyNComb [ 173], 
& yyNComb [ 179], & yyNComb [-105], & yyNComb [  71], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  26], & yyNComb [-105], & yyNComb [ -95], & yyNComb [  64], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [  47], & yyNComb [  -4], 
& yyNComb [  -6], & yyNComb [ -82], & yyNComb [ 129], & yyNComb [ -54], 
& yyNComb [ 154], & yyNComb [  63], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [  -9], & yyNComb [-105], 
& yyNComb [  -8], & yyNComb [  34], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  83], & yyNComb [ -15], & yyNComb [ -16], & yyNComb [ -30], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 133], & yyNComb [ 123], & yyNComb [-105], & yyNComb [  54], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [ 130], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [ -72], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [ 102], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-104], & yyNComb [-105], & yyNComb [  15], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [  45], & yyNComb [-105], 
& yyNComb [  46], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 103], & yyNComb [-105], & yyNComb [ 110], & yyNComb [ -12], 
& yyNComb [-105], & yyNComb [  65], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [ 153], 
& yyNComb [ 147], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  -7], & yyNComb [  33], & yyNComb [  70], & yyNComb [ 108], 
& yyNComb [ -11], & yyNComb [  -5], & yyNComb [-102], & yyNComb [  49], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [   1], & yyNComb [  21], 
& yyNComb [-105], & yyNComb [  37], & yyNComb [  58], & yyNComb [ -69], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [ 129], & yyNComb [-105], 
& yyNComb [  62], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  67], & yyNComb [-105], & yyNComb [ 117], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [ -57], & yyNComb [  34], 
& yyNComb [  86], & yyNComb [  19], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  97], & yyNComb [-105], & yyNComb [-105], & yyNComb [ 198], 
& yyNComb [-105], & yyNComb [ -11], & yyNComb [-105], & yyNComb [ -51], 
& yyNComb [   1], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [ 102], & yyNComb [ -94], & yyNComb [ -12], & yyNComb [ -13], 
& yyNComb [  84], & yyNComb [  65], & yyNComb [  17], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  81], & yyNComb [ -80], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [-105], & yyNComb [-105], & yyNComb [-105], & yyNComb [-105], 
& yyNComb [  -9], & yyNComb [ -38], & yyNComb [  26], & yyNComb [  47], 
& yyNComb [ -57], & yyNComb [-105], & yyNComb [-105], & yyNComb [  -7], 
& yyNComb [  84], & yyNComb [-105], & yyNComb [  -5], 
};
#ifdef YYTDefault
static	unsigned short	yyTDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     0,     0,     0,     0,     0,    13,     0, 
    0,     0,     0,     7,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,    20,    21,     5, 
    0,     0,     0,     0,    25,     0,     0,     0,     0,    48, 
    0,     0,    40,     0,   270,     0,     5,    53,     0,     0, 
  329,    92,    13,    92,   105,     0,     0,     0,     0,     0, 
  203,   349,     0,     0,     0,     0,     0,     0,   119,     0, 
    0,     0,     0,   237,   106,     0,   291,   291,   291,   291, 
  291,   291,    48,   145,     0,   401,     0,     0,     0,   318, 
  237,     0,     0,     0,     0,    45,   149,     0,   329,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,   199,   247, 
  105,     0,   244,     0,     0,     0,   401,     0,     0,    75, 
    0,   291,   291,   135,   149,   101,   357,     0,   104,     0, 
  130,     0,     0,     0,     0,     0,   126,     0,     0,     0, 
    0,   237,   237,   349,     0,     0,     0,     0,     0,     0, 
  237,     0,     0,   125,     0,     0,     0,   237,   319,     0, 
    0,     7,   298,   328,     0,   237,   291,     0,     0,   291, 
  291,   291,   291,   291,   291,   291,   291,   291,   291,   291, 
   75,   291,   291,   291,   291,   291,   291,   291,   237,   237, 
    0,     0,   329,    61,     0,     0,   237,     0,     0,   111, 
    0,     0,   244,     0,     0,     0,   133,     0,   291,     0, 
  298,   328,   210,     0,   291,   237,   298,   220,     0,   219, 
  219,   328,     0,     0,     0,   133,   237,     0,   237,     0, 
    0,     0,   401,   237,   237,   159,   291,     0,     0,     0, 
    0,   157,     0,    75,     0,     0,     0,   307,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,   268,   268,   268, 
  268,   268,   268,   268,   268,   268,   268,   271,   268,     0, 
  273,   271,   274,   275,   276,   277,   357,     0,     0,     0, 
   76,   329,   329,     0,   297,   202,   328,   223,   268,     0, 
  244,     0,   291,   256,   268,     0,   360,   237,   220,   299, 
    0,   328,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   237,   401,   203,     0,   398,   237,   237,     0, 
  341,     0,   237,   328,   342,   291,   328,   237,   401,     0, 
    0,     0,   328,   268,   322,   268,   328,     0,     0,     0, 
    0,     0,   368,   401,   398,   318,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,   103,     0,     0,   197, 
  339,     0,     0,     0,     0,   237,     0,     0,   329,     0, 
    0,     0,     0,   398,   237,   237,   159,   401,   405,     0, 
    0,     0,   365,   380,     0,     0,   369,   369,   401,   405, 
    0,     0,     0,     0,     0,     0,   398,   319,   318,   401, 
  398,     0,     0,   398,   319,     0,   398, 
};
#endif
#ifdef YYNDefault
static	unsigned short	yyNDefault	[yyLastReadState + 1] = { 0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,   279,     0,    42,     0,     0,     0,     0, 
   42,     0,     0,     0,     0,     0,     0,     0,    42,    48, 
    0,     0,    53,     0,     0,     0,     0,   113,     0,     0, 
  329,     0,    48,     0,     0,   195,     0,     0,     0,     0, 
  194,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,   143,   120,     0,   188,   188,   188,   188, 
  188,   188,   244,     0,     0,   401,     0,     0,     0,   143, 
  143,     0,     0,     0,     0,     0,     0,     0,   329,     0, 
    0,     0,     0,     0,     0,   279,     0,     0,     0,     0, 
    0,    56,   244,     0,     0,     0,   401,     0,     0,   279, 
    0,   188,   188,     0,     0,     0,     0,     0,     0,   131, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,   143,   217,     0,     0,     0,     0,     0,     0,     0, 
  143,     0,     0,     0,     0,     0,     0,   143,   236,   230, 
    0,     0,   211,   328,     0,   143,   188,     0,     0,   188, 
  188,   188,   188,   188,   188,   188,   188,   188,   188,   188, 
    0,   188,   188,   188,   188,   188,   188,     0,   143,   143, 
    0,     0,    51,   203,     0,     0,   360,     0,     0,     0, 
    0,     0,   113,     0,     0,     0,     0,     0,   188,     0, 
  188,   328,     0,     0,   188,   143,   211,   220,     0,   300, 
    0,   328,     0,     0,     0,     0,   143,     0,   143,   320, 
    0,     0,   401,   143,   143,   319,   211,     0,     0,     0, 
    0,     0,     0,   181,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,    51,   329,     0,   360,     0,   328,     0,     0,     0, 
  188,     0,   188,     0,     0,     0,   285,   217,   300,     0, 
    0,   328,     0,     0,     0,     0,     0,     0,     0,     0, 
    0,     0,   143,   401,   203,     0,   374,   143,   188,     0, 
    0,     0,   143,   328,     0,   188,   328,   143,   319,     0, 
    0,     0,   328,     0,     0,     0,   328,     0,     0,     0, 
    0,     0,   368,   401,   398,   318,     0,     0,     0,     0, 
    0,     0,     0,     0,     0,     0,     0,     0,     0,   143, 
    0,     0,     0,     0,     0,   143,     0,     0,    51,     0, 
    0,     0,     0,   398,   143,   143,   236,   401,   390,     0, 
    0,     0,     0,     0,     0,     0,   369,   329,   401,   319, 
    0,     0,     0,     0,     0,     0,   374,   401,   318,   401, 
  319,     0,     0,   374,   390,     0,   374, 
};
#endif
#if ! defined NO_RECOVER | defined YYDEC_TABLE
static	unsigned char	yyLength	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
    2,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     3,     3, 
    3,     1,     3,     3,     0,     2,     0,     2,     2,     0, 
    3,     3,     5,     1,     1,     1,     2,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     1,     6,     7,     0, 
    0,     0,     2,     0,     2,     1,     3,     3,     0,     2, 
    2,     1,     1,     1,     1,     1,     1,     0,     0,     4, 
    5,     1,     3,     2,     1,     3,     3,     4,     0,     0, 
    1,     3,     2,     2,     4,     5,     4,     3,     1,     1, 
    0,     1,     1,     3,     2,     3,     0,     2,     1,     3, 
    1,     2,     3,     4,     4,     2,     3,     3,     4,     5, 
    5,     5,     6,     0,     0,     0,     2,     1,     3,     3, 
    0,     2,     1,     2,     1,     1,     0,     2,     1,     3, 
    3,     1,     2,     1,     3,     0,     1,     0,     1,     2, 
    3,     1,     2,     2,     1,     1,     1,     3,     5,     7, 
    5,     9,     1,     3,     5,     7,     5,     9,     1,     3, 
    7,     5,     9,     1,     1,     2,     5,     7,     2,     3, 
    2,     3,     2,     3,     3,     5,     3,     4,     1,     2, 
    2,     2,     2,     1,     1,     2,     3,     3,     4,     1, 
    2,     2,     1,     2,     3,     2,     0,     1,     1,     0, 
    1,     0,     1,     1,     3,     0,     1,     1,     2,     5, 
    2,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     3,     5,     7,     3,     3,     3,     3,     3,     3, 
    3,     3,     3,     3,     4,     6,     6,     4,     4,     4, 
    5,     4,     4,     5,     4,     0,     1,     1,     1,     1, 
    1,     1,     1,     1,     1,     1,     3,     5,     7,     3, 
    3,     3,     3,     3,     3,     3,     3,     3,     3,     4, 
    6,     6,     4,     4,     5,     3,     3,     4,     6,     6, 
    4,     4,     1,     2,     3,     2,     3,     0,     1,     1, 
    3,     1,     1,     2,     2,     2,     2,     2,     2,     2, 
    2,     4,     5,     4,     5,     3,     3,     3,     3,     3, 
    3,     3,     3,     3,     3,     3,     3,     3,     3,     3, 
    3,     3,     3,     3,     3,     5,     1,     3,     3,     1, 
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1, 
    1,     1,     1,     1, 
};
static	yytNonterminal	yyLeftHandSide	[yyLastReduceState - yyFirstReduceState
							+ 1] = {
yyNT0_intern,
yyNTGoal,
yyNTType,
yyNTType,
yyNTType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTPrimitiveType,
yyNTReferenceType,
yyNTReferenceType,
yyNTClassOrInterfaceType,
yyNTClassType,
yyNTInterfaceType,
yyNTArrayType,
yyNTArrayType,
yyNTArrayType,
yyNTName,
yyNTName,
yyNTCompilationUnit,
yyNTImportDeclarations,
yyNTImportDeclarations,
yyNTTypeDeclarations,
yyNTTypeDeclarations,
yyNTTypeDeclarations,
yyNTPackageDeclaration,
yyNTPackageDeclaration,
yyNTImportDeclaration,
yyNTImportDeclaration,
yyNTTypeDeclaration,
yyNTTypeDeclaration,
yyNTModifiers,
yyNTModifiers,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTModifier,
yyNTClassDeclaration,
yyNTClassDeclaration,
yyNTxx_ClassDeclaration_1_3,
yyNTxx_ClassDeclaration_2_4,
yyNTSuper,
yyNTSuper,
yyNTInterfaces,
yyNTInterfaces,
yyNTInterfaceTypeList,
yyNTInterfaceTypeList,
yyNTClassBody,
yyNTClassBodyDeclarations,
yyNTClassBodyDeclarations,
yyNTClassBodyDeclarations,
yyNTClassBodyDeclaration,
yyNTClassBodyDeclaration,
yyNTClassBodyDeclaration,
yyNTClassBodyDeclaration,
yyNTClassBodyDeclaration,
yyNTClassBodyDeclaration,
yyNTget_type_1,
yyNTget_type_2,
yyNTFieldDeclaration,
yyNTFieldDeclaration,
yyNTVariableDeclarators,
yyNTVariableDeclarators,
yyNTVariableDeclarator,
yyNTVariableDeclaratorId,
yyNTVariableDeclaratorId,
yyNTLocalVariableDeclaration,
yyNTLocalVariableDeclaration,
yyNTxx_LocalVariableDeclaration_1_2,
yyNTxx_LocalVariableDeclaration_2_3,
yyNTLocalVariableDeclarators,
yyNTLocalVariableDeclarators,
yyNTLocalVariableDeclarator,
yyNTMethodDeclaration,
yyNTMethodHeader,
yyNTMethodHeader,
yyNTMethodDeclarator,
yyNTMethodDeclarator,
yyNTMethodBody,
yyNTMethodBody,
yyNTFormalParameterListOpt,
yyNTFormalParameterListOpt,
yyNTFormalParameterList,
yyNTFormalParameterList,
yyNTFormalParameter,
yyNTFormalParameter,
yyNTThrowsOpt,
yyNTThrowsOpt,
yyNTClassTypeList,
yyNTClassTypeList,
yyNTClassInitializer,
yyNTClassInitializer,
yyNTConstructorDeclaration,
yyNTConstructorDeclaration,
yyNTConstructorDeclarator,
yyNTConstructorBody,
yyNTConstructorBody,
yyNTConstructorBody,
yyNTConstructorBody,
yyNTExplicitConstructorInvocation,
yyNTExplicitConstructorInvocation,
yyNTInterfaceDeclaration,
yyNTInterfaceDeclaration,
yyNTxx_InterfaceDeclaration_1_3,
yyNTxx_InterfaceDeclaration_2_4,
yyNTExtendsInterfacesOpt,
yyNTExtendsInterfacesOpt,
yyNTExtendsInterfaces,
yyNTExtendsInterfaces,
yyNTInterfaceBody,
yyNTInterfaceMemberDeclarations,
yyNTInterfaceMemberDeclarations,
yyNTInterfaceMemberDeclaration,
yyNTInterfaceMemberDeclaration,
yyNTInterfaceMemberDeclaration,
yyNTInterfaceMemberDeclaration,
yyNTVariableInitializerOpt,
yyNTVariableInitializerOpt,
yyNTVariableInitializer,
yyNTVariableInitializer,
yyNTArrayInitializer,
yyNTVariableInitializersOpt,
yyNTVariableInitializersOpt,
yyNTVariableInitializers,
yyNTVariableInitializers,
yyNTCommaOpt,
yyNTCommaOpt,
yyNTArrayInitializerOpt,
yyNTArrayInitializerOpt,
yyNTBlock,
yyNTBlock,
yyNTBlockStatements,
yyNTBlockStatements,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTBlockStatement,
yyNTStatement,
yyNTStatement,
yyNTStatement,
yyNTStatement,
yyNTStatement,
yyNTStatement,
yyNTStatementNoShortIf,
yyNTStatementNoShortIf,
yyNTStatementNoShortIf,
yyNTStatementNoShortIf,
yyNTStatementNoShortIf,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementWithoutTrailingSubstatement,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTStatementExpression,
yyNTSwitchBlock,
yyNTSwitchBlock,
yyNTSwitchBlock,
yyNTSwitchBlock,
yyNTSwitchBlockStatementGroups,
yyNTSwitchBlockStatementGroups,
yyNTSwitchBlockStatementGroup,
yyNTSwitchLabels,
yyNTSwitchLabels,
yyNTSwitchLabel,
yyNTSwitchLabel,
yyNTForInit,
yyNTForInit,
yyNTForInit,
yyNTExpressionOpt,
yyNTExpressionOpt,
yyNTForUpdate,
yyNTForUpdate,
yyNTStatementExpressionList,
yyNTStatementExpressionList,
yyNTCatchesOpt,
yyNTCatchesOpt,
yyNTCatches,
yyNTCatches,
yyNTCatchClause,
yyNTFinally,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTPrimary,
yyNTxx_Primary_13_6,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTPrimaryNoNewArray,
yyNTClassInstanceCreationExpression,
yyNTFieldAccess,
yyNTFieldAccess,
yyNTMethodInvocation,
yyNTMethodInvocation,
yyNTMethodInvocation,
yyNTArrayAccess,
yyNTArrayAccess,
yyNTDimExprs,
yyNTDimExprs,
yyNTDimExpr,
yyNTDims,
yyNTDims,
yyNTArgumentListOpt,
yyNTArgumentListOpt,
yyNTArgumentList,
yyNTArgumentList,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTcExpression,
yyNTExpression,
yyNTExpression,
yyNTAssignment,
yyNTLeftHandSide,
yyNTLeftHandSide,
yyNTLeftHandSide,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
yyNTAssignmentOperator,
};
#endif
#ifndef NO_RECOVER
static	yySymbolRange	yyContinuation	[yyLastReadState + 1] = { 0,
    0,     1,     0,     0,    10,     0,     1,     1,     0,    10, 
    1,     1,    13,    12,    14,    14,     1,     1,    10,    15, 
   17,    14,    14,     1,    17,     1,    17,    15,    17,    10, 
    1,    17,    17,     1,    17,    17,    17,     1,     1,    38, 
   17,     1,    38,     1,     1,     1,     1,    18,    10,     1, 
   38,    17,    18,    10,    17,     1,    27,    27,    27,     1, 
   35,     1,     9,     9,     9,     9,     9,     9,     9,     9, 
    9,     9,     9,     2,     1,     9,     2,     2,     2,     2, 
    2,     2,    18,    34,    34,    10,    34,    10,    10,    10, 
    2,    17,    34,    34,     1,     9,     9,    10,    38,    10, 
   12,    26,    39,    29,    17,     1,    17,    10,    10,    10, 
   10,     1,    18,     1,    35,    35,    10,    13,     9,     1, 
    9,     2,     2,    35,    26,    10,    10,    35,    29,    26, 
   26,    34,     1,     9,     9,     9,    10,    26,    10,    10, 
    1,     2,     2,     9,    34,    34,    34,    34,     9,    91, 
    2,    10,    10,     9,    10,    10,    98,     2,    10,     1, 
   13,    13,     2,    35,     1,     2,     2,    10,    12,     2, 
    2,     2,     2,     2,     2,     2,     2,     2,     2,     2, 
    1,     2,     2,     2,     2,     2,     2,     2,     2,     2, 
   17,    10,    38,    35,     1,    27,     2,    27,    10,    10, 
    1,    16,    18,     9,     9,    34,     1,    27,     2,    35, 
    2,    35,    35,     1,     9,     2,     2,     9,    17,     9, 
   17,    35,     9,     9,     9,     1,     2,     1,     2,     1, 
   35,    35,    10,     2,     2,    10,     2,    34,    35,    34, 
   98,     1,    35,    18,    10,    10,    10,    10,     9,     9, 
    9,     9,    27,    35,    35,     9,    27,    10,    10,    10, 
   10,    10,    10,    10,    10,    10,    10,    10,    10,    26, 
   10,    10,    10,    10,    10,    10,    10,    63,     1,     9, 
    9,    38,    38,    35,    38,    16,    35,     9,    10,    27, 
    2,    35,     2,     9,    10,    27,    38,    27,     9,     9, 
   35,    35,     9,    27,     9,    27,    10,    17,    17,    35, 
   35,    10,     2,    10,    18,    17,    10,    10,    39,     1, 
    9,     9,     2,    35,     9,     2,    35,    35,    38,    38, 
   38,    35,    35,    10,     9,    10,    35,    38,     9,    35, 
    9,     9,    38,    10,    10,    10,    35,    35,     9,    34, 
   34,    34,   100,     1,    10,    35,    10,    35,    35,     2, 
    9,    35,    35,    17,    26,     2,    63,    38,    38,   100, 
   10,    10,    17,    10,     2,     2,    10,    10,    35,    26, 
   10,    10,    26,    26,    26,    63,    38,    38,    10,    35, 
   35,    35,    10,    35,    35,    35,    10,    10,    10,    10, 
   10,   100,    10,    10,    35,    35,    10, 
};
static	unsigned short	yyCondition	[yyLastState - yyLastReduceState + 1] =
{ 0,
};
#endif
static	unsigned short	yyFinalToProd	[yyLastReadReduceState -
						yyFirstReadReduceState + 2] = {
  497,   498,   499,   500,   501,   502,   503,   504,   505,   511, 
  512,   513,   514,   515,   521,   523,   524,   525,   530,   531, 
  532,   533,   534,   535,   536,   537,   538,   539,   550,   553, 
  562,   563,   567,   568,   579,   580,   582,   597,   598,   599, 
  600,   601,   602,   603,   612,   616,   622,   623,   629,   632, 
  633,   636,   657,   658,   660,   661,   662,   663,   664,   665, 
  666,   667,   678,   679,   680,   681,   687,   688,   777,   778, 
  779,   786,   787,   825,   826,   827,   828,   829,   830,   831, 
  832,   833,   834,   835,   836, 
0
};
static	unsigned short	yyStartLine	[yyLastStopState - yyFirstReduceState
							+ 2] = { 0,
516,
};
#ifdef YYaccDefault

static	unsigned long *	yyDefaultLook	[yyLastReadState + 1] = { 0,
};
#endif

static	int	yyParse			ARGS ((yyStateRange yyStartSymbol,
				yySymbolRange yyToken, int yyLine));

#ifndef NO_RECOVER
static	yyStateRange yyNext		ARGS ((yyStateRange yyState,
				yySymbolRange yySymbol));
static	void	yyErrorRecovery		ARGS ((yySymbolRange * yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
static	void	yyComputeContinuation	ARGS ((yyStateRange * yyStack,
				short yyStackPtr, tSet * yyContinueSet));
static	rbool	yyIsContinuation	ARGS ((yySymbolRange yyTerminal,
				yyStateRange * yyStateStack, short yyStackPtr));
static	void	yyComputeRestartPoints	ARGS ((yyStateRange * yyStateStack,
				short yyStackPtr, tSet * yyRestartSet));
#endif

#if defined YYTrialParse | defined YYReParse | defined YYGetLook

#ifndef yyInitBufferSize
#define yyInitBufferSize 100
#endif
#ifndef TOKENOP
#define TOKENOP
#endif
#ifndef BEFORE_TRIAL
#define BEFORE_TRIAL
#endif
#ifndef AFTER_TRIAL
#define AFTER_TRIAL
#endif

typedef struct { yySymbolRange	yyToken;
		 tScanAttribute	yyAttribute;
#ifdef YYMemoParse
		 short		yyStart;
#endif
	       } yytBuffer;

static yytBuffer *	yyBuffer	;
static unsigned long	yyBufferSize	= yyInitBufferSize;
static long		yyBufferNext	= 1;
static long		yyBufferLast	= 1;
static rbool		yyBufferClear	= rtrue;
static unsigned short	yyParseLevel	= 0;

static void yyBufferSet
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyToken)
#else
   (yyToken) yySymbolRange yyToken;
#endif
{
   if (yyBufferNext == yyBufferLast) {
      if (yyBufferClear) yyBufferLast = 0;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken	= yyToken;
      yyBuffer [yyBufferLast].yyAttribute= Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart	= 0;
#endif
      yyBufferNext = yyBufferLast;
   }
}

static int yyGetToken ARGS ((void))
{
   register yySymbolRange yyToken;

   if (yyBufferNext < yyBufferLast) {
      yyToken = yyBuffer [++ yyBufferNext].yyToken;
      Attribute = yyBuffer [yyBufferNext].yyAttribute;
   } else {
      yyToken = GetToken ();
      if ((yytrial | yybuffer) & yyControl.yyMode) {
	 if (++ yyBufferLast >= (long) yyBufferSize) {
	    ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			     (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (0);
	       (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
		  yyBufferSize / 2, yyBufferSize); yyNl ();
	    }
#endif
	 }
	 yyBuffer [yyBufferLast].yyToken = yyToken;
	 yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
	 yyBuffer [yyBufferLast].yyStart = 0;
#endif
	 yyBufferNext = yyBufferLast;
      }
   }
   TOKENOP
   return yyToken;
}

#else
#define yyGetToken GetToken
#endif

#ifdef YYGetLook

static int yyGetLookahead
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken)
#else
   (yyk, yyToken) int yyk; yySymbolRange yyToken;
#endif
{
   if (yyk == 0) return yyToken;
   if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
   while (yyBufferNext + yyk > yyBufferLast) {
      if (yyBuffer [yyBufferLast].yyToken == EofToken) return EofToken;
      if (++ yyBufferLast >= (long) yyBufferSize) {
	 ExtendArray ((char * *) & yyBuffer, & yyBufferSize,
			  (unsigned long) sizeof (yytBuffer));
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (0);
	    (void) fprintf (yyTrace, "extend  token buffer from %ld to %ld",
	       yyBufferSize / 2, yyBufferSize); yyNl ();
	 }
#endif
      }
      yyBuffer [yyBufferLast].yyToken = GetToken ();
      yyBuffer [yyBufferLast].yyAttribute = Attribute;
#ifdef YYMemoParse
      yyBuffer [yyBufferLast].yyStart = 0;
#endif
   }
   Attribute = yyBuffer [yyBufferNext].yyAttribute;
   return yyBuffer [yyBufferNext + yyk].yyToken;
}

static void xxGetAttribute
#if defined __STDC__ | defined __cplusplus
   (int yyk, yySymbolRange yyToken, tScanAttribute * yyAttribute)
#else
   (yyk, yyToken, yyAttribute)
   int yyk; yySymbolRange yyToken; tScanAttribute * yyAttribute;
#endif
{
   if (yyk == 0) * yyAttribute = Attribute;
   else {
      (void) yyGetLookahead (yyk, yyToken);
      * yyAttribute =
	 yyBuffer [Min (yyBufferNext + yyk, yyBufferLast)].yyAttribute;
   }
}

#endif

#ifdef YYReParse

#define BufferOn(Actions, Messages) yyBufferOn (Actions, Messages, yyTerminal)
#define BufferPosition	yyBufferNext

static yytControl yyPrevControl;

static long yyBufferOn
#if defined __STDC__ | defined __cplusplus
   (rbool yyActions, rbool yyMessages, yySymbolRange yyToken)
#else
   (yyActions, yyMessages, yyToken)
   rbool yyActions, yyMessages; yySymbolRange yyToken;
#endif
{
   if (yyControl.yyMode == yystandard) {
      yyPrevControl		= yyControl;
      yyControl.yyMode		= yybuffer;
      yyControl.yyActions	= yyActions;
      yyControl.yyMessages	= yyMessages;
      yyBufferSet (yyToken);
      yyBufferClear		= rfalse;
   }
   return yyBufferNext;
}

static long BufferOff ARGS ((void))
{
   if (yyControl.yyMode == yybuffer) yyControl = yyPrevControl;
   return yyBufferNext;
}

static void BufferClear ARGS ((void))
{
   yyBufferClear = rtrue;
}

#endif

#ifdef YYDEBUG

static void yyNl ARGS ((void))
{ (void) putc ('\n', yyTrace); (void) fflush (yyTrace); }

static void yyPrintState
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState)
#else
   (yyState) yyStateRange yyState;
#endif
{
   (void) fprintf (yyTrace, "%4ld:", ++ yyCount);
   WritePosition  (yyTrace, Attribute.Position);
   (void) fprintf (yyTrace, ":%5d  %c  ", yyState,
      " ST-B---R" [yyControl.yyMode]);
#if defined YYTrialParse | defined YYReParse
   if (yyParseLevel > 0) {
      register int yyi = yyParseLevel;
      (void) fprintf (yyTrace, "%2d  ", yyi);
      do (void) fputs ("  ", yyTrace); while (-- yyi);
   } else
#endif
   (void) fputs ("    ", yyTrace);
}

static rbool yyPrintResult
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, int yyLine, rbool yyCondition)
#else
   (yyState, yyLine, yyCondition)
   yyStateRange	yyState;
   int		yyLine;
   rbool	yyCondition;
#endif
{
   if (Parser_Debug) {
      yyPrintState (yyState);
      (void) fprintf (yyTrace, "check   predicate in line %d, result = %d",
	 yyLine, yyCondition); yyNl ();
   }
   return yyCondition;
}

#else
#define yyPrintResult(State, Line, Condition) Condition
#endif

#if defined YYDEBUG | defined YYDEC_TABLE
#define yyGotoReduce(State, Rule)	{ yyState = State; goto yyReduce; }
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#else
#define yyGotoReduce(State, Rule)	goto Rule;
#define yyGotoRead(State)		{ yyState = State; goto yyRead; }
#endif

static unsigned long	yyStateStackSize	= yyInitStackSize;
static yyStateRange *	yyStateStack		;
static yyStateRange *	yyEndOfStack		;
static unsigned long	yyAttrStackSize 	= yyInitStackSize;
static tParsAttribute * yyAttributeStack	;
#if defined YYTrialParse | defined YYReParse
static yyStateRange *	yyStateStackPtr 	;
static tParsAttribute * yyAttrStackPtr		;
#endif
static yyStateRange *	yyIsContStackPtr	;
static unsigned long	yyIsContStackSize	= yyInitStackSize;
static yyStateRange *	yyCompResStackPtr	;
static unsigned long	yyCompResStackSize	= yyInitStackSize;

int Parser ARGS ((void))
   {
      return Parser2 (yyStartState);
   }

int Parser2
#if defined __STDC__ | defined __cplusplus
   (int yyStartSymbol)
#else
   (yyStartSymbol) int yyStartSymbol;
#endif
   {
      int		yyErrorCount;
#if defined YYDEBUG | defined YYDCRP
      yyTrace		= stdout;
#endif
      BeginParser ();
      MakeArray ((char * *) & yyStateStack, & yyStateStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
		     (unsigned long) sizeof (tParsAttribute));
      MakeArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
		     (unsigned long) sizeof (yyStateRange));
      MakeArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
		     (unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      MakeArray ((char * *) & yyBuffer, & yyBufferSize,
		     (unsigned long) sizeof (yytBuffer));
#endif
      yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#if defined YYTrialParse | defined YYReParse
      yyStateStackPtr	= yyStateStack;
      yyAttrStackPtr	= yyAttributeStack;
      yyBufferNext	= 1;
      yyBufferLast	= 1;
      yyParseLevel	= 0;
#endif
#ifdef YYDEBUG
      if (Parser_Debug) {
	 (void) fprintf (yyTrace,
      "  #|Position|State|Mod|Lev|Action |Terminal and Lookahead or Rule\n");
	 yyNl ();
      }
#endif
      yyControl.yyMode		= yystandard;
      yyControl.yyActions	= rtrue;
      yyControl.yyMessages	= rtrue;
      yyErrorCount = yyParse ((yyStateRange) yyStartSymbol,
	 (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
      ReleaseArray ((char * *) & yyStateStack, & yyStateStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			(unsigned long) sizeof (tParsAttribute));
      ReleaseArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			(unsigned long) sizeof (yyStateRange));
      ReleaseArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			(unsigned long) sizeof (yyStateRange));
#if defined YYTrialParse | defined YYReParse | defined YYGetLook
      ReleaseArray ((char * *) & yyBuffer, & yyBufferSize,
			(unsigned long) sizeof (yytBuffer));
#endif
      return yyErrorCount;
   }

#ifdef YYTrialParse

#ifdef YYMemoParse
#define MemoryClear(Position) yyBuffer [Position].yyStart = 0
#endif

static int yyTrialParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      int	yyErrorCount		;
      unsigned long yyPrevStateStackPtr	= yyStateStackPtr - yyStateStack;
      unsigned long yyPrevAttrStackPtr	= yyAttrStackPtr - yyAttributeStack;
      long	yyPrevBufferNext	;
      yytControl yyPrevControl		;

      BEFORE_TRIAL
#ifdef YYMemoParse
      if (yyBuffer [yyBufferNext].yyStart ==   yyStartSymbol) return 0;
      if (yyBuffer [yyBufferNext].yyStart == - yyStartSymbol) return 1;
#endif
      yyPrevControl		= yyControl;
      yyStateStackPtr		++;
      yyAttrStackPtr		++;
      yyParseLevel		++;
      if (yyControl.yyMode == yystandard) yyBufferSet (yyToken);
      yyPrevBufferNext		= yyBufferNext;
      yyControl.yyMode		= yytrial;
      yyControl.yyActions	= rfalse;
      yyControl.yyMessages	= rfalse;
      yyErrorCount		= yyParse (yyStartSymbol, yyToken, yyLine);
#ifdef YYMemoParse
      yyBuffer [yyPrevBufferNext].yyStart = yyErrorCount ?
					- yyStartSymbol : yyStartSymbol;
#endif
      yyStateStackPtr		= yyStateStack + yyPrevStateStackPtr;
      yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
      yyBufferNext		= yyPrevBufferNext;
      yyControl			= yyPrevControl;
      yyParseLevel		--;
      Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      AFTER_TRIAL
      return yyErrorCount;
   }

#endif

#ifdef YYReParse

static int ReParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, int yyFrom, int yyTo, rbool yyActions,
      rbool yyMessages)
#else
   (yyStartSymbol, yyFrom, yyTo, yyActions, yyMessages)
   yyStateRange	yyStartSymbol		;
   int		yyFrom, yyTo		;
   rbool	yyActions, yyMessages	;
#endif
   {
      int yyErrorCount = 1;

      if (1 <= yyFrom && yyFrom <= yyTo && yyTo <= yyBufferLast) {
	 unsigned long yyPrevStateStackPtr = yyStateStackPtr - yyStateStack;
	 unsigned long yyPrevAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	 long	yyPrevBufferNext	= yyBufferNext;
	 int	yyToToken		= yyBuffer [yyTo].yyToken;
	 yytControl yyPrevControl	;

	 yyPrevControl		= yyControl;
	 yyStateStackPtr	++;
	 yyAttrStackPtr		++;
	 yyParseLevel		++;
	 yyBufferNext		= yyFrom - 1;
	 yyBuffer [yyTo].yyToken= EofToken;
	 yyControl.yyMode	= yyreparse;
	 yyControl.yyActions	= yyActions;
	 yyControl.yyMessages	= yyMessages;
	 yyErrorCount		= yyParse (yyStartSymbol,
	    (yySymbolRange) yyGetToken (), (int) yyStartLine [yyStartSymbol]);
	 yyStateStackPtr	= yyStateStack + yyPrevStateStackPtr;
	 yyAttrStackPtr		= yyAttributeStack + yyPrevAttrStackPtr;
	 yyBufferNext		= yyPrevBufferNext;
	 yyControl		= yyPrevControl;
	 yyParseLevel		--;
	 yyBuffer [yyTo].yyToken= yyToToken;
	 Attribute		= yyBuffer [yyBufferNext].yyAttribute;
      } else {
      Message ("invalid call of ReParse", xxError, Attribute.Position);
      }
      return yyErrorCount;
   }

#endif

static int yyParse
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyStartSymbol, yySymbolRange yyToken, int yyLine)
#else
   (yyStartSymbol, yyToken, yyLine)
   yyStateRange		yyStartSymbol	;
   yySymbolRange	yyToken		;
   int			yyLine		;
#endif
   {
      register	yyStateRange	yyState		= yyStartSymbol;
      register	yySymbolRange	yyTerminal	= yyToken;
      register	rbool		yyIsRepairing	= rfalse;
		tParsAttribute	yySynAttribute	;   /* synthesized attribute */
		int		yyErrorCount	= 0;
#if ! (defined YYTrialParse | defined YYReParse)
      register	yyStateRange *	yyStateStackPtr	= yyStateStack;
      register	tParsAttribute *yyAttrStackPtr	= yyAttributeStack;
#endif
#ifdef YYDEBUG
		long		yyStartCount	= yyCount + 1;
		yySymbolRange	yyPrevTerminal	= yyToken;
#endif
#ifdef YYGetLook
		yySymbolRange	yy2;
#endif

/* line 364 "Parser.lrk" */



#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (yyStartSymbol);
	 (void) fprintf (yyTrace,
	    "parse   for predicate in line %d, lookahead: %s", yyLine,
	    Parser_TokenName [yyTerminal]); yyNl ();
      }
#endif

   yyParseLoop:
      for (;;) {
	 if (yyStateStackPtr >= yyEndOfStack) {
	    unsigned long yyyStateStackPtr = yyStateStackPtr - yyStateStack;
	    unsigned long yyyAttrStackPtr = yyAttrStackPtr - yyAttributeStack;
	    ExtendArray ((char * *) & yyStateStack, & yyStateStackSize,
			     (unsigned long) sizeof (yyStateRange));
	    ExtendArray ((char * *) & yyAttributeStack, & yyAttrStackSize,
			     (unsigned long) sizeof (tParsAttribute));
	    yyStateStackPtr	= yyStateStack + yyyStateStackPtr;
	    yyAttrStackPtr	= yyAttributeStack + yyyAttrStackPtr;
	    yyEndOfStack	= & yyStateStack [yyStateStackSize - 1];
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyState);
	       (void) fprintf (yyTrace, "extend  stack from %ld to %ld",
		  yyStateStackSize / 2, yyStateStackSize); yyNl ();
	    }
#endif
	 }
	 * yyStateStackPtr = yyState;

   yyTermTrans:
	 for (;;) { /* SPEC State = Next (State, Terminal); terminal transit */
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yyTerminal;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) {
	       yyState = yyTCombPtr->Next; break;
	    }
#ifdef YYTDefault
#ifdef YYaccDefault
	    if ((yylp = yyDefaultLook [yyState]) &&
	       (yylp [yyTerminal >> 5] >> (yyTerminal & 0x1f)) & 1) {
	       yyState = yyTDefault [yyState]; break;
	    }
#else
	    if ((yyState = yyTDefault [yyState]) != yyNoState) goto yyTermTrans;
#endif
#endif

							/* syntax error */
	    if (! yyIsRepairing) {			/* report and recover */
	       yySymbolRange yyyTerminal = (yySymbolRange) yyTerminal;

#ifdef YYTrialParse
	       if (yyControl.yyMode == yytrial) YYABORT;
#endif
	       MY_ERROR
#ifndef NO_RECOVER
	       yyErrorCount ++;
	       yyErrorRecovery (& yyyTerminal, yyStateStack,
				yyStateStackPtr - yyStateStack);
	       yyTerminal = yyyTerminal;
	       yyIsRepairing = rtrue;
#else
	       YYABORT;
#endif
	    }
#ifndef NO_RECOVER
	    yyState = * yyStateStackPtr;
	    for (;;) {
	       if (yyNext (yyState, (yySymbolRange) yyTerminal) == yyNoState) {
		  yySymbolRange		yyRepairToken;		/* repair */
		  tScanAttribute	yyRepairAttribute;

		  yyRepairToken = yyContinuation [yyState];
		  yyState = yyNext (yyState, yyRepairToken);
		  if (yyState > yyLastReduceState) {		/* dynamic ? */
		     yyState = yyCondition [yyState - yyLastReduceState];
		  }
		  if (yyState <= yyLastReadReduceState) {
						/* read or read reduce ? */
		     ErrorAttribute ((int) yyRepairToken,
					& yyRepairAttribute);
		     if (yyControl.yyMessages)
		        ErrorMessageI (xxTokenInserted, xxRepair,
				Attribute.Position, xxString,
				Parser_TokenName [yyRepairToken]);
#ifdef YYDEBUG
		     if (Parser_Debug) {
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "insert  %s",
				Parser_TokenName [yyRepairToken]); yyNl ();
			yyPrintState (* yyStateStackPtr);
			(void) fprintf (yyTrace, "shift   %s, lookahead: %s",
			   Parser_TokenName [yyRepairToken],
			   Parser_TokenName [yyTerminal]); yyNl ();
		     }
#endif
		     if (yyState >= yyFirstFinalState) { /* avoid second push */
			yyState =
			   yyFinalToProd [yyState - yyFirstReadReduceState];
		     }
		     yyGetAttribute (yyAttrStackPtr ++, yyRepairAttribute);
		     * ++ yyStateStackPtr = yyState;
		  }
		  if (yyState >= yyFirstFinalState) goto yyFinal;
							/* final state ? */
	       } else {
		  yyState = yyNext (yyState, (yySymbolRange) yyTerminal);
		  goto yyFinal;
	       }
	    }
#endif
	 }

   yyFinal:
	 if (yyState >= yyFirstFinalState) {		/* final state ? */
	    if (yyState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStateStackPtr ++;
	       yyGetAttribute (yyAttrStackPtr ++, Attribute);
	       yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  yyStateStackPtr [0] = yyStateStackPtr [-1];
		  yyPrintState (* yyStateStackPtr);
		  (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		     Parser_TokenName [yyPrevTerminal],
		     Parser_TokenName [yyTerminal]); yyNl ();
		  yyPrevTerminal = yyTerminal;
	       }
#endif
	       yyIsRepairing = rfalse;
	    }

	    for (;;) {
	       register yytNonterminal yyNonterminal;	/* left-hand side */

   yyReduce:
#ifdef YYDEBUG
	       if (Parser_Debug) {
		  if (yyState <= yyLastReadReduceState)	/* read reduce ? */
		     yyState = yyFinalToProd [yyState - yyFirstReadReduceState];
		  yyPrintState (* yyStateStackPtr);
		  if (yyState <= yyLastReduceState) {
		     (void) fprintf (yyTrace, "reduce  %s",
			yyRule [yyState - yyLastReadReduceState]); yyNl ();
		  } else {
		     (void) fprintf (yyTrace, "dynamic decision %d",
			yyState - yyLastReduceState); yyNl ();
		  }
	       }
#endif
#ifdef YYDEC_TABLE
	       if (yyLastStopState < yyState && yyState <= yyLastReduceState) {
		  register int yyd = yyLength [yyState - yyFirstReduceState];
		  yyStateStackPtr -= yyd;
		  yyAttrStackPtr  -= yyd;
		  yyNonterminal = yyLeftHandSide [yyState - yyFirstReduceState];
	       }
#endif
switch (yyState) {
case 493:
YYACCEPT;
case 494: yyDecrement (1) yySetNT (yyNTGoal) {
} break;
case 495: yyDecrement (1) yySetNT (yyNTType) {
/* line 517 "Parser.lrk" */
yyS.Type.tree = yyA [0].PrimitiveType.tree;

} break;
case 496: yyDecrement (1) yySetNT (yyNTType) {
/* line 519 "Parser.lrk" */
yyS.Type.tree = yyA [0].ReferenceType.tree;

} break;
case 497:
case 408: yyDecrement (1) yySetNT (yyNTType) {
/* line 521 "Parser.lrk" */
 yyS.Type.tree = msimple_type (t_void	);
 ;

} break;
case 498:
case 409: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 524 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_byte	);
 ;

} break;
case 499:
case 410: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 527 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_short	);
 ;

} break;
case 500:
case 411: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 530 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_int	);
 ;

} break;
case 501:
case 412: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 533 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_long	);
 ;

} break;
case 502:
case 413: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 536 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_char	);
 ;

} break;
case 503:
case 414: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 539 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_float	);
 ;

} break;
case 504:
case 415: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 542 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_double);
 ;

} break;
case 505:
case 416: yyDecrement (1) yySetNT (yyNTPrimitiveType) {
/* line 545 "Parser.lrk" */
 yyS.PrimitiveType.tree = msimple_type (t_boolean);
 ;

} break;
case 506: yyDecrement (1) yySetNT (yyNTReferenceType) {
/* line 548 "Parser.lrk" */
yyS.ReferenceType.tree = yyA [0].ClassOrInterfaceType.tree;

} break;
case 507: yyDecrement (1) yySetNT (yyNTReferenceType) {
/* line 550 "Parser.lrk" */
yyS.ReferenceType.tree = yyA [0].ArrayType.tree;

} break;
case 508: yyDecrement (1) yySetNT (yyNTClassOrInterfaceType) {
/* line 552 "Parser.lrk" */
yyS.ClassOrInterfaceType.epos = yyA [0].Name.epos;
yyS.ClassOrInterfaceType.bpos = yyA [0].Name.bpos;
 yyS.ClassOrInterfaceType.tree = mnamed_type (yyA [0].Name.tree);
 ;

} break;
case 509: yyDecrement (1) yySetNT (yyNTClassType) {
/* line 557 "Parser.lrk" */
yyS.ClassType.epos = yyA [0].ClassOrInterfaceType.epos;
yyS.ClassType.bpos = yyA [0].ClassOrInterfaceType.bpos;
yyS.ClassType.tree = yyA [0].ClassOrInterfaceType.tree;

} break;
case 510: yyDecrement (1) yySetNT (yyNTInterfaceType) {
/* line 561 "Parser.lrk" */
yyS.InterfaceType.tree = yyA [0].ClassOrInterfaceType.tree;

} break;
case 511:
case 417: yyDecrement (3) yySetNT (yyNTArrayType) {
/* line 563 "Parser.lrk" */
 yyS.ArrayType.tree = marray_type (yyA [0].PrimitiveType.tree);
 ;

} break;
case 512:
case 418: yyDecrement (3) yySetNT (yyNTArrayType) {
/* line 566 "Parser.lrk" */
 yyS.ArrayType.tree = marray_type (mnamed_type (yyA [0].Name.tree));
 ;

} break;
case 513:
case 419: yyDecrement (3) yySetNT (yyNTArrayType) {
/* line 569 "Parser.lrk" */
 yyS.ArrayType.tree = marray_type (yyA [0].ArrayType.tree);
 ;

} break;
case 514:
case 420: yyDecrement (1) yySetNT (yyNTName) {
/* line 572 "Parser.lrk" */
 yyS.Name.tree = mident (yyA [0].Scan.Position, yyA [0].Scan.Identifier.ident);

		  yyS.Name.bpos  =  yyA [0].Scan.Position;
 yyS.Name.epos.Line = yyS.Name.bpos.Line;
			yyS.Name.epos.Column = yyS.Name.bpos.Column +
			  LengthSt (GetStringRef (yyA [0].Scan.Identifier.ident)) - 1; ;
 ;

} break;
case 515:
case 421: yyDecrement (3) yySetNT (yyNTName) {
/* line 580 "Parser.lrk" */
 yyS.Name.tree = mqualification (yyA [2].Scan.Position, yyA [0].Name.tree, yyA [2].Scan.Identifier.ident);

		  yyS.Name.bpos  =  yyA [0].Name.bpos;
 yyS.Name.epos.Line = yyA [2].Scan.Position.Line;
			yyS.Name.epos.Column = yyA [2].Scan.Position.Column +
			  LengthSt (GetStringRef (yyA [2].Scan.Identifier.ident)) - 1; ;
 ;

} break;
case 516: yyDecrement (3) yySetNT (yyNTCompilationUnit) {
/* line 588 "Parser.lrk" */
 ;
{  TreeRoot = mcompilation_unit (yyA [0].PackageDeclaration.tree,
			ReverseTree (yyA [1].ImportDeclarations.tree),
			ReverseTree (yyA [2].TypeDeclarations.tree)); ; } ;

} break;
case 517: yySetNT (yyNTImportDeclarations) {
/* line 593 "Parser.lrk" */
 yyS.ImportDeclarations.tree = dnoimport;
 ;

} break;
case 518: yyDecrement (2) yySetNT (yyNTImportDeclarations) {
/* line 596 "Parser.lrk" */
 yyS.ImportDeclarations.tree = yyA [1].ImportDeclaration.tree;
			yyS.ImportDeclarations.tree->import.next = yyA [0].ImportDeclarations.tree; ;
 ;

} break;
case 519: yySetNT (yyNTTypeDeclarations) {
/* line 600 "Parser.lrk" */
 yyS.TypeDeclarations.tree = dnofield;
 ;

} break;
case 520: yyDecrement (2) yySetNT (yyNTTypeDeclarations) {
/* line 603 "Parser.lrk" */
 yyS.TypeDeclarations.tree = yyA [1].TypeDeclaration.tree;
			yyS.TypeDeclarations.tree->field.next = yyA [0].TypeDeclarations.tree; ;
 ;

} break;
case 521:
case 422: yyDecrement (2) yySetNT (yyNTTypeDeclarations) {
/* line 607 "Parser.lrk" */
yyS.TypeDeclarations.tree = yyA [0].TypeDeclarations.tree;

} break;
case 522: yySetNT (yyNTPackageDeclaration) {
/* line 609 "Parser.lrk" */
 yyS.PackageDeclaration.tree = dnoexpression;
 ;

} break;
case 523:
case 423: yyDecrement (3) yySetNT (yyNTPackageDeclaration) {
/* line 612 "Parser.lrk" */
yyS.PackageDeclaration.tree = yyA [1].Name.tree;

} break;
case 524:
case 424: yyDecrement (3) yySetNT (yyNTImportDeclaration) {
/* line 614 "Parser.lrk" */
 ;
{  if (hl >= 0) { to_files (yyA [1].Name.tree, current_string);
		   put_symbol (PAF_INCLUDE_DEF, NIL, current_string, current_file,
		      (int) yyA [1].Name.bpos.Line, (int) yyA [1].Name.bpos.Column - 1,
		      (int) yyA [1].Name.epos.Line, (int) yyA [1].Name.epos.Column,
		      0L, NIL, NIL, NIL, NIL, 0, 0, 0, 0); } ; }
		  yyS.ImportDeclaration.tree = mimport (NoTree, yyA [1].Name.tree);
 ;

} break;
case 525:
case 425: yyDecrement (5) yySetNT (yyNTImportDeclaration) {
/* line 623 "Parser.lrk" */
 ;
{  if (hl >= 0) { to_files (yyA [1].Name.tree, current_string);
		   (void) strcat (current_string, "/*");
		   put_symbol (PAF_INCLUDE_DEF, NIL, current_string, current_file,
		      (int) yyA [1].Name.bpos.Line, (int) yyA [1].Name.bpos.Column - 1,
		      (int) yyA [1].Name.epos.Line, (int) yyA [1].Name.epos.Column,
		      0L, NIL, NIL, NIL, NIL, 0, 0, 0, 0); } ; }
		  yyS.ImportDeclaration.tree = mimport_asterisk (NoTree, yyA [1].Name.tree);
 ;

} break;
case 526: yyDecrement (1) yySetNT (yyNTTypeDeclaration) {
/* line 633 "Parser.lrk" */
yyS.TypeDeclaration.tree = yyA [0].ClassDeclaration.tree;

} break;
case 527: yyDecrement (1) yySetNT (yyNTTypeDeclaration) {
/* line 635 "Parser.lrk" */
yyS.TypeDeclaration.tree = yyA [0].InterfaceDeclaration.tree;

} break;
case 528: yyDecrement (1) yySetNT (yyNTModifiers) {
/* line 637 "Parser.lrk" */
yyS.Modifiers.attr = yyA [0].Modifier.attr;

} break;
case 529: yyDecrement (2) yySetNT (yyNTModifiers) {
/* line 639 "Parser.lrk" */
 yyS.Modifiers.attr = yyA [0].Modifiers.attr | yyA [1].Modifier.attr;
 ;

} break;
case 530:
case 426: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 642 "Parser.lrk" */
 yyS.Modifier.attr = PAF_PUBLIC		;
 ;

} break;
case 531:
case 427: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 645 "Parser.lrk" */
 yyS.Modifier.attr = PAF_PROTECTED		;
 ;

} break;
case 532:
case 428: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 648 "Parser.lrk" */
 yyS.Modifier.attr = PAF_PRIVATE		;
 ;

} break;
case 533:
case 429: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 651 "Parser.lrk" */
 yyS.Modifier.attr = PAF_STATIC		;
 ;

} break;
case 534:
case 430: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 654 "Parser.lrk" */
 yyS.Modifier.attr = PAF_ABSTRACT		;
 ;

} break;
case 535:
case 431: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 657 "Parser.lrk" */
 yyS.Modifier.attr = PAF_FINAL		;
 ;

} break;
case 536:
case 432: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 660 "Parser.lrk" */
 yyS.Modifier.attr = PAF_NATIVE		;
 ;

} break;
case 537:
case 433: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 663 "Parser.lrk" */
 yyS.Modifier.attr = PAF_SYNCHRONIZED	;
 ;

} break;
case 538:
case 434: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 666 "Parser.lrk" */
 yyS.Modifier.attr = PAF_TRANSIENT		;
 ;

} break;
case 539:
case 435: yyDecrement (1) yySetNT (yyNTModifier) {
/* line 669 "Parser.lrk" */
 yyS.Modifier.attr = PAF_VOLATILE		;
 ;

} break;
case 540: yyDecrement (6) yySetNT (yyNTClassDeclaration) {
/* line 672 "Parser.lrk" */
 yyS.ClassDeclaration = yyA [2].ClassDeclaration;
 ;
{  if (hl >= 0) {
		   current_attr = PAF_PRIVATE;
		   put_symbol (PAF_CLASS_DEF, top (), current_class, current_file,
		      (int) yyA [1].Scan.Position.Line, (int) yyA [1].Scan.Position.Column - 1,
		      (int) yyA [5].ClassBody.epos.Line, (int) yyA [5].ClassBody.epos.Column,
		      current_attr, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
		   pop (); } ; }
		  yyS.ClassDeclaration.tree = mclass (NoTree, 0L,
			yyA [1].Scan.Identifier.ident, yyA [1].Scan.Position, yyA [5].ClassBody.tree,
			yyA [3].Super.tree, yyA [4].Interfaces.tree);
 ;

} break;
case 541: yyDecrement (7) yySetNT (yyNTClassDeclaration) {
/* line 689 "Parser.lrk" */
 yyS.ClassDeclaration = yyA [3].ClassDeclaration;
 ;
{  if (hl >= 0) {
		   current_attr = yyA [0].Modifiers.attr;
		   if ((current_attr & (PAF_PRIVATE | PAF_PROTECTED | PAF_PUBLIC)) == 0L)
		      current_attr |= PAF_PRIVATE;
		   put_symbol (PAF_CLASS_DEF, top (), current_class, current_file,
		      (int) yyA [2].Scan.Position.Line, (int) yyA [2].Scan.Position.Column - 1,
		      (int) yyA [6].ClassBody.epos.Line, (int) yyA [6].ClassBody.epos.Column,
		      current_attr, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
		   pop (); } ; }
		  yyS.ClassDeclaration.tree = mclass (NoTree, yyA [0].Modifiers.attr,
			yyA [2].Scan.Identifier.ident, yyA [2].Scan.Position, yyA [6].ClassBody.tree,
			yyA [4].Super.tree, yyA [5].Interfaces.tree);
 ;

} break;
case 542: yySetNT (yyNTxx_ClassDeclaration_1_3) {
/* line 686 "Parser.lrk" */
 ;
{  if (hl >= 0) push (yyA [-1].Scan.Identifier.ident); } ;

} break;
case 543: yySetNT (yyNTxx_ClassDeclaration_2_4) {
/* line 705 "Parser.lrk" */
 ;
{  if (hl >= 0) push (yyA [-1].Scan.Identifier.ident); } ;

} break;
case 544: yySetNT (yyNTSuper) {
/* line 708 "Parser.lrk" */
 yyS.Super.tree = dnotype;
 ;

} break;
case 545: yyDecrement (2) yySetNT (yyNTSuper) {
/* line 711 "Parser.lrk" */
yyS.Super.tree = yyA [1].ClassType.tree;
 ;
{  if (hl >= 0) { to_types (yyA [1].ClassType.tree, current_string);
		   put_symbol (PAF_CLASS_INHERIT, current_class, current_string, current_file,
		      (int) yyA [1].ClassType.bpos.Line, (int) yyA [1].ClassType.bpos.Column - 1,
		      (int) yyA [1].ClassType.epos.Line, (int) yyA [1].ClassType.epos.Column,
		      (unsigned long) PAF_PUBLIC, NIL, NIL, NIL, NIL, 0, 0, 0,
		      0); } ; } ;

} break;
case 546: yySetNT (yyNTInterfaces) {
/* line 720 "Parser.lrk" */
 yyS.Interfaces.tree = dnotype_name;
 ;

} break;
case 547: yyDecrement (2) yySetNT (yyNTInterfaces) {
/* line 723 "Parser.lrk" */
 yyS.Interfaces.tree = ReverseTree (yyA [1].InterfaceTypeList.tree);
 ;

} break;
case 548: yyDecrement (1) yySetNT (yyNTInterfaceTypeList) {
/* line 726 "Parser.lrk" */
 yyS.InterfaceTypeList.tree = mtype_name (dnotype_name, yyA [0].InterfaceType.tree);
 ;

} break;
case 549: yyDecrement (3) yySetNT (yyNTInterfaceTypeList) {
/* line 729 "Parser.lrk" */
 yyS.InterfaceTypeList.tree = mtype_name (yyA [0].InterfaceTypeList.tree, yyA [2].InterfaceType.tree);
 ;

} break;
case 550:
case 436: yyDecrement (3) yySetNT (yyNTClassBody) {
/* line 732 "Parser.lrk" */
 yyS.ClassBody.tree = ReverseTree (yyA [1].ClassBodyDeclarations.tree);

		  yyS.ClassBody.epos  =  yyA [2].Scan.Position;
 ;

} break;
case 551: yySetNT (yyNTClassBodyDeclarations) {
/* line 737 "Parser.lrk" */
 yyS.ClassBodyDeclarations.tree = mnofield ();
 ;

} break;
case 552: yyDecrement (2) yySetNT (yyNTClassBodyDeclarations) {
/* line 740 "Parser.lrk" */
 yyS.ClassBodyDeclarations.tree = yyA [1].ClassBodyDeclaration.tree;
			yyS.ClassBodyDeclarations.tree->field.next = yyA [0].ClassBodyDeclarations.tree; ;
 ;

} break;
case 553:
case 437: yyDecrement (2) yySetNT (yyNTClassBodyDeclarations) {
/* line 744 "Parser.lrk" */
yyS.ClassBodyDeclarations.tree = yyA [0].ClassBodyDeclarations.tree;

} break;
case 554: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 746 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].FieldDeclaration.tree;

} break;
case 555: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 748 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].MethodDeclaration.tree;

} break;
case 556: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 750 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].ClassDeclaration.tree;

} break;
case 557: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 752 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].InterfaceDeclaration.tree;

} break;
case 558: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 754 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].ClassInitializer.tree;

} break;
case 559: yyDecrement (1) yySetNT (yyNTClassBodyDeclaration) {
/* line 756 "Parser.lrk" */
yyS.ClassBodyDeclaration.tree = yyA [0].ConstructorDeclaration.tree;

} break;
case 560: yySetNT (yyNTget_type_1) {
/* line 758 "Parser.lrk" */
 ;
{  current_attr = PAF_PRIVATE;
		    current_type = yyA [-1].Type.tree; ; } ;

} break;
case 561: yySetNT (yyNTget_type_2) {
/* line 762 "Parser.lrk" */
 ;
{  current_attr = yyA [-2].Modifiers.attr;
		    if ((current_attr & (PAF_PRIVATE | PAF_PROTECTED | PAF_PUBLIC)) == 0L)
		       current_attr |= PAF_PRIVATE;
		    current_type = yyA [-1].Type.tree; ; } ;

} break;
case 562:
case 438: yyDecrement (4) yySetNT (yyNTFieldDeclaration) {
/* line 768 "Parser.lrk" */
 yyS.FieldDeclaration.tree = mvar_decl (NoTree, 0L, yyA [0].Type.tree, yyA [2].VariableDeclarators.tree);
 ;

} break;
case 563:
case 439: yyDecrement (5) yySetNT (yyNTFieldDeclaration) {
/* line 771 "Parser.lrk" */
 yyS.FieldDeclaration.tree = mvar_decl (NoTree, yyA [0].Modifiers.attr, yyA [1].Type.tree, yyA [3].VariableDeclarators.tree);
 ;

} break;
case 564: yyDecrement (1) yySetNT (yyNTVariableDeclarators) {
/* line 774 "Parser.lrk" */
 yyS.VariableDeclarators.tree = yyA [0].VariableDeclarator.tree;
			yyS.VariableDeclarators.tree->variable.next = dnodecl; ;
 ;

} break;
case 565: yyDecrement (3) yySetNT (yyNTVariableDeclarators) {
/* line 778 "Parser.lrk" */
 yyS.VariableDeclarators.tree = yyA [2].VariableDeclarator.tree;
			yyS.VariableDeclarators.tree->variable.next = yyA [0].VariableDeclarators.tree; ;
 ;

} break;
case 566: yyDecrement (2) yySetNT (yyNTVariableDeclarator) {
/* line 782 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [0].VariableDeclaratorId.ident);
		   put_symbol (PAF_MBR_VAR_DEF, current_class, current_ident, current_file,
		      (int) yyA [0].VariableDeclaratorId.Position.Line, (int) yyA [0].VariableDeclaratorId.Position.Column - 1,
		      (int) yyA [0].VariableDeclaratorId.Position.Line, (int) (yyA [0].VariableDeclaratorId.Position.Column +
		      LengthSt (GetStringRef (yyA [0].VariableDeclaratorId.ident)) - 1),
		      current_attr, NIL, NIL, NIL, NIL, 0, 0, 0, 0); } ; }
		  yyS.VariableDeclarator.tree = mvariable (NoTree, current_attr, current_type,
		  	yyA [0].VariableDeclaratorId.ident, yyA [0].VariableDeclaratorId.Position, yyA [0].VariableDeclaratorId.tree, yyA [1].VariableInitializerOpt.tree);
 ;

} break;
case 567:
case 440: yyDecrement (1) yySetNT (yyNTVariableDeclaratorId) {
/* line 793 "Parser.lrk" */
yyS.VariableDeclaratorId.Position = yyA [0].Scan.Position;
yyS.VariableDeclaratorId.ident = yyA [0].Scan.Identifier.ident;
 yyS.VariableDeclaratorId.tree = dnotype;
 ;

} break;
case 568:
case 441: yyDecrement (3) yySetNT (yyNTVariableDeclaratorId) {
/* line 798 "Parser.lrk" */
yyS.VariableDeclaratorId.ident = yyA [0].VariableDeclaratorId.ident;
 yyS.VariableDeclaratorId.tree = marray_type (yyA [0].VariableDeclaratorId.tree);

		  yyS.VariableDeclaratorId.Position  =  yyA [0].VariableDeclaratorId.Position;
 ;

} break;
case 569: yyDecrement (3) yySetNT (yyNTLocalVariableDeclaration) {
/* line 804 "Parser.lrk" */
 yyS.LocalVariableDeclaration = yyA [1].LocalVariableDeclaration;
 yyS.LocalVariableDeclaration.tree = mvar_decl_stmt (yyA [0].Type.tree, ReverseTree (yyA [2].LocalVariableDeclarators.tree));
 ;

} break;
case 570: yyDecrement (4) yySetNT (yyNTLocalVariableDeclaration) {
/* line 811 "Parser.lrk" */
 yyS.LocalVariableDeclaration = yyA [2].LocalVariableDeclaration;
 yyS.LocalVariableDeclaration.tree = mvar_decl_stmt (yyA [1].Type.tree, ReverseTree (yyA [3].LocalVariableDeclarators.tree));
 ;

} break;
case 571: yySetNT (yyNTxx_LocalVariableDeclaration_1_2) {
/* line 808 "Parser.lrk" */
 ;
{  current_attr = 0L; current_type = yyA [-1].Type.tree; ; } ;

} break;
case 572: yySetNT (yyNTxx_LocalVariableDeclaration_2_3) {
/* line 815 "Parser.lrk" */
 ;
{  current_attr = a_FINAL; current_type = yyA [-1].Type.tree; ; } ;

} break;
case 573: yyDecrement (1) yySetNT (yyNTLocalVariableDeclarators) {
/* line 818 "Parser.lrk" */
 yyS.LocalVariableDeclarators.tree = yyA [0].LocalVariableDeclarator.tree;
			yyS.LocalVariableDeclarators.tree->variable.next = dnodecl; ;
 ;

} break;
case 574: yyDecrement (3) yySetNT (yyNTLocalVariableDeclarators) {
/* line 822 "Parser.lrk" */
 yyS.LocalVariableDeclarators.tree = yyA [2].LocalVariableDeclarator.tree;
			yyS.LocalVariableDeclarators.tree->variable.next = yyA [0].LocalVariableDeclarators.tree; ;
 ;

} break;
case 575: yyDecrement (2) yySetNT (yyNTLocalVariableDeclarator) {
/* line 826 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [0].VariableDeclaratorId.ident);
		   put_symbol (PAF_LOCAL_VAR_DEF, NIL, current_ident, current_file,
		      (int) yyA [0].VariableDeclaratorId.Position.Line, (int) yyA [0].VariableDeclaratorId.Position.Column - 1,
		      (int) yyA [0].VariableDeclaratorId.Position.Line, (int) (yyA [0].VariableDeclaratorId.Position.Column +
		      LengthSt (GetStringRef (yyA [0].VariableDeclaratorId.ident)) - 1),
		      0L, NIL, NIL, NIL, NIL, 0, 0, 0, 0); } ; }
		  yyS.LocalVariableDeclarator.tree = mvariable (NoTree, current_attr, current_type,
		  	yyA [0].VariableDeclaratorId.ident, yyA [0].VariableDeclaratorId.Position, yyA [0].VariableDeclaratorId.tree, yyA [1].VariableInitializerOpt.tree);
 ;

} break;
case 576: yyDecrement (2) yySetNT (yyNTMethodDeclaration) {
/* line 837 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [0].MethodHeader.ident);
		   to_types (yyA [0].MethodHeader.type, ret_buffer);
		   to_types (yyA [0].MethodHeader.tree, arg_types_buffer);
		   to_names (yyA [0].MethodHeader.tree, args_buffer);
		   current_attr = yyA [0].MethodHeader.attr;
		   if ((current_attr & (PAF_PRIVATE | PAF_PROTECTED | PAF_PUBLIC)) == 0L)
		      current_attr |= PAF_PRIVATE;
		   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file,
		      (int) yyA [0].MethodHeader.Position.Line, (int) yyA [0].MethodHeader.Position.Column - 1,
		      (int) yyA [1].MethodBody.epos.Line, (int) yyA [1].MethodBody.epos.Column,
		      current_attr, ret_buffer, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file,
		      (int) yyA [0].MethodHeader.Position.Line, (int) yyA [0].MethodHeader.Position.Column - 1,
		      (int) yyA [1].MethodBody.epos.Line, (int) yyA [1].MethodBody.epos.Column,
		      current_attr, ret_buffer, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		} ; }
		  yyS.MethodDeclaration.tree = mmethod (NoTree, yyA [0].MethodHeader.attr, yyA [0].MethodHeader.ident,
			yyA [0].MethodHeader.Position, yyA [0].MethodHeader.tree,
			yyA [0].MethodHeader.throws, yyA [1].MethodBody.tree,
			yyA [0].MethodHeader.type, yyA [0].MethodHeader.array);
 ;

} break;
case 577: yyDecrement (4) yySetNT (yyNTMethodHeader) {
/* line 862 "Parser.lrk" */
yyS.MethodHeader.array = yyA [2].MethodDeclarator.array;
yyS.MethodHeader.ident = yyA [2].MethodDeclarator.ident;
 yyS.MethodHeader.tree  =  yyA [2].MethodDeclarator.tree;
 yyS.MethodHeader.attr = 0L;

		  yyS.MethodHeader.type  =  yyA [0].Type.tree;
 yyS.MethodHeader.throws  =  yyA [3].ThrowsOpt.tree;

		  yyS.MethodHeader.Position  =  yyA [2].MethodDeclarator.Position;
 ;

} break;
case 578: yyDecrement (5) yySetNT (yyNTMethodHeader) {
/* line 873 "Parser.lrk" */
yyS.MethodHeader.attr = yyA [0].Modifiers.attr;
yyS.MethodHeader.array = yyA [3].MethodDeclarator.array;
yyS.MethodHeader.ident = yyA [3].MethodDeclarator.ident;
 yyS.MethodHeader.tree  =  yyA [3].MethodDeclarator.tree;

		  yyS.MethodHeader.type  =  yyA [1].Type.tree;
 yyS.MethodHeader.throws  =  yyA [4].ThrowsOpt.tree;

		  yyS.MethodHeader.Position  =  yyA [3].MethodDeclarator.Position;
 ;

} break;
case 579:
case 442: yyDecrement (4) yySetNT (yyNTMethodDeclarator) {
/* line 884 "Parser.lrk" */
yyS.MethodDeclarator.ident = yyA [0].Scan.Identifier.ident;
yyS.MethodDeclarator.tree = yyA [2].FormalParameterListOpt.tree;
 yyS.MethodDeclarator.array = dnotype;

		  yyS.MethodDeclarator.Position  =  yyA [0].Scan.Position;
 ;

} break;
case 580:
case 443: yyDecrement (3) yySetNT (yyNTMethodDeclarator) {
/* line 891 "Parser.lrk" */
yyS.MethodDeclarator.ident = yyA [0].MethodDeclarator.ident;
yyS.MethodDeclarator.tree = yyA [0].MethodDeclarator.tree;
 yyS.MethodDeclarator.array = marray_type (yyA [0].MethodDeclarator.array);

		  yyS.MethodDeclarator.Position  =  yyA [0].MethodDeclarator.Position;
 ;

} break;
case 581: yyDecrement (1) yySetNT (yyNTMethodBody) {
/* line 898 "Parser.lrk" */
yyS.MethodBody.epos = yyA [0].Block.epos;
yyS.MethodBody.tree = yyA [0].Block.tree;

} break;
case 582:
case 444: yyDecrement (1) yySetNT (yyNTMethodBody) {
/* line 901 "Parser.lrk" */
 yyS.MethodBody.epos  =  yyA [0].Scan.Position;
 yyS.MethodBody.tree = mnull_stmt ();
 ;

} break;
case 583: yySetNT (yyNTFormalParameterListOpt) {
/* line 905 "Parser.lrk" */
 yyS.FormalParameterListOpt.tree = dnodecl;
 ;

} break;
case 584: yyDecrement (1) yySetNT (yyNTFormalParameterListOpt) {
/* line 908 "Parser.lrk" */
 yyS.FormalParameterListOpt.tree = ReverseTree (yyA [0].FormalParameterList.tree);
 ;

} break;
case 585: yyDecrement (1) yySetNT (yyNTFormalParameterList) {
/* line 911 "Parser.lrk" */
 yyS.FormalParameterList.tree = yyA [0].FormalParameter.tree;
			yyS.FormalParameterList.tree->parameter.next = dnodecl; ;
 ;

} break;
case 586: yyDecrement (3) yySetNT (yyNTFormalParameterList) {
/* line 915 "Parser.lrk" */
 yyS.FormalParameterList.tree = yyA [2].FormalParameter.tree;
			yyS.FormalParameterList.tree->parameter.next = yyA [0].FormalParameterList.tree; ;
 ;

} break;
case 587: yyDecrement (2) yySetNT (yyNTFormalParameter) {
/* line 919 "Parser.lrk" */
 yyS.FormalParameter.tree = mparameter (NoTree, 0L, yyA [0].Type.tree, yyA [1].VariableDeclaratorId.ident,
			yyA [1].VariableDeclaratorId.Position, yyA [1].VariableDeclaratorId.tree);
 ;

} break;
case 588: yyDecrement (3) yySetNT (yyNTFormalParameter) {
/* line 923 "Parser.lrk" */
 yyS.FormalParameter.tree = mparameter (NoTree, PAF_FINAL, yyA [1].Type.tree, yyA [2].VariableDeclaratorId.ident,
			yyA [2].VariableDeclaratorId.Position, yyA [2].VariableDeclaratorId.tree);
 ;

} break;
case 589: yySetNT (yyNTThrowsOpt) {
/* line 927 "Parser.lrk" */
 yyS.ThrowsOpt.tree = dnotype_name;
 ;

} break;
case 590: yyDecrement (2) yySetNT (yyNTThrowsOpt) {
/* line 930 "Parser.lrk" */
 yyS.ThrowsOpt.tree = ReverseTree (yyA [1].ClassTypeList.tree);
 ;

} break;
case 591: yyDecrement (1) yySetNT (yyNTClassTypeList) {
/* line 933 "Parser.lrk" */
 yyS.ClassTypeList.tree = mtype_name (dnotype_name, yyA [0].ClassType.tree);
 ;

} break;
case 592: yyDecrement (3) yySetNT (yyNTClassTypeList) {
/* line 936 "Parser.lrk" */
 yyS.ClassTypeList.tree = mtype_name (yyA [0].ClassTypeList.tree, yyA [2].ClassType.tree);
 ;

} break;
case 593: yyDecrement (1) yySetNT (yyNTClassInitializer) {
/* line 939 "Parser.lrk" */
 yyS.ClassInitializer.tree = minitializer (NoTree, yyA [0].Block.tree);
 ;

} break;
case 594: yyDecrement (2) yySetNT (yyNTClassInitializer) {
/* line 942 "Parser.lrk" */
 yyS.ClassInitializer.tree = mstatic_initializer (NoTree, yyA [1].Block.tree);
 ;

} break;
case 595: yyDecrement (3) yySetNT (yyNTConstructorDeclaration) {
/* line 945 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [0].ConstructorDeclarator.ident);
		   to_types (yyA [0].ConstructorDeclarator.tree, arg_types_buffer);
		   to_names (yyA [0].ConstructorDeclarator.tree, args_buffer);
		   current_attr = PAF_PRIVATE;
		   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file,
		      (int) yyA [0].ConstructorDeclarator.Position.Line, (int) yyA [0].ConstructorDeclarator.Position.Column - 1,
		      (int) yyA [2].ConstructorBody.epos.Line, (int) yyA [2].ConstructorBody.epos.Column,
		      current_attr | PAF_CONSTRUCTOR, NIL, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file,
		      (int) yyA [0].ConstructorDeclarator.Position.Line, (int) yyA [0].ConstructorDeclarator.Position.Column - 1,
		      (int) yyA [2].ConstructorBody.epos.Line, (int) yyA [2].ConstructorBody.epos.Column,
		      current_attr | PAF_CONSTRUCTOR, NIL, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		} ; }
		  yyS.ConstructorDeclaration.tree = mconstructor (NoTree, 0L, yyA [0].ConstructorDeclarator.ident,
			yyA [0].ConstructorDeclarator.Position, yyA [0].ConstructorDeclarator.tree, yyA [1].ThrowsOpt.tree,
			mcompound_stmt (yyA [2].ConstructorBody.tree));
 ;

} break;
case 596: yyDecrement (4) yySetNT (yyNTConstructorDeclaration) {
/* line 966 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [1].ConstructorDeclarator.ident);
		   to_types (yyA [1].ConstructorDeclarator.tree, arg_types_buffer);
		   to_names (yyA [1].ConstructorDeclarator.tree, args_buffer);
		   current_attr = yyA [0].Modifiers.attr;
		   if ((current_attr & (PAF_PRIVATE | PAF_PROTECTED | PAF_PUBLIC)) == 0L)
		      current_attr |= PAF_PRIVATE;
		   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file,
		      (int) yyA [1].ConstructorDeclarator.Position.Line, (int) yyA [1].ConstructorDeclarator.Position.Column - 1,
		      (int) yyA [3].ConstructorBody.epos.Line, (int) yyA [3].ConstructorBody.epos.Column,
		      current_attr | PAF_CONSTRUCTOR, NIL, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file,
		      (int) yyA [1].ConstructorDeclarator.Position.Line, (int) yyA [1].ConstructorDeclarator.Position.Column - 1,
		      (int) yyA [3].ConstructorBody.epos.Line, (int) yyA [3].ConstructorBody.epos.Column,
		      current_attr | PAF_CONSTRUCTOR, NIL, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		} ; }
		  yyS.ConstructorDeclaration.tree = mconstructor (NoTree, yyA [0].Modifiers.attr, yyA [1].ConstructorDeclarator.ident,
			yyA [1].ConstructorDeclarator.Position, yyA [1].ConstructorDeclarator.tree, yyA [2].ThrowsOpt.tree,
			mcompound_stmt (yyA [3].ConstructorBody.tree));
 ;

} break;
case 597:
case 445: yyDecrement (4) yySetNT (yyNTConstructorDeclarator) {
/* line 989 "Parser.lrk" */
yyS.ConstructorDeclarator.ident = yyA [0].Scan.Identifier.ident;
yyS.ConstructorDeclarator.tree = yyA [2].FormalParameterListOpt.tree;
 yyS.ConstructorDeclarator.Position  =  yyA [0].Scan.Position;
 ;

} break;
case 598:
case 446: yyDecrement (2) yySetNT (yyNTConstructorBody) {
/* line 994 "Parser.lrk" */
 yyS.ConstructorBody.tree = dnostatement;

		  yyS.ConstructorBody.epos  =  yyA [1].Scan.Position;
 ;

} break;
case 599:
case 447: yyDecrement (3) yySetNT (yyNTConstructorBody) {
/* line 999 "Parser.lrk" */
 yyS.ConstructorBody.tree = ReverseTree (yyA [1].BlockStatements.tree);

		  yyS.ConstructorBody.epos  =  yyA [2].Scan.Position;
 ;

} break;
case 600:
case 448: yyDecrement (3) yySetNT (yyNTConstructorBody) {
/* line 1004 "Parser.lrk" */
 yyS.ConstructorBody.tree = yyA [1].ExplicitConstructorInvocation.tree;
			yyS.ConstructorBody.tree->statement.next = dnostatement; ;

		  yyS.ConstructorBody.epos  =  yyA [2].Scan.Position;
 ;

} break;
case 601:
case 449: yyDecrement (4) yySetNT (yyNTConstructorBody) {
/* line 1010 "Parser.lrk" */
 yyS.ConstructorBody.tree = yyA [1].ExplicitConstructorInvocation.tree;
		   yyS.ConstructorBody.tree->statement.next = ReverseTree (yyA [2].BlockStatements.tree); ;

		  yyS.ConstructorBody.epos  =  yyA [3].Scan.Position;
 ;

} break;
case 602:
case 450: yyDecrement (5) yySetNT (yyNTExplicitConstructorInvocation) {
/* line 1016 "Parser.lrk" */
 yyS.ExplicitConstructorInvocation.tree = mstatement (dnostatement, mexpression_stmt (mcall
		(yyA [1].Scan.Position, msuper (yyA [0].Scan.Position), yyA [2].ArgumentListOpt.tree)));
 ;

} break;
case 603:
case 451: yyDecrement (5) yySetNT (yyNTExplicitConstructorInvocation) {
/* line 1020 "Parser.lrk" */
 yyS.ExplicitConstructorInvocation.tree = mstatement (dnostatement, mexpression_stmt (mcall
		(yyA [1].Scan.Position, mthis (yyA [0].Scan.Position), yyA [2].ArgumentListOpt.tree)));
 ;

} break;
case 604: yyDecrement (5) yySetNT (yyNTInterfaceDeclaration) {
/* line 1024 "Parser.lrk" */
 yyS.InterfaceDeclaration = yyA [2].InterfaceDeclaration;
 ;
{  if (hl >= 0) {
		   current_attr = PAF_PRIVATE;
		   put_symbol (PAF_CLASS_DEF, top (), current_class, current_file,
		      (int) yyA [1].Scan.Position.Line, (int) yyA [1].Scan.Position.Column - 1,
		      (int) yyA [4].InterfaceBody.epos.Line, (int) yyA [4].InterfaceBody.epos.Column,
		      current_attr | PAF_INTERFACE, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
		   pop (); } ; }
		  yyS.InterfaceDeclaration.tree = minterface (NoTree, 0L,
			yyA [1].Scan.Identifier.ident, yyA [1].Scan.Position,
			yyA [4].InterfaceBody.tree, yyA [3].ExtendsInterfacesOpt.tree);
 ;

} break;
case 605: yyDecrement (6) yySetNT (yyNTInterfaceDeclaration) {
/* line 1041 "Parser.lrk" */
 yyS.InterfaceDeclaration = yyA [3].InterfaceDeclaration;
 ;
{  if (hl >= 0) {
		   current_attr = yyA [0].Modifiers.attr;
		   if ((current_attr & (PAF_PRIVATE | PAF_PROTECTED | PAF_PUBLIC)) == 0L)
		      current_attr |= PAF_PRIVATE;
		   put_symbol (PAF_CLASS_DEF, top (), current_class, current_file,
		      (int) yyA [2].Scan.Position.Line, (int) yyA [2].Scan.Position.Column - 1,
		      (int) yyA [5].InterfaceBody.epos.Line, (int) yyA [5].InterfaceBody.epos.Column,
		      current_attr | PAF_INTERFACE, NIL, NIL, NIL, NIL, 0, 0, 0, 0);
		   pop (); } ; }
		  yyS.InterfaceDeclaration.tree = minterface (NoTree, yyA [0].Modifiers.attr,
			yyA [2].Scan.Identifier.ident, yyA [2].Scan.Position,
			yyA [5].InterfaceBody.tree, yyA [4].ExtendsInterfacesOpt.tree);
 ;

} break;
case 606: yySetNT (yyNTxx_InterfaceDeclaration_1_3) {
/* line 1038 "Parser.lrk" */
 ;
{  if (hl >= 0) push (yyA [-1].Scan.Identifier.ident); } ;

} break;
case 607: yySetNT (yyNTxx_InterfaceDeclaration_2_4) {
/* line 1057 "Parser.lrk" */
 ;
{  if (hl >= 0) push (yyA [-1].Scan.Identifier.ident); } ;

} break;
case 608: yySetNT (yyNTExtendsInterfacesOpt) {
/* line 1060 "Parser.lrk" */
 yyS.ExtendsInterfacesOpt.tree = dnotype_name;
 ;

} break;
case 609: yyDecrement (2) yySetNT (yyNTExtendsInterfacesOpt) {
/* line 1063 "Parser.lrk" */
 yyS.ExtendsInterfacesOpt.tree = ReverseTree (yyA [1].ExtendsInterfaces.tree);

		  ;
{  if (hl >= 0) put_extends (yyS.ExtendsInterfacesOpt.tree); ; } ;

} break;
case 610: yyDecrement (1) yySetNT (yyNTExtendsInterfaces) {
/* line 1068 "Parser.lrk" */
 yyS.ExtendsInterfaces.tree = mtype_name (dnotype_name, yyA [0].InterfaceType.tree);
 ;

} break;
case 611: yyDecrement (3) yySetNT (yyNTExtendsInterfaces) {
/* line 1071 "Parser.lrk" */
 yyS.ExtendsInterfaces.tree = mtype_name (yyA [0].ExtendsInterfaces.tree, yyA [2].InterfaceType.tree);
 ;

} break;
case 612:
case 452: yyDecrement (3) yySetNT (yyNTInterfaceBody) {
/* line 1074 "Parser.lrk" */
 yyS.InterfaceBody.tree = ReverseTree (yyA [1].InterfaceMemberDeclarations.tree);

		  yyS.InterfaceBody.epos  =  yyA [2].Scan.Position;
 ;

} break;
case 613: yySetNT (yyNTInterfaceMemberDeclarations) {
/* line 1079 "Parser.lrk" */
 yyS.InterfaceMemberDeclarations.tree = dnofield;
 ;

} break;
case 614: yyDecrement (2) yySetNT (yyNTInterfaceMemberDeclarations) {
/* line 1082 "Parser.lrk" */
 yyS.InterfaceMemberDeclarations.tree = yyA [1].InterfaceMemberDeclaration.tree;
		     yyS.InterfaceMemberDeclarations.tree->field.next = yyA [0].InterfaceMemberDeclarations.tree; ;
 ;

} break;
case 615: yyDecrement (1) yySetNT (yyNTInterfaceMemberDeclaration) {
/* line 1086 "Parser.lrk" */
yyS.InterfaceMemberDeclaration.tree = yyA [0].FieldDeclaration.tree;

} break;
case 616:
case 453: yyDecrement (2) yySetNT (yyNTInterfaceMemberDeclaration) {
/* line 1088 "Parser.lrk" */
 ;
{  if (hl >= 0) { current_ident = GetCStr (yyA [0].MethodHeader.ident);
		   to_types (yyA [0].MethodHeader.type, ret_buffer);
		   to_types (yyA [0].MethodHeader.tree, arg_types_buffer);
		   to_names (yyA [0].MethodHeader.tree, args_buffer);
		   put_symbol (PAF_MBR_FUNC_DEF, current_class, current_ident, current_file,
		      (int) yyA [0].MethodHeader.Position.Line, (int) yyA [0].MethodHeader.Position.Column - 1,
		      (int) yyA [1].Scan.Position.Line, (int) yyA [1].Scan.Position.Column,
		      current_attr, ret_buffer, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		   put_symbol (PAF_MBR_FUNC_DCL, current_class, current_ident, current_file,
		      (int) yyA [0].MethodHeader.Position.Line, (int) yyA [0].MethodHeader.Position.Column - 1,
		      (int) yyA [1].Scan.Position.Line, (int) yyA [1].Scan.Position.Column,
		      current_attr, ret_buffer, arg_types_buffer,
		      args_buffer, NIL, 0, 0, 0, 0);
		} ; }
		  yyS.InterfaceMemberDeclaration.tree = mmethod (NoTree, yyA [0].MethodHeader.attr, yyA [0].MethodHeader.ident,
			yyA [0].MethodHeader.Position, yyA [0].MethodHeader.tree,
			yyA [0].MethodHeader.throws, dnostatement,
			yyA [0].MethodHeader.type, yyA [0].MethodHeader.array);
 ;

} break;
case 617: yyDecrement (1) yySetNT (yyNTInterfaceMemberDeclaration) {
/* line 1110 "Parser.lrk" */
yyS.InterfaceMemberDeclaration.tree = yyA [0].ClassDeclaration.tree;

} break;
case 618: yyDecrement (1) yySetNT (yyNTInterfaceMemberDeclaration) {
/* line 1112 "Parser.lrk" */
yyS.InterfaceMemberDeclaration.tree = yyA [0].InterfaceDeclaration.tree;

} break;
case 619: yySetNT (yyNTVariableInitializerOpt) {
/* line 1114 "Parser.lrk" */
 yyS.VariableInitializerOpt.tree = dnoexpression;
 ;

} break;
case 620: yyDecrement (2) yySetNT (yyNTVariableInitializerOpt) {
/* line 1117 "Parser.lrk" */
yyS.VariableInitializerOpt.tree = yyA [1].VariableInitializer.tree;

} break;
case 621: yyDecrement (1) yySetNT (yyNTVariableInitializer) {
/* line 1119 "Parser.lrk" */
yyS.VariableInitializer.tree = yyA [0].Expression.tree;

} break;
case 622:
case 454: yyDecrement (3) yySetNT (yyNTVariableInitializer) {
/* line 1121 "Parser.lrk" */
 yyS.VariableInitializer.tree = maggregate (yyA [0].Scan.Position, yyA [1].VariableInitializersOpt.tree);
 ;

} break;
case 623:
case 455: yyDecrement (3) yySetNT (yyNTArrayInitializer) {
/* line 1124 "Parser.lrk" */
 yyS.VariableInitializer.tree = maggregate (yyA [0].Scan.Position, yyA [1].VariableInitializersOpt.tree);
 ;

} break;
case 624: yyDecrement (1) yySetNT (yyNTVariableInitializersOpt) {
/* line 1127 "Parser.lrk" */
 yyS.VariableInitializersOpt.tree = dnoexpression_l;
 ;

} break;
case 625: yyDecrement (2) yySetNT (yyNTVariableInitializersOpt) {
/* line 1130 "Parser.lrk" */
 yyS.VariableInitializersOpt.tree = ReverseTree (yyA [0].VariableInitializers.tree);
 ;

} break;
case 626: yyDecrement (1) yySetNT (yyNTVariableInitializers) {
/* line 1133 "Parser.lrk" */
 yyS.VariableInitializers.tree = mexpression (dnoexpression_l, yyA [0].VariableInitializer.tree);
 ;

} break;
case 627: yyDecrement (3) yySetNT (yyNTVariableInitializers) {
/* line 1136 "Parser.lrk" */
 yyS.VariableInitializers.tree = mexpression (yyA [0].VariableInitializers.tree, yyA [2].VariableInitializer.tree);
 ;

} break;
case 628: yySetNT (yyNTCommaOpt) {
} break;
case 629:
case 456: yyDecrement (1) yySetNT (yyNTCommaOpt) {
} break;
case 630: yySetNT (yyNTArrayInitializerOpt) {
/* line 1141 "Parser.lrk" */
 yyS.ArrayInitializerOpt.tree = dnoexpression;
 ;

} break;
case 631: yyDecrement (1) yySetNT (yyNTArrayInitializerOpt) {
/* line 1144 "Parser.lrk" */
yyS.ArrayInitializerOpt.tree = yyA [0].VariableInitializer.tree;

} break;
case 632:
case 457: yyDecrement (2) yySetNT (yyNTBlock) {
/* line 1146 "Parser.lrk" */
 yyS.Block.tree = mcompound_stmt (dnostatement);

		  yyS.Block.epos  =  yyA [1].Scan.Position;
 ;

} break;
case 633:
case 458: yyDecrement (3) yySetNT (yyNTBlock) {
/* line 1151 "Parser.lrk" */
 yyS.Block.tree = mcompound_stmt (ReverseTree (yyA [1].BlockStatements.tree));

		  yyS.Block.epos  =  yyA [2].Scan.Position;
 ;

} break;
case 634: yyDecrement (1) yySetNT (yyNTBlockStatements) {
/* line 1156 "Parser.lrk" */
 yyS.BlockStatements.tree = mstatement (dnostatement, yyA [0].BlockStatement.tree);
 ;

} break;
case 635: yyDecrement (2) yySetNT (yyNTBlockStatements) {
/* line 1159 "Parser.lrk" */
 yyS.BlockStatements.tree = mstatement (yyA [0].BlockStatements.tree, yyA [1].BlockStatement.tree);
 ;

} break;
case 636:
case 459: yyDecrement (2) yySetNT (yyNTBlockStatement) {
/* line 1162 "Parser.lrk" */
yyS.BlockStatement.tree = yyA [0].LocalVariableDeclaration.tree;

} break;
case 637: yyDecrement (1) yySetNT (yyNTBlockStatement) {
/* line 1164 "Parser.lrk" */
 yyS.BlockStatement.tree = mtype_decl_stmt (yyA [0].ClassDeclaration.tree);
			yyA [0].ClassDeclaration.tree->type_decl.next = dnofield; ;
 ;

} break;
case 638: yyDecrement (1) yySetNT (yyNTBlockStatement) {
/* line 1168 "Parser.lrk" */
 yyS.BlockStatement.tree = mtype_decl_stmt (yyA [0].InterfaceDeclaration.tree);
			yyA [0].InterfaceDeclaration.tree->type_decl.next = dnofield; ;
 ;

} break;
case 639: yyDecrement (1) yySetNT (yyNTBlockStatement) {
/* line 1172 "Parser.lrk" */
yyS.BlockStatement.tree = yyA [0].StatementWithoutTrailingSubstatement.tree;

} break;
case 640: yyDecrement (3) yySetNT (yyNTBlockStatement) {
/* line 1174 "Parser.lrk" */
 yyS.BlockStatement.tree = mlabeled_stmt (yyA [0].Scan.Identifier.ident, yyA [0].Scan.Position, yyA [2].BlockStatement.tree);
 ;

} break;
case 641: yyDecrement (5) yySetNT (yyNTBlockStatement) {
/* line 1177 "Parser.lrk" */
 yyS.BlockStatement.tree = mif_stmt (yyA [2].Expression.tree, yyA [4].BlockStatement.tree);
 ;

} break;
case 642: yyDecrement (7) yySetNT (yyNTBlockStatement) {
/* line 1180 "Parser.lrk" */
 yyS.BlockStatement.tree = mif_else_stmt (yyA [2].Expression.tree, yyA [4].StatementNoShortIf.tree, yyA [6].BlockStatement.tree);
 ;

} break;
case 643: yyDecrement (5) yySetNT (yyNTBlockStatement) {
/* line 1183 "Parser.lrk" */
 yyS.BlockStatement.tree = mwhile_stmt (yyA [2].Expression.tree, yyA [4].BlockStatement.tree);
 ;

} break;
case 644: yyDecrement (9) yySetNT (yyNTBlockStatement) {
/* line 1186 "Parser.lrk" */
 yyS.BlockStatement.tree = mfor_stmt (yyA [2].ForInit.tree, yyA [4].ExpressionOpt.tree, yyA [6].ForUpdate.tree, yyA [8].BlockStatement.tree);
 ;

} break;
case 645: yyDecrement (1) yySetNT (yyNTStatement) {
/* line 1189 "Parser.lrk" */
yyS.BlockStatement.tree = yyA [0].StatementWithoutTrailingSubstatement.tree;

} break;
case 646: yyDecrement (3) yySetNT (yyNTStatement) {
/* line 1191 "Parser.lrk" */
 yyS.BlockStatement.tree = mlabeled_stmt (yyA [0].Scan.Identifier.ident, yyA [0].Scan.Position, yyA [2].BlockStatement.tree);
 ;

} break;
case 647: yyDecrement (5) yySetNT (yyNTStatement) {
/* line 1194 "Parser.lrk" */
 yyS.BlockStatement.tree = mif_stmt (yyA [2].Expression.tree, yyA [4].BlockStatement.tree);
 ;

} break;
case 648: yyDecrement (7) yySetNT (yyNTStatement) {
/* line 1197 "Parser.lrk" */
 yyS.BlockStatement.tree = mif_else_stmt (yyA [2].Expression.tree, yyA [4].StatementNoShortIf.tree, yyA [6].BlockStatement.tree);
 ;

} break;
case 649: yyDecrement (5) yySetNT (yyNTStatement) {
/* line 1200 "Parser.lrk" */
 yyS.BlockStatement.tree = mwhile_stmt (yyA [2].Expression.tree, yyA [4].BlockStatement.tree);
 ;

} break;
case 650: yyDecrement (9) yySetNT (yyNTStatement) {
/* line 1203 "Parser.lrk" */
 yyS.BlockStatement.tree = mfor_stmt (yyA [2].ForInit.tree, yyA [4].ExpressionOpt.tree, yyA [6].ForUpdate.tree, yyA [8].BlockStatement.tree);
 ;

} break;
case 651: yyDecrement (1) yySetNT (yyNTStatementNoShortIf) {
/* line 1206 "Parser.lrk" */
yyS.StatementNoShortIf.tree = yyA [0].StatementWithoutTrailingSubstatement.tree;

} break;
case 652: yyDecrement (3) yySetNT (yyNTStatementNoShortIf) {
/* line 1208 "Parser.lrk" */
 yyS.StatementNoShortIf.tree = mlabeled_stmt (yyA [0].Scan.Identifier.ident, yyA [0].Scan.Position, yyA [2].StatementNoShortIf.tree);
 ;

} break;
case 653: yyDecrement (7) yySetNT (yyNTStatementNoShortIf) {
/* line 1211 "Parser.lrk" */
 yyS.StatementNoShortIf.tree = mif_else_stmt (yyA [2].Expression.tree, yyA [4].StatementNoShortIf.tree, yyA [6].StatementNoShortIf.tree);
 ;

} break;
case 654: yyDecrement (5) yySetNT (yyNTStatementNoShortIf) {
/* line 1214 "Parser.lrk" */
 yyS.StatementNoShortIf.tree = mwhile_stmt (yyA [2].Expression.tree, yyA [4].StatementNoShortIf.tree);
 ;

} break;
case 655: yyDecrement (9) yySetNT (yyNTStatementNoShortIf) {
/* line 1217 "Parser.lrk" */
 yyS.StatementNoShortIf.tree = mfor_stmt (yyA [2].ForInit.tree, yyA [4].ExpressionOpt.tree, yyA [6].ForUpdate.tree, yyA [8].StatementNoShortIf.tree);
 ;

} break;
case 656: yyDecrement (1) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1220 "Parser.lrk" */
yyS.StatementWithoutTrailingSubstatement.tree = yyA [0].Block.tree;

} break;
case 657:
case 460: yyDecrement (1) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1222 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mnull_stmt ();
 ;

} break;
case 658:
case 461: yyDecrement (2) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1225 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mexpression_stmt (yyA [0].StatementExpression.tree);
 ;

} break;
case 659: yyDecrement (5) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1228 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mswitch_stmt (yyA [2].Expression.tree, yyA [4].SwitchBlock.tree);
 ;

} break;
case 660:
case 462: yyDecrement (7) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1231 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mdo_stmt (yyA [1].BlockStatement.tree, yyA [4].Expression.tree);
 ;

} break;
case 661:
case 463: yyDecrement (2) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1234 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mbreak_stmt ();
 ;

} break;
case 662:
case 464: yyDecrement (3) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1237 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mbreak_id_stmt (mident (yyA [1].Scan.Position, yyA [1].Scan.Identifier.ident));
 ;

} break;
case 663:
case 465: yyDecrement (2) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1240 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mcontinue_stmt ();
 ;

} break;
case 664:
case 466: yyDecrement (3) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1243 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mcontinue_id_stmt (mident (yyA [1].Scan.Position, yyA [1].Scan.Identifier.ident));
 ;

} break;
case 665:
case 467: yyDecrement (2) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1246 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mreturn_stmt ();
 ;

} break;
case 666:
case 468: yyDecrement (3) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1249 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mreturn_expr_stmt (yyA [1].Expression.tree);
 ;

} break;
case 667:
case 469: yyDecrement (3) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1252 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mthrow_stmt (yyA [1].Expression.tree);
 ;

} break;
case 668: yyDecrement (5) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1255 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = msynchronized_stmt (yyA [2].Expression.tree, yyA [4].Block.tree);
 ;

} break;
case 669: yyDecrement (3) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1258 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mtry_stmt (yyA [1].Block.tree, ReverseTree (yyA [2].Catches.tree), mnull_stmt ());
 ;

} break;
case 670: yyDecrement (4) yySetNT (yyNTStatementWithoutTrailingSubstatement) {
/* line 1261 "Parser.lrk" */
 yyS.StatementWithoutTrailingSubstatement.tree = mtry_stmt (yyA [1].Block.tree, yyA [2].CatchesOpt.tree, yyA [3].Finally.tree);
 ;

} break;
case 671: yyDecrement (1) yySetNT (yyNTStatementExpression) {
/* line 1264 "Parser.lrk" */
yyS.StatementExpression.tree = yyA [0].Expression.tree;

} break;
case 672: yyDecrement (2) yySetNT (yyNTStatementExpression) {
/* line 1266 "Parser.lrk" */
 yyS.StatementExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, pre_incr);
 ;

} break;
case 673: yyDecrement (2) yySetNT (yyNTStatementExpression) {
/* line 1269 "Parser.lrk" */
 yyS.StatementExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, pre_decr);
 ;

} break;
case 674: yyDecrement (2) yySetNT (yyNTStatementExpression) {
/* line 1272 "Parser.lrk" */
 yyS.StatementExpression.tree = munary (yyA [1].Scan.Position, yyA [0].cExpression.tree, post_incr);
 ;

} break;
case 675: yyDecrement (2) yySetNT (yyNTStatementExpression) {
/* line 1275 "Parser.lrk" */
 yyS.StatementExpression.tree = munary (yyA [1].Scan.Position, yyA [0].cExpression.tree, post_decr);
 ;

} break;
case 676: yyDecrement (1) yySetNT (yyNTStatementExpression) {
/* line 1278 "Parser.lrk" */
yyS.StatementExpression.tree = yyA [0].Primary.tree;

} break;
case 677: yyDecrement (1) yySetNT (yyNTStatementExpression) {
/* line 1280 "Parser.lrk" */
yyS.StatementExpression.tree = yyA [0].Primary.tree;

} break;
case 678:
case 470: yyDecrement (2) yySetNT (yyNTSwitchBlock) {
/* line 1282 "Parser.lrk" */
 yyS.SwitchBlock.tree = dnoswitch;
 ;

} break;
case 679:
case 471: yyDecrement (3) yySetNT (yyNTSwitchBlock) {
/* line 1285 "Parser.lrk" */
 yyS.SwitchBlock.tree = ReverseTree (yyA [1].SwitchBlockStatementGroups.tree);
 ;

} break;
case 680:
case 472: yyDecrement (3) yySetNT (yyNTSwitchBlock) {
/* line 1288 "Parser.lrk" */
 yyS.SwitchBlock.tree = ReverseTree (mswitch_ (dnoswitch,
			ReverseTree (yyA [1].SwitchLabels.tree), dnostatement));
 ;

} break;
case 681:
case 473: yyDecrement (4) yySetNT (yyNTSwitchBlock) {
/* line 1292 "Parser.lrk" */
 yyS.SwitchBlock.tree = ReverseTree (mswitch_ (yyA [1].SwitchBlockStatementGroups.tree,
			ReverseTree (yyA [2].SwitchLabels.tree), dnostatement));
 ;

} break;
case 682: yyDecrement (1) yySetNT (yyNTSwitchBlockStatementGroups) {
/* line 1296 "Parser.lrk" */
 yyS.SwitchBlockStatementGroups.tree = yyA [0].SwitchBlockStatementGroup.tree;
			yyS.SwitchBlockStatementGroups.tree->switch_.next = dnoswitch; ;
 ;

} break;
case 683: yyDecrement (2) yySetNT (yyNTSwitchBlockStatementGroups) {
/* line 1300 "Parser.lrk" */
 yyS.SwitchBlockStatementGroups.tree = yyA [1].SwitchBlockStatementGroup.tree;
		    yyS.SwitchBlockStatementGroups.tree->switch_.next = yyA [0].SwitchBlockStatementGroups.tree; ;
 ;

} break;
case 684: yyDecrement (2) yySetNT (yyNTSwitchBlockStatementGroup) {
/* line 1304 "Parser.lrk" */
 yyS.SwitchBlockStatementGroup.tree = mswitch_ (NoTree, ReverseTree (yyA [0].SwitchLabels.tree), ReverseTree (yyA [1].BlockStatements.tree));
 ;

} break;
case 685: yyDecrement (1) yySetNT (yyNTSwitchLabels) {
/* line 1307 "Parser.lrk" */
 yyS.SwitchLabels.tree = mexpression (dnoexpression_l, yyA [0].SwitchLabel.tree);
 ;

} break;
case 686: yyDecrement (2) yySetNT (yyNTSwitchLabels) {
/* line 1310 "Parser.lrk" */
 yyS.SwitchLabels.tree = mexpression (yyA [0].SwitchLabels.tree, yyA [1].SwitchLabel.tree);
 ;

} break;
case 687:
case 474: yyDecrement (3) yySetNT (yyNTSwitchLabel) {
/* line 1313 "Parser.lrk" */
yyS.SwitchLabel.tree = yyA [1].Expression.tree;

} break;
case 688:
case 475: yyDecrement (2) yySetNT (yyNTSwitchLabel) {
/* line 1315 "Parser.lrk" */
 yyS.SwitchLabel.tree = dnoexpression;
 ;

} break;
case 689: yySetNT (yyNTForInit) {
/* line 1318 "Parser.lrk" */
 yyS.ForInit.tree = dnostatement;
 ;

} break;
case 690: yyDecrement (1) yySetNT (yyNTForInit) {
/* line 1321 "Parser.lrk" */
 yyS.ForInit.tree = ReverseTree (yyA [0].StatementExpressionList.tree);
 ;

} break;
case 691: yyDecrement (1) yySetNT (yyNTForInit) {
/* line 1324 "Parser.lrk" */
 yyS.ForInit.tree = mstatement (dnostatement, yyA [0].LocalVariableDeclaration.tree);
 ;

} break;
case 692: yySetNT (yyNTExpressionOpt) {
/* line 1327 "Parser.lrk" */
 yyS.ExpressionOpt.tree = dnoexpression;
 ;

} break;
case 693: yyDecrement (1) yySetNT (yyNTExpressionOpt) {
/* line 1330 "Parser.lrk" */
yyS.ExpressionOpt.tree = yyA [0].Expression.tree;

} break;
case 694: yySetNT (yyNTForUpdate) {
/* line 1332 "Parser.lrk" */
 yyS.ForUpdate.tree = dnostatement;
 ;

} break;
case 695: yyDecrement (1) yySetNT (yyNTForUpdate) {
/* line 1335 "Parser.lrk" */
 yyS.ForUpdate.tree = ReverseTree (yyA [0].StatementExpressionList.tree);
 ;

} break;
case 696: yyDecrement (1) yySetNT (yyNTStatementExpressionList) {
/* line 1338 "Parser.lrk" */
 yyS.StatementExpressionList.tree = mstatement (dnostatement,
			mexpression_stmt (yyA [0].StatementExpression.tree));
 ;

} break;
case 697: yyDecrement (3) yySetNT (yyNTStatementExpressionList) {
/* line 1342 "Parser.lrk" */
 yyS.StatementExpressionList.tree = mstatement (yyA [0].StatementExpressionList.tree,
			mexpression_stmt (yyA [2].StatementExpression.tree));
 ;

} break;
case 698: yySetNT (yyNTCatchesOpt) {
/* line 1346 "Parser.lrk" */
 yyS.CatchesOpt.tree = dnocatch;
 ;

} break;
case 699: yyDecrement (1) yySetNT (yyNTCatchesOpt) {
/* line 1349 "Parser.lrk" */
 yyS.CatchesOpt.tree = ReverseTree (yyA [0].Catches.tree);
 ;

} break;
case 700: yyDecrement (1) yySetNT (yyNTCatches) {
/* line 1352 "Parser.lrk" */
 yyS.Catches.tree = yyA [0].CatchClause.tree;
			yyS.Catches.tree->catch.next = dnocatch; ;
 ;

} break;
case 701: yyDecrement (2) yySetNT (yyNTCatches) {
/* line 1356 "Parser.lrk" */
 yyS.Catches.tree = yyA [1].CatchClause.tree;
			yyS.Catches.tree->catch.next = yyA [0].Catches.tree; ;
 ;

} break;
case 702: yyDecrement (5) yySetNT (yyNTCatchClause) {
/* line 1360 "Parser.lrk" */
 yyA [2].FormalParameter.tree->parameter.next = dnodecl;
			yyS.CatchClause.tree = mcatch (NoTree, yyA [2].FormalParameter.tree, yyA [4].Block.tree); ;
 ;

} break;
case 703: yyDecrement (2) yySetNT (yyNTFinally) {
/* line 1364 "Parser.lrk" */
yyS.Finally.tree = yyA [1].Block.tree;

} break;
case 704: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1366 "Parser.lrk" */
 yyS.Primary.tree = mint_literal (yyA [0].Scan.Position, yyA [0].Scan.IntegerLiteral.value);
 ;

} break;
case 705: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1369 "Parser.lrk" */
 yyS.Primary.tree = mlong_literal (yyA [0].Scan.Position, yyA [0].Scan.LongLiteral.value);
 ;

} break;
case 706: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1372 "Parser.lrk" */
 yyS.Primary.tree = mfloat_literal (yyA [0].Scan.Position, yyA [0].Scan.FloatingPointLiteral.value);
 ;

} break;
case 707: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1375 "Parser.lrk" */
 yyS.Primary.tree = mfloat_literal (yyA [0].Scan.Position, yyA [0].Scan.DoubleLiteral.value);
 ;

} break;
case 708: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1378 "Parser.lrk" */
 yyS.Primary.tree = mchar_literal (yyA [0].Scan.Position, yyA [0].Scan.CharacterLiteral.value);
 ;

} break;
case 709: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1381 "Parser.lrk" */
 yyS.Primary.tree = mstring_literal (yyA [0].Scan.Position, yyA [0].Scan.StringLiteral.value);
 ;

} break;
case 710: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1384 "Parser.lrk" */
 yyS.Primary.tree = mbool_literal (yyA [0].Scan.Position, rtrue);
 ;

} break;
case 711: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1387 "Parser.lrk" */
 yyS.Primary.tree = mbool_literal (yyA [0].Scan.Position, rfalse);
 ;

} break;
case 712: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1390 "Parser.lrk" */
 yyS.Primary.tree = mnull (yyA [0].Scan.Position);
 ;

} break;
case 713: yyDecrement (1) yySetNT (yyNTPrimary) {
/* line 1393 "Parser.lrk" */
 yyS.Primary.tree = mthis (yyA [0].Scan.Position);
 ;

} break;
case 714: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1396 "Parser.lrk" */
yyS.Primary.tree = yyA [1].Expression.tree;

} break;
case 715: yyDecrement (5) yySetNT (yyNTPrimary) {
/* line 1398 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassType.tree, dnotype,
			yyA [3].ArgumentListOpt.tree, dnoexpression);
 ;

} break;
case 716: yyDecrement (7) yySetNT (yyNTPrimary) {
/* line 1402 "Parser.lrk" */
 yyS.Primary = yyA [5].Primary;
 yyS.Primary.tree = manonymous (yyA [0].Scan.Position, yyA [1].ClassType.tree,
			yyA [3].ArgumentListOpt.tree, yyA [6].ClassBody.tree);

		  ;
{  if (hl >= 0) pop (); } ;

} break;
case 717: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1412 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident);
 ;

} break;
case 718: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1415 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident);
 ;

} break;
case 719: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1418 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, yyA [0].Name.tree);
 ;

} break;
case 720: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1421 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position));
 ;

} break;
case 721: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1424 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, yyA [0].PrimitiveType.tree);
 ;

} break;
case 722: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1427 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, msimple_type (t_void));
 ;

} break;
case 723: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1430 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, yyA [0].Name.tree);
 ;

} break;
case 724: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1433 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position));
 ;

} break;
case 725: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1436 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, yyA [0].PrimitiveType.tree);
 ;

} break;
case 726: yyDecrement (3) yySetNT (yyNTPrimary) {
/* line 1439 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, msimple_type (t_void));
 ;

} break;
case 727: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1442 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].ArgumentListOpt.tree);
 ;

} break;
case 728: yyDecrement (6) yySetNT (yyNTPrimary) {
/* line 1445 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 729: yyDecrement (6) yySetNT (yyNTPrimary) {
/* line 1449 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 730: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1453 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].Expression.tree);
 ;

} break;
case 731: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1456 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Primary.tree, yyA [2].Expression.tree);
 ;

} break;
case 732: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1459 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].PrimitiveType.tree,
			dnotype, ReverseTree (yyA [2].DimExprs.tree),
			yyA [3].ArrayInitializerOpt.tree);
 ;

} break;
case 733: yyDecrement (5) yySetNT (yyNTPrimary) {
/* line 1464 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].PrimitiveType.tree,
			yyA [3].Dims.tree, ReverseTree (yyA [2].DimExprs.tree),
			yyA [4].ArrayInitializerOpt.tree);
 ;

} break;
case 734: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1469 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].PrimitiveType.tree,
			yyA [2].Dims.tree, dnoexpression_l, yyA [3].VariableInitializer.tree);
 ;

} break;
case 735: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1473 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassOrInterfaceType.tree,
			dnotype, ReverseTree (yyA [2].DimExprs.tree),
			yyA [3].ArrayInitializerOpt.tree);
 ;

} break;
case 736: yyDecrement (5) yySetNT (yyNTPrimary) {
/* line 1478 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassOrInterfaceType.tree,
			yyA [3].Dims.tree, ReverseTree (yyA [2].DimExprs.tree),
			yyA [4].ArrayInitializerOpt.tree);
 ;

} break;
case 737: yyDecrement (4) yySetNT (yyNTPrimary) {
/* line 1483 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassOrInterfaceType.tree,
			yyA [2].Dims.tree, dnoexpression_l, yyA [3].VariableInitializer.tree);
 ;

} break;
case 738: yySetNT (yyNTxx_Primary_13_6) {
/* line 1409 "Parser.lrk" */
 ;
{  if (hl >= 0) push (i_anonymous); } ;

} break;
case 739: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1487 "Parser.lrk" */
 yyS.Primary.tree = mint_literal (yyA [0].Scan.Position, yyA [0].Scan.IntegerLiteral.value);
 ;

} break;
case 740: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1490 "Parser.lrk" */
 yyS.Primary.tree = mlong_literal (yyA [0].Scan.Position, yyA [0].Scan.LongLiteral.value);
 ;

} break;
case 741: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1493 "Parser.lrk" */
 yyS.Primary.tree = mfloat_literal (yyA [0].Scan.Position, yyA [0].Scan.FloatingPointLiteral.value);
 ;

} break;
case 742: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1496 "Parser.lrk" */
 yyS.Primary.tree = mfloat_literal (yyA [0].Scan.Position, yyA [0].Scan.DoubleLiteral.value);
 ;

} break;
case 743: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1499 "Parser.lrk" */
 yyS.Primary.tree = mchar_literal (yyA [0].Scan.Position, yyA [0].Scan.CharacterLiteral.value);
 ;

} break;
case 744: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1502 "Parser.lrk" */
 yyS.Primary.tree = mstring_literal (yyA [0].Scan.Position, yyA [0].Scan.StringLiteral.value);
 ;

} break;
case 745: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1505 "Parser.lrk" */
 yyS.Primary.tree = mbool_literal (yyA [0].Scan.Position, rtrue);
 ;

} break;
case 746: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1508 "Parser.lrk" */
 yyS.Primary.tree = mbool_literal (yyA [0].Scan.Position, rfalse);
 ;

} break;
case 747: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1511 "Parser.lrk" */
 yyS.Primary.tree = mnull (yyA [0].Scan.Position);
 ;

} break;
case 748: yyDecrement (1) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1514 "Parser.lrk" */
 yyS.Primary.tree = mthis (yyA [0].Scan.Position);
 ;

} break;
case 749: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1517 "Parser.lrk" */
yyS.Primary.tree = yyA [1].Expression.tree;

} break;
case 750: yyDecrement (5) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1519 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassType.tree, dnotype,
			yyA [3].ArgumentListOpt.tree, dnoexpression);
 ;

} break;
case 751: yyDecrement (7) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1523 "Parser.lrk" */
 yyS.Primary = yyA [5].Primary;
 yyS.Primary.tree = manonymous (yyA [0].Scan.Position, yyA [1].ClassType.tree,
			yyA [3].ArgumentListOpt.tree, yyA [6].ClassBody.tree);

		  ;
{  if (hl >= 0) pop (); } ;

} break;
case 752: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1530 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident);
 ;

} break;
case 753: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1533 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident);
 ;

} break;
case 754: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1536 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, yyA [0].Name.tree);
 ;

} break;
case 755: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1539 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position));
 ;

} break;
case 756: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1542 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, yyA [0].PrimitiveType.tree);
 ;

} break;
case 757: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1545 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, msimple_type (t_void));
 ;

} break;
case 758: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1548 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, yyA [0].Name.tree);
 ;

} break;
case 759: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1551 "Parser.lrk" */
 yyS.Primary.tree = mget_class_of_expr (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position));
 ;

} break;
case 760: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1554 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, yyA [0].PrimitiveType.tree);
 ;

} break;
case 761: yyDecrement (3) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1557 "Parser.lrk" */
 yyS.Primary.tree = mget_class (yyA [2].Scan.Position, msimple_type (t_void));
 ;

} break;
case 762: yyDecrement (4) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1560 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].ArgumentListOpt.tree);
 ;

} break;
case 763: yyDecrement (6) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1563 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 764: yyDecrement (6) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1567 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 765: yyDecrement (4) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1571 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].Expression.tree);
 ;

} break;
case 766: yyDecrement (4) yySetNT (yyNTPrimaryNoNewArray) {
/* line 1574 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Primary.tree, yyA [2].Expression.tree);
 ;

} break;
case 767: yyDecrement (5) yySetNT (yyNTClassInstanceCreationExpression) {
/* line 1577 "Parser.lrk" */
 yyS.Primary.tree = mnew (yyA [0].Scan.Position, yyA [1].ClassType.tree, dnotype,
			yyA [3].ArgumentListOpt.tree, dnoexpression);
 ;

} break;
case 768: yyDecrement (3) yySetNT (yyNTFieldAccess) {
/* line 1581 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident);
 ;

} break;
case 769: yyDecrement (3) yySetNT (yyNTFieldAccess) {
/* line 1584 "Parser.lrk" */
 yyS.Primary.tree = mselect (yyA [2].Scan.Position, msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident);
 ;

} break;
case 770: yyDecrement (4) yySetNT (yyNTMethodInvocation) {
/* line 1587 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].ArgumentListOpt.tree);
 ;

} break;
case 771: yyDecrement (6) yySetNT (yyNTMethodInvocation) {
/* line 1590 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     yyA [0].Primary.tree, yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 772: yyDecrement (6) yySetNT (yyNTMethodInvocation) {
/* line 1594 "Parser.lrk" */
 yyS.Primary.tree = mcall (yyA [3].Scan.Position, mselect (yyA [2].Scan.Position,
		     msuper (yyA [0].Scan.Position), yyA [2].Scan.Identifier.ident), yyA [4].ArgumentListOpt.tree);
 ;

} break;
case 773: yyDecrement (4) yySetNT (yyNTArrayAccess) {
/* line 1598 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Name.tree, yyA [2].Expression.tree);
 ;

} break;
case 774: yyDecrement (4) yySetNT (yyNTArrayAccess) {
/* line 1601 "Parser.lrk" */
 yyS.Primary.tree = msubscript (yyA [1].Scan.Position, yyA [0].Primary.tree, yyA [2].Expression.tree);
 ;

} break;
case 775: yyDecrement (1) yySetNT (yyNTDimExprs) {
/* line 1604 "Parser.lrk" */
 yyS.DimExprs.tree = mexpression (dnoexpression_l, yyA [0].DimExpr.tree);
 ;

} break;
case 776: yyDecrement (2) yySetNT (yyNTDimExprs) {
/* line 1607 "Parser.lrk" */
 yyS.DimExprs.tree = mexpression (yyA [0].DimExprs.tree, yyA [1].DimExpr.tree);
 ;

} break;
case 777:
case 476: yyDecrement (3) yySetNT (yyNTDimExpr) {
/* line 1610 "Parser.lrk" */
yyS.DimExpr.tree = yyA [1].Expression.tree;

} break;
case 778:
case 477: yyDecrement (2) yySetNT (yyNTDims) {
/* line 1612 "Parser.lrk" */
 yyS.Dims.tree = marray_type (dnotype);
 ;

} break;
case 779:
case 478: yyDecrement (3) yySetNT (yyNTDims) {
/* line 1615 "Parser.lrk" */
 yyS.Dims.tree = marray_type (yyA [0].Dims.tree);
 ;

} break;
case 780: yySetNT (yyNTArgumentListOpt) {
/* line 1618 "Parser.lrk" */
 yyS.ArgumentListOpt.tree = dnoexpression_l;
 ;

} break;
case 781: yyDecrement (1) yySetNT (yyNTArgumentListOpt) {
/* line 1621 "Parser.lrk" */
 yyS.ArgumentListOpt.tree = ReverseTree (yyA [0].ArgumentList.tree);
 ;

} break;
case 782: yyDecrement (1) yySetNT (yyNTArgumentList) {
/* line 1624 "Parser.lrk" */
 yyS.ArgumentList.tree = mexpression (dnoexpression_l, yyA [0].Expression.tree);
 ;

} break;
case 783: yyDecrement (3) yySetNT (yyNTArgumentList) {
/* line 1627 "Parser.lrk" */
 yyS.ArgumentList.tree = mexpression (yyA [0].ArgumentList.tree, yyA [2].Expression.tree);
 ;

} break;
case 784: yyDecrement (1) yySetNT (yyNTcExpression) {
/* line 1630 "Parser.lrk" */
yyS.cExpression.tree = yyA [0].Primary.tree;

} break;
case 785: yyDecrement (1) yySetNT (yyNTcExpression) {
/* line 1632 "Parser.lrk" */
yyS.cExpression.tree = yyA [0].Name.tree;

} break;
case 786:
case 479: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1634 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [1].Scan.Position, yyA [0].cExpression.tree, post_incr);
 ;

} break;
case 787:
case 480: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1637 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [1].Scan.Position, yyA [0].cExpression.tree, post_decr);
 ;

} break;
case 788: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1640 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, pre_incr);
 ;

} break;
case 789: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1643 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, pre_decr);
 ;

} break;
case 790: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1646 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, plus);
 ;

} break;
case 791: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1649 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, negate);
 ;

} break;
case 792: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1652 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, complement);
 ;

} break;
case 793: yyDecrement (2) yySetNT (yyNTcExpression) {
/* line 1655 "Parser.lrk" */
 yyS.cExpression.tree = munary (yyA [0].Scan.Position, yyA [1].cExpression.tree, not);
 ;

} break;
case 794: yyDecrement (4) yySetNT (yyNTcExpression) {
/* line 1658 "Parser.lrk" */
 yyS.cExpression.tree = mtype_cast (yyA [0].Scan.Position, yyA [1].PrimitiveType.tree,
			dnotype, yyA [3].cExpression.tree);
 ;

} break;
case 795: yyDecrement (5) yySetNT (yyNTcExpression) {
/* line 1662 "Parser.lrk" */
 yyS.cExpression.tree = mtype_cast (yyA [0].Scan.Position, yyA [1].PrimitiveType.tree,
			yyA [2].Dims.tree, yyA [4].cExpression.tree);
 ;

} break;
case 796: yyDecrement (4) yySetNT (yyNTcExpression) {
/* line 1666 "Parser.lrk" */
 yyS.cExpression.tree = mtype_cast (yyA [0].Scan.Position, mnamed_type (yyA [1].Expression.tree),
			dnotype, yyA [3].cExpression.tree);
 ;

} break;
case 797: yyDecrement (5) yySetNT (yyNTcExpression) {
/* line 1670 "Parser.lrk" */
 yyS.cExpression.tree = mtype_cast (yyA [0].Scan.Position, mnamed_type (yyA [1].Name.tree),
			yyA [2].Dims.tree, yyA [4].cExpression.tree);
 ;

} break;
case 798: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1674 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, times);
 ;

} break;
case 799: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1677 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, divide);
 ;

} break;
case 800: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1680 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, modulo);
 ;

} break;
case 801: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1683 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, plus);
 ;

} break;
case 802: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1686 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, minus);
 ;

} break;
case 803: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1689 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, lshift);
 ;

} break;
case 804: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1692 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, rshift);
 ;

} break;
case 805: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1695 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, us_rshift);
 ;

} break;
case 806: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1698 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, less);
 ;

} break;
case 807: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1701 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, greater);
 ;

} break;
case 808: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1704 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, less_equal);
 ;

} break;
case 809: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1707 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, greater_equal);
 ;

} break;
case 810: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1710 "Parser.lrk" */
 yyS.cExpression.tree = mtype_compare (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].ReferenceType.tree);
 ;

} break;
case 811: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1713 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, equal);
 ;

} break;
case 812: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1716 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, not_equal);
 ;

} break;
case 813: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1719 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, and);
 ;

} break;
case 814: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1722 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, xor);
 ;

} break;
case 815: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1725 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, or);
 ;

} break;
case 816: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1728 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, and_and);
 ;

} break;
case 817: yyDecrement (3) yySetNT (yyNTcExpression) {
/* line 1731 "Parser.lrk" */
 yyS.cExpression.tree = mbinary (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].cExpression.tree, or_or);
 ;

} break;
case 818: yyDecrement (5) yySetNT (yyNTcExpression) {
/* line 1734 "Parser.lrk" */
 yyS.cExpression.tree = mconditional (yyA [1].Scan.Position, yyA [0].cExpression.tree, yyA [2].Expression.tree, yyA [4].cExpression.tree);
 ;

} break;
case 819: yyDecrement (1) yySetNT (yyNTExpression) {
/* line 1737 "Parser.lrk" */
yyS.Expression.tree = yyA [0].cExpression.tree;

} break;
case 820: yyDecrement (3) yySetNT (yyNTExpression) {
/* line 1739 "Parser.lrk" */
 yyS.Expression.tree = massign (yyA [1].AssignmentOperator.Position, yyA [0].LeftHandSide.tree,
			yyA [2].Expression.tree, yyA [1].AssignmentOperator.operator);
 ;

} break;
case 821: yyDecrement (3) yySetNT (yyNTAssignment) {
/* line 1743 "Parser.lrk" */
 yyS.Expression.tree = massign (yyA [1].AssignmentOperator.Position, yyA [0].LeftHandSide.tree,
			yyA [2].Expression.tree, yyA [1].AssignmentOperator.operator);
 ;

} break;
case 822: yyDecrement (1) yySetNT (yyNTLeftHandSide) {
/* line 1747 "Parser.lrk" */
yyS.LeftHandSide.tree = yyA [0].Name.tree;

} break;
case 823: yyDecrement (1) yySetNT (yyNTLeftHandSide) {
/* line 1749 "Parser.lrk" */
yyS.LeftHandSide.tree = yyA [0].Primary.tree;

} break;
case 824: yyDecrement (1) yySetNT (yyNTLeftHandSide) {
/* line 1751 "Parser.lrk" */
yyS.LeftHandSide.tree = yyA [0].Primary.tree;

} break;
case 825:
case 481: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1753 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = assignment;
 ;

} break;
case 826:
case 482: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1757 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = times	;
 ;

} break;
case 827:
case 483: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1761 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = divide	;
 ;

} break;
case 828:
case 484: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1765 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = modulo	;
 ;

} break;
case 829:
case 485: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1769 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = plus	;
 ;

} break;
case 830:
case 486: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1773 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = minus	;
 ;

} break;
case 831:
case 487: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1777 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = lshift	;
 ;

} break;
case 832:
case 488: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1781 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = rshift	;
 ;

} break;
case 833:
case 489: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1785 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = us_rshift	;
 ;

} break;
case 834:
case 490: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1789 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = and	;
 ;

} break;
case 835:
case 491: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1793 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = xor	;
 ;

} break;
case 836:
case 492: yyDecrement (1) yySetNT (yyNTAssignmentOperator) {
/* line 1797 "Parser.lrk" */
yyS.AssignmentOperator.Position = yyA [0].Scan.Position;
 yyS.AssignmentOperator.operator = or	;
 ;

} break;
default: switch (yyState) {
case 1: goto yyAbort;
case 2: goto yyRead;
case 3: goto yyReduce;
}
}

       /* SPEC State = Next (Top (), Nonterminal); nonterminal transition */
#ifdef YYNDefault
	       yyState = * yyStateStackPtr ++;
	       for (;;) {
		  register yytComb * yyNCombPtr =
				yyNBasePtr [yyState] + (int) yyNonterminal;
		  if (yyNCombPtr->Check == yyState) {
		     yyState = yyNCombPtr->Next; break;
		  }
		  yyState = yyNDefault [yyState];
	       }
#else
	       yyState = yyNBasePtr [* yyStateStackPtr ++] [yyNonterminal];
#endif
	       * yyAttrStackPtr ++ = yySynAttribute;
	       if (yyState < yyFirstFinalState) goto yyParseLoop;
							/* read reduce ? */
#ifdef YYDEBUG
	       yyStateStackPtr [0] = yyStateStackPtr [-1];
#endif
	    }

	 } else {					/* read */
   yyRead:  yyStateStackPtr ++;
	    yyGetAttribute (yyAttrStackPtr ++, Attribute);
	    yyTerminal = yyGetToken ();
#ifdef YYDEBUG
	    if (Parser_Debug) {
	       yyPrintState (yyStateStackPtr [-1]);
	       (void) fprintf (yyTrace, "shift   %s, lookahead: %s",
		  Parser_TokenName [yyPrevTerminal],
		  Parser_TokenName [yyTerminal]); yyNl ();
	       yyPrevTerminal = yyTerminal;
	    }
#endif
	    yyIsRepairing = rfalse;
	 }
      }

   yyAbort:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "fail    parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return ++ yyErrorCount;

   yyAccept:
#ifdef YYDEBUG
      if (Parser_Debug) {
	 yyPrintState (* yyStateStackPtr);
	 (void) fprintf (yyTrace, "accept  parse started at %ld", yyStartCount);
	 yyNl ();
      }
#endif
      return yyErrorCount;
   }

#ifndef NO_RECOVER
static void yyErrorRecovery
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange * yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange *	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
#endif
   {
#define	yyContinueSize	5000
      rbool	yyTokensSkipped	;
      tSet	yyContinueSet	;
      tSet	yyRestartSet	;
      int	yyLength	;
      char	yyContinueString [yyContinueSize + 2];

      if (yyControl.yyMessages) {
   /* 1. report an error */
	 ErrorMessage (xxSyntaxError, xxError, Attribute.Position);

   /* 2. report the offending token */
	 (void) strcpy (yyContinueString, Parser_TokenName [* yyTerminal]);
#ifdef SPELLING
	 if (strncmp (yyContinueString, TokenPtr, TokenLength)) {
	    yyContinueString [yyLength = strlen (yyContinueString)] = ' ';
	    (void) GetWord (& yyContinueString [++ yyLength]);
	 }
#endif
	 ErrorMessageI (xxTokenFound, xxInformation, Attribute.Position,
	    xxString, yyContinueString);

   /* 3. report the set of expected terminal symbols */
	 MakeSet (& yyContinueSet, (short) yyLastTerminal);
	 yyComputeContinuation (yyStateStack, yyStackPtr, & yyContinueSet);
	 yyLength = 0;
	 yyContinueString [0] = '\0';
	 while (! IsEmpty (& yyContinueSet)) {
	    char * yyTokenString = Parser_TokenName [Extract (& yyContinueSet)];
	    int yyl = strlen (yyTokenString);
	    if (yyLength + yyl >= yyContinueSize) break;
	    (void) strcpy (& yyContinueString [yyLength], yyTokenString);
	    yyLength += yyl;
	    yyContinueString [yyLength ++] = ' ';
	 }
	 yyContinueString [-- yyLength] = '\0';
	 ErrorMessageI (xxExpectedTokens, xxInformation, Attribute.Position,
	    xxString, yyContinueString);
	 ReleaseSet (& yyContinueSet);
      }

   /* 4. compute the set of terminal symbols for restart of the parse */
      MakeSet (& yyRestartSet, (short) yyLastTerminal);
      yyComputeRestartPoints (yyStateStack, yyStackPtr, & yyRestartSet);

   /* 5. skip terminal symbols until a restart point is reached */
      yyTokensSkipped = rfalse;
      while (! IsElement (* yyTerminal, & yyRestartSet)) {
#ifdef YYDEBUG
	 yySymbolRange yyPrevTerminal = * yyTerminal;
#endif
	 * yyTerminal = yyGetToken ();
	 yyTokensSkipped = rtrue;
#ifdef YYDEBUG
	 if (Parser_Debug) {
	    yyPrintState (yyStateStack [yyStackPtr]);
	    (void) fprintf (yyTrace, "skip    %s, lookahead: %s",
	       Parser_TokenName [yyPrevTerminal],
	       Parser_TokenName [* yyTerminal]); yyNl ();
	 }
#endif
      }
      ReleaseSet (& yyRestartSet);

   /* 6. report the restart point */
      if (yyTokensSkipped & yyControl.yyMessages)
	 ErrorMessage (xxRestartPoint, xxInformation, Attribute.Position);
   }

/*
   compute the set of terminal symbols that can be accepted (read)
   in a given stack configuration (eventually after reduce actions)
*/

static void yyComputeContinuation
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStack, short yyStackPtr, tSet * yyContinueSet)
#else
   (yyStack, yyStackPtr, yyContinueSet)
   yyStateRange *	yyStack		;
   short		yyStackPtr	;
   tSet *		yyContinueSet	;
#endif
   {
      register yySymbolRange	yyTerminal;
      register yyStateRange	yyState = yyStack [yyStackPtr];

      AssignEmpty (yyContinueSet);
      for (yyTerminal = yyFirstTerminal; yyTerminal <= yyLastTerminal;
							yyTerminal ++) {
	 if (yyNext (yyState, yyTerminal) != yyNoState &&
	    yyIsContinuation (yyTerminal, yyStack, yyStackPtr)) {
	    Include (yyContinueSet, (short) yyTerminal);
	 }
      }
   }

/*
   check whether a given terminal symbol can be accepted (read)
   in a certain stack configuration (eventually after reduce actions)
*/

static rbool yyIsContinuation
#if defined __STDC__ | defined __cplusplus
   (yySymbolRange yyTerminal, yyStateRange * yyStateStack, short yyStackPtr)
#else
   (yyTerminal, yyStateStack, yyStackPtr)
   yySymbolRange	yyTerminal	;
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;

      while (yyStackPtr >= (short) yyIsContStackSize)  /* pass Stack by value */
	 ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyIsContStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyIsContStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      yState = yyIsContStackPtr [yyStackPtr];
      for (;;) {
	 yyIsContStackPtr [yyStackPtr] = yState;
	 yState = yyNext (yState, yyTerminal);
	 if (yState == yyNoState) return rfalse;

	 do {						/* reduce */
	    if (yState > yyLastReduceState) {		/* dynamic ? */
	       yState = yyCondition [yState - yyLastReduceState];
	    }
	    if (yState <= yyLastStopState) { /* read, read reduce, or accept? */
	       return rtrue;
	    } else {					/* reduce */
	       yyStackPtr -= yyLength [yState - yyFirstReduceState];
	       yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	    }

	    yState = yyNext (yyIsContStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	    if (yyStackPtr >= (short) yyIsContStackSize)
	       ExtendArray ((char * *) & yyIsContStackPtr, & yyIsContStackSize,
				(unsigned long) sizeof (yyStateRange));
	    yyStackPtr ++;
	 } while (yState >= yyFirstFinalState);
      }
   }

/*
   compute a set of terminal symbols that can be used to restart
   parsing in a given stack configuration. we simulate parsing until
   end of file using a suffix program synthesized by the function (array)
   yyContinuation. All symbols acceptable in the states reached during
   the simulation can be used to restart parsing.
*/

static void yyComputeRestartPoints
#if defined __STDC__ | defined __cplusplus
   (yyStateRange * yyStateStack, short yyStackPtr, tSet * yyRestartSet)
#else
   (yyStateStack, yyStackPtr, yyRestartSet)
   yyStateRange *	yyStateStack	;
   short		yyStackPtr	;
   tSet *		yyRestartSet	;
#endif
   {
      register yyStateRange	yState		;
      register yytNonterminal	yyNonterminal	;
	       tSet		yyContinueSet	;

      while (yyStackPtr >= (short) yyCompResStackSize) /* pass Stack by value */
	 ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			  (unsigned long) sizeof (yyStateRange));
#ifdef BCOPY
      bcopy ((char *) yyStateStack, (char *) yyCompResStackPtr,
		 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#else
      (void) memcpy ((char *) yyCompResStackPtr, (char *) yyStateStack,
			 (int) sizeof (yyStateRange) * (yyStackPtr + 1));
#endif

      MakeSet (& yyContinueSet, (short) yyLastTerminal);
      AssignEmpty (yyRestartSet);
      yState = yyCompResStackPtr [yyStackPtr];

      for (;;) {
	 if (yyStackPtr >= (short) yyCompResStackSize)
	    ExtendArray ((char * *) & yyCompResStackPtr, & yyCompResStackSize,
			     (unsigned long) sizeof (yyStateRange));
	 yyCompResStackPtr [yyStackPtr] = yState;
	 yyComputeContinuation (yyCompResStackPtr, yyStackPtr, & yyContinueSet);
	 Union (yyRestartSet, & yyContinueSet);
	 yState = yyNext (yState, yyContinuation [yState]);

	 if (yState >= yyFirstFinalState) {		/* final state ? */
	    if (yState <= yyLastReadReduceState) {	/* read reduce ? */
	       yyStackPtr ++;
	       yState = yyFinalToProd [yState - yyFirstReadReduceState];
#ifdef YYDCRP
	       yyCompResStackPtr [yyStackPtr] =
					yyCompResStackPtr [yyStackPtr - 1];
	       (void) fprintf (yyTrace, "%5d shift   %s\n",
		  yyCompResStackPtr [yyStackPtr],
		  Parser_TokenName [yyContinuation [yyCompResStackPtr
		  [yyStackPtr]]]);
#endif
	    }

	    do {					/* reduce */
	       if (yState > yyLastReduceState) {	/* dynamic ? */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d dynamic decision %d\n",
		  yyCompResStackPtr [yyStackPtr], yState - yyLastReduceState);
#endif
		  yState = yyCondition [yState - yyLastReduceState];
	       }
	       if (yyFirstReduceState <= yState &&
		   yState <= yyLastStopState) {		/* accept */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d accept\n",
		     yyCompResStackPtr [yyStackPtr]);
#endif
		  ReleaseSet (& yyContinueSet);
		  return;
	       } else if (yState < yyFirstFinalState) {	/* read */
		  goto yyRead;
	       } else {					/* reduce */
#ifdef YYDCRP
		  (void) fprintf (yyTrace, "%5d reduce  %s\n",
		     yyCompResStackPtr [yyStackPtr],
		     yyRule [yState - yyLastReadReduceState]);
#endif
		  yyStackPtr -= yyLength [yState - yyFirstReduceState];
		  yyNonterminal = yyLeftHandSide [yState - yyFirstReduceState];
	       }

	       yState = yyNext (yyCompResStackPtr [yyStackPtr],
				(yySymbolRange) yyNonterminal);
	       yyStackPtr ++;
	    } while (yState >= yyFirstFinalState);
	 } else {					/* read */
yyRead:
#ifdef YYDCRP
	    (void) fprintf (yyTrace, "%5d shift   %s\n",
	       yyCompResStackPtr [yyStackPtr],
	       Parser_TokenName [yyContinuation [yyCompResStackPtr [yyStackPtr]]]);
#endif
	    yyStackPtr ++;
	 }
      }
   }

/* access the parse table:   Next : State x Symbol -> Action */

static yyStateRange yyNext
#if defined __STDC__ | defined __cplusplus
   (yyStateRange yyState, yySymbolRange yySymbol)
#else
   (yyState, yySymbol) yyStateRange yyState; yySymbolRange yySymbol;
#endif
   {
      if (yySymbol <= yyLastTerminal) {
	 for (;;) {
	    register yytComb * yyTCombPtr = yyTBasePtr [yyState] + yySymbol;
#if defined YYTDefault & defined YYaccDefault
	    register unsigned long * yylp;
#endif
	    if (yyTCombPtr->Check == yyState) return yyTCombPtr->Next;
#ifdef YYTDefault
#ifdef YYaccDefault
	    return (yylp = yyDefaultLook [yyState]) &&
	       (yylp [yySymbol >> 5] >> (yySymbol & 0x1f)) & 1 ?
		  yyTDefault [yyState] : yyNoState;
#else
	    if ((yyState = yyTDefault [yyState]) == yyNoState) return yyNoState;
#endif
#else
	    return yyNoState;
#endif
	 }
      }
#ifdef YYNDefault
      for (;;) {
	 register yytComb * yyNCombPtr = yyNBasePtr [yyState] + yySymbol;
	 if (yyNCombPtr->Check == yyState) return yyNCombPtr->Next;
	 yyState = yyNDefault [yyState];
      }
#else
      return yyNBasePtr [yyState] [yySymbol];
#endif
   }
#endif

void BeginParser ARGS ((void))
   {
/* line 367 "Parser.lrk" */


   i_anonymous = MakeIdent (s_anonymous, strlen (s_anonymous));

#ifdef SHARE

   snocatch		= mnocatch ();
   snodecl		= mnodecl ();
   snoexpression	= mnoexpression (NoPosition);
   snoexpression_l	= mnoexpression_l ();
   snofield		= mnofield ();
   snoimport		= mnoimport ();
   snostatement		= mnostatement ();
   snoswitch		= mnoswitch ();
   snotype		= mnotype ();
   snotype_name		= mnotype_name ();

#endif


   }

void CloseParser ARGS ((void))
   {
/* line 388 "Parser.lrk" */


   }


