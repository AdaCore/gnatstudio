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

#include "cpdefines.h"
#include "sn.h"
#include "symbol.h"
#include "cplex.h"

#if VMFL
#include <vmalloc.h>
#endif /* VMFL */

#define False 0
#define True (!False)

#define LEVEL_1 1
#define LEVEL_2 2

#define d_ElemFirst( x ) ((x)==0?0:(x)->ElemFirst)

#define step(i)       (iva+=(i))

#define CONSTRUCTOR       (1<<0)
#define DESTRUCTOR        (1<<1)

#define DECLARATION_CHECK 955
#define DECLARATOR_CHECK  956
#define CLASS_CHECK       957
#define ENUM_CHECK        958
#define EXPR_CHECK        959
#define TYPE_CHECK        960
#define OPER_CHECK        961
#define BASE_CHECK        962
#define NAME_CHECK        963
#define LIST_CHECK        964
#define INIT_CHECK        965
#define MEMBER_CHECK      966
#define ENUMERATOR_CHECK  967


#define MACRO_USE
#ifdef MACRO_USE

/* #define token(i)      ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+(i))%ZZSIZE].yychar) */
/* #define ident(i)      (v[(iva+i)%ZZSIZE].yytext) */
/* #define identlen(i)   (v[(iva+i)%ZZSIZE].yyleng) */
/* #define ident_last()  (v[(ivt-1)%ZZSIZE].yytext) */
/* #define f_lineno(i)       (v[(iva+i)%ZZSIZE].yylineno) */
/* #define f_lineno_last()   (v[(ivt-1)%ZZSIZE].yylineno) */
/* #define f_charno(i)       (v[(iva+i)%ZZSIZE].yycharno) */

#define token(i) ((ivt<=iva+(i)?f_read(i),0:0), keyw_cpp ? v[(iva+(i))&ZZMASK].yytoken_cpp : v[(iva+(i))&ZZMASK].yytoken )
/* #define ident(i)      ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yytext) */
#define identleng(i)  ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yytext.leng)
#define identlen(i)  ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yytext.leng)
#define f_lineno(i)   ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yylineno)
#define f_lineno_last()   (v[(ivt-1)&ZZMASK].yylineno)
#define f_charno(i)   ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yycharno)

#else

extern int token( int i );
extern int f_lineno( int i );
extern int f_charno( int i );

#endif

extern char *ident( int i );

#define DECLARATION_UNKNOWN                    0
#define DECLARATION_ASM                        1
#define DECLARATION_TEMPLATE                   2
#define DECLARATION_LINKAGE_SPECIFICATION      3
#define DECLARATION_NAMESPACE_DEFINITION       4
#define DECLARATION_NAMESPACE_ALIAS_DEFINITION 5
#define DECLARATION_USING_DIRECTIVE            6
#define DECLARATION_USING                      7
#define DECLARATION_OBJECT                     8
#define DECLARATION_MEMBER                     9
#define DECLARATION_ARGUMENT                  10

#define LBRACE '{'
#define RBRACE '}'

#define FUNCTION          1
#define POINTER           2
#define ARRAY             3
#define POINTER_STAR      4
#define POINTER_AMPERSAND 5

#define Save()      int iva_save = iva
#define Restore()   iva = iva_save

#define Save2()      int iva_save2 = iva
#define Restore2()   iva = iva_save2

typedef struct sDeclaration        sDeclaration_t       , *Declaration_t       ;
typedef struct sDeclarationSpecial sDeclarationSpecial_t, *DeclarationSpecial_t;

typedef struct sDeclarator  sDeclarator_t , *Declarator_t ;
typedef struct sClass       sClass_t      , *Class_t      ;
typedef struct sEnum        sEnum_t       , *Enum_t       ;
typedef struct sComp        sComp_t       , *Comp_t       ;
typedef struct sExpr        sExpr_t       , *Expr_t       ;
typedef struct sType        sType_t       , *Type_t       ;
typedef struct sOper        sOper_t       , *Oper_t       ;
typedef struct sBase        sBase_t       , *Base_t       ;
typedef struct sName        sName_t       , *Name_t       ;
typedef struct sList        sList_t       , *List_t       ;
typedef struct sElem        sElem_t       , *Elem_t       ;
typedef struct sInit        sInit_t       , *Init_t       ;
typedef struct sMember      sMember_t     , *Member_t     ;
typedef struct sEnumerator  sEnumerator_t , *Enumerator_t ;

typedef int Boolean_t;

struct sDeclaration
{
   Declaration_t DeclarationNext;
   int iCheck;
   int iType;
   int storage_class;
   int fct_specifier;
   int s_ellipsis;
   int s_const;
   int s_volatile;
   int s_char;
   int s_short;
   int s_int;
   int s_long;
   int s_signed;
   int s_unsigned;
   int s_float;
   int s_double;
   int s_bool;
   int s_void;
   Name_t Name;
   Class_t Class;
   Enum_t Enum;
   List_t ListDeclarator;
   int lineno;
};

struct sDeclarationSpecial
{
   Declaration_t DeclarationNext;
   int iCheck;
   int iType;
   Name_t Name1;                 /* namespace_definition
                                  , namespace_alias_definition
                                  , using_declaration
                                  , using_directive */
   Name_t Name2;                 /* namespace_alias_definition
                                  , using_declaration */
   List_t ListDeclaration;       /* namespace_definition
                                  , linkage_specification */
   List_t ListTemplateArgument; /* template */
   Declaration_t Declaration;   /* template */
};

struct sDeclarator
{
   Declarator_t DeclaratorNext;
   int iCheck;
   Name_t Name;
   Expr_t Expr;      /* inicializalas argument_declarator-nal */
   List_t ListExpr;  /* inicializalas constructor-nal */
   Init_t Init;      /* inicializalas */
   List_t ListOper;
   Boolean_t bStrange;  /* ha indokolatlan zarojelezes van a declaratorban
                           ( Pl.: Boolean (*a); ) akkor True-ra allitjuk */
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
};

struct sType
{
   Type_t TypeNext;
   int iCheck;
   int s_unknown;
   int fct_specifier;
   int s_ellipsis;
   int s_const;
   int s_volatile;
   int s_char;
   int s_short;
   int s_int;
   int s_long;
   int s_signed;
   int s_unsigned;
   int s_float;
   int s_double;
   int s_bool;
   int s_void;
   Name_t Name;
   Class_t Class;
   Enum_t Enum;
   Declarator_t Declarator;
};

struct sClass
{
   Class_t ClassNext;
   int iCheck;
   Name_t Name;
   int iAccess;   /* SN_PRIVATE, SN_PUBLIC, SN_PROTECTED */
   int iType;     /* SN_CLASS, SN_STRUCT, SN_UNION */
   List_t ListBase;
   List_t ListMember;
};

struct sEnum
{
   Enum_t EnumNext;
   int iCheck;
   Name_t Name;
   List_t ListEnumerator;
};

struct sExpr
{
   Expr_t ExprNext;
   int iCheck;
   int _operator;
   Name_t Name;
   Type_t Type;
   Expr_t Expr1;
   Expr_t Expr2;
   Expr_t Expr3;
   Init_t Init;
   List_t ListExpr;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
};

struct sOper
{
   Oper_t OperNext;
   int iCheck;
   int type;         /* POINTER_STAR, POINTER_AMPERSAND, FUNCTION, ARRAY */
   int s_const;                  /* by pointer */
   int s_volatile;               /* by pointer */
   Name_t Name;                  /* by pointer */
   Expr_t Expr;                  /* by array */
   List_t ListDeclaration;       /* by function */
};

struct sBase
{
   Base_t BaseNext;
   int iCheck;
   int iAccess;
   Boolean_t bVirtual;
   Name_t Name;
};

struct sName
{
   Name_t NameNext;
   int iCheck;
   char *pcName;
};

struct sList
{
   List_t ListNext;
   int iCheck;
   Elem_t ElemFirst;
   Elem_t ElemLast;
};

struct sElem
{
   Elem_t ElemNext;
   int iCheck;
};

struct sInit
{
   Init_t InitNext;
   int iCheck;
   Expr_t Expr;            /* assignment_expression */
   List_t ListInit;        /* { initializer_list } */
};

struct sMember
{
   Member_t MemberNext;
   int iCheck;
   int iAccess;
   Declaration_t Declaration;
};

struct sEnumerator
{
   Enumerator_t EnumeratorNext;
   int iCheck;
   Name_t Name;
   Expr_t Expr;
};

/**********************************************************************/

struct sComp
{
   Comp_t CompParent;
   Symtab_t SymtabVariable;
   Symtab_t SymtabClass;
};

/* struct sIfstack */
/* { */
/*    int status; */
/*    int niveau; */
/* }; */

struct sIfstack
{
   int status;
   int niveau;
   int status_max;
   int undef;
   int _else;
};

#define ZZSIZE (0x10000)
#define ZZMASK ( 0xffff)

/* typedef struct */
/* { */
/*    int yychar; */
/*    char *yytext; */
/*    int yyleng; */
/*    int yylineno; */
/*    int yycharno; */
/* } ZZSTYPE; */

typedef struct
{
   int yytoken;
   int yytoken_cpp;
   sString_t yytext;
   int yyleng;
   int yylineno;
   int yycharno;
} ZZSTYPE;

/* extern from lex.c */

extern int yyleng;
extern int yylineno;
extern int yycharno;
extern int yylineno_text;
extern int yycharno_text;
extern char *yytext;
extern int yyfd;

/* extern from crossref */

extern int is_compound_statement;
extern struct sIfstack sIfstack[1000];    /* old: 100 */
extern ZZSTYPE v[ ZZSIZE ];   /* value stack */
extern char *filename_g;
extern int is_cpp_g;
extern int ivt;   /* index of first empty */
extern int iva;   /* index of 0 */
extern int niveau;
extern int niveauComp;
extern int mode_g;
extern char *pcIdent;
extern Comp_t CompAct;
extern Symtab_t SymtabVariable;
extern Symtab_t SymtabClass;
extern int template_arg;
extern int keyw_cpp;

/* Az eppen aktualis sor adatai */

extern int type_g;
extern char *file_g;
extern int start_lineno_g;
extern int start_charno_g;
extern int end_lineno_g;
extern int end_charno_g;
extern int attr_g;
extern char *ret_g;
extern char *scope_g;
extern char *sym_name_g;
extern char *arg_types_g;
extern char *arg_names_g;

extern FILE *test_fp;

extern FILE *pf;

/* --------- */
/* functions */
/* --------- */
extern char * SN_StrDup(char*);

extern List_t f_ArgumentDeclarationList( void );
extern Class_t f_Class( void );
extern Class_t f_ClassCreate( void );
extern void f_ClassDestroy( Class_t Class );
extern Base_t f_BaseCreate( void );
extern void f_BaseDestroy( Base_t Base );
extern Member_t f_MemberCreate( void );
extern void f_MemberDestroy( Member_t Member );
extern Class_t f_ClassPoorDup( Class_t Class );
extern Declaration_t f_Declaration( int iLevel );
extern Declaration_t f_DeclarationCreate( int iType );
extern Declaration_t f_DeclarationDuplicate( Declaration_t Declaration );
extern DeclarationSpecial_t f_DeclarationSpecialCreate( int iType );
extern void f_DeclarationDestroy( Declaration_t Declaration );
extern void f_DeclarationSpecialDestroy( DeclarationSpecial_t DeclarationSpecial );
extern void f_DeclarationPrint( Declaration_t Declaration );
extern Declaration_t f_MemberDeclaration( void );
extern void f_DeclarationStrcat( char *pc, Declaration_t Declaration );
extern void f_DeclarationProcess( Declaration_t Declaration, int record );
extern void f_DeclarationSkip( void );
extern Declarator_t f_Declarator( Boolean_t may_function );
extern Declarator_t f_DeclaratorCreate( void );
extern Declarator_t f_DeclaratorDuplicate( Declarator_t Declarator );
extern void f_DeclaratorDestroy( Declarator_t Declarator );
extern void f_DeclaratorStrcat( char *pc, Declarator_t Declarator, int exact );
extern Declarator_t f_InitDeclarator( Boolean_t may_function );
extern Declarator_t f_MemberDeclarator( void );
extern Declarator_t f_AbstractDeclarator( void );
extern Declarator_t f_NewDeclarator( void );
extern Declarator_t f_ConversionDeclarator( void );
extern List_t f_InitDeclaratorList( Boolean_t may_function );
extern List_t f_MemberDeclaratorList( void );
extern void f_DeclaratorProcess( Declarator_t Declarator );
extern Enum_t f_Enum( void );
extern Enum_t f_EnumCreate( void );
extern void f_EnumDestroy( Enum_t Enum );
extern Enumerator_t f_EnumeratorCreate( void );
extern void f_EnumeratorDestroy( Enumerator_t Enumerator );
extern Enum_t f_EnumPoorDup( Enum_t Enum );
extern Expr_t f_Expression( void );
extern Expr_t f_ConstantExpression( void );
extern Boolean_t f_expression( void );
extern Boolean_t f_constant_expression( void );
extern Boolean_t f_assignment_expression( void );
extern Boolean_t f_expression_list( void );
extern Expr_t expression( void );
extern void f_ExprDestroy( Expr_t Expr );
extern Name_t f_OperatorOrConversionFunctionName( void );
extern Name_t f_CompleteClassName( void );
extern Name_t f_QualifiedClassName( void );
extern Name_t f_NamespaceName( void );
extern Name_t f_QualifiedName( void );
extern Name_t f_ClassName( void );
extern Name_t f_TemplateArgumentList( void );
extern Boolean_t f_CompoundStatement( char *types, char *names );
extern Type_t f_TypeName( char *pcTerminator );
extern Type_t f_NewTypeName( void );
extern Type_t f_ConversionTypeName( void );
extern Type_t f_TypeCreate( void );
extern void f_TypeDestroy( Type_t Type );
extern void f_TypeToString( Type_t Type, char *pc, int exact );
extern void f_TypeDelPointer( Type_t Type );
extern void f_TypeDelFunction( Type_t Type );
extern void f_TypeAddPointer( Type_t Type );
extern Type_t f_TypeCreateUnknown( void );
extern Type_t f_TypeCreateInt( void );
extern Type_t f_TypeCreateLong( void );
extern Type_t f_TypeCreateChar( void );
extern Type_t f_TypeCreateShort( void );
extern Type_t f_TypeCreateFloat( void );
extern Type_t f_TypeCreateDouble( void );
extern Type_t f_TypeCreateBool( void );
extern Type_t f_TypeCreateUnsigned( void );
extern Type_t f_TypeCreateSigned( void );
extern Type_t f_TypeCreateVoid( void );
extern Type_t f_TypeCreateString( void );
extern Type_t f_TypeCreateName( Name_t Name );
extern Type_t f_TypeFromString( char *type );
extern Type_t f_TypeFromDeclarationAndDeclarator( Declaration_t Declaration, Declarator_t Declarator );
extern Type_t f_TypeFromDeclaration( Declaration_t Declaration );
extern Type_t f_TypeDuplicate( Type_t Type );
extern void f_TypeProcess( Type_t Type );

extern Name_t f_NameCreate( char *pcName );
extern Name_t f_NameDuplicate( Name_t Name );
extern void f_NameDestroy( Name_t Name );
extern void f_NameCat( Name_t Name1, Name_t Name2 );
extern Name_t f_Dname( void );
extern Name_t f_Name( void );
extern Name_t f_OperatorOrConversionFunctionName( void );
extern Name_t f_CompleteClassName( void );
extern Name_t f_QualifiedName( void );
extern Name_t f_ClassName( void );
extern Name_t f_NameDup( Name_t Name );

extern Oper_t f_OperCreate( void );
extern Oper_t f_OperDuplicate( Oper_t Oper );
extern void f_OperDestroy( Oper_t Oper );
extern void f_OperProcess( Oper_t Oper );

extern List_t f_ListCreate( void );
extern List_t f_ListDuplicate( List_t List, Elem_t (*pfElemDuplicate)(Elem_t));
extern void f_ListDestroy( List_t List, void (*pfDestroy)( Elem_t ));
extern void f_ListAddLast( List_t *pList, Elem_t Elem );
extern void f_ListAddFirst( List_t *pList, Elem_t Elem );
extern Elem_t f_ListRemoveFirst( List_t List );
extern void f_ListConcat( List_t *pList1, List_t List2 );
extern Elem_t f_ListLastElem( List_t List );

extern Init_t f_Init( char cTerminator );
extern Init_t f_NewInitializer( void );
extern Init_t f_InitCreate( void );
extern void f_InitDestroy( Init_t Init );
extern void f_InitProcess( Init_t Init );

extern void f_Strcat( char *pc1, char *pc2 );

extern void f_SyntaxError( int iError );
extern void f_InternalError( int iError );
extern void f_FatalError( int iError );

extern Expr_t f_AssignmentExpression( void );
extern List_t f_ExpressionList( void );
extern void f_StepTo( int iToken, ... );

extern Boolean_t f_IsLiteral( int iToken );
extern Boolean_t f_IsCppLiteral( int iToken );
extern Boolean_t f_DeclaratorIsFunctionDefinition( Declarator_t Declarator );

#ifndef Malloc
extern void *Malloc( int iSize );
#endif /* Malloc */

extern void f_read( int i );

extern void f_ExprProcess( Expr_t Expr );

extern char *Strdup( char *pc, int leng );

extern void Put_cross_class_or_typedef_ref( int type, int scope_type, int scope_lev, char *fnc_cls, char *fnc, char *fnc_arg_types, char *scope, char *what, char *arg_types, char *file, int lineno, int acc );

extern int Get_symbol( char *scope_global, char *scope, char *name, char *arg_list, char *scope_ret, char *type_ret, char *define_ret, int exact );

extern void Put_cross_ref( int type, int scope_type, int scope_lev, char *fnc_cls, char *fnc, char *fnc_arg_types, char *scope, char *what, char *arg_types, char *file, int lineno, int acc );

extern char *f_NameFromType( Type_t Type );
extern char *f_NameFromDeclaration( Declaration_t Declaration );
extern void f_PutConstructor( Type_t Type, int lineno, int mode );
extern void f_PutConstructorByNewOrDelete( Type_t Type, int lineno, int mode );
extern Type_t f_TypeBasic( Type_t Type, int lineno );
extern void Abort( void );


