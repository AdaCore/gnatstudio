/*

Copyright (c) 2000, 2001, Red Hat, Inc.

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#include "crossrefP.h"
#include "operator.h"
#include "longstr.h"
#include "srchtbl.h"

/* Valid debugging macros for this file:
	#define PRINT
	#define PRINT_EXPR
	#define PRINT_OPERATOR
*/

#define null_safe(x) ((x)==0?(""):(x))

#define ProcTypeAssign( Proc, Proc2 ) Proc.Type = Proc2.Type, Proc2.Type = 0
#define ProcNameAssign( Proc, Proc2 ) Proc.name = Proc2.name, Proc2.name = 0; \
                                      Proc.lineno_beg = Proc2.lineno_beg; \
                                      Proc.charno_beg = Proc2.charno_beg; \
                                      Proc.lineno_end = Proc2.lineno_end; \
                                      Proc.charno_end = Proc2.charno_end
#define ProcScopAssign( Proc, Proc2 ) Proc.scope = Proc2.scope, Proc2.scope = 0

typedef struct sProc Proc_t;

struct sProc
{
   Type_t Type;
   char *name;
   char *scope;
#ifdef PRINT_EXPR
   char ac[10000];
#endif
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
};

static Proc_t f_expr( Expr_t Expr, int access, char *scope, int call, int pass );
/* static Proc_t f_exprlist( List_t ListExpr, int access, char *scope, char *arglist, int pass ); 15.01.98 rigo */
static Proc_t f_exprlist( List_t ListExpr, int access, char *scope, LongString *parglist, int pass ); /* 15.01.98 rigo */
static Type_t get_variable_type( char *scope_global, char *scope, char *name, int lineno_beg, int charno_beg, int lineno_end, int charno_end, char *sope_ret, char *type_ret, char *define_ret, int *paf_type_ret, int *paf_scope_ret );
static Type_t get_function_type( char *scope_global, char *scope, char *name, char *arglist, int lineno_beg, int charno_beg, int lineno_end, int charno_end, char *sope_ret, char *type_ret, char *define_ret, int *paf_type_ret, int *paf_scope_ret );
static void ProcDestroy( int i, Proc_t Proc );
static char *strdup_m( char *pc );
static char *f_TypeToScope( Type_t Type );
static char *get_scope( char *name );
static char *get_name( char *name );
static int my_Get_symbol( char *scope_global, char *scope, char *name, char *arg_list, char *scope_ret, char *type_ret, char *define_ret, int exact );
static char *paf_type_to_string( int paf_type );
static Type_t f_OperatorCall2( char *pcOperator, Type_t Type1, Type_t Type2, int access, int lineno );

static int tab;

extern void f_ExprProcess( Expr_t Expr )
{
   Proc_t Proc;
/* char ac[1000]; */

   Proc = f_expr( Expr, READ, 0, 0, 0 );

#ifdef PRINT_EXPR
   printf( "expression: %s\n", Proc.ac );
#endif
/* f_TypeToString( Proc.Type, ac, 1 ); */
/* printf( "type: %s\n", ac ); */

   ProcDestroy( 99, Proc );
}

static Proc_t f_expr( Expr_t Expr, int access, char *scope, int call, int pass )
{
   char *pcOperator;
/* char arglist[10000]; 15.01.98 rigo */
   LongString arglist; /* 15.01.98 rigo */
   Proc_t Proc;
   Proc_t Proc1;
   Proc_t Proc2;
   Proc_t Proc3;

   tab++;

   Proc.Type = 0;
   Proc.name = 0;
   Proc.scope = 0;

   Proc1.Type = 0;
   Proc1.name = 0;
   Proc1.scope = 0;

   Proc2.Type = 0;
   Proc2.name = 0;
   Proc2.scope = 0;

   Proc3.Type = 0;
   Proc3.name = 0;
   Proc3.scope = 0;

#if 0 /* 15.01.98 rigo : ez mar nem kell */
#define MAX_EXPR_DEPTH  150     /* Zsolt Koppany, 30-dec-97 */
   if (tab >= MAX_EXPR_DEPTH)
   {
      static int printed = FALSE;

      tab--;
      if (!printed)
      {
         printed = TRUE;
         fprintf(stderr,"To deep (> %d) expressions!\n",MAX_EXPR_DEPTH);
      }
      return Proc;
   }
#endif

   if( Expr == 0 )
   {
      f_InternalError( 52 );
      return Proc;
   }

#ifdef PRINT_OPERATOR
   printf( "%*.*soperator: %d\n", tab*4, tab*4, "", Expr->_operator );
#endif

   switch( Expr->_operator )
   {
   case OPERATOR_KOMMA            :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( ",", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc2 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s, %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 1, Proc1 );
      ProcDestroy( 2, Proc2 );
      break;

   case OPERATOR_ASSIGN           : pcOperator = "="  ; goto label1;
   case OPERATOR_MULTassign       : pcOperator = "*=" ; goto label1;
   case OPERATOR_DIVassign        : pcOperator = "/=" ; goto label1;
   case OPERATOR_MODassign        : pcOperator = "%=" ; goto label1;
   case OPERATOR_PLUSassign       : pcOperator = "+=" ; goto label1;
   case OPERATOR_MINUSassign      : pcOperator = "-=" ; goto label1;
   case OPERATOR_LSassign         : pcOperator = "<<="; goto label1;
   case OPERATOR_RSassign         : pcOperator = ">>="; goto label1;
   case OPERATOR_ANDassign        : pcOperator = "&=" ; goto label1;
   case OPERATOR_ERassign         : pcOperator = "^=" ; goto label1;
   case OPERATOR_ORassign         : pcOperator = "|=" ; goto label1;
label1:
      Proc1 = f_expr( Expr->Expr1, WRITE, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ , scope, 0, pass );
      Proc.Type = f_OperatorCall2( pcOperator, Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s %s %s", Proc1.ac, pcOperator, Proc2.ac );
#endif
      ProcDestroy( 3, Proc1 );
      ProcDestroy( 4, Proc2 );
      break;

   case OPERATOR_CONDITIONAL      :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc3 = f_expr( Expr->Expr3, READ, scope, 0, pass );
      ProcTypeAssign( Proc, Proc2 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "(%s?%s:%s)", Proc1.ac, Proc2.ac, Proc3.ac );
#endif
      ProcDestroy( 5, Proc1 );
      ProcDestroy( 6, Proc2 );
      ProcDestroy( 7, Proc3 );
      break;
      
   case OPERATOR_OROR             :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "||", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         Proc.Type = f_TypeCreateInt();
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s || %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 8, Proc1 );
      ProcDestroy( 9, Proc2 );
      break;

   case OPERATOR_ANDAND           :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "&&", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         Proc.Type = f_TypeCreateInt();
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s && %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 10, Proc1 );
      ProcDestroy( 11, Proc2 );
      break;

   case OPERATOR_OR               :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "|", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s | %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 12, Proc1 );
      ProcDestroy( 13, Proc2 );
      break;

   case OPERATOR_ER               :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "'", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s ^ %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 14, Proc1 );
      ProcDestroy( 15, Proc2 );
      break;

   case OPERATOR_AND              :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "&", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s & %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 16, Proc1 );
      ProcDestroy( 17, Proc2 );
      break;

   case OPERATOR_EQ               : pcOperator = "=="; goto label2;
   case OPERATOR_NE               : pcOperator = "!="; goto label2;
   case OPERATOR_L                : pcOperator = "<" ; goto label2;
   case OPERATOR_G                : pcOperator = ">" ; goto label2;
   case OPERATOR_LE               : pcOperator = ">="; goto label2;
   case OPERATOR_GE               : pcOperator = "<="; goto label2;
label2:
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( pcOperator, Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         Proc.Type = f_TypeCreateInt();
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s %s %s", Proc1.ac, pcOperator, Proc2.ac );
#endif
      ProcDestroy( 18, Proc1 );
      ProcDestroy( 19, Proc2 );
      break;

   case OPERATOR_LS               :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "<<", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s << %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 20, Proc1 );
      ProcDestroy( 21, Proc2 );
      break;

   case OPERATOR_RS               :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( ">>", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s >> %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 22, Proc1 );
      ProcDestroy( 23, Proc2 );
      break;

   case OPERATOR_PLUS             : pcOperator = "+"; goto label3;
   case OPERATOR_MINUS            : pcOperator = "-"; goto label3;
   case OPERATOR_MULT             : pcOperator = "*"; goto label3;
   case OPERATOR_DIV              : pcOperator = "/"; goto label3;
label3:
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( pcOperator, Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc1 );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s %s %s", Proc1.ac, pcOperator, Proc2.ac );
#endif
      ProcDestroy( 24, Proc1 );
      ProcDestroy( 25, Proc2 );
      break;

   case OPERATOR_MOD              :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      Proc.Type = f_OperatorCall2( "%", Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         Proc.Type = f_TypeCreateInt();
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s %% %s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 26, Proc1 );
      ProcDestroy( 27, Proc2 );
      break;

   case OPERATOR_DOTstar          : pcOperator = ".*" ; goto label4;
   case OPERATOR_ARROWstar        : pcOperator = "->*"; goto label4;
label4:
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, f_TypeToScope( Proc1.Type ), 0, pass );
      Proc.Type = f_OperatorCall2( pcOperator, Proc1.Type, Proc2.Type, access, 0 );
      if( Proc.Type == 0 )
      {
         ProcTypeAssign( Proc, Proc2 );
      }
      f_TypeDelPointer( Proc.Type );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s%s%s", Proc1.ac, pcOperator, Proc2.ac );
#endif
      ProcDestroy( 28, Proc1 );
      ProcDestroy( 29, Proc2 );
      break;

   case OPERATOR_CAST             :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      ProcNameAssign( Proc, Proc2 );
      ProcScopAssign( Proc, Proc2 );
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "(%s)%s", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 30, Proc1 );
      ProcDestroy( 31, Proc2 );
      break;

   case OPERATOR_SIZEOF_EXPRESSION:
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc.Type = f_TypeCreateInt();
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "sizeof(%s)", Proc1.ac );
#endif
      ProcDestroy( 32, Proc1 );
      break;

   case OPERATOR_SIZEOF_TYPENAME  :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc.Type = f_TypeCreateInt();
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "sizeof(%s)", Proc1.ac );
#endif
      ProcDestroy( 33, Proc1 );
      break;

   case OPERATOR_PRE_ICR          :
      Proc1 = f_expr( Expr->Expr1, READ | WRITE, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "++%s", Proc1.ac );
#endif
      ProcDestroy( 34, Proc1 );
      break;

   case OPERATOR_PRE_DECR         :
      Proc1 = f_expr( Expr->Expr1, READ | WRITE, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "--%s", Proc1.ac );
#endif
      ProcDestroy( 35, Proc1 );
      break;

   case OPERATOR_POST_ICR         :
      Proc1 = f_expr( Expr->Expr1, READ | WRITE, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s++", Proc1.ac );
#endif
      ProcDestroy( 36, Proc1 );
      break;

   case OPERATOR_POST_DECR        :
      Proc1 = f_expr( Expr->Expr1, READ | WRITE, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s--", Proc1.ac );
#endif
      ProcDestroy( 37, Proc1 );
      break;

   case OPERATOR_STAR             :
      Proc1 = f_expr( Expr->Expr1, access, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      f_TypeDelPointer( Proc.Type );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "*%s", Proc1.ac );
#endif
      ProcDestroy( 38, Proc1 );
      break;

   case OPERATOR_ET               :
      Proc1 = f_expr( Expr->Expr1, access, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      f_TypeAddPointer( Proc.Type );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "&%s", Proc1.ac );
#endif
      ProcDestroy( 39, Proc1 );
      break;

   case OPERATOR_UPLUS            :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "+%s", Proc1.ac );
#endif
      ProcDestroy( 40, Proc1 );
      break;

   case OPERATOR_UMINUS           :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "-%s", Proc1.ac );
#endif
      ProcDestroy( 41, Proc1 );
      break;

   case OPERATOR_NOT              :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      Proc.Type = f_TypeCreateInt();
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "!%s", Proc1.ac );
#endif
      ProcDestroy( 42, Proc1 );
      break;

   case OPERATOR_BNOT             :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "~%s", Proc1.ac );
#endif
      ProcDestroy( 43, Proc1 );
      break;

   case OPERATOR_ARRAY            :
      Proc1 = f_expr( Expr->Expr1, access, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, READ  , scope, 0, pass );
      ProcTypeAssign( Proc, Proc1 );
      f_TypeDelPointer( Proc.Type );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s[%s]", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 44, Proc1 );
      ProcDestroy( 45, Proc2 );
      break;

   case OPERATOR_FUNCTION_CALL    :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 1, pass );
      LongStringInit( &arglist, -1 );
/*    Proc2 = f_exprlist( Expr->ListExpr, READ, scope,  arglist, 1 );    15.01.98 rigo */
      Proc2 = f_exprlist( Expr->ListExpr, READ, scope, &arglist, 1 ); /* 15.01.98 rigo */
      if( Proc1.name )
      {
/*       char acScope  [10000]; 15.01.98 rigo */
/*       char acRetType[10000]; 15.01.98 rigo */
         static char acScope  [10000]; /* 15.01.98 rigo */
         static char acRetType[10000]; /* 15.01.98 rigo */
         char objname[1000];
         static char acRetDefine[10000]; /* 16.02.98 rigo */
         static char acArglist[10000]; /* 22.01.98 rigo */
         int paf_type;
         int paf_scope;
         char *scope_global;
         char *scope_name;	/* 16.02.98 rigo */
         char *object;		/* 16.02.98 rigo */
         char *name;
         SearchTable *macro_chain = NULL;
         SearchEntry mac_ent;
         int first_time;

         if( arglist.buf ) /* 22.01.98 rigo */
         {
            strcpy( acArglist, arglist.buf );
         }
         else
         {
            acArglist[0] = 0;
         }

         object = Proc1.name;	/* 16.02.98 rigo */
         for (first_time = 1;;)
         {
            scope_name   = get_scope( object );
            name         = get_name ( object );

            scope_global = scope_name;

            if( scope_global == 0 && scope_g[0] )
            {
               scope_global = scope_g;
            }

            Proc.Type = get_function_type( scope_global
                                         , Proc1.scope
                                         , name
   /*                                    , arglist            15.01.98 rigo */
                                         , acArglist       /* 22.01.98 rigo */
                                         , Proc1.lineno_beg
                                         , Proc1.charno_beg
                                         , Proc1.lineno_end
                                         , Proc1.charno_end
                                         , acScope
                                         , acRetType
                                         , acRetDefine /* 16.02.98 rigo */
                                         , &paf_type
                                         , &paf_scope
                                         );

            if( paf_type == PAF_MACRO_DEF && *acRetDefine )	/* 16.02.98 */
            {
               if (!macro_chain)
               {
                  objname[sizeof(objname) - 1] = '\0';

                  macro_chain = SearchTableCreate(100, SEARCH_HASH_TABLE, NULL);
                  mac_ent.key = object;
                  mac_ent.key_len = -1;
                  mac_ent.data = NULL;
                  mac_ent.data_len = 0;
                  mac_ent.flag = SEARCH_DUP_KEY;

                  macro_chain->add(&macro_chain,mac_ent);
               }
               /* new message with acRetDefine */
               mac_ent.key = acRetDefine;
               mac_ent.key_len = -1;
               /* We have to check whether the name is already in the
                * chain because that would cause an end less loop!*/
               if (!macro_chain->search(&macro_chain, mac_ent))
               {
                  macro_chain->add(&macro_chain,mac_ent);

                  f_TypeDestroy (Proc.Type);

                  /* We generate xref call only for the first macro in the chain. */
                  if (first_time)
                  {
                     int paf_access = PAF_REF_READ;

                     first_time = 0;

                     Put_cross_ref( paf_type
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , paf_scope
                         , scope_g
                         , sym_name_g
                         , arg_types_g
                         , acScope
                         , name
/*                       , arglist         15.01.98 rigo */
                         , acArglist    /* 22.01.98 rigo */
                         , filename_g
                         , Proc1.lineno_beg
                         , paf_access
                         );
                  }
                  strncpy(objname, acRetDefine, sizeof(objname) - 1);
                  object = objname;

                  continue;
               }
            }
            break;
         }
         if (macro_chain)
         {
            macro_chain->destroy(&macro_chain);
            macro_chain = NULL;
         }
         if( paf_type )
         {
            int paf_access = 0;

            if( access & WRITE ) paf_access |= PAF_REF_WRITE | PAF_REF_READ;
            if( access & READ  ) paf_access |= PAF_REF_READ ;
            if( pass           ) paf_access |= PAF_REF_PASS ;

            Put_cross_ref( paf_type
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , paf_scope
                         , scope_g
                         , sym_name_g
                         , arg_types_g
                         , acScope
                         , name
/*                       , arglist         15.01.98 rigo */
                         , acArglist    /* 22.01.98 rigo */
                         , filename_g
                         , Proc1.lineno_beg
                         , paf_access
                         );
         }
         else
         {
            int paf_access = 0;

            *acRetDefine = '\0';
            if( access & WRITE ) paf_access |= PAF_REF_WRITE | PAF_REF_READ;
            if( access & READ  ) paf_access |= PAF_REF_READ ;
            if( pass           ) paf_access |= PAF_REF_PASS ;

            Put_cross_ref( PAF_REF_UNDEFINED
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , PAF_REF_SCOPE_GLOBAL
                         , scope_g
                         , sym_name_g
                         , arg_types_g
/*                       , scope_global 23.02.97 rigo */
                         , scope_name
                         , name
/*                       , arglist         15.01.98 rigo */
                         , acArglist    /* 22.01.98 rigo */
                         , filename_g
                         , Proc1.lineno_beg
                         , paf_access
                         );
         }
      }
      else
      {
         ProcTypeAssign( Proc, Proc1 );
         f_TypeDelFunction( Proc.Type );
      }
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s(%s)", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 46, Proc1 );
      ProcDestroy( 47, Proc2 );
      arglist.free( &arglist ); /* 15.01.98 rigo */
      break;

   case OPERATOR_CONSTRUCTOR_CALL :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
/*    Proc2 = f_exprlist( Expr->ListExpr, READ, scope, arglist, 1 );    15.01.98 rigo */
      Proc2 = f_exprlist( Expr->ListExpr, READ, scope, 0      , 1 ); /* 15.01.98 rigo */
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s(%s)", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 48, Proc1 );
      ProcDestroy( 49, Proc2 );
      break;

   case OPERATOR_DOT              : pcOperator = "."  ; goto label5;
   case OPERATOR_ARROW            : pcOperator = "->" ; goto label5;
label5:
      Proc1 = f_expr( Expr->Expr1, access, scope, 0, pass );
      Proc2 = f_expr( Expr->Expr2, access, f_TypeToScope( Proc1.Type ), call, pass );
      ProcTypeAssign( Proc, Proc2 );
/*    f_TypeDelPointer( Proc.Type ); 14.02.97 rigo: szerintem ez nem kell ide */
      ProcNameAssign( Proc, Proc2 );
      ProcScopAssign( Proc, Proc2 );
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s%s%s", Proc1.ac, pcOperator, Proc2.ac );
#endif
      ProcDestroy( 50, Proc1 );
      ProcDestroy( 51, Proc2 );
      break;

   case OPERATOR_NEW              :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
/*    Proc2 = f_exprlist( Expr->ListExpr, READ, scope, arglist, 1 );    15.01.98 rigo */
      Proc2 = f_exprlist( Expr->ListExpr, READ, scope, 0      , 1 ); /* 15.01.98 rigo */
      f_PutConstructorByNewOrDelete( Proc1.Type, Expr->Expr1->lineno_beg, CONSTRUCTOR );
      if( Expr->Init ) f_InitProcess( Expr->Init );
      ProcTypeAssign( Proc, Proc1 );
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "new (%s)(%s)", Proc1.ac, Proc2.ac );
#endif
      ProcDestroy( 52, Proc1 );
      ProcDestroy( 53, Proc2 );
      break;

   case OPERATOR_DELETE           :
      Proc1 = f_expr( Expr->Expr1, READ, scope, 0, pass );
      f_PutConstructorByNewOrDelete( Proc1.Type, 0 /* Expr->Expr1->lineno_beg */, DESTRUCTOR );
      Proc.Type = f_TypeCreateInt();   /* esetleg void ? */
      Proc.name = 0;
      Proc.scope = 0;
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "delete %s", Proc1.ac );
#endif
      ProcDestroy( 55, Proc1 );
      break;

   case OPERATOR_NAME             :
      {
      char *object;		/* 16.02.98 rigo */
#ifdef PRINT_EXPR
      sprintf( Proc.ac, "%s", Expr->Name->pcName );
#endif
      if( call )
      {
         Proc.Type = 0;
         object = Expr->Name->pcName;
      }
      else
      {
/*       char acScope  [10000]; 15.01.98 rigo */
/*       char acRetType[10000]; 15.01.98 rigo */
         static char acScope  [10000]; /* 15.01.98 rigo */
         static char acRetType[10000]; /* 15.01.98 rigo */
         static char acRetDefine[10000]; /* 18.02.98 rigo */
         char objname[1000];
         int paf_type;
         int paf_scope;
         char *scope_global;
         char *name;
         char *scope_name;	/* 16.02.98 rigo */
         SearchTable *macro_chain = NULL;
         SearchEntry mac_ent;
         int first_time;
         
         object = Expr->Name->pcName;

         for (first_time = 1;;)
         {
            scope_name = get_scope( object );
            name       = get_name ( object );

            scope_global = scope_name;

            if( scope_global == 0 && scope_g[0] )
            {
               scope_global = scope_g;
            }

            Proc.Type = get_variable_type( scope_global
                                         , scope
                                         , name
                                         , Expr->lineno_beg
                                         , Expr->charno_beg
                                         , Expr->lineno_end
                                         , Expr->charno_end
                                         , acScope
                                         , acRetType
                                         , acRetDefine /* 16.02.98 rigo */
                                         , &paf_type
                                         , &paf_scope
                                         );
            if( paf_type == PAF_MACRO_DEF && *acRetDefine )	/* 16.02.98 */
            {
               if (!macro_chain)
               {
                  objname[sizeof(objname) - 1] = '\0';

                  macro_chain = SearchTableCreate(100, SEARCH_HASH_TABLE, NULL);
                  mac_ent.key = object;
                  mac_ent.key_len = -1;
                  mac_ent.data = NULL;
                  mac_ent.data_len = 0;
                  mac_ent.flag = SEARCH_DUP_KEY;

                  macro_chain->add(&macro_chain,mac_ent);
               }
               /* new message with acRetDefine */
               mac_ent.key = acRetDefine;
               mac_ent.key_len = -1;
               /* We have to check whether the name is already in the
                * chain because that would cause an end less loop!*/
               if (!macro_chain->search(&macro_chain, mac_ent))
               {
                  macro_chain->add(&macro_chain,mac_ent);

                  f_TypeDestroy (Proc.Type);

                  /* We generate xref call only for the first macro in the chain. */
                  if (first_time)
                  {
                     int paf_access = PAF_REF_READ;

                     first_time = 0;

                     Put_cross_ref( paf_type
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , paf_scope
                         , scope_g
                         , sym_name_g
                         , arg_types_g
                         , ( paf_type == PAF_REF_TO_LOCAL_VAR ) ? sym_name_g : acScope
                         , name
                         , 0
                         , filename_g
                         , Expr->lineno_beg
                         , paf_access
                         );
                  }
                  strncpy(objname, acRetDefine, sizeof(objname) - 1);
                  object = objname;

                  continue;
               }
            }
            break;
         }
         if (macro_chain)
         {
            macro_chain->destroy(&macro_chain);
            macro_chain = NULL;
         }

         if( paf_type )
         {
            int paf_access = 0;

            if( access & WRITE ) paf_access |= PAF_REF_WRITE | PAF_REF_READ;
            if( access & READ  ) paf_access |= PAF_REF_READ ;
            if( pass           ) paf_access |= PAF_REF_PASS ;

            /* This will output a known variable reference 
	       (e.g. predefined local variable ref, global variable ref, etc.)
	       
	        Example:     local_var = 5;
	     */

            Put_cross_ref( paf_type
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , paf_scope
                         , scope_g
                         , sym_name_g
                         , arg_types_g
                         , ( paf_type == PAF_REF_TO_LOCAL_VAR ) ? sym_name_g : acScope
                         , name
                         , 0
                         , filename_g
                         , Expr->lineno_beg
                         , paf_access
                         );
         }
         else
         {
            int paf_access = 0;

            if( access & WRITE ) paf_access |= PAF_REF_WRITE | PAF_REF_READ;
            if( access & READ  ) paf_access |= PAF_REF_READ ;
            if( pass           ) paf_access |= PAF_REF_PASS ;

            /* This will output a undefined variable reference. */

            Put_cross_ref( PAF_REF_UNDEFINED
                         , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                         , PAF_REF_SCOPE_GLOBAL
                         , scope_g
                         , sym_name_g
                         , arg_types_g
/*                       , scope_global 23.02.97 rigo */
                         , scope_name
                         , name
                         , 0
                         , filename_g
                         , Expr->lineno_beg
                         , paf_access
                         );
         }

         if( access & READ )
         {
/*          printf( "read : %s.%s\n", scope ? scope : scope_global, Expr->name ); */
         }
         if( access & WRITE )
         {
/*          printf( "write: %s.%s\n", scope ? scope : scope_global, Expr->name ); */
         }
      }
      Proc.name = strdup_m( object );  /* itt kerdeses, hogy nem
                                                a name-t kellene-e atadni ? */
      Proc.scope = strdup_m( scope );
      Proc.lineno_beg = Expr->lineno_beg;
      Proc.charno_beg = Expr->charno_beg;
      Proc.lineno_end = Expr->lineno_end;
      Proc.charno_end = Expr->charno_end;
      break;
      }
      
   case OPERATOR_TYPE             :
#ifdef PRINT_EXPR
      f_TypeToString( Expr->Type, Proc.ac, 1 );
#endif
      /* feloldjuk a typedef-eket */
      Expr->Type = f_TypeBasic( Expr->Type, Expr->lineno_beg );
      /* a Type-ban megbuvo kifejezeseket feldolgozzuk */
      f_TypeProcess( Expr->Type );
      if( Expr->lineno_beg != 0 )
      {
/* test okbol atmenetileg kikommentezve
         if( test_fp )
         {
            fprintf( test_fp
                   ,"%d xref_l %d.%d %d.%d\n"
                   , PAF_HIGH
                   , Expr->lineno_beg
                   , Expr->charno_beg
                   , Expr->lineno_end
                   , Expr->charno_end
                   );
         }
*/
      }
      Proc.Type = Expr->Type;
      Expr->Type = 0;
      Proc.name = 0;
      Proc.scope = 0;
      break;
      
   default:
      printf( "Expr->_operator: %d (Expr: %d)\n", Expr->_operator, (int) Expr );
      f_InternalError( 53 );
   }

   tab--;
   return Proc;
}

/* static Proc_t f_exprlist( List_t ListExpr, int access, char *scope, char *arglist, int pass ) 15.01.98 rigo */
static Proc_t f_exprlist( List_t ListExpr, int access, char *scope, LongString *parglist, int pass ) /* 15.01.98 rigo */
{
   Elem_t Elem;
   Proc_t Proc;
   Proc_t Proc1;
/* char type[10000]; 15.01.98 rigo */
   static char type[10000]; /* 15.01.98 rigo */
   int bFirst; /* 15.01.98 rigo */

#ifdef PRINT_EXPR
   Proc.ac[0] = 0;
#endif
/* arglist[0] = 0; 15.01.98 rigo */
   bFirst = True; /* 15.01.98 rigo */

   for( Elem = d_ElemFirst( ListExpr ); Elem; Elem = Elem->ElemNext )
   {
      Expr_t Expr = (Expr_t) Elem;

      Proc1 = f_expr( Expr, READ, 0, 0, pass );

      f_TypeToString( Proc1.Type, type, 0 );

#ifdef PRINT_EXPR
      if( Proc.ac[0] == 0 )
      {
         strcat( Proc.ac, Proc1.ac );
      }
      else
      {
         strcat( Proc.ac, "," );
         strcat( Proc.ac, Proc1.ac );
      }
#endif

      if( parglist ) /* 15.01.98 rigo */
      {
/*       if( arglist[0] == 0 ) 15.01.98 rigo */
         if( bFirst ) /* 15.01.98 rigo */
         {
/*          strcat( arglist, type ); 15.01.98 rigo */
            parglist->append( parglist, type, -1 ); /* 15.01.98 rigo */
            bFirst = False; /* 15.01.98 rigo */
         }
         else
         {
/*          strcat( arglist, "," ); 15.01.98 rigo */
/*          strcat( arglist, type ); 15.01.98 rigo */
            parglist->append( parglist, "," , -1 ); /* 15.01.98 rigo */
            parglist->append( parglist, type, -1 ); /* 15.01.98 rigo */
         }
      }

      ProcDestroy( 56, Proc1 );
   }

   Proc.Type = 0;
   Proc.name = 0;
   Proc.scope = 0;

   return Proc;
}

static void ProcDestroy( int i, Proc_t Proc )
{
   if( Proc.Type ) f_TypeDestroy( Proc.Type );
   if( Proc.name ) ckfree( (char*)Proc.name );
   if( Proc.scope ) ckfree( (char*)Proc.scope );

   Proc.Type = 0;
   Proc.name = 0;
   Proc.scope = 0;
}

static char *strdup_m( char *pc )
{
   if( pc == 0 ) return 0;
   return        SN_StrDup( pc );
}

static char *f_TypeToScope( Type_t Type )
{
   if( Type && Type->Name )
   {
      return Type->Name->pcName;
   }
   else
   {
      return 0;
   }
}

static Type_t get_variable_type( char *scope_global, char *scope, char *name, int lineno_beg, int charno_beg, int lineno_end, int charno_end, char *scope_ret, char *type_ret, char *define_ret, int *paf_type_ret, int *paf_scope_ret )
{
   Type_t Type;
   Type_t TypeReturn;

   *paf_scope_ret = PAF_REF_SCOPE_GLOBAL;

#ifdef PRINT
   printf( "--> search    : get_variable_type: <%s> <%s> %s\n"
         , scope_global ? scope_global : ""
         , scope ? scope : ""
         , name
         );
#endif

   if( scope )
   {
      /* itt majd a SymtabClass -ban kell keresni */
   }
   else
   {
      if(( Type = (Type_t) SymtabFind( SymtabVariable, name )))
      {
         TypeReturn = f_TypeDuplicate( Type );
         *paf_type_ret = PAF_REF_TO_LOCAL_VAR;
         *paf_scope_ret = PAF_REF_SCOPE_LOCAL;
         if( test_fp )
         {
            fprintf( test_fp
                   ,"%d xref_l %d.%d %d.%d\n"
                   , PAF_HIGH
                   , lineno_beg
                   , charno_beg
                   , lineno_end
                   , charno_end
                   );
         }
         return TypeReturn;
      }
   }

   *paf_type_ret = my_Get_symbol( scope_global
                                     , scope
                                     , name
                                     , 0
                                     , scope_ret
                                     , type_ret
                                     , define_ret /* 16.02.98 rigo */
                                     , 0 );
   if( *paf_type_ret  )
   {
#ifdef PRINT
      printf( "<-- found     : get_variable_type: <%s> <%s> %s **** |%s|%s|%s|%d|\n"
            , scope_global ? scope_global : ""
            , scope ? scope : ""
            , name
            , scope_ret
            , type_ret
            , define_ret /* 16.02.98 rigo */
            , *paf_type_ret
            );
#endif
      if( test_fp )
      {
         fprintf( test_fp
                ,"%d xref_g %d.%d %d.%d\n"
                , PAF_HIGH
                , lineno_beg
                , charno_beg
                , lineno_end
                , charno_end
                );
      }
      TypeReturn = f_TypeFromString( type_ret );
      if( TypeReturn )
      {
         /* leasunk a typedef-eken keresztul az igazi tipusig */
         TypeReturn = f_TypeBasic( TypeReturn, lineno_beg );
      }
      else
      {
         TypeReturn = f_TypeCreateInt();
      }
   }
   else
   {
#ifdef PRINT
      printf( "<-- not found : get_variable_type: <%s> <%s> %s ****\n"
            , scope_global ? scope_global : ""
            , scope ? scope : ""
            , name
            );
#endif
      if( test_fp )
      {
         fprintf( test_fp
                ,"%d xref_u %d.%d %d.%d\n"
                , PAF_HIGH
                , lineno_beg
                , charno_beg
                , lineno_end
                , charno_end
                );
      }
      TypeReturn = f_TypeCreateUnknown();
   }

   return TypeReturn;
}

static Type_t get_function_type( char *scope_global, char *scope, char *name, char *arglist, int lineno_beg, int charno_beg, int lineno_end, int charno_end, char *scope_ret, char* type_ret, char *define_ret, int *paf_type_ret, int *paf_scope_ret )
{
   Type_t TypeReturn;

   *paf_scope_ret = PAF_REF_SCOPE_GLOBAL;

#ifdef PRINT
   printf( "--> search    : get_function_type: <%s> <%s> <%s> <%s> %s:%d.%d\n"
         , scope_global ? scope_global : ""
         , scope ? scope : ""
         , name
         , arglist
         , filename_g
         , f_lineno( 0 )
         , f_charno( 0 )
         );
#endif

   *paf_type_ret = my_Get_symbol( scope_global
                                     , scope
                                     , name
                                     , arglist
                                     , scope_ret
                                     , type_ret
                                     , define_ret /* 16.02.98 rigo */
                                     , 0 );
   if( *paf_type_ret )
   {
#ifdef PRINT
      printf( "<-- found     : get_function_type: <%s> <%s> %s( %s ) **** |%s|%s|%s|%d|\n"
            , scope_global ? scope_global : ""
            , scope ? scope : ""
            , name
            , arglist
            , scope_ret
            , type_ret
            , define_ret /* 16.02.98 rigo */
            , *paf_type_ret
            );
#endif
      if( test_fp )
      {
         fprintf( test_fp
                ,"%d xref_g %d.%d %d.%d\n"
                , PAF_HIGH
                , lineno_beg
                , charno_beg
                , lineno_end
                , charno_end
                );
      }
      TypeReturn = f_TypeFromString( type_ret );
      if( TypeReturn == 0 ) TypeReturn = f_TypeCreateInt();
   }
   else
   {
#ifdef PRINT
      printf( "<-- not found : get_function_type: <%s> <%s> %s( %s ) **** not found\n"
            , scope_global ? scope_global : ""
            , scope ? scope : ""
            , name
            , arglist
            );
#endif
      if( test_fp )
      {
         fprintf( test_fp
                ,"%d xref_u %d.%d %d.%d\n"
                , PAF_HIGH
                , lineno_beg
                , charno_beg
                , lineno_end
                , charno_end
                );
      }
      TypeReturn = f_TypeCreateUnknown();
   }

   return TypeReturn;
}

static char *get_scope( char *name )
{
   char *pc;
   static char ac[10000];  /* old: 1000 */

   if(( pc = strrchr( name, ':' )))
   {
      if( pc[-1] == ':' )
      {
         pc[-1] = 0;
         strcpy( ac, name );
         pc[-1] = ':';
         return ac;
      }
   }
   return 0;
}

static char *get_name( char *name )
{
   char *pc;

   if(( pc = strrchr( name, ':' )))
   {
      return pc+1;
   }
   else
   {
      return name;
   }
}

static int my_Get_symbol( char *scope_global, char *scope, char *name, char *arg_list, char *scope_ret, char *type_ret, char *define_ret, int exact )
{
   int paf_type_ret;
   char acName[10000];  /* old: 1000 */
   char *pc1, *pc2;
      
   for( pc1 = name, pc2 = acName; *pc1; pc1++, pc2++ )
   {
      if( *pc1 == ' ' ) *pc2 = '_';
      else              *pc2 = *pc1;
   }

   *pc2 = 0;

   paf_type_ret = Get_symbol( scope_global
                            , scope
                            , acName
                            , arg_list
                            , scope_ret
                            , type_ret
                            , define_ret /* 16.02.98 rigo */
                            , exact );

   /* ha nem talaltuk, akkor megegyszer megkerdezzuk template arglist nelkul */
   if( paf_type_ret == 0 )
   {
      if( scope )
      {
         char *pcTemplateArg;

         if(( pcTemplateArg = strchr( scope, '<' )))
         {
            *pcTemplateArg = 0;

            paf_type_ret = Get_symbol( scope_global
                                     , scope
                                     , acName
                                     , arg_list
                                     , scope_ret
                                     , type_ret
                                     , define_ret /* 16.02.98 rigo */
                                     , exact );

            *pcTemplateArg = '<';
         }
      }
   }

   return paf_type_ret;
}

extern int Get_symbol( char *scope_global, char *scope, char *name, char *arg_list, char *scope_ret, char *type_ret, char *define_ret, int exact )
{
   int retval;

   retval = get_symbol( scope_global
                      , scope
                      , name
                      , arg_list
                      , scope_ret
                      , type_ret
                      , define_ret	/* 16.02.98 rigo */
                      , exact );

   if( pf )
   {
      fprintf( pf, "get_symbol : |%s|%s|%s|%s|%d| --> %s|%s|%s|%s|\n"
            , null_safe( scope_global )
            , null_safe( scope )
            , null_safe( name )
            , null_safe( arg_list )
            , exact
            , paf_type_to_string( retval )
            , null_safe( scope_ret )
            , null_safe( type_ret )
            , null_safe( define_ret ) /* 16.02.98 rigo */
            );
   }

#if 0
   printf( "get_symbol : |%s|%s|%s|%s|%d| --> %s|%s|%s|%s|\n"
         , null_safe( scope_global )
         , null_safe( scope )
         , null_safe( name )
         , null_safe( arg_list )
         , exact
         , paf_type_to_string( retval )
         , null_safe( scope_ret )
         , null_safe( type_ret )
         , null_safe( define_ret ) /* 16.02.98 rigo */
         );
#endif

   return retval;
}

extern int Get_class_or_typedef( char *name, char *type_ret )
{
   int retval;

   retval = get_class_or_typedef( name, type_ret );

   if( pf )
   {
      fprintf( pf, "get_class  : |%s| --> %s|%s|\n"
            , null_safe( name )
            , paf_type_to_string( retval )
            , null_safe( type_ret )
            );
   }

#if 0
   printf( "get_class  : |%s| --> %s|%s|\n"
         , null_safe( name )
         , paf_type_to_string( retval )
         , null_safe( type_ret )
         );
#endif

   return retval;
}

extern void Put_cross_ref( int type, int scope_type, int scope_lev, char *fnc_cls, char *fnc, char *fnc_arg_types, char *scope, char *what, char *arg_types, char *file, int lineno, int acc )
{

    put_cross_ref( type
                 , scope_type
                 , scope_lev
                 , fnc_cls
                 , fnc
                 , fnc_arg_types
                 , scope
                 , what
                 , arg_types
                 , file
                 , lineno
                 , acc );
#ifdef TEST
     printf( "put_cross  : |%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%d|%d|\n"
               , paf_type_to_string( type )
               , paf_type_to_string( scope_type )
               , paf_type_to_string( scope_lev )
               , null_safe( fnc_cls )
               , null_safe( fnc )
               , null_safe( fnc_arg_types )
               , null_safe( scope )
               , null_safe( what )
               , null_safe( arg_types )
               , null_safe( file )
               , lineno
               , acc
               );
#endif /*TEST*/
   if( pf )
   {
      fprintf( pf, "put_cross  : |%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%d|%d|\n"
               , paf_type_to_string( type )
               , paf_type_to_string( scope_type )
               , paf_type_to_string( scope_lev )
               , null_safe( fnc_cls )
               , null_safe( fnc )
               , null_safe( fnc_arg_types )
               , null_safe( scope )
               , null_safe( what )
               , null_safe( arg_types )
               , null_safe( file )
               , lineno
               , acc
               );
   }
}

char *paf_type_to_string( int paf_type )
{
   char *atype;
   static char acType[100];   /* old: 100 */

   switch( paf_type )
   {
   case  PAF_FILE              : atype = "FILE          "; break;
   case  PAF_TYPE_DEF          : atype = "TYPE_DEF      "; break;
   case  PAF_CLASS_DEF         : atype = "CLASS_DEF     "; break;
/* case  PAF_STRUCT_DEF        : atype = "STRUCT_DEF    "; break; */
   case  PAF_MBR_FUNC_DEF      : atype = "MBR_FUNC_DEF  "; break;
   case  PAF_MBR_VAR_DEF       : atype = "MBR_VAR_DEF   "; break;
   case  PAF_ENUM_DEF          : atype = "ENUM_DEF      "; break;
   case  PAF_CONS_DEF          : atype = "CONS_DEF      "; break;
   case  PAF_MACRO_DEF         : atype = "MACRO_DEF     "; break;
   case  PAF_FUNC_DEF          : atype = "FUNC_DEF      "; break;
   case  PAF_SUBR_DEF          : atype = "SUBR_DEF      "; break;
   case  PAF_GLOB_VAR_DEF      : atype = "GLOB_VAR_DEF  "; break;
   case  PAF_COMMON_DEF        : atype = "COMMON_DEF    "; break;
   case  PAF_COMMON_MBR_VAR_DEF: atype = "COMMON_MBR    "; break;
   case  PAF_CLASS_INHERIT     : atype = "CLASS_INHERIT "; break;
   case  PAF_FILE_SYMBOLS      : atype = "FILE_SYMBOLS  "; break;
   case  PAF_CROSS_REF_BY      : atype = "CROSS_REF_BY  "; break;
   case  PAF_CROSS_REF         : atype = "CROSS_REF     "; break;
   case  PAF_MBR_FUNC_DCL      : atype = "MBR_FUNC_DCL  "; break;
   case  PAF_FUNC_DCL          : atype = "FUNC_DCL      "; break;
   case  PAF_ENUM_CONST_DEF    : atype = "ENUM_CONST_DEF"; break;
/* case  PAF_UNION_DEF         : atype = "UNION_DEF     "; break; */
   case  PAF_NAMESPACE_DEF     : atype = "NAMESPACE_DEF "; break;
   case  PAF_EXCEPTION_DEF     : atype = "EXCEPTION_DEF "; break;
   case  PAF_LOCAL_VAR_DEF     : atype = "LOCAL_VAR_DEF "; break;
   case  PAF_VAR_DCL           : atype = "VAR_DCL       "; break;
   case  PAF_INCLUDE_DEF       : atype = "INCLUDE_DEF   "; break;
   case  PAF_COMMENT_DEF       : atype = "COMMENT_DEF   "; break;
   case  PAF_FRIEND_DCL        : atype = "FRIEND_DCL    "; break;
   case  PAF_REF_UNDEFINED     : atype = "REF_UNDEFINED "; break;
    default:
      sprintf( acType, "%3d           ", paf_type );
      atype = acType;
      break;
   }

   return atype;
}

extern char *f_NameFromType( Type_t Type )
{
   char *name;

   if( Type->Name )
   {
      name = Type->Name->pcName;
   }
   else if( Type->Class )
   {
      if( Type->Class->Name )
         name = Type->Class->Name->pcName;
      else
         name = 0;
   }
   else if( Type->Enum )
   {
      if( Type->Enum->Name )
         name = Type->Enum->Name->pcName;
      else
         name = 0;
   }
   else
   {
      name = 0;
   }

   if( name )
   {
      name = SN_StrDup( name );
   }

   return name;
}

extern char *f_NameFromDeclaration( Declaration_t Declaration )
{
   char *name;

   if( Declaration->Name )
   {
      name = Declaration->Name->pcName;
   }
   else if( Declaration->Class )
   {
      if( Declaration->Class->Name )
         name = Declaration->Class->Name->pcName;
      else
         name = 0;
   }
   else if( Declaration->Enum )
   {
      if( Declaration->Enum->Name )
         name = Declaration->Enum->Name->pcName;
      else
         name = 0;
   }
   else
   {
      name = 0;
   }

   if( name )
   {
      name = SN_StrDup( name );
   }

   return name;
}


static Type_t f_OperatorCall2( char *pcOperator, Type_t Type1, Type_t Type2, int access, int lineno )
{
   Type_t TypeReturn;
   char acName[10000];  /* old: 1000 */
   char acScope[10000]; /* old: 1000 */
   char acType[10000];  /* old: 1000 */
   char acDefine[10000];  /* 16.02.98 rigo */
   char acArglist[10000];  /* old: 1000 */
   int paf_type_ret;

   sprintf( acName, "operator%s", pcOperator );
   acArglist[0] = 0;

   f_TypeToString( Type1, acArglist, 0 );
   strcat( acArglist, "," );
   f_TypeToString( Type2, acArglist + strlen( acArglist ), 0 );

   /* itt nem a my_Get_symbol-t hivjuk, mert nincs template */
   if(( paf_type_ret = Get_symbol( scope_g
                                  , 0
                                  , acName
                                  , acArglist
                                  , acScope
                                  , acType
                                  , acDefine /* 16.02.98 rigo */
                                  , 1 /* exact */ )))
   {
      int paf_access = 0;

      if( access & WRITE ) paf_access |= PAF_REF_WRITE | PAF_REF_READ;
      if( access & READ  ) paf_access |= PAF_REF_READ ;
/*    if( pass           ) paf_access |= PAF_REF_PASS ; */

      Put_cross_ref( paf_type_ret
                   , scope_g[0] ? PAF_MBR_FUNC_DEF : PAF_FUNC_DEF
                   , PAF_REF_SCOPE_GLOBAL
                   , scope_g
                   , sym_name_g
                   , arg_types_g
                   , acScope
                   , acName
                   , acArglist
                   , filename_g
                   , lineno
                   , paf_access
                   );

      TypeReturn = f_TypeFromString( acType );
   }
   else
   {
      TypeReturn = 0;
   }

   return TypeReturn;
}

