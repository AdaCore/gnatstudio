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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <tcl.h>

#include "crossrefP.h"

/* Valid debugging macros for this file:
	#define PRINT_STATEMENT_DEFAULT
	#define BREAK_BY_STATEMENT_DEFAULT
	#define SYMTAB_TRACE
*/

static int test = 0;

#define xprintf if( test ) printf

static void CompCreate( void );
static void CompDestroy( void );

static Boolean_t compound_statement( void );
static Boolean_t statement( void );
static Boolean_t case_statement( void );
static Boolean_t default_statement( void );
static Boolean_t if_statement( void );
static Boolean_t else_statement( void );
static Boolean_t switch_statement( void );
static Boolean_t while_statement( void );
static Boolean_t do_statement( void );
static Boolean_t for_statement( void );
static Boolean_t break_statement( void );
static Boolean_t continue_statement( void );
static Boolean_t return_statement( void );
static Boolean_t throw_statement( void );
static Boolean_t goto_statement( void );
static Boolean_t labeled_statement( void );
static Boolean_t expression_statement( void );
static Boolean_t declaration_statement1( void );   /* csak logikus
                                                      konstrukciokat fogad el */
static Boolean_t declaration_statement2( void );   /* minden 
                                                      konstrukciot elfogad */
static Boolean_t try_statement( void );
static Boolean_t handler_list( void );
static Boolean_t handler( void );
static char *my_strchr( char *pc, char c );


extern Boolean_t f_CompoundStatement( char *types, char *names )
{
   int retval;

/* printf( "Compund Statement: (%s) (%s)\n", types, names ); */

   if( SymtabVariable == 0 )
   {
      SymtabVariable = SymtabCreate((void (*)(void*)) f_TypeDestroy );
   }

   if( SymtabClass == 0 )
   {
      SymtabClass = SymtabCreate((void (*)(void*)) f_ClassDestroy );
   }

   if( types && names )
   {
      /* feldolgozzuk a fuggveny argumentum listat */

      char *my_types, *types_beg, *types_end;
      char *my_names, *names_beg, *names_end;
      int iBreak;

      my_types = SN_StrDup( types );
      my_names = SN_StrDup( names );

      types_beg = my_types;
      names_beg = my_names;
      iBreak = False;

      while( ! iBreak )
      {
         if(( types_end = my_strchr( types_beg, ',' ))) { *types_end = 0; }
         else                                           { iBreak = True; }

         if(( names_end = my_strchr( names_beg, ',' ))) { *names_end = 0; }
         else                                           { iBreak = True; }

         if( *names_beg && types_beg[0] != '.' )
         {
            Type_t Type;

            Type = f_TypeFromString( types_beg );

            if( Type == 0 )
            {
/* 24.02.97 rigo */
/* int DEFUN(_IO_fflush, (fp), register _IO_FILE *fp ){} eseten elofordul */
/*             f_InternalError( 61 ); */
               Type = f_TypeCreateInt();
            }

            if( Type->Declarator == 0 )
            {
               Type->Declarator = f_DeclaratorCreate();
               Type->Declarator->Name = f_NameCreate( names_beg );
            }
            else
            {
               if( Type->Declarator->Name )
               {
                  f_InternalError( 62 );
               }
               else
               {
                  Type->Declarator->Name = f_NameCreate( names_beg );
               }
            }

            /* felodjuk az esetleges typedef-eket */
            Type = f_TypeBasic( Type, start_lineno_g - 1 );

            if( SymtabInsert( SymtabVariable
                            , Type->Declarator->Name->pcName
                            , (void *) Type
                            , niveauComp ))
            {
#ifdef SYMTAB_TRACE
               char acType[10000];  /* old: 1000 */

               f_TypeToString( Type, acType, 1 );
               printf( "argument type: %s ----- name: %s\n"
                     , acType
                     , Type->Declarator->Name->pcName
                     );
#endif
            }
            else
            {
               f_TypeDestroy( Type );
            }
         }

         if( ! iBreak )
         {
            types_beg = types_end + 1;
            names_beg = names_end + 1;
         }
      }

      ckfree( my_names );
      ckfree( my_types );
   }

   retval = compound_statement();

#ifdef SYMTAB_TRACE
   printf( "vor Clear\n" );
   SymtabPrint( SymtabVariable, 0 );
#endif

   SymtabClear( SymtabVariable );
   SymtabClear( SymtabClass    );

#ifdef SYMTAB_TRACE
   printf( "after Clear\n" );
   SymtabPrint( SymtabVariable, 0 );
#endif

   return retval;
}

static Boolean_t compound_statement( void )
{
   int lineno_beg;
   int charno_beg;
   int niveau_a;
   Save();
   niveau++;

   if( token( 0 ) != '{' )
   {
      Restore();
      niveau--;
      return False;
   }

   CompCreate();

   step( 1 );

   while( True )
   {
      if( token( 0 ) == '}' )
      {
         step( 1 );
         CompDestroy();
         niveau--;
         return True;
      } 

      if( token( 0 ) == 0 )
      {
         CompDestroy();
         niveau--;
         return True;
      }

      niveau_a = niveau;
      lineno_beg = f_lineno( 0 );
      charno_beg = f_charno( 0 );
      statement();
      if( niveau_a != niveau )
      {
         printf( "different niveau by statement: %s:%d.%d - %d.%d\n"
               , filename_g
               , lineno_beg
               , charno_beg
               , f_lineno( -1 )
               , f_charno( -1 ) + identlen( -1 )
               );
      }
   }
}

static Boolean_t statement( void )
{
/*    printf( "statement: %s\n", ident( 0 )); */
   switch( token( 0 ))
   {
   case ';'         : step( 1 ); return True;
   case '{'         : return compound_statement();
   case SN_CASE     : return case_statement();
   case SN_DEFAULT  : return default_statement();
   case SN_IF       : return if_statement();
   case SN_ELSE     : return else_statement();
   case SN_SWITCH   : return switch_statement();
   case SN_WHILE    : return while_statement();
   case SN_DO       : return do_statement();
   case SN_FOR      : return for_statement();
   case SN_BREAK    : return break_statement();
   case SN_CONTINUE : return continue_statement();
   case SN_RETURN   : return return_statement();
   case SN_THROW    : return throw_statement();
   case SN_GOTO     : return goto_statement();
   case SN_TRY      : return try_statement();

   case SN_IDENTIFIER:
      if( labeled_statement     ()) return True;
      if( declaration_statement1()) return True;
      if( expression_statement  ()) return True;
      if( declaration_statement2()) return True;
      step( 1 );
      return False;

   case SN_CHAR     :
   case SN_SHORT    :
   case SN_INT      :
   case SN_LONG     :
   case SN_SIGNED   :
   case SN_UNSIGNED :
   case SN_FLOAT    :
   case SN_DOUBLE   :
   case SN_BOOL     :
   case SN_VOID     :
      if( declaration_statement1()) return True;
      if( expression_statement  ()) return True;
      if( declaration_statement2()) return True;
      step( 1 );
      return False;

   case SN_ASM      :
   case SN_TEMPLATE :
   case SN_NAMESPACE:
   case SN_USING    :
   case SN_AUTO     :
   case SN_REGISTER :
   case SN_EXTERN   :
   case SN_STATIC   :
   case SN_INLINE   :
   case SN_VIRTUAL  :
   case SN_CONST    :
   case SN_VOLATILE :
   case SN_CLASS    :
   case SN_STRUCT   :
   case SN_UNION    :
   case SN_ENUM     :
   case SN_FRIEND   :
   case SN_TYPEDEF  :
      if( declaration_statement1()) return True;
      if( declaration_statement2()) return True;
      f_StepTo( ';', '}', 0 );
      return False;

   case SN_SIZEOF :
   case SN_NEW :
   case SN_DELETE :
   case SN_THIS :
   case SN_OPERATOR :
   case SN_STRINGliteral :
   case SN_FLOATINGconstant :
   case SN_INTEGERconstant :
   case SN_LONGconstant :
   case SN_CHARACTERconstant :
   case SN_ICR :
   case SN_DECR :
   case SN_CLCL :
   case '(':
   case '~':
   case '*':
   case '&':
   case '+':
   case '-':
   case '!':
      if( expression_statement ()) return True;
      step( 1 );
      return False;

   case 0:
      printf( "unexpected end of file\n" );
      return False;

   default:
#ifdef PRINT_STATEMENT_DEFAULT
      printf( "statement: default: %4d %s file: %s:(%d.%d)\n"
            , token( 0 )
            , ident( 0 )
            , filename_g
            , f_lineno( 0 )
            , f_charno( 0 )
            );
#endif
#ifdef BREAK_BY_STATEMENT_DEFAULT
      exit( -1 );
#endif
      if( declaration_statement1()) return True;
      if( expression_statement  ()) return True;
      if( declaration_statement2()) return True;
      step( 1 );
      return False;

/*    case SN_ARROW : */
/*    case SN_LS : */
/*    case SN_RS : */
/*    case SN_LE : */
/*    case SN_GE : */
/*    case SN_EQ : */
/*    case SN_NE : */
/*    case SN_ANDAND : */
/*    case SN_OROR : */
/*    case SN_ELLIPSIS : */
/*    case SN_DOTstar : */
/*    case SN_ARROWstar : */
/*    case SN_MULTassign : */
/*    case SN_DIVassign : */
/*    case SN_MODassign : */
/*    case SN_PLUSassign : */
/*    case SN_MINUSassign : */
/*    case SN_LSassign : */
/*    case SN_RSassign : */
/*    case SN_ANDassign : */
/*    case SN_ERassign : */
/*    case SN_ORassign : */
/*    case '=' : */
/*    case '|' : */
/*    case '^' : */
/*    case '&' : */
/*    case '<' : */
/*    case '>' : */
/*    case '/' : */
/*    case '%' : */
/*    case ')' : */
/*    case '[' : */
/*    case ']' : */
/*    case '.' : */
/*    case ',' : */
/*    case '{' : */
/*    case '}' : */
/*    case '?' : */
/*    case ':' : */
   }
}

static Boolean_t case_statement( void )
{
   step( 1 );

   f_constant_expression();

   if( token( 0 ) == ':' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t default_statement( void )
{
   step( 1 );

   if( token( 0 ) == ':' )
   {
      step( 1 );
   }
   return True;
}

static Boolean_t if_statement( void )
{
   niveau++;
   step( 1 );

   if( token( 0 ) == '(' )
   {
      step( 1 );
   }

   f_expression();

   if( token( 0 ) == ')' )
   {
      step( 1 );
   }

   niveau--;
   return True;
}

static Boolean_t else_statement( void )
{
   step( 1 );
   return True;
}

static Boolean_t switch_statement( void )
{
   niveau++;

   step( 1 );

   if( token( 0 ) == '(' )
   {
      step( 1 );
   }

   f_expression();

   if( token( 0 ) == ')' )
   {
      step( 1 );
   }

   niveau--;
   return True;
}

static Boolean_t while_statement( void )
{
   niveau++;
   step( 1 );

   if( token( 0 ) == '(' )
   {
      step( 1 );
   }

   f_expression();

   if( token( 0 ) == ')' )
   {
      step( 1 );
   }

   niveau--;
   return True;
}

static Boolean_t do_statement( void )
{
   step( 1 );
   return True;
}

static Boolean_t for_statement( void )
{
   niveau++;
   step( 1 );

   if( token( 0 ) == '(' )
   {
      step( 1 );
   }

   if( ! declaration_statement1())
   {
      expression_statement();
   }

   f_expression();

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   f_expression();

   if( token( 0 ) == ')' )
   {
      step( 1 );
   }

   niveau--;
   return True;
}

static Boolean_t break_statement( void )
{
   step( 1 );

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t continue_statement( void )
{
   step( 1 );

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t return_statement( void )
{
   step( 1 );

   f_expression();

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t throw_statement( void )
{
   step( 1 );

   if( token( 0 ) != ';' )
   {
      f_assignment_expression();
   }

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t goto_statement( void )
{
   step( 1 );

   if( token( 0 ) == SN_IDENTIFIER )
   {
      step( 1 );
   }

   if( token( 0 ) == ';' )
   {
      step( 1 );
   }

   return True;
}

static Boolean_t labeled_statement( void )
{
   Save();

   step( 1 );

   if( token( 0 ) == ':' )
   {
      step( 1 );
      return True;
   }
   else
   {
      Restore();
      return False;
   }
}

static Boolean_t expression_statement( void )
{
   Save();

   if( f_expression())
   {
      if( token( 0 ) == ';' )
      {
         step( 1 );
      }
      else
      {
#ifdef CORVEX
         printf( "expected ';' at end of expression statement: %s:%d.%d\n"
               , filename_g
               , f_lineno( 0 )
               , f_charno( 0 )
               );
#endif
      }
      return True;
   }

   Restore();
   return False;
}

static Boolean_t declaration_statement1( void )
{
   Declaration_t Declaration;
   int record;

   Save();

   xprintf( "declaration_statement1\n" );

   if(( Declaration = f_Declaration( LEVEL_1 )))
   {
      xprintf( "declaration_statement1: OK: %s\n", ident( 0 ));
      f_DeclarationProcess( Declaration, record = True );
      f_DeclarationDestroy( Declaration );
      return True;
   }
   else
   {
      xprintf( "declaration_statement1: 0\n" );
      Restore();
      return False;
   }
}

static Boolean_t declaration_statement2( void )
{
   Declaration_t Declaration;
   int record;

   Save();

   xprintf( "declaration_statement2\n" );

   if(( Declaration = f_Declaration( LEVEL_2 )))
   {
      xprintf( "declaration_statement2: OK: %s\n", ident( 0 ));
      f_DeclarationProcess( Declaration, record = True );
      f_DeclarationDestroy( Declaration );
      return True;
   }
   else
   {
      xprintf( "declaration_statement2: 0\n" );
      Restore();
      return False;
   }
}

static Boolean_t try_statement( void )
{
   step( 1 );

   compound_statement();
   handler_list();

   return True;
}

static Boolean_t handler_list( void )
{
   Save();

   if( ! handler())
   {
      Restore();
      return False;
   }

   while( handler())
   {
   }

   return True;
}

static Boolean_t handler( void )
{
   if( token( 0 ) == SN_CATCH )
   {
      step( 1 );
      if( token( 0 ) == '(' )
      {
         int i = 1;

         step( 1 );

         while( i > 0 )
         {
            switch( token( 0 ))
            {
            case '(': i++; break;
            case ')': i--; break;
            }
            step( 1 );
         }
      }
      compound_statement();
      return True;
   }
   else
   {
      return False;
   }
}

static void CompCreate( void )
{
   Comp_t Comp = (Comp_t) Malloc( sizeof( Comp[0] ));

   niveauComp++;
   Comp->CompParent = CompAct;

   CompAct = Comp;
}

static void CompDestroy( void )
{
   Comp_t Comp = CompAct;

   CompAct = Comp->CompParent;
   niveauComp--;

   SymtabNiveauDestroy( SymtabVariable, niveauComp );
   SymtabNiveauDestroy( SymtabClass   , niveauComp );

   ckfree( (char*)Comp );
}

/* az annyival okosabb. mint az strchr, hogy a <...> -on belul nem keres */

static char *my_strchr( char *pc, char c )
{
   int in = 0;

   while( True )
   {
      if( *pc == 0 ) return 0;
      if( ! in && *pc == c ) return pc;
      if( *pc == '<' ) in++;
      if( *pc == '>' ) in--;

      pc++;
   }
}


