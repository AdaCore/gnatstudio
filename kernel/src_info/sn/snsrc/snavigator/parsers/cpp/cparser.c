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

#define VERSION_1
#undef  VERSION_COMPARE
#undef  PP_TEST
#undef  FUNCTION_MACRO
#undef  TRACE           /* ez irja ki a tokeneket */

#undef TRACE_FILE       /* ezt kell bekapcsolni, hogy a put_symbol jelentesek
                           bekeruljenek a 'cbrowser.trace' file-ba */
#undef ARRAY_TEST
#undef BETTER_TEST      /* azert, hogy a variable declaration is piros legyen */

#undef PRINT_TOKEN_LIST

#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <ctype.h>

#include "cplex.h"
#include "longstr.h"
#include "cpdefines.h"
#include "sn.h"

#define null_safe(x) ((x)==0?"":(x))
#define False 0
#define True (!False)

#define Return( x ) { retval = x; goto return_label; }

#define ANONYM "anonymous"

#ifndef FUNCTION_MACRO
   #define step(i) (iva+=(i))
   #define token(i) ((ivt<=iva+(i)?f_read(i),0:0), keyw_cpp ? v[(iva+(i))&ZZMASK].yytoken_cpp : v[(iva+(i))&ZZMASK].yytoken )
   #define ident(i)      ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yytext)
   #define identleng(i)  ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yytext.leng)
   #define f_lineno(i)   ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yylineno)
   #define f_charno(i)   ((ivt<=iva+(i)?f_read(i),0:0), v[(iva+i)&ZZMASK].yycharno)
#endif

#define Save_d()    int iva_save = iva
#define Restore_d() iva = iva_save

#define GetStatus( depth ) sIfstack[depth].status
#define IfstackStatusMax( depth ) sIfstack[depth].status_max
#define IfstackElse( depth ) sIfstack[depth]._else
#define IfstackUndef( depth ) sIfstack[depth].undef

#define DECLARATION_UNKNOWN               0
#define DECLARATION_ASM                   1
#define DECLARATION_TEMPLATE              2
#define DECLARATION_LINKAGE_SPECIFICATION 3
#define DECLARATION_NAMESPACE             4
#define DECLARATION_USING                 5
#define DECLARATION_OBJECT                6

#define LBRACE '{'
#define RBRACE '}'

#define FUNCTION 1
#define POINTER  2
#define ARRAY    3

#define my_strdup(x) (((x)==0)?0:SN_StrDup(x))
#define my_free(x)   if(x)ckfree(x)

/* #define LongStringsMyInit( plstr ) (LongStringInit(plstr,-1),LongStringMyCopy(plstr,"",-1)) */

#define LongStringsMyCopy( plstr1, plstr2 ) (plstr1)->copy((plstr1),(plstr2)->buf, (plstr2)->len )
#define LongStringMyCopy( plstr, pc ) (plstr)->copy((plstr), pc, -1 )
#define LongStringsMyAppend( plstr1, plstr2 ) (plstr1)->append((plstr1),(plstr2)->buf, (plstr2)->len )
#define LongStringMyAppend( plstr, pc ) (plstr)->append((plstr), pc, -1 )
#define LongStringMyFree( plstr ) (plstr)->free( plstr );
#define LongStringIdCopy( plstr, ident ) { sString_t sString = ident; (plstr)->copy((plstr), sString.text, sString.leng ); }
#define LongStringIdAppend( plstr, ident ) { sString_t sString = ident; (plstr)->append((plstr), sString.text, sString.leng ); }
#define LongStringMySetLen( plstr, l ) (((plstr)->buf ? (plstr)->buf[l] = 0 : 0), (plstr)->len = (l))

extern FILE *cross_ref_fp;

typedef struct sDeclaration sDeclaration_t, *Declaration_t;
typedef struct sDeclarator  sDeclarator_t , *Declarator_t ;
typedef struct sClass       sClass_t      , *Class_t      ;
typedef struct sEnum        sEnum_t       , *Enum_t       ;
typedef struct sArray       sArray_t      , *Array_t      ;

struct sArray
{
   char *string;
   int changed;
};

struct sClass
{
   Class_t ClassParent;
   LongString name;
   int access;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
};

struct sEnum      /* struct for enumeration */
{
   LongString name;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
};

struct sDeclaration  /* struct for declaration */
{
   Declaration_t Declaration;
   int type;
   LongString name;     /* space for namespace_name or template_args */
   int storage_class;
   int fct_specifier;
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
   LongString type_name;   /* space for typedef_name or class_name to message */
   int type_of_type_name;  /* SN_CLASS or SN_STRUCT or SN_UNION or SN_ENUM or SN_INTERFACE */
   LongString complete_class_name;  /* space for complete_class_name */
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   sClass_t sClass;
   sEnum_t sEnum;
};

struct sDeclarator
{
   LongString name;
   LongString type;
   LongString types; /* falls function definition */
   LongString names; /* falls function definition */
   int base_typ;
   int pure;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   int name_lineno_beg;
   int name_charno_beg;
};

static int depth;

struct sIfstack
{
   int status;
   int niveau;
   int status_max;
   int undef;
   int _else;
};

static struct sIfstack sIfstack[1000];

#define ZZSIZE (0x10000)
#define ZZMASK ( 0xffff)

typedef struct
{
   int yytoken;
   int yytoken_cpp;
   sString_t yytext;
   int yyleng;
   int yylineno;
   int yycharno;
} ZZSTYPE;

char *filename_g;
char *classname_g;
char *funcname_g;
int is_cpp_g;  /* are the cpp keywords highlighted ? */
int keyw_cpp;  /* are the cpp keywords as keywords or identifiers processed ? */
int keyw_cpp_orig;
int keyw_cpp_cnt; /* how many cpp keyword and operator is already getting */
int declaration_try;
int mode_g;
int pass = 1;
int yyfd;
extern FILE *hig_fp;

static ZZSTYPE v[ ZZSIZE ];   /* value stack */

static int ivt;   /* index of first empty */
static int iva;   /* index of 0 */

static int niveau;
static int bPutSymbol_m;

extern int Put_symbol(int type, char *scope, char *sym_name,
		      char *file, int start_lineno, int start_colpos,
		      int end_lineno, int end_colpos,
		      unsigned long attr, char *ret, char *arg_types,
		      char *args, char	*reserved,
		      int start_lineno_highlight,
		      int start_colpos_highlight, 
		      int end_lineno_highlight,
		      int end_colpos_highlight);

extern void declaration( Declaration_t Declaration );
extern int _declaration( Declaration_t Declaration );
extern int asm_declaration( Declaration_t Declaration );
extern int template_declaration( Declaration_t Declaration );
extern int extern_declaration( Declaration_t Declaration );
extern int linkage_specification( Declaration_t Declaration );
extern int using( Declaration_t Declaration );
extern int namespace_definition( Declaration_t Declaration );
extern int obj_declaration( Declaration_t Declaration );
extern int declarator_list( Declaration_t Declaration );
extern int declarator( Declarator_t Declarator );
extern int declarator_paren( Declarator_t Declarator );
extern int declarator_star( Declarator_t Declarator );
extern int declarator_identifier( Declarator_t Declarator );
extern int declarator_operator( Declarator_t Declarator );
extern int declarator_tilde( Declarator_t Declarator );
extern int ptr_operator( LongString *plstr );
extern void cv_qualifier_list_opt( LongString *plstr );
extern int function_or_array_list( Declarator_t Declarator );
extern int f_class( Declaration_t Declaration, Class_t Class );
extern int base_spec( Class_t Class );
extern int base_specifier( Class_t Class );
extern int member_list( Class_t Class );
extern int member( Class_t Class );
extern int member_declaration( Class_t Class );
extern int _member_declaration( Class_t Class );
extern int member_declarator_list( Class_t Class, Declaration_t Declaration );
extern int member_declarator( Declarator_t Declarator );
extern int f_enum( Declaration_t Declaration, Enum_t Enum );
extern int enum_list( Enum_t Enum );
extern int enumerator( Enum_t Enum );
extern int operator_function_name( LongString *plstr );
extern int conversion_type_name( LongString *plstr );
extern int conversion_type_specifier( LongString *plstr );
extern int complete_class_name( LongString *plstr );
extern void qualified_class_name( LongString *plstr );
extern int qualified_name( LongString *plstr, int *plineno, int *pcharno );
extern int function_operator( Declarator_t Declarator );
extern int array_operator( Declarator_t Declarator );
extern void initializer( void );
extern void ctor_initializer( void );
extern void constant_expression( void );
extern int kr_argument_declaration_list( LongString *types, LongString *names );
extern int kr_argument_declaration( Array_t ArrayTypes, Array_t ArrayNames );
extern int kr_argument_declarator_list( Declaration_t Declaration, Array_t ArrayTypes, Array_t ArrayNames );
extern int kr_argument_declarator( Declarator_t Declarator );
extern int kr_argument_declarator_paren( Declarator_t Declarator );
extern int kr_argument_declarator_star( Declarator_t Declarator );
extern int kr_argument_declarator_identifier( Declarator_t Declarator );
extern void compound_statement( void );
extern void variable( Declaration_t Declaration, Declarator_t Declarator );
extern void function( Declaration_t Declaration, Declarator_t Declarator, int lineno_end, int charno_end, int lineno_brace_beg, int charno_brace_beg, char *comment );
extern void class_member( Class_t Class, Declaration_t Declaration, Declarator_t Declarator );
extern void class_method( Class_t Class, Declaration_t Declaration, Declarator_t Declarator, int lineno_end, int charno_end, int lineno_brace_beg, int charno_brace_beg, char *comment );
extern void class_empty_declarator_list( Class_t Class, Declaration_t Declaration );
extern void template_argument_skip( LongString *plstr );
extern int namespace_name( LongString *plstr );
extern void skip_declaration( void );
extern int skip_member_declaration( void );
extern void function_argument_declaration_list( LongString *types, LongString *names );
extern void function_argument_declaration( LongString *types, LongString *names );
extern void function_argument_class( int storage_class, LongString *plstr );
extern void function_argument_enum( int storage_class, LongString *plstr );
extern int function_argument_declarator_one( Declarator_t Declarator );
extern void skip_expression( void );
extern int function_argument_declarator( Declarator_t Declarator );
extern int function_argument_declarator_paren( Declarator_t Declarator );
extern int function_argument_declarator_star( Declarator_t Declarator );
extern int function_argument_declarator_identifier( Declarator_t Declarator );
extern int function_argument_declarator_operator( Declarator_t Declarator );
extern int function_argument_declarator_tilde( Declarator_t Declarator );
extern int function_argument_function_or_array_list( Declarator_t Declarator );
extern int function_argument_function_operator( Declarator_t Declarator );
extern int function_argument_array_operator( Declarator_t Declarator );
extern int function_argument_base_spec( void );
extern unsigned long attr_from_declaration( Declaration_t Declaration );
extern void create_type( LongString *type, Declaration_t Declaration, Declarator_t Declarator );
#ifndef VERSION_1
extern void create_type_argument_list( LongString *type, Declaration_t Declaration, Declarator_t Declarator );
#endif /* VERSION_1 */
extern char *get_scope( char *name );
extern char *get_name( char *name );
extern void put_cross1( int type, char *scope, char *sym_name, char *file, int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr, char *ret, char *types, char *names, char *reserved );
extern int get_struct_name( int *plineno, int *pcharno, sString_t *psString );
extern int is_single_parameter_list( LongString *types );
extern int array_change_type_and_name( Array_t ArrayTypes, Array_t ArrayNames, char *type, char *name );
extern void array_change_not_changed( Array_t ArrayTypes, Array_t ArrayNames );
extern Array_t array_create( LongString *plstr );
extern void array_destroy( Array_t Array );
#ifdef ARRAY_TEST
extern void array_print( Array_t Array );
#endif
extern void array_to_string( LongString *plstr, Array_t Array );
extern int skip_macro( int t );
extern void skip_macro_2( void );  /* 17.11.97 rigo */
extern int skip_member_macro( void );
extern char *paf_type_to_string( int paf_type );
extern char *get_comment( int lineno );
extern char *find_comment( int lineno );
extern void init_comment( void );  /* 07.04.97 rigo */
extern void flush_comment( void );    /* 08.04.97 */

extern char *StringToText( sString_t sString );
extern void SetStatus( int depth, int value );
extern char *identstring( int i );
extern void Put_Macro( Token_t Token );
extern void Put_Include( Token_t Token );

#ifdef FUNCTION_MACRO
extern void step( int i );
extern int token( int i );
extern sString_t ident( int i );
extern int identleng( int i );
extern int f_lineno( int i );
extern int f_charno( int i );
#endif

extern void f_read( int i );
extern void f_read_end( void );
extern void HighLightBeg( void );
extern void HighLight( void );
extern void HighLightEnd( void );

extern void start_parser( char *filename_a, int is_cpp, FILE *pf_hig, int mode )
{
   if( yyfd == -1 )
   {
      return;
   }

   init_comment();   /* 07.04.97 rigo */

   mode_g = mode;
   filename_g = filename_a;
   is_cpp_g = is_cpp;
   keyw_cpp = is_cpp;      /* default keyword processing is equal to
                              default keyword highlighting */
   keyw_cpp_orig = keyw_cpp;
   keyw_cpp_cnt = 0;

   depth = 0;
   ivt = 0;                   /* 12.11.97 rigo */
   iva = 0;                   /* 12.11.97 rigo */
   sIfstack[0].status = True;
   niveau = 0;
   bPutSymbol_m = True;

   /* beolvassuk az egesz file-t */
   if( f_ReadFile( yyfd ))
   {
      return;
   }

   if( mode == -1 )
   {
      Token_t Token;

      while( True )
      {
         int lex;
         Token = yylex();
         lex = Token->lex;
         f_TokenFreeAll( Token );
         if( lex == 0 ) break;
      }
   }
   else
   {
      while( token( 0 ) != 0 )
      {
         declaration( 0 );
         if( niveau != 0 )
         {
#ifdef TEST
            printf( "bad niveau: %d (line:%d)\n", niveau, f_lineno( 0 ));
#endif
         }
      }
   }

   f_read_end();

#ifndef PERFORMANCE_TEST
   f_MacroFreeAll();
#endif
/*    f_TokenCheck(); */
   flush_comment();     /* 08.04.97 */
}

extern void declaration( Declaration_t Declaration )
{
#ifdef rigo
   if( keyw_cpp == keyw_cpp_orig )  /* not yet changes */
#endif
   {
      if( keyw_cpp ) /* we are in cpp mode */
      {
      }

      declaration_try = 1;

/*    printf( "first try: %d mode from: %d.%d\n", keyw_cpp, f_lineno(0), f_charno(0) ); */
      if( ! _declaration( Declaration ))
      {
         keyw_cpp = ! keyw_cpp;
         declaration_try = 2;
/*       printf( "second try: %d mode from: %d.%d\n", keyw_cpp, f_lineno(0), f_charno(0) ); */
         if( _declaration( Declaration ))
         {
#ifdef TEST
            printf( "change mode: %s (%d.%d)\n"
                  , filename_g
                  , f_lineno(0)
                  , f_charno(0)
                  );
#endif
         }
         else
         {
            keyw_cpp = ! keyw_cpp;  /* restore mode */
/*          printf( "third try: %d mode from: %d.%d\n", keyw_cpp, f_lineno(0), f_charno(0) ); */
            declaration_try = 3;
            if( ! _declaration( Declaration ))
            {
               skip_declaration();
            }
         }
      }
   }
#ifdef rigo
   else  /* already changed */
   {
      declaration_try = 3;
      if( ! _declaration( Declaration ))
      {
         skip_declaration();
      }
   }
#endif
}

extern int _declaration( Declaration_t Declaration )
{
   int iRetval;
   sDeclaration_t sDeclaration;

   niveau++;

   sDeclaration.Declaration   = Declaration;
   sDeclaration.type          = DECLARATION_UNKNOWN;
   LongStringInit( &sDeclaration.name, -1 );
   sDeclaration.storage_class = 0;
   sDeclaration.fct_specifier = 0;
   sDeclaration.s_const       = 0;
   sDeclaration.s_volatile    = 0;
   sDeclaration.s_char        = 0;
   sDeclaration.s_short       = 0;
   sDeclaration.s_int         = 0;
   sDeclaration.s_long        = 0;
   sDeclaration.s_signed      = 0;
   sDeclaration.s_unsigned    = 0;
   sDeclaration.s_float       = 0;
   sDeclaration.s_double      = 0;
   sDeclaration.s_bool        = 0;
   sDeclaration.s_void        = 0;
   LongStringInit( &sDeclaration.type_name, -1 );
   sDeclaration.type_of_type_name = 0;
   LongStringInit( &sDeclaration.complete_class_name, -1 );
   sDeclaration.lineno_beg    = 0;
   sDeclaration.charno_beg    = 0;
   sDeclaration.lineno_end    = 0;
   sDeclaration.charno_end    = 0;

   sDeclaration.sClass.ClassParent      = 0;
   LongStringInit( &sDeclaration.sClass.name, -1 );
   sDeclaration.sClass.access           = 0;
   sDeclaration.sClass.lineno_beg       = 0;
   sDeclaration.sClass.charno_beg       = 0;
   sDeclaration.sClass.lineno_end       = 0;
   sDeclaration.sClass.charno_end       = 0;

   LongStringInit( &sDeclaration.sEnum.name, -1 );
   sDeclaration.sEnum.lineno_beg        = 0;
   sDeclaration.sEnum.charno_beg        = 0;
   sDeclaration.sEnum.lineno_end        = 0;
   sDeclaration.sEnum.charno_end        = 0;

   switch( token( 0 ))
   {
   case SN_ASM      : iRetval = asm_declaration     ( &sDeclaration ); break;
   case SN_TEMPLATE : iRetval = template_declaration( &sDeclaration ); break;
   case SN_NAMESPACE: iRetval = namespace_definition( &sDeclaration ); break;
   case SN_USING    : iRetval = using               ( &sDeclaration ); break;
   case SN_EXTERN   : iRetval = extern_declaration  ( &sDeclaration ); break;
   default          : iRetval = obj_declaration     ( &sDeclaration ); break;
   }

   LongStringMyFree( &sDeclaration.name );
   LongStringMyFree( &sDeclaration.type_name );
   LongStringMyFree( &sDeclaration.complete_class_name );
   LongStringMyFree( &sDeclaration.sClass.name );
   LongStringMyFree( &sDeclaration.sEnum.name );
   niveau--;
   return iRetval;
}

extern int asm_declaration( Declaration_t Declaration )
{
   niveau++;
   Declaration->type = DECLARATION_ASM;

   while( True )
   {
      switch( token( 0 ))
      {
      case 0  :
      case ';':
         step( 1 );
         niveau--;
         return True;
         break;
      }
      step( 1 );
   }
}

extern int template_declaration( Declaration_t Declaration )
{
   int t;
   Save_d();

   niveau++;
   Declaration->type = DECLARATION_TEMPLATE;

   step( 1 );

   if(( t = token( 0 )) == SN_CLASS || t == SN_INTERFACE )
   {
/* template class Vector<const CmdLineApp::AppChar *>; miatt */
      step( 1 );
      if( token( 0 ) == SN_IDENTIFIER )
      {
         step( 1 );
      }
   }

   if( token( 0 ) == '<' )
   {
      template_argument_skip( &Declaration->name );
      declaration( Declaration );
      niveau--;
      return True;
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int extern_declaration( Declaration_t Declaration )
{
   int ret;

   niveau++;

   if( token( 1 ) == SN_STRINGliteral )
   {
      ret = linkage_specification( Declaration );
      niveau--;
      return ret;
   }
   else
   {
      ret = obj_declaration( Declaration );
      niveau--;
      return ret;
   }
   niveau--;
}

extern int linkage_specification( Declaration_t Declaration )
{
   int t;
   niveau++;

   Declaration->type = DECLARATION_LINKAGE_SPECIFICATION;

   step( 1 );
   do
   {
      step( 1 );
   } while( token( 0 ) == SN_STRINGliteral );

   if( token( 0 ) == LBRACE )
   {
      step( 1 );
      while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
      {
         declaration( Declaration );
      }
      step( 1 );
   }
   else
   {
      declaration( Declaration );
   }
   niveau--;

   return True;
}

extern int using( Declaration_t Declaration )
{
   Save_d();
   int lineno;
   int charno;

   niveau++;
   Declaration->type = DECLARATION_USING;

   if( token( 1 ) == SN_NAMESPACE )
   {
      step( 1 );
      step( 1 );
      lineno = f_lineno( 0 );
      charno = f_charno( 0 );
      if( namespace_name( &Declaration->name ))
      {
/*          report_using_directive( filename_g */
/*                                , lineno */
/*                                , charno */
/*                                , f_lineno(0) */
/*                                , f_charno(0) */
/*                                , Declaration->name ); */
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      step( 1 );
      lineno = f_lineno( 0 );
      charno = f_charno( 0 );
      if( namespace_name( &Declaration->name ))
      {
/*          report_using_declaration( filename_g */
/*                                   , lineno */
/*                                   , charno */
/*                                   , f_lineno(0) */
/*                                   , f_charno(0) */
/*                                   , Declaration->name ); */
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
   }
   niveau--;
}

extern int namespace_definition( Declaration_t Declaration )
{
   Save_d();
   int lineno;
   int charno;
   int t;

   niveau++;
   Declaration->type = DECLARATION_NAMESPACE;

   if( token( 1 ) == SN_IDENTIFIER )
   {
      lineno = f_lineno( 1 );
      charno = f_charno( 1 );
      step( 1 );
      step( 1 );
      if( token( 0 ) == '=' )
      {
         step( 1 );
         if( namespace_name( &Declaration->name ))
         {
/*             report_namespace_alias_definition( filename_g */
/*                                              , lineno */
/*                                              , charno */
/*                                              , f_lineno(0) */
/*                                              , f_charno(0) */
/*                                              , id */
/*                                              , Declaration->name );  */
         }
         else
         {
            Restore_d();
            niveau--;
            return False;
         }
      }
      else
      {
/*          report_namespace_definition( filename_g */
/*                                     , lineno */
/*                                     , charno */
/*                                     , f_lineno(0) */
/*                                     , f_charno(0) */
/*                                     , id );  */
      }
   }
   else
   {
/*       report_namespace_definition( filename_g */
/*                                  , f_lineno( 0 ) */
/*                                  , f_charno( 0 ) */
/*                                  , f_lineno(0) */
/*                                  , f_charno(0) */
/*                                  , ANONYM ); */
      step( 1 );
   }

   if( token( 0 ) == LBRACE )
   {
      step( 1 );
      while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
      {
         declaration( Declaration );
      }
      step( 1 );
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
   return True;
}

extern int obj_declaration( Declaration_t Declaration )
{
   int retval;
   Save_d();
   int lineno_beg;
   int charno_beg;
   niveau++;

   Declaration->type = DECLARATION_OBJECT;

   while( True )
   {
      switch( token( 0 ))
      {
      case ';'        :
         step( 1 );
         Return( True );

      case SN_INTERFACE  :
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         if( ! f_class( Declaration, &Declaration->sClass ))
         {
            Restore_d();
            Return( False );
         }
         break;

      case SN_ENUM       :
         if( ! f_enum( Declaration, &Declaration->sEnum ))
         {
            Restore_d();
            Return( False );
         }
         break;

      case SN_AUTO       : Declaration->storage_class = SN_AUTO    ; step( 1 ); break;
      case SN_REGISTER   : Declaration->storage_class = SN_REGISTER; step( 1 ); break;
      case SN_STATIC     : Declaration->storage_class = SN_STATIC  ; step( 1 ); break;
      case SN_EXTERN     : Declaration->storage_class = SN_EXTERN  ; step( 1 ); break;
      case SN_INLINE     : Declaration->fct_specifier = SN_INLINE  ; step( 1 ); break;
      case SN_VIRTUAL    : Declaration->fct_specifier = SN_VIRTUAL ; step( 1 ); break;
      case SN_FRIEND     : Declaration->storage_class = SN_FRIEND  ; step( 1 ); break;
      case SN_TYPEDEF    : Declaration->storage_class = SN_TYPEDEF ; step( 1 ); break;
      case SN_CONST      : Declaration->s_const       = True    ; step( 1 ); break;
      case SN_VOLATILE   : Declaration->s_volatile    = True    ; step( 1 ); break;
      case SN_CHAR       : Declaration->s_char        = True    ; step( 1 ); break;
      case SN_SHORT      : Declaration->s_short       = True    ; step( 1 ); break;
      case SN_INT        : Declaration->s_int         = True    ; step( 1 ); break;
      case SN_LONG       : Declaration->s_long        = True    ; step( 1 ); break;
      case SN_SIGNED     : Declaration->s_signed      = True    ; step( 1 ); break;
      case SN_UNSIGNED   : Declaration->s_unsigned    = True    ; step( 1 ); break;
      case SN_FLOAT      : Declaration->s_float       = True    ; step( 1 ); break;
      case SN_DOUBLE     : Declaration->s_double      = True    ; step( 1 ); break;
      case SN_BOOL		 : Declaration->s_bool        = True;   ; step( 1 ); break;
      case SN_VOID       : Declaration->s_void        = True    ; step( 1 ); break;
      case SN_IDENTIFIER :
         /* ha az ident utan * vagy & vagy ident all ( kiveve az 'ident iden (('
            konstrukciot ), akkor nem lehet deklarator_list */
         if(( token( 1 ) == '*' ) ||
            ( token( 1 ) == '&' ) ||
            ( token( 1 ) == SN_IDENTIFIER && ( token( 2 ) != '(' || token( 3 ) != '(' ))
           )
         {
         }
         else
         {
            if( declarator_list( Declaration ))
            {
               Return( True );
            }
         }
         LongStringMySetLen( &Declaration->type_name, 0 );
         Declaration->type_of_type_name = 0;
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         if( ! complete_class_name( &Declaration->type_name ))
         {
            Restore_d();
            Return( False );
         }
         else
         {
            LongStringsMyCopy( &Declaration->complete_class_name
                             , &Declaration->type_name );

            Declaration->lineno_beg = lineno_beg;
            Declaration->charno_beg = charno_beg;
            Declaration->lineno_end = f_lineno( -1 );
            Declaration->charno_end = f_charno( -1 ) + 1;
         }
         break;

      case SN_CLCL       :
         if( declarator_list( Declaration ))
         {
            Return( True );
         }
         LongStringMySetLen( &Declaration->type_name, 0 );
         Declaration->type_of_type_name = 0;
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         if( ! complete_class_name( &Declaration->type_name ))
         {
            Restore_d();
            Return( False );
         }
         else
         {
            LongStringsMyCopy( &Declaration->complete_class_name
                             , &Declaration->type_name );

            Declaration->lineno_beg = lineno_beg;
            Declaration->charno_beg = charno_beg;
            Declaration->lineno_end = f_lineno( -1 );
            Declaration->charno_end = f_charno( -1 ) + 1;
         }
         break;

      case SN_OPERATOR   :
      case '('        :
      case '*'        :
      case '&'        :
      case '~'        :
         if( declarator_list( Declaration ))
         {
            Return( True );
         }
         else
         {
            Restore_d();
            Return( False );
         }
         break;

      default         :
         Restore_d();
         Return( False );
         break;
      }
   }
return_label:
   niveau--;
   return retval;
}

extern int declarator_list( Declaration_t Declaration )
{
   Save_d();
   sDeclarator_t  sDeclarator;
   niveau++;

   LongStringInit( &sDeclarator.name , -1 );
   LongStringInit( &sDeclarator.type , -1 );
   LongStringInit( &sDeclarator.types, -1 );
   LongStringInit( &sDeclarator.names, -1 );

   while( True )
   {
      LongStringMyFree( &sDeclarator.name  );
      LongStringMyFree( &sDeclarator.type  );
      LongStringMyFree( &sDeclarator.types );
      LongStringMyFree( &sDeclarator.names );
      sDeclarator.base_typ = 0;
      sDeclarator.pure     = 0;
      sDeclarator.lineno_beg = 0;
      sDeclarator.charno_beg = 0;
      sDeclarator.lineno_end = 0;
      sDeclarator.charno_end = 0;
      sDeclarator.name_lineno_beg = 0; /* Zsolt Koppany, 18-apr-97 */
      sDeclarator.name_charno_beg = 0; /* Zsolt Koppany, 18-apr-97 */

      if( declarator( &sDeclarator ))
      {
         switch( token( 0 ))
         {
         case '=':
            initializer();
            break;

         case SN_RETURN:   /* gc++: inline Int operator - (Int &a) return r(a) {}; miatt 11.09.96 rigo */
         case SN_THROW:    /* gc++: int m1() throw(char*); miatt 02.10.97 rigo*/
         case ':':
            ctor_initializer();
            break;
         }

         if( token( 0 ) == ',' )
         {
            variable( Declaration, &sDeclarator );
            step( 1 );
            continue;
         }

         if( token( 0 ) == ';' )
         {
            variable( Declaration, &sDeclarator );
            step( 1 );
            goto return_true;
         }

         if( sDeclarator.base_typ == FUNCTION && token( 0 ) != LBRACE )
         {
            if( ! skip_macro( LBRACE ))
            {
               if( ! kr_argument_declaration_list( &sDeclarator.types, &sDeclarator.names ))
               {
/* Az elso nekifutasra False-szal terunk vissza, ami lehetoseget ad arra,
** hogy az ellentetes keyw_cpp ertekkel ujra megprobaljuk az egesz deklaraciot
** feldolgozni .
** A masodik nekifutasra is False-szal terunk vissza, ami lehetoseget ad arra,
** hogy visszaallitsuk a keyw_cpp-t az eredeti ertekre.
** A harmadik nekifutasra True-val terunk vissza.
*/
                  if( declaration_try == 1 || declaration_try == 2 )
                  {
                     goto return_false;
                  }
                  else
                  {
                     goto return_true;
                  }
               }
            }
         }

         if( sDeclarator.base_typ == FUNCTION && token( 0 ) == LBRACE )
         {
            int lineno = f_lineno( 0 );
            int charno = f_charno( 0 );
            char *comment = get_comment( f_lineno( -1 ));
            char *classname_save = classname_g;             /* 08.04.97 */
            char *funcname_save = funcname_g;               /* 08.04.97 */

            classname_g = my_strdup( get_scope( sDeclarator.name.buf )); /* 07.04.97 */
            funcname_g  = my_strdup( get_name ( sDeclarator.name.buf )); /* 07.04.97 */

            compound_statement();
            function( Declaration, &sDeclarator, f_lineno( -1 ), f_charno( -1 ), lineno, charno, comment );

            my_free( classname_g );                         /* 08.04.97 */
            my_free( funcname_g );                          /* 08.04.97 */
            classname_g = classname_save;                   /* 08.04.97 */
            funcname_g = funcname_save;                     /* 08.04.97 */

            goto return_true;
         }
         Restore_d();
         goto return_false;
      }
      Restore_d();
      goto return_false;
   }

return_false:
   niveau--;
   LongStringMyFree( &sDeclarator.name  );
   LongStringMyFree( &sDeclarator.type  );
   LongStringMyFree( &sDeclarator.types );
   LongStringMyFree( &sDeclarator.names );
   return False;

return_true:
   niveau--;
   LongStringMyFree( &sDeclarator.name  );
   LongStringMyFree( &sDeclarator.type  );
   LongStringMyFree( &sDeclarator.types );
   LongStringMyFree( &sDeclarator.names );
   return True;
}

extern int declarator( Declarator_t Declarator )
{
   int ret;
   int len = Declarator->type.len;

   niveau++;

   switch( token( 0 ))
   {
   case '('           :
      ret = declarator_paren( Declarator );
      break;
   case '*'           :
      ret = declarator_star( Declarator );
      break;
   case '&'           :
      ret = declarator_star( Declarator );
      break;

   case SN_IDENTIFIER :
   case SN_CLCL       :
      ret = declarator_identifier( Declarator );
      break;

   case SN_OPERATOR   : ret = declarator_operator  ( Declarator ); break;
   case '~'           : ret = declarator_tilde     ( Declarator ); break;
   default            : ret = False;
   }

   if( ! ret )
   {
      LongStringMySetLen( &Declarator->type, len );
   }

   niveau--;
/*    printf( "declarator return: %d\n", ret ); */
   return ret;
}

extern int declarator_paren( Declarator_t Declarator )
{
   Save_d();
   int len;

   len = Declarator->type.len;

   niveau++;

   LongStringMyAppend( &Declarator->type, "(" );

   step( 1 );
   if( declarator( Declarator ))
   {
      if( token( 0 ) == ')' )
      {
         LongStringMyAppend( &Declarator->type, ")" );
         step( 1 );
         if( function_or_array_list( Declarator ))
         {
            niveau--;
/*             printf( "declarator_paren return: 1\n" ); */
            return True;
         }
         else
         {
            Restore_d();
            LongStringMySetLen( &Declarator->type, len );
            niveau--;
/*             printf( "declarator_paren return: 0\n" ); */
            return False;
         }
      }
      else
      {
         Restore_d();
         LongStringMySetLen( &Declarator->type, len );
         niveau--;
/*          printf( "declarator_paren return: 0\n" ); */
         return False;
      }
   }
   else
   {
      Restore_d();
      LongStringMySetLen( &Declarator->type, len );
      niveau--;
/*       printf( "declarator_paren return: 0\n" ); */
      return False;
   }
   LongStringMySetLen( &Declarator->type, len );
   niveau--;
/*    printf( "declarator_paren return: 0\n" ); */
   return False;
}

extern int declarator_star( Declarator_t Declarator )
{
   Save_d();
   LongString aname;
   int len;
   int retval;

   niveau++;
   LongStringInit( &aname, -1 );

   if( ptr_operator( &aname ))
   {
      LongStringsMyAppend( &Declarator->type, &aname );
      len = Declarator->type.len;
      if( declarator( Declarator ))
      {
         if( Declarator->base_typ == 0 )
         {
            Declarator->base_typ = POINTER;
         }
         Return( True )
      }
      else
      {
         LongStringMySetLen( &Declarator->type, len );
         Restore_d();
         Return( False )
      }
   }
   else
   {
      Restore_d();
      Return( False )
   }

return_label:
   LongStringMyFree( &aname );
   niveau--;
   return retval;
}

extern int declarator_identifier( Declarator_t Declarator )
{
   Save_d();
   int lineno = f_lineno( 0 );
   int charno = f_charno( 0 );
   int t;
   LongString aname;
   int len;

   niveau++;
   LongStringInit( &aname, -1 );

   t = token( 0 );

   if( t == SN_IDENTIFIER )
   {
      t = token( 1 );

      if( t == SN_IDENTIFIER )
      {
         if( token( 2 ) == '(' && token( 3 ) == '(' )
         {
         /* int f ANSI(( int b )); miatt */
         }
         else
         {
         /* const CString& AFXAPI AfxGetEmptyString(); miatt */
            step( 1 );
            lineno = f_lineno( 0 );
            charno = f_charno( 0 );
         }
      }
      else if( token( 1 ) == SN_OPERATOR )
         /* void* AFX_CDECL operator new( void ); miatt */
      {
         step( 1 );
         lineno = f_lineno( 0 );
         charno = f_charno( 0 );
         if( declarator_operator( Declarator ))
         {
            LongStringMyFree( &aname );
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            LongStringMyFree( &aname );
            niveau--;
            return False;
         }
      }
   }

   LongStringMyFree( &aname );

   if( ptr_operator( &aname ))
   {
      LongStringsMyAppend( &Declarator->type, &aname );
      LongStringMyFree( &aname );
      len = Declarator->type.len;
      if( declarator( Declarator ))
      {
         if( Declarator->base_typ == 0 )
         { /* 07.03.97 rigo: int (XX:*a)(); miatt */
            Declarator->base_typ = POINTER;
         }
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->type, len );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else if( qualified_name( &Declarator->name
                          , &Declarator->name_lineno_beg
                          , &Declarator->name_charno_beg ))
   {
      LongStringMyFree( &aname );
      Declarator->lineno_beg = lineno;
      Declarator->charno_beg = charno;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identleng( -1 );
      if( function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->name, 0 );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      LongStringMyFree( &aname );
      LongStringMySetLen( &Declarator->name, 0 );
      Restore_d();
      niveau--;
      return False;
   }
   LongStringMyFree( &aname );
   Restore_d();
   niveau--;
   return False;
}

extern int declarator_operator( Declarator_t Declarator )
{
   int lineno = f_lineno( 0 );
   int charno = f_charno( 0 );
   niveau++;

   if( operator_function_name( &Declarator->name ))
   {
      Declarator->name_lineno_beg = Declarator->lineno_beg = lineno;
      Declarator->name_charno_beg = Declarator->charno_beg = charno;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identleng( -1 );
      if( function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->name, 0 );
         niveau--;
         return False;
      }
   }
   else
   {
      LongStringMySetLen( &Declarator->name, 0 );
      niveau--;
      return False;
   }
}

extern int declarator_tilde( Declarator_t Declarator )
{
   int lineno_beg;
   int charno_beg;
   Save_d();
   niveau++;

   lineno_beg = f_lineno( 0 );
   charno_beg = f_charno( 0 );

   step( 1 );

   if( token( 0 ) == SN_IDENTIFIER )
   {
      Declarator->name_lineno_beg = Declarator->lineno_beg = lineno_beg;
      Declarator->name_charno_beg = Declarator->charno_beg = charno_beg;

      LongStringMyCopy  ( &Declarator->name, "~" );
      LongStringIdAppend( &Declarator->name, ident( 0 ));
      Declarator->lineno_end = f_lineno( 0 );
      Declarator->charno_end = f_charno( 0 ) + identleng( 0 );
      step( 1 );
      if( function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         LongStringMySetLen( &Declarator->name, 0 );
         niveau--;
         return False;
      }
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int ptr_operator( LongString *plstr )
{
   Save_d();
   niveau++;

   switch( token( 0 ))
   {
   case '*'        :
   case '&'        :
      if( plstr )
      {
         LongStringIdAppend( plstr, ident( 0 ));
      }
      step( 1 );
      cv_qualifier_list_opt( plstr );
      niveau--;
/*       printf( "***** ptr operator ***** return True\n" ); */
      return True;
      break;

   case SN_IDENTIFIER :
   case SN_CLCL       :
      if( complete_class_name( plstr ))
      {
         if( token( 0 ) == SN_CLCL && token( 1 ) == '*' )
         {
            step( 1 );
            step( 1 );
            if( plstr )
            {
               LongStringMyAppend( plstr, "::*" );
            }
            cv_qualifier_list_opt( plstr );
            niveau--;
/*             printf( "***** ptr operator ***** return True\n" ); */
            return True;
         }
         else if( token( 0 ) == '*' )
            /* typedef int (__cdecl * _PNH)( size_t ); miatt */
         {
            step( 1 );
            if( plstr )
            {
               LongStringMyAppend( plstr, " *" );
            }
            cv_qualifier_list_opt( plstr );
            niveau--;
/*             printf( "***** ptr operator ***** return True\n" ); */
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
/*             printf( "***** ptr operator ***** return False\n" ); */
            return False;
         }
      }
      else
      {
         Restore_d();
         niveau--;
/*          printf( "***** ptr operator ***** return False\n" ); */
         return False;
      }
      break;

   default         :
      Restore_d();
      niveau--;
/*       printf( "***** ptr operator ***** return False\n" ); */
      return False;
      break;
   }
   Restore_d();
   niveau--;
/*    printf( "***** ptr operator ***** return False\n" ); */
   return False;
}

extern void cv_qualifier_list_opt( LongString *plstr )
{
   int t;
   int iCONST = 0;
   int iVOLATILE = 0;
   niveau++;

   while( True )
   {
      t = token( 0 );

      if( t == SN_CONST )
      {
         iCONST = 1;
      }
      else if( t == SN_VOLATILE )
      {
         iVOLATILE = 1;
      }
      else
      {
         break;
      }

      step( 1 );
   }

   if( plstr )
   {
      if( iCONST    ) { LongStringMyAppend( plstr, "const "    ); }
      if( iVOLATILE ) { LongStringMyAppend( plstr, "volatile " ); }
   }

   niveau--;
}

extern int function_or_array_list( Declarator_t Declarator )
{
   Save_d();
   int i = 0;
   niveau++;

   /* 11.09.96 rigo - int a ANSI(( int b )); miatt */
   if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == '(' && token( 2 ) == '(' )
   {
      step( 1 );
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '('       :
         if( ! function_operator( Declarator ))
         {
            Restore_d();
            niveau--;
            return False;
         }
         if( ++i == 2 )
         {
            Restore_d();
            niveau--;
            return False;
         }
         break;

      case '['       :
         if( ! array_operator( Declarator ))
         {
            Restore_d();
            niveau--;
            return False;
         }
         break;

      default        :
         niveau--;
         return True;
      }
   }
}

extern int function_operator( Declarator_t Declarator )
{
   Save_d();
   int i;
   niveau++;

   step( 1 );

   if( Declarator->base_typ == 0 )
   {
      function_argument_declaration_list( &Declarator->types
                                        , &Declarator->names );
   }
   else
   {
      i = 1;
      while( i > 0 )
      {
         switch( token( 0 ))
         {
         case '(': step( 1 ); i++; break;
         case ')': step( 1 ); i--; break;
         case  0 : 
            Restore_d();
            niveau--;
            return False;
         default : step( 1 ); break;
         }
      }
   }

   while( True )
   {
      int t = token( 0 );

      if( t == SN_CONST || t == SN_VOLATILE )
      {
         step( 1 );
      }
      else
      {
         break;
      }
   }

   if( Declarator->base_typ == 0 )
   {
      Declarator->base_typ = FUNCTION;
   }
   else
   {
      LongStringMyAppend( &Declarator->type, "()" );
   }

   niveau--;
   return True;
}

extern int array_operator( Declarator_t Declarator )
{
   Save_d();
   int i;
   niveau++;

   step( 1 );
   i = 1;

   while( i > 0 )
   {
      switch( token( 0 ))
      {
      case '[': step( 1 ); i++; break;
      case ']': step( 1 ); i--; break;
      case  0 : 
         Restore_d();
         niveau--;
         return False;
      default : step( 1 ); break;
      }
   }

   LongStringMyAppend( &Declarator->type, "[]" );

   if( Declarator->base_typ == 0 )
   {
      Declarator->base_typ = ARRAY;
   }

   niveau--;
   return True;
}

extern int f_class( Declaration_t Declaration, Class_t Class )
{
   Save_d();
   int lineno;
   int charno;
   int paf;
   unsigned long attr;
   char *template_args;
   int t;
   char *comment;
   char *classname_save;         /* 08.04.97 rigo */

   niveau++;

   /* LongStringInit( &Class->name, -1 ); Zsolt Koppany, 30-dec-97 */
   Class->access = 0;
   Class->lineno_beg = f_lineno( 0 );
   Class->charno_beg = f_charno( 0 );
   Class->lineno_end = f_lineno( 0 );
   Class->charno_end = f_charno( 0 ) + identleng( 0 );

   switch( token( 0 ))
   {
   case SN_INTERFACE:
   case SN_CLASS:
      if( Declaration ) Declaration->type_of_type_name = SN_CLASS;
      Class->access = SN_PRIVATE;
      paf  = PAF_CLASS_DEF ;
      attr = 0 ;
      break;
   case SN_STRUCT:
      if( Declaration ) Declaration->type_of_type_name = SN_STRUCT;
      Class->access = SN_PUBLIC;
      paf  = PAF_CLASS_DEF;
      attr = PAF_STRUCT_DEF;
      break;
   case SN_UNION:
      if( Declaration ) Declaration->type_of_type_name = SN_UNION;
      Class->access = SN_PUBLIC;
      paf  = PAF_UNION_DEF ;
      attr = 0 ;
      break;
   }

   step( 1 );

   skip_macro_2();   /* 17.11.97 rigo */

   switch( token( 0 ))
   {
   case SN_IDENTIFIER:

      if( token( 1 ) == SN_IDENTIFIER &&
        ( token( 2 ) == ':' || token( 2 ) == LBRACE || token( 2 ) == '<' ))
      {
         step( 1 );  /* atlepjuk a G_EXP_IMP-et */
      }

      lineno = f_lineno( 0 );
      charno = f_charno( 0 );
      Class->lineno_beg = lineno;
      Class->charno_beg = charno;

      LongStringIdAppend( &Class->name, ident( 0 ));

      step( 1 );

      if( token( 0 ) == '<' )
      {
         template_argument_skip( &Class->name );
      }

      Class->lineno_end = f_lineno( -1 );
      Class->charno_end = f_charno( -1 ) + identleng( -1 );

      if( token( 0 ) == ':' )
      {
         base_spec( Class );
         while(( t = token( 0 )) != LBRACE && t != 0 && t != ';' )
                                                /* t != 0   10.02.97 rigo */
                                                /* t != ';' 17.02.97 rigo */
         {
            step( 1 );
         }
      }

      if( token( 0 ) == LBRACE )
      {
         comment = get_comment( Class->lineno_end );
         classname_save = classname_g;                   /* 08.04.97 */
         classname_g = my_strdup( Class->name.buf );         /* 07.04.97 */
         member_list( Class );
         while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
         {
            step( 1 );
         }

         step( 1 );

         my_free( classname_g );                         /* 08.04.97 */
         classname_g = classname_save;                   /* 08.04.97 */

         if( Declaration &&
             Declaration->Declaration &&
             Declaration->Declaration->name.buf )
         {
            template_args = Declaration->Declaration->name.buf;
         }
         else
         {
            template_args = 0;
         }

         Put_symbol( paf
                   , get_scope( Class->name.buf )
                   , get_name( Class->name.buf )
                   , filename_g
                   , lineno
                   , charno
                   , f_lineno( -1 )
                   , f_charno( -1 ) + 1
                   , (unsigned long) attr
                   , (char *) 0
                   , template_args
                   , (char *) 0
                   , comment
                   , Class->lineno_beg
                   , Class->charno_beg
                   , Class->lineno_end
                   , Class->charno_end
                   );
      }
      if( Declaration )
      {
         LongStringsMyAppend( &Declaration->type_name, &Class->name );
      }
      niveau--;
      return True;

   case ':'       :
      if( Declaration && Declaration->storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( &Class->lineno_beg, &Class->charno_beg, &str_name ))
         {
            Declaration->storage_class = SN_TYPEDEF_TO_CLASS;
            LongStringIdCopy( &Class->name, str_name );
            LongStringsMyAppend( &Declaration->type_name, &Class->name );
            Class->lineno_end = Class->lineno_beg;
            Class->charno_end = Class->charno_beg + Class->name.len;
         }
         else
         {
            LongStringMyCopy( &Class->name, ANONYM );
         }
      }
      else
      {
         LongStringMyCopy( &Class->name, ANONYM );
      }

      base_spec( Class );

      while(( t = token( 0 )) != LBRACE && t != 0 && t != ';' )
                                             /* t != 0   10.02.97 rigo */
                                             /* t != ';' 17.02.97 rigo */
      {
         step( 1 );
      }

      if( token( 0 ) == LBRACE )
      {
         lineno = f_lineno( 0 );
         charno = f_charno( 0 ) + 1;
         comment = get_comment( Class->lineno_end );
         classname_save = classname_g;                /* 08.04.97 */
         classname_g = my_strdup( Class->name.buf );      /* 07.04.97 */
         member_list( Class );
         while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
         {
            step( 1 );
         }
         step( 1 );

         my_free( classname_g );                         /* 08.04.97 */
         classname_g = classname_save;                   /* 08.04.97 */

         Put_symbol( paf
                   , get_scope( Class->name.buf )
                   , get_name( Class->name.buf )
                   , filename_g
                   , lineno
                   , charno
                   , f_lineno( -1 )
                   , f_charno( -1 ) + 1
                   , (unsigned long) attr
                   , (char *) 0
                   , (char *) 0
                   , (char *) 0
                   , comment
                   , Class->lineno_beg
                   , Class->charno_beg
                   , Class->lineno_end
                   , Class->charno_end
                   );
         niveau--;
         return True;
      }
      else  /* ez itt nem class, hanem class nevu bitfield (int class : 5;) */
            /* 17.02.97 rigo */
      {
         Restore_d();
         niveau--;
         return False;
      }

   case LBRACE    :
      if( Declaration && Declaration->storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( &Class->lineno_beg, &Class->charno_beg, &str_name ))
         {
            Declaration->storage_class = SN_TYPEDEF_TO_CLASS;
            LongStringIdCopy( &Class->name, str_name );
            LongStringsMyAppend( &Declaration->type_name, &Class->name );
            Class->lineno_end = Class->lineno_beg;
            Class->charno_end = Class->charno_beg + Class->name.len;
         }
         else
         {
            LongStringMyCopy( &Class->name, ANONYM );
         }
      }
      else
      {
         LongStringMyCopy( &Class->name, ANONYM );
      }
      lineno = f_lineno( 0 );
      charno = f_charno( 0 ) + 1;
      comment = get_comment( Class->lineno_end );
      classname_save = classname_g;                /* 08.04.97 */
      classname_g = my_strdup( Class->name.buf );      /* 07.04.97 */
      member_list( Class );
      while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
      {
         step( 1 );
      }
      step( 1 );

      my_free( classname_g );                      /* 08.04.97 */
      classname_g = classname_save;                /* 08.04.97 */

      Put_symbol( paf
                , get_scope( Class->name.buf )
                , get_name( Class->name.buf )
                , filename_g
                , lineno
                , charno
                , f_lineno( -1 )
                , f_charno( -1 ) + 1
                , (unsigned long) attr
                , (char *) 0
                , (char *) 0
                , (char *) 0
                , comment
                , Class->lineno_beg
                , Class->charno_beg
                , Class->lineno_end
                , Class->charno_end
                );
      niveau--;
      return True;

   default:
      niveau--;
      return True;
   }
   niveau--;
   return True;
}

extern int base_spec( Class_t Class )
{
   Save_d();
   niveau++;

   step( 1 );

   while( base_specifier( Class ))
   {
      switch( token( 0 ))
      {
      case LBRACE:
         niveau--;
         return True;
      case ',':
         step( 1 );
         break;
      default:
         goto end;
         break;
      }
   }

end:
   while( True )
   {
      switch( token( 0 ))
      {
      case LBRACE:
         niveau--;
         return True;
      case ';':      /* 17.02.97 rigo */
      case 0:
         Restore_d();
         niveau--;
         return False;
      }
      step( 1 );
   }
   niveau--;
}

extern int base_specifier( Class_t Class )
{
   LongString aname;
   LongString atemplate_args;
   char *template_args;
   char *pc;
   int lineno;
   int charno;
   unsigned long attr = 0;
   niveau++;

   LongStringInit( &aname, -1 );
   LongStringInit( &atemplate_args, -1 );

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_VIRTUAL   :  attr |= PAF_VIRTUAL  ; step( 1 ); break;
      case SN_PRIVATE   :  attr |= PAF_PRIVATE  ; step( 1 ); break;
      case SN_PROTECTED :  attr |= PAF_PROTECTED; step( 1 ); break;
      case SN_PUBLIC    :  attr |= PAF_PUBLIC   ; step( 1 ); break;
      case SN_CLCL      :
      case SN_IDENTIFIER:
         if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == SN_IDENTIFIER )
         {  /* struct x : PRIVATE y { ... } miatt */
            step( 1 );
         }
         lineno = f_lineno( 0 );
         charno = f_charno( 0 );
         if( complete_class_name( &aname ))
         {
            if( attr == 0 )
            {
               switch( Class->access )
               {
               case SN_PUBLIC : attr = PAF_PUBLIC ; break;
               case SN_PRIVATE: attr = PAF_PRIVATE; break;
               }
            }

            if( aname.buf != 0 && ( pc = strchr( aname.buf, '<' )))
            {
               LongStringMyCopy( &atemplate_args, pc );
               template_args = atemplate_args.buf;
               *pc = 0;
            }
            else
            {
               template_args = 0;
            }

            Put_symbol( PAF_CLASS_INHERIT
                      , Class->name.buf
                      , aname.buf
                      , filename_g
                      , lineno
                      , charno
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      , attr
                      , (char *) 0
                      , (char *) 0
                      , template_args
                      , get_comment( f_lineno( 0 ))
                      , lineno
                      , charno
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      );
            LongStringMyFree( &aname );
            LongStringMyFree( &atemplate_args );
            niveau--;
            return True;
         }
         else
         {
            LongStringMyFree( &aname );
            LongStringMyFree( &atemplate_args );
            niveau--;
            return False;
         }
         break;
      case 0:
      default:       /* 10.02.97 rigo */
         LongStringMyFree( &aname );
         LongStringMyFree( &atemplate_args );
         niveau--;
         return False;
         break;
      }
   }
}

extern int member_list( Class_t Class )
{
   niveau++;
   step( 1 );

   while( member( Class ))
   {
   }

   niveau--;
   return True;
}

extern int member( Class_t Class )
{
#ifndef VERSION_COMPARE
   if( skip_member_macro())   /* 01.08.97 rigo */
   {
      return member( Class );
   }
#endif

   niveau++;

   switch( token( 0 ))
   {
   case RBRACE      :
      niveau--;
      return False;

   case SN_PRIVATE     :
   case SN_PUBLIC      :
   case SN_PROTECTED   :
      if( token( 1 ) == ':' )
      {
         Class->access = token( 0 );
         step( 1 );
         step( 1 );
         niveau--;
         return True;
      }
      else
      {
         step( 1 );
         niveau--;
         return True;
      }
      break;

   default:
      if( member_declaration( Class ))
      {
         niveau--;
         return True;
      }
      else
      {
         niveau--;
         return False;
      }
   }
   niveau--;
}

extern int member_declaration( Class_t Class )
{
   if( ! _member_declaration( Class ))
   {
      keyw_cpp = ! keyw_cpp;

      if( _member_declaration( Class ))
      {
#ifdef TEST
         printf( "change mode: %s (%d.%d)\n"
               , filename_g
               , f_lineno(0)
               , f_charno(0)
               );
#endif
         return True;
      }
      else
      {
         keyw_cpp = ! keyw_cpp;  /* restore mode */
         return skip_member_declaration();
      }
   }
   return True;
}

extern int _member_declaration( Class_t Class )
{
   Save_d();
   sDeclaration_t sDeclaration;
   int lineno_beg;
   int charno_beg;
   int retval;
   niveau++;

   sDeclaration.Declaration   = 0;
   sDeclaration.type          = DECLARATION_UNKNOWN;
   LongStringInit( &sDeclaration.name, -1 );
   sDeclaration.storage_class = 0;
   sDeclaration.fct_specifier = 0;
   sDeclaration.s_const       = 0;
   sDeclaration.s_volatile    = 0;
   sDeclaration.s_char        = 0;
   sDeclaration.s_short       = 0;
   sDeclaration.s_int         = 0;
   sDeclaration.s_long        = 0;
   sDeclaration.s_signed      = 0;
   sDeclaration.s_unsigned    = 0;
   sDeclaration.s_float       = 0;
   sDeclaration.s_double      = 0;
   sDeclaration.s_bool        = 0;
   sDeclaration.s_void        = 0;
   LongStringInit( &sDeclaration.type_name, -1 );
   sDeclaration.type_of_type_name = 0;
   LongStringInit( &sDeclaration.complete_class_name, -1 );
   sDeclaration.lineno_beg    = 0;
   sDeclaration.charno_beg    = 0;
   sDeclaration.lineno_end    = 0;
   sDeclaration.charno_end    = 0;

   sDeclaration.sClass.ClassParent      = Class;
   LongStringInit( &sDeclaration.sClass.name, -1 );
   sDeclaration.sClass.access           = 0;
   sDeclaration.sClass.lineno_beg       = 0;
   sDeclaration.sClass.charno_beg       = 0;
   sDeclaration.sClass.lineno_end       = 0;
   sDeclaration.sClass.charno_end       = 0;

   LongStringInit( &sDeclaration.sEnum.name, -1 );
   sDeclaration.sEnum.lineno_beg        = 0;
   sDeclaration.sEnum.charno_beg        = 0;
   sDeclaration.sEnum.lineno_end        = 0;
   sDeclaration.sEnum.charno_end        = 0;

   while( True )
   {
      switch( token( 0 ))
      {
      case ';'        : /* empty member_declarator_list */
         step( 1 );
         class_empty_declarator_list( Class, &sDeclaration );
         Return( True )

      case SN_INTERFACE  :
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         if( ! f_class( &sDeclaration, &sDeclaration.sClass ))
         {
            Restore_d();
            Return( False )
         }
         break;

      case SN_ENUM       :
         if( ! f_enum( &sDeclaration, &sDeclaration.sEnum ))
         {
            Restore_d();
            Return( False )
         }
         break;

      case SN_AUTO       : sDeclaration.storage_class = SN_AUTO    ; step( 1 ); break;
      case SN_REGISTER   : sDeclaration.storage_class = SN_REGISTER; step( 1 ); break;
      case SN_STATIC     : sDeclaration.storage_class = SN_STATIC  ; step( 1 ); break;
      case SN_EXTERN     : sDeclaration.storage_class = SN_EXTERN  ; step( 1 ); break;
      case SN_INLINE     : sDeclaration.fct_specifier = SN_INLINE  ; step( 1 ); break;
      case SN_VIRTUAL    : sDeclaration.fct_specifier = SN_VIRTUAL ; step( 1 ); break;
      case SN_FRIEND     : sDeclaration.storage_class = SN_FRIEND  ; step( 1 ); break;
      case SN_TYPEDEF    : sDeclaration.storage_class = SN_TYPEDEF ; step( 1 ); break;
      case SN_CONST      : sDeclaration.s_const       = True    ; step( 1 ); break;
      case SN_VOLATILE   : sDeclaration.s_volatile    = True    ; step( 1 ); break;
      case SN_CHAR       : sDeclaration.s_char        = True    ; step( 1 ); break;
      case SN_SHORT      : sDeclaration.s_short       = True    ; step( 1 ); break;
      case SN_INT        : sDeclaration.s_int         = True    ; step( 1 ); break;
      case SN_LONG       : sDeclaration.s_long        = True    ; step( 1 ); break;
      case SN_SIGNED     : sDeclaration.s_signed      = True    ; step( 1 ); break;
      case SN_UNSIGNED   : sDeclaration.s_unsigned    = True    ; step( 1 ); break;
      case SN_FLOAT      : sDeclaration.s_float       = True    ; step( 1 ); break;
      case SN_DOUBLE     : sDeclaration.s_double      = True    ; step( 1 ); break;
      case SN_BOOL       : sDeclaration.s_bool        = True    ; step( 1 ); break;
      case SN_VOID       : sDeclaration.s_void        = True    ; step( 1 ); break;
      case SN_IDENTIFIER :
/* printf( "IDENTIFIER: %s\n", identstring( 0 )); */
         /* ha az ident utan * vagy & vagy ident all ( kiveve az 'ident iden (('
            konstrukciot ), akkor nem lehet deklarator_list */
         if(( token( 1 ) == '*' ) ||
            ( token( 1 ) == '&' ) ||
            ( token( 1 ) == SN_IDENTIFIER && ( token( 2 ) != '(' || token( 3 ) != '(' ))
           )
         {
         }
         else
         {
            if( member_declarator_list( Class, &sDeclaration ))
            {
               Return( True )
            }
         }
         LongStringMySetLen( &sDeclaration.type_name, 0 );
         sDeclaration.type_of_type_name = 0;
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         if( ! complete_class_name( &sDeclaration.type_name ))
         {
            Restore_d();
            Return( False )
         }
         else
         {
            LongStringsMyCopy( &sDeclaration.complete_class_name
                             , &sDeclaration.type_name );

            sDeclaration.lineno_beg = lineno_beg;
            sDeclaration.charno_beg = charno_beg;
            sDeclaration.lineno_end = f_lineno( -1 );
            sDeclaration.charno_end = f_charno( -1 ) + 1;
         }
         break;

      case SN_CLCL       :
         if( member_declarator_list( Class, &sDeclaration ))
         {
            Return( True )
         }
         LongStringMySetLen( &sDeclaration.type_name, 0 );
         sDeclaration.type_of_type_name = 0;
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         if( ! complete_class_name( &sDeclaration.type_name ))
         {
            Restore_d();
            Return( False )
         }
         else
         {
            LongStringsMyCopy( &sDeclaration.complete_class_name
                             , &sDeclaration.type_name );

            sDeclaration.lineno_beg = lineno_beg;
            sDeclaration.charno_beg = charno_beg;
            sDeclaration.lineno_end = f_lineno( -1 );
            sDeclaration.charno_end = f_charno( -1 ) + 1;
         }
         break;

      case SN_OPERATOR   :
      case '('        :
      case '*'        :
      case '&'        :
      case '~'        :
         if( member_declarator_list( Class, &sDeclaration ))
         {
            Return( True )
         }
         else
         {
            Restore_d();
            Return( False )
         }
         break;

      default         :
         Restore_d();
         Return( False )
         break;
      }
   }

return_label:
   LongStringMyFree( &sDeclaration.name );
   LongStringMyFree( &sDeclaration.type_name );
   LongStringMyFree( &sDeclaration.complete_class_name );
   LongStringMyFree( &sDeclaration.sClass.name );
   LongStringMyFree( &sDeclaration.sEnum.name );
   niveau--;
   return retval;
}

extern int member_declarator_list( Class_t Class, Declaration_t Declaration )
{
   int retval;
   Save_d();
   sDeclarator_t sDeclarator;
   niveau++;

   LongStringInit( &sDeclarator.name , -1 );
   LongStringInit( &sDeclarator.type , -1 );
   LongStringInit( &sDeclarator.types, -1 );
   LongStringInit( &sDeclarator.names, -1 );

   while( True )
   {
      LongStringMyFree( &sDeclarator.name  );
      LongStringMyFree( &sDeclarator.type  );
      LongStringMyFree( &sDeclarator.types );
      LongStringMyFree( &sDeclarator.names );
      sDeclarator.base_typ   = 0;
      sDeclarator.pure       = 0;
      sDeclarator.lineno_beg = 0;
      sDeclarator.charno_beg = 0;
      sDeclarator.lineno_end = 0;
      sDeclarator.charno_end = 0;

      if( member_declarator( &sDeclarator ))
      {
         switch( token( 0 ))
         {
         case '=':
            initializer();
            sDeclarator.pure = True;
            break;

         case ':':
            if( sDeclarator.base_typ == FUNCTION ) /* 11.09.96 rigo */
            {
               ctor_initializer();
            }
            else
            {
               constant_expression();
            }
            break;

         case SN_RETURN:   /* gc++: inline Int operator - (Int &a) return r(a) {}; miatt 11.09.96 rigo */
         case SN_THROW:    /* gc++: int m1() throw(char*); miatt 02.10.97 rigo*/
            ctor_initializer();
            break;
         }

         if( token( 0 ) == ',' )
         {
            class_member( Class, Declaration, &sDeclarator );
            step( 1 );
            continue;
         }

         if( token( 0 ) == ';' )
         {
            class_member( Class, Declaration, &sDeclarator );
            step( 1 );
            Return( True )
         }

         /* itt nem lehet kernighan - ritchie fuggveny definicio */

         if( token( 0 ) == LBRACE )
         {
            int lineno = f_lineno( 0 );
            int charno = f_charno( 0 );
            char *comment = get_comment( f_lineno( -1 ));
            char *classname_save = classname_g;          /* 08.04.97 */
            char *funcname_save = funcname_g;            /* 08.04.97 */

            classname_g = my_strdup( Class->name.buf );                 /* 07.04.97 */
            funcname_g  = my_strdup( get_name( sDeclarator.name.buf )); /* 07.04.97 */

            compound_statement();

            my_free( classname_g );                         /* 08.04.97 */
            my_free( funcname_g );                          /* 08.04.97 */
            classname_g = classname_save;                   /* 08.04.97 */
            funcname_g = funcname_save;                     /* 08.04.97 */

            class_method( Class, Declaration, &sDeclarator, f_lineno( -1 ), f_charno( -1 ), lineno, charno, comment );
            Return( True )
         }
      }
      Restore_d();
      Return( False )
   }
return_label:
   LongStringMyFree( &sDeclarator.name  );
   LongStringMyFree( &sDeclarator.type  );
   LongStringMyFree( &sDeclarator.types );
   LongStringMyFree( &sDeclarator.names );
   niveau--;
   return retval;
}

extern int member_declarator( Declarator_t Declarator )
{
   int ret;

/* printf( "member_declarator: %s\n", identstring( 0 )); */
   niveau++;
   switch( token( 0 ))
   {
   case '('           : ret = declarator_paren     ( Declarator ); break;
   case '*'           : ret = declarator_star      ( Declarator ); break;
   case '&'           : ret = declarator_star      ( Declarator ); break;
   case SN_IDENTIFIER :
   case SN_CLCL       : ret = declarator_identifier( Declarator ); break;
   case SN_OPERATOR   : ret = declarator_operator  ( Declarator ); break;
   case '~'           : ret = declarator_tilde     ( Declarator ); break;
   default            : ret = True; break;  /* 'int : 2;' miatt nem return False */
   }
   niveau--;
/* printf( "return from member_declarator: %d : %s\n", ret, identstring( 0 )); */
   return ret;
}

extern int f_enum( Declaration_t Declaration, Enum_t Enum )
{
   char *name;
   int lineno;
   int charno;
   int t;
   char *comment;
   
   niveau++;

   /* LongStringInit( &Enum->name, -1 ); 15.01.97 rigo */
   Enum->lineno_beg = f_lineno( 0 );
   Enum->charno_beg = f_charno( 0 );
   Enum->lineno_end = f_lineno( 0 );
   Enum->charno_end = f_charno( 0 ) + identleng( 0 );

   if( Declaration )
   {
      Declaration->type_of_type_name = SN_ENUM;
   }
   
   step( 1 );

   switch( token( 0 ))
   {
   default:
   case SN_IDENTIFIER:
      LongStringIdCopy( &Enum->name, ident( 0 ));
      name = Enum->name.buf;
      lineno = f_lineno( 0 );
      charno = f_charno( 0 );
      Enum->lineno_beg = lineno;
      Enum->charno_beg = charno;
      Enum->lineno_end = lineno;
      Enum->charno_end = charno + Enum->name.len;

      step( 1 );
      if( token( 0 ) == LBRACE )
      {
         char *classname_save = classname_g;         /* 08.04.97 */
         comment = get_comment( Enum->lineno_end );
         classname_g = my_strdup( Enum->name.buf );      /* 07.04.97 */
         enum_list( Enum );
         while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
         {
            step( 1 );
         }
         step( 1 );

         my_free( classname_g );                      /* 08.04.97 */
         classname_g = classname_save;                /* 08.04.97 */

         Put_symbol( PAF_ENUM_DEF
                   , get_scope( name )
                   , get_name( name )
                   , filename_g
                   , lineno
                   , charno
                   , f_lineno( -1 )
                   , f_charno( -1 ) + 1
                   , (unsigned long) 0
                   , (char *) 0
                   , (char *) 0
                   , (char *) 0
                   , comment
                   , Enum->lineno_beg
                   , Enum->charno_beg
                   , Enum->lineno_end
                   , Enum->charno_end
                   );
      }
      if( Declaration )
      {
         LongStringsMyAppend( &Declaration->type_name, &Enum->name );
      }
      niveau--;
      return True;
      break;

   case LBRACE:
      if( Declaration && Declaration->storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( &Enum->lineno_beg, &Enum->charno_beg, &str_name ))
         {
            Declaration->storage_class = SN_TYPEDEF_TO_CLASS;
            LongStringIdCopy( &Enum->name, str_name );
            LongStringsMyAppend( &Declaration->type_name, &Enum->name );
            Enum->lineno_end = Enum->lineno_beg;
            Enum->charno_end = Enum->charno_beg + Enum->name.len;
         }
         else
         {
            LongStringMyCopy( &Enum->name, ANONYM );
         }
      }
      else
      {
         LongStringMyCopy( &Enum->name, ANONYM );
      }
      name = Enum->name.buf;
      lineno = f_lineno( 0 );
      charno = f_charno( 0 ) + 1;
      comment = get_comment( Enum->lineno_end );
      enum_list( Enum );
      while(( t = token( 0 )) != RBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
      {
         step( 1 );
      }
      step( 1 );

      Put_symbol( PAF_ENUM_DEF
                , get_scope( name )
                , get_name( name )
                , filename_g
                , lineno
                , charno
                , f_lineno( -1 )
                , f_charno( -1 ) + 1
                , (unsigned long) 0
                , (char *) 0
                , (char *) 0
                , (char *) 0
                , comment
                , Enum->lineno_beg
                , Enum->charno_beg
                , Enum->lineno_end
                , Enum->charno_end
                );

      if( Declaration && Declaration->type_name.buf == NULL)
      {
         LongStringsMyAppend( &Declaration->type_name, &Enum->name );
      }

      niveau--;
      
      return True;
      break;
   }
   if( Declaration )
   {
      LongStringsMyAppend( &Declaration->type_name, &Enum->name );
   }
   niveau--;
   return True;
}

extern int enum_list( Enum_t Enum )
{
   niveau++;
   step( 1 );

   while( enumerator( Enum ))
   {
      if( token( 0 ) == ',' )
      {
         step( 1 );
      }
   }

   niveau--;
   return True;
}

extern int enumerator( Enum_t Enum )
{
   Save_d();
   niveau++;

   if( token( 0 ) == SN_IDENTIFIER )
   {
      Put_symbol( PAF_ENUM_CONST_DEF
/*              , Enum->name */
                , NULL                          /* 17.02.97 rigo */
                , StringToText( ident( 0 ))
                , filename_g
                , f_lineno( 0 )
                , f_charno( 0 )
                , f_lineno( 0 )
                , f_charno( 0 )
                , (unsigned long) 0
/*              , (char *) 0 */
                , Enum->name.buf                   /* 17.02.97 rigo */
                , (char *) 0
                , (char *) 0
                , get_comment( f_lineno( 0 ))
                , f_lineno( 0 )
                , f_charno( 0 )
                , f_lineno( 0 )
                , f_charno( 0 ) + identleng( 0 )
                );

      step( 1 );

      if( token( 0 ) == '=' )
      {
         step( 1 );
         constant_expression();
         niveau--;
         return True;
      }
      niveau--;
      return True;   /* ? */
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

#define AppendStep_d(x) LongStringMyAppend(plstr,x); step( 1 ); niveau--; return True;

extern int operator_function_name( LongString *plstr )
{
   Save_d();
   niveau++;

   if( token( 0 ) != SN_OPERATOR )
   {
      Restore_d();
      niveau--;
      return False;
   }

   step( 1 );

   switch( token( 0 ))
   {
   case '@'            : AppendStep_d( "operator@"   )
   case '+'            : AppendStep_d( "operator+"   )
   case '-'            : AppendStep_d( "operator-"   )
   case '*'            : AppendStep_d( "operator*"   )
   case '/'            : AppendStep_d( "operator/"   )
   case '%'            : AppendStep_d( "operator%"   )
   case '^'            : AppendStep_d( "operator^"   )
   case '&'            : AppendStep_d( "operator&"   )
   case '|'            : AppendStep_d( "operator|"   )
   case '~'            : AppendStep_d( "operator~"   )
   case '!'            : AppendStep_d( "operator!"   )
   case '<'            : AppendStep_d( "operator<"   )
   case '>'            : AppendStep_d( "operator>"   )
   case SN_LS          : AppendStep_d( "operator<<"  )
   case SN_RS          : AppendStep_d( "operator>>"  )
   case SN_ANDAND      : AppendStep_d( "operator&&"  )
   case SN_OROR        : AppendStep_d( "operator||"  )
   case SN_ARROW       : AppendStep_d( "operator->"  )
   case SN_ARROWstar   : AppendStep_d( "operator->*" )
   case '.'            : AppendStep_d( "operator."   )
   case SN_DOTstar     : AppendStep_d( "operator.*"  )
   case SN_ICR         : AppendStep_d( "operator++"  )
   case SN_DECR        : AppendStep_d( "operator--"  )
   case SN_LE          : AppendStep_d( "operator<="  )
   case SN_GE          : AppendStep_d( "operator>="  )
   case SN_EQ          : AppendStep_d( "operator=="  )
   case SN_NE          : AppendStep_d( "operator!="  )
   case '='            : AppendStep_d( "operator="   )
   case SN_MULTassign  : AppendStep_d( "operator*="  )
   case SN_DIVassign   : AppendStep_d( "operator/="  )
   case SN_MODassign   : AppendStep_d( "operator%="  )
   case SN_PLUSassign  : AppendStep_d( "operator+="  )
   case SN_MINUSassign : AppendStep_d( "operator-="  )
   case SN_LSassign    : AppendStep_d( "operator<<=" )
   case SN_RSassign    : AppendStep_d( "operator>>=" )
   case SN_ANDassign   : AppendStep_d( "operator&="  )
   case SN_ERassign    : AppendStep_d( "operator^="  )
   case SN_ORassign    : AppendStep_d( "operator|="  )
   case '('            :
      if( token( 1 ) == ')' )
      {
         LongStringMyAppend( plstr, "operator()" );
         step( 1 );
         step( 1 );
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      
   case '['            :
      if( token( 1 ) == ']' )
      {
         LongStringMyAppend( plstr, "operator[]" );
         step( 1 );
         step( 1 );
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      
   case SN_NEW         : AppendStep_d( "operator_new"    )
   case SN_DELETE      : AppendStep_d( "operator_delete" )
   case ','            : AppendStep_d( "operator,"       )
   default             :
      LongStringMyAppend( plstr, "operator_" );
      if( conversion_type_name( plstr ))
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
   }
   niveau--;
}

extern int conversion_type_name( LongString *plstr )
{
   Save_d();
   niveau++;

   if( conversion_type_specifier( plstr ))
   {
      while( conversion_type_specifier( plstr ))
      {
      }
      if( ptr_operator( 0 ))
      {
         LongStringMyAppend( plstr, "*" );
      }
      niveau--;
      return True;
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int conversion_type_specifier( LongString *plstr )
{
   Save_d();
   int t;
   char *name = 0;
   sClass_t sClass;
   sEnum_t sEnum;
   int f_ret;

   niveau++;

   t = token( 0 );

   switch( t )
   {
   case SN_INTERFACE  : name = "_class" ; break;
   case SN_CLASS      : name = "_class" ; break;
   case SN_STRUCT     : name = "_struct"; break;
   case SN_UNION      : name = "_union" ; break;
   }

   if( name )
   {
      LongStringMyAppend( plstr, name );
   }

   switch( t )
   {
   case SN_INTERFACE  :
   case SN_CLASS      :
   case SN_STRUCT     :
   case SN_UNION      :
      LongStringInit( &sClass.name, -1 ); /* Zsolt Koppany, 30-dec-97 */
      f_ret = f_class( 0, &sClass );
      LongStringMyFree( &sClass.name ); /* Zsolt Koppany, 30-dec-97 */
      if( f_ret)
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      break;

   case SN_ENUM       :
      LongStringMyAppend( plstr, "_enum" );
      LongStringInit( &sEnum.name, -1 ); /* 15.01.97 rigo */
      f_ret = f_enum( 0, &sEnum );
      LongStringMyFree( &sEnum.name ); /* Zsolt Koppany, 30-dec-97 */
      if( f_ret)
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      break;

   case SN_AUTO       :
   case SN_REGISTER   :
   case SN_STATIC     :
   case SN_EXTERN     :
   case SN_INLINE     :
   case SN_VIRTUAL    :
   case SN_FRIEND     :
   case SN_TYPEDEF    :
   case SN_CONST      :
   case SN_VOLATILE   :
   case SN_CHAR       :
   case SN_SHORT      :
   case SN_INT        :
   case SN_LONG       :
   case SN_SIGNED     :
   case SN_UNSIGNED   :
   case SN_FLOAT      :
   case SN_DOUBLE     :
   case SN_BOOL       :
   case SN_VOID       :
      LongStringIdAppend( plstr, ident( 0 ));
      step( 1 );
      niveau--;
      return True;

   case SN_IDENTIFIER :
   case SN_CLCL       :
      if( complete_class_name( plstr ))
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      break;
   default:
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int complete_class_name( LongString *plstr )
{
   Save_d();
   niveau++;

   switch( token( 0 ))
   {
   case SN_CLCL:
      step( 1 );

      if( plstr )
      {
         LongStringMyAppend( plstr, "::" );
      }

      if( token( 0 ) == SN_IDENTIFIER )
      {
         qualified_class_name( plstr );
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         niveau--;
         return False;
      }
      
   case SN_IDENTIFIER:
      qualified_class_name( plstr );
      niveau--;
      return True;

   default:
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern void qualified_class_name( LongString *plstr )
{
   niveau++;

   if( plstr )
   {
      LongStringIdAppend( plstr, ident( 0 ));
   }

   step( 1 );

   if( token( 0 ) == '<' )
   {
      template_argument_skip( plstr );
   }

   if( token( 0 ) == SN_CLCL && token( 1 ) == SN_IDENTIFIER )
   {
      if( plstr )
      {
         LongStringMyAppend( plstr, "::" );
      }

      step( 1 );
      qualified_class_name( plstr );
   }
   niveau--;
}

extern int qualified_name( LongString *plstr, int *plineno, int *pcharno )
{
   Save_d();
   niveau++;

   if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == SN_CLCL )
   {
      if( plstr )
      {
         LongStringIdAppend( plstr, ident( 0 ));
         LongStringMyAppend( plstr, "::" );
      }

      step( 1 );
      step( 1 );
      qualified_name( plstr, plineno, pcharno );
      niveau--;
      return True;
   }
   else if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == '<' )
   {
      if( plstr )
      {
         LongStringIdAppend( plstr, ident( 0 ));
         LongStringMyAppend( plstr, "::" );
      }

      step( 1 );
      template_argument_skip( plstr );
      if( token( 0 ) == SN_CLCL )
      {
         step( 1 );
      }
      else  /* 21.02.97 rigo */
      {
         Restore_d();
         niveau--;
         return False;
      }
      qualified_name( plstr, plineno, pcharno );
      niveau--;
      return True;
   }
   else
   {
      if( plineno ) (*plineno) = f_lineno( 0 );
      if( pcharno ) (*pcharno) = f_charno( 0 );

      switch( token( 0 ))
      {
      case SN_IDENTIFIER :
         if( plstr )
         {
            LongStringIdAppend( plstr, ident( 0 ));
         }
         step( 1 );
         niveau--;
         return True;
      case SN_OPERATOR  :
         if( operator_function_name( plstr ))
         {
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
            return False;
         }
      case '~'       :
         if( token( 1 ) == SN_IDENTIFIER )
         {
            step( 1 );
            if( plstr )
            {
               LongStringMyAppend( plstr, "~" );
               LongStringIdAppend( plstr, ident( 0 ));
            }
            step( 1 );
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
            return False;
         }
      default        :
         Restore_d();
         niveau--;
         return False;
      }
   }
   niveau--;
}

extern void initializer( void )
{
   int paren1 = 0;
   int paren2 = 0;
   int paren3 = 0;
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case '('   : paren1++; break;
      case ')'   : paren1--; break;
      case LBRACE: paren2++; break;
      case RBRACE: paren2--; break;
      case '['   : paren3++; break;
      case ']'   : paren3--; break;
      case  0    : 
         niveau--;
         return;
      case ','   :
      case ';'   :
         if( paren1 == 0 && paren2 == 0 && paren3 == 0 )
         {
            niveau--;
            return;
         }
         break;
      default :
         break;
      }
      step( 1 );
   }
   niveau--;
}

extern void ctor_initializer( void )
{
   niveau++;
   while( True )
   {
      switch( token( 0 ))
      {
      case ';':   /* 10.02.97 rigo: int m1() throw(char*); */
         if( token( 1 ) == LBRACE )
         {
            step( 1 );
         }
         niveau--;
         return;
         break;
      case LBRACE:
         niveau--;
         return;
         break;
      case  0 : 
         niveau--;
         return;
      default :
         break;
      }
      step( 1 );
   }
   niveau--;
}

extern void constant_expression( void )
{
   int paren1 = 0;
   int paren3 = 0;
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case '(': paren1++; break;
      case ')': paren1--; break;
      case '[': paren3++; break;
      case ']': paren3--; break;
      case  0 : 
         niveau--;
         return;
      case ',':
      case ';':
      case RBRACE:
         if( paren1 == 0 && paren3 == 0 )
         {
            niveau--;
            return;
         }
         break;
      default :
         break;
      }
      step( 1 );
   }
   niveau--;
}

extern int kr_argument_declaration_list( LongString *types, LongString *names )
{
   Save_d();
   Array_t ArrayTypes;
   Array_t ArrayNames;
   int retval;

   niveau++;

   if( ! is_single_parameter_list( types ))
   {
      niveau--;
      Restore_d();
      return False;
   }

   ArrayTypes = array_create( types );
   ArrayNames = array_create( names );

#ifdef ARRAY_TEST
   array_print( ArrayTypes );
   array_print( ArrayNames );
#endif

   while( True )
   {
      if( kr_argument_declaration( ArrayTypes, ArrayNames ))
      {
ok:
         if( token( 0 ) == LBRACE )
         {
            retval = True;
            goto end;
         }
      }
      else
      {
         if( token( 0 ) == SN_IDENTIFIER )   /* 10.02.97 rigo */
         {
            sString_t sString = ident( 0 );

            if( strncmp( "va_dcl", sString.text, sString.leng ) == 0 )
            {
               step( 1 );
               goto ok;
            }
         }
         Restore_d();
         retval = False;
         goto end;
      }
   }

end:
#ifdef ARRAY_TEST
   array_print( ArrayTypes );
   array_print( ArrayNames );
#endif

   if( retval )
   {
      array_change_not_changed( ArrayTypes, ArrayNames );

      array_to_string( types, ArrayTypes );
      array_to_string( names, ArrayNames );
   }

   array_destroy( ArrayTypes );
   array_destroy( ArrayNames );

   niveau--;
   return retval;
}

extern int kr_argument_declaration( Array_t ArrayTypes, Array_t ArrayNames )
{
   Save_d();
   sDeclaration_t sDeclaration;
   int lineno_beg;
   int charno_beg;
   int retval = False;
   niveau++;

   sDeclaration.Declaration   = 0;
   sDeclaration.type          = DECLARATION_UNKNOWN;
   LongStringInit( &sDeclaration.name, -1 );
   sDeclaration.storage_class = 0;
   sDeclaration.fct_specifier = 0;
   sDeclaration.s_const       = 0;
   sDeclaration.s_volatile    = 0;
   sDeclaration.s_char        = 0;
   sDeclaration.s_short       = 0;
   sDeclaration.s_int         = 0;
   sDeclaration.s_long        = 0;
   sDeclaration.s_signed      = 0;
   sDeclaration.s_unsigned    = 0;
   sDeclaration.s_float       = 0;
   sDeclaration.s_double      = 0;
   sDeclaration.s_bool        = 0;
   sDeclaration.s_void        = 0;
   LongStringInit( &sDeclaration.type_name, -1 );
   sDeclaration.type_of_type_name = 0;
   LongStringInit( &sDeclaration.complete_class_name, -1 );
   sDeclaration.lineno_beg    = 0;
   sDeclaration.charno_beg    = 0;
   sDeclaration.lineno_end    = 0;
   sDeclaration.charno_end    = 0;

   sDeclaration.sClass.ClassParent      = 0;
   LongStringInit( &sDeclaration.sClass.name, -1 );
   sDeclaration.sClass.access           = 0;
   sDeclaration.sClass.lineno_beg       = 0;
   sDeclaration.sClass.charno_beg       = 0;
   sDeclaration.sClass.lineno_end       = 0;
   sDeclaration.sClass.charno_end       = 0;

   LongStringInit( &sDeclaration.sEnum.name, -1 );
   sDeclaration.sEnum.lineno_beg        = 0;
   sDeclaration.sEnum.charno_beg        = 0;
   sDeclaration.sEnum.lineno_end        = 0;
   sDeclaration.sEnum.charno_end        = 0;

   while( True )
   {
      switch( token( 0 ))
      {
      case ';'        :
         step( 1 );
         Return( False ) /* 13.02.97 rigo: ures kr_argument_declarator_list */

      case SN_INTERFACE  :
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
/*       if( ! f_class( 0, &sClass ))
         {
            Restore_d();
            Return( False )
         } 13.02.97 rigo */

         function_argument_class( sDeclaration.storage_class
                                , &sDeclaration.type_name );
         break;

      case SN_ENUM       :
/*       if( ! f_enum( 0, &sEnum ))
         {
            Restore_d();
            Return( False )
         } 13.02.97 rigo */
         function_argument_enum( sDeclaration.storage_class, &sDeclaration.type_name );
         break;

      case SN_AUTO       : sDeclaration.storage_class = SN_AUTO    ; step( 1 ); break;
      case SN_REGISTER   : sDeclaration.storage_class = SN_REGISTER; step( 1 ); break;
      case SN_CONST      : sDeclaration.s_const       = True; step( 1 ); break;
      case SN_VOLATILE   : sDeclaration.s_volatile    = True; step( 1 ); break;
      case SN_CHAR       : sDeclaration.s_char        = True; step( 1 ); break;
      case SN_SHORT      : sDeclaration.s_short       = True; step( 1 ); break;
      case SN_INT        : sDeclaration.s_int         = True; step( 1 ); break;
      case SN_LONG       : sDeclaration.s_long        = True; step( 1 ); break;
      case SN_SIGNED     : sDeclaration.s_signed      = True; step( 1 ); break;
      case SN_UNSIGNED   : sDeclaration.s_unsigned    = True; step( 1 ); break;
      case SN_FLOAT      : sDeclaration.s_float       = True; step( 1 ); break;
      case SN_DOUBLE     : sDeclaration.s_double      = True; step( 1 ); break;
      case SN_BOOL       : sDeclaration.s_bool        = True; step( 1 ); break;
      case SN_VOID       : sDeclaration.s_void        = True; step( 1 ); break;
      case SN_IDENTIFIER :
         /* ha az ident utan * vagy & vagy ident all ( kiveve az 'ident iden (('
            konstrukciot ), akkor nem lehet deklarator_list */
         if(( token( 1 ) == '*' ) ||
            ( token( 1 ) == '&' ) ||
            ( token( 1 ) == SN_IDENTIFIER && ( token( 2 ) != '(' || token( 3 ) != '(' ))
           )
         {
         }
         else
         {
            if( kr_argument_declarator_list( &sDeclaration, ArrayTypes, ArrayNames ))
            {
               Return( True )
            }
         }
         LongStringMySetLen( &sDeclaration.type_name, 0 );
         lineno_beg = f_lineno( 0 );
         charno_beg = f_charno( 0 );
         if( ! complete_class_name( &sDeclaration.type_name ))
         {
            Restore_d();
            Return( False )
         }
         else
         {
            LongStringsMyCopy( &sDeclaration.complete_class_name
                             , &sDeclaration.type_name );

            sDeclaration.lineno_beg = lineno_beg;
            sDeclaration.charno_beg = charno_beg;
            sDeclaration.lineno_end = f_lineno( -1 );
            sDeclaration.charno_end = f_charno( -1 ) + 1;
         }
         break;

      case '('        :
      case '*'        :
         if( kr_argument_declarator_list( &sDeclaration, ArrayTypes, ArrayNames ))
         {
            Return( True )
         }
         else
         {
            Restore_d();
            Return( False )
         }
         break;

      default         :
         Restore_d();
         Return( False )
      }
   }

return_label:
   LongStringMyFree( &sDeclaration.name );
   LongStringMyFree( &sDeclaration.type_name );
   LongStringMyFree( &sDeclaration.complete_class_name );
   LongStringMyFree( &sDeclaration.sClass.name );
   LongStringMyFree( &sDeclaration.sEnum.name );
   niveau--;
#ifdef ARRAY_TEST
   printf( "kr_argument_declaration: %d\n", retval );
#endif
   return retval;
}

extern int kr_argument_declarator_list( Declaration_t Declaration, Array_t ArrayTypes, Array_t ArrayNames )
{
   int retval;
   Save_d();
   sDeclarator_t  sDeclarator;
   niveau++;

   LongStringInit( &sDeclarator.name , -1 );
   LongStringInit( &sDeclarator.type , -1 );
   LongStringInit( &sDeclarator.types, -1 );
   LongStringInit( &sDeclarator.names, -1 );

   while( True )
   {
      LongStringMyFree( &sDeclarator.name  );
      LongStringMyFree( &sDeclarator.type  );
      LongStringMyFree( &sDeclarator.types );
      LongStringMyFree( &sDeclarator.names );
      sDeclarator.base_typ   = 0;
      sDeclarator.pure       = 0;
      sDeclarator.lineno_beg = 0;
      sDeclarator.charno_beg = 0;
      sDeclarator.lineno_end = 0;
      sDeclarator.charno_end = 0;

      if( kr_argument_declarator( &sDeclarator ))
      {
         LongString type;

         LongStringInit( &type, -1 );
         create_type( &type, Declaration, &sDeclarator );
#ifdef ARRAY_TEST
         printf( "kr_declarator: %s %s\n", type.buf, sDeclarator.name );
#endif

         if( ! array_change_type_and_name( ArrayTypes
                                         , ArrayNames
                                         , type.buf
                                         , sDeclarator.name.buf ))
         {
#ifdef ARRAY_TEST
            printf( "unknown kr_declarator: <%s>\n", type.buf );
#endif
            Restore_d();
            LongStringMyFree( &type );
            Return( False )
         }

         LongStringMyFree( &type );

         if( token( 0 ) == ',' )
         {
            step( 1 );
            continue;
         }

         if( token( 0 ) == ';' )
         {
            step( 1 );
            Return( True )
         }
      }
      Restore_d();
      Return( False )
   }

return_label:
   LongStringMyFree( &sDeclarator.name  );
   LongStringMyFree( &sDeclarator.type  );
   LongStringMyFree( &sDeclarator.types );
   LongStringMyFree( &sDeclarator.names );
   niveau--;
   return retval;
}

extern int kr_argument_declarator( Declarator_t Declarator )
{
   int ret;

   niveau++;

   switch( token( 0 ))
   {
   case '('        : ret = kr_argument_declarator_paren     ( Declarator ); break;
   case '*'        : ret = kr_argument_declarator_star      ( Declarator ); break;
   case SN_IDENTIFIER :
      ret = kr_argument_declarator_identifier( Declarator );
      break;
   default         : ret = False; break;
   }
   niveau--;
   return ret;
}

extern int kr_argument_declarator_paren( Declarator_t Declarator )
{
   Save_d();
   int len;

   len = Declarator->type.len;

   niveau++;

   LongStringMyAppend( &Declarator->type, "(" );

   step( 1 );
   if( kr_argument_declarator( Declarator ))
   {
      if( token( 0 ) == ')' )
      {
         LongStringMyAppend( &Declarator->type, ")" );
         step( 1 );
         if( function_or_array_list( Declarator ))
         {
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
            LongStringMySetLen( &Declarator->type, len );
            return False;
         }
      }
      else
      {
         Restore_d();
         niveau--;
         LongStringMySetLen( &Declarator->type, len );
         return False;
      }
   }
   else
   {
      Restore_d();
      niveau--;
      LongStringMySetLen( &Declarator->type, len );
      return False;
   }
   niveau--;
   LongStringMySetLen( &Declarator->type, len );
   return False;
}

extern int kr_argument_declarator_star( Declarator_t Declarator )
{
   Save_d();
   LongString aname;
   int len;

   niveau++;

   LongStringInit( &aname, -1 );
   if( ptr_operator( &aname ))
   {
      LongStringsMyAppend( &Declarator->type, &aname );
      LongStringMyFree( &aname );
      len = Declarator->type.len;
      if( kr_argument_declarator( Declarator ))
      {
         if( Declarator->base_typ == 0 )
         {
            Declarator->base_typ = POINTER;
         }
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->type, len );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int kr_argument_declarator_identifier( Declarator_t Declarator )
{
   Save_d();

   niveau++;

   /* inx x( y )
   ** X *CONST y[];
   ** {
   ** }
   ** miatt; ( atlepjuk a CONST-ot ) (17.11.97 rigo)
   */
   if( token( 1 ) == SN_IDENTIFIER )   
   {
      step( 1 );
   }

   LongStringIdCopy( &Declarator->name, ident( 0 ));
   Declarator->name_lineno_beg = Declarator->lineno_beg = f_lineno( 0 );
   Declarator->name_charno_beg = Declarator->charno_beg = f_charno( 0 );
   step( 1 );
   Declarator->lineno_end = f_lineno( -1 );
   Declarator->charno_end = f_charno( -1 ) + identleng( -1 );
   if( function_or_array_list( Declarator ))
   {
      niveau--;
      return True;
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
}

extern void compound_statement( void )
{
   int paren1 = 0;
   int niveau_save = niveau;

   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case LBRACE:
         paren1++;
         niveau++;
         break;
      case RBRACE:
         paren1--;
         niveau--;
#ifdef rigo
/* ez nem megy, peldaul
int main()
{
   static items it[] = {
   { 1, 2 },
   { 1, 2 },
};
}
miatt
*/
/* 22.09.97 rigo */
/* ha gyanus, hogy a fuggvenytorzs befejezodott, akkor kilepunk */
/* printf( "compound: %d %d\n", paren1, f_charno( 0 )); */
         if( paren1 != 0 && f_charno( 0 ) == 0 )
         {
            int bPutSymbol_save = bPutSymbol_m;

            Save_d();
            step( 1 );
            bPutSymbol_m = False;   /* no need Put_symbol */
            if( token( 0 ) == 0 || _declaration( 0 )) /* vege van a fuggvenytorzsnek */
            {
               bPutSymbol_m = bPutSymbol_save;
               Restore_d();
               step( 1 );
               niveau = niveau_save;
               return;
            }
            else
            {
               bPutSymbol_m = bPutSymbol_save;
               Restore_d();
            }
         }
#endif
         break;
      case  0    : 
         niveau = niveau_save;
         return;
      default :
         break;
      }

      step( 1 );
      if( paren1 == 0 )
      {
         niveau--;
         return;
      }
   }
   niveau--;
}

extern void variable( Declaration_t Declaration, Declarator_t Declarator )
{
   unsigned long attr;
   LongString type;
   char *scope = get_scope( Declarator->name.buf );
   char *name  = get_name ( Declarator->name.buf );

   if( Declaration->storage_class == SN_TYPEDEF_TO_CLASS ) /* 11.02.97 rigo */
   {
      if( Declaration->sClass.name.buf != 0 &&
          name != 0 &&
          strcmp( Declaration->sClass.name.buf, name ) == 0 ) /* 17.11.97 rigo*/
      {
         return;
      }
   }

   niveau++;

   if( name == 0 || name[0] == 0 )
   {
      niveau--;
      return;
   }

   attr = attr_from_declaration( Declaration );
   LongStringInit( &type, -1 );
   create_type( &type, Declaration, Declarator );

   if( Declaration->storage_class == SN_TYPEDEF ||
       Declaration->storage_class == SN_TYPEDEF_TO_CLASS ) /* 17.11.97 rigo */
   {
      Put_symbol( PAF_TYPE_DEF
/*              , scope */
                , NULL                          /* 17.02.97 rigo */
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , f_lineno( 0 )
                , f_charno( 0 )
                , attr
                , type.buf
/*              , (char *) 0 */
                , scope                         /* 17.02.97 rigo */
                , (char *) 0
                , get_comment( Declarator->lineno_end )
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   else if( Declarator->base_typ == FUNCTION )
   {
      int paf;

      if( scope )
      {
         paf = PAF_MBR_FUNC_DCL;

         if( *name == '~' )
         {
            attr |= PAF_DESTRUCTOR;
         }
         else if( name != 0 && strcmp( scope, name ) == 0 )
         {
            attr |= PAF_CONSTRUCTOR;
         }
      }
      else
      {
         paf = PAF_FUNC_DCL;
      }

      Put_symbol( paf
                , scope
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , f_lineno( 0 )
                , f_charno( 0 )
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , get_comment( Declarator->lineno_end )
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   else
   {
      if( Declaration->s_const )
      {
#ifdef BETTER_TEST
         if( Declaration->storage_class == SN_EXTERN )
         {
         }
         else
#endif   /* BETTER_TEST */
         {
            int paf;

            if( Declarator->base_typ == 0 || Declarator->base_typ == ARRAY )
            {
               paf = PAF_CONS_DEF;
            }
            else
            {
               paf = PAF_GLOB_VAR_DEF;
            }

            Put_symbol( paf
/*                    , scope */
                      , NULL                          /* 17.02.97 rigo */
                      , name
                      , filename_g
                      , Declarator->name_lineno_beg
                      , Declarator->name_charno_beg
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      , attr
                      , type.buf
/*                    , (char *) 0 */
                      , scope                         /* 17.02.97 rigo */
                      , (char *) 0
                      , get_comment( Declarator->lineno_end )
                      , Declarator->lineno_beg
                      , Declarator->charno_beg
                      , Declarator->lineno_end
                      , Declarator->charno_end
                      );
         }
      }
      else
      {
#ifdef BETTER_TEST
         if( Declaration->storage_class == SN_EXTERN )
         {
#if 0 /* ezt most nem hasznaljuk */
            Put_symbol( PAF_VAR_DCL
                      , scope
                      , name
                      , filename_g
                      , Declarator->name_lineno_beg
                      , Declarator->name_charno_beg
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      , attr
                      , type.buf
                      , (char *) 0
                      , (char *) 0
                      , get_comment( Declarator->lineno_end )
                      , Declarator->lineno_beg
                      , Declarator->charno_beg
                      , Declarator->lineno_end
                      , Declarator->charno_end
                      );
#endif
         }
         else
#endif /* BETTER_TEST*/
         {
            Put_symbol( PAF_GLOB_VAR_DEF
/*                    , scope */
                      , NULL                          /* 17.02.97 rigo */
                      , name
                      , filename_g
                      , Declarator->name_lineno_beg
                      , Declarator->name_charno_beg
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      , attr
                      , type.buf
/*                    , (char *) 0 */
                      , scope                         /* 17.02.97 rigo */
                      , (char *) 0
                      , get_comment( Declarator->lineno_end )
                      , Declarator->lineno_beg
                      , Declarator->charno_beg
                      , Declarator->lineno_end
                      , Declarator->charno_end
                      );
         }
      }
   }
   LongStringMyFree( &type );
   niveau--;
   return;
}

extern void function( Declaration_t Declaration, Declarator_t Declarator, int lineno_end, int charno_end, int lineno_brace_beg, int charno_brace_beg, char *comment )
{
   unsigned long attr = attr_from_declaration( Declaration );
   LongString type;

   niveau++;

   LongStringInit( &type, -1 );
   create_type( &type, Declaration, Declarator );

   if( Declaration->storage_class == SN_TYPEDEF )
   {
      Put_symbol( PAF_TYPE_DEF
/*              , get_scope( Declarator->name ) */
                , NULL                          /* 17.02.97 rigo */
                , get_name( Declarator->name.buf )
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , lineno_end
                , charno_end + 1
                , attr
                , type.buf
/*              , (char *) 0 */
                , get_scope( Declarator->name.buf )   /* 17.02.97 rigo */
                , (char *) 0
                , comment
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   else
   {
      int paf;
      char *scope = get_scope( Declarator->name.buf );
      char *name  = get_name ( Declarator->name.buf );

      if( scope )
      {
         paf = PAF_MBR_FUNC_DEF;

         if( *name == '~' )
         {
            attr |= PAF_DESTRUCTOR;
         }
         else if( name != 0 && strcmp( scope, name ) == 0 )
         {
            attr |= PAF_CONSTRUCTOR;
         }
      }
      else
      {
         paf = PAF_FUNC_DEF;
      }

      Put_symbol( paf
                , scope
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , lineno_end
                , charno_end + 1
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , comment
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );

      put_cross1( paf
                , scope
                , name
                , filename_g
                , lineno_brace_beg
                , charno_brace_beg
                , lineno_end
                , charno_end
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , (char *) 0
                );
   }
   LongStringMyFree( &type );
   niveau--;
}

extern void class_member( Class_t Class, Declaration_t Declaration, Declarator_t Declarator )
{
   unsigned long attr = attr_from_declaration( Declaration );
   LongString type;

   niveau++;

   LongStringInit( &type, -1 );
   create_type( &type, Declaration, Declarator );

   switch( Class->access )
   {
   case SN_PRIVATE   :  attr |= PAF_PRIVATE  ; break;
   case SN_PROTECTED :  attr |= PAF_PROTECTED; break;
   case SN_PUBLIC    :  attr |= PAF_PUBLIC   ; break;
   }

   if( Declaration->storage_class == SN_TYPEDEF )
   {
      Put_symbol( PAF_TYPE_DEF
/*              , get_scope( Declarator->name ) */
                , NULL                          /* 17.02.97 rigo */
                , get_name( Declarator->name.buf )
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , f_lineno( 0 )
                , f_charno( 0 )
                , attr
                , type.buf
/*              , (char *) 0 */
                , get_scope( Declarator->name.buf )   /* 17.02.97 rigo */
                , (char *) 0
                , get_comment( Declarator->lineno_end )
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   else if( Declarator->base_typ == FUNCTION )
   {
      int paf;
      char *scope = Class->name.buf;
      char *name  = get_name( Declarator->name.buf );

      if( Declaration->storage_class == SN_FRIEND )
      {
         unsigned long attr_friend = attr;

         attr_friend |= PAF_PROTECTED;
         attr_friend |= PAF_PRIVATE;
         attr_friend |= PAF_PUBLIC;

         paf = PAF_FUNC_DCL;

         /* extra jelentes a friend-rol */
         Put_symbol( PAF_FRIEND_DCL
                   , scope
                   , name
                   , filename_g
                   , Declarator->name_lineno_beg
                   , Declarator->name_charno_beg
                   , f_lineno( 0 )
                   , f_charno( 0 )
                   , attr_friend
                   , type.buf
                   , Declarator->types.buf
                   , Declarator->names.buf
                   , get_comment( Declarator->lineno_end )
                   , Declarator->lineno_beg
                   , Declarator->charno_beg
                   , Declarator->lineno_end
                   , Declarator->charno_end
                   );
      }
      else
      {
         paf = PAF_MBR_FUNC_DCL;

         if( *name == '~' )
         {
            attr |= PAF_DESTRUCTOR;
         }
         else if( scope != 0 && name != 0 && strcmp( scope, name ) == 0 )
         {
            attr |= PAF_CONSTRUCTOR;
         }

         if( Declaration->fct_specifier == SN_VIRTUAL && Declarator->pure )
         {
            attr |= PAF_PUREVIRTUAL;
         }
      }

      if (paf != PAF_FUNC_DCL)   /* Zsolt Koppany, 1-may-97 */
      {                          /* If it is a friend we don't report it. */
          Put_symbol( paf
                , scope
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , f_lineno( 0 )
                , f_charno( 0 )
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , get_comment( Declarator->lineno_end )
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
     }
   }
   else  /* class member variable */
   {
      if( Declarator->name.len ) /* 11.02.97 rigo: lehet, hogy ures bitfield */
      {
         if( Declaration->storage_class == SN_FRIEND )
         {
            unsigned long attr_friend = attr;

            attr_friend |= PAF_PROTECTED;
            attr_friend |= PAF_PRIVATE;
            attr_friend |= PAF_PUBLIC;

            /* extra jelentes a friend-rol */
            Put_symbol( PAF_FRIEND_DCL
                      , Class->name.buf
                      , Declarator->name.buf
                      , filename_g
                      , Declarator->name_lineno_beg
                      , Declarator->name_charno_beg
                      , f_lineno( 0 )
                      , f_charno( 0 )
                      , attr_friend
                      , type.buf
                      , (char *) 0
                      , (char *) 0
                      , get_comment( Declarator->lineno_end )
                      , Declarator->lineno_beg
                      , Declarator->charno_beg
                      , Declarator->lineno_end
                      , Declarator->charno_end
                      );
         }

         Put_symbol( PAF_MBR_VAR_DEF
                   , Class->name.buf
                   , Declarator->name.buf
                   , filename_g
                   , Declarator->name_lineno_beg
                   , Declarator->name_charno_beg
                   , f_lineno( 0 )
                   , f_charno( 0 )
                   , attr
                   , type.buf
                   , (char *) 0
                   , (char *) 0
                   , get_comment( Declarator->lineno_end )
                   , Declarator->lineno_beg
                   , Declarator->charno_beg
                   , Declarator->lineno_end
                   , Declarator->charno_end
                   );
      }
   }
   LongStringMyFree( &type );
   niveau--;
}

extern void class_method( Class_t Class, Declaration_t Declaration, Declarator_t Declarator, int lineno_end, int charno_end, int lineno_brace_beg, int charno_brace_beg, char *comment )
{
   unsigned long attr = attr_from_declaration( Declaration );
   LongString type;

   niveau++;

   LongStringInit( &type, -1 );
   create_type( &type, Declaration, Declarator );

   switch( Class->access )
   {
   case SN_PRIVATE   :  attr |= PAF_PRIVATE  ; break;
   case SN_PROTECTED :  attr |= PAF_PROTECTED; break;
   case SN_PUBLIC    :  attr |= PAF_PUBLIC   ; break;
   }

   if( Declaration->storage_class == SN_TYPEDEF )
   {
      Put_symbol( PAF_TYPE_DEF
/*              , get_scope( Declarator->name ) */
                , NULL                          /* 17.02.97 rigo */
                , get_name( Declarator->name.buf )
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , f_lineno( 0 )
                , f_charno( 0 )
                , attr
                , type.buf
/*              , (char *) 0 */
                , get_scope( Declarator->name.buf )   /* 17.02.97 rigo */
                , (char *) 0
                , comment
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   else
   {
      int paf_def;
      int paf_dcl;
      char *scope = Class->name.buf;
      char *name  = get_name( Declarator->name.buf );

      attr |= PAF_INLINE;

      if( *name == '~' )
      {
         attr |= PAF_DESTRUCTOR;
      }
      else if( scope != 0 && name != 0 && strcmp( scope, name ) == 0 )
      {
         attr |= PAF_CONSTRUCTOR;
      }

      paf_def = PAF_MBR_FUNC_DEF;
      paf_dcl = PAF_MBR_FUNC_DCL;

      if( Declaration->storage_class == SN_FRIEND )
      {
         unsigned long attr_friend = attr;

         attr_friend |= PAF_PROTECTED;
         attr_friend |= PAF_PRIVATE;
         attr_friend |= PAF_PUBLIC;

         /* extra jelentes a friend-rol */
         Put_symbol( PAF_FRIEND_DCL
                   , scope
                   , name
                   , filename_g
                   , Declarator->name_lineno_beg
                   , Declarator->name_charno_beg
                   , lineno_end
                   , charno_end
                   , attr_friend
                   , type.buf
                   , Declarator->types.buf
                   , Declarator->names.buf
                   , comment
                   , Declarator->lineno_beg
                   , Declarator->charno_beg
                   , Declarator->lineno_end
                   , Declarator->charno_end
                   );
      }

      Put_symbol( paf_def
                , scope
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , lineno_end
                , charno_end
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , comment
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );

      put_cross1( paf_def
                , scope
                , name
                , filename_g
                , lineno_brace_beg
                , charno_brace_beg
                , lineno_end
                , charno_end
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , (char *) 0
                );

      Put_symbol( paf_dcl
                , scope
                , name
                , filename_g
                , Declarator->name_lineno_beg
                , Declarator->name_charno_beg
                , lineno_end
                , charno_end
                , attr
                , type.buf
                , Declarator->types.buf
                , Declarator->names.buf
                , comment
                , Declarator->lineno_beg
                , Declarator->charno_beg
                , Declarator->lineno_end
                , Declarator->charno_end
                );
   }
   type.free(&type);    /* 17.11.97 zsolt */
   niveau--;
}

extern void class_empty_declarator_list( Class_t Class, Declaration_t Declaration )
{
   char *name;
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;

   if( Declaration->storage_class == SN_FRIEND )
   {
      if( Declaration->sClass.name.len )
      {
         name = Declaration->sClass.name.buf;
         lineno_beg = Declaration->sClass.lineno_beg;
         charno_beg = Declaration->sClass.charno_beg;
         lineno_end = Declaration->sClass.lineno_end;
         charno_end = Declaration->sClass.charno_end;
      }
      else if( Declaration->sEnum.name.len )
      {
         name = Declaration->sEnum.name.buf;
         lineno_beg = Declaration->sEnum.lineno_beg;
         charno_beg = Declaration->sEnum.charno_beg;
         lineno_end = Declaration->sEnum.lineno_end;
         charno_end = Declaration->sEnum.charno_end;
      }
      else if( Declaration->complete_class_name.len )
      {
         name = Declaration->complete_class_name.buf;
         lineno_beg = Declaration->lineno_beg;
         charno_beg = Declaration->charno_beg;
         lineno_end = Declaration->lineno_end;
         charno_end = Declaration->charno_end;
      }
      else
      {
         name = 0;
      }

      if( name )
      {
         unsigned long attr_friend = attr_from_declaration( Declaration );

         attr_friend |= PAF_PROTECTED;
         attr_friend |= PAF_PRIVATE;
         attr_friend |= PAF_PUBLIC;

         /* extra jelentes a friend-rol */
         Put_symbol( PAF_FRIEND_DCL
                   , Class->name.buf
                   , name
                   , filename_g
                   , lineno_beg
                   , charno_beg
                   , lineno_end
                   , charno_end
                   , attr_friend
                   , (char *) 0
                   , (char *) 0
                   , (char *) 0
                   , get_comment( lineno_end )
                   , lineno_beg
                   , charno_beg
                   , lineno_end
                   , charno_end
                   );
      }
   }
}

extern void template_argument_skip( LongString *plstr )
{
   int i;
   int bFirst = True;

   niveau++;

   step( 1 );
   i = 1;

   if( plstr )
   {
      LongStringMyAppend( plstr, "<" );
   }

   while( True )
   {
      switch( token( 0 ))
      {
         case '<': i++; break;
         case '>': i--; break;
         case  0 :
            niveau--;
            return;
      }

      if( i == 0 )
      {
         break;
      }

      if( plstr )
      {
         sString_t sIdent = ident( 0 );

         if( bFirst )
         {
            bFirst = False;
         }
         else
         {
            unsigned char c;
            /* 12.02.97 rigo ( '_' -t es '$' -t is vizsgalni kell ) */
            if(( isalnum( c = plstr->buf[plstr->len-1] ) || c == '_' || c == '$' )
            && ( isalnum( c = sIdent.text[ 0] ) || c == '_' || c == '$' ))
            {
               LongStringMyAppend( plstr, " " );
            }
         }

         LongStringIdAppend( plstr, sIdent );
      }

      step( 1 );
   }

   if( plstr )
   {
      LongStringMyAppend( plstr, ">" );
   }

   step( 1 );
   niveau--;
   return;
}

extern int namespace_name( LongString *plstr )
{
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case SN_CLCL:
         if( plstr )
         {
            LongStringMyAppend( plstr, "::" );
         }
         step( 1 );
         break;
         
      case SN_IDENTIFIER:
         if( plstr )
         {
            LongStringIdAppend( plstr, ident( 0 ));
         }
         step( 1 );
         break;

      default:
         niveau--;
         return True;
      }
   }
   niveau--;
}

extern void skip_declaration( void )
{
   int i = 0;
   niveau++;

#ifdef TEST
   printf( "syntax error by file: %s line: %d %s\n"
         , filename_g
         , f_lineno( 0 )
         , StringToText( ident( 0 ))
         );
/*   exit( -1 ); */
#endif

   while( True )
   {
      switch( token( 0 ))
      {
      case LBRACE:
         i++;
         break;
      case RBRACE:
         i--;
/*       if( i <  0 )    rigo */
         if( i <= 0 ) /* rigo */
         {
            step( 1 );
            niveau--;
            return;
         }
         break;
      case 0:  /* EOF */
         niveau--;
         return;
      case ';':
         if( i == 0 )
         {
            step( 1 );
            niveau--;
            return;
         }
         break;
      }

      step( 1 );
   }
   niveau--;
}

extern int skip_member_declaration( void )
{
   int i = 0;
   niveau++;

   while( True )
   {
      int t;

      switch( t = token( 0 ))
      {
      case LBRACE: i++; break;
      case RBRACE: i--; break;
      case  0 :
         niveau--;
         return False;
      }

      if( i == 0 )      /* 10.02.97 rigo */
      {
         switch( t )
         {
         case ';':
            step( 1 );
            niveau--;
            return True;
         case SN_PRIVATE:
         case SN_PUBLIC:
         case SN_PROTECTED:
            if( token( 1 ) == ':' )
            {
               niveau--;
               return True;
            }
            break;
         }
      }

      if( i < 0 )
      {
         niveau--;
         return True;
      }

      step( 1 );
   }
}

extern void function_argument_declaration_list( LongString *types, LongString *names )
{
   int t;
   int i;
   niveau++;

   if( token( 0 ) == '(' )    /* ANSI(( ... )) */
   {
      step( 1 );
      function_argument_declaration_list( types, names );
      i = 0;
      while( True )
      {
         t = token( 0 );
         step( 1 );

         switch( t )
         {
         case '(':
            i++;
            break;
         case ')':
            if( i == 0 )
            {
               niveau--;
               return;
            }
            i--;
            break;
         case 0:
            niveau--;
            return;
         }
      }
   }

   if( token( 0 ) == ')' )
   {
      step( 1 );
      niveau--;
      return;
   }

   while( True )
   {
      function_argument_declaration( types, names );

once_more:
      t = token( 0 );

      if( t == ',' )
      {
         LongStringMyAppend( types, "," );
         LongStringMyAppend( names, "," );
         step( 1 );
      }
      else if( t == ')' )
      {
         step( 1 );
         niveau--;
         return;
         break;
      }
      else if( t == SN_ELLIPSIS )
      {
         LongStringMyAppend( types, "," );
         LongStringMyAppend( names, "," );
      }
      else
      {
         int i;

#ifdef TEST
/*
         printf( "unexpected token in function_argument_declaration: %s: %s (%d.%d)\n"
               , filename_g
               , StringToText( ident( 0 ))
               , f_lineno( 0 )
               , f_charno( 0 )
               );
         exit( -1 );
*/
#endif

         i = 0;

         while( True )
         {
            switch( token( 0 ))
            {
            case '(':
               i++;
               break;

            case ')':
               if( i == 0 )
               {
/*                goto once_more; */
                  step( 1 );  /* 04.10.96 rigo */
                  niveau--;
                  return; /* 02.10.96 rigo */
               }
               i--;
               break;

            case SN_ELLIPSIS:
            case ',':
               if( i == 0 )
                  goto once_more;
               break;

            case 0:
               niveau--;
               return; /* 02.10.96 rigo */
               break;
            }

            step( 1 );
         }
      }
   }
}

extern void function_argument_declaration( LongString *types, LongString *names )
{
   sDeclaration_t sDeclaration;
   sDeclarator_t sDeclarator;
   int count = -1;
   int t;

   niveau++;

   sDeclaration.Declaration   = 0;
   sDeclaration.type          = DECLARATION_UNKNOWN;
   LongStringInit( &sDeclaration.name, -1 );
   sDeclaration.storage_class = 0;
   sDeclaration.fct_specifier = 0;
   sDeclaration.s_const       = 0;
   sDeclaration.s_volatile    = 0;
   sDeclaration.s_char        = 0;
   sDeclaration.s_short       = 0;
   sDeclaration.s_int         = 0;
   sDeclaration.s_long        = 0;
   sDeclaration.s_signed      = 0;
   sDeclaration.s_unsigned    = 0;
   sDeclaration.s_float       = 0;
   sDeclaration.s_double      = 0;
   sDeclaration.s_bool        = 0;
   sDeclaration.s_void        = 0;
   LongStringInit( &sDeclaration.type_name, -1 );
   sDeclaration.type_of_type_name = 0;
   LongStringInit( &sDeclaration.complete_class_name, -1 );
   sDeclaration.lineno_beg    = 0;
   sDeclaration.charno_beg    = 0;
   sDeclaration.lineno_end    = 0;
   sDeclaration.charno_end    = 0;

   sDeclaration.sClass.ClassParent      = 0;
   LongStringInit( &sDeclaration.sClass.name, -1 );
   sDeclaration.sClass.access           = 0;
   sDeclaration.sClass.lineno_beg       = 0;
   sDeclaration.sClass.charno_beg       = 0;
   sDeclaration.sClass.lineno_end       = 0;
   sDeclaration.sClass.charno_end       = 0;

   LongStringInit( &sDeclaration.sEnum.name, -1 );
   sDeclaration.sEnum.lineno_beg        = 0;
   sDeclaration.sEnum.charno_beg        = 0;
   sDeclaration.sEnum.lineno_end        = 0;
   sDeclaration.sEnum.charno_end        = 0;

   LongStringInit( &sDeclarator.name , -1 );
   LongStringInit( &sDeclarator.type , -1 );
   LongStringInit( &sDeclarator.types, -1 );
   LongStringInit( &sDeclarator.names, -1 );
   sDeclarator.base_typ = 0;
   sDeclarator.pure     = 0;
   sDeclarator.lineno_beg = 0;
   sDeclarator.charno_beg = 0;
   sDeclarator.lineno_end = 0;
   sDeclarator.charno_end = 0;

   t = token( 0 );

   if( t == SN_INTEGERconstant )
   {
      LongStringMyAppend( types, "int" );
      LongStringIdAppend( names, ident( 0 ));
      step( 1 );
      niveau--;
      return;
   }

   if( t == SN_FLOATINGconstant )
   {
      LongStringMyAppend( types, "double" );
      LongStringIdAppend( names, ident( 0 ));
      step( 1 );
      niveau--;
      return;
   }

   if( t == SN_STRINGliteral )
   {
      LongStringMyAppend( types, "char*" );

      do
      {
#if 0 /* Zsolt Koppany, 2-apr-1998; We have to skip strings! */
         LongStringIdAppend( names, ident( 0 ));
#endif
         step( 1 );
      } while( token( 0 ) == SN_STRINGliteral );

      niveau--;
      return;
   }

   if( t == SN_CHARACTERconstant )
   {
      LongStringMyAppend( types, "char" );
#if 0 /* Zsolt Koppany, 2-apr-1998; We have to skip strings! */
      LongStringIdAppend( names, ident( 0 ));
#endif
      step( 1 );
      niveau--;
      return;
   }

   if( t == SN_LONGconstant )
   {
      LongStringMyAppend( types, "char" );
      LongStringIdAppend( names, ident( 0 ));
      step( 1 );
      niveau--;
      return;
   }

   if( t == SN_ELLIPSIS )
   {
      LongStringMyAppend( types, "..." );
      LongStringIdAppend( names, ident( 0 ));
      step( 1 );
      niveau--;
      return;
   }

   while( True )
   {
      count++;

      switch( token( 0 ))
      {
      case SN_INTERFACE  :
      case SN_CLASS      :
      case SN_STRUCT     :
      case SN_UNION      :
         function_argument_class(  sDeclaration.storage_class
                                , &sDeclaration.type_name );
         break;

      case SN_ENUM       :
         function_argument_enum(  sDeclaration.storage_class
                               , &sDeclaration.type_name );
         break;

      case SN_AUTO       : sDeclaration.storage_class = SN_AUTO    ; step( 1 ); break;
      case SN_REGISTER   : sDeclaration.storage_class = SN_REGISTER; step( 1 ); break;
      case SN_STATIC     : sDeclaration.storage_class = SN_STATIC  ; step( 1 ); break;
      case SN_EXTERN     : sDeclaration.storage_class = SN_EXTERN  ; step( 1 ); break;
      case SN_INLINE     : sDeclaration.fct_specifier = SN_INLINE  ; step( 1 ); break;
      case SN_VIRTUAL    : sDeclaration.fct_specifier = SN_VIRTUAL ; step( 1 ); break;
      case SN_FRIEND     : sDeclaration.storage_class = SN_FRIEND  ; step( 1 ); break;
      case SN_TYPEDEF    : sDeclaration.storage_class = SN_TYPEDEF ; step( 1 ); break;
      case SN_CONST      : sDeclaration.s_const       = True; step( 1 ); break;
      case SN_VOLATILE   : sDeclaration.s_volatile    = True; step( 1 ); break;
      case SN_CHAR       : sDeclaration.s_char        = True; step( 1 ); break;
      case SN_SHORT      : sDeclaration.s_short       = True; step( 1 ); break;
      case SN_INT        : sDeclaration.s_int         = True; step( 1 ); break;
      case SN_LONG       : sDeclaration.s_long        = True; step( 1 ); break;
      case SN_SIGNED     : sDeclaration.s_signed      = True; step( 1 ); break;
      case SN_UNSIGNED   : sDeclaration.s_unsigned    = True; step( 1 ); break;
      case SN_FLOAT      : sDeclaration.s_float       = True; step( 1 ); break;
      case SN_DOUBLE     : sDeclaration.s_double      = True; step( 1 ); break;
      case SN_BOOL       : sDeclaration.s_bool        = True; step( 1 ); break;
      case SN_VOID       : sDeclaration.s_void        = True; step( 1 ); break;
      case SN_IDENTIFIER :
         /* ha az ident utan * vagy & vagy ident all ( kiveve az 'ident iden (('
            konstrukciot ), akkor nem lehet deklarator_list */
         if(( count      == 0   ) ||
            ( token( 1 ) == '*' ) ||
            ( token( 1 ) == '&' ) ||
            ( token( 1 ) == SN_IDENTIFIER && ( token( 2 ) != '(' || token( 3 ) != '(' ))
           )
         {
         }
         else
         {
            if( function_argument_declarator_one( &sDeclarator ))
            {
               goto end;
            }
            LongStringMyFree( &sDeclarator.name  );
            LongStringMyFree( &sDeclarator.type  );
            LongStringMyFree( &sDeclarator.types );
            LongStringMyFree( &sDeclarator.names );
            sDeclarator.base_typ   = 0;
            sDeclarator.pure       = 0;
            sDeclarator.lineno_beg = 0;
            sDeclarator.charno_beg = 0;
            sDeclarator.lineno_end = 0;
            sDeclarator.charno_end = 0;
         }
         LongStringMySetLen( &sDeclaration.type_name, 0 );
         if( ! complete_class_name( &sDeclaration.type_name ))
         {
            goto end;
         }
         break;

      case SN_CLCL       :
         if( count > 0 )
         {
            if( function_argument_declarator_one( &sDeclarator ))
            {
               goto end;
            }
            LongStringMyFree( &sDeclarator.name  );
            LongStringMyFree( &sDeclarator.type  );
            LongStringMyFree( &sDeclarator.types );
            LongStringMyFree( &sDeclarator.names );
            sDeclarator.base_typ = 0;
            sDeclarator.pure     = 0;
            sDeclarator.lineno_beg = 0;
            sDeclarator.charno_beg = 0;
            sDeclarator.lineno_end = 0;
            sDeclarator.charno_end = 0;
         }
         LongStringMySetLen( &sDeclaration.type_name, 0 );
         if( ! complete_class_name( &sDeclaration.type_name ))
         {
            goto end;
         }
         break;

      case ','        :
      case ')'        :
         /* abstract declarator */
         goto end;
         
      case '+'        :
         /* abstract declarator + abstract_declarator */
      case '-'        :
         /* abstract declarator - abstract_declarator */
      case '='        :
         /* abstract declarator intialisiert */
         skip_expression();
         goto end;

      case SN_OPERATOR   :
      case '('        :
      case '*'        :
      case '&'        :
      case '~'        :
      case '['        : /* 30.09.96 abstract declarator */
         if( function_argument_declarator_one( &sDeclarator ))
         {
            goto end;
         }
         else
         {
            LongStringMyFree( &sDeclarator.name  );
            LongStringMyFree( &sDeclarator.type  );
            LongStringMyFree( &sDeclarator.types );
            LongStringMyFree( &sDeclarator.names );
            sDeclarator.base_typ = 0;
            sDeclarator.pure     = 0;
            sDeclarator.lineno_beg = 0;
            sDeclarator.charno_beg = 0;
            sDeclarator.lineno_end = 0;
            sDeclarator.charno_end = 0;
            goto end;
         }
         break;

      default         :
         goto end;
         break;
      }
   }

end:
#ifdef VERSION_1
   create_type( types, &sDeclaration, &sDeclarator );
#else
   create_type_argument_list( types, &sDeclaration, &sDeclarator );
#endif
   LongStringsMyAppend( names, &sDeclarator.name );
   LongStringMyFree( &sDeclaration.name );
        LongStringMyFree( &sDeclaration.complete_class_name );
        LongStringMyFree( &sDeclaration.sClass.name );
        LongStringMyFree( &sDeclaration.type_name );
        LongStringMyFree( &sDeclaration.sClass.name );
        LongStringMyFree( &sDeclaration.sEnum.name );
   LongStringMyFree( &sDeclarator.name  );
   LongStringMyFree( &sDeclarator.type  );
   LongStringMyFree( &sDeclarator.types );
   LongStringMyFree( &sDeclarator.names );
   niveau--;
   return;
}

extern void function_argument_class( int storage_class, LongString *plstr )
{
   int i;
   int t;

   niveau++;

   step( 1 );

   switch( token( 0 ))
   {
   case SN_IDENTIFIER:

      if( token( 1 ) == SN_IDENTIFIER &&
        ( token( 2 ) == ':' || token( 2 ) == LBRACE || token( 2 ) == '<' ))
      {
         step( 1 );  /* atlepjuk a G_EXP_IMP-et */
      }

      LongStringIdAppend( plstr, ident( 0 ));

      step( 1 );

      if( token( 0 ) == '<' )
      {
         template_argument_skip( plstr );
      }

      if( token( 0 ) == ':' )
      {
         function_argument_base_spec();
         while(( t = token( 0 )) != LBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
         {
            step( 1 );
         }
      }

      if( token( 0 ) == LBRACE )
      {
         step( 1 );
         i = 1;
         while( i > 0 )
         {
            switch( token( 0 ))
            {
            case '{': i++; break;
            case '}': i--; break;
            case  0 :
               niveau--;
               return;
            }
            step( 1 );
         }
      }
      niveau--;
      return;

   case ':'       :
      if( storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( 0, 0, &str_name ))
         {
            LongStringIdAppend( plstr, str_name );
         }
         else
         {
            LongStringMyAppend( plstr, ANONYM );
         }
      }
      else
      {
         LongStringMyAppend( plstr, ANONYM );
      }
      function_argument_base_spec();

      while(( t = token( 0 )) != LBRACE && t != 0 )   /* t != 0 10.02.97 rigo */
      {
         step( 1 );
      }

      if( token( 0 ) == LBRACE )
      {
         step( 1 );
         i = 1;
         while( i > 0 )
         {
            switch( token( 0 ))
            {
            case '{': i++; break;
            case '}': i--; break;
            case  0 :
               niveau--;
               return;
            }
            step( 1 );
         }
      }
      niveau--;
      return;

   case LBRACE    :
      if( storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( 0, 0, &str_name ))
         {
            LongStringIdAppend( plstr, str_name );
         }
         else
         {
            LongStringMyAppend( plstr, ANONYM );
         }
      }
      else
      {
         LongStringMyAppend( plstr, ANONYM );
      }
      step( 1 );
      i = 1;
      while( i > 0 )
      {
         switch( token( 0 ))
         {
         case '{': i++; break;
         case '}': i--; break;
         case  0 :
            niveau--;
            return;
         }
         step( 1 );
      }
      niveau--;
      return;

   default:
      niveau--;
      return;
   }
   niveau--;
   return;
}

extern void function_argument_enum( int storage_class, LongString *plstr )
{
   int i;

   niveau++;

   LongStringMyAppend( plstr, "enum " );
   step( 1 );

   switch( token( 0 ))
   {
   case ',':
   case ')':
      niveau--;
      return;
      
   default: /* c++ keywords */
   case SN_IDENTIFIER:
      
      LongStringIdAppend( plstr, ident( 0 ));
      step( 1 );
      if( token( 0 ) == LBRACE )
      {
         step( 1 );
         i = 1;
         while( i > 0 )
         {
            switch( token( 0 ))
            {
            case '{': i++; break;
            case '}': i--; break;
            case  0 :
               niveau--;
               return;
            }
            step( 1 );
         }
      }
      niveau--;
      return;
      break;

   case LBRACE:
      if( storage_class == SN_TYPEDEF ) /* 11.02.97 rigo */
      {
         sString_t str_name;

         if( get_struct_name( 0, 0, &str_name ))
         {
            LongStringIdAppend( plstr, str_name );
         }
         else
         {
            LongStringMyAppend( plstr, ANONYM );
         }
      }
      else
      {
         LongStringMyAppend( plstr, ANONYM );
      }
      step( 1 );
      i = 1;
      while( i > 0 )
      {
         switch( token( 0 ))
         {
         case '{': i++; break;
         case '}': i--; break;
         case  0 :
            niveau--;
            return;
         }
         step( 1 );
      }
      niveau--;
      return;
      break;
   }
   niveau--;
   return;
}

extern int function_argument_declarator_one( Declarator_t Declarator )
{
   Save_d();
   int t;
   niveau++;

   if( function_argument_declarator( Declarator ))
   {
      t = token( 0 );

      if( t == ',' || t == ')' || t == SN_ELLIPSIS )
      {
         niveau--;
         return True;
      }
      if( t == '=' || t == '+' || t == '-' )
      {
         skip_expression();
         niveau--;
         return True;
      }
   }

   Restore_d();
   niveau--;
   return False;
}

extern void skip_expression( void )
{
   int paren = 0;
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case '(':
         paren++;
         break;

      case ')':
         if( paren == 0 )
         {
            niveau--;
            return;
         }
         paren--;
         break;

      case ',':
         if( paren == 0 )
         {
            niveau--;
            return;
         }
         break;
      }
      step( 1 );
   }
   niveau--;
   return;
}

extern int function_argument_declarator( Declarator_t Declarator )
{
   Save_d();
   int ret;
   int len = Declarator->type.len;

   niveau++;

   switch( token( 0 ))
   {
   case '('        :
      ret = function_argument_declarator_paren( Declarator );
      break;
   case '*'        :
      ret = function_argument_declarator_star( Declarator );
      break;
   case '&'        :
      ret = function_argument_declarator_star( Declarator );
      break;

   case SN_IDENTIFIER :
   case SN_CLCL       :
      ret = function_argument_declarator_identifier( Declarator );
      break;

   case SN_OPERATOR   :
      ret = function_argument_declarator_operator( Declarator );
      break;

   case '~'        :
      ret = function_argument_declarator_tilde( Declarator );
      break;

   default         : /* abstract declarator */
      ret = function_argument_function_or_array_list( Declarator );  /* 30.09.96 abstract declarator */
      break;
   }

   if( ! ret )
   {
      Restore_d();
      LongStringMySetLen( &Declarator->type, len );
   }

   niveau--;
   return ret;
}

extern int function_argument_declarator_paren( Declarator_t Declarator )
{
   Save_d();
   int len = Declarator->type.len;

   niveau++;

   LongStringMyAppend( &Declarator->type, "(" );

   step( 1 );
   if( function_argument_declarator( Declarator ))
   {
      if( token( 0 ) == ')' )
      {
         LongStringMyAppend( &Declarator->type, ")" );
         step( 1 );
         if( function_argument_function_or_array_list( Declarator ))
         {
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
            LongStringMySetLen( &Declarator->type, len );
            return False;
         }
      }
      else
      {
         Restore_d();
         niveau--;
         LongStringMySetLen( &Declarator->type, len );
         return False;
      }
   }
   else
   {
      Restore_d();
      niveau--;
      LongStringMySetLen( &Declarator->type, len );
      return False;
   }
   niveau--;
   LongStringMySetLen( &Declarator->type, len );
   return False;
}

extern int function_argument_declarator_star( Declarator_t Declarator )
{
   Save_d();
   LongString aname;
   int len;

   niveau++;
   LongStringInit( &aname, -1 );

   if( ptr_operator( &aname ))
   {
      LongStringsMyAppend( &Declarator->type, &aname );
      LongStringMyFree( &aname );
      len = Declarator->type.len;
      if( function_argument_declarator( Declarator ))
      {
         if( Declarator->base_typ == 0 )
         {
            Declarator->base_typ = POINTER;
         }
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->type, len );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      LongStringMyFree( &aname );
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int function_argument_declarator_identifier( Declarator_t Declarator )
{
   Save_d();
   int lineno = f_lineno( 0 );
   int charno = f_charno( 0 );
   int t;
   LongString aname;
   int len;

   niveau++;

   t = token( 0 );

   if( t == SN_IDENTIFIER )
   {
      t = token( 1 );

      if( t == SN_IDENTIFIER )
      {
         if( token( 2 ) == '(' && token( 3 ) == '(' )
         {
         /* int f ANSI(( int b )); miatt */
         }
         else
         {
         /* const CString& AFXAPI AfxGetEmptyString(); miatt */
            step( 1 );
            lineno = f_lineno( 0 );
            charno = f_charno( 0 );
         }
      }
      else if( token( 1 ) == SN_OPERATOR )
         /* void* AFX_CDECL operator new( void ); miatt */
      {
         step( 1 );
         lineno = f_lineno( 0 );
         charno = f_charno( 0 );
         if( function_argument_declarator_operator( Declarator ))
         {
            niveau--;
            return True;
         }
         else
         {
            Restore_d();
            niveau--;
            return False;
         }
      }
   }

   LongStringInit( &aname, -1 );
   if( ptr_operator( &aname ))
   {
      LongStringsMyAppend( &Declarator->type, &aname );
      LongStringMyFree( &aname );
      len = Declarator->type.len;
      if( function_argument_declarator( Declarator ))
      {
         if( Declarator->base_typ == 0 )
         { /* 07.03.97 rigo: int (XX:*a)(); miatt */
            Declarator->base_typ = POINTER;
         }
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->type, len );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else if( qualified_name( &Declarator->name
                          , &Declarator->name_lineno_beg
                          , &Declarator->name_charno_beg ))
   {
      LongStringMyFree( &aname );
      Declarator->lineno_beg = lineno;
      Declarator->charno_beg = charno;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identleng( -1 );
      if( function_argument_function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->name, 0 );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      LongStringMyFree( &aname );
      LongStringMySetLen( &Declarator->name, 0 );
      Restore_d();
      niveau--;
      return False;
   }
   LongStringMyFree( &aname );
   Restore_d();
   niveau--;
   return False;
}

extern int function_argument_declarator_operator( Declarator_t Declarator )
{
   Save_d();
   int lineno = f_lineno( 0 );
   int charno = f_charno( 0 );
   niveau++;

   if( operator_function_name( &Declarator->name ))
   {
      Declarator->name_lineno_beg = Declarator->lineno_beg = lineno;
      Declarator->name_charno_beg = Declarator->charno_beg = charno;
      Declarator->lineno_end = f_lineno( -1 );
      Declarator->charno_end = f_charno( -1 ) + identleng( -1 );
      if( function_argument_function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         LongStringMySetLen( &Declarator->name, 0 );
         Restore_d();
         niveau--;
         return False;
      }
   }
   else
   {
      Restore_d();
      LongStringMySetLen( &Declarator->name, 0 );
      niveau--;
      return False;
   }
   niveau--;
}

extern int function_argument_declarator_tilde( Declarator_t Declarator )
{
   int lineno_beg;
   int charno_beg;
   Save_d();
   niveau++;

   lineno_beg = f_lineno( 0 );
   charno_beg = f_charno( 0 );
   step( 1 );

   if( token( 0 ) == SN_IDENTIFIER )
   {
      Declarator->name_lineno_beg = Declarator->lineno_beg = lineno_beg;
      Declarator->name_charno_beg = Declarator->charno_beg = charno_beg;
      LongStringMyCopy  ( &Declarator->name, "~" );
      LongStringIdAppend( &Declarator->name, ident( 0 ));
      Declarator->lineno_end = f_lineno( 0 );
      Declarator->charno_end = f_charno( 0 ) + identleng( 0 );
      step( 1 );
      if( function_argument_function_or_array_list( Declarator ))
      {
         niveau--;
         return True;
      }
      else
      {
         Restore_d();
         LongStringMySetLen( &Declarator->name, 0 );
         niveau--;
         return False;
      }
   }
   else
   {
      Restore_d();
      niveau--;
      return False;
   }
   niveau--;
}

extern int function_argument_function_or_array_list( Declarator_t Declarator )
{
   Save_d();
   int i = 0;
   niveau++;

   /* 11.09.96 rigo - int a ANSI(( int b )); miatt */
   if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == '(' && token( 2 ) == '(' )
   {
      step( 1 );
   }

   while( True )
   {
      switch( token( 0 ))
      {
      case '('       :
         if( ! function_argument_function_operator( Declarator ))
         {
            Restore_d();
            niveau--;
            return False;
         }
         if( ++i == 2 )
         {
            Restore_d();
            niveau--;
            return False;
         }
         break;

      case '['       :
         if( ! function_argument_array_operator( Declarator ))
         {
            Restore_d();
            niveau--;
            return False;
         }
         break;

      default        :
         niveau--;
         return True;
      }
   }
   niveau--;
   return True;
}

extern int function_argument_function_operator( Declarator_t Declarator )
{
   Save_d();
   int i;

   niveau++;

   step( 1 );

   i = 1;
   while( i > 0 )
   {
      switch( token( 0 ))
      {
      case '(': step( 1 ); i++; break;
      case ')': step( 1 ); i--; break;
      case  0 : 
         Restore_d();
         niveau--;
         return False;
      default : step( 1 ); break;
      }
   }

   LongStringMyAppend( &Declarator->type, "()" );

   cv_qualifier_list_opt( &Declarator->type );

   if( Declarator->base_typ == 0 )
   {
      Declarator->base_typ = FUNCTION;
   }

   niveau--;
   return True;
}

extern int function_argument_array_operator( Declarator_t Declarator )
{
   Save_d();
   int i;
   niveau++;

   step( 1 );
   i = 1;

   while( i > 0 )
   {
      switch( token( 0 ))
      {
      case '[': step( 1 ); i++; break;
      case ']': step( 1 ); i--; break;
      case  0 : 
         Restore_d();
         niveau--;
         return False;
      default : step( 1 ); break;
      }
   }

#if ARRAY
   LongStringMyAppend( &Declarator->type, "[]" );
#else
/* azt szeretnenk elerni, hogy a fuggvenyargumentum listaban az array helyett
   pointer alljon (17.11.97 rigo) */
   {
      LongString name;

      LongStringInit( &name, -1 );
      LongStringMyCopy( &name, "*" );
      LongStringsMyAppend( &name, &Declarator->type );

      LongStringMyFree( &Declarator->type );
      Declarator->type = name;
   }
#endif /* ARRAY */
   if( Declarator->base_typ == 0 )
   {
      Declarator->base_typ = ARRAY;
   }

   niveau--;
   return True;
}

extern int function_argument_base_spec( void )
{
   Save_d();
   niveau++;

   while( True )
   {
      switch( token( 0 ))
      {
      case 0:
         Restore_d();
         niveau--;
         return False;
      case LBRACE:
         niveau--;
         return True;
      }
      step( 1 );
   }
   niveau--;
}

#define f_Strcat( type, string ) \
   if( ! empty ) { LongStringMyAppend( type, " " ); } \
   LongStringMyAppend( type, string ); \
   empty = False;

extern void create_type( LongString *type, Declaration_t Declaration, Declarator_t Declarator )
{
   int empty = True;
   int bType = False;   /* is explicit type defined ? */

/* printf( "***** create_type vor: <%s>\n", type->buf ); */

   if( Declaration->fct_specifier == SN_INLINE   )
   {
      f_Strcat( type, "inline" );
   }

   if( Declaration->fct_specifier == SN_VIRTUAL  )
   {
      f_Strcat( type, "virtual" );
   }

   if( Declaration->s_const )
   {
      f_Strcat( type, "const" );
   }

   if( Declaration->s_volatile )
   {
      f_Strcat( type, "volatile" );
   }

   if( Declaration->s_unsigned )
   {
      f_Strcat( type, "unsigned" );
   }

   if( Declaration->s_short )
   {
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double )
      {
         f_Strcat( type, "short" );
         bType = True;
      }
   }

   if( Declaration->s_long )
   {
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double )
      {
         f_Strcat( type, "long" );
         bType = True;
      }
   }

   if( Declaration->s_char )
   {
      f_Strcat( type, "char" );
      bType = True;
   }

   if( Declaration->s_int )
   {
      /**/ if( Declaration->s_long  )
      {
         f_Strcat( type, "long" );
      }
      else if( Declaration->s_short )
      {
         f_Strcat( type, "short" );
      }
      else
      {
         f_Strcat( type, "int" );
      }
      bType = True;
   }

   if( Declaration->s_float )
   {
      if( Declaration->s_long )
      {
         f_Strcat( type, "double" );
      }
      else
      {
         f_Strcat( type, "float" );
      }
      bType = True;
   }

   if( Declaration->s_double )
   {
      if( Declaration->s_short )
      {
         f_Strcat( type, "float" );
      }
      else
      {
         f_Strcat( type, "double" );
      }
      bType = True;
   }
   
   if( Declaration->s_bool )
   {
      f_Strcat( type, "bool" );
      bType = True;
   }

   if( Declaration->s_void )
   {
      f_Strcat( type, "void" );
      bType = True;
   }

   if( Declaration->type_name.len )
   {
      f_Strcat( type, Declaration->type_name.buf );
      bType = True;
   }
   else if( Declaration->type_of_type_name )
   {
      switch( Declaration->type_of_type_name )
      {
      case SN_INTERFACE:
      case SN_CLASS:
      case SN_STRUCT:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      case SN_UNION:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      case SN_ENUM:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      }
      bType = True;
   }
   
   if( ! bType )
   {
      f_Strcat( type, "int" );
      bType = True;
   }

   if( Declarator->type.len )
   {
/*       printf( "Declarator->type: %s\n", Declarator->type.buf ); */
      f_Strcat( type, Declarator->type.buf );
   }

/* printf( "..... create_type aft: <%s>\n", type->buf ); */

}

#ifndef VERSION_1
extern void create_type_argument_list( LongString *type, Declaration_t Declaration, Declarator_t Declarator )
{
   int empty = True;
   int bType = False;   /* is explicit type defined ? */

   if( Declaration->fct_specifier == SN_INLINE   )
   {
      f_Strcat( type, "inline" );
   }

   if( Declaration->fct_specifier == SN_VIRTUAL  )
   {
      f_Strcat( type, "virtual" );
   }

   if( Declaration->s_const )
   {
      f_Strcat( type, "const" );
   }

   if( Declaration->s_volatile )
   {
      f_Strcat( type, "volatile" );
   }

   if( Declaration->s_unsigned )
   {
      f_Strcat( type, "unsigned" );
   }

   if( Declaration->s_short )
   {
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double )
      {
         f_Strcat( type, "short" );
         bType = True;
      }
   }

   if( Declaration->s_long )
   {
      if( ! Declaration->s_int &&
          ! Declaration->s_float &&
          ! Declaration->s_double )
      {
         f_Strcat( type, "long" );
         bType = True;
      }
   }

   if( Declaration->s_char )
   {
      f_Strcat( type, "char" );
      bType = True;
   }

   if( Declaration->s_int )
   {
      /**/ if( Declaration->s_long  )
      {
         f_Strcat( type, "long" );
      }
      else if( Declaration->s_short )
      {
         f_Strcat( type, "short" );
      }
      else
      {
         f_Strcat( type, "int" );
      }
      bType = True;
   }

   if( Declaration->s_float )
   {
      if( Declaration->s_long )
      {
         f_Strcat( type, "double" );
      }
      else
      {
         f_Strcat( type, "float" );
      }
      bType = True;
   }

   if( Declaration->s_double )
   {
      if( Declaration->s_short )
      {
         f_Strcat( type, "float" );
      }
      else
      {
         f_Strcat( type, "double" );
      }
      bType = True;
   }

   if( Declaration->s_bool )
   {
      f_Strcat( type, "bool" );
      bType = True;
   }

   if( Declaration->s_void )
   {
      f_Strcat( type, "void" );
      bType = True;
   }

   if( Declaration->type_name[0] )
   {
      f_Strcat( type, Declaration->type_name );
      bType = True;
   }
   else if( Declaration->type_of_type_name )
   {
      switch( Declaration->type_of_type_name )
      {
      case SN_INTERFACE:
      case SN_CLASS:
      case SN_STRUCT:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      case SN_UNION:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      case SN_ENUM:
         LongStringMyAppend( type, ANONYM );
         empty = False;
         break;
      }
      bType = True;
   }
   
   if( ! bType )
   {
      if( ! isalnum( Declarator->type[0] ))
      {
         f_Strcat( type, "int" );
         bType = True;
      }
   }

   if( Declarator->type[0] )
   {
/*    printf( "Declarator->type: %s\n", Declarator->type ); */
      f_Strcat( type, Declarator->type );
   }
}
#endif /* VERSION_1 */

/*****************************************************************************/

extern unsigned long attr_from_declaration( Declaration_t Declaration )
{
   unsigned long attr = 0;

   switch( Declaration->storage_class )
   {
      case SN_STATIC: attr |= PAF_STATIC; break;
   }

   switch( Declaration->fct_specifier )
   {
      case SN_INLINE : attr |= PAF_INLINE ; break;
      case SN_VIRTUAL: attr |= PAF_VIRTUAL; break;
   }

   if( Declaration->s_volatile )
   {
      attr |= PAF_VOLATILE;
   }

   return attr;
}

extern char *get_scope( char *name )
{
   char *pc;
   static char ac[2000];

   if( name && ( pc = strrchr( name, ':' )))
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

extern char *get_name( char *name )
{
   char *pc;

   if( name && ( pc = strrchr( name, ':' )))
   {
      return pc+1;
   }
   else
   {
      return name;
   }
}

extern void put_cross1( int type, char *scope, char *sym_name, char *file, int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr, char *ret, char *types, char *names, char *reserved )
{
   if( cross_ref_fp )
   {
      Tcl_DString utfString;
      char tmp[1000];

      snprintf(tmp, sizeof(tmp), "%d;%d;%s;%d;%d;%d;%d;%ld;%s;%s;%s;%s;%s;%d;\n"
          , PAF_CROSS_REF_CPP
          , type
          , null_safe( file )
          , start_lineno
          , start_colpos
          , end_lineno
          , end_colpos
          , attr
          , null_safe( ret )
          , null_safe( scope )
          , null_safe( sym_name )
          , null_safe( types )
          , null_safe( names )
          , keyw_cpp
          );
      Tcl_ExternalToUtfDString(NULL, tmp, -1, &utfString);
      fprintf( cross_ref_fp, "%s", Tcl_DStringValue(&utfString));
      Tcl_DStringFree(&utfString);
   }
}

extern int get_struct_name( int *plineno, int *pcharno, sString_t *psString ) /* 11.02.97 rigo */
{
   Save_d();
   int i = 0;
   int t;

   while( True )
   {
      if(( t = token( 0 )) == LBRACE || t == 0 )
      {
         break;
      }
      step( 1 );
   }

   while( True )
   {
      t = token( 0 );

      if( t == 0      ) break;
      if( t == LBRACE ) i++;
      if( t == RBRACE ) i--;

      step( 1 );

      if( i == 0      ) break;
   }

   while( True )  /* 17.11.97 rigo */
   {
      if( token( 0 ) == SN_IDENTIFIER )
      {
         t = token( 1 );

         if( t == ',' || t == ';' || t == '=' )
         {
            (*psString) = ident( 0 );
            if( plineno ) (*plineno) = f_lineno( 0 );
            if( pcharno ) (*pcharno) = f_charno( 0 );
            Restore_d();
            return True;
         }
      }

      while( True )
      {
         step( 1 );

         t = token( 0 );

         if( t == 0      ) break;
         if( t == LBRACE || t == '[' || t == '(' ) i++;
         if( t == RBRACE || t == ']' || t == ')' ) i--;

         if( i == 0 && ( t == ',' || t == ';' || t == 0 )) break;
      }

      if( t == ';' || t == 0 ) break;

      step( 1 );
   }

   Restore_d();
   return False;
}

extern int is_single_parameter_list( LongString *types )
{
   register unsigned char c;
   char *pc = types->buf;

   if( pc == 0 || *pc == 0 )  /* 17.11.97 rigo */
   {
      return False;
   }

   while(( c = *pc++ ))
   {
      if( ! ( isalnum( c ) ||
          c == '_' ||
          c == '$' ||
          c == '.' ||   /* ellipsis */
          c == ',' ))
      {
         return False;
      }
   }

   return True;
}

extern int array_change_type_and_name( Array_t ArrayTypes, Array_t ArrayNames, char *type, char *name )
{
   int i;

   if( ArrayTypes && ArrayNames )
   {
      for( i = 0; ArrayTypes[i].string; i++ )
      {
         if( ! ArrayTypes[i].changed && strcmp( ArrayTypes[i].string, name ) == 0 )
         {
            my_free( ArrayTypes[i].string );
            my_free( ArrayNames[i].string );

            ArrayTypes[i].string  = my_strdup( type );
            ArrayNames[i].string  = my_strdup( name );

            ArrayTypes[i].changed = True;
            ArrayNames[i].changed = True;
            return True;
         }
      }
   }

   return False;
}

extern void array_change_not_changed( Array_t ArrayTypes, Array_t ArrayNames )
{
   int i;

   if( ArrayTypes && ArrayNames )
   {
      for( i = 0; ArrayTypes[i].string; i++ )
      {
         if( ! ArrayTypes[i].changed )
         {
            my_free( ArrayNames[i].string );

            ArrayNames[i].string  = ArrayTypes[i].string;
            ArrayTypes[i].string  = my_strdup( "int" );

            ArrayTypes[i].changed = True;
            ArrayTypes[i].changed = True;
         }
      }
   }
}

extern Array_t array_create( LongString *plstr )
{
   Array_t Array;
   char *pcEnd;
   char *pcBeg;
   int i;
   char *string;

   if( plstr == 0 )
   {
      return 0;
   }

   string = plstr->buf;

   if( string == 0 )
   {
/*    return 0; */
      string = "";
   }

   for( i = 0, pcEnd = string; *pcEnd; pcEnd++ )
   {
      if( *pcEnd == ',' )
      {
         i++;
      }
   }

   Array = (Array_t) ckalloc(( i + 2 ) * sizeof( Array[0] ));

   for( i = 0, pcBeg = pcEnd = string; *pcEnd; pcEnd++ )
   {
      if( *pcEnd == ',' )
      {
         char c = *pcEnd;

         *pcEnd = 0;

         Array[i].string = my_strdup( pcBeg );
         Array[i].changed = False;
   
         *pcEnd = c;
         pcBeg = pcEnd+1;
         i++;
      }
   }

   Array[i].string = my_strdup( pcBeg );
   Array[i].changed = False;
   
   i++;

   Array[i].string = 0;
   Array[i].changed = False;

   return Array;
}

extern void array_destroy( Array_t Array )
{
   if( Array )
   {
      int i;

      for( i = 0; Array[i].string; i++ )
      {
         ckfree( (char*)Array[i].string );
      }

      ckfree( (char*)Array );
   }
}

#ifdef ARRAY_TEST
extern void array_print( Array_t Array )
{
   if( Array )
   {
      int i;

      printf( "Array:\n" );

      for( i = 0; Array[i].string; i++ )
      {
         printf( "  elem: %s <%s>\n"
               , Array[i].changed ? "    changed" : "not changed"
               , Array[i].string
               );
      }
   }
   else
   {
      printf( "Array is null\n" );
   }
}
#endif /* ARRAY_TEST */

extern void array_to_string( LongString *plstr, Array_t Array )
{
/* LongStringInit( plstr, -1 );  17.11.97 rigo */
   LongStringMyFree( plstr ); /* 17.11.97 rigo */

   if( Array )
   {
      int i;

      for( i = 0; Array[i].string; i++ )
      {
         if( i > 0 )
         {
            LongStringMyAppend( plstr, "," );
         }
         LongStringMyAppend( plstr, Array[i].string );
      }
   }
}

/* A kovetkezo miatt:
**    int imag (const x) __attribute__ ((const))
**    {
**    }
** Atlepem a macrot, es megnezem, hogy t a kovetkezo token.
** Ha nem , akkor restore es return False
** Ha igen, akkor return True
*/

extern int skip_macro( int t )
{
   Save_d();

   niveau++;

   if( token( 0 ) == SN_IDENTIFIER )
   {
      step( 1 );

      if( token( 0 ) == '(' )
      {
         int i = 1;
         step( 1 );

         while( i > 0 )
         {
            if( token( 0 ) == ')' )
            {
               i--;
            }
            else if( token( 0 ) == '(' )
            {
               i++;
            }
            else if( token( 0 ) == '0' )
            {
               break;
            }
            step( 1 );
         }
      }
   }

   if( token( 0 ) == t )
   {
      niveau--;
      return True;
   }
   else
   {
      niveau--;
      Restore_d();
      return False;
   }
}

/* A kovetkezo miatt:
**    class _OS_DLLIMPORT(_OS_TSOM) os_date
**    {
**    };
**
** fuggvenykent jelentodik le
** A macro csak zarojeles lehet
*/

extern void skip_macro_2( void )   /* 17.11.97 rigo */
{
   niveau++;

   if( token( 0 ) == SN_IDENTIFIER && token( 1 ) == '(' )
   {
      int i = 1;
      step( 2 );

      while( i > 0 )
      {
         if( token( 0 ) == ')' )
         {
            i--;
         }
         else if( token( 0 ) == '(' )
         {
            i++;
         }
         else if( token( 0 ) == '0' )
         {
            break;
         }
         step( 1 );
      }
   }
   niveau--;
}

static FILE *fp;

extern int Put_symbol (int type, char *scope, char *sym_name, char *file, int start_lineno, int start_colpos, int end_lineno, int end_colpos, unsigned long attr, char *ret, char *arg_types, char *args, char *reserved, int start_lineno_highlight, int start_colpos_highlight, int end_lineno_highlight, int end_colpos_highlight )
{
   int retval;

   if( ! bPutSymbol_m )
   {
      return 0;
   }

#ifdef TRACE_FILE
   if( fp == 0 )
   {
      fp = fopen( "cbrowser.trace", "w+" );
   }
#endif

   retval = put_symbol( type
                      , scope
                      , sym_name
                      , file
                      , start_lineno
                      , start_colpos
                      , end_lineno
                      , end_colpos
                      , attr
                      , ret
                      , arg_types
                      , args
                      , reserved
                      , start_lineno_highlight
                      , start_colpos_highlight
                      , end_lineno_highlight
                      , end_colpos_highlight );

#ifdef COMMENT_TEST
   printf( "%s comment: %s\n"
         , null_safe( sym_name )
         , null_safe( reserved )
         ); 
#endif

   if( fp )
   {
      fprintf( fp, "put_symbol : |%s|%s|%s|%s|%d|%d|%d|%d|%ld|%s|%s|%s|%s|%d|%d|%d|%d|\n"
             , paf_type_to_string( type )
             , null_safe( scope )
             , null_safe( sym_name )
             , null_safe( file )
             , start_lineno
             , start_colpos
             , end_lineno
             , end_colpos
             , attr
             , null_safe( ret )
             , null_safe( arg_types )
             , null_safe( args )
             , null_safe( reserved )
             , start_lineno_highlight
             , start_colpos_highlight
             , end_lineno_highlight
             , end_colpos_highlight );
   }

   return retval;
}

extern char *paf_type_to_string( int paf_type )
{
   char *atype;
   static char acType[100];

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
   default:
      sprintf( acType, "%3d           ", paf_type );
      atype = acType;
      break;
   }

   return atype;
}

extern char *get_comment( int lineno )
{
   Save_d();
   extern int comment_database;

   if( ! comment_database ) return 0;

   /* beolvassuk addig a file-t, amig biztosak nem lehetunk abban, hogy a keresett commentet is beolvastuk */

   while( True )
   {
      if( token( 0 ) == 0 ) break;  /* end of file */
      if( f_lineno( 0 ) > lineno ) break;

      step( 1 );
   }

   Restore_d();
   return find_comment( lineno );
}

struct sComment
{
   char *classname;     /* 08.04.97 */
   char *funcname;      /* 08.04.97 */
   char *text;
   int lineno;
   int charno;          /* 08.04.97 */
   int length;
   int putted;          /* 08.04.97 */
};

static struct sComment asComment[10];
static int iComment;

#define size(x) (sizeof(x)/sizeof(x[0]))

extern void init_comment( void )   /* 07.04.97 rigo */
{
   int i;

   for( i = 0; i < size( asComment ); i++ )
   {  
      my_free( asComment[i].classname );     /* 08.04.97 */
      my_free( asComment[i].funcname );      /* 08.04.97 */
      asComment[i].classname = 0;            /* 08.04.97 */
      asComment[i].funcname = 0;             /* 08.04.97 */
      asComment[i].text   = 0;
      asComment[i].lineno = 0;
      asComment[i].charno = 0;               /* 08.04.97 */
      asComment[i].length = 0;
      asComment[i].putted = False;           /* 08.04.97 */
   }

   return;
}

extern void save_comment( int lineno, int charno, char *text, int length )
{
   extern int comment_database;

   /* 08.04.97 */
   if( comment_database && asComment[iComment].text && ! asComment[iComment].putted )
   {
/*    asComment[iComment].text[length] = 0; 18.11.97 rigo */
      asComment[iComment].text[asComment[iComment].length] = 0;

      put_comment( asComment[iComment].classname
                 , asComment[iComment].funcname
                 , filename_g
                 , asComment[iComment].text
                 , asComment[iComment].lineno
                 , asComment[iComment].charno );
   }

   my_free( asComment[iComment].classname );                   /* 08.04.97 */
   my_free( asComment[iComment].funcname );                    /* 08.04.97 */
   asComment[iComment].classname = my_strdup( classname_g );   /* 08.04.97 */
   asComment[iComment].funcname = my_strdup( funcname_g );     /* 08.04.97 */
   asComment[iComment].text   = text;
   asComment[iComment].lineno = lineno;
   asComment[iComment].charno = charno;                        /* 08.04.97 */
   asComment[iComment].length = length;
   asComment[iComment].putted = False;                         /* 08.04.97 */

   if( ++iComment == size( asComment ))
   {
      iComment = 0;
   }
}

extern char *find_comment( int lineno )
{
   int i;
   int j;
   char *text;
   int length;

   for( i = 0, j = iComment; i < size( asComment ); i++ )
   {  
      if( asComment[j].lineno == lineno )
      {
         text = asComment[j].text;
         length = asComment[j].length;

         if( text )
         {
            text[length] = 0;
            asComment[j].putted = True;         /* 08.04.97 */
            return text;
         }
         else
         {
            return 0;
         }
      }
      if( ++j == size( asComment ))
      {
         j = 0;
      }
   }

   return 0;
}

/* 08.04.97 */
extern void flush_comment( void )
{
   extern int comment_database;
   int i;

   for( i = 0; i < size( asComment ); i++ )
   {  
      if( comment_database && asComment[i].text && ! asComment[i].putted )
      {
         asComment[i].text[asComment[i].length] = 0;

         put_comment( asComment[i].classname
                    , asComment[i].funcname
                    , filename_g
                    , asComment[i].text
                    , asComment[i].lineno
                    , asComment[i].charno );
      }

      my_free( asComment[i].classname );
      my_free( asComment[i].funcname );
      asComment[i].classname = 0;
      asComment[i].funcname = 0;
      asComment[i].text   = 0;
      asComment[i].lineno = 0;
      asComment[i].charno = 0;
      asComment[i].length = 0;
      asComment[i].putted = False;
   }
}

/* A kovetkezo miatt:
**    struct abc
**    {
**       DECLARE_DYN(xy)
**       int a;
**    }
** Atlepem a macrot, es megnezem, hogy mi kovetkezik utana.
** Ha ; : vagy LBRACE, akkor restore es return False.
** Egyebkent return True.
*/

extern int skip_member_macro( void )  /* 01.08.97 rigo */
{
   Save_d();
   int t;

   niveau++;

   if( token( 0 ) == SN_IDENTIFIER )
   {
      step( 1 );

      if( token( 0 ) == '(' )
      {
         int i = 1;
         step( 1 );

         while( i > 0 )
         {
            if( token( 0 ) == ')' )
            {
               i--;
            }
            else if( token( 0 ) == '(' )
            {
               i++;
            }
            else if( token( 0 ) == '0' )
            {
               break;
            }
            step( 1 );
         }

         t = token( 0 );

         if( t != ';' && t != ':' && t != LBRACE && t != '(' )
         {
            niveau--;
            return True;
         }
      }
   }

   niveau--;
   Restore_d();
   return False;
}

/************************ some little function ************************/

extern char *StringToText( sString_t sString )
{
   static char buffer[500];
   static char *text = buffer;
   static int leng = sizeof(buffer) - 1;

   /* Malloc only if that is really necessary ! */
   if( leng < sString.leng + 1 )
   {
      leng = ((( sString.leng + 1 ) / 100 ) + 1 ) * 100;
      if(text != buffer)
      {
         text = ckrealloc(text,leng);
      }
      else
      {
         text = ckalloc( leng );
      }
   }

   strncpy( text, sString.text, sString.leng );
   text[sString.leng] = 0;
   return text;
}

extern void Put_Macro( Token_t Token )
{
   char *pcBeg;
   char *pcEnd;
   char  cEnd;
   int leng;

   leng = Token->sString.leng;

   if( leng < 0 ) return;

   pcBeg = Token->sString.text + 0     ; 
   pcEnd = Token->sString.text + (leng);
   cEnd  = *pcEnd;

   *pcEnd = 0;

   Put_symbol( PAF_MACRO_DEF
             , 0
             , pcBeg
             , filename_g
             , Token->lineno_beg
             , Token->charno_beg
             , Token->lineno_end
             , Token->charno_end
             , (unsigned long) 0
/*           , (char *) 0           21.11.97 rigo */
             , Token->pcValue    /* 21.11.97 rigo */
             , (char *) 0
             , (char *) 0
             , (char *) 0
             , Token->lineno_beg
             , Token->charno_beg
             , Token->lineno_end
             , Token->charno_end
             );

   *pcEnd = cEnd;
}

extern void Put_Include( Token_t Token )
{
   char *pcBeg;
   char *pcEnd;
   char  cEnd;
   int leng;

   leng = Token->sString.leng;

   if( leng < 1 ) return;

   pcBeg = Token->sString.text           ; 
   pcEnd = Token->sString.text + (leng-1);

   if( *pcBeg == '"' )
   {
      pcBeg++;
      if( *pcEnd != '"' )
         pcEnd++;
   }
   else  /* '<' */
   {
      pcBeg++;
      if( *pcEnd != '>' )
         pcEnd++;
   }

   cEnd  = *pcEnd;

   *pcEnd = 0;

   Put_symbol( PAF_INCLUDE_DEF
            , NULL
            , Paf_Search_Include_dir( pcBeg )
            , filename_g
            , Token->lineno_beg
            , Token->charno_beg +1
            , 0
            , 0
            , (unsigned long) 0
            , (char *) 0
            , (char *) 0
            , (char *) 0
            , (char *) 0
            , Token->lineno_beg
            , Token->charno_beg +1
            , 0
            , 0
            );

   *pcEnd = cEnd;
}

#ifdef FUNCTION_MACRO
extern void step( int i )
{
   iva += i;
}

extern int token( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   if( keyw_cpp )
      return v[(iva+i)&ZZMASK].yytoken_cpp;
   else
      return v[(iva+i)&ZZMASK].yytoken;
}

extern sString_t ident( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yytext;
}

extern int identleng( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yytext.leng;
}

extern int f_lineno( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yylineno;
}

extern int f_charno( int i )
{
   if( ivt <= iva+i )
   {
      f_read( i );
   }

   return v[(iva+i)&ZZMASK].yycharno;
}
#endif

extern char *identstring( int i )
{
   sString_t sString;
   static char ac[1000];

   sString = ident( i );

   strncpy( ac, sString.text, sString.leng );
   ac[sString.leng] = 0;

   return ac;
}

static Token_t TokenAll;
static Token_t TokenAct;
static Token_t TokenEnd;

extern void f_read_end( void )
{
   if( TokenAll )
   {
      f_TokenFreeAll( TokenAll );
   }

   TokenAll = 0;  /* 12.11.97 rigo */
   TokenAct = 0;  /* 12.11.97 rigo */
   TokenEnd = 0;  /* 12.11.97 rigo */
}

extern void f_read( int i )
{
   register ZZSTYPE *pvt;

   while( ivt <= iva + i )
   {
      if( TokenAll == 0 )
      {
         TokenAll = yylex();
         TokenAct = TokenAll;
         TokenEnd = TokenAll->TokenPrev;
      }
      else
      {
         if( TokenAct == TokenEnd )
         {
            f_TokenFreeAll( TokenAll );
            TokenAll = yylex();
            TokenAct = TokenAll;
            TokenEnd = TokenAll->TokenPrev;
         }
         else
         {
            TokenAct = TokenAct->TokenNext;
         }
      }

#ifdef VERSION_COMPARE
      /* minden #if agba belemegy ( meg az #if 0 -ba is ) */
      TokenAct->mode = CPLEX_UNDEF;
#endif

/*    f_TokenOnePrint( TokenAct ); */

      switch( TokenAct->lex )
      {
      case SN_PP_IF:
         depth++;
#ifdef PP_TEST
printf( "#if niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         sIfstack[depth].niveau = niveau;
         sIfstack[depth].status_max = False;
         IfstackUndef( depth ) = False;
         IfstackElse( depth ) = False;

         if( TokenAct->mode == CPLEX_UNDEF )
            IfstackUndef( depth ) = True;

         if( IfstackUndef( depth ))
         {
            SetStatus( depth, True );
         }
         else
         {
            if( TokenAct->mode == FALSE )
               SetStatus( depth, False );
            else
               SetStatus( depth, True );
         }
         HighLightBeg();
         continue;
         break;

      case SN_PP_ELSE:
#ifdef PP_TEST
printf( "#else niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         if( depth > 0 )
         {
            IfstackElse( depth ) = True;
            if( IfstackUndef( depth ))
            {
               if( niveau != sIfstack[depth].niveau )
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
            else
            {
               if( IfstackStatusMax( depth ))
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
         }
         HighLight();
         continue;
         break;

      case SN_PP_ELIF:
#ifdef PP_TEST
printf( "#elif niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         if( depth > 0 )
         {
            IfstackElse( depth ) = True;
            if( TokenAct->mode == CPLEX_UNDEF )
               IfstackUndef( depth ) = True;

            if( IfstackUndef( depth ))
            {
               if( niveau != sIfstack[depth].niveau )
                  SetStatus( depth, False );
               else
                  SetStatus( depth, True );
            }
            else
            {
               if( IfstackStatusMax( depth ))
               {
                  SetStatus( depth, False );
               }
               else
               {
                  if( TokenAct->mode == FALSE )
                     SetStatus( depth, False );
                  else
                     SetStatus( depth, True );
               }
            }
         }
         HighLight();
         continue;
         break;

      case SN_PP_ENDIF:
#ifdef PP_TEST
printf( "#endif niveau: %d depth: %d line: %d\n"
      , niveau
      , depth
      , TokenAct->lineno_beg
      );
#endif
         HighLightEnd();
         depth--;
         if( depth < 0 )
         {
            depth = 0;
         }
         continue;
         break;

      case SN_PP_DEFINE:
         if( GetStatus( depth ))
         {
            Put_Macro( TokenAct );
         }
         continue;
         break;

      case SN_PP_INCLUDE:
         if( GetStatus( depth ))
         {
            Put_Include( TokenAct );
         }
         continue;
         break;

      case 0:
         goto process;

      default:
         if( ! GetStatus( depth ))
         {
            continue;
         }

process:
         pvt = v + ( ivt&ZZMASK );

         if( TokenAct->lex == SN_IDENTIFIER && TokenAct->keyword > 0 )
         {
            if( TokenAct->is_cpp )
            {
               pvt->yytoken     = TokenAct->lex;
               pvt->yytoken_cpp = TokenAct->keyword;
            }
            else
            {
               pvt->yytoken     = TokenAct->keyword;
               pvt->yytoken_cpp = TokenAct->keyword;
            }
         }
         else
         {
            pvt->yytoken     = TokenAct->lex;
            pvt->yytoken_cpp = TokenAct->lex;
         }
         pvt->yytext   = TokenAct->sString;
         pvt->yylineno = TokenAct->lineno_beg;
         pvt->yycharno = TokenAct->charno_beg;
         ivt++;

#ifdef TRACE
         printf( "token: %*.*s (%d/%d)\n"
               , pvt->yytext.leng
               , pvt->yytext.leng
               , pvt->yytext.text
               , pvt->yytoken
               , pvt->yytoken_cpp
               );
#endif
         break;
      }
   }
}

extern void SetStatus( int depth, int value )
{
   if( sIfstack[depth-1].status )
   {
      sIfstack[depth].status = value;
   }
   else
   {
      sIfstack[depth].status = False;
   }

   if( sIfstack[depth].status )
   {
      sIfstack[depth].status_max = True;
   }
}

static int highlight_lineno_beg;
static int highlight_charno_beg;
static int highlight;

extern void HighLightBeg( void )
{
   if( sIfstack[depth-1].status )
   {
      if( ! sIfstack[depth].status )
      {
         highlight = True;
         highlight_lineno_beg = TokenAct->lineno_beg;
         highlight_charno_beg = 0;
      }
      else
      {
         highlight = False;
      }
   }
}

extern void HighLight( void )
{
   if( sIfstack[depth-1].status )
   {
      HighLightEnd();
      HighLightBeg();
   }
}

extern void HighLightEnd( void )
{
   if( sIfstack[depth-1].status )
   {
      if( highlight )
      {
#ifdef PRINT_REM
         printf( "%d rem %d.%d %d.%d\n"
                , PAF_HIGH
                , highlight_lineno_beg
                , highlight_charno_beg
                , TokenAct->lineno_beg
                , 0
                );
#endif
         if( hig_fp )
         {
            fprintf( hig_fp
                   ,"%d rem %d.%d %d.%d\n"
                   , PAF_HIGH
                   , highlight_lineno_beg
                   , highlight_charno_beg
                   , TokenAct->lineno_beg
                   , 0
                   );
         }
      }
   }
}


