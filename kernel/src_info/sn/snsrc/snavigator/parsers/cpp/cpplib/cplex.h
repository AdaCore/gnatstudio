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


#define CPP_DEFINE  1
#define CPP_MACRO   2
#define CPP_UNDEF   3

#define True (!False)
#define False 0

#define CPLEX_TRUE   2
#define CPLEX_FALSE  0
#define CPLEX_UNDEF  1

#define WHILE_TOKEN(Token) \
   { \
      Token_t __TokenEnd = Token; \
      Token_t __TokenNext; \
      for( \
         ; Token && ( __TokenNext = Token->TokenNext ) \
         ; ( Token = __TokenNext ) == __TokenEnd ? ( Token = 0 ) : 0 ) \
      {

#define WHILE_MACRO(Macro) \
   { \
      Macro_t __MacroEnd = Macro; \
      Macro_t __MacroNext; \
      for( \
         ; Macro && ( __MacroNext = Macro->MacroNext ) \
         ; ( Macro = Macro->MacroNext ) == __MacroEnd ? ( Macro = 0 ) : 0 ) \
      {

#define END_WHILE }}

typedef struct sToken sToken_t, *Token_t;
typedef struct sMacro sMacro_t, *Macro_t;
typedef struct sParam sParam_t, *Param_t;
typedef struct sString sString_t, *String_t;

struct sString
{
   unsigned char *text;
   int leng;
};

struct sToken
{
   int lex;             /* code in lex */
   int keyword;         /* by identifier: keyword  = 0 && lex = SN_IDENTIFIER
                           by keyword   : keyword != 0 && lex = SN_IDENTIFIER */ 
   sString_t sString;   /* text of lex */
   char *pcValue;       /* value of #define (lex == SN_PP_DEFINE) */
   unsigned int is_free   : 1;  /* is free or occupied */
   unsigned int is_cpp    : 1;  /* is keyword c++ or not c++ keyword */
   unsigned int mode      : 2;  /* if( lex == SN_PP_IF || lex == SN_PP_ELIF )
                                   mode == TRUE  ||
                                   mode == FALSE ||
                                   mode == UNDEF */
   int iParameter;      /* is identifier a parameter (>= 0) or not (== -1) */
   int lineno_beg;
   int charno_beg;
   int lineno_end;
   int charno_end;
   Token_t TokenNext;      /* locale ring next pointer */
   Token_t TokenPrev;      /* locale ring prev pointer */
   Token_t TokenFreeNext;  /* free Token next pointer */
#ifdef TOKEN_ALL_CHECK
   Token_t TokenAllNext;   /* all Token next pointer */
#endif
};

struct sParam
{
   int iToken;
   Token_t *pToken;
};

struct sMacro
{
   char *parameter_list;
   char *macro;
   int typ;
   int paf_typ;   /* 27.11.97 rigo */
   int tilt;
   sString_t sStringName;
   Token_t Token;
   Token_t TokenProcessed;
   Macro_t MacroNext;
   Macro_t MacroPrev;
};

extern int iMacroCount;

extern Token_t yylex( void );
extern Macro_t f_MacroFind( sString_t sString );
extern Token_t f_MacroMerge( Macro_t Macro, Token_t TokenBasic, Param_t Param, int bProcessed );
extern void f_MacroTokenPrint( Token_t Token );
extern void f_MacroAppend( Macro_t *pMacroBeg1, Macro_t MacroBeg2 );
extern void f_MacroPrintAll( void );
extern void f_MacroPrint( Macro_t Macro );
extern void f_MacroFreeAll( void );

extern void f_MacroProcess( Macro_t Macro );

extern Token_t f_TokenCreate( void );
extern Token_t f_TokenCreate2( void );
extern void f_TokenFree( Token_t Token );
extern void f_TokenFreeAll( Token_t Token );
extern void f_TokenAppend( Token_t *pTokenBeg1, Token_t TokenBeg2 );
extern void f_TokenDelete( Token_t Token );
extern Token_t f_TokenDuplicate( Token_t Token );
extern Token_t f_TokenDuplicate2( Token_t Token );
extern void f_TokenPrint( Token_t Token );
extern void f_TokenCheck( void );
extern int f_IsDefined( sString_t sString );
extern int f_StringCompare( sString_t sString, char *pc );
extern void save_comment( int lineno, int charno, char *text, int length );
extern int __ConstantExpression( Token_t Token );


