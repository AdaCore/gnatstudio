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

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <tcl.h>

#ifdef __MSVC__
#include <io.h>
#endif

#include "limits.h"
#ifndef WIN32
#include <unistd.h>
#include <sys/param.h>
#endif /* WIN32 */

#include <tcl.h>
#include "cpdefines.h"
#include "cplex.h"
#include "cpkeyw.h"
#include "sn.h"
#include "longstr.h"

/* Valid debugging macros for this file:
         #define TRACE
*/

#define my_isxdigit(x) (isxdigit(x))
#define my_isdigit(x) (isdigit(x))
#define my_isalnum(x) (isalnum(x))

extern Tcl_Encoding encoding;
static int yyeof;

#define TABSIZE(pos)		(9 - ((pos) & 7))

# define input() ( yyleng++, \
                   _prev_yycharno = yycharno, \
                   (*yyptr == '\t') ? yycharno += TABSIZE(yycharno) : yycharno++, \
                   *yyptr ? (yychar = *yyptr++) : (yyeof++, yychar = '\0' ) \
                 )

# define unput() ( yyleng--, \
                   yycharno = _prev_yycharno, \
                   *yyptr ? --yyptr : ( yyeof ? (--yyeof, yyptr) : --yyptr ) \
                 )

#ifdef rigo

# define input() (yyleng++,(yychar=yygetc())=='\n'?(yylineno++,(yycharnoprev=yycharno),(yycharno=0),yychar):(yycharno++,yychar))

# define unput() (((yyleng--,(*--yyptr))=='\n')?(yylineno--,(yycharno=yycharnoprev)):yycharno--)

# define yygetc() ((yyptr[0]=='\r' && yyptr[1]!='\n') ? *yyptr++ = '\n' : *yyptr++ )
#endif

# define d_TokenReturn(x) \
   { \
      Token_t Token = f_TokenCreate(); \
      Token->lex          = x; \
      Token->sString.text = yytext; \
      Token->sString.leng = yyleng; \
      Token->lineno_beg   = yylineno_beg; \
      Token->charno_beg   = yycharno_beg; \
      Token->lineno_end   = yylineno; \
      Token->charno_end   = yycharno; \
      token_in_line_count++; \
      return Token; \
   }

#define LEX_MODE_NORMAL  0
#define LEX_MODE_PP      1
#define LEX_MODE_INCLUDE 2

extern FILE *hig_fp;
static int lex_mode;

static int token_in_line_count;
int yylineno;
int yycharno, _prev_yycharno;
static Tcl_DString yybuf;
static unsigned char *yyptr;

static Token_t TokenUnput;

extern int f_ReadFile( int fd );
extern Token_t f_TokenInput( void );
extern void f_TokenUnput( Token_t Token );
extern Param_t f_ParamCreate( void );
extern Token_t f_ParamToken( int *pbContinue );
extern void f_ParamFree( Param_t Param );
extern Token_t f_ParameterPosition( Param_t Param, int position );
extern int f_IsMacro( Token_t Token, Macro_t *pMacro );
extern Token_t f_TokenMacroInput( Token_t Token, int *pmacro );
extern Token_t f_TokenSpecMacroInput( int *pmacro );
extern Token_t f_Preprocessor( void );
extern void f_FileTokenPrint( Token_t Token );

extern Token_t yylex( void )
{
   int macro;
   Token_t Token;

   while( True )
   {
      Token = f_TokenInput();
      if( Token->lex == SN_IDENTIFIER )
      {
         Token = f_TokenMacroInput( Token, &macro );
         if( Token == 0 )   /* 24.11.97 rigo */
         {
            continue;
         }
      }
      else
      {
         macro = False;
      }
#ifdef TRACE
      f_FileTokenPrint( Token );
#endif
      return Token;
   }
}

extern int
f_ReadFile (int fd)
{
  static Tcl_Encoding ascii = NULL;
  unsigned char buf[8192];
  struct stat filestats;
  static int first = 1;
  int i =0, nbytes;
  Tcl_DString tmpBuf;
  
  if( fstat( fd, &filestats ))
    {
      perror( "fstat error" );
      return 1;
    }
  
  if (first) 
    {
      Tcl_DStringInit(&yybuf);
      first = 0;
    }

  if (ascii == NULL && encoding != NULL)
    {
      /* We never free this, but this lives for the life of the program
	 anyway. */
      if ((ascii = Tcl_GetEncoding(NULL, "ascii")) == NULL)
	{
	  printf("Could not locate ASCII Tcl encoding\n");
	  return 3;
	}
    }
  
  Tcl_DStringInit(&tmpBuf);
  Tcl_DStringFree(&yybuf);
  
  while ((nbytes = read(fd, buf, sizeof(buf))) > 0)
    {
      /* Only do this translation if the `-e' option is given. */
      if (encoding != NULL)
	{
	  Tcl_DString intermediate;
	  
	  Tcl_DStringInit(&intermediate);
	  
	  /* Translate to Tcl intermediate form. */
	  Tcl_ExternalToUtfDString(encoding, buf, nbytes, &intermediate);
	  
	  /* Translate to ASCII. */
	  Tcl_UtfToExternalDString(ascii,
				   Tcl_DStringValue(&intermediate),
				   Tcl_DStringLength(&intermediate),
				   &tmpBuf);
  	  Tcl_DStringAppend(&yybuf, Tcl_DStringValue(&tmpBuf), Tcl_DStringLength(&tmpBuf));
   	  Tcl_DStringFree(&tmpBuf);
   	  Tcl_DStringFree(&intermediate);
	} 
      else
	{
  	  Tcl_DStringAppend(&yybuf, buf, nbytes);
	}
    }
  
  if (nbytes < 0)
    {
      perror("read");
      return 3;
    }
  
  yyptr = Tcl_DStringValue(&yybuf);
  
  yyeof = 0;
  yylineno = 1;
  yycharno = 1;
  token_in_line_count = 0;
  lex_mode = LEX_MODE_NORMAL;
  
  return 0;
}

extern Token_t f_TokenInput( void )
{
   register unsigned char *yytext;
   register int yyleng;
   register int yychar;
   int yycharprev;
   int yylineno_beg;
   int yycharno_beg;

   if( TokenUnput )
   {
      Token_t Token = TokenUnput;
      TokenUnput = 0;
      return Token;
   }

   while( 1 )
   {
      yytext = yyptr;
      yyleng = 0;
      yylineno_beg = yylineno;
      yycharno_beg = yycharno;
      input();

/* printf( "yychar: %d\n", yychar ); */
      switch( yychar )
      {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
         if( yychar == '0' && ( input() == 'X' || yychar == 'x' ))
         {   /* hexa nummer */
            while( my_isxdigit( input()))
            {
            }
            if( yychar == 'l' || yychar == 'L' )
            {
               d_TokenReturn( SN_LONGconstant )
            }
            else if( yychar == 'u' || yychar == 'U' )
            {
               d_TokenReturn( SN_INTEGERconstant )
            }
            else
            {
               unput();
               d_TokenReturn( SN_INTEGERconstant )
            }
         }
         else
         {
            int bFloat = False;
            unput();

            while( my_isdigit( input()))
            {
            }
            if( yychar == '.' )
            {
               bFloat = True;
               while( my_isdigit( input()))
               {
               }
            }
            if( yychar == 'E' || yychar == 'e' )
            {
               bFloat = True;
               input();
               if( yychar == '+' || yychar == '-' )
               {
               }
               else
               {
                  unput();
               }
               while( my_isdigit( input()))
               {
               }
               if( yychar == '.' )
               {
                  while( my_isdigit( input()))
                  {
                  }
               }
            }
            if( yychar == 'l' || yychar == 'L' )
            {
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_LONGconstant )
            }
            else if( yychar == 'u' || yychar == 'U' )
            {
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_INTEGERconstant )
            }
            else
            {
               unput();
               if( bFloat ) d_TokenReturn( SN_FLOATINGconstant )
               else         d_TokenReturn( SN_INTEGERconstant )
            }
         }

      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'u':
      case 'x':
      case 'y':
      case 'v':
      case 'w':
      case 'z':

      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'R':
      case 'S':
      case 'T':
      case 'U':
      case 'X':
      case 'Y':
      case 'V':
      case 'W':
      case 'Z':

      case '_':
      case '$':

         {
            register int ihash = 0;
            register int j = 0;
            int keyword;
            int is_cpp;

            token_in_line_count++;

            do
            {
               if( j++ < 6 && ihash < HASH_MAX )
               {
                  ihash += c_hash[ (int) yychar ];
               }
            }
            while( my_isalnum( input()) || yychar == '_' || yychar == '$' );

            unput();

            if( ihash < HASH_MAX &&
                LexKeyWordTab[ihash].leng == yyleng &&
                strncmp( LexKeyWordTab[ihash].pcName, (char *)yytext, yyleng ) == 0 )
            {
               Token_t Token = f_TokenCreate();

               if( hig_fp )
               {
                  fprintf( hig_fp
                         ,"%d key %d.%d %d.%d\n"
                         , PAF_HIGH
                         , yylineno_beg
                         , yycharno_beg
                         , yylineno
                         , yycharno
                         );
               }

               keyword = LexKeyWordTab[ihash].wLexId;
               is_cpp  = LexKeyWordTab[ihash].is_cpp;

               Token->lex          = SN_IDENTIFIER;
               Token->keyword      = keyword;
               Token->is_cpp       = is_cpp;
               Token->sString.text = yytext;
               Token->sString.leng = yyleng;
               Token->lineno_beg   = yylineno_beg;
               Token->charno_beg   = yycharno_beg;
               Token->lineno_end   = yylineno;
               Token->charno_end   = yycharno;
               return Token;
            }

#ifdef rigo
            if( Paf_IsSkip && (*Paf_IsSkip)((char *)yytext, yyleng ))
            {
               if( hig_fp )
               {
                  fprintf( hig_fp
                         ,"%d rem %d.%d %d.%d\n"
                         , PAF_HIGH
                         , yylineno_beg
                         , yycharno_beg
                         , yylineno
                         , yycharno
                         );
               }
               break;   /* skipping identifier */
            }
            else
#endif
            {
               Token_t Token = f_TokenCreate();

               keyword = 0;
               is_cpp  = 0;

               Token->lex          = SN_IDENTIFIER;
               Token->keyword      = keyword;
               Token->is_cpp       = is_cpp;
               Token->sString.text = yytext;
               Token->sString.leng = yyleng;
               Token->lineno_beg   = yylineno_beg;
               Token->charno_beg   = yycharno_beg;
               Token->lineno_end   = yylineno;
               Token->charno_end   = yycharno;
               return Token;
            }
         }
         break;

      case ' ':
      case '\t':
         break;
      case '\r':
         input();
         if( yychar != '\n' )
         {
            unput();
         }
         goto newline;
         break;

      case '\n':
newline:
         yylineno++;
         yycharno = 1;
         if( lex_mode == LEX_MODE_PP )
         {
            token_in_line_count = -1;
            d_TokenReturn( SN_NEWLINE )
         }
         else
         {
            token_in_line_count = 0;
         }
         break;

      case '\\':  /* folytatosor */
         input();
         if( yychar == '\r' )
         {
            input();
            if( yychar != '\n' )
            {
               unput();
            }
            yylineno++;
            yycharno = 1;
         }
         else if( yychar == '\n' )
         {
            yylineno++;
            yycharno = 1;
         }
         break;

      case '"':
         while( input())
         {
            if( yychar == '\\' )
            {
/* A \ utani karaktert lenyeljuk.
   Ha \ utan cr+lf jon, akkor midkettot lenyeljuk */
               input();
               if( yychar == '\r' )
               {
                  input();
                  if( yychar != '\n' )
                  {
                     unput();
                  }
                  yylineno++;
                  yycharno = 1;
               }
               else if( yychar == '\n' )
               {
                  yylineno++;
                  yycharno = 1;
               }
            }
            else if( yychar == '"' )
            {
               break;
            }
/* 08.12.97 rigo: nem feladatunk az ellenorzes, viszont szamolnunk kell a sorokat
 *          else if( yychar == '\n' )
 *          {
 *             unput();
 *             break;
 *          }
 */
            else if( yychar == '\r' )
            {
               input();
               if( yychar != '\n' )
               {
                  unput();
               }
               yylineno++;
               yycharno = 1;
            }
            else if( yychar == '\n' )
            {
               yylineno++;
               yycharno = 1;
            }
         }
         d_TokenReturn( SN_STRINGliteral )

      case '\'':
         while( input())
         {
            if( yychar == '\\' )
            {
/* A \ utani karaktert lenyeljuk.
   Ha \ utan cr+lf jon, akkor midkettot lenyeljuk */
               input();
               if( yychar == '\r' )
               {
                  input();
                  if( yychar != '\n' )
                  {
                     unput();
                  }
                  yylineno++;
                  yycharno = 1;
               }
               else if( yychar == '\n' )
               {
                  yylineno++;
                  yycharno = 1;
               }
            }
            else if( yychar == '\'' )
            {
               break;
            }
            else if( yychar == '\n' )
            {
               unput();
               break;
            }
         }
         d_TokenReturn( SN_CHARACTERconstant )

      case '#':
/* printf( "# %d\n", token_in_line_count ); */
         /* vizsgaljuk, hogy ebben a sorban nem volt-e mar mas Token */
         if( token_in_line_count == 0 )
         {
            Token_t Token;
            if(( Token = f_Preprocessor()))
            {
               return Token;
            }
         }
         break;

      case '<':   /* <, <<, <=, <<= */
         if( lex_mode == LEX_MODE_INCLUDE )
         {
            while( input())
            {
               if( yychar == '\\' )
               {
/* A \ utani karaktert lenyeljuk.
   Ha \ utan cr+lf jon, akkor midkettot lenyeljuk */
                  input();
                  if( yychar == '\r' )
                  {
                     input();
                     if( yychar != '\n' )
                     {
                        unput();
                     }
                     yylineno++;
                     yycharno = 1;
                  }
                  else if( yychar == '\n' )
                  {
                     yylineno++;
                     yycharno = 1;
                  }
               }
               else if( yychar == '>' )
               {
                  break;
               }
               else if( yychar == '\n' )
               {
                  unput();
                  break;
               }
            }
            d_TokenReturn( SN_INCLUDEliteral )
         }
         else
         {
            switch( input())
            {
            case '<':
               switch( input())
               {
               case '=':
                  d_TokenReturn( SN_LSassign )
               default:
                  unput();
                  d_TokenReturn( SN_LS )
               }

            case '=':
               d_TokenReturn( SN_LE )
            default:
               unput();
               d_TokenReturn( '<' )
            }
         }

      case '>':   /* >, >>, >=, >>= */
         switch( input())
         {
         case '>':
            switch( input())
            {
            case '=':
               d_TokenReturn( SN_RSassign )
            default:
               unput();
               d_TokenReturn( SN_RS )
            }

         case '=':
            d_TokenReturn( SN_GE )

         default:
            unput();
            d_TokenReturn( '>' )
         }

      case '=':   /* =, == */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_EQ )

         default:
            unput();
            d_TokenReturn( '=' )
         }

      case '!':   /* !, != */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_NE )

         default:
            unput();
            d_TokenReturn( '!' )
         }

      case '+':   /* +, ++, += */
         switch( input())
         {
         case '+':
            d_TokenReturn( SN_ICR )

         case '=':
            d_TokenReturn( SN_PLUSassign )

         default:
            unput();
            d_TokenReturn( '+' )
         }

      case '-':   /* -, --, -=, ->, ->* */
         switch( input())
         {
         case '>':
            switch( input())
            {
            case '*':
               d_TokenReturn( SN_ARROWstar )
            default:
               unput();
               d_TokenReturn( SN_ARROW )
            }

         case '-':
            d_TokenReturn( SN_DECR )

         case '=':
            d_TokenReturn( SN_MINUSassign )

         default:
            unput();
            d_TokenReturn( '-' )
         }

      case '*':   /* *, *= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_MULTassign )

         default:
            unput();
            d_TokenReturn( '*' )
         }

      case '/':   /* /, /=, / *, // */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_DIVassign )

         case '*':   /* comment */
            yycharprev = 0;
            while( input())
            {
               if( yychar == '/' && yycharprev == '*' )
               {
                  break;
               }
               if( yychar == '\r' )
               {
                  input();
                  if( yychar != '\n' )
                  {
                     unput();
                  }
                  yylineno++;
                  yycharno = 1;
               }
               else if( yychar == '\n' )
               {
                  yylineno++;
                  yycharno = 1;
               }
               yycharprev = yychar;
            }
#ifdef PRINT_REM
            printf( "%d rem %d.%d %d.%d\n"
                   , PAF_HIGH
                   , yylineno_beg
                   , yycharno_beg
                   , yylineno
                   , yycharno
                   );
#endif
            if( hig_fp )
            {
               fprintf( hig_fp
                      ,"%d rem %d.%d %d.%d\n"
                      , PAF_HIGH
                      , yylineno_beg
                      , yycharno_beg
                      , yylineno
                      , yycharno
                      );
            }
            save_comment( yylineno_beg, yycharno_beg + 2, yytext + 2, yyleng - 4 );
            break;

         case '/':   /* comment */
            while( input())
            {
               if( yychar == '\n' )
               {
                  yylineno++;
                  yycharno = 1;
                  if( lex_mode == LEX_MODE_PP )
                  {
                     token_in_line_count = -1;
                     d_TokenReturn( SN_NEWLINE )
                  }
                  else
                  {
                     token_in_line_count = 0;
                  }
                  break;
               }
            }
#ifdef PRINT_REM
            printf( "%d rem %d.%d %d.%d\n"
                   , PAF_HIGH
                   , yylineno_beg
                   , yycharno_beg
                   , yylineno
                   , yycharno
                   );
#endif
            if( hig_fp )
            {
               fprintf( hig_fp
                      ,"%d rem %d.%d %d.%d\n"
                      , PAF_HIGH
                      , yylineno_beg
                      , yycharno_beg
                      , yylineno
                      , yycharno
                      );
            }
            save_comment( yylineno_beg, yycharno_beg + 2, yytext + 2, yyleng - 3 );
            break;

         default:
            unput();
            d_TokenReturn( '/' )
         }
         break;

      case '|':   /* |, ||, |= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ORassign )

         case '|':
            d_TokenReturn( SN_OROR )

         default:
            unput();
            d_TokenReturn( '|' )
         }

      case '^':   /* ^, ^= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ERassign )

         default:
            unput();
            d_TokenReturn( '^' )
         }

      case '%':   /* %, %= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_MODassign )

         default:
            unput();
            d_TokenReturn( '%' )
         }

      case '&':   /* &, &&, &= */
         switch( input())
         {
         case '=':
            d_TokenReturn( SN_ANDassign )

         case '&':
            d_TokenReturn( SN_ANDAND )

         default:
            unput();
            d_TokenReturn( '&' )
         }

      case ':':   /* :, :: */
         switch( input())
         {
         case ':':
            d_TokenReturn( SN_CLCL )

         default:
            unput();
            d_TokenReturn( ':' )
         }

      case '.':   /* ., .*, ... */
         switch( input())
         {
         case '*':
            d_TokenReturn( SN_DOTstar )

         case '.':
            switch( input())
            {
            case '.':
               d_TokenReturn( SN_ELLIPSIS )
            default:
               unput();
               break;
            }
            unput();
            d_TokenReturn( '.' )

         case '0':
         case '1':
         case '2':
         case '3':
         case '4':
         case '5':
         case '6':
         case '7':
         case '8':
         case '9':
            while( my_isdigit( input()))
            {
            }
            if( yychar == 'E' || yychar == 'e' )
            {
               input();
               if( yychar == '+' || yychar == '-' )
               {
               }
               else
               {
                  unput();
               }
               while( my_isdigit( input()))
               {
               }
               if( yychar == '.' )
               {
                  while( my_isdigit( input()))
                  {
                  }
               }
            }
            if( !( yychar == 'l' || yychar == 'L' ))
            {
               unput();
            }
            d_TokenReturn( SN_FLOATINGconstant )

         default:
            unput();
            d_TokenReturn( '.' )
         }

      case '(':
      case ')':
      case '[':
      case ']':
      case '{':
      case '}':
      case ',':
      case ';':
      case '~':
      case '?':
      case '@':
         d_TokenReturn( yychar )

      case 0:
         yyleng = 0;
         d_TokenReturn( 0 )

      default:
/*          printf( "syntax error %d (%c)\n", yychar, yychar ); */
         break;
      }
   }
}

extern void f_TokenUnput( Token_t Token )
{
   if( TokenUnput )
   {
      printf( "fatal error by f_TokenUnput\n" );
      exit( -1 );
   }
   TokenUnput = Token;
}

extern Param_t f_ParamCreate( void )
{
   Param_t Param = (Param_t) ckalloc( sizeof( sParam_t ));
   Token_t Token;

   Param->iToken = 0;
   Param->pToken = (Token_t *) ckalloc( 100 * sizeof( Token_t ));

   while( True )
   {
      int bContinue;

      Token = f_ParamToken( &bContinue );
      Param->pToken[Param->iToken++] = Token;
      if( ! bContinue )
      {
         break;
      }
   }

   return Param;
}

extern Token_t f_ParamToken( int *pbContinue )
{
   Token_t TokenReturn = 0;
   Token_t Token;
   int p = 0;
   int lex;

   while( True )
   {
      int macro;

      Token = f_TokenInput();
/*    if( iMacroCount && Token->lex == SN_IDENTIFIER ) 21.11.97 rigo */
      if( Token->lex == SN_IDENTIFIER )
      {
         sString_t sString = Token->sString;

         Token = f_TokenMacroInput( Token, &macro );
         if( Token == 0 )   /* Atlepjuk a macro-t. 25.11.97 rigo */
         {
            printf( "Token Skip: %*.*s\n", sString.leng, sString.leng, sString.text );
            continue;
         }
      }
      else
      {
         macro = False;
      }

      if( ! macro )
      {
         lex = Token->lex;

         if( p == 0 )
         {
            if( lex == ',' )
            {
               *pbContinue = True;
               f_TokenFree( Token );
               return TokenReturn;
            }
            if( lex == ')' || lex == 0 )
            {
               *pbContinue = False;
               f_TokenFree( Token );
               return TokenReturn;
            }
         }
         /**/ if( lex == '(' ) p++;
         else if( lex == ')' ) p--;
      }

      f_TokenAppend( &TokenReturn, Token );
   }
}

extern void f_ParamFree( Param_t Param )
{
   int i;

   for( i = 0; i < Param->iToken; i++ )
   {
      Token_t Token = Param->pToken[i];
      f_TokenFreeAll( Token );
   }

   ckfree((char*) Param->pToken );
   ckfree((char*) Param );
}

extern Token_t f_ParameterPosition( Param_t Param, int position )
{
   if( position < Param->iToken )
   {
      return Param->pToken[position];
   }
   else
   {
      return 0;
   }
}

extern Token_t f_TokenMacroInput( Token_t Token, int *pmacro )
{
   Token_t TokenMerge;
   Token_t TokenNext;
   Macro_t Macro;
   Param_t Param;

   if(( Macro = f_MacroFind( Token->sString )))
   {
      *pmacro = True;

      if( Macro->typ == CPP_DEFINE )
      {
         TokenMerge = f_MacroMerge( Macro, Token, (Param_t) 0, True );
         f_TokenFree( Token );
         return TokenMerge;
      }
      else if( Macro->typ == CPP_MACRO )
      {
         TokenNext = f_TokenInput();
         if( TokenNext->lex == '(' )
         {
            f_TokenFree( TokenNext );
            Param = f_ParamCreate();
            TokenMerge = f_MacroMerge( Macro, Token, Param, True );
            f_TokenFree( Token );
            f_ParamFree( Param );
            return TokenMerge;
         }
         else
         {
            f_TokenUnput ( TokenNext );
            return Token;
         }
      }
      else  /* Macro->typ == CPP_UNDEF */
      {
         *pmacro = False;
         return Token;
      }
   }
   else
   {
      *pmacro = False;
      return Token;
   }
}

/* special f_TokenMacroInput() for #if and #elif, where after defined()
there is no macro substitute */
extern Token_t f_TokenSpecMacroInput( int *pmacro )
{
   Token_t TokenMerge;
   Token_t TokenNext;
   Macro_t Macro;
   Param_t Param;
   Token_t Token;

   Token = f_TokenInput();

   if( Token->lex == SN_IDENTIFIER )
   {
      if( f_StringCompare( Token->sString, "defined" ) == 0 )
      {
         int mode;
         Token_t TokenDefined;

         *pmacro = False;

         f_TokenFree( Token );
         Token = f_TokenInput();
         if( Token->lex == '(' )
         {
            f_TokenFree( Token );
            Token = f_TokenInput();
            mode = f_IsDefined( Token->sString );
            TokenDefined = Token;
            Token = f_TokenInput();
            f_TokenFree( Token );
         }
         else
         {
            mode = f_IsDefined( Token->sString );
            TokenDefined = Token;
         }
         if( mode == CPLEX_UNDEF )
         {
            TokenDefined->lex = SN_UNDEF;
         }
         else if( mode == CPLEX_TRUE )
         {
            TokenDefined->lex = SN_TRUE;
         }
         else  /* mode == CPLEX_FALSE */
         {
            TokenDefined->lex = SN_FALSE;
         }
         return TokenDefined;
      }
      else if(( Macro = f_MacroFind( Token->sString )))
      {
         *pmacro = True;

         if( Macro->typ == CPP_DEFINE )
         {
            TokenMerge = f_MacroMerge( Macro, Token, (Param_t) 0, True );
            f_TokenFree( Token );
            return TokenMerge;
         }
         else if( Macro->typ == CPP_MACRO )
         {
            TokenNext = f_TokenInput();
            if( TokenNext->lex == '(' )
            {
               f_TokenFree( TokenNext );
               Param = f_ParamCreate();
               TokenMerge = f_MacroMerge( Macro, Token, Param, True );
               f_TokenFree( Token );
               f_ParamFree( Param );
               return TokenMerge;
            }
            else
            {
               f_TokenUnput ( TokenNext );
               return Token;
            }
         }
         else /* Macro->typ == CPP_UNDEF */
         {
            *pmacro = False;
            return Token;
         }
      }
      else
      {
         *pmacro = False;
         return Token;
      }
   }
   else
   {
      *pmacro = False;
      return Token;
   }
}


extern Token_t __Preprocessor( void );
extern Token_t f_TokenReadToNewline( void );
extern void f_TokenSkipToNewline( void );
extern char *f_TokenSkipToNewlineByDefine( void );   /* 21.11.97 rigo */

extern Token_t f_Preprocessor( void )
{
   Token_t Token;
   int lex_mode_save = lex_mode;

   lex_mode = LEX_MODE_PP;
   Token = __Preprocessor();
   lex_mode = lex_mode_save;

   return Token;
}

extern Token_t __Preprocessor( void )
{
   Token_t Token;
   int mode;

   Token = f_TokenInput();

   if( f_StringCompare( Token->sString, "define" ) == 0 )
   {
      f_TokenFree( Token );
      Token = f_TokenInput();
      if( Token->lex == SN_IDENTIFIER )
      {
         Token->lex = SN_PP_DEFINE;
         Token->pcValue = f_TokenSkipToNewlineByDefine();   /* 21.11.97 rigo */
         return Token;
      }
      else
      {
         f_TokenFree( Token );
         f_TokenSkipToNewline();
         return 0;
      }
   }
   else if( f_StringCompare( Token->sString, "include" ) == 0 )
   {
      int lex_mode_save = lex_mode;

      f_TokenFree( Token );
      lex_mode = LEX_MODE_INCLUDE;
      Token = f_TokenInput();
      lex_mode = lex_mode_save;
      f_TokenSkipToNewline();
      if( Token->lex == SN_STRINGliteral )
      {
         Token->lex = SN_PP_INCLUDE;
         return Token;
      }
      else if( Token->lex == SN_INCLUDEliteral )
      {
         Token->lex = SN_PP_INCLUDE;
         return Token;
      }
      else
      {
         f_TokenFree( Token );
         return 0;
      }
   }
   else if( f_StringCompare( Token->sString, "if" ) == 0 )
   {
      f_TokenFree( Token );
      Token = f_TokenReadToNewline();
      mode = __ConstantExpression( Token );
      f_TokenFreeAll( Token );
      Token = f_TokenCreate();
      Token->lex = SN_PP_IF;
      Token->mode = mode;
      return Token;
   }
   else if( f_StringCompare( Token->sString, "ifdef" ) == 0 )
   {
      f_TokenFree( Token );
      Token = f_TokenInput();
      f_TokenSkipToNewline();
      mode = f_IsDefined( Token->sString );
      f_TokenFree( Token );
      Token = f_TokenCreate();
      Token->lex = SN_PP_IF;
      Token->mode = mode;
      return Token;
   }
   else if( f_StringCompare( Token->sString, "ifndef" ) == 0 )
   {
      f_TokenFree( Token );
      Token = f_TokenInput();
      f_TokenSkipToNewline();
      mode = f_IsDefined( Token->sString );
      f_TokenFree( Token );
      Token = f_TokenCreate();
      Token->lex = SN_PP_IF;
      /**/ if( mode == CPLEX_TRUE  ) Token->mode = CPLEX_FALSE;
      else if( mode == CPLEX_FALSE ) Token->mode = CPLEX_TRUE ;
      else                           Token->mode = CPLEX_UNDEF;
      return Token;
   }
   else if( f_StringCompare( Token->sString, "else" ) == 0 )
   {
      f_TokenFree( Token );
      f_TokenSkipToNewline();
      Token = f_TokenCreate();
      Token->lex = SN_PP_ELSE;
      return Token;
   }
   else if( f_StringCompare( Token->sString, "elif" ) == 0 )
   {
      f_TokenFree( Token );
      Token = f_TokenReadToNewline();
      mode = __ConstantExpression( Token );
      f_TokenFreeAll( Token );
      Token = f_TokenCreate();
      Token->lex = SN_PP_ELIF;
      Token->mode = mode;
      return Token;
   }
   else if( f_StringCompare( Token->sString, "endif" ) == 0 )
   {
      f_TokenFree( Token );
      f_TokenSkipToNewline();
      Token = f_TokenCreate();
      Token->lex = SN_PP_ENDIF;
      return Token;
   }
   else if( Token->lex == SN_NEWLINE )
   {
      f_TokenFree( Token );
      return 0;
   }
   else
   {
      f_TokenFree( Token );
      f_TokenSkipToNewline();
      return 0;
   }
}

extern Token_t f_TokenReadToNewline( void )
{
   Token_t TokenReturn = 0;
   Token_t Token;

   while( True )
   {
      int macro;

      Token = f_TokenSpecMacroInput( &macro );  /* spec process defined() */
      if( Token == 0 )   /* 24.11.97 rigo */
      {
         continue;
      }
      f_TokenAppend( &TokenReturn, Token );

      if( ! macro && ( Token->lex == SN_NEWLINE || Token->lex == 0 ))
      {
         return TokenReturn;
      }
   }
}

extern void f_TokenSkipToNewline( void )
{
   Token_t Token;
   int lex;

   while( True )
   {
      Token = f_TokenInput();
      lex = Token->lex;
      f_TokenFree( Token );

      if( lex == SN_NEWLINE || lex == 0 )
      {
         return;
      }
   }
}

extern char *f_TokenSkipToNewlineByDefine( void ) /* 21.11.97 rigo */
{
   Token_t Token;
   LongString lstr;
   int lex_last;

   LongStringInit( &lstr, -1 );

   Token = f_TokenInput();
   if(( Token->lex == SN_IDENTIFIER && Token->keyword == 0 ) || Token->lex == SN_CLCL )
   {
      lstr.append( &lstr, Token->sString.text, Token->sString.leng );
      lex_last = Token->lex;
      f_TokenFree( Token );

      while( True )
      {
         Token = f_TokenInput();
         if((( Token->lex == SN_IDENTIFIER && Token->keyword == 0 ) || Token->lex == SN_CLCL ) &&
              Token->lex != lex_last )
         {
            lstr.append( &lstr, Token->sString.text, Token->sString.leng );
            lex_last = Token->lex;
            f_TokenFree( Token );
         }
         else
         {
            if( Token->lex != SN_NEWLINE && Token->lex != 0 )
            {
               f_TokenSkipToNewline();
            }
            f_TokenFree( Token );
            return lstr.buf;
         }
      }
   }
   else
   {
      if( Token->lex != SN_NEWLINE && Token->lex != 0 )
      {
         f_TokenSkipToNewline();
      }
      f_TokenFree( Token );
      return 0;
   }
}

extern int f_IsDefined( sString_t sString )
{
   Macro_t Macro;

/* if(( Macro = f_MacroFind( sString ))) 27.11.97 rigo */
   if(( Macro = f_MacroFind( sString )) && Macro->paf_typ != PAF_WORD_REPLACE )
   {
      if( Macro->typ == CPP_UNDEF )
         return CPLEX_FALSE;
      else
         return CPLEX_TRUE;
   }
   else
   {
      return CPLEX_UNDEF;
   }
}

extern int f_StringCompare( sString_t sString, char *pc )
{
   char ac[1000];

   strncpy( ac, sString.text, sString.leng );
   ac[sString.leng] = 0;

   return strcmp( ac, pc );
}

extern void f_FileTokenPrint( Token_t Token )
{
   static int lineno = 0;
   static int charno = 0;
   static FILE *pfile;
   static int bFirst = True;

   if( bFirst )
   {
      bFirst = False;
      pfile = fopen( "macro.trace", "w+" );
      if( pfile == 0 )
      {
          pfile = stdout;
          perror( "malloc.trace" );
      }
   }

   WHILE_TOKEN( Token )

      unsigned char *text = Token->sString.text;
      int            leng = Token->sString.leng;

      if( Token->lineno_beg > lineno )
      {
         while( Token->lineno_beg > lineno )
         {
            fprintf( pfile, "\n" );
            lineno++;
            charno = 0;
         }
      }

      if( Token->lex == SN_PP_IF )
      {
         fprintf( pfile, "#if %d", Token->mode );
      }
      else if( Token->lex == SN_PP_ELIF )
      {
         fprintf( pfile, "#elif %d", Token->mode );
      }
      else if( Token->lex == SN_PP_ELSE )
      {
         fprintf( pfile, "#else" );
      }
      else if( Token->lex == SN_PP_ENDIF )
      {
         fprintf( pfile, "#endif" );
      }
      else if( Token->lex == SN_PP_DEFINE )
      {
         fprintf( pfile, "#define %*.*s"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               );
      }
      else if( Token->lex == SN_PP_INCLUDE )
      {
         fprintf( pfile, "#include %*.*s"
               , Token->sString.leng
               , Token->sString.leng
               , Token->sString.text
               );
      }
      else
      {
         fprintf( pfile, " " ); charno++; /* 15.01.97 rigo */

         if( Token->lineno_beg == lineno && Token->charno_beg > charno )
         {
            while( Token->charno_beg > charno )
            {
               fprintf( pfile, " " );
               charno++;
            }
         }

         charno += leng;

         while( leng-- )
         {
            fputc( *text++, pfile );
         }
      }

   END_WHILE
}

void
free_lex_buffers()
{
  Tcl_DStringFree(&yybuf);
}

