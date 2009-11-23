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

%{

/*
    fortran.y

    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it.  There is no warranty
    for this program.


	    This grammar is ANSI standard-conforming, except for:
		-- complex constant and a few other ambiguities needing
		   significant lookahead cannot be split across lines.

	    Extensions supported:
	        -- Case insensitive.
	 	-- Hollerith constants.
		-- Variable names may be longer than 6 characters.  Also
		   allows underscores and dollar signs in names.
		-- DO ... ENDDO and DO WHILE loop forms allowed.
		-- NAMELIST supported.
		-- TYPE and ACCEPT I/O statements allowed.
		-- Tabs are permitted in input, and (except in character data)
		   expand into blanks up to the next column equal to 1 mod 8.
		-- Type declarations INTEGER*2, REAL*8, etc. are allowed.
		-- IMPLICIT NONE allowed.
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#include "symtab.h"
#include <tcl.h>
#include "sn.h"

	/* The following section is for use with bison-derived
	   parser.  Define alloca to be ckalloc for those cases
	   not covered by the cases covered there.  The ifdefs
	   are those in the skeleton parser with includes removed */
#ifdef AIXC	/* IBM RS/6000 xlc compiler does it this way */
#pragma alloca
#endif
#ifndef alloca
#ifdef __GNUC__
#else /* Not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__)
#else /* Not sparc */
#ifdef MSDOS
#endif /* MSDOS */
#endif /* Not sparc.  */
#endif /* Not GNU C.  */
#define alloca ckalloc
#endif /* alloca now defined.  */

extern	int	highlight;
void exit();

#ifndef YYDEBUG	/* If not declared otherwise... */
int yydebug;	/* declare yydebug to satisfy extern in ftnchek.c */
#ifdef DEVELOPMENT
#define YYDEBUG 1		/* For development it is handy */
#else
#define YYDEBUG 0
#endif
#endif

#ifdef DEVELOPMENT
#define DEBUG_PARSER
#endif

int current_datatype,	/* set when parse type_name or type_stmt */
    stmt_sequence_no,   /* set when parsing, reset to 0 at end_stmt */
    control_item_count;	/* count of items in control_info_list */
long current_typesize;	/* for type*len declarations */

extern unsigned prev_stmt_line_num; /* shared with advance */

unsigned true_prev_stmt_line_num;	/* shared with symtab.c */

int current_struct_hash = -1;
int current_common_hash = -1;
int current_record_hash = -1;

int current_module_hash = -1,	/* hashtable index of current module name */
    current_module_type,
    executable_stmt=FALSE,
    prev_stmt_class=0,
		 /* flags for lexer */
    complex_const_allowed=FALSE, /* for help in lookahead for these */
    in_assignment_stmt=FALSE,
    inside_format=FALSE,	/* when inside parens of FORMAT  */
    integer_context=FALSE;	/* says integers-only are to follow */

long exec_stmt_count=0;	/* count of executable stmts in program */
int	cross_scope_type = PAF_SUBR_DEF;

#ifdef DEBUG_PARSER
PRIVATE void
print_comlist(), print_exprlist();
#endif

PRIVATE void	END_processing();

PRIVATE Token *
  append_token();
#if 0
PRIVATE Token *
  append_dot_token();
#endif
PRIVATE int
  do_bounds_type();

static Token *token_dup( Token *t );

		/* Uses of Token fields for nonterminals: */
/*
  1. dim_bound_lists: dimensioning info for arrays:
       token.class = no. of dimensions,
       token.subclass = no. of elements
  2. expressions
       token.value.integer = hash index (of identifier)
       token.class = type_byte = storage_class << 4 + datatype
       token.subclass = flags: CONST_EXPR, LVALUE_EXPR, etc.
  3. common variable lists
       token.subclass = flag: COMMA_FLAG used to handle extra/missing commas
*/

enum {
	SEQ_HEADER = 1,
	SEQ_IMPLICIT,
	SEQ_SPECIF,
	SEQ_STMT_FUN,
	SEQ_EXEC,
	SEQ_END
};

%}

%token tok_identifier
%token tok_array_identifier
%token tok_label
%token tok_integer_const
%token tok_real_const
%token tok_dp_const
%token tok_complex_const
%token tok_dcomplex_const
%token tok_logical_const
%token tok_string
%token tok_hollerith
%token tok_edit_descriptor
%token tok_letter
%token tok_relop	/* .EQ. .NE. .LT. .LE. .GT. .GE. */
%token tok_AND
%token tok_OR
%token tok_EQV
%token tok_NEQV
%token tok_NOT
%token tok_power	/*   **   */
%token tok_concat	/*   //   */
%token tok_ACCEPT
%token tok_ASSIGN
%token tok_BACKSPACE
%token tok_BLOCK
%token tok_BLOCKDATA
%token tok_BYTE
%token tok_CALL
%token tok_CHARACTER
%token tok_CLOSE
%token tok_COMMON
%token tok_COMPLEX
%token tok_CONTINUE
%token tok_DATA
%token tok_DIMENSION
%token tok_DO
%token tok_DOUBLE
%token tok_DOUBLECOMPLEX
%token tok_DOUBLEPRECISION
%token tok_DOWHILE
%token tok_ELSE
%token tok_ELSEIF
%token tok_END
%token tok_ENDDO
%token tok_ENDFILE
%token tok_ENDIF
%token tok_ENDMAP
%token tok_ENDSTRUCTURE
%token tok_ENDUNION
%token tok_ENTRY
%token tok_EQUIVALENCE
%token tok_EXTERNAL
%token tok_FILE
%token tok_FORMAT
%token tok_FUNCTION
%token tok_GO
%token tok_GOTO
%token tok_IF
%token tok_IMPLICIT
%token tok_INCLUDE
%token tok_INQUIRE
%token tok_INTEGER
%token tok_INTRINSIC
%token tok_LOGICAL
%token tok_MAP
%token tok_NAMELIST
%token tok_NONE
%token tok_OPEN
%token tok_PARAMETER
%token tok_PAUSE
%token tok_POINTER
%token tok_PRECISION
%token tok_PRINT
%token tok_PROGRAM
%token tok_READ
%token tok_REAL
%token tok_RECORD
%token tok_RETURN
%token tok_REWIND
%token tok_SAVE
%token tok_STOP
%token tok_STRUCTURE
%token tok_SUBROUTINE
%token tok_UNION
%token tok_THEN
%token tok_TO
%token tok_TYPE
%token tok_WHILE
%token tok_WRITE

%token tok_illegal  /* Illegal token unused in grammar: induces syntax error */

%token EOS	127	/* Character for end of statement.  */

%nonassoc tok_relop

%left REDUCE ')'	/* Used at unit_io to force a reduction */


%%
	/*  The following grammar is based on the ANSI manual, diagrams
	 *  of section F.  Numbers in the comments refer to the diagram
	 *  corresponding to the grammar rule.
	 */


/* 1-5 */

prog_body	:	stmt_list
		|	/* empty file */
		;

stmt_list	:	stmt_list_item
		|	stmt_list stmt_list_item
		;


stmt_list_item	:	ordinary_stmt
			{
				/* Create id token for prog if unnamed. */
			  if(current_module_hash == -1) {
			    implied_id_token(&($1),unnamed_prog);
			    def_function(
				type_PROGRAM,size_DEFAULT,&($1),(Token*)NULL);
			    current_module_hash =
			      def_curr_module(&($1));
			    current_module_type = type_PROGRAM;
			  }

					/* Handle END statement */
			  if(curr_stmt_class == tok_END) {
			    if(prev_stmt_class != tok_RETURN)
			      do_RETURN(current_module_hash,&($1));
			    END_processing(&($$));
			  }
			  prev_stmt_class = curr_stmt_class;
			  integer_context = FALSE;
			  true_prev_stmt_line_num = $$.line_num;
			}
 		|	include_stmt
		|	EOS	/* "sticky" EOF for needed delay */
		;

			/* Statements: note that ordering by category
			   of statement is not enforced in the grammar
			   but is deferred to semantic processing.
			 */

ordinary_stmt	:	stmt
		|	end_stmt
		;

stmt		:	tok_label unlabeled_stmt
			{
#ifdef CHECK_LABELS
			  def_label(&($1));
#endif
			}
		|	unlabeled_stmt
		;

unlabeled_stmt	:	subprogram_header
			{
			    exec_stmt_count = 0;
			    executable_stmt = FALSE;
			}
		|	specification_stmt
			{
			    executable_stmt = FALSE;
			}
		|	executable_stmt
			{	/* handle statement functions correctly */
			  if(is_true(STMT_FUNCTION_EXPR, $1.subclass)
				     && stmt_sequence_no <= SEQ_STMT_FUN) {
			    stmt_sequence_no = SEQ_STMT_FUN;
			    executable_stmt = FALSE;
			  }
			  else {
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			  }
			}
		|	restricted_stmt
			{
			    stmt_sequence_no = SEQ_EXEC;
			    ++exec_stmt_count;
			    executable_stmt = TRUE;
			}
		|	error EOS
			{
			    executable_stmt = TRUE;
			    if(stmt_sequence_no == 0)
			      stmt_sequence_no = SEQ_HEADER;
			    complex_const_allowed = FALSE; /* turn off flags */
			    inside_format=FALSE;
			    integer_context = FALSE;
			    in_assignment_stmt = FALSE;
			    $$.line_num = prev_stmt_line_num; /* best guess */
			    yyerrok; /* (error message already given) */
			}
		;

subprogram_header:	prog_stmt
			{
			    current_module_type = type_PROGRAM;
			}
		|	function_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	subroutine_stmt
			{
			    current_module_type = type_SUBROUTINE;
			}
		|	block_data_stmt
			{
			    current_module_type = type_BLOCK_DATA;
			}
		;

end_stmt	:	unlabeled_end_stmt
		|	tok_label unlabeled_end_stmt
		;

unlabeled_end_stmt:	tok_END EOS
		;

include_stmt	:	tok_INCLUDE tok_string EOS
 			{
#ifdef ALLOW_INCLUDE
 			  open_include_file($2.value.string);
#endif
 			}
 		;

/* 5,6 */
		/* Note that stmt_function_stmt is not distinguished from
		   assignment_stmt, but assign (label to variable) is.
		   Also, format_stmt w/o label is accepted here.
		   ANSI standard for statement sequencing is enforced here. */
specification_stmt:	anywhere_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				stmt_sequence_no = SEQ_IMPLICIT;
			     }
			}
		|	parameter_stmt
			{
			     if(stmt_sequence_no < SEQ_IMPLICIT) {
				   stmt_sequence_no = SEQ_IMPLICIT;
			     }
			     else if(stmt_sequence_no > SEQ_SPECIF) {
			       check_stmt_sequence(&($1),SEQ_SPECIF);
			     }
			}
		|	implicit_stmt
			{
			  check_stmt_sequence(&($1),SEQ_IMPLICIT);
			}
		|	data_stmt
			{
			     if(stmt_sequence_no < SEQ_STMT_FUN) {
				stmt_sequence_no = SEQ_STMT_FUN;
		 	     }
			}
		|	specif_stmt
			{
			  check_stmt_sequence(&($1),SEQ_SPECIF);
			}
		;

anywhere_stmt	:	entry_stmt
		|	format_stmt
		;

specif_stmt	:	dimension_stmt
		|	equivalence_stmt
		|	common_stmt
		|	namelist_stmt
		|	type_stmt
		|	external_stmt
		|	intrinsic_stmt
		|	save_stmt
        |   struct_stmt
		;

struct_stmt :    tok_STRUCTURE '/' symbolic_name '/' EOS
        {
			if( highlight != -1 )
			{
				put_symbol(PAF_CLASS_DEF,NULL,
					hashtab[$3.value.integer].name,
					current_filename,
					$3.line_num,
					$3.curr_index,
					0,0,
					(long)0,NULL,NULL,NULL,
					get_comment(current_filename,$3.line_num),
					0,0,0,0);
			}
            current_struct_hash = $3.value.integer;
        }
            struct_list tok_ENDSTRUCTURE EOS
        {
            current_struct_hash = -1;
        }
        ;

struct_list     :       struct_item
                |       struct_list struct_item
                ;

struct_item     :       type_stmt
                ;


/* 7 */
executable_stmt:		/* Allowed in logical IF */
			transfer_stmt
		|	nontransfer_stmt
		;

transfer_stmt	:	unconditional_goto
		|	assigned_goto
		|	arithmetic_if_stmt
		|	stop_stmt
		|	return_stmt
		;

nontransfer_stmt:	assignment_stmt
		|	assign_stmt
		|	computed_goto	/* fallthru allowed */
		|	continue_stmt
		|	pause_stmt
		|	read_stmt
		|	accept_stmt
		|	write_stmt
		|	print_stmt
		|       type_output_stmt
		|	rewind_stmt
		|	backspace_stmt
		|	endfile_stmt
		|	open_stmt
		|	close_stmt
		|	inquire_stmt
		|	call_stmt
		;

restricted_stmt:		/* Disallowed in logical IF */
			restricted_nontransfer_stmt
		|	else_or_endif_stmt
		;

restricted_nontransfer_stmt:
			logical_if_stmt
		|	block_if_stmt
		|	do_stmt
		|	enddo_stmt
		;

else_or_endif_stmt:	else_if_stmt
		|	else_stmt
		|	end_if_stmt
		;

/* 8 */
prog_stmt	:	tok_PROGRAM {check_seq_header(&($1));}
				 symbolic_name EOS
			{
			     def_function(
				type_PROGRAM,size_DEFAULT,&($3),(Token*)NULL);
			     current_module_hash =
			       def_curr_module(&($3));
			}
		;

			/* Note that function & subroutine entry not
			 * distinguished in this grammar.
			 */
/* 9 */
entry_stmt	:	tok_ENTRY symbolic_name EOS
			{
			  do_ENTRY(&($2),(Token*)NULL
				   ,current_module_hash);
			}
		|	tok_ENTRY symbolic_name '(' dummy_argument_list ')' EOS
			{
			  do_ENTRY(&($2),&($4)
				   ,current_module_hash);
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("entry stmt",&($4));
#endif
			}
		;

/* 10 */

function_stmt
		:	typed_function_handle symbolic_name EOS
			{
			 def_function(
				current_datatype,current_typesize,
				      &($2),(Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	typed_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				current_datatype,current_typesize,
				      &($2),&($4));
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		|	plain_function_handle symbolic_name EOS
			{
			 def_function(
				type_UNDECL,size_DEFAULT,&($2),(Token*)NULL);
			 current_module_hash=
			   def_curr_module(&($2));
			}
		|	plain_function_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			 def_function(
				type_UNDECL,size_DEFAULT,&($2),&($4));
			 current_module_hash=
			   def_curr_module(&($2));
#ifdef DEBUG_PARSER
			 if(debug_parser)
			   print_exprlist("function stmt",&($4));
#endif
			}
		;

typed_function_handle:	type_name tok_FUNCTION
			{
			  check_seq_header(&($2));
			}
		;

plain_function_handle:	tok_FUNCTION
			{
			  check_seq_header(&($1));
			}
		;

type_name	:	arith_type_name
			{
				current_record_hash = -1;
			}
		|	plain_char_type_name
		|	char_type_name
		;


/* 11 not present: see 9 */

/* 12 */

subroutine_stmt
		:	subroutine_handle symbolic_name EOS
			{
			  def_function(
				 type_SUBROUTINE,size_DEFAULT,
				       &($2),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		|	subroutine_handle symbolic_name
				'(' dummy_argument_list ')' EOS
			{
			  def_function(
				 type_SUBROUTINE,size_DEFAULT,&($2),&($4));
			  current_module_hash=
			    def_curr_module(&($2));
#ifdef DEBUG_PARSER
			  if(debug_parser)
			    print_exprlist("subroutine stmt",&($4));
#endif
			}
		;

subroutine_handle:	tok_SUBROUTINE
			{
			  check_seq_header(&($1));
			}
		;

dummy_argument_list:	/* empty */
			{
			    $$.next_token = (Token*)NULL;
			}
		|	non_empty_arg_list
		;

non_empty_arg_list:	dummy_argument
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	non_empty_arg_list ',' dummy_argument
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

dummy_argument	:	symbolic_name
			{
			     def_arg_name(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	'*'
			{
			     $$.class = type_byte(class_LABEL,type_LABEL);
			     $$.subclass = 0;
			}
		;

/* 13 not present: see 9 */

/* 14 */
block_data_stmt	:	block_data_handle EOS
			{
				  /* form name %DATnn */
			  ++block_data_number;
			  sprintf(unnamed_block_data+4,"%02d"
				  ,block_data_number%100);
			  implied_id_token(&($$),unnamed_block_data);

			  def_function(
				 type_BLOCK_DATA,size_DEFAULT,
				       &($$),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($$));
			}
		|	block_data_handle symbolic_name EOS
			{
			  def_function(
				 type_BLOCK_DATA,size_DEFAULT,
				       &($2),(Token*)NULL);
			  current_module_hash=
			    def_curr_module(&($2));
			}
		;

block_data_handle:	tok_BLOCK tok_DATA
			{
			  check_seq_header(&($2));
			}
		|	tok_BLOCKDATA
			{
			  check_seq_header(&($1));
			}

		;
/* 15 */
dimension_stmt	:	tok_DIMENSION array_declarator_list EOS
		;

array_declarator_list:	array_declarator
		|	array_declarator_list ',' array_declarator
		;

/* 16 */
array_declarator:	symbolic_name '(' dim_bound_list ')'
			{
			     def_array_dim(&($1),&($3));
			}
		;

dim_bound_list	:	dim_bound_item      /* token class = no. of dimensions,
					       subclass = no. of elements */
			{
			     $$.class = 1;
			     $$.subclass = $1.subclass;
			}
		|	dim_bound_list ',' dim_bound_item
			{
			     $$.class = $1.class + 1; /* one more dimension */
			     $$.subclass = $1.subclass * $3.subclass;
			}
		;

dim_bound_item	:	dim_bound_expr
			{
			      if( datatype_of($1.class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.subclass) )
				$$.subclass = $1.value.integer;
			      else
				$$.subclass = 0;
			}
		|	dim_bound_expr ':' dim_bound_expr
			{	/* avoid getting 0 - 0 + 1 = 1 if bounds nonconstant */
			      if( datatype_of($1.class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$1.subclass)
				 && datatype_of($3.class) == type_INTEGER
				 && is_true(EVALUATED_EXPR,$3.subclass) )
				$$.subclass = $3.value.integer - $1.value.integer + 1;
			      else
				$$.subclass = 0;
			}
		|	'*'
			{
			     $$.subclass = 0;
			}
		|	dim_bound_expr ':' '*'
			{
			     $$.subclass = 0;
			}
		;

/* 17 */
equivalence_stmt:	tok_EQUIVALENCE {equivalence_flag = TRUE;}
			equivalence_list EOS {equivalence_flag = FALSE;}
		;

equivalence_list:	'(' equivalence_list_item ')'
		|	equivalence_list ',' '(' equivalence_list_item ')'
		;

equivalence_list_item:	equiv_entity ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		|	equivalence_list_item ',' equiv_entity
			{
			  equivalence(&($1), &($3));
			}
		;

/* 17 */
equiv_entity	:	symbolic_name
			{
			     def_equiv_name(&($1));
			}
		|	array_equiv_name
			{
			     def_equiv_name(&($1));
			}
		|	substring_equiv_name
			{
			     def_equiv_name(&($1));
			}
		;

array_equiv_name:	symbolic_name '(' subscript_list ')'
				/* should check */
		;

substring_equiv_name:	symbolic_name substring_interval
		|	array_equiv_name substring_interval
		;

/* 19 */
common_stmt	:	tok_COMMON blank_common_block EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$), &($2));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif

			}
		|	tok_COMMON common_block_list EOS
		|	tok_COMMON blank_common_block common_block_list EOS
			{
			     implied_id_token(&($$),blank_com_name);
			     def_com_block(&($$),&($2));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("blank common",&($2));
#endif
			}
		;

blank_common_block :
		{
		;
			if( highlight != -1 )
			{
				put_symbol(PAF_COMMON_DEF,NULL,blank_com_name,
					current_filename,
					line_num,
					curr_index,
					0,0,
					(long)0,NULL,NULL,NULL,
					get_comment(current_filename,line_num),
					0,0,0,0);
			}
		}
			common_variable_list
		{ $$ = $2; }
		;

	/*  The following defns allow trailing commas and missing commas in
	    order to tolerate the optional comma before /blockname/.  The
	    token subclass holds comma status to allow errors to be caught. */
common_block_list:	labeled_common_block
			{
			     $$.subclass = $1.subclass;
			}
		|	common_block_list labeled_common_block
			{
			     $$.subclass = $2.subclass;
			     $$.line_num = $2.line_num;
			     $$.col_num = $2.col_num;
			}
		;

labeled_common_block:	common_block_name
			{
				if( highlight != -1 )
				{
					put_symbol(PAF_COMMON_DEF,NULL,
						hashtab[$1.value.integer].name,
						current_filename,
						$1.line_num,
						$1.curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,$1.line_num),
						0,0,0,0);
				}
            	current_common_hash = $1.value.integer;
			}
						common_variable_list
			{
            	current_common_hash = -1;
			     def_com_block(&($1),&($3));
			     $$.subclass = $3.subclass;
			     $$.line_num = $3.line_num;
			     $$.col_num = $3.col_num;
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_comlist("labeled common",&($3));
#endif
			}
		;

common_block_name:	'/' symbolic_name '/'
			{
			     $$ = $2;
			}

		|	'/'  '/'		/* block with no name */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		|	tok_concat		/* "//" becomes this */
			{
			     implied_id_token(&($$),blank_com_name);
			}
		;

common_variable_list:	common_list_item
			{
			    $$.subclass = $1.subclass;
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	common_variable_list common_list_item
			{
			    $$.subclass = $2.subclass;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

common_list_item:	common_entity
			{			   /* no comma */
			     $$.subclass = $1.subclass;
			     make_false(COMMA_FLAG,$$.subclass);
			}
		|	common_entity ','
			{			   /* has comma */
			     $$.subclass = $1.subclass;
			     make_true(COMMA_FLAG,$$.subclass);
   			}
		;

common_entity	:	symbolic_name
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		|	array_declarator
			{
			     def_com_variable(&($1));
			     primary_id_expr(&($1),&($$));
			}
		;


/* NAMELIST : Not Standard
   Syntax is:
	NAMELIST /group/ var [,var...] [[,] /group/ var [,var...]...]
*/

namelist_stmt	:	tok_NAMELIST namelist_list EOS
		;

namelist_list	:	namelist_decl
		|	namelist_list namelist_decl
			{
			    $$ = $2;
			}
		;

namelist_decl	:	namelist_name namelist_var_list
			{
			     def_namelist(&($1),&($2));
			     $$ = $2;
			}
		;

namelist_name	:	'/' symbolic_name '/'
			{
			    $$ = $2;
			}
		;

namelist_var_list:	namelist_item
			{
			     $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	namelist_var_list namelist_item
			{
			    $$.subclass = $2.subclass;
			    $$.line_num = $2.line_num;
			    $$.col_num = $2.col_num;
			    $$.next_token = append_token($1.next_token,&($2));
			}
		;

namelist_item	:	symbolic_name
			{			   /* no comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_false(COMMA_FLAG,$$.subclass);
			}
		|	symbolic_name ','
			{			   /* has comma */
			     def_namelist_item(&($1));
			     primary_id_expr(&($1),&($$));
			     make_true(COMMA_FLAG,$$.subclass);
			}
		;

/* 20 */
type_stmt	:	arith_type_name arith_type_decl_list EOS
			{
				current_record_hash = -1;
			}
		|	plain_char_type_name char_type_decl_list EOS
		|	char_type_name char_type_decl_list EOS
		|	char_type_name ',' char_type_decl_list EOS
        |   pointer_type_name pointer_type_decl_list EOS
		;

arith_type_name	:	sizeable_type_name
			{
			  current_typesize = size_DEFAULT;
			}
				/* Allow *len to modify some arith types */
		|	sizeable_type_name '*' nonzero_unsigned_int_const
			{
			    current_typesize = $3.value.integer;
#if 0 /* defunct feature */
			    if(local_wordsize > 0) {
			      /*  recognize REAL*2w as DOUBLE PRECISION */
			      if(current_datatype == type_REAL
				 && $3.value.integer == type_size[type_DP])
				current_datatype = type_DP;
			      /*  recognize COMPLEX*4w as DOUBLE COMPLEX */
			      if(current_datatype == type_COMPLEX
				 && $3.value.integer==type_size[type_DCOMPLEX])
				current_datatype = type_DCOMPLEX;
			    }
#endif
			}
				/* Other type disallow *len modifier */
		|	unsizeable_type_name
		;

sizeable_type_name:	tok_INTEGER
			{
			     current_datatype = type_INTEGER;
			     integer_context = TRUE;
			}
		|	tok_REAL
			{
			     current_datatype = type_REAL;
			     integer_context = TRUE;
			}
		|	tok_COMPLEX
			{
			     current_datatype = type_COMPLEX;
			     integer_context = TRUE;
			}
		|	tok_LOGICAL
			{
			     current_datatype = type_LOGICAL;
			     integer_context = TRUE;
			}
		;

unsizeable_type_name:	tok_DOUBLE tok_PRECISION
			{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			}
		|	tok_DOUBLEPRECISION
			{
			     current_datatype = type_DP;
			     current_typesize = size_DEFAULT;
			}
		|	tok_DOUBLE tok_COMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			}
		|	tok_DOUBLECOMPLEX
			{
			     current_datatype = type_DCOMPLEX;
			     current_typesize = size_DEFAULT;
			}
		|	tok_BYTE /* treate BYTE as a form of integer for now */
			{
			     current_datatype = type_INTEGER;
			     current_typesize = 1;
			}
        |   tok_RECORD '/' symbolic_name '/'
            {
                current_datatype = type_RECORD;
                current_typesize = size_DEFAULT;
				current_record_hash = $3.value.integer;
            }
		;

plain_char_type_name:	tok_CHARACTER
			{
			     current_datatype = type_STRING;
			     current_typesize = 1;
			     integer_context = TRUE;
			}
		;

char_type_name	:	plain_char_type_name '*' len_specification
			{
			     current_typesize = $3.value.integer;
			}
		;

arith_type_decl_list:	arith_type_decl_item
		|	arith_type_decl_list ',' arith_type_decl_item
		;

arith_type_decl_item:	symbolic_name
			{
			     declare_type(&($1),
					  current_datatype,current_typesize);
			}
		|	array_declarator
			{
			     declare_type(&($1),
					  current_datatype,current_typesize);
			}
		;

char_type_decl_list:	char_type_decl_item
		|	char_type_decl_list ',' char_type_decl_item
		;

char_type_decl_item:	symbolic_name
			{
			     declare_type(&($1),
					  current_datatype,current_typesize);
			}
		|	symbolic_name '*' len_specification
			{
			     declare_type(&($1),
					  current_datatype,$3.value.integer);
			}
		|	array_declarator
			{
			     declare_type(&($1),
					  current_datatype,current_typesize);
			}
		|	array_declarator '*' len_specification
			{
			     declare_type(&($1),
					  current_datatype,$3.value.integer);
			}
   		;

pointer_type_name       :       tok_POINTER
                        {
                             current_datatype = type_POINTER;
                             current_typesize = size_DEFAULT;
                        }
                ;

pointer_type_decl_list: pointer_type_decl_item
                |       pointer_type_decl_list ',' pointer_type_decl_item
                ;

pointer_type_decl_item: '(' symbolic_name ',' symbolic_name ')'
                ;

/* 21 */
				/* implicit_flag helps is_keyword's work */
implicit_handle	:	tok_IMPLICIT {implicit_flag=TRUE;}
		;

implicit_stmt	:	implicit_handle implicit_decl_list EOS
			{
			    implicit_flag=FALSE;
			    if(implicit_none) {
			    }
			    else {
				implicit_type_given = TRUE;
			    }
			}
				/* IMPLICIT NONE statement */
		|	implicit_handle tok_NONE EOS
			{
			    implicit_flag=FALSE;
				if(implicit_type_given) {
				}
				else {
				    implicit_none = TRUE;
				}
			}
		;

implicit_decl_list:	implicit_decl_item
		|	implicit_decl_list ',' {initial_flag = TRUE;}
				       implicit_decl_item
		;

		/* implicit_letter_flag tells lexer to treat letters as letters,
			   not as identifiers */
implicit_decl_item:	type_name '('  {implicit_letter_flag = TRUE;}
				letter_list ')'  {implicit_letter_flag = FALSE;}
		;

letter_list	:	letter_list_item
		|	letter_list ',' letter_list_item
		;

letter_list_item:	tok_letter
			{
			  int c1 = (int)$1.subclass;
			   set_implicit_type(current_datatype,current_typesize,
			     		c1,c1);
			}
		|	tok_letter '-' tok_letter
			{
			  int c1 = (int)$1.subclass,
			      c2 = (int)$3.subclass;
			   set_implicit_type(current_datatype,current_typesize,
					c1,c2);
			}
		;


/* 22 */
len_specification:	'(' '*' ')'
			    {$$.value.integer = size_ADJUSTABLE;}
		|	nonzero_unsigned_int_const
			    {$$.value.integer = $1.value.integer;}
		|	'(' int_constant_expr ')'
			    {
			      if(($$.value.integer = $2.value.integer) <= 0 ){
				warning($2.line_num,$2.col_num,
					"invalid length specification");
				msg_tail(": substituting 1");
				$$.value.integer = 1;
			      }
			    }
		;

/* 23 */
parameter_stmt	:	tok_PARAMETER '(' parameter_defn_list ')' EOS
   		;

parameter_defn_list:	parameter_defn_item
		|	parameter_defn_list ',' parameter_defn_item
		;

parameter_defn_item:	symbolic_name {complex_const_allowed = TRUE;} '='
				expr
			{
			     def_parameter(&($1),&($4));
			     primary_id_expr(&($1),&($1));
			     assignment_stmt_type(&($1),&($3),&($4));
			     complex_const_allowed = FALSE;
			}
		;

/* 24 */
external_stmt	:	tok_EXTERNAL external_name_list EOS
		;

external_name_list:	symbolic_name
			{
			     def_ext_name(&($1));
			}
		|	external_name_list ',' symbolic_name
			{
			     def_ext_name(&($3));
			}
		;

/* 25 */
intrinsic_stmt	:	tok_INTRINSIC intrinsic_name_list EOS
		;

intrinsic_name_list:	symbolic_name
			{
			     def_intrins_name(&($1));
			}
		|	intrinsic_name_list ',' symbolic_name
			{
			     def_intrins_name(&($3));
			}
		;

/* 26 */
save_stmt	:	tok_SAVE EOS
			{
			  global_save = TRUE;
			}
		|	tok_SAVE save_list EOS
		;

save_list	:	save_item
		|	save_list ',' save_item
		;

save_item	:	symbolic_name
			{
			     save_variable(&($1));
			}
		|	'/' symbolic_name '/'
			{
			     save_com_block(&($2));
			}
		;

/* 27 */
data_stmt	:	tok_DATA data_defn_list EOS
   		;

data_defn_list	:	data_defn_item
		|	data_defn_list data_defn_item
		|	data_defn_list ',' data_defn_item
		;

data_defn_item	:	data_defn_assignee_list '/'
				{complex_const_allowed=TRUE;}
					data_value_list
				{complex_const_allowed=FALSE;}  '/'
		;

data_defn_assignee_list
		:	data_defn_assignee
		|	data_defn_assignee_list ',' data_defn_assignee
		;

data_defn_assignee:	lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_value_list:	data_value
		|	data_value_list ',' data_value
		;

data_value	:	data_constant_value
		|	data_repeat_factor '*' data_constant_value
		;

data_repeat_factor:	nonzero_unsigned_int_const
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;

data_constant_value:	data_constant
		|	symbolic_name
			{
			     use_parameter(&($1));
			}
		;


data_dlist	:	data_dlist_item
		|	data_dlist ',' data_dlist_item
		;

data_dlist_item	:	array_element_lvalue
			{
			     use_lvalue(&($1));
			}
		|	data_implied_do_list
		;

data_implied_do_list:  '(' data_dlist ',' symbolic_name
				'=' data_do_loop_bounds ')'
			{
			    use_implied_do_index(&($4));
			}
		;

data_do_loop_bounds:	int_constant_expr ',' int_constant_expr
		| int_constant_expr ',' int_constant_expr ',' int_constant_expr
		;


/* 29 */
assignment_stmt	:	lvalue '=' {complex_const_allowed = TRUE;
				    in_assignment_stmt = TRUE;} expr
			{
			  assignment_stmt_type(&($1),&($2),
					&($4));
			  complex_const_allowed = FALSE;
			  in_assignment_stmt = FALSE;
			}
				 EOS
			{
				/* Clear u-b-s flags spuriously set */
			  if(is_true(STMT_FUNCTION_EXPR, $1.subclass)
				     && stmt_sequence_no <= SEQ_STMT_FUN)
			     stmt_function_stmt(&($1));
		        }
		;

lvalue		:	variable_name
		|	lvalue '.' lvalue
			{
			    $$.dot_token = token_dup(&($3)); /* rigo */
			}
		|	array_element_lvalue
		|	substring_lvalue
		|	stmt_function_handle
		;


/* array-element_lvalue is at 88 */

assign_stmt	:    	tok_ASSIGN pre_label label tok_TO variable_name EOS
			{
			    do_ASSIGN(&($5));
			}
		;


/* 31 */
unconditional_goto:	goto pre_label label EOS
		;

/* 32 */
computed_goto	:	goto '(' goto_list ')' integer_expr EOS
		|	goto '(' goto_list ')' ',' integer_expr EOS
		;

/* 33 */
assigned_goto	:	goto symbolic_name EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		|	goto symbolic_name ',' '(' goto_list ')' EOS
			{
			     do_assigned_GOTO(&($2));
			}
		;

goto		:	tok_GOTO
			{
			    integer_context=TRUE;
			}
		|	tok_GO tok_TO
			{
			    integer_context=TRUE;
			}
		;

goto_list	:	pre_label label
		|	goto_list ',' pre_label label
		;

/* 34 */
arithmetic_if_stmt:	if_handle pre_label label ',' pre_label label
				 ',' pre_label label EOS
		;

/* 35 */
logical_if_stmt	:	if_handle executable_stmt
		;

/* 36 */
block_if_stmt	:	if_handle tok_THEN EOS
		;

if_handle	:	tok_IF '(' {complex_const_allowed = TRUE;}  expr ')'
			{
			    if(is_true(ID_EXPR,$4.subclass)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;	/* for is_keyword */
			    $$ = $4; /* Inherit expr for type checking above */
			}
		;

/* 37 */
else_if_stmt	:	tok_ELSE block_if_stmt
		|	tok_ELSEIF '(' {complex_const_allowed = TRUE;} expr ')'
			{
			    if(is_true(ID_EXPR,$4.subclass)){
				use_variable(&($4));
			    }
			    complex_const_allowed = FALSE;

			    initial_flag = TRUE;
			}
			tok_THEN EOS
		;

/* 38 */
else_stmt	:	tok_ELSE EOS
		;

/* 39 */
end_if_stmt	:	tok_ENDIF EOS
		|	tok_END tok_IF EOS
		;

/* 40 */
			/* Allow VAX/VMS extensions:
			   DO [label [,]] var = expr , expr [,expr]
			   DO [label [,]] WHILE ( expr )
			      ...
			   ENDDO
			*/

do_stmt		:	do_handle variable_name
				'=' do_loop_bounds EOS
			{
			     use_lvalue(&($2));
			     use_variable(&($2));

				/* Check for non-integer DO index or bounds */
			     if(datatype_of($2.class) == type_INTEGER
				&& datatype_of($4.class) != type_INTEGER)
			       warning($3.line_num,$3.col_num,
				  "type mismatch between DO index and bounds");

			     else if(datatype_of($2.class) != type_INTEGER)
			       if(datatype_of($4.class) != type_INTEGER) {
				 if(port_check)
				   nonportable($4.line_num,$4.col_num,
					       "non-integer DO loop bounds");
			       }
			       else {
				 if(trunc_check)
				   warning($2.line_num,$2.col_num,
					   "DO index is not integer");
			       }
			}
		|	do_handle tok_WHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$5.subclass)){
				use_variable(&($5));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.subclass);
			}
		|	tok_DOWHILE '('
				{complex_const_allowed=TRUE;} expr ')' EOS
			{
			    if(is_true(ID_EXPR,$4.subclass)){
				use_variable(&($4));
			    }
			    complex_const_allowed=FALSE;
			    make_true(NONSTD_USAGE_FLAG,$$.subclass);
			}
		;

do_handle	:	tok_DO pre_label label
		|	tok_DO pre_label label ','
		|	tok_DO pre_label
			{
			    make_true(NONSTD_USAGE_FLAG,$$.subclass);
			    integer_context=FALSE;
			}
		;

do_loop_bounds	:	int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.class=do_bounds_type(&($1),&($3),&($3));
			}
		|   int_real_dp_expr ',' int_real_dp_expr ',' int_real_dp_expr
			{
			    $$.class=do_bounds_type(&($1),&($3),&($5));
			}
		;

enddo_stmt	:	tok_END tok_DO EOS
		|	tok_ENDDO EOS
		;

/* 41 */
continue_stmt	:	tok_CONTINUE EOS
		;

/* 42 */
stop_stmt	:	tok_STOP stop_info EOS
		;

/* 43 */
pause_stmt	:	tok_PAUSE stop_info EOS
		;

stop_info	:	/* empty */
		|	tok_integer_const
		|	symbolic_name
			{
			     use_variable(&($1));
			}
		|	tok_string
		;

/* 44 */
write_stmt	:	write_handle
				{complex_const_allowed = FALSE;} EOS
		|	write_handle io_list
				{complex_const_allowed = FALSE;} EOS
		;

write_handle	:	tok_WRITE {control_item_count = 0;}
				'(' control_info_list ')'
				{complex_const_allowed = TRUE;}
		;

/* 45 */
		/* Note that parenthesized format_id's will end up in
		   control_info_list. Disambiguation left to semantic phase.
		   This is why we need the optional comma */
read_stmt	:	read_handle '(' control_info_list ')' EOS
		|	read_handle '(' control_info_list ')' io_list EOS
		|	read_handle '(' control_info_list ')' ',' io_list EOS
		|	read_handle format_id EOS
		|	read_handle format_id ',' io_list EOS
		;
read_handle	:	tok_READ {control_item_count = 0;}
		;

accept_stmt	:	tok_ACCEPT format_id EOS
		|	tok_ACCEPT format_id ',' io_list EOS
		;

/* 46 */
print_stmt	:	tok_PRINT format_id EOS
   		|	tok_PRINT format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
		;

type_output_stmt:	tok_TYPE format_id EOS
   		|	tok_TYPE format_id ','
				{complex_const_allowed = TRUE;} io_list
				{complex_const_allowed = FALSE;}  EOS
		;

/* 47 */
control_info_list:	control_info_item
			{
			    ++control_item_count;
			}
		|	control_info_list ',' control_info_item
			{
			    ++control_item_count;
			}
		;

	/* Note that unit id is not distinguished from format id
	   by the grammar. Use sequence no. to tell which is which.
	 */
control_info_item:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	unit_id
			{
			    if( $1.class != '*'
			       && is_true(ID_EXPR,$1.subclass)){
					/* WRITE(string,...) means store
					   output in the string */
				if(curr_stmt_class == tok_WRITE
				 && control_item_count == 0
				 && datatype_of($1.class) == type_STRING)
				    use_lvalue(&($1));
					/* READ/WRITE(..,namelist) means
					   I/O with variables of namelist. */
				else if( control_item_count == 1
				    && datatype_of($1.class) == type_NAMELIST)
				    ref_namelist(&($1),curr_stmt_class);

				use_variable(&($1));
			    }
			}
		;

			/* OPEN stmt needs its own control list defn to
			   allow for VMS READONLY and similar keywords.
			   Special prodn for unit_id as optional 1st item
			   needed to avoid reduce/reduce conflict with
			   later-occurring symbolic_name items.   */
open_info_list	:	unit_id
			{
			    if( $1.class != '*'
			       && is_true(ID_EXPR,$1.subclass)){
				use_variable(&($1));
			    }
			    ++control_item_count;
			}
		|	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			    ++control_item_count;
			}
		|	open_info_list ',' open_info_item
			{
			    ++control_item_count;
			}
		;

open_info_item	:	symbolic_name '=' unit_id
			{
			    use_io_keyword(&($1),&($3),curr_stmt_class);
			}
		|	symbolic_name	/* NOSPANBLOCKS, READONLY or SHARED */
			{
			    use_special_open_keywd(&($1));
			}
		;

/* 48 */
io_list		:	io_item
		|	io_list ',' io_item
		;

io_item		:	expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				if( curr_stmt_class == tok_READ ||
				    curr_stmt_class == tok_ACCEPT )
				    use_lvalue(&($1));
				else
				    use_variable(&($1));
			    }
			}
		|	io_implied_do_list
		;

/* 49 */
io_implied_do_list:	'(' io_list ',' variable_name '=' do_loop_bounds ')'
			{
			     use_implied_do_index(&($4));
			}
		;

/* 50 */
open_stmt	:	tok_OPEN {control_item_count = 0;}
				 '(' open_info_list ')' EOS
		;

/* 51 */
close_stmt	:	tok_CLOSE {control_item_count = 0;}
				'(' control_info_list ')' EOS
		;

/* 52 */
inquire_stmt	:	tok_INQUIRE {control_item_count = 0;}
				'(' control_info_list ')' EOS
		;

/* 53 */
backspace_stmt	:	backspace_handle unit_id EOS
		|	backspace_handle '(' control_info_list ')' EOS
		;
backspace_handle:	tok_BACKSPACE {control_item_count = 0;}
		;

/* 54 */
endfile_stmt	:	endfile_handle unit_id EOS
		|	endfile_handle '(' control_info_list ')' EOS
		;
endfile_handle	:	tok_ENDFILE {control_item_count = 0;}
		|	tok_END tok_FILE {control_item_count = 0;}
		;

/* 55 */
rewind_stmt	:	rewind_handle unit_id EOS
		|	rewind_handle '(' control_info_list ')' EOS
		;
rewind_handle	:	tok_REWIND {control_item_count = 0;}
		;


/* 56 */
		/* "expr" causes shift/reduce conflict on ')' between
		   red'n  unit_id: expr_  and shift  primary: ( expr_ ).
		   Use "associativity" rule to force reduction */
unit_id		:	expr		%prec REDUCE
		|	'*'
		;

/* 57 */
format_id	:	char_expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				 use_variable(&($1));
			    }
			}
		|	'*'
		;

/* 58,59 */
format_stmt	:	tok_FORMAT { inside_format=TRUE; } EOS
			{ inside_format=FALSE; }
		;

/* 70 handle only: complete defn handled as assignment stmt */

stmt_function_handle:	scalar_name '(' stmt_function_dummy_list ')'
			{
			  check_stmt_sequence(&($1),SEQ_STMT_FUN);

				def_stmt_function(&($1),&($3));
					/* make token info */
				primary_id_expr(&($1),&($$));
#ifdef DEBUG_PARSER
				if(debug_parser)
				  print_exprlist("stmt function",&($3));
#endif
			}
		;

stmt_function_dummy_list: /* empty list */
			{
			    $$.next_token = (Token*)NULL;
			}
		| nonempty_stmt_fun_dummy_list
		;

nonempty_stmt_fun_dummy_list:	  stmt_function_dummy_arg
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	  nonempty_stmt_fun_dummy_list ','
					stmt_function_dummy_arg
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		;

stmt_function_dummy_arg:  variable_name	/* for now: later, handle correctly */
		;

/* 71 */
call_stmt	:	call_handle
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' ')'
			{
			     call_subr(&($1),(Token*)NULL);
			     complex_const_allowed = FALSE;
			} EOS

		|	call_handle '(' expr_list ')'
			{
			     call_subr(&($1),&($3));
#ifdef DEBUG_PARSER
			     if(debug_parser)
				print_exprlist("call stmt",&($3));
#endif
			     complex_const_allowed = FALSE;
			} EOS
		;

call_handle	:	tok_CALL symbolic_name
			{
				if( current_module_hash != -1 )
				{
					if( highlight != -1 )
					{
						put_cross_ref(PAF_REF_TO_SUBROUTINE,
							cross_scope_type,
							PAF_REF_SCOPE_GLOBAL,
							NULL,
							hashtab[current_module_hash].name,
							NULL,
							NULL,
							hashtab[$2.value.integer].name,
							NULL,
							current_filename,
							$2.line_num,
							PAF_REF_PASS);
					}
				}
			    complex_const_allowed = TRUE;
			    $$ = $2;
			}
		;

expr_list	:	expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				 use_actual_arg(&($1));
				 use_variable(&($1));
			    }
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	'*' pre_label label
			{
			    $$.next_token = append_token((Token*)NULL,&($3));
			}
		|	expr_list ',' expr
			{
			    if(is_true(ID_EXPR,$3.subclass)){
				 use_actual_arg(&($3));
				 use_variable(&($3));
			    }
			    $$.next_token = append_token($1.next_token,&($3));
			}
		|	expr_list ',' '*' pre_label label
			{
			    $$.next_token = append_token($1.next_token,&($5));
			}
		;

/* 72 */
return_stmt	:	tok_RETURN EOS
			{
			     do_RETURN(current_module_hash,&($1));
			}
		|	tok_RETURN integer_expr EOS
			{
			     do_RETURN(current_module_hash,&($1));
			}
		;

/* 73 */
function_reference:	fun_or_substr_handle '(' fun_arg_list ')'
			{
				if( highlight != -1 )
				{
					put_cross_ref(PAF_REF_TO_FUNCTION,
						cross_scope_type,
						PAF_REF_SCOPE_GLOBAL,
						NULL,
						hashtab[current_module_hash].name,
						NULL,
						NULL,
						hashtab[$1.value.integer].name,
						NULL,
						current_filename,
						$1.line_num,
						PAF_REF_PASS);
				}
				   /* restore context */
				if(!is_true(COMPLEX_FLAG,$1.subclass))
				  complex_const_allowed=FALSE;
				if(is_true(IN_ASSIGN,$1.subclass))
				  in_assignment_stmt = TRUE;

				  /* Change empty arg list to no arg list */
				if($3.next_token == NULL)
				  call_func(&($1),(Token *)NULL);
				else
				  call_func(&($1),&($3));
							/* make token info */
				func_ref_expr(&($1),&($3),&($$));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("function",&($3));
#endif
			}
		;

fun_or_substr_handle:	scalar_name
			{
			    if(complex_const_allowed)/* save context */
			        make_true(COMPLEX_FLAG,$$.subclass);
			    complex_const_allowed=TRUE;
			    if(in_assignment_stmt)
			        make_true(IN_ASSIGN,$$.subclass);
			    in_assignment_stmt = FALSE;
			}
		;

fun_arg_list	:	/* empty */
			{
				$$.class = 0;
				$$.next_token = NULL;
			}
		|	nonempty_fun_arg_list
		;

nonempty_fun_arg_list:	expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				 use_actual_arg(&($1));
/* 				 use_variable(&($1)); */
			    }
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	nonempty_fun_arg_list ',' expr
			{
			    if(is_true(ID_EXPR,$3.subclass)){
				 use_actual_arg(&($3));
/* 				 use_variable(&($3)); */
			    }
			    $$.next_token = append_token($1.next_token,&($3));
			}

/* 74 not present: type checking not done at this level */

expr	:	log_disjunct

		|	expr tok_EQV log_disjunct
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		|	expr tok_NEQV log_disjunct
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_disjunct	:	log_term

		|	log_disjunct tok_OR log_term
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_term	:	log_factor

		|	log_term tok_AND log_factor
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

log_factor	:	log_primary

		|	tok_NOT log_primary
			{
			    unexpr_type(&($1),&($2),&($$));
			}
		;

log_primary	:	arith_expr

		|	log_primary tok_relop log_primary
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;


arith_expr	:	term

		|	'-' term
			{
			    unexpr_type(&($1),&($2),&($$));
			}
		|	'+' term
			{
			    unexpr_type(&($1),&($2),&($$));
			}
		|	arith_expr '+' term
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		|	arith_expr '-' term
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

term		:	factor

		|	term '/' factor
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			    if(div_check &&
			       !is_true(CONST_EXPR,$3.subclass)){
				warning($2.line_num,$2.col_num,
					"Possible division by zero");
			    }
			}
		|	term '*' factor
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

factor		:	char_expr

		|	char_expr tok_power factor
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

char_expr	:	primary

		|	primary '.' char_expr
			{
			    $$.dot_token = token_dup(&($3)); /* rigo */
/* 			    $$.next_token = append_token($1.next_token,&($3)); */
			}
		|	char_expr tok_concat primary
			{
			    binexpr_type(&($1),&($2),&($3)
					 ,&($$));
			}
		;

primary		:	variable_name

		|	array_element_name

		|	function_reference

		|	substring_name

		|	literal_const
			{
			    $$.subclass = 0;
			    make_true(CONST_EXPR,$$.subclass);
			    make_true(PARAMETER_EXPR,$$.subclass);
			    make_true(LIT_CONST,$$.subclass);
			    make_true(EVALUATED_EXPR,$$.subclass);
			}
		|	'(' expr ')'
			{
			    $$ = $2;
				/* (identifier) becomes a non-identifier */
			    if(is_true(LVALUE_EXPR,$2.subclass)) {
				use_variable(&($2));
				make_false(LVALUE_EXPR,$$.subclass);
				make_false(ARRAY_ID_EXPR,$$.subclass);
				make_false(ID_EXPR,$$.subclass);
			    }
			}
		;

				/* Literal constants are numbers, strings
				   holleriths, and logical constants */
literal_const	:	numeric_const
			{
			    /* (class is set in numeric_const productions) */
			    $$.size = size_DEFAULT;
			}
		|	tok_string
			{
			    $$.class = type_byte(class_VAR,type_STRING);
			    /* (size is set in get_string) */
			}
		|	tok_hollerith
			{
			    $$.class = type_byte(class_VAR,type_HOLLERITH);
			    /* (size is set in get_hollerith) */
			    if(port_check && hollerith_check) {
				warning($1.line_num,$1.col_num,
				"hollerith constant may not be portable");
			    }
			}
		|	tok_logical_const
			{
			    $$.class = type_byte(class_VAR,type_LOGICAL);
			    $$.size = size_DEFAULT;
			}
		;

numeric_const	:	tok_integer_const
			{
			    $$.class = type_byte(class_VAR,type_INTEGER);
			}
		|	tok_real_const
			{
			    $$.class = type_byte(class_VAR,type_REAL);
			}
		|	tok_dp_const
			{
			    $$.class = type_byte(class_VAR,type_DP);
			}
		|	tok_complex_const
			{
			    $$.class = type_byte(class_VAR,type_COMPLEX);
			}
		|	tok_dcomplex_const
			{
			    $$.class = type_byte(class_VAR,type_DCOMPLEX);
			}
		;

/* 77 */
integer_expr	:	/* integer */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				use_variable(&($1));
			    }
			}
		;

/* 78 */
int_real_dp_expr:	/* integer, real, or double */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				use_variable(&($1));
			    }
			}
		;

/* 79 absent */

/* 80 */
int_constant_expr:	/* integer const */ arith_expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				use_variable(&($1));
			    }
			    if( is_true(CONST_EXPR,$1.subclass) ) {
			      if(datatype_of($1.class) == type_INTEGER){
				$$.value.integer = int_expr_value(&($1));
			      }
			    }
			}
		;

/* 81 */
dim_bound_expr	:       /* integer */  arith_expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				use_variable(&($1));
			    }

			    if( datatype_of($1.class) != type_INTEGER ){
				$$.value.integer = 0;
			    }
			    else {
			      if( is_true(EVALUATED_EXPR,$1.subclass) )
				$$.value.integer =
				  int_expr_value(&($1));
			      else		/* must be dummy */
				$$.value.integer = 0;
			    }
			}
		;

/* 82-85 absent: no type checking here */
/* 86-87 absent: see 76 */

/* 88 */
array_element_lvalue:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array lvalue",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.subclass);
			}
		;

array_element_name:	array_name '(' subscript_list ')'
			{
				ref_array(&($1),&($3));
#ifdef DEBUG_PARSER
				if(debug_parser)
				    print_exprlist("array",&($3));
#endif
					/* array now becomes scalar */
				make_false(ARRAY_ID_EXPR,$$.subclass);
			}
		;

subscript_list	:	subscript
			{
			    $$.next_token = append_token((Token*)NULL,&($1));
			}
		|	subscript_list ',' subscript
			{
			    $$.next_token = append_token($1.next_token,&($3));
			}
		     ;

subscript	:	expr
			{
			    if(is_true(ID_EXPR,$1.subclass)){
				 use_variable(&($1));
			    }
				/* check subscript exprs for integer type */
			    if(datatype_of($1.class) != type_INTEGER)
			      if(trunc_check)
			         warning($1.line_num,$1.col_num,
					 "subscript is not integer");
			}
		;

/* 89 */
substring_name	:	fun_or_substr_handle  substring_interval
			{
				   /* restore status of complex flag */
			    if(!is_true(COMPLEX_FLAG,$1.subclass))
				  complex_const_allowed=FALSE;
			    $$.size=substring_size(&($1),&($2));
			}

		|	array_element_name substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		;

substring_lvalue:	scalar_name substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		|	array_element_lvalue substring_interval
			{
			    $$.size=substring_size(&($1),&($2));
			}
		;
			/* substring interval: limits go into class,subclass */
substring_interval:	'(' ':' ')'
			{
			    $$.class=1;
			    $$.subclass=0; /* 0 means LEN */
			}

		  |	'(' substr_index_expr ':' ')'
			{
			    $$.class=$2.value.integer;
			    $$.subclass=0; /* 0 means LEN */
			}
		  |	'(' ':' substr_index_expr ')'
			{
			    $$.class=1;
			    $$.subclass=$3.value.integer;
			}
		  |	'(' substr_index_expr ':' substr_index_expr ')'
			{
			      $$.class=$2.value.integer;
			      $$.subclass=$4.value.integer;
			}
		  ;

substr_index_expr:	arith_expr
			{
			  if(is_true(ID_EXPR,$1.subclass)){
			    use_variable(&($1));
			  }
				/* check validity and replace nonconst
				   value by size_UNKNOWN. */
			  if(is_true(CONST_EXPR,$1.subclass)) {
			    $$.value.integer=int_expr_value(&($1));
			  }
			  else  /* (no longer need ID hash index) */
			    $$.value.integer=size_UNKNOWN;
			}
		;

/* 90-98 absent: name categories not distinguished */

/* 99 */
variable_name	:	scalar_name
		|	array_name
		;

scalar_name	:	tok_identifier
			{
/* 				printf( "Scalar name: %d <%s>\n" */
/* 					  , $1.value.integer */
/* 					  , hashtab[$1.value.integer].name ); */
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));

				if( highlight != -1 && hashtab[$1.value.integer].define )
				{
					put_cross_ref(PAF_REF_TO_CONSTANT,
						cross_scope_type,
						PAF_REF_SCOPE_GLOBAL,
						NULL,
						hashtab[current_module_hash].name,
						NULL,
						NULL,
						hashtab[$1.value.integer].name,
						NULL,
						current_filename,
						$1.line_num,
						PAF_REF_READ);
				}
			}
		;

array_name	:	tok_array_identifier
			{
/* 				printf( "Array name: <%s>\n", hashtab[$1.value.integer].name ); */
			    ref_variable(&($1));
			    primary_id_expr(&($1),&($$));

				if( highlight != -1 && hashtab[$1.value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[$1.value.integer].name,
						current_filename,
						$1.line_num,
						$1.curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,$1.line_num),
						0,0,0,0);
				}
			}
		;


/* symbolic_name refers to a name without making it into an id expr */
symbolic_name	:	tok_identifier
			{
				if( highlight != -1 && hashtab[$1.value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[$1.value.integer].name,
						current_filename,
						$1.line_num,
						$1.curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,$1.line_num),
						0,0,0,0);
				}
			}
		|	tok_array_identifier
			{
				if( highlight != -1 && hashtab[$1.value.integer].define )
				{
					put_symbol(PAF_CONS_DEF,NULL,
						hashtab[$1.value.integer].name,
						current_filename,
						$1.line_num,
						$1.curr_index,
						0,0,
						(long)0,NULL,NULL,NULL,
						get_comment(current_filename,$1.line_num),
						0,0,0,0);
				}
			}
		;

/* 100 */
data_constant	:	numeric_const
		|	'-' numeric_const
		|	'+' numeric_const
		|	tok_logical_const
   		|	tok_string
		|	tok_hollerith
		;

/* 101-102 absent */

/* 103 */
nonzero_unsigned_int_const:
			tok_integer_const
			{
			  if($1.value.integer == 0) {
			    warning($1.line_num,$1.col_num,
				    "nonzero integer expected");
			    msg_tail(": substituting 1");
			    $$.value.integer = 1;
			  }
			}
		;

/* 104-109 absent: lexer handles these */
	/* pre_label prepares for an expected label by setting flag
	   so that lexer won't look for E-format number.  All grammar
	   rules that have "label" precede it with "pre_label" */
pre_label	:	/* NOTHING */
			{
			    integer_context=TRUE;
			}
		;

/* 110 */
label		:	tok_integer_const
			{
				integer_context=FALSE;
				$$.class = type_byte(class_LABEL,type_LABEL);
				$$.subclass = 0;
			}
		;

/* 111-116 absent: lexer handles these */

%%
void
init_parser()			/* Initialize various flags & counters */
{
	initial_flag = TRUE;	/* set flag for keyword test */
	implicit_flag=FALSE;	/* clear flags for IMPLICIT stmt */
	implicit_letter_flag = FALSE;
	implicit_type_given = FALSE;
	implicit_none = FALSE;
	global_save = FALSE;
	prev_token_class = EOS;
	complex_const_allowed = FALSE;
	stmt_sequence_no = 0;
	true_prev_stmt_line_num = 0;
}

		/* Propagate non-integer type if any of DO loop
		   bounds are non-integer. */
PRIVATE int
do_bounds_type(t1,t2,t3)
     Token *t1, *t2, *t3;
{
  int result_class;
       if(datatype_of(t1->class) != type_INTEGER) result_class = t1->class;
  else if(datatype_of(t2->class) != type_INTEGER) result_class = t2->class;
  else if(datatype_of(t3->class) != type_INTEGER) result_class = t3->class;
  else result_class = t1->class;
  return result_class;
}


/* Debugging routine: prints the expression list of various productions */

#ifdef DEBUG_PARSER
PRIVATE void
print_exprlist(s,t)
	char *s;
	Token *t;
{

	fprintf(list_fd,"\n%s arglist: ",s);

	if(t == NULL)
		fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->class)]);
		  if( is_true(ID_EXPR,t->subclass) )
			fprintf(list_fd,"(%s) ",token_name(*t));
	    }
	}
}
#endif /* DEBUG_PARSER */

#ifdef DEBUG_PARSER
PRIVATE void
print_comlist(s,t)
	char *s;
	Token *t;
{

	fprintf(list_fd,"\n%s varlist: ",s);

	if(t == NULL)
		fprintf(list_fd,"(empty)");
	else {
  	    while( (t=t->next_token) != NULL) {
		  fprintf(list_fd,"%s ",type_name[datatype_of(t->class)]);
		  if( is_true(ID_EXPR,t->subclass) )
			fprintf(list_fd,"(%s) ",token_name(*t));
		}
	  }
}
#endif /* DEBUG_PARSER */

/* After having parsed prog_stmt, function_stmt, subroutine_stmt,
   block_data_stmt, the stmt_sequence_no is set to the value SEQ_HEADER.
*/

void
check_seq_header(t)
     Token *t;
{
	if(stmt_sequence_no >= SEQ_HEADER) {
	   END_processing(t);
	}
	stmt_sequence_no = SEQ_HEADER;
}

void
check_stmt_sequence(t,seq_num)
     Token *t;
     int seq_num;
{
    if(stmt_sequence_no <= seq_num) {
	stmt_sequence_no = seq_num;
    }
}

	/* After having parsed end_stmt, common block lists and
	   subprogram argument lists are copied over into global symbol
	   table, the local symbol table is printed out and then cleared,
	   and stmt_sequence_no is set to zero for start of next module.
	*/

PRIVATE void
END_processing(t)
	Token *t;
{
  if(current_module_hash != -1) {
	if(do_list && t != (Token *)NULL)
	    flush_line_out(t->line_num);
/* 	check_loose_ends(current_module_hash); */
	process_lists(current_module_hash);
	debug_symtabs();
#ifdef ERROR_MESS
#endif
	print_loc_symbols(current_module_hash);
	init_symtab();
  }
  exec_stmt_count = 0;
  stmt_sequence_no = 0;
  current_module_hash = -1;
  implicit_type_given = FALSE;
  implicit_none = FALSE;
  true_prev_stmt_line_num = 0;
  integer_context = FALSE;
  global_save = FALSE;
}

		/* Routine to add token t to the front of a token list. */
PRIVATE Token *
append_token(tlist,t)
     Token *tlist, *t;
{
	Token *tcopy;
	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->next_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}

		/* Routine to add token t to the front of a dot_token list. */
#if 0
PRIVATE Token *
append_dot_token(tlist,t)
     Token *tlist, *t;
{
	Token *tcopy;
	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */
	tcopy->dot_token = tlist; /* link it onto front of list */
	return tcopy;		/* return it as new tlist */
}
#endif

char *print_line_num( int line_num )
{
	static char ac[100];

	if( strcmp( current_filename, top_filename ))
	{
		sprintf( ac, "%d %s", line_num, current_filename );
	}
	else
	{
		sprintf( ac, "%d", line_num );
	}

	return ac;
}

static Token *token_dup( Token *t )
{
	Token *tcopy;

	if((tcopy=new_token()) == (Token *)NULL){
	  oops_message(OOPS_FATAL,line_num,NO_COL_NUM,
#ifdef LARGE_MACHINE
"Out of token space\nRecompile me with larger TOKENSPACESZ value"
#else
"Out of token space\nRecompile me with LARGE_MACHINE option"
#endif
		       );
	}

	*tcopy = *t;		/* make permanent copy of token */

	return tcopy;
}

#ifdef NO_DATABASE

int put_cross_ref( int typ, int scope_type, int scope, char *name1, char *name2, char *name3, char *name4, char *filename, int lineno, int mode )
{
	FILE *sym_fp = stdout;

	fprintf( sym_fp,"%d", typ );
	fprintf( sym_fp," %d #", scope );
	if( name1 ) fprintf( sym_fp," %s", name1 );
	if( name2 ) fprintf( sym_fp," %s", name2 );
	if( name3 ) fprintf( sym_fp," %s", name3 );
	if( name4 ) fprintf( sym_fp," %s", name4 );
	if( filename ) fprintf( sym_fp," %s", filename );
	fprintf( sym_fp," %d", lineno );
	fprintf( sym_fp," %d", mode );
	fprintf( sym_fp, "\n" );
	return 0;
}

int put_symbol( int typ, char *function, char *name, char *filename, int lineno , int colpos, long l, void *x1, void *x2, void *x3)
{
	FILE *sym_fp = stdout;

	fprintf( sym_fp,"%d #", typ );
	if( function ) fprintf( sym_fp," %s", function );
	if( name ) fprintf( sym_fp," %s", name );
	if( filename ) fprintf( sym_fp," %s", filename );
	fprintf( sym_fp," %d.%d", lineno,colpos);
	fprintf( sym_fp, "\n" );
}

int put_symbol_comment( char *class, char *function, char *current_filename, char *comment, int start_line, int start_char, int end_line, int end_char )
{
	printf( "comment: <%s>\n", comment );
}

int put_comment( char *class_name, char *func_name, char *filename, char *comment, int line_num, int col_num )
{
}

#endif

