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

/*
 * parse.y
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * A GNU Bison grammar for the CHILL programming language.
 *
 * Almost directly translated from Per Bothner's two-pass recursive descent
 * parser that forms GNU CHILL.
 */

%{

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#include "common.h"
#include "symtab.h"
#include "emit.h"

static char brackets[] = "()";

/* Should we emit information about procedure calls and variables when
   encountered inside an expr?  This will mostly be set to 1, but
   there are some situations when this should be set to 0. */

static int emit = 0;

%}

%union {
   struct punctuation punct;
   struct blockregion block;
   struct identifier id;
   struct identifierlist * idlist;
   struct kword keyword;
   struct datatype type;
   struct attribute attrib;
   struct sig signature;
}

%token <punct> ';'

%token <attrib> DYNAMIC IN OUT INOUT LOC

%token <type> ACCESS ARRAY BIN BOOLS BUFFER CHARS EVENT POWERSET RANGE READ REF ROW
%token <type> SET STRUCT TEXT

%token <keyword> MODULE REGION SPEC END

%token AFTER ALL AND ANDIF ASSERT AT
%token BASED BEG BODY BY
%token CASE CAUSE CONTEXT CONTINUE CYCLE
%token DCL DELAY DO DOWN 
%token ELSE ELSIF ESAC EVER EXCEPTIONS EXIT
%token FI FOR FORBID
%token GENERAL GOTO GRANT
%token IF INIT INLINE 
%token MOD 
%token NEWMODE NONREF NOPACK NOT
%token OD OF ON OR ORIF
%token PACK POS PREFIXED PRIORITY PROC PROCESS
%token RECEIVE RECURSIVE REM REMOTE RESULT RETURN RETURNS 
%token SEIZE SEND SIGNAL SIMPLE START STATIC STEP STOP SYN SYNMODE
%token THEN THIS TIMEOUT TO
%token UP
%token VARYING
%token WHILE WITH
%token XOR

%token BITLITERAL BOOLITERAL INTLITERAL STRINGLITERAL CHARLITERAL 
%token EMPTYLITERAL FLOATLITERAL
%token <id> NAME

%token ASSIGN LESSTHANEQ GREATERTHANEQ NOTEQ LEFTTUPLE RIGHTTUPLE ARROW
%token DOUBLESOLIDUS EXPONENT

%type <id> name_string, opt_name_string, set_list_element, variable
%type <idlist> def_occ_list, set_list, param_list, variable_list
%type <type> mode, opt_result_spec, index_mode, procedure_mode, structure_mode
%type <type> param
%type <signature> procedure_definition, process_definition
%type <attrib> param_attr, opt_loc
%type <block> modulion, spec_module

%%

program:
	program { set_module_name(NULL); } modulion
		{
		  emit_module(NULL, $3);
		}
|	program NAME ':' { set_module_name($2.name); } modulion
		{
		  emit_module(&$2, $5);
		}
|	program { set_spec_module_name(NULL); } spec_module
		{
		  emit_spec_module(NULL, $3);
		}
|	program NAME ':' { set_spec_module_name($2.name); } spec_module
		{
		  emit_spec_module(&$2, $5);
		}
|	/* an empty translation unit */
;

/* According to gcc/ch/parse.c, we can still parse multi-dimension cases using the single-dimension syntax. */

case_action:
	CASE expr_list OF opt_range_list case_alternative_list ESAC
|	CASE expr_list OF opt_range_list case_alternative_list ELSE opt_actions ESAC
;

opt_range_list:
	range_list ';'
|	/* empty */
;

range_list:
	name_string {}
|	range_list ',' name_string
;

case_alternative_list:
	case_alternative
|	case_alternative_list case_alternative
;

case_alternative:
	case_label_specification ':' opt_actions
;

/* End of case action stuff. */

def_occ_list:
	NAME
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  memcpy($$, &$1, sizeof(struct identifier));
		  $$->next = NULL;
		}
|	def_occ_list ',' NAME
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  memcpy($$, &$3, sizeof(struct identifier));
		  $$->next = $1;
		}
;

delay_case_action:
	DELAY CASE dcase_event_superlist ESAC
|	DELAY CASE SET expr ';' dcase_event_superlist ESAC
|	DELAY CASE SET expr ';' PRIORITY expr ';' dcase_event_superlist ESAC
;

dcase_event_superlist:
	delay_case_event_list dcase_event_superlist
|	action dcase_event_superlist
|	/* empty */
;

modulion:
	MODULE body END opt_handler opt_name_string ';'
		{
		  $$.startline = $1.line;
		  $$.startcol = $1.startcol;
		  $$.endline = $6.line;
		  $$.endcol = $6.endcol;
		}
|	REGION body END opt_handler opt_name_string ';'
		{
		  $$.startline = $1.line;
		  $$.startcol = $1.startcol;
		  $$.endline = $6.line;
		  $$.endcol = $6.endcol;
		}
;

delay_case_event_list:
	'(' expr_list ')' ':'
;

expr_list:
	expr
|	expr ',' expr_list
;

field:
	CASE OF variant_alternative_list ESAC
|	CASE OF variant_alternative_list ELSE ESAC
|	CASE OF variant_alternative_list ELSE variant_field_list ESAC
|	CASE def_occ_list OF variant_alternative_list ESAC
|	CASE def_occ_list OF variant_alternative_list ELSE ESAC
|	CASE def_occ_list OF variant_alternative_list ELSE variant_field_list ESAC
|	fixed_field
;

variant_field_list:
	fixed_field
|	variant_field_list ',' fixed_field
;

variant_alternative_list:
	variant_alternative
|	variant_alternative_list ',' variant_alternative
;

receive_spec:
	'(' NAME ')' ':'
|	'(' NAME IN expr_list ')' ':'
;

signal_definition:
	NAME
		{ }
|	NAME '=' '(' mode_list ')'
		{ }
|	NAME TO name_string
		{ }
|	NAME '=' '(' mode_list ')' TO name_string
		{ }
;

mode_list:
	mode
		{ }
|	mode_list ',' mode
		{ }
;

receive_spec_list:
	receive_spec receive_spec_list
|	action receive_spec_list
|	/* empty */
;

opt_varying:
	VARYING
|	/* empty */
;

opt_handler:
	handler
|	/* empty */
;

handler:
	ON on_alternatives END
|	ON on_alternatives ELSE opt_actions END
;

on_exception_list:
	'(' name_string_list ')' ':'
;

end:
	opt_handler opt_name_string ';'
;

args:
	/* empty */
|	arglist
;

arglist:
	expr
|	args ',' expr
;

action:
	other_actions end
|	';'
		{ }
;

other_actions:
	AFTER expr IN opt_actions TIMEOUT opt_actions END
|	AFTER expr DELAY IN opt_actions TIMEOUT opt_actions END
|	ASSERT expr
|	AT primval IN opt_actions TIMEOUT opt_actions END
|	BEG body END
|	case_action
|	CAUSE name_string
|	CONTINUE expr
|	CYCLE expr IN opt_actions END
|	DELAY expr opt_prio_expr
|	delay_case_action
|	do_action
|	EXIT name_string
|	GOTO name_string
|	IF expr then_clause FI
|	IF expr then_clause else_clause FI
|	RECEIVE CASE receive_spec_list opt_else_actions ESAC
|	RECEIVE CASE SET NAME ';' receive_spec opt_else_actions ESAC
|	RESULT expr
|	RETURN opt_expr
|	SEND name_string opt_with_expr opt_to_expr opt_prio_expr
|	SEND name_string '(' expr_list ')' opt_with_expr opt_to_expr opt_prio_expr
|	START name_string '(' args ')'
		{
		  emit_xref_procedure($2.name, $2.line);
		}
|	START name_string '(' args ')' SET NAME
		{
		  emit_xref_procedure($2.name, $2.line);
		}
|	STOP
;

opt_expr:
	expr
|	/* empty */
;

opt_else_actions:
	ELSE opt_actions
;

grant_stmt:
	rename_clauses
|	postfix_list opt_prefix_clause
;

opt_result_spec:
	RETURNS '(' mode ')'
		{
		  $$ = $3;
		}
|	RETURNS '(' mode LOC ')'
		{
		  $$ = $3;
		}
|	RETURNS '(' mode LOC DYNAMIC ')'	
		{
		  $$ = $3;
		}
|	RETURNS '(' mode NONREF LOC ')'
		{
		  $$ = $3;
		}
|	RETURNS '(' mode NONREF LOC DYNAMIC ')'
		{
		  $$ = $3;
		}
|	/* empty */
		{
		  $$.text = NULL;
		}
;
     
procedure_mode:
	PROC '(' ')' opt_result_spec opt_except opt_recursive
		{
		  $$.text = SN_StrDup("PROC");
		}
|	PROC '(' proc_mode_list ')' opt_result_spec opt_except opt_recursive
		{
		  $$.text = SN_StrDup("PROC");
		}
;

proc_mode_list:
	mode param_attr
		{ }
|	proc_mode_list ',' mode param_attr
		{ }
;
 
opt_procedureattr:
	GENERAL opt_recursive
|	SIMPLE opt_recursive
|	INLINE opt_recursive
|	opt_recursive
;

opt_recursive:
	RECURSIVE
|       /* empty */
;

rename_clauses:
	rename_clause
|	rename_clauses ',' rename_clause
;

rename_clause:
	'(' opt_name_string ARROW opt_name_string ')' '!' postfix
;

seize_stmt:
	rename_clauses
|	postfix_list opt_prefix_clause
;

opt_with_expr:
	WITH expr
|	/* empty */
;

opt_to_expr:
	TO expr
|	/* empty */
;

opt_prio_expr:
	PRIORITY expr
|	/* empty */
;

set_list:
	set_list_element
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  memcpy($$, &$1, sizeof(struct identifier));
		  $$->next = NULL;
		}
|	set_list ',' set_list_element
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  memcpy($$, &$3, sizeof(struct identifier));
		  $$->next = $1;
		}
;

set_list_element:
	NAME
		{
		  $$ = $1;
		}
|	NAME '=' expr
		{
		  $$ = $1;
		}
;

signal_definition_stmt:
	signal_definition
|	signal_definition_stmt ',' signal_definition
;

spec_module:
	SPEC MODULE spec_definitions END opt_name_string ';'
	{
	  $$.startline = $1.line;
	  $$.startcol = $1.startcol;
	  $$.endline = $6.line;
	  $$.endcol = $6.endcol;
	}
;

spec_definitions:
	spec_definitions definition
|	spec_definitions NAME ':' PROC { set_proc_name($2.name); } procedure_definition
|	spec_definitions NAME ':' PROCESS { set_process_name($2.name); } process_definition
|	/* empty */
;

structure_mode:
	STRUCT '(' fields ')'
		{
		  $$ = $1;
		}
;

fields:
	field
|	fields ',' field
;

fixed_field:
	def_occ_list mode opt_layout
		{
		  /* for now */
		}
;

primval:
	INTLITERAL
|	BITLITERAL
|	BOOLITERAL
|	CHARLITERAL
|	EMPTYLITERAL
|	FLOATLITERAL
|	STRINGLITERAL
|	THIS
|	tuple
;

opt_args:
	/* empty */
|	opt_args '(' args ')'
	;

opt_dot:
	/* empty */
|	'.'
;

variable:
	NAME
		{
		  $$ = $1;
		  $$.type = simple;
		}
|	NAME '(' args ')'
		{
		  $$ = $1;
		  $$.type = call;
		}
|	NAME '(' args ')' '(' args ')' opt_args
		{
		  $$ = $1;
		  $$.type = array; /* multidimensional array */
		}	
|	NAME '(' expr ':' expr ')'
		{
		  $$ = $1;
		  $$.type = array; /* slice */
		}
|	NAME '(' expr UP expr ')'
		{
		  $$ = $1;
		  $$.type = array; /* slice */
		}
|	ARROW opt_dot NAME
		{
		  $$ = $3;
		  $$.type = simple;
		}
|	ARROW opt_dot NAME '(' args ')' opt_args
		{
		  $$ = $3;
		  $$.type = array;
		}
|       ARROW opt_dot NAME '(' expr ':' expr ')'
		{
		  $$ = $3;
		  $$.type = array; /* slice */
		}
|       ARROW opt_dot NAME '(' expr UP expr ')'
		{
		  $$ = $3;
		  $$.type = array; /* slice */
		}
|	variable '.' NAME
|	variable '.' NAME '(' args ')' opt_args
|	variable '.' NAME '(' expr ':' expr ')'
|	variable '.' NAME '(' expr UP expr ')'
|	variable ARROW opt_dot NAME
|	variable ARROW opt_dot NAME '(' args ')' opt_args
|	variable ARROW opt_dot NAME '(' expr ':' expr ')'
|	variable ARROW opt_dot NAME '(' expr UP expr ')'
;

operand7:
	primval
|	variable
		{
		  if (with_active())
		  {
			if (get_with_scope() == NULL)
			{
#ifdef DEBUG
			  printf("WITH: into scope %s\n", $1.name);
#endif
			  set_with_scope($1.name);
			}
		  }
		  else if (emit)
		  {
			if ($1.type == call)
		    	{
			  emit_xref_procedure($1.name, $1.line);
		    	}
			else 
			{
			  emit_xref_variable($1.name, $1.line);
			}
		  }
		}
|	'(' operand0 ')'	
|	'(' operand0 ')' primval
; 

operand6:
	NOT operand7
|	'-' operand7
|	operand7
;

operand5:
	operand5 EXPONENT operand6
|	operand6
;

operand4:
	operand4 '*' operand5
|	operand4 '/' operand5
|	operand4 MOD operand5
|	operand4 REM operand5
|	operand5
;

operand3:
	operand3 '+' operand4
|	operand3 '-' operand4
|	operand3 DOUBLESOLIDUS operand4
|	operand4
; 

operand2:
	operand2 IN operand3
|	operand2 '>' operand3
|	operand2 GREATERTHANEQ operand3
|	operand2 '<' operand3
|	operand2 LESSTHANEQ operand3
|	operand2 '=' operand3
|	operand2 NOTEQ operand3
|	operand3
;

operand1:
	operand1 AND operand2
|	operand1 ANDIF operand2
|	operand2
;

operand0:
	operand0 OR operand1
|	operand0 XOR operand1
|	operand0 ORIF operand1
|	operand1
;

expr:
	if_expr
|	case_expr
|	operand0
;

dyadic_operator:
	'+' | '-' | '/' | '*' | MOD | REM | DOUBLESOLIDUS | AND | OR | XOR
;

opt_layout:
	layout
|	/* empty */
;

layout:
	PACK
|	NOPACK
|	POS pos
|	STEP step
;

if_expr:
	IF if_expr_body FI
		{ }
;

if_expr_body:
	expr then_alternative else_alternative
;

else_alternative:
	ELSIF if_expr_body
		{ }
|	ELSE expr
		{ }
;

opt_prefix_clause:
	PREFIXED opt_name_string
|	/* empty */
;

else_clause:
	ELSIF expr then_clause 
		{ }
|	ELSIF expr then_clause else_clause
		{ }
|	ELSE opt_actions
		{ }
;

then_clause:
	THEN opt_actions
		{ }
;

then_alternative:
	THEN expr
;

tuple:
	LEFTTUPLE RIGHTTUPLE
|	LEFTTUPLE tuple_element_list RIGHTTUPLE
;

tuple_element_list:
	tuple_element
|	tuple_element_list ',' tuple_element
;

tuple_element:
	tuple_fieldname_list ':' expr
|	case_label_list ':' expr
|	expr ':' expr
|	expr
;

tuple_fieldname_list:
	'.' NAME
	| tuple_fieldname_list ',' '.' NAME ','
;

opt_actions:
	opt_actions action
|	opt_actions NAME ':' action
|	opt_actions NAME '(' args ')' end
		{
		  emit_xref_procedure($2.name, $2.line);
		}
|	opt_actions NAME ':' NAME '(' args ')' end
		{
		  emit_xref_procedure($4.name, $4.line);
		}
|	opt_actions variable_list ASSIGN expr end
		{
		  emit_xref_assignment($2);
		}
|	opt_actions NAME ':' variable_list ASSIGN expr end
		{
		  emit_xref_assignment($4);
		}
|	opt_actions variable_list dyadic_operator ASSIGN expr end
		{
		  emit_xref_assignment($2);
		}
|	opt_actions NAME ':' variable_list dyadic_operator ASSIGN expr end
		{
		  emit_xref_assignment($4);
		}
|	/* empty */
;

body:
	body definition
|	body NAME ':' PROC { set_proc_name($2.name); } procedure_definition
		{
		  emit_procedure(&$2, $6);
		}
|	body NAME ':' PROCESS { set_process_name($2.name); } process_definition
		{
		  emit_process(&$2, $6);
		}
|	body { emit = 1; } action { emit = 0; }
|	body NAME ':' { emit = 1; } action { emit = 0; }
|	body NAME '(' args ')' end
		{
		  emit_xref_procedure($2.name, $2.line);
		}
|	body NAME ':' NAME '(' args ')' end
		{
		  emit_xref_procedure($4.name, $4.line);
		}
|	body variable_list ASSIGN { emit = 1; } expr { emit = 0; } end
		{
		  emit_xref_assignment($2);
		}
|	body NAME ':' variable_list ASSIGN expr end
		{
		  emit_xref_assignment($4);
		}
|	body variable_list dyadic_operator ASSIGN expr end
		{
		  emit_xref_assignment($2);
		}
|	body NAME ':' variable_list dyadic_operator ASSIGN expr end
		{
		  emit_xref_assignment($4);
		}
|	/* empty */
;
		  
definition:
	DCL declaration_stmt ';'
|	GRANT grant_stmt ';'
|	NEWMODE mode_definition_stmt ';'
|	SEIZE seize_stmt ';'
|	SIGNAL signal_definition_stmt ';'
|	SYN synonym_definition_stmt ';'
|	SYNMODE mode_definition_stmt ';'
;

synonym_definition:
	def_occ_list mode '=' expr
		{
		  emit_synonyms($1, $2.text);
		}
|	def_occ_list '=' expr
		{
		  emit_synonyms($1, "unknown");
		}
|	def_occ_list '=' NAME expr
		{
		  emit_synonyms($1, "unknown");
		}
;

synonym_definition_stmt:
	synonym_definition
|	synonym_definition_stmt ',' synonym_definition
;

declaration_stmt:
	declaration
|	declaration_stmt ',' declaration
;

declaration:
	def_occ_list mode opt_based opt_loc opt_init opt_assign opt_handler
		{
		  /* We have a bunch of names and their mode. */
		  if (local)
		  {
		    remember_locals($1);
		  }
		  emit_declarations($1, $2.text);
		}
|	def_occ_list mode STATIC opt_based opt_loc opt_init opt_assign opt_handler
		{
		  /* Likewise, but take qualifiers into account. */
		  if (local)
		  {
		    remember_locals($1);
		  }
		  emit_declarations($1, $2.text);
		}
;

variable_list:
	variable
                {
                  $$ = ckalloc(sizeof(struct identifierlist));
                  memcpy($$, &$1, sizeof(struct identifier));
                  $$->next = NULL;
                }
|	variable_list ',' variable
                {
                  $$ = ckalloc(sizeof(struct identifierlist));
                  memcpy($$, &$3, sizeof(struct identifier));
                  $$->next = $1;
                }
;

opt_assign:
	ASSIGN { emit = 1; } expr { emit = 0; }
|	/* empty */
;

opt_based:
	BASED '(' name_string ')'
|	/* empty */
;

opt_loc:
	LOC
		{
		  $$ = $1;
		}
|	/* empty */
		{
		  $$.text = SN_StrDup("");
		}
;

opt_init:
	INIT
|	/* empty */
;

on_alternatives:
	on_alternatives on_exception_list
|	on_alternatives action
|	/* empty */
;

iteration:
	variable_list ASSIGN expr TO expr
		{
		  emit_xref_assignment($1);
		}
|	variable_list ASSIGN expr BY expr TO expr
		{
		  emit_xref_assignment($1);
		}
|	variable_list ASSIGN expr DOWN TO expr
		{
		  emit_xref_assignment($1);
		}		
|	variable_list ASSIGN expr BY expr DOWN TO expr
		{
		  emit_xref_assignment($1);
		}
|	variable_list IN expr
		{ }
|	variable_list DOWN IN expr
		{ }
;

case_expr:
	CASE expr_list OF case_way_list ESAC
		{ }
|	CASE expr_list OF case_way_list ELSE expr ESAC
		{ }
;

case_way_list:
	case_way
		{ }
|	case_way_list case_way
		{ }
;

case_way:
	'(' case_label_specification ':' expr ';'
;

case_label_list:
	'(' '*' ')'
|	'(' case_label_list2 ')'
;

case_label_list2:
	case_label
|	case_label_list2 ',' case_label
;

case_label_specification:
	case_label_list
|	case_label_specification ',' case_label_list
;

case_label:
	ELSE
|	expr
|	expr ':' expr
;

do_action:
	DO opt_actions OD
|	DO FOR EVER ';' opt_actions OD
|	DO FOR iteration_list ';' opt_actions OD
|	DO FOR iteration_list WHILE expr ';' opt_actions OD
|	DO WHILE expr ';' opt_actions OD
|	DO WITH { activate_with(); } expr ';' opt_actions { leave_with(); } OD
;

variant_alternative:
	fixed_field
|	':' fixed_field
|	case_label_specification ':' fixed_field
;

index_mode_list:
	index_mode
		{ }
|	index_mode_list ',' index_mode
		{ }
;

index_mode:
	NAME '(' expr ':' expr ')'
		{ }
|       mode
		{ $$ = $1; }
|	expr ':' expr
		{ }
;

iteration_list:
	iteration
		{ }
|	iteration_list ',' iteration
		{ }
;

opt_forbid:
	FORBID		/* no names, as per GNU CHIILL */
|	FORBID ALL
|	/* empty */
;

postfix_list:
	postfix
|	postfix_list ',' postfix
;

postfix:
	opt_name_string opt_forbid
		{
		  /* forbid for GRANT only, but we'll relax it */
		}
;

mode_definition_stmt:
	mode_definition
|	mode_definition_stmt ',' mode_definition
;

mode_definition:
	def_occ_list '=' mode
		{
		  emit_type_synonyms($1, $3.text);
		}
;

procedure_definition:
	'(' param_list ')' opt_result_spec opt_except opt_procedureattr opt_semicolon proc_body
		{
		  unsigned length = 0;
		  struct identifierlist * idlist = $2;
		  while (idlist != NULL)
		  {
		    length += strlen(idlist->name);
		    if (idlist->next != NULL)
		    {
		      length += 2; /* ", " */
		    }
		    idlist = idlist->next;
		  }
		  $$.args = ckalloc((length + 1)*sizeof(char));
		  memset ($$.args, 0, (length + 1)*sizeof(char));
		  idlist = $2;
		  while (idlist != NULL)
		  {
		    strcat($$.args, idlist->name);
		    if (idlist->next != NULL) {
		      strcat($$.args, ", ");
		    }
		    idlist = idlist->next;
		  }
		  $$.rettype = $4.text;
		}
		
|	'(' ')' opt_result_spec opt_except opt_procedureattr opt_semicolon proc_body
		{
		  $$.rettype = $3.text;
		  $$.args = NULL;
		}
;

opt_except:
	EXCEPTIONS '(' ')'
|	EXCEPTIONS '(' name_string_list ')'
|	/* empty */
;

name_string_list:
	name_string
		{ }
|	name_string_list ',' name_string
		{ }
;

opt_name_string:
	name_string
		{ }
|	/* empty */
		{ $$.name = SN_StrDup(""); }
;

name_string:
	NAME '!' opt_name_string 
		{
		  $$ = $1;
		}
|	NAME
		{
		  $$ = $1;
		}
|	ALL
		{ }
;

pos:
	'(' expr ')'
|	'(' expr ',' expr ')'
|	'(' expr ',' expr ',' expr ')'
|	'(' expr ',' expr ':' expr ')'
;

step:
	'(' POS pos ')'
|	'(' POS pos ',' expr ')'
;

proc_body:
	{ local = 1; } body { local = 0; } END end
;

process_definition:
	'(' param_list ')' { local = 1; } opt_semicolon proc_body
		{
		  $$.rettype = NULL;
		  $$.args = NULL; /* for now */
		  local = 0;
		}
|	'(' ')' { local = 1; } opt_semicolon proc_body
		{
		  $$.rettype = NULL;
		  $$.args = NULL; /* for now */
		  local = 0;
		}
;

param_list:
	param
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  $$->name = $1.text;
		  $$->next = NULL;
		}
|	param_list ',' param
		{
		  $$ = ckalloc(sizeof(struct identifierlist));
		  $$->name = $3.text;
		  $$->next = $1;
		}
;

param:
	def_occ_list mode param_attr
		{
		  struct identifierlist * idlist = $1;
		  $$.text = NULL;

		  /* Emit the actual local variable declarations produced
		     by these formal parameters while we have context. */
		  
		  emit_declarations($1, $2.text);
		  remember_locals($1);

		  while (idlist != NULL)
		  {
		    unsigned length = strlen(idlist->name) + 1 + strlen($2.text) + 1 + strlen($3.text) + 2 /* ", " */;
		    if ($$.text == NULL)
		    {
		      $$.text = ckalloc(length + 1 /*NUL*/);
		      $$.text[0] = 0;
		    }
		    else
		    {
		      $$.text = ckrealloc($$.text, strlen($$.text) + length);
		    } 
			   
		    /* Now populate the string. */
		    strcat($$.text, idlist->name);
		    strcat($$.text, " ");
		    strcat($$.text, $2.text);
		    if (strcmp($3.text, "") != 0)
		    {
		  	strcat($$.text, " ");
		        strcat($$.text, $3.text);
		    }
		    strcat($$.text, ", ");
		    if (idlist->next == NULL)
		    {
		      $$.text[strlen($$.text) - 2] = 0;
		    }
		    idlist = idlist->next;
		  }
		} 
;

mode:
	ACCESS
		{
		  $$ = $1;
		}
|	ACCESS mode DYNAMIC
		{
		  $$.text = strjoin($1.text, strjoin($2.text, $3.text));
		}
|	ACCESS mode
		{
		  $$.text = strjoin($1.text, $2.text);
		}
|	ACCESS '(' index_mode ')'
		{
		  $$.text = (char *) ckalloc(strlen($1.text) + strlen($3.text) + 3); /* (,) and a NULL. */
		  sprintf($$.text, "%s (%s)", $1.text, $3.text);
		}
|	ACCESS '(' index_mode ')' mode
		{
		  $$.text = (char *) ckalloc(strlen($1.text) + strlen($3.text) + strlen($5.text) + 3);
		  sprintf($$.text, "%s (%s) %s", $1.text, $3.text, $5.text);
		}
|	ACCESS '(' index_mode ')' mode DYNAMIC
		{
		  $$.text = (char *) ckalloc(strlen($1.text) + strlen($3.text) + strlen($5.text) + 3);
		  sprintf($$.text, "%s (%s) %s", $1.text, $3.text, $5.text);
		}
|	ARRAY '(' index_mode_list ')' opt_varying mode opt_layout
		{
		  /* $$.text = strjoin($1.text, strjoin(brackets, $5.text)); */
		}
|	BIN '(' expr ')'
		{
		  $$ = $1;
		}
|	BOOLS '(' expr ')' opt_varying
		{	
		  $$.text = strjoin($1.text, brackets);
		}
|	BUFFER '(' expr ')' mode
		{
		  $$.text = strjoin($1.text, $5.text);
		}
|	BUFFER mode	
		{	
		  $$.text = strjoin($1.text, $2.text);
		}
|	CHARS '(' expr ')' opt_varying
		{
		  $$.text = strjoin($1.text, brackets);
		}
|	EVENT
		{
		  $$ = $1;
		}
|	EVENT '(' expr ')'
		{	
		  $$ = $1;
		}
|	mode '(' expr ':' expr ')'
		{
		  $$.text = strjoin($1.text, brackets);
		}
|	mode '(' expr ')' opt_varying
		{
		  $$.text = strjoin($1.text, brackets);
		}
|	NAME
		{
		  $$.text = $1.name;
		}
|	POWERSET mode
		{
		  $$.text = strjoin($1.text, $2.text);
		}
|	procedure_mode
		{
		  $$ = $1;
		}
|	RANGE '(' expr ':' expr ')'
		{
		  $$ = $1;
		}
|	READ mode
		{
		  $$.text = strjoin($1.text, $2.text);
		}
|	REF mode
		{
		  $$.text = strjoin($1.text, $2.text);
		}
|	ROW mode
		{
		  $$.text = strjoin($1.text, $2.text);
		}
|	SET '(' set_list ')'
		{
		  $$ = $1; emit_enumeration(); emit_enumeration_values($3);
		}
|	structure_mode
		{
		  $$ = $1;
		}
|	TEXT '(' expr ')'
		{
		  $$ = $1;
		}
|	TEXT '(' expr ')' DYNAMIC
		{
		  $$.text = strjoin($1.text, $5.text);
		}
|	TEXT '(' expr ')' mode
		{
		  $$.text = strjoin($1.text, $5.text);
		}
|	TEXT '(' expr ')' mode DYNAMIC
		{
		  $$.text = strjoin($1.text, strjoin($5.text, $6.text));
		}
;

param_attr:
	IN
		{
	  	  $$ = $1;
		}
|	OUT
		{
		  $$ =$1;
		}
|	INOUT
		{
		  $$ = $1;
		}
|	LOC
		{
		  $$ = $1;
		}
|	DYNAMIC
		{
		  $$ = $1;
		}
|	/* empty */
		{
		  $$.text = SN_StrDup("");
		}
;

opt_semicolon:
	';'
		{ }
|	/* empty */
;

%%

