/*
 * parser.y
 *
 * Copyright (C) 1998 Cygnus Solutions
 *
 * Description:
 * A parser for Microsoft Visual Basic (blech).
 */

%{
#include "emit.h"
%}

%union {
	char * text;
}

%token ALIAS AS BYREF BYVAL CASE CLOSE CONST DECLARE DIM DO ELSE ERROR END EXIT
%token FUNCTION GLOBAL IF INPUT FOR LET LIB LOOP NEXT NUMBER ON PRIVATE PUBLIC 
%token SELECT STEP STRING STRINGLITERAL SUB THEN TO TYPE UNTIL WHILE
%token NOTEQ

%token <text> NAME

%type <text> varname type

%%

program:
	program declaration
|	program function
|	program sub
|	program record
|	program global
|	/* empty */
;

global:
	GLOBAL varlist
		{
		  emit_variable("foo", "integer");
		}
; 

args:
	arglist
|	/* empty */
;

arglist:
	convention NAME
	convention NAME AS type
|	arglist ',' convention NAME
|	arglist ',' convention NAME AS type
;

convention:
	BYREF
|	BYVAL
|	/* empty */
;

record:
	access TYPE NAME field_list END TYPE
		{
		  printf("got a typedef\n");
		}
;

field_list:
	field_list NAME AS type
|	/* empty */
; 

type:
	NAME
|	STRING '*' NUMBER { }
;

declaration:
	DECLARE FUNCTION NAME options '(' args ')' AS type
|	DECLARE SUB NAME  options '(' args ')'
;

options:
	options LIB STRING
|	options ALIAS STRING
|	/* empty */
;

function:
	access FUNCTION NAME '(' args ')' AS type stmts END FUNCTION
		{
			emit_function($3);
		}
;

sub:
	access SUB NAME '(' args ')' stmts END SUB
		{
			emit_subprogram($3);
		}
;

access:
	PUBLIC
|	PRIVATE
;

stmts:
	stmts stmt
|	/* empty */
;

stmt:
	constdecl
|	DIM varlist
|	assignment
|	doloop
|	forloop
|	ifblock
|	caseblock
|	EXIT FUNCTION
|	CLOSE NAME
;

constdecl:
	CONST NAME '=' STRINGLITERAL
|	CONST NAME '=' NUMBER
;

varname:
	NAME
|	NAME '(' expr TO expr ')'
|	NAME '(' expr ')'
;

varlist:
	varname { }				/* Type is inferred. */
|	varname AS type { }
|	varlist ',' varname { } 		/* Type is inferred. */
|	varlist ',' varname AS type { }
;

assignment:
	NAME '=' expr { }
|	NAME '=' STRINGLITERAL { }
|	LET NAME '=' expr
|	LET NAME '=' STRINGLITERAL
|	LET NAME '(' expr ')' '=' expr		/* Array reference. */
|	LET NAME '(' expr ')' '=' STRINGLITERAL	/* Likewise. */
;

forloop:
	FOR NAME '=' expr TO expr stmts NEXT NAME
|	FOR NAME '=' expr TO expr STEP expr stmts NEXT NAME
;

doloop:
	DO WHILE expr stmts LOOP
|	DO stmts LOOP UNTIL expr
;

ifblock:
	IF expr THEN stmts END IF
|	IF expr THEN stmts ELSE stmts END IF
;

caseblock:
	SELECT CASE expr cases END SELECT
;

cases:
	cases CASE exprlist stmts
|	cases CASE ELSE stmts
|	/* empty */
;

exprlist:
	expr
|	exprlist ',' expr
;

expr:
	NAME { }
|	NAME '.' fieldref { }
|	NAME '(' exprlist ')' { }
|	NUMBER
;

fieldref:
	NAME { }
|	fieldref '.' NAME
;

%%
