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

/* exprtype.c -- propagates datatype thru expressions.

    Copyright (C) 1992 by Robert K. Moniot.
    This program is free software.  Permission is granted to
    modify it and/or redistribute it, retaining this notice.
    No guarantees accompany this software.


*/

/* I. */

/*  exprtype.c:

	Routines to propagate datatype through expressions.

	binexpr_type()		Yields result type of binary expression.
	unexpr_type()		Yields result type of unary expression.
	assignment_stmt_type()	Checks assignment statement type.
	func_ref_expr(id,args,result) Forms token for a function invocation.
	primary_id_expr()	Forms token for primary which is an identifier.
	stmt_fun_arg_cmp(t1,t2) Checks agreement between stmt func args.
    int	int_power(x,n)		Computes x**n for value propagation.
        init_typesizes(wdsize)	Sets standard type sizes
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "ftnchek.h"
#define EXPRTYPE
#include "symtab.h"
#include "fortran.h"

PRIVATE int int_power();
PRIVATE char *sized_typename(),*op_string();
PRIVATE void report_mismatch(),report_type();

extern int in_assignment_stmt;	/* shared with fortran.y */

#define max(x,y) ((y)>(x)?(y):(x))

	/* shorthand for datatypes.  must match those in symtab.h */
	/* N.B. Also, the fact that type_DEFAULT=0 is assumed in size
	   propagation code. */
#define E 0	/*  Error for invalid type combos  */
#define I 1
#define R 2
#define D 3
#define C 4
#define Z 5
#define L 6
#define S 7
#define H 8
#define NumT (H+1)		/* number of types in tables below */

#define W 10		/*  Warning for nonstandard type combos: W>NumT */

			/* for  + - / * **	ANSI book pp. 6-5,6-6	*/
			    /* Mixed double+complex = double complex with
			       warning, double + double complex is OK */
unsigned char arith_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  R,  D,  C,  Z,  E,  E,  E },	/* I */
{ E,  R,  R,  D,  C,  Z,  E,  E,  E },	/* R */
{ E,  D,  D,  D,W+Z,  Z,  E,  E,  E },	/* D */
{ E,  C,  C,W+Z,  C,  Z,  E,  E,  E },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H */
};

			/* for  relops.  Corresponds to arith type table
			   except that nonstandard comparisons of like
			   types have warning, not error. */
unsigned char rel_expr_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  L,  L,  L,  L,  L,  E,  E,W+L },	/* I */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* R */
{ E,  L,  L,  L,W+L,  L,  E,  E,  E },	/* D */
{ E,  L,  L,W+L,  L,  L,  E,  E,  E },	/* C */
{ E,  L,  L,  L,  L,  L,  E,  E,  E },	/* Z */
{ E,  E,  E,  E,  E,  E,W+L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  L,  E },	/* S */
{ E,W+L,  E,  E,  E,  E,W+L,  E,W+L }	/* H */
};

			/* Result of assignment:  lvalue = expr.  Here rows
			   correspond to type of lvalue, columns to type
			   of expr */
unsigned char assignment_type[NumT][NumT]={
/*E   I   R   D   C   Z   L   S   H   */
{ E,  E,  E,  E,  E,  E,  E,  E,  E },	/* E */
{ E,  I,  I,  I,  I,  I,  E,  E,W+I },	/* I */
{ E,  R,  R,  R,  R,  R,  E,  E,W+R },	/* R */
{ E,  D,  D,  D,  D,  D,  E,  E,W+D },	/* D */
{ E,  C,  C,  C,  C,  C,  E,  E,W+C },	/* C */
{ E,  Z,  Z,  Z,  Z,  Z,  E,  E,W+Z },	/* Z */
{ E,  E,  E,  E,  E,  E,  L,  E,W+L },	/* L */
{ E,  E,  E,  E,  E,  E,  E,  S,  E },	/* S */
{ E,  E,  E,  E,  E,  E,  E,  E,  E }	/* H not possible for lvalue */
};


#define INTRINS_ARGS (op == ',') /* Flag to modify behavior of binexpr_type */

	/* Routine used in printing diagnostics: returns string "type" for
	   unsized objects, "type*size" for explicitly sized things.  Due
	   to use of local static variable, cannot be invoked twice in the
	   same expression.  */
PRIVATE char*
sized_typename(type,size)
  int type; long size;
{
  static char strbuf[]="type*000000"; /* template */
  static char *char_unk="char*(?)";
  static char *char_adj="char*(*)";
  if(size == size_DEFAULT) {
    return type_name[type];	/* no explicit size */
  }
  else {
    if(type != S || size > 0) {
      sprintf(strbuf,"%4s*%ld",	/* type*size */
	    type_name[type],
	    size%1000000);
    }
    else {			/* handle special character size codes */
      if(size == size_ADJUSTABLE)
	return char_adj;
      else /*size_UNKNOWN*/
	return char_unk;
    }
  }
  return strbuf;
}

PRIVATE char*			/* Returns string containing the operator */
op_string(op_token)
     Token *op_token;
{
  int op=op_token->class;
  static char op_str[]=".AND.";	/* Longest instance */
  if(ispunct(op)) {
    op_str[0] = op;
    op_str[1] = '\0';
    return op_str;
  }
  else switch(op) {
    case tok_power:
      return "**";
      break;

    case tok_concat:
      return "//";
      break;

    case tok_relop:
    case tok_AND:
    case tok_OR:
    case tok_EQV:
    case tok_NEQV:
    case tok_NOT:
      sprintf(op_str,".%s.",op_token->value.string);
      return op_str;
      break;

    default:
      return "?";
      break;
    }
  return NULL;/*NOTREACHED*/
}

void
init_typesizes()
		/* Only executes once.  Thus cannot change wordsize
		   after processing starts. */
{
  static int trapdoor=FALSE;
  if(trapdoor) {
    if(given_wordsize != local_wordsize) {
      fprintf(stderr,
	      "\nSorry-Cannot change wordsize after processing starts");
    }
    given_wordsize = local_wordsize;
  }
  else {
    trapdoor = TRUE;
    local_wordsize = given_wordsize;
    if(given_wordsize != 0) {
      if(given_wordsize != BpW) {
	type_size[I] = type_size[R] = type_size[L] = given_wordsize;
	type_size[D] = type_size[C] = 2*given_wordsize;
	type_size[Z] = 4*given_wordsize;
      }
    }
  }
}

	/* this routine propagates type in binary expressions */

void
binexpr_type(term1,operator,term2,result)
	Token *term1, *operator, *term2, *result;
{
    int	op = operator->class,
	type1 = datatype_of(term1->class),
	type2 = datatype_of(term2->class),
	result_type;
    long
	size1 = term1->size,
	size2 = term2->size,
        result_size;
    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"noncomputational primary in expression:");
		report_type(term1);
		result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
		syntax_error(term2->line_num,term2->col_num,
			"noncomputational primary in expression:");
		report_type(term2);
		result_type = E;
    }
    else {
	switch(op) {
				/* arithmetic operators: use lookup table */
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    case tok_power:
		result_type = (unsigned)arith_expr_type[type1][type2];
		break;

				/* relational operators: use lookup table */
 	    case tok_relop:
		result_type = (unsigned)rel_expr_type[type1][type2];
		break;

				/*  logical operators: operands should be
				    logical, but allow integers with a
				    warning. */
	    case tok_AND:
	    case tok_OR:
	    case tok_EQV:
	    case tok_NEQV:
		if(type1 == L && type2 == L)
		    result_type = L;
		else if(type1 == I && type2 == I)
		    result_type = W+I;
		else
		    result_type = E;
		break;

				/*  // operator: operands must be strings */
	    case tok_concat:
		if(type1 == S && type2 == S)
		    result_type = S;
		else
		    result_type = E;
		break;

			/* Intrinsic function argument list: no promotion
			   across type categories.  Accept matching type
			   categories: size match will be checked later. */
	    case ',':
		if( type_category[type1] != type_category[type2] )
		  result_type = E;
		else if(type1 == S)
		  result_type = S;
		else
		  result_type = (unsigned)arith_expr_type[type1][type2];
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     operator->line_num,operator->col_num,
			     "operator unknown: type not propagated");
		result_type = type1;
		break;
	}

	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
		syntax_error(operator->line_num,operator->col_num,
			"type mismatch");
		if(INTRINS_ARGS) {
		  msg_tail("between intrinsic function arguments:");
		}
		else {
		  msg_tail("in expression:");
		}
		report_mismatch(term1,operator,term2);
	    }
	    else if(result_type >= W) {	/* W result */
	      if(f77_standard) {
		warning(operator->line_num,operator->col_num,
			"nonstandard type combination in expression:");
		report_mismatch(term1,operator,term2);
	      }
	      result_type -= W;
	    }
				/* Obscure standard rule */
	    else if(f77_standard && op == tok_concat && !in_assignment_stmt &&
		  (size1 == size_ADJUSTABLE || size2 == size_ADJUSTABLE) ) {
		nonstandard(operator->line_num,operator->col_num);
		msg_tail("adjustable size cannot be concatenated here");
	    }
	}
    }

				/* Figure out the size of result */
    result_size = size_DEFAULT;
    if(result_type != E &&	/* Error type gets DEFAULT size */
       op != tok_relop) {	/* Result of compare gets DEFAULT size */

      if(op == tok_concat) {	/* string//string yields sum of lengths */
	if(size1 == size_UNKNOWN || size2 == size_UNKNOWN)
	  result_size = size_UNKNOWN;
	else
	  if(size1 == size_ADJUSTABLE || size2 == size_ADJUSTABLE)
	    result_size = size_ADJUSTABLE;
	  else
	    result_size = size1 + size2;
      }
			/* DEFAULT op DEFAULT always yields DEFAULT. So need
			   to handle only explicitly sized expressions,
			   except intrinsic arglists, where no promotion
			   of plain real to dble or plain complex to dcpx,
			   and check for promotions of real types.
			 */
      else if(INTRINS_ARGS?
	      (type1 != type2) :
	      ((size1 != size_DEFAULT || size2 != size_DEFAULT) ||
	        (trunc_check && is_float_type(type1) && is_float_type(type2))))
     {
				/* Local variables for convenience.
				   N.B. Use tc1/2,ls1/2 for tests,
				   t1/2,s1/2 for assigning result.
				 */
	int t1,t2;	/* sorted types: t1 <= t2. */
	long s1,s2;	/* sizes of t1 and t2. */
	int tc1,tc2;	/* type categories: D->R and Z->C */
	long ls1,ls2;	/* local sizes = declared size else type_size */
	int defsize1,defsize2; /* flags for default size */

				/* Sort so that t1 <= t2 */
	if(type1 <= type2) {
	  t1 = type1; s1 = size1;
	  t2 = type2; s2 = size2;
	}
	else {
	  t1 = type2; s1 = size2;
	  t2 = type1; s2 = size1;
	}
				/* Assign type categories and local sizes */
	tc1 = type_category[t1];
	tc2 = type_category[t2];

	defsize1 = (s1 == size_DEFAULT);
	defsize2 = (s2 == size_DEFAULT);
	ls1 = (defsize1? type_size[t1]: s1);
	ls2 = (defsize2? type_size[t2]: s2);

#ifdef DEBUG_EXPRTYPE
if(debug_latest)
  fprintf(list_fd,"\nt1=%s s1=%d ls1=%d t2=%s s2=%d ls2=%d",
	  type_name[t1],s1,ls1, type_name[t2], s2, ls2);
#endif
	if(tc1 == tc2) {/* same type category */
				/* Intrins args: size promotion illegal */
	  if(INTRINS_ARGS && ls1 != ls2) {
	    syntax_error(operator->line_num,operator->col_num,
			 "precision mismatch in intrinsic argument list:");
	    report_mismatch(term1,operator,term2);
	  }
				/* Give -port warning if e.g. plain I+I*2
				   (variables only) */
	  else if(port_check || local_wordsize==0) {
	    if(defsize1 != defsize2
	        && !is_true(CONST_EXPR,term1->subclass)
	        && !is_true(CONST_EXPR,term2->subclass))
	    {
	      nonportable(operator->line_num,operator->col_num,
			  INTRINS_ARGS?"intrinsic argument list":"expr");
	      msg_tail("mixes default and explicit");
	      msg_tail((is_numeric_type(t1)&&is_numeric_type(t2))?
			 "precision":"size");
	      msg_tail("operands:");
	      report_mismatch(term1,operator,term2);
	    }
	  }

		/* If same type category, use the larger of the two sizes if
		   both declared.  If only one size declared, use the
		   larger of the declared size and the default size.
		   If result is equal in size to default, use size_DEFAULT.
		*/
	  if(ls1 > ls2) {
	    result_size = s1;
	  }
	  else if(ls2 > ls1) {
	    result_size = s2;
	  }
	  else /*ls1 == ls2*/{
	    if(!defsize1 && !defsize2)
	      result_size = s1;	/* otherwise DEFAULT */
	  }
	}/* end(tc1==tc2) */
	else /* tc1!=tc2 */ {
			/* Differing type categories: only two cases. */

				/* Case 1:  I + R|D|C|Z
				   Result: size of dominant type */
	  if(tc1 == I) {
	    result_size = s2;
	  }
				/* Case 2:  R|D + C|Z
				   Result: larger of C|Z and 2*size of R|D */
	  else {
	    if(ls2 >= 2*ls1)
	      result_size = s2;
	    else
	      result_size = 2*s1; /* 2*size_DEFAULT = 0 is still DEFAULT */
	  }
	}/* end tc1 != tc2 */
				/* change D or Z to default size or else
				   to explicitly sized R or C
				 */
	if(result_type == D || result_type == Z) {
	  if(result_size != size_DEFAULT
	     && result_size != type_size[result_type])
	       result_type = (result_type==D)?R:C;
	     else
	       result_size = size_DEFAULT;
	}

				/* Give -trunc warning if a real or
				   complex type is promoted to double. */
	if(trunc_check && !INTRINS_ARGS && is_float_type(t1) ) {
		  /* First clause checks R+R size agreement */
	  if( (type_category[result_type] == R && ls1 != ls2)
		     /* Second clause checks R+C and C+C */
	     || (type_category[result_type] == C &&
		 (type_category[t1] == R? ls2 != 2*ls1 : ls2 != ls1)) ){
	    warning(operator->line_num,operator->col_num,
		    "promotion may not give desired precision:");
	    report_mismatch(term1,operator,term2);
	  }
	}

      }/*end if(non-DEFAULT sizes)*/

    }/*end if(result_size != E)*/

#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
fprintf(list_fd,"\nsize of %s %c",sized_typename(type1,size1),
	ispunct(op)?op:'~');
fprintf(list_fd," %s = ",sized_typename(type2,size2));
fprintf(list_fd,"%s",sized_typename(result_type,result_size));
}
#endif

    result->class = type_byte(class_VAR, result_type);
    result->subclass = 0;	/* clear all flags */
    result->size = result_size;


		/* Keep track of constant expressions */
    if( is_true(CONST_EXPR,term1->subclass)
	 && is_true(CONST_EXPR,term2->subclass)
         && !(op==tok_power && type2!=I) ) { /* exclude **REAL */
		make_true(CONST_EXPR,result->subclass);
    }

		/* Parameter expressions are like constant exprs
		   except we bend the rules to allow intrinsic functions
		   and **REAL */
    if( is_true(PARAMETER_EXPR,term1->subclass)
	 && is_true(PARAMETER_EXPR,term2->subclass) ) {
		make_true(PARAMETER_EXPR,result->subclass);
    }

    if( is_true(EVALUATED_EXPR,term1->subclass)
	 && is_true(EVALUATED_EXPR,term2->subclass) ) {
		make_true(EVALUATED_EXPR,result->subclass);
    }
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
fprintf(list_fd,"\nconst param eval: (%d %d %d) %s (%d %d %d) = (%d %d %d)",
is_true(CONST_EXPR,term1->subclass),
is_true(PARAMETER_EXPR,term1->subclass),
is_true(EVALUATED_EXPR,term1->subclass),

op_string(op),

is_true(CONST_EXPR,term2->subclass),
is_true(PARAMETER_EXPR,term2->subclass),
is_true(EVALUATED_EXPR,term2->subclass),

is_true(CONST_EXPR,result->subclass),
is_true(PARAMETER_EXPR,result->subclass),
is_true(EVALUATED_EXPR,result->subclass));
#endif

  if(! INTRINS_ARGS) {		/* Remaining steps only applicable to exprs */

		/* Remember if integer division was used */
    if(result_type == type_INTEGER &&
	   (op == '/' ||
	    (is_true(INT_QUOTIENT_EXPR,term1->subclass) ||
	     is_true(INT_QUOTIENT_EXPR,term2->subclass))) ) {
		make_true(INT_QUOTIENT_EXPR,result->subclass);
    }
		/* Issue warning if integer expr involving division is
		   later converted to any real type, or if it is used
		   as an exponent. */
    if( is_true(INT_QUOTIENT_EXPR,term1->subclass)
	|| is_true(INT_QUOTIENT_EXPR,term2->subclass) ) {

	int r=result_type;
	if(r == type_LOGICAL)		/* relational tests are equivalent */
	    r = arith_expr_type[type1][type2];		/* to subtraction */

	if(op == tok_power && is_true(INT_QUOTIENT_EXPR,term2->subclass) ) {
	  if(trunc_check)
	    warning(operator->line_num,operator->col_num,
			"integer quotient expr used in exponent");
	  if( ! is_true(INT_QUOTIENT_EXPR,term1->subclass) )
		make_false(INT_QUOTIENT_EXPR,result->subclass);
	}
	else if( r == type_REAL || r == type_DP || r == type_COMPLEX) {
	  if(trunc_check)
	    warning(operator->line_num,operator->col_num,
	    		"integer quotient expr converted to real");
	}
    }

			/* If either term is an identifier, set use flag */
    if(is_true(ID_EXPR,term1->subclass))
	use_variable(term1);
    if(is_true(ID_EXPR,term2->subclass))
	use_variable(term2);

		/* Propagate the value of integer constant expressions */
    if(is_true(EVALUATED_EXPR,result->subclass)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      b = int_expr_value(term2),
	      c;
	  switch(op) {
	    case '+': c = a+b; break;
	    case '-': c = a-b; break;
	    case '*': c = a*b; break;
	    case '/': if(b == 0) {
			syntax_error(term2->line_num,term2->col_num,
				"division by zero attempted");
			c = 0;
		      }
		      else {
			c = a/b;
		      }
		      break;
	    case tok_power: c = int_power(a,b); break;
	    case tok_AND: c = a&b; break;
	    case tok_OR: c = a|b; break;
	    case tok_EQV: c = ~(a^b); break;
	    case tok_NEQV: c = a^b; break;
	    default:
	      oops_message(OOPS_NONFATAL,
			   operator->line_num,operator->col_num,
			   "invalid int expr operator");
			c = 0; break;
	  }

	  make_true(EVALUATED_EXPR,result->subclass);
	  result->value.integer = c;	/* Result goes into token value */

				/* Integer division (including i**neg)
				   that yields 0 is suspicious.  */
	  if(trunc_check)
	    if(c==0 && (op=='/' || op==tok_power)) {
	      warning(operator->line_num,operator->col_num,
	    		"integer const expr yields result of 0");
	    }
	}
      }
				/* Also nonconstant**neg is 0 unless
				   nonconstant=1 */
      else if(trunc_check)
	if(result_type == type_INTEGER && op == tok_power
	      && is_true(EVALUATED_EXPR,term2->subclass)
	      && int_expr_value(term2) < 0) {
	  warning(operator->line_num,operator->col_num,
		  "integer to negative power usually yields 0");
	}
  }/* end if !INTRINS_ARGS */
}/*binexpr_type*/


	/* this routine propagates type in unary expressions */

void
unexpr_type(operator,term1,result)
	Token *term1, *operator, *result;
{
   int	op = operator->class,
	type1 = datatype_of(term1->class),
	result_type;

    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"noncomputational primary in expression:");
		report_type(term1);
		result_type = E;
    }
    else {
	switch(op) {
			/* arith operators: use diagonal of lookup table */
	    case '+':
	    case '-':
		result_type = arith_expr_type[type1][type1];
		break;

				/*  NOT: operand should be
				    logical, but allow integers with a
				    warning. */
	    case tok_NOT:
		if(type1 == L)
		    result_type = L;
		else if(type1 == I)
		    result_type = W+I;
		else
		    result_type = E;
		break;

	    default:
		oops_message(OOPS_NONFATAL,
			     operator->line_num,operator->col_num,
			     "unary operator type not propagated");
		result_type = type1;
		break;
	}

	if( type1 != E )
	    if( result_type == E) {
		syntax_error(operator->line_num,operator->col_num,
			"expression incompatible with operator:");
		msg_tail(op_string(operator));
		msg_tail("used with");
		report_type(term1);
	    }
	    else if(result_type >= W) {
	      if(f77_standard) {
		warning(operator->line_num,operator->col_num,
			"nonstandard type used with operator:");
		msg_tail(op_string(operator));
		msg_tail("used with");
		report_type(term1);
	      }
	      result_type -= W;
	    }
    }

    result->class = type_byte(class_VAR, result_type);
    result->subclass = 0;	/* clear all flags */
    result->size = term1->size;	/* result is same size as operand */

		/* Keep track of constant expressions */
    copy_flag(CONST_EXPR,result->subclass,term1->subclass);
    copy_flag(PARAMETER_EXPR,result->subclass,term1->subclass);

		/* Remember if integer division was used */
    if(result_type == type_INTEGER)
	    copy_flag(INT_QUOTIENT_EXPR,result->subclass,term1->subclass);

    if(is_true(ID_EXPR,term1->subclass))
	use_variable(term1);

		/* Propagate the value of integer constant expressions */
    if(is_true(EVALUATED_EXPR,term1->subclass)) {
	if(result_type == type_INTEGER) {	/* Only ints propagated */
	  int a = int_expr_value(term1),
	      c;
	  switch(op) {
	    case '+': c = a; break;
	    case '-': c = -a; break;
	    case tok_NOT: c = ~a; break;
	    default: oops_message(OOPS_NONFATAL,
			     operator->line_num,operator->col_num,
			     "invalid int expr operator");
			c = 0; break;
	  }
	  make_true(EVALUATED_EXPR,result->subclass);
	  result->value.integer = c;	/* Result goes into token value */
	}
    }
}

	/* this routine checks type and size match in assignment statements
	   and in parameter assignments */

void
assignment_stmt_type(term1,equals,term2)
	Token *term1, *equals, *term2;
{
    int type1 = datatype_of(term1->class),
	type2 = datatype_of(term2->class),
	result_type;


    if( ! is_computational_type(type1) ) {
		syntax_error(term1->line_num,term1->col_num,
			"noncomputational primary in expression:");
		report_type(term1);
		result_type = E;
    }
    else if( ! is_computational_type(type2) ) {
		syntax_error(term2->line_num,term2->col_num,
			"noncomputational primary in expression:");
		report_type(term2);
		result_type = E;
    }
    else {
	result_type = (unsigned)assignment_type[type1][type2];


	if( (type1 != E && type2 != E) ) {
	    if( result_type == E) {
		syntax_error(equals->line_num,equals->col_num,
			"type mismatch:");
		report_type(term2);
		msg_tail("assigned to");
		report_type(term1);
	    }
	    else {
	      if(result_type >= W) {		/* W result */
		if(f77_standard) {
		  warning(equals->line_num,equals->col_num,
		     "nonstandard type combination:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
		result_type -= W;
	      }

			/* Watch for truncation to lower precision type */
	      if(trunc_check || port_check || local_wordsize==0) {
		long size1 = term1->size;
		long size2 = term2->size;
		int type_trunc=FALSE, /* flags for kind of truncation */
		    size_trunc=FALSE,
		    mixed_size=FALSE,
		    promotion=FALSE,
		    trunc_warn,mixed_warn;

		if(size1 == size_DEFAULT && size2 == size_DEFAULT) {
		  type_trunc = ( is_numeric_type(type1) &&
				 is_numeric_type(type2) &&
				(type1 < type2 ||
					/* C = D truncates precision of D */
				(type1 == C && type2 == D)) );

				/* Watch for promotions also */
		  if(type_category[type2] == R) {
		    if(type_category[type1] == R) /* R|D = R|D */
		      promotion = (type1 > type2);
		    else if(type_category[type1] == C) /* C|Z = R|D */
		      promotion = (type_size[type1] > 2*type_size[type2]);
		  }
		  else if(type_category[type2] == C) /* any = C|Z */
		    promotion = (type1 > type2);
		}
		else if(type1 == S) { /* character strings */
		  if(size1>0 && size2>0) /* ignore ADJUSTABLE and UNKNOWN */
		    size_trunc = size1 < size2;
		} else {
		  int tc1,tc2;/* type categories: D->R, Z->C, H->I */
		  int ls1,ls2;/* local sizes */

				/* Assign type categories and local sizes */
		  tc1 = type_category[type1];
		  tc2 = type_category[type2];
		  ls1 = size1; if(ls1 == size_DEFAULT)  ls1 = type_size[type1];
		  ls2 = size2; if(ls2 == size_DEFAULT)  ls2 = type_size[type2];

				/* type truncation: any numeric type category
				   to a lower category. */
		  type_trunc = ( /***is_numeric_type(type1) &&
				 is_numeric_type(type2) &&***/
				 tc1 < tc2 );

				/* size truncation: assigned to smaller
				   local size.  For C = R correct test is
				   Csize < 2*Rsize */
		  if(tc1 == C && tc2 == R) {
		    size_trunc = (ls1 < ls2*2);
		    promotion = (ls1 > ls2*2);
		  }
		  else {
		    size_trunc = (ls1 < ls2);
		    promotion = ((tc2 == R || tc2 == C) && (ls1 > ls2));
		  }
				/* mixed size: default size assigned to
				   declared size of like type category
				   or vice-versa. -port only, and superseded
				   by truncation warning if any. */
		  mixed_size = (tc1 == tc2) &&
			   (size1==size_DEFAULT ||
			   (size2==size_DEFAULT &&
			    !is_true(CONST_EXPR,term2->subclass)));

		}

			/* Under -trunc, report type truncation or size
			   truncation.  Say "possibly" if -nowordsize.
			   Also report promotions under -trunc.
			   If no truncation warning given and under -port,
			   report mixed assignment */
#ifdef DEBUG_EXPRTYPE
#define TorF(x) ((x)?"":"no")
if(debug_latest) {
fprintf(list_fd,"\nassign %s =",sized_typename(type1,size1));
fprintf(list_fd," %s : ",sized_typename(type2,size2));
fprintf(list_fd,"%s type %s size %s mixed",
	TorF(type_trunc),
	TorF(size_trunc),
	TorF(mixed_size));
}
#endif
		trunc_warn = (trunc_check &&
			      (type_trunc || size_trunc || promotion));
		mixed_warn = ((port_check || local_wordsize==0) && mixed_size);
		if( trunc_warn ) {
		  warning(equals->line_num,equals->col_num,"");
		  report_type(term2);
		  if(trunc_warn && !type_trunc && mixed_size
		       && local_wordsize == 0)
		    msg_tail("possibly");
		  if(promotion)
		    msg_tail("promoted to");
		  else
		    msg_tail("truncated to");
		  report_type(term1);
		  if(promotion)
		    msg_tail(": may not give desired precision");
		}
		else if(mixed_warn) {
		  nonportable(equals->line_num,equals->col_num,
		    "mixed default and explicit");
		  msg_tail((is_numeric_type(type1)&&is_numeric_type(type2))?
			 "precision":"size");
		  msg_tail("items:");
		  report_type(term2);
		  msg_tail("assigned to");
		  report_type(term1);
		}
	      }
	    }/*end else (result_type != E)*/
	}/*end if (type1,type2 != E)*/
    }/*end else (is_computational_type(type2))*/


		/* Issue warning if integer expr involving division is
		   later converted to any real type. */
    if(trunc_check)
      if( is_true(INT_QUOTIENT_EXPR,term2->subclass) ) {

	int r=result_type;

	if( r == type_REAL || r == type_DP || r == type_COMPLEX)
	    warning(equals->line_num,equals->col_num,
			"integer quotient expr converted to real");
      }


    if(is_true(ID_EXPR,term2->subclass))
	use_variable(term2);

    use_lvalue(term1);
}

	/* Make an expression-token for a function invocation */

void
func_ref_expr(id,args,result)
	Token *id,*args,*result;
{
	Lsymtab *symt;
	IntrinsInfo *defn;
	int rettype, retsize;

	symt = hashtab[id->value.integer].loc_symtab;

	if( symt->intrinsic ) {
	    defn = symt->info.intrins_info;
			/* Intrinsic functions: type stored in info field */
	    rettype = defn->result_type;
	    retsize = size_DEFAULT;

		/* Generic Intrinsic functions: use propagated arg type */
	    if(rettype == type_GENERIC) {
		if(args->next_token == NULL) {
		  rettype = type_UNDECL;
		  retsize = size_DEFAULT;
		}
		else {
#ifdef OLDSTUFF
		  rettype = args->next_token->class;
		  retsize = args->next_token->size;
#else
		  rettype = args->class;
		  retsize = args->size;
#endif
		}
				/* special cases: ABS([d]complex) -> [d]real */
		if(rettype == type_COMPLEX && (defn->intrins_flags&I_C_TO_R)) {
			rettype = type_REAL;
			retsize = retsize/2;
		}
		if(rettype == type_DCOMPLEX &&(defn->intrins_flags&I_C_TO_R)) {
			rettype = type_DP;
			retsize = size_DEFAULT;
		}
	    }
	}
	else {
	    rettype = get_type(symt);
	    retsize = get_size(symt,rettype);
	}
		/* referencing function makes it no longer a class_SUBPROGRAM
		   but an expression. */
	result->class = type_byte(class_VAR,rettype);
	result->subclass = 0;	/* clear all flags */
	result->size = retsize;
#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
fprintf(list_fd,"\n%sFunction %s() = %s",
symt->intrinsic?"Intrinsic ":"",
symt->name,sized_typename(rettype,retsize));
}
#endif

		/* If intrinsic and all arguments are PARAMETER_EXPRs,
		   then result is one too. */
	if( symt->intrinsic ) {
				/* Evaluate intrinsic if result is
				   integer, the args are const (except for
				   LEN), and a handler is defined.
				 */
	    if(rettype == type_INTEGER &&
	           (defn->intrins_flags&I_EVALUATED) )
	    {
		     result->value.integer = eval_intrins(defn,args);
				/* Evaluation routines can affect the flags */
		     copy_flag(EVALUATED_EXPR,result->subclass,args->subclass);
	    }
	    copy_flag(PARAMETER_EXPR,result->subclass,args->subclass);
#ifdef DEBUG_EXPRTYPE
if(debug_latest) {
fprintf(list_fd,"\n%s(...) ",defn->name);
if(is_true(EVALUATED_EXPR,args->subclass))
  fprintf(list_fd,"=%d",result->value.integer);
else
  fprintf(list_fd,"not evaluated");
fprintf(list_fd,": const param eval=(%d %d %d)",
is_true(CONST_EXPR,result->subclass),
is_true(PARAMETER_EXPR,result->subclass),
is_true(EVALUATED_EXPR,result->subclass));
}
#endif
	}
}/*func_ref_expr*/



		/* Make an expression-token for primary consisting of
		   a symbolic name */

void
primary_id_expr(id,primary)
	Token *id,*primary;
{
	Lsymtab *symt;
	int id_type;
	symt = hashtab[id->value.integer].loc_symtab;
	id_type=get_type(symt);
	primary->class = type_byte(storage_class_of(symt->type),id_type);
	primary->subclass = 0;
	primary->size =get_size(symt,id_type);
	make_true(ID_EXPR,primary->subclass);

	if( storage_class_of(symt->type) == class_VAR) {
		if(symt->parameter) {
		    make_true(CONST_EXPR,primary->subclass);
		    make_true(PARAMETER_EXPR,primary->subclass);
		    make_true(EVALUATED_EXPR,primary->subclass);
		}
		else {
		    make_true(LVALUE_EXPR,primary->subclass);
		}
		if(symt->array_var)
		    make_true(ARRAY_ID_EXPR,primary->subclass);
		if(symt->set_flag || symt->common_var || symt->parameter
				  || symt->argument)
		    make_true(SET_FLAG,primary->subclass);
		if(symt->assigned_flag)
		    make_true(ASSIGNED_FLAG,primary->subclass);
		if(symt->used_before_set)
		    make_true(USED_BEFORE_SET,primary->subclass);
	}
	else if(storage_class_of(symt->type) == class_STMT_FUNCTION) {
		make_true(STMT_FUNCTION_EXPR,primary->subclass);
	}

#ifdef DEBUG_PARSER
if(debug_parser){
	fprintf(list_fd,"\nprimary %s: class=0x%x subclass=0x%x",
		symt->name,primary->class,primary->subclass);
      }
#endif
}/*primary_id_expr*/

int
intrins_arg_cmp(defn,t)
     IntrinsInfo *defn;		/* Definition */
     Token *t;			/* Argument */
{
  int defn_types=defn->arg_type;
  int a_type = datatype_of(t->class);
  int type_OK;
				/* Check for argument type mismatch.
				 */
	    type_OK = ( (1<<a_type) & defn_types );
	    if(! type_OK) {
	      int ct;/* compatible type */
				/* Accept compatible types if
				   sizes agree, e.g. DSQRT(REAL*8).
				   The macros check the two cases and
				   set ct to the compatible type.
				 */
#define EXCEPTION1 (a_type==type_REAL && ((1<<(ct=type_DP))&defn_types))
#define EXCEPTION2 (a_type==type_COMPLEX&&((1<<(ct=type_DCOMPLEX))&defn_types))

	      if(!( (EXCEPTION1||EXCEPTION2) && t->size==type_size[ct] )){
		syntax_error(t->line_num,t->col_num,
			"illegal argument data type for intrinsic function");
		msg_tail(defn->name);
		msg_tail(":");
		report_type(t);
	      }
	      else {
		if(port_check || local_wordsize==0) {
		  nonportable(t->line_num,t->col_num,
	      "argument precision may not be correct for intrinsic function");
		  msg_tail(defn->name);
		  msg_tail(":");
		  report_type(t);
		}
		type_OK = TRUE; /* Acceptable after all */
	      }
	    }/* end if type mismatch */

  return type_OK;
}/*intrins_arg_cmp*/


				/* Check agreement between statement function
				   dummy (t1) and actual (t2) args.  At this
				   time, checks only class, type and size,
				   not arrayness.  */
void
stmt_fun_arg_cmp(symt,d_arg,a_arg)
     Lsymtab *symt;
     Token *d_arg,*a_arg;
{
  int d_class = class_VAR,
      a_class = storage_class_of(a_arg->class),
      d_type = datatype_of(d_arg->class),
      a_type = datatype_of(a_arg->class),
      d_size = d_arg->size,
      a_size = a_arg->size,
      d_defsize = (d_size == size_DEFAULT),
      a_defsize = (a_size == size_DEFAULT);
  int d_cmptype= (d_type==type_HOLLERITH && a_type!=type_STRING)?
				a_type:type_category[d_type];
  int a_cmptype= (a_type==type_HOLLERITH && d_type!=type_STRING)?
				d_type:type_category[a_type];

  if(!(port_check || local_wordsize==0)) {
    if(d_defsize)
      d_size = type_size[d_type];
    if(a_defsize)
      a_size = type_size[a_type];
  }

  if(d_size < 0 || a_size < 0) { /* char size_ADJUSTABLE or UNKNOWN */
    d_size = a_size = size_DEFAULT;	/* suppress warnings on size */
    d_defsize = a_defsize = TRUE;
  }

  if(d_class != a_class || d_cmptype != a_cmptype ||
     (d_type == type_STRING? d_size > a_size: d_size != a_size) ) {
		syntax_error(a_arg->line_num,a_arg->col_num,
		  "argument mismatch in stmt function");
		msg_tail(symt->name); /* Give the stmt func name */
		msg_tail(": dummy");
		report_type(d_arg); /* Dummy arg type */
		msg_tail("vs actual");
		report_type(a_arg);
  }
}/*stmt_fun_arg_cmp*/


				/* Routine to document the types of
				   two terms and their operator */
PRIVATE void
report_mismatch(term1,operator,term2)
     Token *term1,*operator,*term2;
{
  report_type(term1);
  msg_tail(op_string(operator));
  report_type(term2);
}
				/* Routine to document the type
				   of a token, with its name if it
				   has one. */
PRIVATE void
report_type(t)
     Token *t;
{
  msg_tail(sized_typename(datatype_of(t->class),t->size));
  if(is_true(ID_EXPR,t->subclass))
    msg_tail(hashtab[t->value.integer].name);
  else if(is_true(LIT_CONST,t->subclass))
    msg_tail("const");
  else
    msg_tail("expr");
}


int
substring_size(id,limits)
     Token *id,*limits;
{
	Lsymtab *symt;
	int id_type,id_len;
	int startindex,endindex,substr_len;
	symt = hashtab[id->value.integer].loc_symtab;
	id_type=get_type(symt);

	substr_len=size_UNKNOWN;

	if(id_type != type_STRING) {
	  syntax_error(id->line_num,id->col_num,
		       "string variable expected");
	}
	else {
	  id_len = id->size;
		/* fortran.y stores (startindex:endindex) in class,subclass */
	  startindex = limits->class;
	  endindex = limits->subclass;
	  if(startindex != size_UNKNOWN && endindex != size_UNKNOWN) {
		/* Check limits unless endindex=0 */
	    if( startindex > endindex && endindex > 0 ) {
	      syntax_error(limits->line_num,limits->col_num,
		      "invalid substring limits");
	    }
	    else {
	      if(endindex == 0)	/* 0 means it was (startindex: ) */
		endindex=id_len;
	      substr_len = endindex-startindex+1;
	      if(id_len > 0 && substr_len > id_len)
		syntax_error(limits->line_num,limits->col_num,
		      "substring size exceeds string size");
	    }
	  }
	}
	return substr_len;
}

	/* Integer power: uses recursion x**n = (x**(n/2))**2 */
PRIVATE int
int_power(x,n)
	int x,n;
{
	int temp;
			/* Order of tests puts commonest cases first */
	if(n > 1) {
		temp = int_power(x,n>>1);
		temp *= temp;
		if(n&1) return temp*x;	/* Odd n */
		else	return temp;	/* Even n */
	}
	else if(n == 1) return x;
	else if(n < 0) return 1/int_power(x,-n);	/* Usually 0 */
	else return 1;
}

				/* Intrinsic function handlers */

PRIVATE int
    ii_abs(), ii_sign(),  ii_dim(),   ii_mod(),
    ii_max(), ii_min(),   ii_ichar(), ii_len(),  ii_index();

/* Array of pointers to functions for evaluating integer-valued intrinsic
   functions.  The order matches definitions of I_ABS thru I_INDEX in
   symtab.h */

PRIVATE int (*ii_fun[])()={
  NULL,
  ii_abs,
  ii_sign,
  ii_dim,
  ii_mod,
  ii_max,
  ii_min,
  ii_ichar,
  ii_len,
  ii_index,
};

int
eval_intrins(defn,args)
     IntrinsInfo *defn;
     Token *args;
{
    int index;
    index = (defn->intrins_flags & I_EVALUATED);

				/* Args must be evaluated, except for LEN */
    if( (is_true(EVALUATED_EXPR,args->subclass) || index==I_LEN) &&
       index > 0 && index < (sizeof(ii_fun)/sizeof(ii_fun[0])) ) {
      return (*ii_fun[index])(args);
    }
    else {
#ifdef DEBUG_EXPRTYPE
      if(debug_latest)
	fprintf(list_fd,"\nIntrinsic %s not handled",defn->name);
      make_false(EVALUATED_EXPR,args->subclass);
#endif
      return 0;
    }
}


PRIVATE int
ii_abs(args)
     Token *args;
{
  Token *t;
  int val, result=0;
  t = args->next_token;
  if(t->class != type_INTEGER) {/* wrong arg type: message given elsewhere */
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    val = int_expr_value(t);
    result = (val >= 0? val: -val);
  }
  return result;
}

PRIVATE int
ii_sign(args)			/* SIGN(value,sign) */
     Token *args;
{
  Token *t1,*t2;
  int val1,val2, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->class != type_INTEGER
     || t2->class != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    val1 = int_expr_value(t1);
    if(val1 < 0) val1 = -val1;
    val2 = int_expr_value(t2);
    result = (val2 >= 0? val1: -val1);
  }
  return result;
}

PRIVATE int
ii_dim(args)			/* DIM(int,int) */
     Token *args;
{
  Token *t1,*t2;
  int val, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->class != type_INTEGER
     || t2->class != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    val = int_expr_value(t1)-int_expr_value(t2);
    result = (val >= 0? val: 0);
  }
  return result;
}

PRIVATE int
ii_mod(args)			/* MOD(int,int) */
     Token *args;
{
  Token *t1,*t2;
  int val1,val2,quotient, result=0;
  t1 = args->next_token;
  t2 = t1->next_token;
  if(t2 == NULL || t1->class != type_INTEGER
     || t2->class != type_INTEGER) {/* wrong arg type: message given elswr */
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    val1 = int_expr_value(t1);
    val2 = int_expr_value(t2);
    if((val1 < 0) == (val2 < 0)) {
      quotient = val1/val2;	/* Both positive or both negative*/
    }
    else {
      quotient = -(-val1/val2);	/* Unlike signs */
    }
    result = val1 - quotient*val2;
  }
  return result;
}


PRIVATE int
ii_max(args)			/* MAX(int,int,...) */
     Token *args;
{
  Token *t=args;
  int val,result=0,n=0;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
fprintf(list_fd,"\nEvaluating MAX(");
#endif
  while( (t=t->next_token) != NULL) {

      if(t->class != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->subclass);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val > result)
	  result = val;
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
fprintf(list_fd,"%d ",val);
#endif
      }
  }
#ifdef DEBUG_EXPRTYPE
if(debug_latest)
fprintf(list_fd,") = %d",result);
#endif
  return result;
}

PRIVATE int
ii_min(args)			/* MIN(int,int,...) */
     Token *args;
{
  Token *t=args;
  int val,result=0,n=0;
  while( (t=t->next_token) != NULL) {
      if(t->class != type_INTEGER) {/* wrong arg type: message given elswr */
	make_false(EVALUATED_EXPR,args->subclass);
	break;
      }
      else {
	val = int_expr_value(t);
	if(n++ == 0 || val < result)
	  result = val;
      }
  }
  return result;
}

PRIVATE int
ii_ichar(args)		/* ICHAR(string) */
     Token *args;
{
  Token *t=args->next_token;

  if(t->class != type_STRING) {
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    return t->value.string[0];	/* Processor collating sequence is used */
  }
  return 0;
}

PRIVATE int
ii_len(args)		/* LEN(string) */
     Token *args;
{
  Token *t=args->next_token;
  int val,result=0;

		/* Set the PARAMETER_EXPR flag since LEN of string does
		   not require contents to be known */
  if( t->class == type_STRING && (val = t->size) > 0 ) {
    make_true(PARAMETER_EXPR,args->subclass);
    make_true(EVALUATED_EXPR,args->subclass);
    result = val;
  }
  else {			/* nonstring or adjustable or unknown */
    make_false(PARAMETER_EXPR,args->subclass);
    make_false(EVALUATED_EXPR,args->subclass);
  }

  return result;
}

PRIVATE int
ii_index(args)		/* INDEX(str1,str2) */
     Token *args;
{
  Token *t1,*t2;
  t1=args->next_token;
  t2=t1->next_token;

  if(t2 == NULL || t1->class != type_STRING
     || t2->class != type_STRING) {
    make_false(EVALUATED_EXPR,args->subclass);
  }
  else {
    int i;
    char *s1=t1->value.string;
    char *s2=t2->value.string;
    int n1=strlen(s1), n2=strlen(s2);

    for(i=1; n1 > 0 && n1 >= n2; i++,s1++,n1--) {
      if(strncmp(s1,s2,n2) == 0)
	return i;
    }
  }
  return 0;
}




				/* Undefine special macros */
#undef E
#undef I
#undef R
#undef D
#undef C
#undef L
#undef S
#undef H
#undef W



