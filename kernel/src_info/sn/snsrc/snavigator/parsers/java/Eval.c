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

#define DEP(a, b) a
#define SELF yyt
#include "Eval.h"
/* line 245 "java.ast" */

#include "Errors.h"
#include "Trafo.h"
#include "deftab.h"



static char * yyCheck1 = & Tree_module_does_not_match_evaluator_module_30586066;
static char * yyCheck2 = & generate_Tree_module_without_option_0;

static void yyVisit1 ARGS ((register tTree yyt));

void Eval
#if defined __STDC__ | defined __cplusplus
 (tTree yyt)
#else
 (yyt) tTree yyt;
#endif
{ yyVisit1 (yyt); }

static void yyVisit1
#if defined __STDC__ | defined __cplusplus
 (register tTree yyt)
#else
 (yyt) register tTree yyt;
#endif
{

yyRecursion:
 switch (yyt->Kind) {

case kcompilation_unit:
/* line 291 "java.ast" */

		    yyt->compilation_unit.package->qualified_symbol.env		= NoTree;
yyVisit1 (yyt->compilation_unit.package);
/* line 293 "java.ast" */

		    yyt->compilation_unit.field_list->field_list.env	= mEnv (get_objects (yyt->compilation_unit.field_list,
		       get_objects (yyt->compilation_unit.import_list, NoTree)), NoTree, SELF);
yyVisit1 (yyt->compilation_unit.field_list);
/* line 292 "java.ast" */

		    yyt->compilation_unit.import_list->import_list.env	= NoTree;
yyt = yyt->compilation_unit.import_list; goto yyRecursion;

case kimport:
yyt->import.next->import_list.env=yyt->import.env;
yyVisit1 (yyt->import.next);
yyt->import.qualified_symbol->qualified_symbol.env=yyt->import.env;
yyt = yyt->import.qualified_symbol; goto yyRecursion;

case kimport_asterisk:
yyt->import_asterisk.next->import_list.env=yyt->import_asterisk.env;
yyVisit1 (yyt->import_asterisk.next);
yyt->import_asterisk.qualified_symbol->qualified_symbol.env=yyt->import_asterisk.env;
yyt = yyt->import_asterisk.qualified_symbol; goto yyRecursion;

case kfield:
yyt->field.next->field_list.env=yyt->field.env;
yyt = yyt->field.next; goto yyRecursion;

case kfunction:
yyt->function.next->field_list.env=yyt->function.env;
yyVisit1 (yyt->function.next);
yyt->function.block->statement_c.env=yyt->function.env;
yyVisit1 (yyt->function.block);
yyt->function.throws->type_name_list.env=yyt->function.env;
yyVisit1 (yyt->function.throws);
yyt->function.decl_list->decl_list.env=yyt->function.env;
yyt = yyt->function.decl_list; goto yyRecursion;

case kmethod:
yyt->method.next->field_list.env=yyt->method.env;
yyVisit1 (yyt->method.next);
yyt->method.array->type.env=yyt->method.env;
yyVisit1 (yyt->method.array);
yyt->method.type->type.env=yyt->method.env;
yyVisit1 (yyt->method.type);
/* line 299 "java.ast" */
 yyt->method.block->statement_c.env = mEnv (get_objects (yyt->method.decl_list, NoTree), yyt->method.env, SELF);
yyVisit1 (yyt->method.block);
yyt->method.throws->type_name_list.env=yyt->method.env;
yyVisit1 (yyt->method.throws);
yyt->method.decl_list->decl_list.env=yyt->method.env;
yyt = yyt->method.decl_list; goto yyRecursion;

case kconstructor:
yyt->constructor.next->field_list.env=yyt->constructor.env;
yyVisit1 (yyt->constructor.next);
/* line 300 "java.ast" */
 yyt->constructor.block->statement_c.env = mEnv (get_objects (yyt->constructor.decl_list, NoTree), yyt->constructor.env, SELF);
yyVisit1 (yyt->constructor.block);
yyt->constructor.throws->type_name_list.env=yyt->constructor.env;
yyVisit1 (yyt->constructor.throws);
yyt->constructor.decl_list->decl_list.env=yyt->constructor.env;
yyt = yyt->constructor.decl_list; goto yyRecursion;

case kvar_decl:
yyt->var_decl.next->field_list.env=yyt->var_decl.env;
yyVisit1 (yyt->var_decl.next);
yyt->var_decl.decl_list->decl_list.env=yyt->var_decl.env;
yyVisit1 (yyt->var_decl.decl_list);
yyt->var_decl.type->type.env=yyt->var_decl.env;
yyt = yyt->var_decl.type; goto yyRecursion;

case kstatic_initializer:
yyt->static_initializer.next->field_list.env=yyt->static_initializer.env;
yyVisit1 (yyt->static_initializer.next);
yyt->static_initializer.statement_c->statement_c.env=yyt->static_initializer.env;
yyt = yyt->static_initializer.statement_c; goto yyRecursion;

case kinitializer:
yyt->initializer.next->field_list.env=yyt->initializer.env;
yyVisit1 (yyt->initializer.next);
yyt->initializer.statement_c->statement_c.env=yyt->initializer.env;
yyt = yyt->initializer.statement_c; goto yyRecursion;

case ktype_decl:
yyt->type_decl.next->field_list.env=yyt->type_decl.env;
yyVisit1 (yyt->type_decl.next);
yyt->type_decl.block->field_list.env=yyt->type_decl.env;
yyt = yyt->type_decl.block; goto yyRecursion;

case kclass:
yyt->class.next->field_list.env=yyt->class.env;
yyVisit1 (yyt->class.next);
yyt->class.interfaces->type_name_list.env=yyt->class.env;
yyVisit1 (yyt->class.interfaces);
yyt->class.extends->type.env=yyt->class.env;
yyVisit1 (yyt->class.extends);
/* line 296 "java.ast" */
 yyt->class.block->field_list.env = menv2 (mEnv (get_objects (yyt->class.block, NoTree), NoTree, SELF), yyt->class.env);
yyt = yyt->class.block; goto yyRecursion;

case kinterface:
yyt->my_interface.next->field_list.env=yyt->my_interface.env;
yyVisit1 (yyt->my_interface.next);
yyt->my_interface.extends->type_name_list.env=yyt->my_interface.env;
yyVisit1 (yyt->my_interface.extends);
/* line 297 "java.ast" */
 yyt->my_interface.block->field_list.env = menv2 (mEnv (get_objects (yyt->my_interface.block, NoTree), NoTree, SELF), yyt->my_interface.env);
yyt = yyt->my_interface.block; goto yyRecursion;

case kdecl_list:
/* line 326 "java.ast" */
 yyt->decl_list.no_of_args	= 0;
break;

case knodecl:
/* line 326 "java.ast" */
 yyt->nodecl.no_of_args	= 0;
break;

case kdecl:
yyt->decl.next->decl_list.env=yyt->decl.env;
yyt->decl.array->type.env=yyt->decl.env;
yyVisit1 (yyt->decl.array);
yyt->decl.type->type.env=yyt->decl.env;
yyVisit1 (yyt->decl.type);
yyVisit1 (yyt->decl.next);
/* line 327 "java.ast" */
 yyt->decl.no_of_args	= yyt->decl.next->decl_list.no_of_args + 1;
break;

case kvariable:
yyt->variable.next->decl_list.env=yyt->variable.env;
yyt->variable.expression->expression_c.env=yyt->variable.env;
yyVisit1 (yyt->variable.expression);
yyt->variable.array->type.env=yyt->variable.env;
yyVisit1 (yyt->variable.array);
yyt->variable.type->type.env=yyt->variable.env;
yyVisit1 (yyt->variable.type);
yyVisit1 (yyt->variable.next);
/* line 327 "java.ast" */
 yyt->variable.no_of_args	= yyt->variable.next->decl_list.no_of_args + 1;
break;

case kparameter:
yyt->parameter.next->decl_list.env=yyt->parameter.env;
yyt->parameter.array->type.env=yyt->parameter.env;
yyVisit1 (yyt->parameter.array);
yyt->parameter.type->type.env=yyt->parameter.env;
yyVisit1 (yyt->parameter.type);
yyVisit1 (yyt->parameter.next);
/* line 327 "java.ast" */
 yyt->parameter.no_of_args	= yyt->parameter.next->decl_list.no_of_args + 1;
break;

case kstatement:
yyt->statement.next->statement_list.env=yyt->statement.env;
yyVisit1 (yyt->statement.next);
yyt->statement.statement->statement_c.env=yyt->statement.env;
yyt = yyt->statement.statement; goto yyRecursion;

case kcompound_stmt:
/* line 301 "java.ast" */
 yyt->compound_stmt.statement_list->statement_list.env = mEnv (get_objects (yyt->compound_stmt.statement_list, NoTree), yyt->compound_stmt.env, SELF);
yyt = yyt->compound_stmt.statement_list; goto yyRecursion;

case kexpression_stmt:
yyt->expression_stmt.expression->expression_c.env=yyt->expression_stmt.env;
yyt = yyt->expression_stmt.expression; goto yyRecursion;

case kvar_decl_stmt:
yyt->var_decl_stmt.type->type.env=yyt->var_decl_stmt.env;
yyVisit1 (yyt->var_decl_stmt.type);
yyt->var_decl_stmt.decl_list->decl_list.env=yyt->var_decl_stmt.env;
yyt = yyt->var_decl_stmt.decl_list; goto yyRecursion;

case ktype_decl_stmt:
yyt->type_decl_stmt.type_decl->type_decl.env=yyt->type_decl_stmt.env;
yyt = yyt->type_decl_stmt.type_decl; goto yyRecursion;

case kif_stmt:
yyt->if_stmt.expression->expression_c.env=yyt->if_stmt.env;
yyVisit1 (yyt->if_stmt.expression);
yyt->if_stmt.then->statement_c.env=yyt->if_stmt.env;
yyt = yyt->if_stmt.then; goto yyRecursion;

case kif_else_stmt:
yyt->if_else_stmt.expression->expression_c.env=yyt->if_else_stmt.env;
yyVisit1 (yyt->if_else_stmt.expression);
yyt->if_else_stmt.Else->statement_c.env=yyt->if_else_stmt.env;
yyVisit1 (yyt->if_else_stmt.Else);
yyt->if_else_stmt.then->statement_c.env=yyt->if_else_stmt.env;
yyt = yyt->if_else_stmt.then; goto yyRecursion;

case kwhile_stmt:
yyt->while_stmt.expression->expression_c.env=yyt->while_stmt.env;
yyVisit1 (yyt->while_stmt.expression);
yyt->while_stmt.statement->statement_c.env=yyt->while_stmt.env;
yyt = yyt->while_stmt.statement; goto yyRecursion;

case kdo_stmt:
yyt->do_stmt.statement->statement_c.env=yyt->do_stmt.env;
yyVisit1 (yyt->do_stmt.statement);
yyt->do_stmt.expression->expression_c.env=yyt->do_stmt.env;
yyt = yyt->do_stmt.expression; goto yyRecursion;

case kbreak_id_stmt:
yyt->break_id_stmt.expression->expression_c.env=yyt->break_id_stmt.env;
yyt = yyt->break_id_stmt.expression; goto yyRecursion;

case kcontinue_id_stmt:
yyt->continue_id_stmt.expression->expression_c.env=yyt->continue_id_stmt.env;
yyt = yyt->continue_id_stmt.expression; goto yyRecursion;

case kreturn_expr_stmt:
yyt->return_expr_stmt.expression->expression_c.env=yyt->return_expr_stmt.env;
yyt = yyt->return_expr_stmt.expression; goto yyRecursion;

case kfor_stmt:
/* line 302 "java.ast" */
 yyt->for_stmt.statement->statement_c.env = mEnv (get_objects (yyt->for_stmt.for_init, NoTree), yyt->for_stmt.env, SELF);
/* line 303 "java.ast" */

		    yyt->for_stmt.for_init->statement_list.env	= yyt->for_stmt.statement->statement_c.env;
yyVisit1 (yyt->for_stmt.for_init);
yyVisit1 (yyt->for_stmt.statement);
/* line 305 "java.ast" */

		    yyt->for_stmt.for_incr->statement_list.env	= yyt->for_stmt.statement->statement_c.env;
yyVisit1 (yyt->for_stmt.for_incr);
/* line 304 "java.ast" */

		    yyt->for_stmt.expression->expression_c.env	= yyt->for_stmt.statement->statement_c.env;
yyt = yyt->for_stmt.expression; goto yyRecursion;

case kthrow_stmt:
yyt->throw_stmt.expression->expression_c.env=yyt->throw_stmt.env;
yyt = yyt->throw_stmt.expression; goto yyRecursion;

case ksynchronized_stmt:
yyt->synchronized_stmt.expression->expression_c.env=yyt->synchronized_stmt.env;
yyVisit1 (yyt->synchronized_stmt.expression);
yyt->synchronized_stmt.statement->statement_c.env=yyt->synchronized_stmt.env;
yyt = yyt->synchronized_stmt.statement; goto yyRecursion;

case klabeled_stmt:
yyt->labeled_stmt.statement->statement_c.env=yyt->labeled_stmt.env;
yyt = yyt->labeled_stmt.statement; goto yyRecursion;

case ktry_stmt:
yyt->try_stmt.statement->statement_c.env=yyt->try_stmt.env;
yyVisit1 (yyt->try_stmt.statement);
yyt->try_stmt.finally->statement_c.env=yyt->try_stmt.env;
yyVisit1 (yyt->try_stmt.finally);
yyt->try_stmt.catch_list->catch_list.env=yyt->try_stmt.env;
yyt = yyt->try_stmt.catch_list; goto yyRecursion;

case kswitch_stmt:
yyt->switch_stmt.expression->expression_c.env=yyt->switch_stmt.env;
yyVisit1 (yyt->switch_stmt.expression);
yyt->switch_stmt.switch_list->switch_list.env=yyt->switch_stmt.env;
yyt = yyt->switch_stmt.switch_list; goto yyRecursion;

case kswitch_:
yyt->switch_.next->switch_list.env=yyt->switch_.env;
yyVisit1 (yyt->switch_.next);
yyt->switch_.statement_list->statement_list.env=yyt->switch_.env;
yyVisit1 (yyt->switch_.statement_list);
yyt->switch_.expression_list->expression_list.env=yyt->switch_.env;
yyt = yyt->switch_.expression_list; goto yyRecursion;

case kcatch:
yyt->catch.next->catch_list.env=yyt->catch.env;
yyVisit1 (yyt->catch.next);
/* line 307 "java.ast" */
 yyt->catch.statement->statement_c.env = mEnv (get_objects (yyt->catch.decl_list, NoTree), yyt->catch.env, SELF);
yyVisit1 (yyt->catch.statement);
yyt->catch.decl_list->decl_list.env=yyt->catch.env;
yyt = yyt->catch.decl_list; goto yyRecursion;

case ktype_name:
yyt->type_name.next->type_name_list.env=yyt->type_name.env;
yyVisit1 (yyt->type_name.next);
yyt->type_name.named_type->named_type.env=yyt->type_name.env;
yyt = yyt->type_name.named_type; goto yyRecursion;

case knamed_type:
yyt->named_type.qualified_symbol->qualified_symbol.env=yyt->named_type.env;
yyt = yyt->named_type.qualified_symbol; goto yyRecursion;

case karray_type:
yyt->array_type.type->type.env=yyt->array_type.env;
yyt = yyt->array_type.type; goto yyRecursion;

case kexpression_list:
/* line 328 "java.ast" */
 yyt->expression_list.no_of_args	= 0;
break;

case knoexpression_l:
/* line 328 "java.ast" */
 yyt->noexpression_l.no_of_args	= 0;
break;

case kexpression:
yyt->expression.next->expression_list.env=yyt->expression.env;
yyt->expression.expression->expression_c.env=yyt->expression.env;
yyVisit1 (yyt->expression.expression);
yyVisit1 (yyt->expression.next);
/* line 329 "java.ast" */
 yyt->expression.no_of_args	= yyt->expression.next->expression_list.no_of_args + 1;
break;

case kqualified_symbol:
/* line 322 "java.ast" */
 yyt->qualified_symbol.object	= nnoobject;
break;

case kqualification:
/* line 322 "java.ast" */
 yyt->qualification.object	= nnoobject;
yyt->qualification.qualified_symbol->qualified_symbol.env=yyt->qualification.env;
yyt = yyt->qualification.qualified_symbol; goto yyRecursion;

case kident:
/* line 322 "java.ast" */
 yyt->ident.object	= nnoobject;
break;

case knoexpression:
/* line 322 "java.ast" */
 yyt->noexpression.object	= nnoobject;
break;

case kunary:
yyt->unary.expression->expression_c.env=yyt->unary.env;
yyt = yyt->unary.expression; goto yyRecursion;

case kbinary:
yyt->binary.lop->expression_c.env=yyt->binary.env;
yyVisit1 (yyt->binary.lop);
yyt->binary.rop->expression_c.env=yyt->binary.env;
yyt = yyt->binary.rop; goto yyRecursion;

case kassign:
yyt->assign.lval->expression_c.env=yyt->assign.env;
yyVisit1 (yyt->assign.lval);
yyt->assign.rval->expression_c.env=yyt->assign.env;
yyt = yyt->assign.rval; goto yyRecursion;

case kaggregate:
yyt->aggregate.expression_list->expression_list.env=yyt->aggregate.env;
yyt = yyt->aggregate.expression_list; goto yyRecursion;

case kcall:
yyt->call.expression->expression_c.env=yyt->call.env;
yyVisit1 (yyt->call.expression);
yyt->call.expression_list->expression_list.env=yyt->call.env;
yyt = yyt->call.expression_list; goto yyRecursion;

case kselect:
yyt->select.expression->expression_c.env=yyt->select.env;
yyt = yyt->select.expression; goto yyRecursion;

case kget_class_of_expr:
yyt->get_class_of_expr.expression->expression_c.env=yyt->get_class_of_expr.env;
yyt = yyt->get_class_of_expr.expression; goto yyRecursion;

case kget_class:
yyt->get_class.type->type.env=yyt->get_class.env;
yyt = yyt->get_class.type; goto yyRecursion;

case ksubscript:
yyt->subscript.base->expression_c.env=yyt->subscript.env;
yyVisit1 (yyt->subscript.base);
yyt->subscript.index->expression_c.env=yyt->subscript.env;
yyt = yyt->subscript.index; goto yyRecursion;

case ktype_compare:
yyt->type_compare.expression->expression_c.env=yyt->type_compare.env;
yyVisit1 (yyt->type_compare.expression);
yyt->type_compare.type->type.env=yyt->type_compare.env;
yyt = yyt->type_compare.type; goto yyRecursion;

case ktype_cast:
yyt->type_cast.type->type.env=yyt->type_cast.env;
yyVisit1 (yyt->type_cast.type);
yyt->type_cast.expression->expression_c.env=yyt->type_cast.env;
yyVisit1 (yyt->type_cast.expression);
yyt->type_cast.dims->type.env=yyt->type_cast.env;
yyt = yyt->type_cast.dims; goto yyRecursion;

case knew:
yyt->new.type->type.env=yyt->new.env;
yyVisit1 (yyt->new.type);
yyt->new.expression->expression_c.env=yyt->new.env;
yyVisit1 (yyt->new.expression);
yyt->new.expression_list->expression_list.env=yyt->new.env;
yyVisit1 (yyt->new.expression_list);
yyt->new.dims->type.env=yyt->new.env;
yyt = yyt->new.dims; goto yyRecursion;

case kanonymous:
yyt->anonymous.type->type.env=yyt->anonymous.env;
yyVisit1 (yyt->anonymous.type);
/* line 298 "java.ast" */
 yyt->anonymous.block->field_list.env = menv2 (mEnv (get_objects (yyt->anonymous.block, NoTree), NoTree, SELF), yyt->anonymous.env);
yyVisit1 (yyt->anonymous.block);
yyt->anonymous.expression_list->expression_list.env=yyt->anonymous.env;
yyt = yyt->anonymous.expression_list; goto yyRecursion;

case kconditional:
yyt->conditional.condition->expression_c.env=yyt->conditional.env;
yyVisit1 (yyt->conditional.condition);
yyt->conditional.false_expr->expression_c.env=yyt->conditional.env;
yyVisit1 (yyt->conditional.false_expr);
yyt->conditional.true_expr->expression_c.env=yyt->conditional.env;
yyt = yyt->conditional.true_expr; goto yyRecursion;

case kenv:
yyVisit1 (yyt->env.objects);
yyt = yyt->env.env; goto yyRecursion;

case kenv2:
yyVisit1 (yyt->env2.env1);
yyt = yyt->env2.env2; goto yyRecursion;

case kobject:
yyt = yyt->object.next; goto yyRecursion;
 default: ;
 }
}

void BeginEval ARGS ((void))
{
}

void CloseEval ARGS ((void))
{
}

