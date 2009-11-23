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
/* line 105 "tcl.ast" */

#include "Trafo.h"



static char * yyCheck1 = & Tree_module_does_not_match_evaluator_module_7188957;
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

case kprogram:
/* line 137 "tcl.ast" */
 yyt->program.stmts->stmts.env = menv (mnoobject (), NoTree, SELF);
yyt = yyt->program.stmts; goto yyRecursion;

case kstmt_c:
yyt->stmt_c.next->stmts.env=yyt->stmt_c.env;
yyt = yyt->stmt_c.next; goto yyRecursion;

case kstmt:
yyt->stmt.next->stmts.env=yyt->stmt.env;
yyVisit1 (yyt->stmt.next);
yyt->stmt.words->words.env=yyt->stmt.env;
yyt = yyt->stmt.words; goto yyRecursion;

case kproc_c:
yyt->proc_c.next->stmts.env=yyt->proc_c.env;
yyVisit1 (yyt->proc_c.next);
/* line 138 "tcl.ast" */
 yyt->proc_c.block->texts.env = menv (mnoobject (), yyt->proc_c.env, SELF);
yyVisit1 (yyt->proc_c.block);
yyt->proc_c.parameter->words.env=yyt->proc_c.env;
yyVisit1 (yyt->proc_c.parameter);
/* line 139 "tcl.ast" */

		    yyt->proc_c.param_names->words.env = yyt->proc_c.block->texts.env;
yyVisit1 (yyt->proc_c.param_names);
yyt->proc_c.qualification->qualification_c.env=yyt->proc_c.env;
yyt = yyt->proc_c.qualification; goto yyRecursion;

case kproc:
yyt->proc.next->stmts.env=yyt->proc.env;
yyVisit1 (yyt->proc.next);
/* line 138 "tcl.ast" */
 yyt->proc.block->texts.env = menv (mnoobject (), yyt->proc.env, SELF);
yyVisit1 (yyt->proc.block);
yyt->proc.parameter->words.env=yyt->proc.env;
yyVisit1 (yyt->proc.parameter);
/* line 139 "tcl.ast" */

		    yyt->proc.param_names->words.env = yyt->proc.block->texts.env;
yyVisit1 (yyt->proc.param_names);
yyt->proc.qualification->qualification_c.env=yyt->proc.env;
yyt = yyt->proc.qualification; goto yyRecursion;

case kmethod:
yyt->method.next->stmts.env=yyt->method.env;
yyVisit1 (yyt->method.next);
/* line 138 "tcl.ast" */
 yyt->method.block->texts.env = menv (mnoobject (), yyt->method.env, SELF);
yyVisit1 (yyt->method.block);
yyt->method.parameter->words.env=yyt->method.env;
yyVisit1 (yyt->method.parameter);
/* line 139 "tcl.ast" */

		    yyt->method.param_names->words.env = yyt->method.block->texts.env;
yyVisit1 (yyt->method.param_names);
yyt->method.qualification->qualification_c.env=yyt->method.env;
yyt = yyt->method.qualification; goto yyRecursion;

case kbody:
yyt->body.next->stmts.env=yyt->body.env;
yyVisit1 (yyt->body.next);
/* line 138 "tcl.ast" */
 yyt->body.block->texts.env = menv (mnoobject (), yyt->body.env, SELF);
yyVisit1 (yyt->body.block);
yyt->body.parameter->words.env=yyt->body.env;
yyVisit1 (yyt->body.parameter);
/* line 139 "tcl.ast" */

		    yyt->body.param_names->words.env = yyt->body.block->texts.env;
yyVisit1 (yyt->body.param_names);
yyt->body.qualification->qualification_c.env=yyt->body.env;
yyt = yyt->body.qualification; goto yyRecursion;

case kconfigbody:
yyt->configbody.next->stmts.env=yyt->configbody.env;
yyVisit1 (yyt->configbody.next);
/* line 138 "tcl.ast" */
 yyt->configbody.block->texts.env = menv (mnoobject (), yyt->configbody.env, SELF);
yyVisit1 (yyt->configbody.block);
yyt->configbody.parameter->words.env=yyt->configbody.env;
yyVisit1 (yyt->configbody.parameter);
/* line 139 "tcl.ast" */

		    yyt->configbody.param_names->words.env = yyt->configbody.block->texts.env;
yyVisit1 (yyt->configbody.param_names);
yyt->configbody.qualification->qualification_c.env=yyt->configbody.env;
yyt = yyt->configbody.qualification; goto yyRecursion;

case kconstructor:
yyt->constructor.next->stmts.env=yyt->constructor.env;
yyVisit1 (yyt->constructor.next);
yyt->constructor.init->words.env=yyt->constructor.env;
yyVisit1 (yyt->constructor.init);
/* line 138 "tcl.ast" */
 yyt->constructor.block->texts.env = menv (mnoobject (), yyt->constructor.env, SELF);
yyVisit1 (yyt->constructor.block);
yyt->constructor.parameter->words.env=yyt->constructor.env;
yyVisit1 (yyt->constructor.parameter);
/* line 139 "tcl.ast" */

		    yyt->constructor.param_names->words.env = yyt->constructor.block->texts.env;
yyVisit1 (yyt->constructor.param_names);
yyt->constructor.qualification->qualification_c.env=yyt->constructor.env;
yyt = yyt->constructor.qualification; goto yyRecursion;

case kdestructor:
yyt->destructor.next->stmts.env=yyt->destructor.env;
yyVisit1 (yyt->destructor.next);
/* line 138 "tcl.ast" */
 yyt->destructor.block->texts.env = menv (mnoobject (), yyt->destructor.env, SELF);
yyVisit1 (yyt->destructor.block);
yyt->destructor.parameter->words.env=yyt->destructor.env;
yyVisit1 (yyt->destructor.parameter);
/* line 139 "tcl.ast" */

		    yyt->destructor.param_names->words.env = yyt->destructor.block->texts.env;
yyVisit1 (yyt->destructor.param_names);
yyt->destructor.qualification->qualification_c.env=yyt->destructor.env;
yyt = yyt->destructor.qualification; goto yyRecursion;

case knamespace_c:
yyt->namespace_c.next->stmts.env=yyt->namespace_c.env;
yyVisit1 (yyt->namespace_c.next);
/* line 141 "tcl.ast" */
 yyt->namespace_c.block->texts.env = menv (mnoobject (), yyt->namespace_c.env, SELF);
yyVisit1 (yyt->namespace_c.block);
yyt->namespace_c.qualification->qualification_c.env=yyt->namespace_c.env;
yyt = yyt->namespace_c.qualification; goto yyRecursion;

case knamespace:
yyt->namespace.next->stmts.env=yyt->namespace.env;
yyVisit1 (yyt->namespace.next);
/* line 141 "tcl.ast" */
 yyt->namespace.block->texts.env = menv (mnoobject (), yyt->namespace.env, SELF);
yyVisit1 (yyt->namespace.block);
yyt->namespace.qualification->qualification_c.env=yyt->namespace.env;
yyt = yyt->namespace.qualification; goto yyRecursion;

case kclass:
yyt->class.next->stmts.env=yyt->class.env;
yyVisit1 (yyt->class.next);
/* line 141 "tcl.ast" */
 yyt->class.block->texts.env = menv (mnoobject (), yyt->class.env, SELF);
yyVisit1 (yyt->class.block);
yyt->class.qualification->qualification_c.env=yyt->class.env;
yyt = yyt->class.qualification; goto yyRecursion;

case kitcl_class:
yyt->itcl_class.next->stmts.env=yyt->itcl_class.env;
yyVisit1 (yyt->itcl_class.next);
/* line 141 "tcl.ast" */
 yyt->itcl_class.block->texts.env = menv (mnoobject (), yyt->itcl_class.env, SELF);
yyVisit1 (yyt->itcl_class.block);
yyt->itcl_class.qualification->qualification_c.env=yyt->itcl_class.env;
yyt = yyt->itcl_class.qualification; goto yyRecursion;

case kword_c:
yyt->word_c.next->words.env=yyt->word_c.env;
yyt = yyt->word_c.next; goto yyRecursion;

case kone_word:
yyt->one_word.next->words.env=yyt->one_word.env;
yyt = yyt->one_word.next; goto yyRecursion;

case kobj_word:
yyt->obj_word.next->words.env=yyt->obj_word.env;
yyt = yyt->obj_word.next; goto yyRecursion;

case kqual_word:
yyt->qual_word.next->words.env=yyt->qual_word.env;
yyVisit1 (yyt->qual_word.next);
yyt->qual_word.qualification->qualification_c.env=yyt->qual_word.env;
yyt = yyt->qual_word.qualification; goto yyRecursion;

case kqual_words:
yyt->qual_words.next->words.env=yyt->qual_words.env;
yyVisit1 (yyt->qual_words.next);
yyt->qual_words.qualifications->qualifications.env=yyt->qual_words.env;
yyt = yyt->qual_words.qualifications; goto yyRecursion;

case kone_qualification:
yyt->one_qualification.next->qualifications.env=yyt->one_qualification.env;
yyVisit1 (yyt->one_qualification.next);
yyt->one_qualification.qualification->qualification_c.env=yyt->one_qualification.env;
yyt = yyt->one_qualification.qualification; goto yyRecursion;

case ktext:
yyt->text.next->texts.env=yyt->text.env;
yyt = yyt->text.next; goto yyRecursion;

case kcharacter:
yyt->character.next->texts.env=yyt->character.env;
yyt = yyt->character.next; goto yyRecursion;

case kident:
yyt->ident.next->texts.env=yyt->ident.env;
yyt = yyt->ident.next; goto yyRecursion;

case kblock:
yyt->block.next->texts.env=yyt->block.env;
yyVisit1 (yyt->block.next);
yyt->block.stmts->stmts.env=yyt->block.env;
yyt = yyt->block.stmts; goto yyRecursion;

case kcontent:
yyt->content.next->texts.env=yyt->content.env;
yyVisit1 (yyt->content.next);
yyt->content.qualification->qualification_c.env=yyt->content.env;
yyt = yyt->content.qualification; goto yyRecursion;

case kblock_content:
yyt->block_content.next->texts.env=yyt->block_content.env;
yyVisit1 (yyt->block_content.next);
yyt->block_content.stmts->stmts.env=yyt->block_content.env;
yyt = yyt->block_content.stmts; goto yyRecursion;

case klocal_text:
yyt->local_text.texts->texts.env=yyt->local_text.env;
yyt = yyt->local_text.texts; goto yyRecursion;

case kglobal_text:
yyt->global_text.texts->texts.env=yyt->global_text.env;
yyt = yyt->global_text.texts; goto yyRecursion;

case kqualification:
yyt->qualification.qualification->qualification_c.env=yyt->qualification.env;
yyt = yyt->qualification.qualification; goto yyRecursion;

case kcomplex_qual:
yyt->complex_qual.qualification->qualification_c.env=yyt->complex_qual.env;
yyVisit1 (yyt->complex_qual.qualification);
yyt->complex_qual.texts->texts.env=yyt->complex_qual.env;
yyt = yyt->complex_qual.texts; goto yyRecursion;

case ksubscription:
yyt->subscription.qualification->qualification_c.env=yyt->subscription.env;
yyVisit1 (yyt->subscription.qualification);
yyt->subscription.index->qualification_c.env=yyt->subscription.env;
yyt = yyt->subscription.index; goto yyRecursion;

case kenv:
yyVisit1 (yyt->env.env);
yyt = yyt->env.objects; goto yyRecursion;

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

