/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

   Copyright (C) 2000 Helix Code, Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHcANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#include "htmlundo.h"


static void
destroy_action_list (GList *lp)
{
	GList *p;

	for (p = lp; p != NULL; p = p->next)
		html_undo_action_destroy ((HTMLUndoAction *) p->data);
}


HTMLUndo *
html_undo_new (void)
{
	HTMLUndo *new;

	new = g_new (HTMLUndo, 1);

	new->undo_stack = NULL;
	new->undo_stack_size = 0;

	new->redo_stack = NULL;
	new->redo_stack_size = 0;

	new->undo_levels = NULL;
	new->redo_levels = NULL;
	new->undo_levels_size = 0;

	return new;
}

void
html_undo_destroy  (HTMLUndo *undo)
{
	g_return_if_fail (undo != NULL);

	destroy_action_list (undo->undo_stack);
	destroy_action_list (undo->redo_stack);

	g_free (undo);
}


static void
do_action (HTMLUndo *undo,
	   HTMLEngine *engine,
	   GList **action_list)
{
	HTMLUndoAction *action;
	HTMLCursor *cursor;
	GList *first;
	gint current_position;

	cursor = engine->cursor;
	current_position = html_cursor_get_position (cursor);

	action = (HTMLUndoAction *) (*action_list)->data;

	/* 1.  Restore the cursor position.  */
	html_cursor_jump_to_position (cursor, engine, action->position);

	/* 2.  Call the function that executes the action.  */
	(* action->function) (engine, action->closure);

	/* 3.  Destroy the action.  */
	html_undo_action_destroy (action);

	first = *action_list;
	*action_list = (*action_list)->next;

	if (*action_list == NULL)
		g_warning ("No more actions in list!");
	else
		(*action_list)->prev = NULL;

	first->next = NULL;
	g_list_free (first);
}

void
html_undo_do_undo (HTMLUndo *undo,
		   HTMLEngine *engine)
{
	g_return_if_fail (undo != NULL);
	g_return_if_fail (engine != NULL);

	if (undo->undo_stack_size > 0) {
		do_action (undo, engine, &undo->undo_stack);
		undo->undo_stack_size--;
	}
}

void
html_undo_do_redo (HTMLUndo *undo,
		   HTMLEngine *engine)
{
	g_return_if_fail (undo != NULL);
	g_return_if_fail (engine != NULL);

	if (undo->redo_stack_size > 0) {
		do_action (undo, engine, &undo->redo_stack);
		undo->redo_stack_size--;
	}
}


void
html_undo_discard_redo (HTMLUndo *undo)
{
	g_return_if_fail (undo != NULL);

	if (undo->redo_stack == NULL)
		return;

	destroy_action_list (undo->redo_stack);

	undo->redo_stack = NULL;
	undo->redo_stack_size = 0;
}


void
html_undo_add_undo_action  (HTMLUndo *undo,
			    HTMLUndoAction *action)
{
	g_return_if_fail (undo != NULL);
	g_return_if_fail (action != NULL);

	if (!undo->undo_levels_size && undo->undo_stack_size >= HTML_UNDO_LIMIT) {
		HTMLUndoAction *last_action;
		GList *last;

		last = g_list_last (undo->undo_stack);
		last_action = (HTMLUndoAction *) last->data;

		undo->undo_stack = g_list_remove_link (undo->undo_stack, last);
		g_list_free (last);

		html_undo_action_destroy (last_action);

		undo->undo_stack_size--;
	}

	undo->undo_stack = g_list_prepend (undo->undo_stack, action);
	undo->undo_stack_size++;
}

void
html_undo_add_redo_action  (HTMLUndo *undo,
			    HTMLUndoAction *action)
{
	g_return_if_fail (undo != NULL);
	g_return_if_fail (action != NULL);

	undo->redo_stack = g_list_prepend (undo->redo_stack, action);
	undo->redo_stack_size++;
}

/*
  undo levels

  * IDEA: it closes number of undo steps into one
  * examples: paste
               - it first cuts active selection and then inserts objects
                 from cut_buffer on actual cursor position
               - if you don't use udo levels, it will generate two undo steps/actions
              replace
               - replace uses paste operation, so when it replaces N occurences,
                 it generates 2*N steps (without using undo levels in paste and replace)

  * usage is simple - just call html_undo_level_begin before using functions with undo
    and html_undo_level_end after them

*/

struct _HTMLUndoLevel {
	HTMLUndo *undo;
	GList    *stack;
	guint     size;
	gchar    *description;
};

typedef struct _HTMLUndoLevel HTMLUndoLevel;

static HTMLUndoLevel *
level_new (HTMLUndo *undo, GList *stack, guint size, const gchar *description)
{
	HTMLUndoLevel *nl = g_new (HTMLUndoLevel, 1);

	nl->undo  = undo;
	nl->stack = stack;
	nl->size  = size;
	nl->description = g_strdup (description);

	return nl;
}

static void
level_destroy (HTMLUndoLevel *level)
{
	g_assert (level);

	g_free (level->description);
	g_free (level);
}

void
html_undo_level_begin (HTMLUndo *undo, const gchar *desription)
{
	undo->undo_levels     = g_slist_prepend (undo->undo_levels,
						 level_new (undo, undo->undo_stack, undo->undo_stack_size, desription));
	undo->undo_stack      = NULL;
	undo->undo_stack_size = 0;
	undo->undo_levels_size++;
}

static void
redo_level_begin (HTMLUndo *undo, const gchar *desription)
{
	undo->redo_levels     = g_slist_prepend (undo->redo_levels,
						 level_new (undo, undo->redo_stack, undo->redo_stack_size, desription));
	undo->redo_stack      = NULL;
	undo->redo_stack_size = 0;
}

static void
redo_step_action (HTMLEngine *e, HTMLUndoLevel *level)
{
	HTMLUndo *undo = level->undo;
	GList    *stack;
	guint     size;

	/* prepare undo step */
	html_undo_level_begin (undo, level->description);

	/* preserve current redo stack */
	stack = undo->redo_stack;
	size  = undo->redo_stack_size;

	/* set this level */
	undo->redo_stack      = level->stack;
	undo->redo_stack_size = level->size;

	while (undo->redo_stack_size) {
		html_undo_do_redo (undo, e);
	}

	/* restore current redo stack */
	undo->redo_stack      = stack;
	undo->redo_stack_size = size;

	/* end undo step */
	html_undo_level_end (undo);	
}

static void
redo_level_end (HTMLUndo *undo)
{
	GList *stack;
	guint  size;
	HTMLUndoLevel *level;

	g_assert (undo->redo_levels);

	/* preserve current redo stack */
	stack = undo->redo_stack;
        size  = undo->redo_stack_size;

	/* restore last level from levels stack */
	level = (HTMLUndoLevel *) undo->redo_levels->data;
	undo->redo_stack      = level->stack;
	undo->redo_stack_size = level->size;
	undo->redo_levels     = g_slist_remove (undo->redo_levels, level);

	/* add redo step redo action */
	if (size) {
		HTMLUndoAction *action;

		/* fill level with current redo step */
		level->stack = stack;
		level->size  = size;

		/* we use position from last redo action on the stack */
		action = (HTMLUndoAction *) stack->data;
		html_undo_add_redo_action (undo, html_undo_action_new
					   (level->description,
					    (HTMLUndoActionFunction) redo_step_action,
					    (HTMLUndoActionClosureDestroyFunction) level_destroy,
					    level, action->position));
	} else
		level_destroy (level);
}

static void
undo_step_action (HTMLEngine *e, HTMLUndoLevel *level)
{
	HTMLUndo *undo = level->undo;
	GList    *stack;
	guint     size;

	/* prepare redo step */
	redo_level_begin (undo, level->description);

	/* preserve current undo stack */
	stack = undo->undo_stack;
	size  = undo->undo_stack_size;

	/* set this level */
	undo->undo_stack      = level->stack;
	undo->undo_stack_size = level->size;

	while (undo->undo_stack_size) {
		html_undo_do_undo (undo, e);
	}

	/* restore current undo stack */
	undo->undo_stack      = stack;
	undo->undo_stack_size = size;

	/* end redo step */
	redo_level_end (undo);
}

void
html_undo_level_end (HTMLUndo *undo)
{
	GList *stack;
	guint  size;
	HTMLUndoLevel *level;

	g_assert (undo->undo_levels);
	g_assert (undo->undo_levels_size);

	/* preserve current undo stack */
	stack = undo->undo_stack;
        size  = undo->undo_stack_size;

	/* restore last level from levels stack */
	level = (HTMLUndoLevel *) undo->undo_levels->data;
	undo->undo_stack      = level->stack;
	undo->undo_stack_size = level->size;
	undo->undo_levels     = g_slist_remove (undo->undo_levels, level);

	/* add undo step undo action */
	if (size) {
		HTMLUndoAction *action;

		/* fill level with current undo step */
		level->stack = stack;
		level->size  = size;

		/* we use position from last undo action on the stack */
		action = (HTMLUndoAction *) stack->data;
		html_undo_add_undo_action (undo, html_undo_action_new 
					   (level->description,
					    (HTMLUndoActionFunction) undo_step_action,
					    (HTMLUndoActionClosureDestroyFunction) level_destroy,
					    level, action->position));
	} else
		level_destroy (level);

	undo->undo_levels_size--;
}
