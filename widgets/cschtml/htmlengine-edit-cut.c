/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

   Copyright (C) 2000 Helix Code, Inc.
   
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
   
   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#include "htmltextmaster.h"

#include "htmlengine-cutbuffer.h"
#include "htmlengine-edit-copy.h"
#include "htmlengine-edit-delete.h"
#include "htmlengine-edit-movement.h"
#include "htmlengine-edit-paste.h"

#include "htmlengine-edit-cut.h"


/* Undo/redo.  */

struct _ActionData {
	/* Reference count.  This is necessary because we want to share the data between
           undo and redo.  */
	guint ref_count;

	/* Contents of the cut buffer.  */
	GList *buffer;

	/* Number of character elements in the buffer.  */
	guint buffer_count;

	/* Whether the mark preceded the cursor when cut happened.  */
	guint mark_precedes_cursor;
};
typedef struct _ActionData ActionData;

static void  closure_destroy  (gpointer closure);
static void  do_redo          (HTMLEngine *engine, gpointer closure);
static void  do_undo          (HTMLEngine *engine, gpointer closure);
static void  setup_redo       (HTMLEngine *engine, ActionData *data);
static void  setup_undo       (HTMLEngine *engine, ActionData *data);

static void
closure_destroy (gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;
	g_assert (data->ref_count > 0);

	data->ref_count --;
	if (data->ref_count > 0)
		return;

	html_engine_cut_buffer_destroy (data->buffer);

	g_free (data);
}

static void
do_undo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;

	html_engine_paste_buffer (engine, data->buffer);

	if (! data->mark_precedes_cursor)
		html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT, data->buffer_count);

	setup_redo (engine, data);
}

static void
setup_undo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *action;

	data->ref_count ++;

	/* FIXME i18n */
	action = html_undo_action_new ("cut", do_undo, closure_destroy, data,
				       html_cursor_get_position (engine->cursor));
	html_undo_add_undo_action (engine->undo, action);
}

static void
do_redo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;

	if (data->mark_precedes_cursor)
		html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT, data->buffer_count);

	html_engine_delete (engine, data->buffer_count, FALSE, FALSE);

	setup_undo (engine, data);
}

static void
setup_redo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *action;

	data->ref_count ++;

	/* FIXME i18n */
	action = html_undo_action_new ("cut", do_redo, closure_destroy, data,
				       html_cursor_get_position (engine->cursor));
	html_undo_add_redo_action (engine->undo, action);
}

static void
init_undo (HTMLEngine *engine,
	   GList *buffer,
	   guint num_elems,
	   gboolean mark_precedes_cursor)
{
	ActionData *data;

	data = g_new (ActionData, 1);

	data->ref_count = 0;
	data->buffer = html_engine_cut_buffer_dup (buffer);
	data->buffer_count = num_elems;
	data->mark_precedes_cursor = mark_precedes_cursor;

	setup_undo (engine, data);
}


/**
 * html_engine_cut:
 * @engine: An HTMLEngine
 * @do_undo: Whether to save undo information for this command
 *
 * Cut the current selection and put it into @engine's cut buffer.
 * Save undo information only if @do_undo is true.
 **/
void
html_engine_cut (HTMLEngine *engine,
		 gboolean do_undo)
{
	gboolean mark_precedes_cursor;
	guint elems_copied;

	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));
	g_return_if_fail (engine->active_selection);

	if (do_undo)
		html_undo_discard_redo (engine->undo);

	elems_copied = html_engine_copy (engine);
	mark_precedes_cursor = html_cursor_precedes (engine->mark, engine->cursor);

	html_engine_disable_selection (engine);

	if (mark_precedes_cursor)
		html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT, elems_copied);
	html_engine_delete (engine, elems_copied, FALSE, FALSE);

	if (do_undo)
		init_undo (engine, engine->cut_buffer, elems_copied, mark_precedes_cursor);
}
