/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

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

/* WARNING / FIXME : All of this code assumes that the only kind of
   selectable object that does not accept the cursor is
   `HTMLClueFlow'.  */


#include "htmlengine-edit-selection-updater.h"


#define SAME_SIGN(x, y) (((x) > 0 && (y) > 0) || ((x) < 0 && (y) < 0))


/* Select from @old_point to @new_point.  */
static void
extend_selection (HTMLEngine *engine,
		  const HTMLCursor *old_point,
		  const HTMLCursor *new_point)
{
	HTMLObject *prev_clueflow;
	HTMLCursor *mark;
	HTMLObject *obj;
	gboolean forward;
	guint start;
	gint length;

	if (old_point->position == new_point->position) {
		g_warning ("%s:%s Extending selection with same start/end???  This should not happen.",
			   __FILE__, G_GNUC_FUNCTION);
		return;
	}

	if (html_cursor_precedes (old_point, new_point))
		forward = TRUE;
	else
		forward = FALSE;

	mark = engine->mark;

	obj = old_point->object;

	while (obj != new_point->object) {
		if (forward) {
			if (obj == mark->object)
				start = mark->offset;
			else
				start = 0;
			html_object_select_range (obj, engine, start, -1, TRUE);
		} else {
			if (obj == mark->object)
				length = mark->offset;
			else
				length = -1;
			html_object_select_range (obj, engine, 0, length, TRUE);
		}

		if (HTML_OBJECT_TYPE (obj->parent) == HTML_TYPE_CLUEFLOW)
			prev_clueflow = obj->parent;
		else
			prev_clueflow = NULL;

		if (forward)
			obj = html_object_next_for_cursor (obj);
		else
			obj = html_object_prev_for_cursor (obj);

		if (obj->parent != prev_clueflow && prev_clueflow != NULL)
			html_object_select_range (prev_clueflow, engine, 0, -1, TRUE);
	}

	if (forward) {
		if (new_point->object == mark->object)
			start = mark->offset;
		else
			start = 0;

		length = new_point->offset - start;
	} else {
		start = new_point->offset;

		if (new_point->object == mark->object)
			length = mark->offset - new_point->offset;
		else
			length = -1;
	}

	html_object_select_range (obj, engine, start, length, TRUE);
}

/* Unselect from @old_point to @new_point.  */
static void
reduce_selection (HTMLEngine *engine,
		  const HTMLCursor *old_point,
		  const HTMLCursor *new_point)
{
	HTMLObject *prev_clueflow;
	HTMLCursor *mark;
	HTMLObject *obj;
	gboolean forward;

	if (old_point->position == new_point->position) {
		g_warning ("%s:%s Reducing selection with same start/end???  This should not happen.",
			   __FILE__, G_GNUC_FUNCTION);
		return;
	}

	if (html_cursor_precedes (old_point, new_point))
		forward = TRUE;
	else
		forward = FALSE;

	mark = engine->mark;

	obj = old_point->object;

	while (obj != new_point->object) {
		html_object_select_range (obj, engine, 0, 0, TRUE);

		if (HTML_OBJECT_TYPE (obj->parent) == HTML_TYPE_CLUEFLOW)
			prev_clueflow = obj->parent;
		else
			prev_clueflow = NULL;

		if (forward)
			obj = html_object_next_for_cursor (obj);
		else
			obj = html_object_prev_for_cursor (obj);

		if (obj->parent != prev_clueflow)
			html_object_select_range (obj->parent, engine, 0, 0, TRUE);
	}

	if (forward) {
		if (new_point->object == mark->object)
			html_object_select_range (new_point->object,
						  engine,
						  new_point->offset,
						  mark->offset - new_point->offset,
						  TRUE);
		else
			html_object_select_range (new_point->object,
						  engine,
						  new_point->offset,
						  -1,
						  TRUE);
	} else {
		if (new_point->object == mark->object)
			html_object_select_range (new_point->object,
						  engine,
						  mark->offset,
						  new_point->offset - mark->offset,
						  TRUE);
		else
			html_object_select_range (new_point->object,
						  engine,
						  0, new_point->offset,
						  TRUE);
	}
}

static void
update_selection (HTMLEngine *engine,
		  const HTMLCursor *old_point,
		  const HTMLCursor *new_point)
{
	const HTMLCursor *mark;
	gint delta_mark, delta_point;

	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));
	g_return_if_fail (new_point != NULL);
	g_return_if_fail (old_point != NULL);

	mark = engine->mark;

	/* First rule out the simple cases.  */

	if (html_cursor_equal (new_point, old_point))
		return;

	if (html_cursor_equal (new_point, mark)) {
		html_engine_unselect_all (engine, TRUE);
		engine->active_selection = TRUE;
		return;
	}

	/* First of all, see if we are crossing the mark.  In this
	   case, we can unselect everything and start selecting from
	   the mark to the new point.  */

	if ((html_cursor_precedes (new_point, mark) && html_cursor_follows (old_point, mark))
	    || (html_cursor_follows (new_point, mark) && html_cursor_precedes (old_point, mark)))
	{
		html_engine_unselect_all (engine, TRUE);
		old_point = mark;
	}

	delta_mark = new_point->position - mark->position;
	delta_point = new_point->position - old_point->position;

	/* (Notice that neither of the deltas can be zero at this
           point.)  */

	if (SAME_SIGN (delta_point, delta_mark))
		extend_selection (engine, old_point, new_point);
	else
		reduce_selection (engine, old_point, new_point);

	engine->active_selection = TRUE;
}


/**
 * html_engine_edit_selection_updater_new:
 * @engine: A CscHTML engine object.
 * 
 * Create a new updater associated with @engine.
 * 
 * Return value: A pointer to the new updater object.
 **/
HTMLEngineEditSelectionUpdater *
html_engine_edit_selection_updater_new (HTMLEngine *engine)
{
	HTMLEngineEditSelectionUpdater *new;

	g_return_val_if_fail (engine != NULL, NULL);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), NULL);

	new = g_new (HTMLEngineEditSelectionUpdater, 1);

	gtk_object_ref (GTK_OBJECT (engine));
	new->engine = engine;

	new->old_point = NULL;
	new->idle_id = 0;

	return new;
}

/**
 * html_engine_edit_selection_updater_destroy:
 * @updater: An HTMLEngineEditSelectionUpdater object.
 * 
 * Destroy @updater.
 **/
void
html_engine_edit_selection_updater_destroy (HTMLEngineEditSelectionUpdater *updater)
{
	g_return_if_fail (updater != NULL);

	if (updater->idle_id != 0)
		gtk_idle_remove (updater->idle_id);

	gtk_object_unref (GTK_OBJECT (updater->engine));

	if (updater->old_point != NULL)
		html_cursor_destroy (updater->old_point);

	g_free (updater);
}


static gint
updater_idle_callback (gpointer data)
{
	HTMLEngineEditSelectionUpdater *updater;
	HTMLEngine *engine;

	updater = (HTMLEngineEditSelectionUpdater *) data;
	engine = updater->engine;

	if (engine->mark != NULL) {
		if (updater->old_point == NULL)
			update_selection (engine, engine->mark, engine->cursor);
		else
			update_selection (engine, updater->old_point, engine->cursor);
	}

	if (updater->old_point != NULL)
		html_cursor_destroy (updater->old_point);

	if (engine->mark != NULL)
		updater->old_point = html_cursor_dup (engine->cursor);
	else
		updater->old_point = NULL;

	updater->idle_id = 0;
	return FALSE;
}

/**
 * html_engine_edit_selection_updater_schedule:
 * @updater: An HTMLEngineEditSelectionUpdater object.
 * 
 * Schedule an update for the keyboard-selected region on @updater.
 **/
void
html_engine_edit_selection_updater_schedule (HTMLEngineEditSelectionUpdater *updater)
{
	g_return_if_fail (updater != NULL);

	if (updater->idle_id != 0)
		return;

	updater->idle_id = gtk_idle_add (updater_idle_callback, updater);
}

/**
 * html_engine_edit_selection_updater_reset:
 * @updater: An HTMLEngineEditSelectionUpdater object.
 * 
 * Reset @updater so after no selection is active anymore.
 **/
void
html_engine_edit_selection_updater_reset (HTMLEngineEditSelectionUpdater *updater)
{
	g_return_if_fail (updater != NULL);

	if (updater->idle_id != 0) {
		gtk_idle_remove (updater->idle_id);
		updater->idle_id = 0;
	}

	if (updater->old_point != NULL) {
		html_cursor_destroy (updater->old_point);
		updater->old_point = NULL;
	}
}

/**
 * html_engine_edit_selection_update_now:
 * @updater: An HTMLEngineEditSelectionUpdater object.
 * 
 * Remove @updater idle callback and run's update callback immediately.
 **/
void
html_engine_edit_selection_update_now (HTMLEngineEditSelectionUpdater *updater)
{
	/* remove scheduled idle cb */
	if (updater->idle_id != 0) {
		gtk_idle_remove (updater->idle_id);
		updater->idle_id = 0;
	}

	/* run it now */
	updater_idle_callback (updater);
}
