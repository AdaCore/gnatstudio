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

#include "htmlclue.h"
#include "htmlclueflow.h"
#include "htmltext.h"
#include "htmltextmaster.h"

#include "htmlengine-edit-cursor.h"
#include "htmlengine-edit-delete.h"
#include "htmlengine-edit-movement.h"
#include "htmlengine-edit-paste.h"
#include "htmlengine-cutbuffer.h"


/* Utility functions.  */

static void
append (GList **list,
	GList **tail,
	gpointer data)
{
	*tail = g_list_append (*tail, data);
	if (*list == NULL)
		*list = *tail;
	else
		*tail = (* tail)->next;
}

static void
safe_remove (HTMLEngine *e,
	     HTMLObject *object)
{
	HTMLObject *p;
	HTMLCursor *cursor;

	cursor = e->cursor;

	if (object == cursor->object) {
		HTMLObject *prev, *next;

		prev = html_object_prev_not_slave (object);
		if (prev != NULL) {
			e->cursor->object = prev;
			if (html_object_is_text (prev))
				e->cursor->offset = HTML_TEXT (prev)->text_len;
			else
				e->cursor->offset = 1;
		} else {
			next = html_object_next_not_slave (object);

			if (next != NULL) {
				e->cursor->object = next;
				e->cursor->offset = 0;
			} else {
				HTMLObject *master;
				GdkColor black = { 0, 0, 0 };

				/* FIXME black.  */
				master = html_text_master_new("", CSC_HTML_FONT_STYLE_DEFAULT, &black, NULL);
				html_clue_prepend (HTML_CLUE (object->parent), master);

				e->cursor->object = master;
				e->cursor->offset = 0;
			}
		}
	}

	for (p = object->next;
	     p != NULL && HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE;
	     p = p->next) {
		html_clue_remove (HTML_CLUE (p->parent), p);
		html_object_destroy (p);
	}

	html_clue_remove (HTML_CLUE (object->parent), object);
}


/* Undo/redo support.  */

struct _ActionData {
	guint ref_count;
	GList *buffer;
	gboolean backwards;
};
typedef struct _ActionData ActionData;

static void  closure_destroy  (gpointer closure);
static void  do_redo          (HTMLEngine *engine, gpointer closure);
static void  do_undo          (HTMLEngine *engine, gpointer closure);
static void  setup_undo       (HTMLEngine *engine, ActionData *data);
static void  setup_redo       (HTMLEngine *engine, ActionData *data);

static void
closure_destroy (gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;

	g_assert (data->ref_count > 0);
	data->ref_count--;

	if (data->ref_count > 0)
		return;

	html_engine_cut_buffer_destroy (data->buffer);

	g_free (data);
}

static void
do_redo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;
	guint count;

	data = (ActionData *) closure;

	count = html_engine_cut_buffer_count (data->buffer);

	html_engine_delete (engine, count, FALSE, data->backwards);

	setup_undo (engine, data);
}

static void
setup_redo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *undo_action;

	data->ref_count ++;

	/* FIXME i18n */
	undo_action = html_undo_action_new ("paste",
					    do_redo,
					    closure_destroy,
					    data,
					    html_cursor_get_position (engine->cursor));

	html_undo_add_redo_action (engine->undo, undo_action);
}

static void
do_undo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;

	html_engine_paste_buffer (engine, data->buffer);

	/* FIXME: Instead of this ugly hackish way, there should be a
           flag so that we can prevent `html_engine_paste_buffer' from
           skipping the pasted text.  */
	if (! data->backwards)
		html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT,
					 html_engine_cut_buffer_count (data->buffer));

	setup_redo (engine, data);
}

static void
setup_undo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *undo_action;

	data->ref_count ++;

	/* FIXME i18n */
	undo_action = html_undo_action_new ("paste",
					    do_undo,
					    closure_destroy,
					    data,
					    html_cursor_get_position (engine->cursor));

	html_undo_add_undo_action (engine->undo, undo_action);
}

static ActionData *
create_action_data (GList *buffer,
		    gboolean backwards)
{
	ActionData *data;

	data = g_new (ActionData, 1);
	data->ref_count = 0;
	data->buffer = buffer;
	data->backwards = backwards;

	return data;
}


static void
append_to_buffer (GList **buffer,
		  GList **buffer_tail,
		  HTMLObject *object)
{
	HTMLObject *last_object;

	/* don't add "" */
	if (html_object_is_text (object) && !HTML_TEXT (object)->text [0])
		return;

	/* debug msg */
	g_print ("Adding object %p [%s] to cut_buffer.\n",
		 object, html_type_name (HTML_OBJECT_TYPE (object)));
	if (html_object_is_text (object))
		g_print ("\ttext `%s'\n", HTML_TEXT (object)->text);

	if (*buffer == NULL) {
		*buffer = *buffer_tail = g_list_append (NULL, object);
		return;
	}

	g_assert (*buffer_tail != NULL);

	last_object = HTML_OBJECT ((*buffer_tail)->data);

	if (html_object_is_text (object)
	    && html_object_is_text (last_object)
	    && html_text_check_merge (HTML_TEXT (object), HTML_TEXT (last_object))) {
		html_text_merge (HTML_TEXT (last_object), HTML_TEXT (object), FALSE);
		return;
	}

	*buffer_tail = g_list_append (*buffer_tail, object);
	if (buffer == NULL)
		*buffer = *buffer_tail;
	else
		*buffer_tail = (*buffer_tail)->next;
}


static void
delete_same_parent (HTMLEngine *e,
		    HTMLObject *start_object,
		    gboolean destroy_start,
		    GList **buffer,
		    GList **buffer_last)
{
	HTMLObject *parent;
	HTMLObject *p, *pnext;

	parent = start_object->parent;

	if (destroy_start)
		p = start_object;
	else
		p = html_object_next_not_slave (start_object);

	while (p != NULL && p != e->cursor->object) {
		pnext = p->next;

		html_clue_remove (HTML_CLUE (p->parent), p);

		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE)
			html_object_destroy (p);
		else
			append_to_buffer (buffer, buffer_last, p);

		p = pnext;
	}
}

static void
add_par_objects (HTMLClue *par, HTMLObject *end,
		 GList **buffer,
		 GList **buffer_last,
		 gboolean dest)
{
	HTMLObject *obj;

	/* now append all object from this paragraph */
	g_assert (HTML_OBJECT_TYPE (par) == HTML_TYPE_CLUEFLOW);
	obj = HTML_CLUE (par)->head;
	while (obj && obj != end) {
		/* we need dup as obj will be destroyed by flow destroy */
		if (HTML_OBJECT_TYPE (obj) != HTML_TYPE_TEXTSLAVE)
			append_to_buffer (buffer, buffer_last, html_object_dup (obj));
		if (dest) {
			html_clue_remove (par, obj);
			html_object_destroy (obj);
		}
		obj = obj->next;
	}

}

/* This destroys object from the cursor backwards to the specified
   `start_object'.  */
static void
delete_different_parent (HTMLEngine *e,
			 HTMLObject *start_object,
			 gboolean destroy_start,
			 GList **buffer,
			 GList **buffer_last)
{
	HTMLObject *p, *pnext;
	HTMLObject *end_paragraph;

	if (destroy_start)
		p = start_object;
	else
		p = html_object_next_not_slave (start_object);

	/* First destroy the elements in the `start_object's paragraph.  */

	while (p != NULL) {
		html_clue_remove (HTML_CLUE (start_object->parent), p);

		pnext = p->next;

		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE)
			html_object_destroy (p);
		else
			append_to_buffer (buffer, buffer_last, p);

		p = pnext;
	}

	/* Then destroy all the paragraphs from the one after `start_object's, until the
	   cursor's paragraph.  Of course, we don't destroy the cursor's paragraph.  */

	p = start_object->parent->next;
	while (1) {
		if (p == e->cursor->object->parent)
			break;

		pnext = p->next;

		if (p->parent != NULL)
			html_clue_remove (HTML_CLUE (p->parent), p);

		append_to_buffer (buffer, buffer_last, html_object_dup (p));
		add_par_objects (HTML_CLUE (p), NULL, buffer, buffer_last, FALSE);

		html_object_destroy (p);
		p = pnext;
	}

	/* Destroy the elements before the cursor object.  */
	/* FIXME ugly cut & pasted code.  */

	p = HTML_CLUE (e->cursor->object->parent)->head;
	while (p != NULL && p != e->cursor->object) {
		pnext = p->next;

		html_clue_remove (HTML_CLUE (p->parent), p);

		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE)
			html_object_destroy (p);
		else
			append_to_buffer (buffer, buffer_last, p);

		p = pnext;
	}

	/* Copy elements from the cursor to the end into the starting paragraph.  */

	end_paragraph = e->cursor->object->parent;
	g_assert (end_paragraph != NULL);

	g_assert (p == e->cursor->object);
	while (p != NULL) {
		pnext = p->next;

		html_clue_remove (HTML_CLUE (p->parent), p);
		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE) {
			html_object_destroy (p);
		} else {
			append_to_buffer (buffer, buffer_last, html_object_dup (p));
			html_clue_append (HTML_CLUE (start_object->parent), p);
		}

		p = pnext;
	}

	/* Remove the end paragraph.  */

	html_clue_remove (HTML_CLUE (end_paragraph->parent), end_paragraph);
	html_object_destroy (end_paragraph);
}

static void
destroy_slaves_for_merge (HTMLObject *a,
			  HTMLObject *b)
{
	HTMLObject *p;
	HTMLObject *pnext;
	gboolean slaves_only;

	slaves_only = FALSE;
	for (p = a->next; p != NULL; p = pnext) {
		pnext = p->next;

		if (slaves_only && HTML_OBJECT_TYPE (p) != HTML_TYPE_TEXTSLAVE)
			break;

		html_clue_remove (HTML_CLUE (p->parent), p);
		html_object_destroy (p);

		if (p == b)
			slaves_only = TRUE;
	}
}

static void
merge_text_at_cursor (HTMLEngine *e)
{
	HTMLCursor *cursor;
	HTMLObject *object;
	HTMLObject *prev;
	HTMLObject *next;

	cursor = e->cursor;
	object = cursor->object;

	prev = html_object_prev_not_slave (object);
	if (prev != NULL
	    && html_object_is_text (prev)
	    && html_text_check_merge (HTML_TEXT (prev), HTML_TEXT (object))) {
		cursor->object = prev;
		cursor->offset += HTML_TEXT (prev)->text_len;
		html_text_merge (HTML_TEXT (prev), HTML_TEXT (object), FALSE);
		destroy_slaves_for_merge (prev, object);
	}

	object = cursor->object;

	next = html_object_next_not_slave (object);
	if (next != NULL
	    && html_object_is_text (next)
	    && html_text_check_merge (HTML_TEXT (next), HTML_TEXT (object))) {
		html_text_merge (HTML_TEXT (object), HTML_TEXT (next), FALSE);
		destroy_slaves_for_merge (object, next);
	}
}

static void
remove_empty_text_at_cursor_if_necessary (HTMLEngine *e)
{
	HTMLObject *object;
	HTMLObject *prev;
	HTMLObject *next;

	object = e->cursor->object;

	g_assert (html_object_is_text (object));
	g_assert (HTML_TEXT (object)->text_len == 0);
	g_assert (e->cursor->offset == 0);

	prev = html_object_prev_not_slave (object);
	next = html_object_next_not_slave (object);

	if (prev == NULL && next == NULL)
		return;

	if (prev != NULL) {
		e->cursor->object = prev;
		if (html_object_is_text (prev))
			e->cursor->offset = HTML_TEXT (prev)->text_len;
		else
			e->cursor->offset = 1;
	} else {
		e->cursor->object = next;
		e->cursor->offset = 0;
	}

	html_clue_remove (HTML_CLUE (object->parent), object);
	html_object_destroy (object);
}


static HTMLObject *
delete_in_object (HTMLEngine *e,
		  HTMLObject *object,
		  guint start_offset,
		  guint end_offset)
{
	HTMLText *extracted_text;

	if (end_offset == start_offset)
		return NULL;

	if (! html_object_is_text (object)) {
		if (start_offset == 0 && end_offset > 0) {
			safe_remove (e, object);
			return object;
		} else {
			return NULL;
		}
	}

	if (start_offset == 0 && end_offset >= HTML_TEXT (object)->text_len) {
		safe_remove (e, object);
		return object;
	}

	extracted_text = html_text_extract_text (HTML_TEXT (object),
						 start_offset, end_offset - start_offset);
	html_text_remove_text (HTML_TEXT (object), e, start_offset, end_offset - start_offset);

	if (e->cursor->object == object && e->cursor->offset > start_offset) {
		if (end_offset - start_offset < e->cursor->offset)
			e->cursor->offset -= end_offset - start_offset;
		else
			e->cursor->offset = 0;
	}

	return HTML_OBJECT (extracted_text);
}

/**
 * html_engine_delete:
 * @e: 
 * @amount:
 * @do_undo: Whether we want to save undo information for this operation
 * 
 * Delete @count characters forward, starting at the current cursor position.
 **/
void
html_engine_delete (HTMLEngine *e,
		    guint count,
		    gboolean do_undo,
		    gboolean backwards)
{
	HTMLObject *start_object;
	guint start_offset;
	guint start_position;
	GList *save_buffer;
	GList *save_buffer_tail;
	HTMLObject *obj;
	HTMLClueFlow *start_parent;
	gboolean different_parent;

	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	if (e->cursor->object->parent == NULL || e->cursor->object->parent == NULL)
		return;

	if (count == 0)
		return;

	if (backwards)
		count = html_engine_move_cursor (e, HTML_ENGINE_CURSOR_LEFT, count);

	save_buffer = NULL;
	save_buffer_tail = NULL;

	html_engine_hide_cursor (e);

	start_object = e->cursor->object;
	start_parent = HTML_CLUEFLOW (start_object->parent);
	start_offset = e->cursor->offset;
	start_position = e->cursor->position;

	/* Find the other end.  */
	html_engine_move_cursor (e, HTML_ENGINE_CURSOR_RIGHT, count);

	different_parent = FALSE;

	if (e->cursor->object == start_object) {
		obj = delete_in_object (e, start_object, start_offset, e->cursor->offset);
		if (obj != NULL)
			append (&save_buffer, &save_buffer_tail, obj);
	} else {
		if (start_object->parent != e->cursor->object->parent) {
			different_parent = TRUE;
			delete_different_parent (e, start_object, FALSE,
						 &save_buffer, &save_buffer_tail);
		} else {
			delete_same_parent (e, start_object, FALSE,
					    &save_buffer, &save_buffer_tail);
		}

		obj = delete_in_object (e, start_object, start_offset, (guint) -1);
		if (obj != NULL) {
			save_buffer = g_list_prepend (save_buffer, obj);
			if (save_buffer_tail == NULL)
				save_buffer_tail = save_buffer;
		}

		obj = delete_in_object (e, e->cursor->object, 0, e->cursor->offset);
		if (obj != NULL)
			append (&save_buffer, &save_buffer_tail, obj);
	}

	if (html_object_is_text (e->cursor->object)) {
		merge_text_at_cursor (e);

		if (HTML_TEXT (e->cursor->object)->text_len == 0) {
			/* FIXME: this is broken queueing this draw is far to agressive in what 
			   it redraws, but since this only happens once in a while and it leaves
			   bad artifacts without the redraw we will hack it in for now */

			html_engine_queue_draw (e, e->cursor->object->parent->parent);
			remove_empty_text_at_cursor_if_necessary (e);
		}
	}

	html_object_relayout (e->cursor->object->parent->parent, e,
			      e->cursor->object->parent);

	e->cursor->position = start_position;

	html_cursor_normalize (e->cursor);
	html_engine_show_cursor (e);

	if (do_undo) {
		/* FIXME this is a nasty workaround for the common case.  The above code
                   to set up the save buffer is utterly broken and we should use the code
                   in htmlengine-edit-copy instead.  */
		if (count == 1 && different_parent) {
			HTMLClueFlow *clueflow_orig;
			HTMLObject *clueflow_copy;

			html_engine_cut_buffer_destroy (save_buffer);

			clueflow_orig = HTML_CLUEFLOW (start_parent);
			clueflow_copy = html_clueflow_new (clueflow_orig->style,
							   clueflow_orig->level);

			save_buffer = g_list_prepend (NULL, clueflow_copy);
		}

		setup_undo (e, create_action_data (save_buffer, backwards));
	}
}


