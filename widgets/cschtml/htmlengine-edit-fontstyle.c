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

#include "cschtmldebug.h"
#include "htmlclue.h"
#include "htmltextmaster.h"
#include "htmlengine-edit-movement.h"

#include "htmlengine-edit-fontstyle.h"

/* #define PARANOID_DEBUG */


struct _FontStyleSegment {
	CscHTMLFontStyle style;
	guint count;
};
typedef struct _FontStyleSegment FontStyleSegment;

struct _ActionData {
	guint ref_count;

	/* List of style segments to restore.  */
	GList *segments;

	/* Number of characters selected.  */
	guint count;

	/* Whether the selection is forwards (cursor at the beginning)
           or backwards.  */
	gboolean backwards;

	/* How to change the style when redoing.  */
	CscHTMLFontStyle and_mask;
	CscHTMLFontStyle or_mask;
};
typedef struct _ActionData ActionData;

static void  	   closure_destroy    (gpointer closure);
static void  	   do_redo            (HTMLEngine *engine, gpointer closure);
static void  	   do_undo            (HTMLEngine *engine, gpointer closure);
static void  	   setup_undo         (HTMLEngine *engine, ActionData *data);
static void        setup_undo         (HTMLEngine *engine, ActionData *data);
static ActionData *create_action_data (CscHTMLFontStyle and_mask,
				       CscHTMLFontStyle or_mask,
				       guint count, gboolean backwards,
				       GList *orig_styles);


static void
prepend_style_segment (GList **segments,
		       CscHTMLFontStyle style,
		       guint count)
{
	FontStyleSegment *new_segment;

	new_segment = g_new (FontStyleSegment, 1);
	new_segment->style = style;
	new_segment->count = count;

	*segments = g_list_prepend (*segments, new_segment);
}


static void
update_cursor_for_merge (HTMLText *text,
			 HTMLText *other,
			 gboolean prepend,
			 HTMLCursor *cursor)
{
	if (cursor->object == HTML_OBJECT (other)) {
		if (! prepend)
			cursor->offset += text->text_len;
		cursor->object = HTML_OBJECT (text);
	} else if (cursor->object == HTML_OBJECT (text) && prepend) {
		cursor->offset += HTML_TEXT (other)->text_len;
	}
}

/* This merges @text with @other, prepending as defined by @prepend,
   and makes sure that @cursor is still valid (i.e. still points to a
   valid object) after this.  */
static void
merge_safely (HTMLText *text,
	      HTMLText *other,
	      gboolean prepend,
	      HTMLCursor *cursor,
	      HTMLCursor *mark)
{
	html_text_master_destroy_slaves (HTML_TEXT_MASTER (text));
	html_text_master_destroy_slaves (HTML_TEXT_MASTER (other));

	update_cursor_for_merge (text, other, prepend, cursor);
	if (mark != NULL)
		update_cursor_for_merge (text, other, prepend, mark);

	html_text_merge (text, other, prepend);
	html_clue_remove (HTML_CLUE (HTML_OBJECT (text)->parent), HTML_OBJECT (other));
	html_object_destroy (HTML_OBJECT (other));
}

static void
update_cursor_for_split (HTMLText *text,
			 HTMLText *new,
			 guint offset,
			 HTMLCursor *cursor)
{
	if (cursor->object == HTML_OBJECT (text) && cursor->offset >= offset) {
		cursor->object = HTML_OBJECT (new);
		cursor->offset -= offset;
	}
}

/* This splits making sure that @cursor is still valid (i.e. it still points to
   a valid offset in a valid object).  */
static HTMLText *
split_safely (HTMLText *text,
	      guint offset,
	      HTMLCursor *cursor,
	      HTMLCursor *mark)
{
	HTMLText *new;

	new = html_text_split (text, offset);

	update_cursor_for_split (text, new, offset, cursor);
	if (mark != NULL)
		update_cursor_for_split (text, new, offset, mark);

	return new;
}

/* This handles merging of consecutive text elements with the same
   properties.  */
static guint
merge_backward (HTMLObject *object,
		HTMLCursor *cursor,
		HTMLCursor *mark)
{
	CscHTMLFontStyle font_style;
	HTMLObject *p, *pprev;
	guint total_merge;

	font_style = HTML_TEXT (object)->font_style;
	total_merge = 0;

	for (p = object->prev; p != NULL; p = pprev) {
		pprev = p->prev;
		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE) {
			html_clue_remove (HTML_CLUE (p->parent), p);
			html_object_destroy (p);
		} else if (html_object_is_text (p)
			   && HTML_OBJECT_TYPE (object) == HTML_OBJECT_TYPE (p)
			   && html_text_check_merge (HTML_TEXT (object), HTML_TEXT (p))) {
			total_merge += HTML_TEXT (object)->text_len;
			merge_safely (HTML_TEXT (object), HTML_TEXT (p), TRUE, cursor, mark);
		} else {
			break;
		}
	}

	return total_merge;
}

static guint
merge_forward (HTMLObject *object,
	       HTMLCursor *cursor,
	       HTMLCursor *mark)
{
	CscHTMLFontStyle font_style;
	HTMLObject *p, *pnext;
	guint total_merge;

	font_style = HTML_TEXT (object)->font_style;
	total_merge = 0;

	for (p = object->next; p != NULL; p = pnext) {
		pnext = p->next;
		if (HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE) {
			html_clue_remove (HTML_CLUE (p->parent), p);
			html_object_destroy (p);
		} else if (html_object_is_text (p)
			   && HTML_OBJECT_TYPE (object) == HTML_OBJECT_TYPE (p)
			   && html_text_check_merge (HTML_TEXT (object), HTML_TEXT (p))) {
			total_merge += HTML_TEXT (object)->text_len;
			merge_safely (HTML_TEXT (object), HTML_TEXT (p), FALSE, cursor, mark);
		} else {
			break;
		}
	}

	return total_merge;
}


static void
figure_interval (HTMLObject *p,
		 HTMLCursor *cursor,
		 gboolean backwards,
		 guint count,
		 guint *start_return,
		 guint *end_return)
{
	guint start, end;

	start = 0;
	end = HTML_TEXT (p)->text_len;

	if (backwards) {
		if (cursor->object == p) {
			if (end > cursor->offset)
				end = cursor->offset;
		}

		if (end - start > count)
			start = end - count;
	} else {
		if (cursor->object == p) {
			if (start < cursor->offset)
				start = cursor->offset;
		}

		if (end - start > count)
			end = start + count;
	}

	*start_return = start;
	*end_return = end;

	printf ("%s %d %d %d (%d)\n", __FUNCTION__, count, start, end, backwards);
}

static GList *
set_font_style (HTMLEngine *engine,
		CscHTMLFontStyle and_mask,
		CscHTMLFontStyle or_mask,
		guint count,
		gboolean backwards,
		gboolean want_undo)
{
	CscHTMLFontStyle font_style;
	CscHTMLFontStyle last_font_style;
	HTMLObject *p, *pparent;
	HTMLObject *previous_parent;
	GList *orig_styles;
	guint total_merged;

	g_print ("%s -- setting %d elements.\n", __FUNCTION__, count);

	p = engine->cursor->object;
	last_font_style = CSC_HTML_FONT_STYLE_DEFAULT;
	orig_styles = NULL;

	previous_parent = NULL;
	while (count > 0) {
		HTMLTextMaster *curr;
		guint start, end;

		if (! html_object_is_text (p)) {
			count --;
			goto next;
		}
		curr = HTML_TEXT_MASTER (p);

		g_print ("Doing text -- %s\n", HTML_TEXT (curr)->text);

		figure_interval (p, engine->cursor, backwards, count, &start, &end);
		if (start == end) {
			g_print ("\tSkipped\n");
			goto next;
		}

		count -= end - start;
		printf ("\tSetting %d -- count now %d\n", end - start, count);

		font_style = (HTML_TEXT (p)->font_style & and_mask) | or_mask;
		if (font_style == HTML_TEXT (p)->font_style) {
			g_print ("\tSkipping\n");
			goto next;
		}

		/* Split at start.  */

		if (start > 0) {
			curr = HTML_TEXT_MASTER (split_safely (HTML_TEXT (p), start,
							       engine->cursor, engine->mark));
			g_print ("Splitting at start %p -> %p\n", p, curr);
			html_clue_append_after (HTML_CLUE (p->parent), HTML_OBJECT (curr), p);
			p = HTML_OBJECT (curr);
			end -= start;
			start = 0;
		}

		/* Split at end.  */

		if (end < HTML_TEXT (curr)->text_len) {
			HTMLText *new;

			new = split_safely (HTML_TEXT (curr), end, engine->cursor, engine->mark);
			g_print ("Splitting at end %p -> %p\n", curr, new);
			html_clue_append_after (HTML_CLUE (HTML_OBJECT (curr)->parent),
						HTML_OBJECT (new), HTML_OBJECT (curr));
		}

		/* Save the original style for undo.  */
		if (want_undo)
			prepend_style_segment (&orig_styles,
					       HTML_TEXT (curr)->font_style,
					       end - start);

		/* Finally set the style.  */
		html_text_set_font_style (HTML_TEXT (curr), NULL, font_style);

	next:
		/* At this point, we might have elements before or after us with the
		   same font style.  We don't want to have contiguous equal text
		   elements, so we merge more stuff here.  */

		total_merged = merge_backward (p, engine->cursor, engine->mark);
		if (backwards) {
			if (total_merged > count)
				count = 0;
			else
				count -= total_merged;
		}

		total_merged = merge_forward (p, engine->cursor, engine->mark);
		if (! backwards) {
			if (total_merged > count)
				count = 0;
			else
				count -= total_merged;
		}

		/* FIXME slow slow slow!  */
		html_object_relayout (p->parent->parent, engine, p->parent);
		html_engine_queue_draw (engine, p->parent);

		if (count == 0)
			break;

		pparent = p->parent;
		if (backwards)
			p = html_object_prev_for_cursor (p);
		else
			p = html_object_next_for_cursor (p);

		if (p == NULL)
			break;

		if (pparent != p->parent) {
			count--;
			if (count == 0)
				break;
		}

		if (p == NULL)
			break;
	}

	if (backwards) {
		if (engine->cursor->object->prev != NULL)
			merge_forward (engine->cursor->object->prev, engine->cursor, engine->mark);
		if (p != NULL)
			merge_backward (p, engine->cursor, engine->mark);
	} else {
		merge_backward (engine->cursor->object, engine->cursor, engine->mark);
		if (p != NULL)
			merge_forward (p, engine->cursor, engine->mark);
	}

	html_object_relayout (p->parent->parent, engine, p->parent);
	html_engine_queue_draw (engine, p->parent);

	html_cursor_normalize (engine->cursor);

	if (! want_undo)
		return NULL;

	orig_styles = g_list_reverse (orig_styles);

	return orig_styles;
}

static void
set_font_style_in_selection (HTMLEngine *engine,
			     CscHTMLFontStyle and_mask,
			     CscHTMLFontStyle or_mask,
			     gboolean want_undo)
{
	gboolean backwards;
	GList *orig_styles;
	guint count;
	gint mark_position, cursor_position;

	g_return_if_fail (engine->clue != NULL);
	g_return_if_fail (engine->mark != NULL);

#ifdef PARANOID_DEBUG
	g_print ("Tree before changing font style:\n");
	csc_html_debug_dump_tree_simple (engine->clue, 2);
#endif

	mark_position = engine->mark->position;
	cursor_position = engine->cursor->position;

	g_return_if_fail (mark_position != cursor_position);

	if (mark_position < cursor_position) {
		count = cursor_position - mark_position;
		backwards = TRUE;
	} else {
		count = mark_position - cursor_position;
		backwards = FALSE;
	}

	orig_styles = set_font_style (engine, and_mask, or_mask, count, backwards, want_undo);

#ifdef PARANOID_DEBUG
	g_print ("Tree after changing font style:\n");
	csc_html_debug_dump_tree_simple (engine->clue, 2);
#endif

	if (! want_undo)
		return;

	setup_undo (engine,
		    create_action_data (and_mask, or_mask, count, backwards, orig_styles));
}


static void
free_segment (FontStyleSegment *segment)
{
	g_free (segment);
}

static void
free_segment_list (GList *segments)
{
	GList *p;

	for (p = segments; p != NULL; p = p->next)
		free_segment ((FontStyleSegment *) p->data);
}

static void
closure_destroy (gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;
	g_assert (data->ref_count > 0);

	data->ref_count--;

	if (data->ref_count > 0)
		return;

	free_segment_list (data->segments);
	g_free (data);
}

static void
do_redo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;

	data = (ActionData *) closure;

	set_font_style (engine, data->and_mask, data->or_mask, data->count,
			! data->backwards, FALSE);

	setup_undo (engine, data);
}

static void
setup_redo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *action;

	data->ref_count ++;

	/* FIXME i18n */
	action = html_undo_action_new ("Font style change",
				       do_redo,
				       closure_destroy,
				       data,
				       html_cursor_get_position (engine->cursor));

	html_undo_add_redo_action (engine->undo, action);
}

static gboolean
move_to_next_text_segment_forwards (HTMLEngine *engine)
{
	HTMLCursor *cursor;
	gboolean retval;

	cursor = engine->cursor;

	while (1) {
		guint object_length;

		/* Check if there is a text element on the immediate right of the cursor.  */

		if (html_object_is_text (cursor->object))
			object_length = HTML_TEXT (cursor->object)->text_len;
		else
			object_length = 1;

		if (cursor->offset == object_length) {
			HTMLObject *next;

			next = html_object_next_not_slave (cursor->object);
			if (next != NULL && html_object_is_text (next)) {
				retval = TRUE;
				break;
			}
		} else if (html_object_is_text (cursor->object)) {
			retval = TRUE;
			break;
		}

		if (! html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_RIGHT, 1)) {
			retval = FALSE;
			break;
		}
	}

	return retval;
}

static gboolean
move_to_next_text_segment_backwards (HTMLEngine *engine)
{
	HTMLCursor *cursor;
	gboolean retval;

	cursor = engine->cursor;

	while (1) {
		/* Check if there is a text element on the immediate left of the cursor.  */

		if (cursor->offset == 0) {
			HTMLObject *prev;

			prev = html_object_prev_not_slave (cursor->object);
			if (prev != NULL && html_object_is_text (prev)) {
				retval = TRUE;
				break;
			}
		} else if (html_object_is_text (cursor->object)) {
			retval = TRUE;
			break;
		}

		if (! html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT, 1)) {
			retval = FALSE;
			break;
		}
	}

	return retval;
}

static void
do_undo (HTMLEngine *engine,
	 gpointer closure)
{
	ActionData *data;
	GList *p;

	data = (ActionData *) closure;

	for (p = data->segments; p != NULL; p = p->next) {
		FontStyleSegment *segment;

		segment = (FontStyleSegment *) p->data;

		set_font_style (engine, (CscHTMLFontStyle) 0,
				segment->style, segment->count,
				data->backwards, FALSE);

		if (data->backwards) {
			html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_LEFT,
						 segment->count);
			if (! move_to_next_text_segment_backwards (engine))
				break;
		} else {
			html_engine_move_cursor (engine, HTML_ENGINE_CURSOR_RIGHT,
						 segment->count);
			if (! move_to_next_text_segment_forwards (engine))
				break;
		}
	}

	setup_redo (engine, data);
}

static void
setup_undo (HTMLEngine *engine,
	    ActionData *data)
{
	HTMLUndoAction *action;

	data->ref_count ++;

	/* FIXME i18n */
	action = html_undo_action_new ("Font style change",
				       do_undo,
				       closure_destroy,
				       data,
				       html_cursor_get_position (engine->cursor));

	html_undo_add_undo_action (engine->undo, action);
}

static ActionData *
create_action_data (CscHTMLFontStyle and_mask,
		    CscHTMLFontStyle or_mask,
		    guint count,
		    gboolean backwards,
		    GList *orig_styles)
{
	ActionData *data;

	data = g_new (ActionData, 1);

	data->ref_count = 0;
	data->segments = orig_styles;
	data->count = count;
	data->backwards = backwards;
	data->and_mask = and_mask;
	data->or_mask = or_mask;

	return data;
}


static CscHTMLFontStyle
get_font_style_from_selection (HTMLEngine *engine)
{
	CscHTMLFontStyle style;
	CscHTMLFontStyle conflicts;
	gboolean backwards;
	gboolean first;
	HTMLObject *p;

	g_return_val_if_fail (engine->clue != NULL, CSC_HTML_FONT_STYLE_DEFAULT);
	g_assert (engine->mark != NULL);

	printf ("%s mark %p,%d cursor %p,%d\n",
		__FUNCTION__,
		engine->mark, engine->mark->position,
		engine->cursor, engine->cursor->position);

	if (engine->mark->position < engine->cursor->position)
		backwards = TRUE;
	else
		backwards = FALSE;

	style = CSC_HTML_FONT_STYLE_DEFAULT;
	conflicts = CSC_HTML_FONT_STYLE_DEFAULT;
	first = TRUE;

	p = engine->cursor->object;

	while (1) {
		if (html_object_is_text (p) && p->selected) {
			if (first) {
				style = HTML_TEXT (p)->font_style;
				first = FALSE;
			} else {
				conflicts |= HTML_TEXT (p)->font_style ^ style;
			}
		}

		if (p == engine->mark->object)
			break;

		if (backwards)
			p = html_object_prev_for_cursor (p);
		else
			p = html_object_next_for_cursor (p);

		g_assert (p != NULL);
	}

	return style & ~conflicts;
}

/**
 * html_engine_update_insertion_font_style:
 * @engine: An HTMLEngine
 * 
 * Update @engine's current insertion font style according to the
 * current selection and cursor position.
 * 
 * Return value: 
 **/
gboolean
html_engine_update_insertion_font_style (HTMLEngine *engine)
{
	HTMLObject *curr;
	CscHTMLFontStyle new_style;

	g_return_val_if_fail (engine != NULL, CSC_HTML_FONT_STYLE_DEFAULT);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), CSC_HTML_FONT_STYLE_DEFAULT);
	g_return_val_if_fail (engine->editable, CSC_HTML_FONT_STYLE_DEFAULT);

	curr = engine->cursor->object;

	if (curr == NULL)
		new_style = CSC_HTML_FONT_STYLE_DEFAULT;
	else if (! html_object_is_text (curr))
		new_style = CSC_HTML_FONT_STYLE_DEFAULT;
	else if (! engine->active_selection)
		new_style = HTML_TEXT (curr)->font_style;
	else
		new_style = get_font_style_from_selection (engine);

	if (new_style != engine->insertion_font_style) {
		engine->insertion_font_style = new_style;
		return TRUE;
	}

	return FALSE;
}

/**
 * html_engine_set_font_style:
 * @engine: An HTMLEngine
 * @style: An HTMLFontStyle
 * 
 * Set the current font style for `engine'.  This has the same semantics as the
 * bold, italics etc. buttons in a word processor, i.e.:
 *
 * - If there is a selection, the selection gets the specified style.
 *
 * - If there is no selection, the style gets "attached" to the cursor.  So
 *   inserting text after this will cause text to have this style.
 *
 * Instead of specifying an "absolute" style, we specify it as a "difference"
 * from the current one, through an AND mask and an OR mask.
 *
 **/
void
html_engine_set_font_style (HTMLEngine *engine,
			    CscHTMLFontStyle and_mask,
			    CscHTMLFontStyle or_mask)
{
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));
	g_return_if_fail (engine->editable);

	if (engine->active_selection) {
		html_engine_freeze (engine);
		set_font_style_in_selection (engine, and_mask, or_mask, TRUE);
		html_engine_thaw (engine);
		return;
	}

	engine->insertion_font_style &= and_mask;
	engine->insertion_font_style |= or_mask;
}
