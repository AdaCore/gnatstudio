/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

   Copyright 1999, 2000 Helix Code, Inc.

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

/* This file is a bit of a hack.  To make things work in a really nice way, we
   should have some extra methods in the various subclasses to implement cursor
   movement.  But for now, I think this is a reasonable way to get things to
   work.  */

#include <glib.h>

#include "htmlclue.h"
#include "htmltext.h"
#include "htmltextslave.h"
#include "htmltype.h"

#include "htmlcursor.h"


#define _HTML_CURSOR_DEBUG


#ifdef _HTML_CURSOR_DEBUG
static void
debug_location (const HTMLCursor *cursor)
{
	HTMLObject *object;

	object = cursor->object;
	if (object == NULL) {
		g_print ("Cursor has no position.\n");
		return;
	}

	g_print ("Cursor in %s (%p), offset %d, position %d\n",
		 html_type_name (HTML_OBJECT_TYPE (object)),
		 object, cursor->offset, cursor->position);
}
#else
#define debug_location(cursor)
#endif


static void
normalize (HTMLObject **object,
	   guint *offset)
{
	if (*offset == 0 && (*object)->prev != NULL) {
		HTMLObject *p;

		p = (*object)->prev;
		while (p != NULL && HTML_OBJECT_TYPE (p) == HTML_TYPE_TEXTSLAVE)
			p = p->prev;

		*object = p;
		if (html_object_is_text (p))
			*offset = HTML_TEXT (p)->text_len;
		else
			*offset = 1;
	}
}


HTMLCursor *
html_cursor_new (void)
{
	HTMLCursor *new;

	new = g_new (HTMLCursor, 1);
	new->object = NULL;
	new->offset = 0;

	new->target_x = 0;
	new->have_target_x = FALSE;

	new->position = 0;

	return new;
}

void
html_cursor_destroy (HTMLCursor *cursor)
{
	g_return_if_fail (cursor != NULL);

	g_free (cursor);
}

/**
 * html_cursor_copy:
 * @dest: A cursor object to copy into 
 * @src: A cursor object to copy from
 * 
 * Copy @src into @dest.  @dest does not need to be an initialized cursor, so
 * for example declaring a cursor as a local variable and then calling
 * html_cursor_copy() to initialize it from another cursor's position works.
 **/
void
html_cursor_copy (HTMLCursor *dest,
		  const HTMLCursor *src)
{
	g_return_if_fail (dest != NULL);
	g_return_if_fail (src != NULL);

	dest->object = src->object;
	dest->offset = src->offset;
	dest->target_x = src->target_x;
	dest->have_target_x = src->have_target_x;
	dest->position = src->position;
}

HTMLCursor *
html_cursor_dup (const HTMLCursor *cursor)
{
	HTMLCursor *new;

	new = html_cursor_new ();
	html_cursor_copy (new, cursor);

	return new;
}

void
html_cursor_normalize (HTMLCursor *cursor)
{
	g_return_if_fail (cursor != NULL);

	normalize (&cursor->object, &cursor->offset);
}


/* This is a gross hack as we don't have a `is_a()' function in the object
   system.  */

static gboolean
is_clue (HTMLObject *object)
{
	HTMLType type;

	type = HTML_OBJECT_TYPE (object);

	return (type == HTML_TYPE_CLUE || type == HTML_TYPE_CLUEV
		|| type == HTML_TYPE_CLUEH || type == HTML_TYPE_CLUEFLOW);
}

static HTMLObject *
next (HTMLObject *object)
{
	while (object->next == NULL) {
		if (object->parent == NULL) {
			object = NULL;
			break;
		}
		object = object->parent;
	}

	if (object == NULL)
		return NULL;
	else
		return object->next;
}


void
html_cursor_home (HTMLCursor *cursor,
		  HTMLEngine *engine)
{
	HTMLObject *obj;

	g_return_if_fail (cursor != NULL);
	g_return_if_fail (engine != NULL);

	if (engine->clue == NULL) {
		cursor->object = NULL;
		cursor->offset = 0;
		return;
	}

	obj = engine->clue;
	while (is_clue (obj))
		obj = HTML_CLUE (obj)->head;

	cursor->object = obj;
	cursor->offset = 0;
	cursor->position = 0;

	debug_location (cursor);
}


static HTMLObject *
forward_object (HTMLObject *obj)
{
	obj = next (obj);

	/* Traverse the tree in top-bottom, left-right order until an element
           that accepts the cursor is found.  */

	while (obj != NULL) {
		if (is_clue (obj)) {
			obj = HTML_CLUE (obj)->head;
			continue;
		}

		if (html_object_accepts_cursor (obj))
			break;

		obj = next (obj);
	}

	if (obj == NULL)
		return NULL;

	return obj;
}

static gboolean
forward (HTMLCursor *cursor,
	 HTMLEngine *engine)
{
	HTMLObject *obj;
	guint offset;

	obj = cursor->object;
	if (obj == NULL) {
		/* FIXME this is probably not the way it should be.  */
		g_warning ("The cursor is in a NULL position: going home.");
		html_cursor_home (cursor, engine);
		return TRUE;
	}		

	offset = cursor->offset;
	obj = cursor->object;

	if (html_object_is_text (obj)) {
		switch (HTML_OBJECT_TYPE (obj)) {
		case HTML_TYPE_TEXTMASTER:
		case HTML_TYPE_LINKTEXTMASTER:
		{
			HTMLText *text;

			text = HTML_TEXT (obj);

			if (text->text_len == 0)
				break;

			if (offset < text->text_len) {
				cursor->offset++;
				cursor->position++;
				return TRUE;
			}

			if (html_object_next_not_slave (obj) == NULL)
				offset = 0;
			else
				offset = 1;
			break;
		}

		case HTML_TYPE_TEXTSLAVE:
			/* Do nothing: go to the next element.  */
			break;

		default:
			g_assert_not_reached ();
		}
	} else if (! is_clue (obj)) {
		/* Objects that are not a clue or text are always skipped, as
                   they are a unique non-splittable element.  But if the
                   element is the last in a clue, then we let the offset be 1
                   so that you have the cursor positioned at the end of the
                   clue.  */

		if (offset == 0) {
			cursor->offset = 1;
			cursor->position++;
			return TRUE;
		}
	}

	obj = forward_object (cursor->object);
	if (obj == NULL)
		return FALSE;

	cursor->object = obj;
	cursor->offset = 0;
	cursor->position++;

	if (cursor->object->prev != NULL)
		cursor->offset++;

	return TRUE;
}

gboolean
html_cursor_forward (HTMLCursor *cursor,
		     HTMLEngine *engine)
{
	gboolean retval;

	g_return_val_if_fail (cursor != NULL, FALSE);
	g_return_val_if_fail (engine != NULL, FALSE);

	cursor->have_target_x = FALSE;
	retval = forward (cursor, engine);

	debug_location (cursor);

	return retval;
}

HTMLObject *
html_object_next_for_cursor (HTMLObject *obj)
{
	g_return_val_if_fail (obj != NULL, NULL);

	return forward_object (obj);
}


static HTMLObject *
backward_object (HTMLObject *obj)
{
	while (obj != NULL) {
		while (obj != NULL && obj->prev == NULL)
			obj = obj->parent;

		if (obj == NULL)
			break;

		obj = obj->prev;
		if (is_clue (obj)) {
			while (is_clue (obj) && HTML_CLUE (obj)->head != NULL) {
				obj = HTML_CLUE (obj)->head;
				while (obj->next != NULL)
					obj = obj->next;
			}
		}

		if (html_object_is_text (obj)) {
			switch (HTML_OBJECT_TYPE (obj)) {
			case HTML_TYPE_TEXT:
			case HTML_TYPE_LINKTEXT:
			case HTML_TYPE_TEXTMASTER:
			case HTML_TYPE_LINKTEXTMASTER:
				return obj;

			case HTML_TYPE_TEXTSLAVE:
				/* Do nothing: go to the previous element, as
				   this is not a suitable place for the
				   cursor.  */
				break;

			default:
				g_assert_not_reached ();
			}
		} else if (html_object_accepts_cursor (obj)) {
			return obj;
		}
	}

	return NULL;
}

static gboolean
backward (HTMLCursor *cursor,
	  HTMLEngine *engine)
{
	HTMLObject *obj;
	guint offset;

	obj = cursor->object;
	if (obj == NULL) {
		/* FIXME this is probably not the way it should be.  */
		g_warning ("The cursor is in a NULL position: going home.");
		html_cursor_home (cursor, engine);
		return TRUE;
	}		

	offset = cursor->offset;
	obj = cursor->object;

	if (html_object_is_text (obj)) {
		switch (HTML_OBJECT_TYPE (obj)) {
		case HTML_TYPE_TEXT:
		case HTML_TYPE_LINKTEXT:
		case HTML_TYPE_TEXTMASTER:
		case HTML_TYPE_LINKTEXTMASTER:
			if (offset > 1 || (offset == 1 && obj->prev == NULL)) {
				cursor->offset = offset - 1;
				cursor->position--;
				return TRUE;
			}
			break;

		case HTML_TYPE_TEXTSLAVE:
			/* Do nothing: go to the previous element, as this is
			   not a suitable place for the cursor.  */
			break;

		default:
			g_assert_not_reached ();
		}
	} else if (offset > 0 && obj->prev == NULL) {
		cursor->offset = 0;
		cursor->position--;
		return TRUE;
	}

	obj = backward_object (obj);
	if (obj == NULL)
		return FALSE;

	if (html_object_is_text (obj))
		cursor->offset = HTML_TEXT (obj)->text_len;
	else
		cursor->offset = 1;

	cursor->object = obj;
	cursor->position--;

	return TRUE;
}

gboolean
html_cursor_backward (HTMLCursor *cursor,
		      HTMLEngine *engine)
{
	gboolean retval;

	g_return_val_if_fail (cursor != NULL, FALSE);
	g_return_val_if_fail (engine != NULL, FALSE);

	cursor->have_target_x = FALSE;
	retval = backward (cursor, engine);

	debug_location (cursor);

	return retval;
}

HTMLObject *
html_object_prev_for_cursor (HTMLObject *object)
{
	g_return_val_if_fail (object != NULL, NULL);

	return backward_object (object);
}


gboolean
html_cursor_up (HTMLCursor *cursor,
		HTMLEngine *engine)
{
	HTMLCursor orig_cursor;
	HTMLCursor prev_cursor;
	gint prev_x, prev_y;
	gint x, y;
	gint target_x;
	gint orig_y;
	gboolean new_line;

	if (cursor->object == NULL) {
		g_warning ("The cursor is in a NULL position: going home.");
		html_cursor_home (cursor, engine);
		return TRUE;
	}

	html_cursor_copy (&orig_cursor, cursor);

	html_object_get_cursor_base (cursor->object,
				     engine->painter, cursor->offset,
				     &x, &y);

	if (! cursor->have_target_x) {
		cursor->target_x = x;
		cursor->have_target_x = TRUE;
	}

	target_x = cursor->target_x;

	orig_y = y;

	new_line = FALSE;

	while (1) {
		html_cursor_copy (&prev_cursor, cursor);

		prev_x = x;
		prev_y = y;

		if (! backward (cursor, engine))
			return FALSE;

		html_object_get_cursor_base (cursor->object,
					     engine->painter, cursor->offset,
					     &x, &y);

		if (html_cursor_equal (&prev_cursor, cursor)) {
			html_cursor_copy (cursor, &orig_cursor);
			return FALSE;
		}

		if (prev_y != y) {
			if (new_line) {
				html_cursor_copy (cursor, &prev_cursor);
				return FALSE;
			}

			new_line = TRUE;
		}

		if (new_line && x <= target_x) {
			if (! cursor->have_target_x) {
				cursor->have_target_x = TRUE;
				cursor->target_x = target_x;
			}

			/* Choose the character which is the nearest to the
                           target X.  */
			if (prev_y == y && target_x - x >= prev_x - target_x) {
				cursor->object = prev_cursor.object;
				cursor->offset = prev_cursor.offset;
				cursor->position = prev_cursor.position;
			}

			debug_location (cursor);
			return TRUE;
		}
	}
}


gboolean
html_cursor_down (HTMLCursor *cursor,
		  HTMLEngine *engine)
{
	HTMLCursor orig_cursor;
	HTMLCursor prev_cursor;
	gint prev_x, prev_y;
	gint x, y;
	gint target_x;
	gint orig_y;
	gboolean new_line;

	if (cursor->object == NULL) {
		g_warning ("The cursor is in a NULL position: going home.");
		html_cursor_home (cursor, engine);
		return TRUE;
	}

	html_object_get_cursor_base (cursor->object,
				     engine->painter, cursor->offset,
				     &x, &y);

	if (! cursor->have_target_x) {
		cursor->target_x = x;
		cursor->have_target_x = TRUE;
	}

	target_x = cursor->target_x;

	orig_y = y;

	new_line = FALSE;

	while (1) {
		prev_cursor = *cursor;
		prev_x = x;
		prev_y = y;

		if (! forward (cursor, engine))
			return FALSE;

		html_object_get_cursor_base (cursor->object,
					     engine->painter, cursor->offset,
					     &x, &y);

		if (html_cursor_equal (&prev_cursor, cursor)) {
			html_cursor_copy (cursor, &orig_cursor);
			return FALSE;
		}

		if (prev_y != y) {
			if (new_line) {
				html_cursor_copy (cursor, &prev_cursor);
				return FALSE;
			}

			new_line = TRUE;
		}

		if (new_line && x >= target_x) {
			if (! cursor->have_target_x) {
				cursor->have_target_x = TRUE;
				cursor->target_x = target_x;
			}

			/* Choose the character which is the nearest to the
                           target X.  */
			if (prev_y == y && x - target_x >= target_x - prev_x) {
				cursor->object = prev_cursor.object;
				cursor->offset = prev_cursor.offset;
				cursor->position = prev_cursor.position;
			}

			debug_location (cursor);
			return TRUE;
		}
	}
}


/**
 * html_cursor_jump_to:
 * @cursor: 
 * @object: 
 * @offset: 
 * 
 * Move the cursor to the specified @offset in the specified @object.
 * 
 * Return value: %TRUE if successfull, %FALSE if failed.
 **/
gboolean
html_cursor_jump_to (HTMLCursor *cursor,
		     HTMLEngine *engine,
		     HTMLObject *object,
		     guint offset)
{
	HTMLCursor original;

	g_return_val_if_fail (cursor != NULL, FALSE);
	g_return_val_if_fail (object != NULL, FALSE);

	html_cursor_normalize (cursor);
	normalize (&object, &offset);

	if (cursor->object == object && cursor->offset == offset)
		return TRUE;

	html_cursor_copy (&original, cursor);

	while (forward (cursor, engine)) {
		if (cursor->object == object && cursor->offset == offset)
			return TRUE;
	}

	html_cursor_copy (cursor, &original);

	while (backward (cursor, engine)) {
		if (cursor->object == object && cursor->offset == offset)
			return TRUE;
	}

	return FALSE;
}


/* Complex cursor movement commands.  */

void
html_cursor_beginning_of_document (HTMLCursor *cursor,
				   HTMLEngine *engine)
{
	g_return_if_fail (cursor != NULL);
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	while (backward (cursor, engine))
		;
}

void
html_cursor_end_of_document (HTMLCursor *cursor,
			     HTMLEngine *engine)
{
	g_return_if_fail (cursor != NULL);
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	while (forward (cursor, engine))
		;
}

gboolean
html_cursor_end_of_line (HTMLCursor *cursor,
			 HTMLEngine *engine)
{
	HTMLCursor prev_cursor;
	gint x, y, prev_y;

	g_return_val_if_fail (cursor != NULL, FALSE);
	g_return_val_if_fail (engine != NULL, FALSE);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), FALSE);

	cursor->have_target_x = FALSE;

	html_cursor_copy (&prev_cursor, cursor);
	html_object_get_cursor_base (cursor->object, engine->painter, cursor->offset,
				     &x, &prev_y);

	while (1) {
		if (! forward (cursor, engine))
			return TRUE;

		html_object_get_cursor_base (cursor->object, engine->painter, cursor->offset,
					     &x, &y);

		if (y != prev_y) {
			html_cursor_copy (cursor, &prev_cursor);
			return TRUE;
		}

		html_cursor_copy (&prev_cursor, cursor);
	}
}

gboolean
html_cursor_beginning_of_line (HTMLCursor *cursor,
			       HTMLEngine *engine)
{
	HTMLCursor prev_cursor;
	gint x, y, prev_y;

	g_return_val_if_fail (cursor != NULL, FALSE);
	g_return_val_if_fail (engine != NULL, FALSE);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), FALSE);

	cursor->have_target_x = FALSE;

	html_cursor_copy (&prev_cursor, cursor);
	html_object_get_cursor_base (cursor->object, engine->painter, cursor->offset,
				     &x, &prev_y);

	while (1) {
		if (! backward (cursor, engine))
			return TRUE;

		html_object_get_cursor_base (cursor->object, engine->painter, cursor->offset,
					     &x, &y);

		if (y != prev_y) {
			html_cursor_copy (cursor, &prev_cursor);
			return TRUE;
		}

		html_cursor_copy (&prev_cursor, cursor);
	}
}


gint
html_cursor_get_position (HTMLCursor *cursor)
{
	g_return_val_if_fail (cursor != NULL, 0);

	return cursor->position;
}

void
html_cursor_jump_to_position (HTMLCursor *cursor,
			      HTMLEngine *engine,
			      gint position)
{
	g_return_if_fail (cursor != NULL);
	g_return_if_fail (position >= 0);

	if (cursor->position < position) {
		while (cursor->position < position) {
			if (! forward (cursor, engine))
				break;
		}
	} else if (cursor->position > position) {
		while (cursor->position > position) {
			if (! backward (cursor, engine))
				break;
		}
	}
}


/* Comparison.  */

gboolean
html_cursor_equal (const HTMLCursor *a,
		   const HTMLCursor *b)
{
	g_return_val_if_fail (a != NULL, FALSE);
	g_return_val_if_fail (b != NULL, FALSE);

	return a->object == b->object && a->offset == b->offset;
}

gboolean
html_cursor_precedes (const HTMLCursor *a,
		      const HTMLCursor *b)
{
	g_return_val_if_fail (a != NULL, FALSE);
	g_return_val_if_fail (b != NULL, FALSE);

	return a->position < b->position;
}

gboolean
html_cursor_follows (const HTMLCursor *a,
		     const HTMLCursor *b)
{
	g_return_val_if_fail (a != NULL, FALSE);
	g_return_val_if_fail (b != NULL, FALSE);

	return a->position > b->position;
}


gchar
html_cursor_get_current_char (const HTMLCursor *cursor)
{
	HTMLObject *next;

	g_return_val_if_fail (cursor != NULL, 0);

	if (! html_object_is_text (cursor->object)) {
		if (cursor->offset == 0)
			return 0;

		next = cursor->object->next;
		if (next != NULL && html_object_is_text (next))
			return HTML_TEXT (next)->text[0];

		return 0;
	}

	if (cursor->offset < HTML_TEXT (cursor->object)->text_len)
		return HTML_TEXT (cursor->object)->text[cursor->offset];

	next = html_object_next_not_slave (cursor->object);

	if (next == NULL || ! html_object_is_text (next))
		return 0;

	return HTML_TEXT (next)->text[0];
}
