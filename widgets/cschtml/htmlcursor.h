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

   Author: Ettore Perazzoli <ettore@helixcode.com>
*/

#ifndef HTMLCURSOR_H
#define HTMLCURSOR_H

#include <glib.h>

#include "htmlobject.h"
#include "htmlengine.h"


struct _HTMLCursor {
	HTMLObject *object;
	guint offset;

	gint target_x;
	guint have_target_x : 1;

	gint position;
};
typedef struct _HTMLCursor HTMLCursor;


/* Lifecycle.  */
HTMLCursor *html_cursor_new      (void);
void        html_cursor_destroy  (HTMLCursor       *cursor);
HTMLCursor *html_cursor_dup      (const HTMLCursor *cursor);
void        html_cursor_copy     (HTMLCursor       *dest,
				  const HTMLCursor *src);
/* Basic movement.   */
void      html_cursor_home                   (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_forward                (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_backward               (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_up                     (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_down                   (HTMLCursor *cursor,
					      HTMLEngine *engine);
void      html_cursor_beginning_of_document  (HTMLCursor *cursor,
					      HTMLEngine *engine);
void      html_cursor_end_of_document        (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_beginning_of_line      (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_end_of_line            (HTMLCursor *cursor,
					      HTMLEngine *engine);
gboolean  html_cursor_jump_to                (HTMLCursor *cursor,
					      HTMLEngine *engine,
					      HTMLObject *obj,
					      guint       offset);

/* Utility functions for moving around the tree in cursor order.  */
HTMLObject *html_object_next_for_cursor  (HTMLObject *object);
HTMLObject *html_object_prev_for_cursor  (HTMLObject *object);

/* Internals.  */
void  html_cursor_normalize     (HTMLCursor *cursor);

/* Position handling.  */
gint  html_cursor_get_position      (HTMLCursor *cursor);
void  html_cursor_jump_to_position  (HTMLCursor *cursor,
				     HTMLEngine *engine,
				     gint        position);

/* Retrieving the character under the cursor.  */
gchar  html_cursor_get_current_char  (const HTMLCursor *cursor);

/* Comparison.  */
gboolean  html_cursor_equal     (const HTMLCursor *a,
				 const HTMLCursor *b);
gboolean  html_cursor_precedes  (const HTMLCursor *a,
				 const HTMLCursor *b);
gboolean  html_cursor_follows   (const HTMLCursor *a,
				 const HTMLCursor *b);

#endif
