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

/* The cut buffer is used to temporarily store parts of the documents.  The
   implementation is a quick & dirty hack, but should work well for our
   purposes.

   So basically, we just store all the leaf elements of the document in a list.
   We simply use HTMLClueFlows as end-of-paragraphs marker, and consequently
   they also carry information about the style and indentation of the paragraph
   they belong to.  This is used to emulate the common word processor behavior
   of associating the paragraph properties to a new line.  */


#include "htmlclueflow.h"
#include "htmlobject.h"
#include "htmltext.h"

#include "htmlengine-cutbuffer.h"


/**
 * html_engine_cut_buffer_destroy:
 * @cut_buffer: 
 * 
 * Destroy @cut_buffer.
 **/
void
html_engine_cut_buffer_destroy (GList *cut_buffer)
{
	GList *p;

	if (cut_buffer == NULL)
		return;

	for (p = cut_buffer; p != NULL; p = p->next)
		html_object_destroy (HTML_OBJECT (p->data));

	g_list_free (cut_buffer);
}


/**
 * html_engine_cut_buffer_dup:
 * @cut_buffer: 
 * 
 * Duplicate @cut_buffer.
 * 
 * Return value: 
 **/
GList *
html_engine_cut_buffer_dup (GList *cut_buffer)
{
	GList *new_buffer, *new_buffer_tail;
	GList *p;

	if (cut_buffer == NULL)
		return NULL;

	new_buffer = NULL;
	new_buffer_tail = NULL;

	for (p = cut_buffer; p != NULL; p = p->next) {
		HTMLObject *obj_copy;

		obj_copy = html_object_dup (HTML_OBJECT (p->data));

		new_buffer_tail = g_list_append (new_buffer_tail, obj_copy);
		if (new_buffer == NULL)
			new_buffer = new_buffer_tail;
		else
			new_buffer_tail = new_buffer_tail->next;
	}

	return new_buffer;
}

/**
 * html_engine_cut_buffer_count:
 * @cut_buffer: A cut buffer.
 * 
 * Count the number of cursor elements in the @cut_buffer.
 * 
 * Return value: the number of cursor elements in @cut_buffer.
 **/
guint
html_engine_cut_buffer_count (GList *cut_buffer)
{
	HTMLObject *obj;
	GList *p;
	guint count;
	gboolean prev_was_clueflow;

	count = 0;
	prev_was_clueflow = FALSE;
	for (p = cut_buffer; p != NULL; p = p->next) {
		obj = HTML_OBJECT (p->data);

		if (html_object_is_text (obj))
			count += HTML_TEXT (obj)->text_len;
		else
			count ++;
	}

	return count;
}
