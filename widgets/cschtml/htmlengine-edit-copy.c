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

#include "htmlengine-cutbuffer.h"
#include "htmlengine-edit-copy.h"
#include "htmltext.h"


struct _CopyForallData {
	GList *buffer;
	guint size;
};
typedef struct _CopyForallData CopyForallData;


static void
copy_forall (HTMLObject *obj,
	     gpointer closure)
{
	HTMLObject *selection;
	CopyForallData *data;
	guint size;

	if (! obj->selected)
		return;

	data = (CopyForallData *) closure;

	selection = html_object_get_selection (obj, &size);
	if (selection == NULL)
		return;

	data->size += size;

	g_print ("Adding object %p [%s] to selection.\n",
		 selection, html_type_name (HTML_OBJECT_TYPE (selection)));
	if (html_object_is_text (selection))
		g_print ("\ttext `%s'\n", HTML_TEXT (selection)->text);

	data->buffer = g_list_prepend (data->buffer, selection);
}

static guint
copy (HTMLEngine *engine,
      GList **buffer)
{
	CopyForallData *forall_data;
	guint size;

	if (*buffer != NULL) {
		html_engine_cut_buffer_destroy (*buffer);
		*buffer = NULL;
	}

	forall_data = g_new (CopyForallData, 1);
	forall_data->buffer = NULL;
	forall_data->size = 0;

	html_object_forall (engine->clue, copy_forall, forall_data);

	*buffer = g_list_reverse (forall_data->buffer);
	size = forall_data->size;

	g_free (forall_data);

	return size;
}


guint
html_engine_copy (HTMLEngine *engine)
{
	g_return_val_if_fail (engine != NULL, 0);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), 0);
	g_return_val_if_fail (engine->active_selection, 0);
	g_return_val_if_fail (engine->clue != NULL, 0);

	return copy (engine, &engine->cut_buffer);
}

guint
html_engine_copy_to_buffer (HTMLEngine *engine,
			    GList **buffer)
{
	g_return_val_if_fail (engine != NULL, 0);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), 0);
	g_return_val_if_fail (engine->active_selection, 0);
	g_return_val_if_fail (engine->clue != NULL, 0);
	g_return_val_if_fail (buffer != NULL, 0);

	return copy (engine, buffer);
}
