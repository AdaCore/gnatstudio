/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML widget.

    Copyright (C) 2000 Jonas Borgström <jonas_b@bitsmart.com>

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

#include "htmlform.h"
#include <string.h>


HTMLForm *
html_form_new (HTMLEngine *engine, gchar *_action, gchar *_method) {
	HTMLForm *new;
	
	new = g_new (HTMLForm, 1);
	new->action = g_strdup(_action);
	new->method = g_strdup(_method);

	new->elements = NULL;
	new->hidden = NULL;
	new->engine = engine;

	new->radio_group = NULL;

	return new;
}

void
html_form_add_element (HTMLForm *form, HTMLEmbedded *element)
{
	form->elements = g_list_append (form->elements, element);

	html_embedded_set_form (element, form);
}

void
html_form_add_hidden (HTMLForm *form, HTMLHidden *hidden)
{
	html_form_add_element (form, HTML_EMBEDDED (hidden));

	form->hidden = g_list_append (form->hidden, hidden);
}

static void
destroy_hidden (gpointer o, gpointer data)
{
	html_object_destroy (HTML_OBJECT (o));
}

static void
reset_element (gpointer o, gpointer data)
{
	html_embedded_reset (HTML_EMBEDDED (o));
}

void
html_form_destroy (HTMLForm *form)
{
	g_list_foreach (form->hidden, destroy_hidden, NULL);
	g_list_free (form->elements);
	g_list_free (form->hidden);


	if (form->action)
		g_free (form->action);

	if (form->method)
		g_free (form->method);

	g_free (form);
}

void
html_form_submit (HTMLForm *form)
{
	GString *encoding = g_string_new ("");
	gint first = TRUE;
	GList *i = form->elements;
	gchar *ptr;

	while (i) {
		ptr = html_embedded_encode (HTML_EMBEDDED (i->data));

		if (strlen (ptr)) {
			if(!first)
				encoding = g_string_append_c (encoding, '&');
			else
				first = FALSE;
			
			encoding = g_string_append (encoding, ptr);
			g_free (ptr);
		}
		i = g_list_next (i);		
	}

	html_engine_form_submitted (form->engine, form->method, form->action, encoding->str);

	g_string_free (encoding, TRUE);
}

void
html_form_reset (HTMLForm *form)
{
	g_list_foreach (form->elements, reset_element, NULL);
}

