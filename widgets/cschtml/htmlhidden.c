/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

    Copyright (C) 2000 Jonas Borgström <jonas_b@bitsmart.com>.

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
#include "htmlhidden.h"
#include <string.h>

HTMLHiddenClass html_hidden_class;
static HTMLEmbeddedClass *parent_class = NULL;


static gchar *
encode (HTMLEmbedded *e)
{
	GString *encoding = g_string_new ("");
	gchar *ptr;

	if(strlen (e->name)) {
		ptr = html_embedded_encode_string (e->name);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);

		encoding = g_string_append_c (encoding, '=');

		ptr = html_embedded_encode_string (e->value);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);
	}

	ptr = encoding->str;
	g_string_free(encoding, FALSE);

	return ptr;
}

void html_hidden_type_init (void)
{
	html_hidden_class_init (&html_hidden_class, HTML_TYPE_HIDDEN, sizeof (HTMLHidden));
}

void html_hidden_class_init (HTMLHiddenClass *klass,
			     HTMLType type,
			     guint object_size)
{
	HTMLEmbeddedClass *element_class;
	HTMLObjectClass *object_class;

	element_class = HTML_EMBEDDED_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_embedded_class_init (element_class, type, object_size);

	element_class->encode = encode;

	parent_class = &html_embedded_class;
}

void html_hidden_init (HTMLHidden *hidden, 
		       HTMLHiddenClass *klass, 
		       gchar *name, 
		       gchar *value)
{
	HTMLEmbedded *element;
	HTMLObject *object;

	element = HTML_EMBEDDED (hidden);
	object = HTML_OBJECT (hidden);

	html_embedded_init (element, HTML_EMBEDDED_CLASS (klass), NULL, name, value);

	object->descent = 0;
	object->width = 0;
	object->ascent = 0;
}

HTMLObject *html_hidden_new (gchar *name, gchar *value)
{
	HTMLHidden *hidden;

	hidden = g_new0 (HTMLHidden, 1);
	html_hidden_init (hidden, &html_hidden_class, name, value);

	return HTML_OBJECT (hidden);
}
