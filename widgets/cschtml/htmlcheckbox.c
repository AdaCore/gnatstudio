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
#include "htmlcheckbox.h"
#include <string.h>

HTMLCheckBoxClass html_checkbox_class;
static HTMLEmbeddedClass *parent_class = NULL;


static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_CHECKBOX (dest)->default_checked = HTML_CHECKBOX (self)->default_checked;
}

static gchar *
encode (HTMLEmbedded *e)
{
	GString *encoding = g_string_new ("");
	gchar *ptr;

	if(strlen (e->name) && gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (e->widget))) {

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


static void
reset (HTMLEmbedded *e)
{
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(e->widget), HTML_CHECKBOX(e)->default_checked);
}


void
html_checkbox_type_init (void)
{
	html_checkbox_class_init (&html_checkbox_class, HTML_TYPE_CHECKBOX, sizeof (HTMLCheckBoxClass));
}

void
html_checkbox_class_init (HTMLCheckBoxClass *klass,
			  HTMLType type,
			  guint size)
{
	HTMLEmbeddedClass *element_class;
	HTMLObjectClass *object_class;


	element_class = HTML_EMBEDDED_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_embedded_class_init (element_class, type, size);

	/* HTMLObject methods.  */
	object_class->copy = copy;

	/* HTMLEmbedded methods.  */
	element_class->reset = reset;
	element_class->encode = encode;

	parent_class = &html_embedded_class;
}

void
html_checkbox_init (HTMLCheckBox *checkbox, 
		    HTMLCheckBoxClass *klass, 
		    GtkWidget *parent, 
		    gchar *name, 
		    gchar *value, 
		    gboolean checked) 
{
	HTMLEmbedded *element;
	HTMLObject *object;
	GtkWidget  *check;

	element = HTML_EMBEDDED (checkbox);
	object = HTML_OBJECT (checkbox);

	if (value == NULL)
		value = g_strdup ("on");

	html_embedded_init (element, HTML_EMBEDDED_CLASS (klass), parent, name, value);

	check = gtk_check_button_new();
	html_embedded_set_widget (element, check);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), checked);
	checkbox->default_checked = checked;

	/*	gtk_widget_show(element->widget);
		gtk_layout_put(GTK_LAYOUT(parent), element->widget, 0, 0);*/
}

HTMLObject *
html_checkbox_new (GtkWidget *parent, 
		   gchar *name, 
		   gchar *value, 
		   gboolean checked)
{
	HTMLCheckBox *checkbox;

	checkbox = g_new0 (HTMLCheckBox, 1);
	html_checkbox_init (checkbox, &html_checkbox_class, parent, name, value, checked);

	return HTML_OBJECT (checkbox);
}
