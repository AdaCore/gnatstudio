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

#define GTK_ENABLE_BROKEN
#include "htmltextarea.h"
#include <string.h>


HTMLTextAreaClass html_textarea_class;
static HTMLEmbeddedClass *parent_class = NULL;


static void
destroy (HTMLObject *o)
{
	HTMLTextArea *ta;

	ta = HTML_TEXTAREA (o);

	if (ta->default_text)
		g_free (ta->default_text);

	HTML_OBJECT_CLASS (parent_class)->destroy (o);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	/* FIXME TODO this is not going to work.  */

	HTML_TEXTAREA (dest)->text = NULL;
	HTML_TEXTAREA (dest)->default_text = g_strdup (HTML_TEXTAREA (self)->default_text);

	g_warning ("HTMLTextArea::copy is not complte.");
}


static void
reset (HTMLEmbedded *e)
{
	html_textarea_set_text ( HTML_TEXTAREA (e), HTML_TEXTAREA (e)->default_text);
}

static gchar *
encode (HTMLEmbedded *e)
{
	GString *encoding = g_string_new ("");
	gchar *ptr, *ptr2;

	if(strlen (e->name)) {
		ptr = html_embedded_encode_string (e->name);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);

		encoding = g_string_append_c (encoding, '=');

		ptr2 = gtk_editable_get_chars (GTK_EDITABLE (HTML_TEXTAREA(e)->text), 0, -1);

		ptr = html_embedded_encode_string ( ptr2 );
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);
		g_free (ptr2);
	}

	ptr = encoding->str;
	g_string_free(encoding, FALSE);

	return ptr;
}

static int
on_button_press_event (GtkWidget *widget, GdkEventButton *event)
{
	return TRUE;
}

void
html_textarea_type_init (void)
{
	html_textarea_class_init (&html_textarea_class, HTML_TYPE_TEXTAREA, sizeof (HTMLTextArea));
}

void
html_textarea_class_init (HTMLTextAreaClass *klass,
			  HTMLType type,
			  guint object_size)
{
	HTMLEmbeddedClass *element_class;
	HTMLObjectClass *object_class;

	element_class = HTML_EMBEDDED_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_embedded_class_init (element_class, type, object_size);

	/* HTMLEmbedded methods.   */
	element_class->reset = reset;
	element_class->encode = encode;

	/* HTMLObject methods.   */
	object_class->destroy = destroy;
	object_class->copy = copy;

	parent_class = &html_embedded_class;
}

void
html_textarea_init (HTMLTextArea *ta,
		      HTMLTextAreaClass *klass,
		      GtkWidget *parent,
		      gchar *name,
		      gint row,
		      gint col)
{
	HTMLEmbedded *element;
	HTMLObject *object;
	GtkWidget *widget;

	element = HTML_EMBEDDED (ta);
	object = HTML_OBJECT (ta);

	html_embedded_init (element, HTML_EMBEDDED_CLASS (klass),
			   parent, name, NULL);

	ta->text = gtk_text_new (NULL, NULL);
	gtk_widget_show(ta->text);
	gtk_text_set_editable (GTK_TEXT (ta->text), TRUE);

	gtk_widget_set_events (ta->text, GDK_BUTTON_PRESS_MASK);

	gtk_signal_connect_after (GTK_OBJECT (ta->text), "button_press_event",
			    GTK_SIGNAL_FUNC (on_button_press_event), NULL);

	widget = gtk_scrolled_window_new (NULL, NULL);
	html_embedded_set_widget (element, widget);

	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (widget),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_container_add (GTK_CONTAINER (widget), ta->text);

#define FONT_HEIGHT(f)              ((f)->ascent + (f)->descent)

	gtk_widget_set_usize ( GTK_WIDGET (widget), 
			       gdk_char_width(gtk_style_get_font (widget->style), '0') * col + 8,
			       FONT_HEIGHT(gtk_style_get_font (ta->text->style)) * row + 4);

#undef FONT_HEIGHT
	ta->default_text = NULL;
}

HTMLObject *
html_textarea_new (GtkWidget *parent,
		     gchar *name,
		     gint row,
		     gint col)
{
	HTMLTextArea *ta;

	ta = g_new0 (HTMLTextArea, 1);
	html_textarea_init (ta, &html_textarea_class,
			      parent, name, row, col);

	return HTML_OBJECT (ta);
}

void html_textarea_set_text (HTMLTextArea *ta, 
			   gchar *text) 
{
	if (!ta->default_text)
		ta->default_text = g_strdup (text);

	gtk_editable_delete_text (GTK_EDITABLE (ta->text), 0, -1);
	gtk_text_insert (GTK_TEXT (ta->text), NULL, NULL, NULL, text, strlen (text));
}
