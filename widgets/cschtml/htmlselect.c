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

#include "htmlselect.h"
#include <string.h>


HTMLSelectClass html_select_class;
static HTMLEmbeddedClass *parent_class = NULL;


static void
free_strings (gpointer o, gpointer data)
{
	g_free (o);
}

static void
destroy (HTMLObject *o)
{
	HTMLSelect *select;

	select = HTML_SELECT (o);

	if (select->default_selection)
		g_list_free (select->default_selection);

	if (select->values) {

		g_list_foreach (select->values, free_strings, NULL);
		g_list_free (select->values);
	}

	if (select->strings) {

		g_list_foreach (select->strings, free_strings, NULL);
		g_list_free (select->strings);
	}

	HTML_OBJECT_CLASS (parent_class)->destroy (o);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	/* FIXME TODO */
}


static void
reset (HTMLEmbedded *e)
{
	HTMLSelect *s = HTML_SELECT(e);
	GList *i = s->default_selection;
	gint row = 0;

	if (s->multi) {
		while (i) {
			if (i->data)
				gtk_clist_select_row (GTK_CLIST(s->clist), row, 0);
			else
				gtk_clist_unselect_row (GTK_CLIST(s->clist), row, 0);

			i = i->next;
			row++;
		}		
	} else if (s->size > 1) {
				gtk_clist_select_row (GTK_CLIST(s->clist), s->default_selected, 0);
	} else {
		gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(e->widget)->entry), (gchar *)g_list_nth(s->strings, s->default_selected)->data);
	}
}

static gchar *
encode (HTMLEmbedded *e)
{
	HTMLSelect *s = HTML_SELECT(e);
	GList *i;
	GString *encoding = g_string_new ("");
	gchar *txt, *ptr;

	if(strlen (e->name)) {
		if (s->size > 1) {
			gint i, rows = g_list_length (s->values);
			GList *work;
			
			for (i = 0; i < rows; i++) {
				work = g_list_nth (GTK_CLIST (s->clist)->row_list, i);
				
				if (GTK_CLIST_ROW (work)->state == GTK_STATE_SELECTED) {
					
					if (encoding->len) {
						encoding = g_string_append_c (encoding, '&');
					}
					ptr = html_embedded_encode_string (e->name);
					encoding = g_string_append (encoding, ptr);
					g_free (ptr);
					
					encoding = g_string_append_c (encoding, '=');
					
					ptr = html_embedded_encode_string ((gchar *)g_list_nth (s->values, i)->data);
					encoding = g_string_append (encoding, ptr);
					g_free (ptr);
				}
			}
		} else {
			gint item;

			ptr = html_embedded_encode_string (e->name);
			encoding = g_string_assign (encoding, ptr);
			g_free (ptr);
			encoding = g_string_append_c (encoding, '=');

			txt = gtk_entry_get_text (GTK_ENTRY(GTK_COMBO(e->widget)->entry));
			i = s->strings;
			item = 0;

			while (i) {

				if (strcmp(txt, (gchar *)i->data) == 0) {

					ptr = html_embedded_encode_string ((gchar *)g_list_nth (s->values, item)->data);
					encoding = g_string_append (encoding, ptr);
					g_free (ptr);
					
					break;
				}
				i = i->next;
				item++;
			}

		}
	}
	ptr = encoding->str;
	g_string_free(encoding, FALSE);
	
	return ptr;
}

void
html_select_type_init (void)
{
	html_select_class_init (&html_select_class, HTML_TYPE_SELECT, sizeof (HTMLSelect));
}

void
html_select_class_init (HTMLSelectClass *klass,
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
html_select_init (HTMLSelect *select,
		      HTMLSelectClass *klass,
		      GtkWidget *parent,
		      gchar *name,
		      gint size,
		      gboolean multi)
{

	HTMLEmbedded *element;
	HTMLObject *object;
	GtkWidget *widget;

	element = HTML_EMBEDDED (select);
	object = HTML_OBJECT (select);

	html_embedded_init (element, HTML_EMBEDDED_CLASS (klass),
			   parent, name, NULL);

	if (size > 1 || multi) {
		select->clist = gtk_clist_new (1);
		gtk_clist_set_column_auto_resize (GTK_CLIST (select->clist), 0, TRUE);

		if (multi)
			gtk_clist_set_selection_mode (GTK_CLIST (select->clist), GTK_SELECTION_MULTIPLE);

		widget = gtk_scrolled_window_new (NULL, NULL);
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (widget),
						GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
		gtk_container_add (GTK_CONTAINER (widget), select->clist);
		gtk_widget_show(select->clist);

		gtk_widget_set_usize (widget, 120, (GTK_CLIST(select->clist)->row_height + 1) * size + 5);

	} else {

		widget = gtk_combo_new ();
		gtk_entry_set_editable (GTK_ENTRY(GTK_COMBO(widget)->entry), FALSE);
		gtk_widget_set_usize ( GTK_WIDGET (widget), 120, -2);
	}
	html_embedded_set_widget (element, widget);

	select->size = size;
	select->multi = multi;
	select->default_selected = 0;
	select->values = NULL;
	select->strings = NULL;
	select->default_selection = NULL;
}

HTMLObject *
html_select_new (GtkWidget *parent,
		     gchar *name,
		     gint size,
		     gboolean multi)
{
	HTMLSelect *ti;

	ti = g_new0 (HTMLSelect, 1);
	html_select_init (ti, &html_select_class,
			      parent, name, size, multi);

	return HTML_OBJECT (ti);
}

void html_select_add_option (HTMLSelect *select, 
			     gchar *value, 
			     gboolean selected)
{
	gchar *data[] = { "", NULL};
	GtkWidget *w;

	if(select->size > 1 || select->multi) {

		w = select->clist;

		gtk_clist_append (GTK_CLIST(w), data);
		if(selected) {

			select->default_selected = GTK_CLIST(w)->rows - 1;
			gtk_clist_select_row (GTK_CLIST(w), select->default_selected, 0);

		} else if (GTK_CLIST(w)->rows == 1) {

			gtk_clist_unselect_row (GTK_CLIST(w), 0, 0);
		}
	} else {
		w = HTML_EMBEDDED (select)->widget;
		select->strings = g_list_append (select->strings, "");
		gtk_combo_set_popdown_strings (GTK_COMBO(w), select->strings);

		if(selected || g_list_length (select->strings) == 1) {

			select->default_selected = g_list_length (select->strings) - 1;
		}
	}
	if (value)
		select->values = g_list_append (select->values, g_strdup (value));
	else
		select->values = g_list_append (select->values, NULL);

	if(select->multi)
		select->default_selection = g_list_append (select->default_selection, GINT_TO_POINTER(selected));
}

static char *
longest_string(HTMLSelect *s)
{
	GList *i = s->strings;
	gint max = 0;
	gchar *str = NULL;

	while (i) {
		if (strlen(i->data) > max) {
			max = strlen (i->data);
			str = i->data;
		}
		i = i->next;
	}
	return str;
}

void html_select_set_text (HTMLSelect *select, 
			   gchar *text) 
{
	GtkWidget *w = GTK_WIDGET (HTML_EMBEDDED (select)->widget);
	gint item;

	if(select->size > 1 || select->multi) {
		item = GTK_CLIST(select->clist)->rows - 1;
		gtk_clist_set_text (GTK_CLIST(select->clist), item, 0, text);

		HTML_OBJECT(select)->width = gtk_clist_optimal_column_width (GTK_CLIST (select->clist), 0) + 12;
		/* Add width of scrollbar */
		if ((item + 1) > select->size && GTK_SCROLLED_WINDOW(w)->vscrollbar) {
			GtkRequisition req;

			gtk_widget_size_request(GTK_SCROLLED_WINDOW(w)->vscrollbar, &req);
			HTML_OBJECT(select)->width += req.width + 8;
		}

		gtk_widget_set_usize ( w, HTML_OBJECT(select)->width, -2);
	} else {
		item = g_list_length (select->strings) - 1;
		w = HTML_EMBEDDED (select)->widget;
		g_list_last (select->strings)->data = g_strdup (text);
		gtk_combo_set_popdown_strings (GTK_COMBO(w), select->strings);

		gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(w)->entry), g_list_nth(select->strings, select->default_selected)->data);

		HTML_OBJECT(select)->width = gdk_string_width(w->style->font, 
							      longest_string(select)) + 30;

		gtk_widget_set_usize ( GTK_WIDGET (w), 
				       HTML_OBJECT(select)->width, -2);
	}

	if(g_list_nth (select->values, item)->data == NULL)
		g_list_nth (select->values, item)->data = g_strdup(text);
}
