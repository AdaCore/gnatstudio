/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

    Copyright (C) 1997 Martin Jones (mjones@kde.org)
    Copyright (C) 1997 Torben Weis (weis@kde.org)
    Copyright (C) 1999 Helix Code, Inc.

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
#include <string.h>
#include <stdio.h>
#include "htmlembedded.h"


HTMLEmbeddedClass html_embedded_class;
static HTMLObjectClass *parent_class = NULL;


static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	g_warning ("HTMLEmbedded::copy is broken.");

	HTML_EMBEDDED (dest)->name = g_strdup (HTML_EMBEDDED (self)->name);
	HTML_EMBEDDED (dest)->value = g_strdup (HTML_EMBEDDED (self)->value);
	HTML_EMBEDDED (dest)->form = HTML_EMBEDDED (self)->form;

	HTML_EMBEDDED (dest)->widget = NULL;
	HTML_EMBEDDED (dest)->parent = NULL;

	HTML_EMBEDDED (dest)->abs_x = HTML_EMBEDDED (self)->abs_x;
	HTML_EMBEDDED (dest)->abs_y = HTML_EMBEDDED (self)->abs_y;
}

static void
draw (HTMLObject *o,
      HTMLPainter *p,
      gint x, gint y,
      gint width, gint height,
      gint tx, gint ty)
{
	HTMLEmbedded *element = HTML_EMBEDDED(o);
	gint new_x, new_y;
	ArtIRect paint;

	html_object_calc_intersection (o, &paint, x, y, width, height);
	if (art_irect_empty (&paint))
		return;

	if (element->widget) {

		new_x = GTK_LAYOUT (element->parent)->hadjustment->value + o->x + tx;
		new_y = GTK_LAYOUT (element->parent)->vadjustment->value + o->y + ty - o->ascent;
		
		if(element->abs_x == -1 && element->abs_y == -1) {
			gtk_layout_put(GTK_LAYOUT(element->parent), element->widget,
					new_x, new_y);

			gtk_widget_show (element->widget);			
		}
		else if(new_x != element->abs_x || new_y != element->abs_y) {
			
			gtk_layout_move(GTK_LAYOUT(element->parent), element->widget,
					new_x, new_y);
		} else {
			gtk_widget_queue_draw (element->widget);
		}
		element->abs_x = new_x;
		element->abs_y = new_y;

	}
}

static void
destroy (HTMLObject *o)
{
	HTMLEmbedded *element;

	element = HTML_EMBEDDED (o);

	if(element->name)
		g_free(element->name);
	if(element->value)
		g_free(element->value);
	if(element->widget) {
		if (element->widget->parent)
			gtk_widget_destroy (element->widget);
		else
			gtk_widget_unref (element->widget);
		}

	HTML_OBJECT_CLASS (parent_class)->destroy (o);
}

static void
reset (HTMLEmbedded *e)
{
	/* Nothing to do?  */
}

static gint
calc_min_width (HTMLObject *self,
		HTMLPainter *painter)
{
	HTMLEmbedded *emb = HTML_EMBEDDED (self);
	GtkWidget *widget;
	gint pixel_size;
	gint min_width;

	widget = HTML_EMBEDDED (self)->widget;
	if (widget == NULL)
		return 0;

	pixel_size = html_painter_get_pixel_size (painter);

	if (HTML_EMBEDDED (self)->allocated) {
		/* set width and height to minimize updates */
		emb->width  = widget->allocation.width;
		emb->height = widget->allocation.height;
		min_width = widget->allocation.width * pixel_size;
	} else {
		GtkRequisition req;

		gtk_widget_size_request (widget, &req);

		/* set width and height to minimize updates */
		emb->width   = req.width;
		emb->height  = req.height;
		min_width = req.width * pixel_size;
	}

	return min_width;
}

static gboolean
calc_size (HTMLObject *self,
	   HTMLPainter *painter)
{
	GtkWidget *widget;
	HTMLEmbedded *emb = HTML_EMBEDDED (self);
	gint pixel_size;
	gint old_width, old_ascent;

	widget = emb->widget;
	if (widget == NULL)
		return FALSE;

	pixel_size = html_painter_get_pixel_size (painter);

	old_width = self->width;
	old_ascent = self->ascent;

	if (HTML_EMBEDDED (self)->allocated) {
		/* set width and height to minimize updates */
		emb->width   = widget->allocation.width;
		emb->height  = widget->allocation.height;
		self->width  = widget->allocation.width * pixel_size;
		self->ascent = widget->allocation.height * pixel_size;
	} else {
		GtkRequisition req;

		gtk_widget_size_request (widget, &req);
		/* set width and height to minimize updates */
		emb->width   = req.width;
		emb->height  = req.height;
		self->width  = req.width * pixel_size;
		self->ascent = req.height * pixel_size;
	}

	/* This never changes.  */
	self->descent = 0;

	if (old_width != self->width || old_ascent != self->ascent)
		return TRUE;

	return FALSE;
}


void
html_embedded_reset (HTMLEmbedded *e)
{
	HTML_EMBEDDED_CLASS (HTML_OBJECT (e)->klass)->reset (e);
}

static gchar *
encode (HTMLEmbedded *e)
{
	return g_strdup("");
}

gchar *
html_embedded_encode (HTMLEmbedded *e)
{
	return HTML_EMBEDDED_CLASS (HTML_OBJECT (e)->klass)->encode (e);
}

void
html_embedded_set_form (HTMLEmbedded *e, HTMLForm *form)
{
	e->form = form;
}

gchar *
html_embedded_encode_string (gchar *str)
{
        static gchar *safe = "$-._!*(),"; /* RFC 1738 */
        unsigned pos = 0;
        GString *encoded = g_string_new ("");
        gchar buffer[5], *ptr;
	guchar c;
	
        while ( pos < strlen(str) ) {

		c = (unsigned char) str[pos];
			
		if ( (( c >= 'A') && ( c <= 'Z')) ||
		     (( c >= 'a') && ( c <= 'z')) ||
		     (( c >= '0') && ( c <= '9')) ||
		     (strchr(safe, c))
		     )
			{
				encoded = g_string_append_c (encoded, c);
			}
		else if ( c == ' ' )
			{
				encoded = g_string_append_c (encoded, '+');
			}
		else if ( c == '\n' )
			{
				encoded = g_string_append (encoded, "%0D%0A");
			}
		else if ( c != '\r' )
			{
				sprintf( buffer, "%%%02X", (int)c );
				encoded = g_string_append (encoded, buffer);
				}
		pos++;
	}
	
	ptr = encoded->str;

	g_string_free (encoded, FALSE);

        return ptr;
}


void
html_embedded_type_init (void)
{
	html_embedded_class_init (&html_embedded_class, HTML_TYPE_EMBEDDED, sizeof (HTMLEmbedded));
}

void
html_embedded_class_init (HTMLEmbeddedClass *klass, 
			  HTMLType type,
			  guint size)
{
	HTMLObjectClass *object_class;

	g_return_if_fail (klass != NULL);

	object_class = HTML_OBJECT_CLASS (klass);
	html_object_class_init (object_class, type, size);

	/* HTMLEmbedded methods.   */
	klass->reset = reset;
	klass->encode = encode;

	/* HTMLObject methods.   */
	object_class->destroy = destroy;
	object_class->copy = copy;
	object_class->draw = draw;
	object_class->calc_size = calc_size;
	object_class->calc_min_width = calc_min_width;

	parent_class = &html_object_class;
}

void
html_embedded_init (HTMLEmbedded *element, 
		   HTMLEmbeddedClass *klass, 
		   GtkWidget *parent, 
		   gchar *name, 
		   gchar *value)
{
	HTMLObject *object;

	object = HTML_OBJECT (element);
	html_object_init (object, HTML_OBJECT_CLASS (klass));

	element->form = NULL;
	if (name)
		element->name = g_strdup(name);
	else
		element->name = g_strdup("");
	if (value)
		element->value = g_strdup(value);
	else
		element->value = g_strdup("");
	element->widget = NULL;
	element->parent = parent;
	element->width  = 0;
	element->height = 0;
	element->allocated = FALSE;

	element->abs_x = element->abs_y = -1;
}

void
html_embedded_size_recalc(HTMLEmbedded *em)
{
	HTMLObject *o;
	GtkRequisition req;
	CscHTMLEmbedded *eb;

	o = HTML_OBJECT (em);

	if (em->widget == NULL)
		return;
	eb = (CscHTMLEmbedded *)em->widget;

	gtk_widget_size_request(em->widget, &req);

	o->width = req.width;
	if (GTK_IS_CSCHTML_EMBEDDED(eb)) {
		o->descent = eb->descent;
	} else {
		o->descent = 0;
	}

	o->ascent = req.height - o->descent;
}

static gboolean
html_embedded_grab_cursor(GtkWidget *eb, GdkEvent *event)
{
	/* Keep the focus! Fight the power */
	return TRUE;
}

HTMLEmbedded *
html_embedded_new_widget(GtkWidget *parent, CscHTMLEmbedded *eb)
{
	HTMLEmbedded *em;

	em = g_new0(HTMLEmbedded, 1);
	html_embedded_init (em, HTML_EMBEDDED_CLASS (&html_embedded_class), parent, eb->name, "");

	em->widget = (GtkWidget *)eb;
	html_embedded_size_recalc(em);
	gtk_signal_connect(GTK_OBJECT(eb), "button_press_event",
			   GTK_SIGNAL_FUNC(html_embedded_grab_cursor), NULL);

	return em;
}

static void
allocate (GtkWidget *w, GtkAllocation  *allocation, HTMLEmbedded *e)
{
	if (e->width != allocation->width || e->height != allocation->height) {
		if (e->width != allocation->width) {
			html_object_change_set (HTML_OBJECT (e), HTML_CHANGE_MIN_WIDTH);
			e->width = allocation->width;
		}
		e->height = allocation->height;

		g_assert (GTK_IS_CSCHTML (w->parent));
		html_engine_schedule_update (CSC_HTML (w->parent)->engine);
	}

	e->allocated = TRUE;
}

static void
request (GtkWidget *w, GtkRequisition *req, HTMLEmbedded *e)
{
	e->allocated = FALSE;
}

void
html_embedded_set_widget (HTMLEmbedded *e, GtkWidget *w)
{
	e->widget = w;

	gtk_signal_connect (GTK_OBJECT (w), "size_allocate",
			    GTK_SIGNAL_FUNC (allocate), e);
	gtk_signal_connect (GTK_OBJECT (w), "size_request",
			    GTK_SIGNAL_FUNC (request), e);

}
