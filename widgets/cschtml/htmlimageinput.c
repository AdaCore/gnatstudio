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
#include "htmlimageinput.h"
#include "htmlform.h"
#include <string.h>


HTMLImageInputClass html_imageinput_class;
static HTMLEmbeddedClass *parent_class;


static void
destroy (HTMLObject *o)
{
	html_object_destroy (HTML_OBJECT (HTML_IMAGEINPUT (o)->image));

	HTML_OBJECT_CLASS (parent_class)->destroy (o);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	HTMLObject *duplicate_image;

	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_IMAGEINPUT (dest)->m_x = HTML_IMAGEINPUT (self)->m_x;
	HTML_IMAGEINPUT (dest)->m_y = HTML_IMAGEINPUT (self)->m_y;

	duplicate_image = html_object_dup (HTML_OBJECT (HTML_IMAGEINPUT (self)->image));
	HTML_IMAGEINPUT (dest)->image = HTML_IMAGE (duplicate_image);
}

static void
draw (HTMLObject *o,
      HTMLPainter *p,
      gint x, gint y,
      gint width, gint height,
      gint tx, gint ty)
{
	HTML_OBJECT (HTML_IMAGEINPUT (o)->image)->x = o->x;
	HTML_OBJECT (HTML_IMAGEINPUT (o)->image)->y = o->y;

	html_object_draw (HTML_OBJECT (HTML_IMAGEINPUT (o)->image),
			  p,
			  x, y,
			  width, height,
			  tx, ty);
}


/* Even if it's an HTMLEmbeddable, HTMLImageInput does not use a
   widget, so we need to implement these methods ourselves instead of
   using the HTMLEmbeddable default implementations.  */

static gint
calc_min_width (HTMLObject *self,
		HTMLPainter *painter)
{
	HTMLImageInput *image_input;

	image_input = HTML_IMAGEINPUT (self);

	return html_object_calc_min_width (HTML_OBJECT (image_input->image),
					   painter);
}

static gboolean
calc_size (HTMLObject *self,
	   HTMLPainter *painter)
{
	HTMLImageInput *image_input;
	HTMLObject *image_object;
	gboolean retval;

	image_input = HTML_IMAGEINPUT (self);
	image_object = HTML_OBJECT (image_input->image);

	retval = html_object_calc_size (image_object, painter);

	self->width = image_object->width;
	self->ascent = image_object->ascent;
	self->descent = image_object->descent;

	return retval;
}


static gchar *
encode (HTMLEmbedded *e)
{
	GString *encoding = g_string_new ("");
	gchar *ptr;

	if(strlen (e->name)) {
		ptr = html_embedded_encode_string (e->name);
		encoding = g_string_assign (encoding, ptr);
		g_free (ptr);

		ptr = g_strdup_printf(".x=%d&", HTML_IMAGEINPUT(e)->m_x);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);

		ptr = html_embedded_encode_string (e->name);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);

		ptr = g_strdup_printf(".y=%d", HTML_IMAGEINPUT(e)->m_y);
		encoding = g_string_append (encoding, ptr);
		g_free (ptr);
	}

	ptr = encoding->str;
	g_string_free(encoding, FALSE);

	return ptr;
}


void
html_imageinput_type_init (void)
{
	html_imageinput_class_init (&html_imageinput_class, HTML_TYPE_IMAGEINPUT, sizeof (HTMLImageInput));
}

void
html_imageinput_class_init (HTMLImageInputClass *klass,
			    HTMLType type,
			    guint size)
{
	HTMLEmbeddedClass *element_class;
	HTMLObjectClass *object_class;

	element_class = HTML_EMBEDDED_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_embedded_class_init (element_class, type, size);

	/* HTMLEmbedded methods.  */
	element_class->encode = encode;

	/* HTMLObject methods.   */
	object_class->destroy = destroy;
	object_class->copy = copy;
	object_class->draw = draw;
	object_class->calc_min_width = calc_min_width;
	object_class->calc_size = calc_size;

	parent_class = &html_embedded_class;
}

void
html_imageinput_init (HTMLImageInput *img, 
		      HTMLImageInputClass *klass,
		      HTMLImageFactory *imf,
		      gchar *name, gchar *url)
{
	HTMLEmbedded *element;
	HTMLObject *object;

	element = HTML_EMBEDDED (img);
	object = HTML_OBJECT (img);

	html_embedded_init (element, HTML_EMBEDDED_CLASS (klass), NULL, name, NULL);

	object->width = object->ascent = 32;

	img->image = HTML_IMAGE (html_image_new (imf, 
						 url, NULL, NULL,
						 -1, -1, 0, 0,
						 NULL,
						 HTML_VALIGN_BOTTOM));

	object->ascent = 32;
	object->width = 0;
	object->descent = 0;
}

HTMLObject *
html_imageinput_new (HTMLImageFactory *imf,
		     gchar *name, 
		     gchar *url)
{
	HTMLImageInput *img;

	img = g_new0 (HTMLImageInput, 1);
	html_imageinput_init (img, &html_imageinput_class, imf, name, url);

	return HTML_OBJECT (img);
}
