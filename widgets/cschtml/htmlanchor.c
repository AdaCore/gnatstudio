/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.
    
   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
   Copyright (C) 1999, 2000 Helix Code, Inc.

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

#include "htmlanchor.h"


HTMLAnchorClass html_anchor_class;
static HTMLObjectClass *parent_class = NULL;


/* HTMLObject methods.  */

static void
destroy (HTMLObject *object)
{
	HTMLAnchor *anchor;

	anchor = HTML_ANCHOR (object);

	g_string_free (anchor->name, TRUE);

	HTML_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_ANCHOR (dest)->name = g_string_new (HTML_ANCHOR (self)->name->str);
}

static void
set_max_ascent (HTMLObject *object,
		HTMLPainter *painter,
		gint a)
{
	/*  object->y -= a; */
}

static HTMLAnchor *
find_anchor (HTMLObject *o, const char *name, gint *x, gint *y)
{
	if (strcmp (name, HTML_ANCHOR(o)->name->str) == 0) {
		*x += o->x;
		*y += o->y;

		return HTML_ANCHOR (o);
	}
	return NULL;
}


void
html_anchor_type_init (void)
{
	html_anchor_class_init (&html_anchor_class, HTML_TYPE_ANCHOR, sizeof (HTMLAnchor));
}

void
html_anchor_class_init (HTMLAnchorClass *klass,
			HTMLType type,
			guint object_size)
{
	HTMLObjectClass *object_class;

	object_class = HTML_OBJECT_CLASS (klass);

	html_object_class_init (object_class, type, object_size);

	object_class->destroy = destroy;
	object_class->copy = copy;
	object_class->find_anchor = find_anchor;
	object_class->set_max_ascent = set_max_ascent;

	parent_class = &html_object_class;
}

void
html_anchor_init (HTMLAnchor *anchor,
		  HTMLAnchorClass *klass,
		  const gchar *name)
{
	html_object_init (HTML_OBJECT (anchor), HTML_OBJECT_CLASS (klass));

	anchor->name = g_string_new (name);
}

HTMLObject *
html_anchor_new (const gchar *name)
{
	HTMLAnchor *anchor;

	anchor = g_new (HTMLAnchor, 1);
	html_anchor_init (anchor, &html_anchor_class, name);

	return HTML_OBJECT (anchor);
}


const gchar *
html_anchor_get_name (HTMLAnchor *anchor)
{
	return anchor->name->str;
}
