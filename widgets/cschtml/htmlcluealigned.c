/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* 
   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   (C) 1997 Torben Weis (weis@kde.org)

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

#include "htmlcluealigned.h"


#define ALIGN_BORDER 0


static HTMLClueAlignedClass html_cluealigned_class;
static HTMLClueClass *parent_class = NULL;


/* HTMLObject methods.  */

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_CLUEALIGNED (dest)->next_aligned = NULL;
}

static gboolean
calc_size (HTMLObject *o,
	   HTMLPainter *painter)
{
	HTMLObject *obj;
	gboolean changed;
	gint old_width, old_ascent;

	changed = HTML_OBJECT_CLASS (&html_clue_class)->calc_size (o, painter);

	old_width = o->width;
	old_ascent = o->ascent;

	o->width = 0;
	o->ascent = ALIGN_BORDER;
	o->descent = 0;

	/* FIXME: Shouldn't it call `calc_size()' on the children first!?!  */

	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next) {
		if (obj->width > o->width)
			o->width = obj->width;

		o->ascent += obj->ascent + obj->descent;

		if (obj->x != ALIGN_BORDER) {
			obj->x = ALIGN_BORDER;
			changed = TRUE;
		}

		if (obj->y != o->ascent - obj->descent) {
			obj->y = o->ascent - obj->descent;
			changed = TRUE;
		}
	}
	
	o->ascent += ALIGN_BORDER;
	o->width += ALIGN_BORDER * 2;

	if (old_width != o->width || old_ascent != o->ascent)
		changed = TRUE;

	return changed;
}

static void
set_max_width (HTMLObject *o, HTMLPainter *painter, gint max_width)
{
	HTMLObject *obj;

	o->max_width = max_width;

	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next)
		html_object_set_max_width (obj, painter, max_width);
}


void
html_cluealigned_type_init (void)
{
	html_cluealigned_class_init (&html_cluealigned_class, HTML_TYPE_CLUEALIGNED, sizeof (HTMLClueAligned));
}

void
html_cluealigned_class_init (HTMLClueAlignedClass *klass,
			     HTMLType type,
			     guint size)
{
	HTMLObjectClass *object_class;
	HTMLClueClass *clue_class;

	clue_class = HTML_CLUE_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_clue_class_init (clue_class, type, size);

	/* HTMLObject functions FIXME destroy? */

	object_class->copy = copy;
	object_class->calc_size = calc_size;
	object_class->set_max_width = set_max_width;

	parent_class = &html_clue_class;
}

void
html_cluealigned_init (HTMLClueAligned *aligned,
		       HTMLClueAlignedClass *klass,
		       HTMLObject *parent,
		       gint x, gint y,
		       gint max_width, gint percent)
{
	HTMLClue *clue;
	HTMLObject *object;

	clue = HTML_CLUE (aligned);
	object = HTML_OBJECT (aligned);

	html_clue_init (clue, HTML_CLUE_CLASS (klass));

	object->x = x;
	object->y = y;
	object->max_width = max_width;
	object->percent = percent;

	if (percent > 0)
		object->flags &= ~HTML_OBJECT_FLAG_FIXEDWIDTH;

	clue->valign = HTML_VALIGN_BOTTOM;
	clue->halign = HTML_HALIGN_LEFT;

	aligned->next_aligned = NULL;

	object->parent = parent;
	object->flags |= HTML_OBJECT_FLAG_ALIGNED;
}

HTMLObject *
html_cluealigned_new (HTMLObject *parent,
		      gint x, gint y,
		      gint max_width, gint percent)
{
	HTMLClueAligned *aclue;

	aclue = g_new (HTMLClueAligned, 1);
	html_cluealigned_init (aclue, &html_cluealigned_class,
			       parent, x, y, max_width, percent);

	return HTML_OBJECT (aclue);
}

