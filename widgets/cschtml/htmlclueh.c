/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
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

#include "htmlclueh.h"


static HTMLClueClass *parent_class = NULL;

HTMLClueHClass html_clueh_class;


static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_CLUEH (dest)->indent = HTML_CLUEH (self)->indent;
}

static void
set_max_width (HTMLObject *o, HTMLPainter *painter, gint w)
{
	HTMLObject *obj;

	o->max_width = w;

	/* First calculate width minus fixed width objects */
	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next) {
		if (obj->percent <= 0) /* i.e. fixed width objects */
			w -= obj->width;
	}

	/* Now call set_max_width for variable objects */
	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next) {
		if (obj->percent > 0)
			html_object_set_max_width (obj, painter, 
						   w - HTML_CLUEH (o)->indent);
	}
}

static gboolean
calc_size (HTMLObject *clue,
	   HTMLPainter *painter)
{
	HTMLObject *obj;
	gint lmargin = 0;
	gint a = 0, d = 0;
	gboolean changed;

	changed = FALSE;

	/* Make sure the children are properly sized */
	html_object_set_max_width (clue, painter, clue->max_width);

	changed = HTML_OBJECT_CLASS (&html_clue_class)->calc_size (clue, painter);

	if (clue->parent != NULL)
		lmargin = html_object_get_left_margin (clue->parent, clue->y);

	clue->width = lmargin + HTML_CLUEH (clue)->indent;
	clue->descent = 0;
	clue->ascent = 0;

	for (obj = HTML_CLUE (clue)->head; obj != 0; obj = obj->next) {
		html_object_fit_line (obj,
				      painter,
				      (obj == HTML_CLUE (clue)->head),
				      TRUE, -1);
		obj->x = clue->width;
		clue->width += obj->width;
		if (obj->ascent > a)
			a = obj->ascent;
		if (obj->descent > d)
			d = obj->descent;
	}


	switch (HTML_CLUE (clue)->valign) {
	case HTML_VALIGN_TOP:
		clue->ascent = a + d;
		for (obj = HTML_CLUE (clue)->head; obj != 0; obj = obj->next) {
			if (obj->y != obj->ascent) {
				obj->y = obj->ascent;
				changed = TRUE;
			}
		}
		break;

	case HTML_VALIGN_CENTER:
		clue->ascent = a + d;
		for (obj = HTML_CLUE (clue)->head; obj != 0; obj = obj->next) {
			if (obj->y != clue->ascent / 2) {
				obj->y = clue->ascent / 2;
				changed = TRUE;
			}
		}
		break;

	default:
		clue->ascent = a + d;
		for (obj = HTML_CLUE (clue)->head; obj != 0; obj = obj->next) {
			if (obj->y != clue->ascent - d) {
				obj->y = clue->ascent - d;
				changed = TRUE;
			}
		}
	}

	return changed;
}

static gint
calc_min_width (HTMLObject *o,
		HTMLPainter *painter)
{
	HTMLObject *obj;
	gint minWidth = 0;

	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next)
		minWidth += html_object_calc_min_width (obj, painter);

	return minWidth + HTML_CLUEH (o)->indent;
}

static gint
calc_preferred_width (HTMLObject *o,
		      HTMLPainter *painter)
{
	HTMLObject *obj;
	gint prefWidth = 0;

	for (obj = HTML_CLUE (o)->head; obj != 0; obj = obj->next) 
		prefWidth += html_object_calc_preferred_width (obj, painter);

	return prefWidth + HTML_CLUEH (o)->indent;
}


void
html_clueh_type_init (void)
{
	html_clueh_class_init (&html_clueh_class, HTML_TYPE_CLUEH, sizeof (HTMLClueH));
}

void
html_clueh_class_init (HTMLClueHClass *klass,
		       HTMLType type,
		       guint size)
{
	HTMLClueClass *clue_class;
	HTMLObjectClass *object_class;

	clue_class = HTML_CLUE_CLASS (klass);
	object_class = HTML_OBJECT_CLASS (klass);

	html_clue_class_init (clue_class, type, size);

	object_class->copy = copy;
	object_class->set_max_width = set_max_width;
	object_class->calc_size = calc_size;
	object_class->calc_min_width = calc_min_width;
	object_class->calc_preferred_width = calc_preferred_width;

	parent_class = &html_clue_class;
}

void
html_clueh_init (HTMLClueH *clueh, HTMLClueHClass *klass,
		 gint x, gint y, gint max_width)
{
	HTMLObject *object;
	HTMLClue *clue;

	clue = HTML_CLUE (clueh);
	object = HTML_OBJECT (clueh);

	html_clue_init (clue, HTML_CLUE_CLASS (klass));

	clue->valign = HTML_VALIGN_BOTTOM;
	clue->halign = HTML_HALIGN_LEFT;
	clue->head = clue->tail = clue->curr = 0;

	object->x = x;
	object->y = y;
	object->max_width = max_width;
	object->percent = 100;
	object->width = object->max_width;
	object->flags &= ~ HTML_OBJECT_FLAG_FIXEDWIDTH;
}

HTMLObject *
html_clueh_new (gint x, gint y, gint max_width)
{
	HTMLClueH *clueh;

	clueh = g_new0 (HTMLClueH, 1);
	html_clueh_init (clueh, &html_clueh_class, x, y, max_width);

	return HTML_OBJECT (clueh);
}
