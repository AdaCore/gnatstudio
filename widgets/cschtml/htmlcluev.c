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
#include "htmlcluev.h"
#include "htmlobject.h"


HTMLClueVClass html_cluev_class;

static HTMLClueClass *parent_class = NULL;


/* FIXME this must be rewritten as the multiple type casts make my head spin.
   The types in `HTMLClueAligned' are chosen wrong.  */
static void
remove_aligned_by_parent ( HTMLClueV *cluev,
			   HTMLObject *p )
{
    HTMLClueAligned *tmp;
	HTMLObject *obj;

    tmp = 0;
    obj = cluev->align_left_list;

    while ( obj ) {
		if ( obj->parent == p ) {
			if ( tmp ) {
				tmp->next_aligned = HTML_CLUEALIGNED (obj)->next_aligned;
				tmp = HTML_CLUEALIGNED (obj);
			} else {
				cluev->align_left_list
					= HTML_OBJECT (HTML_CLUEALIGNED (obj)->next_aligned);
				tmp = 0;
			}
		} else {
			tmp = HTML_CLUEALIGNED (obj);
		}

		obj = HTML_OBJECT (HTML_CLUEALIGNED (obj)->next_aligned);
    }

    tmp = 0;
    obj = cluev->align_right_list;

    while ( obj ) {
		if ( obj->parent == p ) {
			if ( tmp ) {
				tmp->next_aligned = HTML_CLUEALIGNED (obj)->next_aligned;
				tmp = HTML_CLUEALIGNED (obj);
			} else {
				cluev->align_right_list
					= HTML_OBJECT (HTML_CLUEALIGNED (obj)->next_aligned);
				tmp = 0;
			}
		} else {
			tmp = HTML_CLUEALIGNED (obj);
		}

		obj = HTML_OBJECT (HTML_CLUEALIGNED (obj)->next_aligned);
    }
}

static HTMLObject *
cluev_next_aligned (HTMLObject *aclue)
{
	return HTML_OBJECT (HTML_CLUEALIGNED (aclue)->next_aligned);
}

static gboolean
do_layout (HTMLObject *o,
	   HTMLPainter *painter,
	   gboolean calc_size)
{
	HTMLClueV *cluev;
	HTMLClue *clue;
	HTMLObject *obj;
	HTMLObject *aclue;
	gint lmargin;
	gboolean changed;
	gint old_width, old_ascent, old_descent;
	gint new_x;

	cluev = HTML_CLUEV (o);
	clue = HTML_CLUE (o);

	old_width = o->width;
	old_ascent = o->ascent;
	old_descent = o->descent;

	changed = FALSE;

	if (o->parent != NULL) {
		lmargin = html_object_get_left_margin (o->parent, o->y);
		lmargin += cluev->padding;
	} else {
		lmargin = cluev->padding;
	}

	/* If we have already called calc_size for the children, then just
	   continue from the last object done in previous call. */
	
	if (clue->curr != NULL) {
		o->ascent = cluev->padding;
		
		/* Get the current ascent not including curr */
		obj = clue->head;
		while (obj != clue->curr) {
			o->ascent += obj->ascent + obj->descent;
			obj = obj->next;
		}

		/* Remove any aligned objects previously added by the current
		   object.  */
		remove_aligned_by_parent (cluev, clue->curr);
	} else {
		o->width = 0;
		o->ascent = cluev->padding;
		o->descent = 0;
		clue->curr = clue->head;
	}

	while (clue->curr != NULL) {
		/* Set an initial ypos so that the alignment stuff knows where
		   the top of this object is */
		clue->curr->y = o->ascent;

		if (calc_size)
			changed |= html_object_calc_size (clue->curr, painter);

		if (o->width < clue->curr->width + 2 * cluev->padding)
			o->width = clue->curr->width + 2 * cluev->padding;

		o->ascent += clue->curr->ascent + clue->curr->descent;

		clue->curr->x = lmargin;
		clue->curr->y = o->ascent - clue->curr->descent;

		clue->curr = clue->curr->next;
	}

	o->ascent += cluev->padding;

	/* Remember the last object so that we can start from here next time
	   we are called. */
	clue->curr = clue->tail;

	if (o->max_width != 0 && o->width < o->max_width)
		o->width = o->max_width;
	
	if (clue->halign == HTML_HALIGN_CENTER) {
		for (obj = clue->head; obj != 0; obj = obj->next) {
			new_x = lmargin + (o->width - obj->width) / 2;
			if (obj->x != new_x) {
				obj->x = new_x;
				changed = TRUE;
			}
		}
	} else if (clue->halign == HTML_HALIGN_RIGHT) {
		for (obj = clue->head; obj != 0; obj = obj->next) {
			new_x = lmargin + (o->width - obj->width);
			if (obj->x != new_x) {
				obj->x = new_x;
				changed = TRUE;
			}
		}
	}
	
	for (aclue = cluev->align_left_list; aclue != NULL; aclue = cluev_next_aligned (aclue)) {
		if (aclue->y + aclue->parent->y - aclue->parent->ascent > o->ascent)
			o->ascent = aclue->y + aclue->parent->y - aclue->parent->ascent;
	}

	for (aclue = cluev->align_right_list; aclue != NULL; aclue = cluev_next_aligned (aclue)) {
		if (aclue->y + aclue->parent->y - aclue->parent->ascent > o->ascent)
			o->ascent = aclue->y + aclue->parent->y - aclue->parent->ascent;
	}

	if (! changed
	    && (o->ascent != old_ascent || o->descent != old_descent || o->width != old_width))
		changed = TRUE;

	return changed;
}


/* HTMLObject methods.  */

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_CLUEV (dest)->padding = HTML_CLUEV (self)->padding;

	HTML_CLUEV (dest)->align_left_list = NULL;
	HTML_CLUEV (dest)->align_right_list = NULL;
}

static gboolean
calc_size (HTMLObject *o,
	   HTMLPainter *painter)
{
	return do_layout (o, painter, TRUE);
}

static void
set_max_width (HTMLObject *o, HTMLPainter *painter, gint max_width)
{
	HTMLObject *obj;

	o->max_width = max_width;
	for (obj = HTML_CLUE (o)->head; obj != NULL; obj = obj->next)
		html_object_set_max_width (obj, painter, o->max_width);
}

static void
reset (HTMLObject *clue)
{
	HTMLClueV *cluev;

	cluev = HTML_CLUEV (clue);

	HTML_OBJECT_CLASS (&html_clue_class)->reset (clue);

	cluev->align_left_list = NULL;
	cluev->align_right_list = NULL;
}

static void
draw (HTMLObject *o,
      HTMLPainter *p,
      gint x, gint y,
      gint width, gint height,
      gint tx, gint ty)
{
	HTMLObject *aclue;

	HTML_OBJECT_CLASS (&html_clue_class)->draw (o,
						    p,
						    x, y ,
						    width, height,
						    tx, ty);

	if (y + height < o->y - o->ascent || y > o->y + o->descent)
		return;

	tx += o->x;
	ty += o->y - o->ascent;

	for ( aclue = HTML_CLUEV (o)->align_left_list;
	      aclue != NULL;
	      aclue = cluev_next_aligned (aclue) ) {
		html_object_draw (aclue,
				  p, 
				  0, 0, 0xffff, 0xffff,
				  tx + aclue->parent->x,
				  ty + aclue->parent->y - aclue->parent->ascent);
	}

	for (aclue = HTML_CLUEV (o)->align_right_list;
	     aclue != NULL;
	     aclue = cluev_next_aligned (aclue)) {
		html_object_draw (aclue,
				  p,
				  0, 0, 0xffff, 0xffff,
				  tx + aclue->parent->x, 
				  ty + aclue->parent->y - aclue->parent->ascent);
	}
}

static HTMLObject *
check_point (HTMLObject *self,
	     HTMLPainter *painter,
	     gint x, gint y,
	     guint *offset_return,
	     gboolean for_cursor)
{
	HTMLObject *p;
	HTMLObject *obj;
	HTMLClueAligned *clue;

	if (x < self->x || x >= self->x + self->width
	    || y < self->y - self->ascent || y >= self->y + self->descent)
		return NULL;

	x = x - self->x;
	y = y - self->y + self->ascent;

	for (p = HTML_CLUE (self)->head; p != 0; p = p->next) {
		gint x1, y1;

		if (! for_cursor) {
			x1 = x;
			y1 = y;
		} else {
			if (x >= p->width) {
				x1 = p->width - 1;
			} else if (x < p->x) {
				x1 = p->x;
			} else {
				x1 = x;
			}

			if (p->next == NULL && y > p->y + p->descent) {
				x1 = p->width - 1;
				y1 = p->y + p->descent;
			} else if (p->prev == NULL && y < p->y - p->ascent) {
				y1 = p->y - p->ascent;
			} else {
				y1 = y;
			}
		}

		obj = html_object_check_point (p, painter, x1, y1, offset_return, for_cursor);
		if (obj != NULL)
			return obj;
	}

	for (clue = HTML_CLUEALIGNED (HTML_CLUEV (self)->align_left_list);
	     clue != NULL;
	     clue = clue->next_aligned) {
		HTMLObject *parent;

		parent = HTML_OBJECT (clue)->parent;
		obj = html_object_check_point (HTML_OBJECT (clue),
					       painter,
					       x, y,
					       offset_return,
					       for_cursor);
		if (obj != NULL) {
			if (offset_return != NULL)
				*offset_return = 0;
			return obj;
		}
	}

	return NULL;
}

static gboolean
relayout (HTMLObject *self,
	  HTMLEngine *engine,
	  HTMLObject *child)
{
	gint prev_width, prev_ascent, prev_descent;
	gboolean changed;

	if (html_engine_frozen (engine))
		return FALSE;

	if (child == NULL)
		child = HTML_CLUE (self)->head;
	html_object_calc_size (child, engine->painter);

	HTML_CLUE (self)->curr = NULL;

	prev_width = self->width;
	prev_ascent = self->ascent;
	prev_descent = self->descent;

	changed = do_layout (self, engine->painter, FALSE);
	if (changed)
		html_engine_queue_draw (engine, self);

	if (prev_width == self->width
	    && prev_ascent == self->ascent
	    && prev_descent == self->descent)
		return FALSE;

	if (self->parent == NULL) {
		/* FIXME resize the widget, e.g. scrollbars and such.  */
		html_engine_queue_draw (engine, self);

		/* FIXME extreme ugliness.  */
		self->x = 0;
		self->y = self->ascent;
	} else {
		/* Relayout our parent starting from us.  */
		if (! html_object_relayout (self->parent, engine, self))
			html_engine_queue_draw (engine, self);
	}

	/* If the object has shrunk, we have to clean the areas around
	   it so that we don't leave garbage on the screen.  FIXME:
	   this wastes some time if there is an object on the right of
	   or under this one.  */

	if (prev_ascent + prev_descent > self->ascent + self->descent)
		html_engine_queue_clear (engine,
					 self->x,
					 self->y + self->descent,
					 self->width,
					 (prev_ascent + prev_descent
					  - (self->ascent + self->descent)));

	if (prev_width > self->width)
		html_engine_queue_clear (engine,
					 self->x + self->width,
					 self->y - self->ascent,
					 prev_width - self->width,
					 self->ascent + self->descent);

	return TRUE;
}

static gint
check_page_split (HTMLObject *self,
		  gint y)
{
	HTMLClue *clue;
	HTMLObject *p;

	clue = HTML_CLUE (self);

	y -= (self->y - self->ascent);

	for (p = clue->head; p != NULL; p = p->next) {
		gint y1, y2;

		y1 = p->y - p->ascent;
		y2 = p->y + p->descent;

		if (y1 > y)
			return y;

		if (y >= y1 && y < y2)
			return html_object_check_page_split (p, y - y1) + y1;
	}

	return y;
}

static gint
get_left_margin (HTMLObject *self, gint y)
{
	HTMLObject *aclue;
	HTMLClueV *cluev;
	gint margin;

	cluev = HTML_CLUEV (self);
	margin = 0;
	
	for (aclue = cluev->align_left_list;
	     aclue != NULL;
	     aclue = cluev_next_aligned (aclue)) {
		if ((aclue->y - aclue->ascent + aclue->parent->y - aclue->parent->ascent
		     <= y)
		    && (aclue->y + aclue->parent->y - aclue->parent->ascent
			> y))
			margin = aclue->x + aclue->width;
	}

	return margin;
}

static gint
get_right_margin (HTMLObject *self, gint y)
{
	HTMLClueV *cluev;
	/* FIXME: Should be HTMLAligned */
	HTMLObject *aclue;
	gint margin;
	
	cluev = HTML_CLUEV (self);
	margin = self->max_width;

	for (aclue = cluev->align_right_list;
	     aclue != NULL;
	     aclue = cluev_next_aligned (aclue)) {
		if ((aclue->y - aclue->ascent + aclue->parent->y - aclue->parent->ascent) <= y
		    && aclue->y + aclue->parent->y - aclue->parent->ascent > y)
			margin = aclue->x;
	}
	
	return margin;
}


/* HTMLClue methods.  */

static void
find_free_area (HTMLClue *clue, gint y, gint width, gint height,
		gint indent, gint *y_pos, gint *_lmargin, gint *_rmargin)
{
	HTMLClueV *cluev = HTML_CLUEV (clue);
	gint try_y = y;
	gint lmargin;
	gint rmargin;
	gint lm, rm;
	HTMLObject *aclue;
	gint next_y, top_y, base_y=0;

	while (1) {
		lmargin = indent;
		rmargin = MAX (HTML_OBJECT (clue)->max_width, HTML_OBJECT (clue)->width);
		next_y = 0;
		
		for (aclue = cluev->align_left_list; aclue != 0; aclue = cluev_next_aligned (aclue)) {
			base_y = (aclue->y + aclue->parent->y
				  - aclue->parent->ascent);
			top_y = base_y - aclue->ascent;

			if ((top_y < try_y + height) && (base_y > try_y)) {
				lm = aclue->x + aclue->width;
				if (lm > lmargin)
					lmargin = lm;
				
				if ((next_y == 0) || (base_y < next_y)) {
					next_y = base_y;

				}
			}
		}

		for (aclue = cluev->align_right_list; aclue != 0; aclue = cluev_next_aligned (aclue)) {
			base_y = (aclue->y + aclue->parent->y
				  - aclue->parent->ascent);
			top_y = base_y - aclue->ascent;

			if ((top_y < try_y + height) && (base_y > try_y)) {
				rm = aclue->x;
				if (rm < rmargin)
					rmargin = rm;
				
				if ((next_y == 0) || (base_y < next_y)) {
					next_y = base_y;
				}
			}
		}
		
		if ((lmargin == indent) && (rmargin == HTML_OBJECT (clue)->max_width))
			break;
		
		if ((rmargin - lmargin) >= width)
			break;

		try_y = next_y;
	}
	
	*y_pos = try_y;
	*_rmargin = rmargin;
	*_lmargin = lmargin;
}

static gboolean
appended (HTMLClue *clue, HTMLClue *aclue)
{
	/* Returns whether aclue is already in the alignList */
	HTMLClueAligned *aligned;
	
	if (aclue->halign == HTML_HALIGN_LEFT) {
		aligned = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_left_list);
	}
	else {
		aligned = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_right_list);
	}

	while (aligned) {
		if (aligned == HTML_CLUEALIGNED (aclue))
			return TRUE;
		aligned = HTML_CLUEALIGNED (aligned->next_aligned);
	}
	return FALSE;
}

static void
append_left_aligned (HTMLClue *clue, HTMLClue *aclue)
{
	gint y_pos;
	gint start_y = 0;
	gint lmargin;
	gint rmargin;
	HTMLClueAligned *aligned;
	
	aligned = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_left_list);
	if (aligned) {
		HTMLObject *parent;

		while (aligned->next_aligned) {
			aligned = aligned->next_aligned;
		}

		parent = HTML_OBJECT (aligned)->parent;
		y_pos = (HTML_OBJECT (aligned)->y
			 + parent->y - parent->ascent);
		if (y_pos > start_y)
			start_y = y_pos;
	}

	y_pos = (HTML_OBJECT (aclue)->y
		 + HTML_OBJECT (aclue)->parent->y
		 - HTML_OBJECT (aclue)->parent->ascent);
	
	if (y_pos > start_y)
		start_y = y_pos;

	/* Start looking for space from the position of the last object in
	   the left-aligned list on, or from the current position of the
	   object */
	html_clue_find_free_area (clue,
				  start_y - HTML_OBJECT (aclue)->ascent,
				  HTML_OBJECT (aclue)->width, 
				  HTML_OBJECT (aclue)->ascent + HTML_OBJECT (aclue)->descent,
				  0, &y_pos, &lmargin, &rmargin);

	/* Set position */
	HTML_OBJECT (aclue)->x = lmargin;
	HTML_OBJECT (aclue)->y = y_pos - HTML_OBJECT (aclue)->parent->y 
		+ HTML_OBJECT (aclue)->parent->ascent +
		HTML_OBJECT (aclue)->ascent;

	/* Insert clue in align list */
	if (!HTML_CLUEV (clue)->align_left_list) {
		HTML_CLUEV (clue)->align_left_list = HTML_OBJECT (aclue);
		HTML_CLUEALIGNED (aclue)->next_aligned = NULL;
	}
	else {
		HTMLClueAligned *obj = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_left_list);
		while (obj->next_aligned) {
			if (obj == HTML_CLUEALIGNED (aclue))
				return;
			obj = obj->next_aligned;
		}
		if (obj == HTML_CLUEALIGNED (aclue))
			return;

		obj->next_aligned = HTML_CLUEALIGNED (aclue);
		HTML_CLUEALIGNED (aclue)->next_aligned = NULL;
	}
}

static void
append_right_aligned (HTMLClue *clue, HTMLClue *aclue)
{
	gint y_pos;
	gint start_y = 0;
	gint lmargin;
	gint rmargin;
	HTMLClueAligned *aligned;

	aligned = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_right_list);

	if (aligned) {
		HTMLObject *parent;

		while (aligned->next_aligned) {
			aligned = aligned->next_aligned;
		}

		parent = HTML_OBJECT (aligned)->parent;
		y_pos = (HTML_OBJECT (aligned)->y
			 + parent->y - parent->ascent);
		if (y_pos > start_y)
			start_y = y_pos;
	}

	y_pos = HTML_OBJECT (aclue)->y + HTML_OBJECT (aclue)->parent->y -
		HTML_OBJECT (aclue)->parent->ascent;

	if (y_pos > start_y)
		start_y = y_pos;
	
	/* Start looking for space from the position of the last object in
	   the left-aligned list on, or from the current position of the
	   object. */
	html_clue_find_free_area (clue, start_y - HTML_OBJECT (aclue)->ascent,
				  HTML_OBJECT (aclue)->width, 
				  HTML_OBJECT (aclue)->ascent + 
				  HTML_OBJECT (aclue)->descent, 0,
				  &y_pos, &lmargin, &rmargin);

	/* Set position */
	HTML_OBJECT (aclue)->x = rmargin - HTML_OBJECT (aclue)->width;
	HTML_OBJECT (aclue)->y = y_pos - HTML_OBJECT (aclue)->parent->y + 
		HTML_OBJECT (aclue)->ascent + HTML_OBJECT (aclue)->parent->ascent;
	
	/* Insert clue in align list */
	if (!HTML_CLUEV (clue)->align_right_list) {
		HTML_CLUEV (clue)->align_right_list = HTML_OBJECT (aclue);
		HTML_CLUEALIGNED (aclue)->next_aligned = NULL;
	}
	else {
		HTMLClueAligned *obj = HTML_CLUEALIGNED (HTML_CLUEV (clue)->align_right_list);
		while (obj->next_aligned) {
			if (obj == HTML_CLUEALIGNED (aclue))
				return;
			obj = obj->next_aligned;
		}
		if (obj == HTML_CLUEALIGNED (aclue))
			return;

		obj->next_aligned = HTML_CLUEALIGNED (aclue);
		HTML_CLUEALIGNED (aclue)->next_aligned = NULL;
	}
}

static gint
get_left_clear (HTMLClue *self,
		gint y)
{
	HTMLObject *p;
	gint top_y, base_y;

	/* XXX we assume the parent's size has already been calculated here.  */
    	
	for (p = HTML_CLUEV (self)->align_left_list;
	     p != NULL;
	     p = HTML_OBJECT (HTML_CLUEALIGNED (p)->next_aligned)) {
		base_y = p->y + p->parent->y - p->parent->ascent;
		top_y = base_y - p->ascent;

		if (top_y <= y && base_y > y)
			y = base_y;
	}

	return y;
}

static gint
get_right_clear (HTMLClue *self,
		 gint y)
{
	HTMLObject *p;
	gint top_y, base_y;

	/* XXX we assume the parent's size has already been calculated here.  */
    	
	for (p = HTML_CLUEV (self)->align_right_list;
	     p != NULL;
	     p = HTML_OBJECT (HTML_CLUEALIGNED (p)->next_aligned)) {
		base_y = p->y + p->parent->y - p->parent->ascent;
		top_y = base_y - p->ascent;

		if (top_y <= y && base_y > y)
			y = base_y;
	}

	return y;
}


void
html_cluev_type_init (void)
{
	html_cluev_class_init (&html_cluev_class, HTML_TYPE_CLUEV, sizeof (HTMLClueV));
}

void
html_cluev_class_init (HTMLClueVClass *klass,
		       HTMLType type,
		       guint size)
{
	HTMLObjectClass *object_class;
	HTMLClueClass *clue_class;

	object_class = HTML_OBJECT_CLASS (klass);
	clue_class = HTML_CLUE_CLASS (klass);

	html_clue_class_init (clue_class, type, size);

	object_class->copy = copy;
	object_class->calc_size = calc_size;
	object_class->relayout = relayout;
	object_class->set_max_width = set_max_width;
	object_class->reset = reset;
	object_class->draw = draw;
	object_class->check_point = check_point;
	object_class->check_page_split = check_page_split;
	object_class->get_left_margin = get_left_margin;
	object_class->get_right_margin = get_right_margin;

	clue_class->get_left_clear = get_left_clear;
	clue_class->get_right_clear = get_right_clear;
	clue_class->find_free_area = find_free_area;
	clue_class->appended = appended;
	clue_class->append_left_aligned = append_left_aligned;
	clue_class->append_right_aligned = append_right_aligned;

	parent_class = &html_clue_class;
}

void
html_cluev_init (HTMLClueV *cluev,
		 HTMLClueVClass *klass,
		 gint x, gint y,
		 gint percent)
{
	HTMLObject *object;
	HTMLClue *clue;

	object = HTML_OBJECT (cluev);
	clue = HTML_CLUE (cluev);

	html_clue_init (clue, HTML_CLUE_CLASS (klass));

	object->x = x;
	object->y = y;
	object->percent = percent;

	clue->valign = HTML_VALIGN_BOTTOM;
	clue->halign = HTML_HALIGN_LEFT;
	clue->head = clue->tail = clue->curr = 0;

	cluev->align_left_list = 0;
	cluev->align_right_list = 0;
	cluev->padding = 0;
}

HTMLObject *
html_cluev_new (gint x, gint y, gint percent)
{
	HTMLClueV *cluev;

	cluev = g_new (HTMLClueV, 1);
	html_cluev_init (cluev, &html_cluev_class, x, y, percent);
	
	return HTML_OBJECT (cluev);
}
