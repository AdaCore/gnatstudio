/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
   Copyright (C) 1999 Anders Carlsson (andersca@gnu.org)
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

#include <glib.h>
#include "htmlcluev.h"
#include "htmltablecell.h"
#include "htmlobject.h"
#include <libart_lgpl/art_rect.h>

/* FIXME: This always behaves as a transparent object, even when it
   actually is not.  */


HTMLTableCellClass html_table_cell_class;
static HTMLClueVClass *parent_class = NULL;


static void
draw_background_helper (HTMLTableCell *cell,
			HTMLPainter *p,
			ArtIRect *paint,
			gint tx, gint ty)
{
	HTMLObject *o;
	GdkPixbuf  *pixbuf = NULL;
	GdkColor   *color = NULL;

	o = HTML_OBJECT (cell);

	if (cell->have_bg) {
		if (! cell->bg_allocated) {
			html_painter_alloc_color (p, &cell->bg);
			cell->bg_allocated = TRUE;
		}
		color = &cell->bg;
	}

	if (cell->have_bgPixmap) {
		if (cell->bgPixmap->pixbuf) {
			pixbuf = cell->bgPixmap->pixbuf;
		}
	}
	html_painter_draw_background (p,
				      color,
				      pixbuf,
				      tx + paint->x0,
				      ty + paint->y0,
				      paint->x1 - paint->x0,
				      paint->y1 - paint->y0,
				      paint->x0 - o->x,
				      paint->y0 - (o->y - o->ascent));
	
}


/* HTMLObject methods.  */

static void
reset (HTMLObject *self)
{
	HTMLTableCell *cell;

	cell = HTML_TABLE_CELL (self);

	cell->bg_allocated = FALSE;

	(* HTML_OBJECT_CLASS (parent_class)->reset) (self);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	g_warning ("HTMLTableCell::copy is not implemented.");
}

static gint
calc_min_width (HTMLObject *o,
		HTMLPainter *painter)
{
	HTMLObject *obj;
	gint minWidth;

	minWidth = 0; 
	for (obj = HTML_CLUE (o)->head; obj != NULL; obj = obj->next) {
		gint w;

		w = html_object_calc_min_width (obj, painter);
		if (w > minWidth)
			minWidth = w;
	}

	if (o->flags & HTML_OBJECT_FLAG_FIXEDWIDTH) {
		gint fixed_width;
		gint pixel_size;

		/* Our minimum width is at least our fixed width */

		pixel_size = html_painter_get_pixel_size (painter);
		fixed_width = HTML_TABLE_CELL (o)->fixed_width * pixel_size;
		
		if (fixed_width > minWidth)
			minWidth = fixed_width;
	}

	return minWidth;
}

static void
set_max_width (HTMLObject *o,
	       HTMLPainter *painter,
	       gint max_width)
{
	HTMLObject *obj;

	o->max_width = max_width;

	for (obj = HTML_CLUE (o)->head; obj != NULL; obj = obj->next)
		html_object_set_max_width (obj, painter, max_width);
}

static void
draw (HTMLObject *o,
      HTMLPainter *p,
      gint x, gint y, 
      gint width, gint height,
      gint tx, gint ty)
{
	HTMLTableCell *cell = HTML_TABLE_CELL (o);
	ArtIRect paint;

	
	html_object_calc_intersection (o, &paint, x, y, width, height);
	if (art_irect_empty (&paint))
	    return;
	
	draw_background_helper (cell, p, &paint, tx, ty);

	(* HTML_OBJECT_CLASS (&html_cluev_class)->draw) (o, p, x, y, width, height, tx, ty);
}

static void
draw_background (HTMLObject *self,
		 HTMLPainter *painter,
		 gint x, gint y, 
		 gint width, gint height,
		 gint tx, gint ty)
{
	ArtIRect paint;
	
	(* HTML_OBJECT_CLASS (parent_class)->draw_background) (self, painter, x, y, width, height, tx, ty);

	html_object_calc_intersection (self, &paint, x, y, width, height);
	if (art_irect_empty (&paint))
	    return;

	draw_background_helper (HTML_TABLE_CELL (self), painter, &paint, tx, ty);
}

static void
set_bg_color (HTMLObject *object, GdkColor *color)
{
	HTMLTableCell *cell;

	cell = HTML_TABLE_CELL (object);

	if (color == NULL) {
		cell->have_bg = FALSE;
		return;
	}

	if (cell->have_bg && ! gdk_color_equal (&cell->bg, color))
		cell->bg_allocated = FALSE;

	cell->bg = *color;
	cell->have_bg = TRUE;
}


void
html_table_cell_type_init (void)
{
	html_table_cell_class_init (&html_table_cell_class, HTML_TYPE_TABLECELL, sizeof (HTMLTableCell));
}

void
html_table_cell_class_init (HTMLTableCellClass *klass,
			    HTMLType type,
			    guint object_size)
{
	HTMLObjectClass *object_class;
	HTMLClueVClass *cluev_class;

	object_class = HTML_OBJECT_CLASS (klass);
	cluev_class = HTML_CLUEV_CLASS (klass);

	html_cluev_class_init (cluev_class, type, object_size);

	object_class->reset = reset;
	object_class->copy = copy;
	object_class->calc_min_width = calc_min_width;
	object_class->set_max_width = set_max_width;
	object_class->draw = draw;
	object_class->draw_background = draw_background;
	object_class->set_bg_color = set_bg_color;

	parent_class = &html_cluev_class;
}

void
html_table_cell_init (HTMLTableCell *cell,
		      HTMLTableCellClass *klass,
		      gint percent,
		      gint rs, gint cs,
		      gint pad)
{
	HTMLObject *object;
	HTMLClueV *cluev;
	HTMLClue *clue;

	object = HTML_OBJECT (cell);
	cluev = HTML_CLUEV (cell);
	clue = HTML_CLUE (cell);

	html_cluev_init (cluev, HTML_CLUEV_CLASS (klass), 0, 0, percent);

	object->flags &= ~HTML_OBJECT_FLAG_FIXEDWIDTH;

	clue->valign = HTML_VALIGN_BOTTOM;
	clue->halign = HTML_HALIGN_LEFT;

	cell->fixed_width = 0;

	cell->padding = 0;
	cluev->padding = pad;
	cell->refcount = 0;
	cell->rspan = rs;
	cell->cspan = cs;

	cell->have_bg = FALSE;
	cell->have_bgPixmap = FALSE;
	cell->bg_allocated = FALSE;
}

HTMLObject *
html_table_cell_new (gint percent,
		     gint rs, gint cs,
		     gint pad)
{
	HTMLTableCell *cell;

	cell = g_new (HTMLTableCell, 1);
	html_table_cell_init (cell, &html_table_cell_class, percent, rs, cs, pad);

	return HTML_OBJECT (cell);
}

void
html_table_cell_link (HTMLTableCell *cell)
{
	cell->refcount++;
}

void
html_table_cell_unlink (HTMLTableCell *cell)
{
	cell->refcount--;

	if (cell->refcount == 0)
		html_object_destroy (HTML_OBJECT (cell));
}

void
html_table_cell_set_fixed_width (HTMLTableCell *cell,
				 gint width)
{
	HTMLObject *obj;

	obj = HTML_OBJECT (cell);
	obj->flags |= HTML_OBJECT_FLAG_FIXEDWIDTH;

	cell->fixed_width = width;
}

void
html_table_cell_set_bg_pixmap (HTMLTableCell *cell,
				    HTMLImagePointer *imagePtr)
{
	if(imagePtr) {
		cell->have_bgPixmap = TRUE;
		cell->bgPixmap = imagePtr;
	}
}
