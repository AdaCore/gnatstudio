/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

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

#include "htmlengine-edit-cursor.h"

#include "htmldrawqueue.h"


/* HTMLDrawQueueClearElement handling.  */

static HTMLDrawQueueClearElement *
clear_element_new (gint            x,
		   gint            y,
		   guint           width,
		   guint           height,
		   const GdkColor *background_color)
{
	HTMLDrawQueueClearElement *new;

	new = g_new (HTMLDrawQueueClearElement, 1);

	new->x = x;
	new->y = y;
	new->width = width;
	new->height = height;

	/* GDK color API non const-correct.  */
	new->background_color = gdk_color_copy ((GdkColor *) background_color);

	new->background_image = NULL;
	new->background_image_x_offset = 0;
	new->background_image_y_offset = 0;

	return new;
}

static HTMLDrawQueueClearElement *
clear_element_new_with_background (gint       x,
				   gint       y,
				   guint      width,
				   guint      height,
				   GdkPixbuf *background_image,
				   guint      background_image_x_offset,
				   guint      background_image_y_offset)
{
	HTMLDrawQueueClearElement *new;

	new = g_new (HTMLDrawQueueClearElement, 1);

	new->x = x;
	new->y = y;
	new->width = width;
	new->height = height;

	new->background_image = gdk_pixbuf_ref (background_image);

	new->background_image_x_offset = background_image_x_offset;
	new->background_image_y_offset = background_image_y_offset;

	new->background_color = NULL;

	return new;
}

static void
clear_element_destroy (HTMLDrawQueueClearElement *elem)
{
	g_return_if_fail (elem != NULL);

	if (elem->background_color != NULL)
		gdk_color_free (elem->background_color);

	if (elem->background_image != NULL)
		gdk_pixbuf_unref (elem->background_image);

	g_free (elem);
}


HTMLDrawQueue *
html_draw_queue_new (HTMLEngine *engine)
{
	HTMLDrawQueue *new;

	g_return_val_if_fail (engine != NULL, NULL);

	new = g_new (HTMLDrawQueue, 1);

	new->engine = engine;

	new->elems = NULL;
	new->last = NULL;

	new->clear_elems = NULL;
	new->clear_last = NULL;

	return new;
}

void
html_draw_queue_destroy (HTMLDrawQueue *queue)
{
	GList *p;

	g_return_if_fail (queue != NULL);

	for (p = queue->elems; p != NULL; p = p->next) {
		HTMLObject *obj;

		obj = p->data;
		obj->redraw_pending = FALSE;
	}

	g_list_free (queue->elems);

	g_free (queue);
}

void
html_draw_queue_add (HTMLDrawQueue *queue, HTMLObject *object)
{
	g_return_if_fail (queue != NULL);
	g_return_if_fail (object != NULL);

	if (object->redraw_pending)
		return;

	object->redraw_pending = TRUE;

	queue->last = g_list_append (queue->last, object);
	if (queue->elems == NULL && queue->clear_elems == NULL)
		gtk_signal_emit_by_name (GTK_OBJECT (queue->engine), "draw_pending");

	if (queue->elems == NULL)
		queue->elems = queue->last;
	else
		queue->last = queue->last->next;
}


static void
add_clear (HTMLDrawQueue *queue,
	   HTMLDrawQueueClearElement *elem)
{
	queue->clear_last = g_list_append (queue->clear_last, elem);
	if (queue->elems == NULL && queue->clear_elems == NULL)
		gtk_signal_emit_by_name (GTK_OBJECT (queue->engine), "draw_pending");

	if (queue->clear_elems == NULL)
		queue->clear_elems = queue->clear_last;
	else
		queue->clear_last = queue->clear_last->next;
}

void
html_draw_queue_add_clear (HTMLDrawQueue *queue,
			   gint x,
			   gint y,
			   guint width,
			   guint height,
			   const GdkColor *background_color)
{
	HTMLDrawQueueClearElement *new;

	g_return_if_fail (queue != NULL);
	g_return_if_fail (background_color != NULL);

	new = clear_element_new (x, y, width, height, background_color);
	add_clear (queue, new);
}

void
html_draw_queue_add_clear_with_background  (HTMLDrawQueue *queue,
					    gint x,
					    gint y,
					    guint width,
					    guint height,
					    GdkPixbuf *background_image,
					    guint background_image_x_offset,
					    guint background_image_y_offset)
{
	HTMLDrawQueueClearElement *new;

	g_return_if_fail (queue != NULL);
	g_return_if_fail (background_image != NULL);

	new = clear_element_new_with_background (x, y, width, height, background_image,
						 background_image_x_offset, background_image_y_offset);
	add_clear (queue, new);
}


static void
draw_obj (HTMLDrawQueue *queue,
	  HTMLObject *obj)
{
	HTMLEngine *e;
	HTMLObject *p;
	gint x1, y1, x2, y2;
	gint tx, ty;

	if (obj->width == 0 || obj->ascent + obj->descent == 0)
		return;

	e = queue->engine;

	/* First calculate the translation offset for drawing the object.  */

	tx = 0;
	ty = 0;

	for (p = obj->parent; p != NULL; p = p->parent) {
		tx += p->x;
		ty += p->y - p->ascent;
	}

	tx = tx + e->leftBorder - e->x_offset;
	ty = ty + e->topBorder - e->y_offset;

	/* Then prepare for drawing.  We will only update this object, so we
           only allocate enough size for it.  */

	x1 = obj->x + tx;
	y1 = obj->y - obj->ascent + ty;
	x2 = obj->x + obj->width + tx;
	y2 = obj->y + obj->descent + ty;

	if (x2 < 0 || y2 < 0 || x1 > e->width || y1 > e->height)
		return;

	if (x1 < 0)
		x1 = 0;
	if (y1 < 0)
		y1 = 0;
	if (x2 > e->width)
		x2 = e->width;
	if (y2 > e->height)
		y2 = e->height;

	html_painter_begin (e->painter, x1, y1, x2, y2);

	/* FIXME we are duplicating code from HTMLEngine here.
           Instead, there should be a function in HTMLEngine to paint
           stuff.  */

	/* Draw the actual object.  */

	if (html_object_is_transparent (obj)) {
		html_engine_draw_background (e, x1, y1, x2, y2);
		html_object_draw_background (obj, e->painter,
					     obj->x, obj->y - obj->ascent,
					     obj->width, obj->ascent + obj->descent,
					     tx, ty);
	}

	html_object_draw (obj,
			  e->painter, 
			  obj->x, obj->y - obj->ascent,
			  obj->width, obj->ascent + obj->descent,
			  tx, ty);

#if 0
	{
		GdkColor c;

		c.pixel = rand ();
		html_painter_set_pen (e->painter, &c);
		html_painter_draw_line (e->painter, x1, y1, x2 - 1, y2 - 1);
		html_painter_draw_line (e->painter, x2 - 1, y1, x1, y2 - 1);
	}
#endif

	/* Done.  */

	html_painter_end (e->painter);

	if (e->editable)
		html_engine_draw_cursor_in_area (e, x1, y1, x2 - x1, y2 - y1);
}

static void
clear (HTMLDrawQueue *queue,
       HTMLDrawQueueClearElement *elem)
{
	HTMLEngine *e;
	gint x1, y1, x2, y2;

	e = queue->engine;

	x1 = elem->x + e->leftBorder - e->x_offset;
	y1 = elem->y + e->topBorder - e->y_offset;

	x2 = x1 + elem->width;
	y2 = y1 + elem->height;

	html_painter_begin (e->painter, x1, y1, x2, y2);

	if (elem->background_color != NULL) {
		html_painter_set_pen (e->painter, elem->background_color);
		html_painter_fill_rect (e->painter, x1, y1, x2, y2);
	}

	/* TODO: Background pixmap.  */

#if 0
	html_painter_set_pen (e->painter, html_painter_get_black (e->painter));
	html_painter_draw_line (e->painter, x1, y1, x2 - 1, y2 - 1);
	html_painter_draw_line (e->painter, x2 - 1, y1, x1, y2 - 1);
#endif

	html_painter_end (e->painter);

	if (e->editable)
		html_engine_draw_cursor_in_area (e, x1, y1, x2 - x1, y2 - y1);
}

void
html_draw_queue_flush (HTMLDrawQueue *queue)
{
	GList *p;

	/* Draw clear areas.  */

	for (p = queue->clear_elems; p != NULL; p = p->next) {
		HTMLDrawQueueClearElement *clear_elem;

		clear_elem = p->data;
		clear (queue, clear_elem);
		clear_element_destroy (clear_elem);
	}

	g_list_free (queue->clear_elems);

	queue->clear_elems = NULL;
	queue->clear_last = NULL;

	/* Draw objects.  */

	for (p = queue->elems; p != NULL; p = p->next) {
		HTMLObject *obj;

		obj = p->data;

		if (obj->free_pending) {
			g_free (obj);
		} else if (obj->redraw_pending) {
			draw_obj (queue, obj);
			obj->redraw_pending = FALSE;
		}
	}

	g_list_free (queue->elems);

	queue->elems = NULL;
	queue->last = NULL;
}
