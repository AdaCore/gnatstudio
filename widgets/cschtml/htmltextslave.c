/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

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

#include "htmltextslave.h"
#include "htmlclue.h"
#include "htmlcursor.h"


/* #define HTML_TEXT_SLAVE_DEBUG */

HTMLTextSlaveClass html_text_slave_class;
static HTMLObjectClass *parent_class = NULL;


/* Split this TextSlave at the specified offset.  */
static void
split (HTMLTextSlave *slave, gshort offset)
{
	HTMLObject *obj;
	HTMLObject *new;

	g_return_if_fail (offset >= 0);
	g_return_if_fail (offset < slave->posLen);

	obj = HTML_OBJECT (slave);

	new = html_text_slave_new (slave->owner,
				   slave->posStart + offset,
				   slave->posLen - offset);

	html_clue_append_after (HTML_CLUE (obj->parent), new, obj);
}

/* Split this TextSlave at the first newline character.  */
static void
split_at_newline (HTMLTextSlave *slave)
{
	const gchar *text;
	const gchar *p;

	text = HTML_TEXT (slave->owner)->text + slave->posStart;

	p = memchr (text, '\n', slave->posLen);
	if (p == NULL)
		return;

	split (slave, p - text + 1);
}


/* HTMLObject methods.  */

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	/* FIXME it does not make much sense to me to share the owner.  */
	HTML_TEXT_SLAVE (dest)->owner = HTML_TEXT_SLAVE (self)->owner;
	HTML_TEXT_SLAVE (dest)->posStart = HTML_TEXT_SLAVE (self)->posStart;
	HTML_TEXT_SLAVE (dest)->posLen = HTML_TEXT_SLAVE (self)->posLen;
}

static gboolean calc_size(HTMLObject *self, HTMLPainter *painter) {
	HTMLText *owner;
	HTMLTextSlave *slave;
	CscHTMLFontStyle font_style;
	gchar *font_face;
	gint new_ascent, new_descent, new_width;
	gboolean changed;

	slave = HTML_TEXT_SLAVE(self);
	owner = HTML_TEXT(slave->owner);
	font_style = html_text_get_font_style(owner);
	font_face = html_text_get_font_face(owner);

	new_ascent = html_painter_calc_ascent(painter, font_style, font_face);
	new_descent = html_painter_calc_descent(painter, font_style, font_face);
	new_width = html_painter_calc_text_width(painter, owner->text + slave->posStart, slave->posLen, font_style, font_face);

	changed = FALSE;

	if (new_ascent != self->ascent) {
		self->ascent = new_ascent;
		changed = TRUE;
	}

	if (new_descent != self->descent) {
		self->descent = new_descent;
		changed = TRUE;
	}

	if (new_width != self->width) {
		self->width = new_width;
		changed = TRUE;
	}

	return changed;
}

static HTMLFitType
fit_line (HTMLObject *o,
	  HTMLPainter *painter,
	  gboolean startOfLine,
	  gboolean firstRun,
	  gint widthLeft)
{
	HTMLTextSlave *textslave;
	HTMLTextMaster *textmaster;
	HTMLText *ownertext;
	HTMLObject *next_obj;
	HTMLFitType return_value;
	CscHTMLFontStyle font_style;
	gchar *font_face;
	gint newLen;
	gint newWidth;
	gchar *splitPtr;
	gchar *text;

	textslave = HTML_TEXT_SLAVE (o);
	textmaster = HTML_TEXT_MASTER (textslave->owner);
	ownertext = HTML_TEXT (textmaster);

	font_style = html_text_get_font_style(ownertext);
	font_face = html_text_get_font_face(ownertext);

	next_obj = o->next;
	text = ownertext->text;

	/* Remove following existing slaves.  */

	if (next_obj != NULL
	    && (HTML_OBJECT_TYPE (next_obj) == HTML_TYPE_TEXTSLAVE)) {
		do {
			o->next = next_obj->next;
			html_clue_remove (HTML_CLUE (next_obj->parent), next_obj);
			html_object_destroy (next_obj);
			next_obj = o->next;
			if (next_obj != NULL)
				next_obj->prev = o;
		} while (next_obj && HTML_OBJECT_TYPE (next_obj) == HTML_TYPE_TEXTSLAVE);
		textslave->posLen = HTML_TEXT (textslave->owner)->text_len - textslave->posStart;
	}

	split_at_newline (HTML_TEXT_SLAVE (o));

	text += textslave->posStart;

	o->width = html_painter_calc_text_width(painter, text, textslave->posLen, font_style, font_face);
	if (o->width <= widthLeft || textslave->posLen <= 1 || widthLeft < 0) {
		/* Text fits completely */
		return_value = HTML_FIT_COMPLETE;
		goto done;
	} else {
		splitPtr = index (text + 1, ' ');
	}
	
	if (splitPtr) {
		newLen = splitPtr - text + 1;
		newWidth = html_painter_calc_text_width(painter, text, newLen, font_style, font_face);
		if (newWidth > widthLeft) {
			/* Splitting doesn't make it fit */
			splitPtr = 0;
		} else {
			gint extraLen;
			gint extraWidth;

			for (;;) {
				gchar *splitPtr2 = index (splitPtr + 1, ' ');
				if (!splitPtr2)
					break;
				extraLen = splitPtr2 - splitPtr;
				extraWidth = html_painter_calc_text_width(painter, splitPtr, extraLen, font_style, font_face);
				if (extraWidth + newWidth <= widthLeft) {
					/* We can break on the next separator cause it still fits */
					newLen += extraLen;
					newWidth += extraWidth;
					splitPtr = splitPtr2;
				} else {
					/* Using this separator would over-do it */
					break;
				}
			}
		}
	} else {
		newLen = textslave->posLen;
		newWidth = o->width;
	}
	
	if (!splitPtr) {
		/* No separator available */
		if (firstRun == FALSE) {
			/* Text does not fit, wait for next line */
			return_value = HTML_FIT_NONE;
			goto done;
		}

		/* Text doesn't fit, too bad. 
		   newLen & newWidth are valid */
	}

	if (textslave->posLen - newLen > 0)
		split (textslave, newLen);

	textslave->posLen = newLen;

	o->width = newWidth;
	o->ascent = html_painter_calc_ascent (painter, font_style, font_face);
	o->descent = html_painter_calc_descent (painter, font_style, font_face);

	return_value = HTML_FIT_PARTIAL;

 done:
#ifdef HTML_TEXT_SLAVE_DEBUG
	/* FIXME */
	{
		gint i;

		printf ("Split text");
		switch (return_value) {
		case HTML_FIT_PARTIAL:
			printf (" (Partial): `");
			break;
		case HTML_FIT_NONE:
			printf (" (NoFit): `");
			break;
		case HTML_FIT_COMPLETE:
			printf (" (Complete): `");
			break;
		}

		for (i = 0; i < textslave->posLen; i++)
			putchar (ownertext->text[textslave->posStart + i]);

		printf ("'\n");
	}
#endif

	return return_value;
}

static gboolean
select_range (HTMLObject *self,
	      HTMLEngine *engine,
	      guint start, gint length,
	      gboolean queue_draw)
{
	return FALSE;
}


/* HTMLObject::draw() implementation.  */

static void draw_normal(HTMLTextSlave *self, HTMLPainter *p, CscHTMLFontStyle font_style, gchar *font_face, gint x, gint y, gint width, gint height, gint tx, gint ty) {
	HTMLObject *obj;

	obj = HTML_OBJECT(self);

	html_painter_set_font_style(p, font_style);
	html_painter_set_font_face(p, font_face);
	html_painter_set_pen(p, html_text_get_color(HTML_TEXT(self->owner), p));
	html_painter_draw_text (p, obj->x + tx, obj->y + ty, HTML_TEXT(self->owner)->text + self->posStart, self->posLen);
}

static void draw_highlighted(HTMLTextSlave *slave, HTMLPainter *p, CscHTMLFontStyle font_style,gchar *font_face, gint x, gint y, gint width, gint height, gint tx, gint ty) {
	HTMLTextMaster *owner;
	HTMLObject *obj;
	guint start, end, len;
	guint offset_width, text_width;
	const gchar *text;

	obj = HTML_OBJECT(slave);
	owner = HTML_TEXT_MASTER(slave->owner);
	start = owner->select_start;
	end = start + owner->select_length;

	text = HTML_TEXT (owner)->text;

	if (start < slave->posStart)
		start = slave->posStart;
	if (end > slave->posStart + slave->posLen)
		end = slave->posStart + slave->posLen;
	len = end - start;

	offset_width = html_painter_calc_text_width(p, text + slave->posStart, start - slave->posStart, font_style, font_face);
	text_width = html_painter_calc_text_width(p, text + start, len, font_style, font_face);

	html_painter_set_font_style(p, font_style);
	html_painter_set_font_face(p, font_face);

	/* Draw the highlighted part with a highlight background.  */

	html_painter_set_pen(p, html_painter_get_default_highlight_color(p));
	html_painter_fill_rect(p, obj->x + tx + offset_width, obj->y + ty - obj->ascent, text_width, obj->ascent + obj->descent);
	html_painter_set_pen(p, html_painter_get_default_highlight_foreground_color(p));
	html_painter_draw_text(p, obj->x + tx + offset_width, obj->y + ty, text + start, len);

	/* Draw the non-highlighted part.  */

	html_painter_set_pen(p, html_text_get_color(HTML_TEXT(owner), p));

	/* 1. Draw the leftmost non-highlighted part, if any.  */

	if (start > slave->posStart)
		html_painter_draw_text (p, obj->x + tx, obj->y + ty, text + slave->posStart, start - slave->posStart);

	/* 2. Draw the rightmost non-highlighted part, if any.  */

	if (end < slave->posStart + slave->posLen)
		html_painter_draw_text (p, obj->x + tx + offset_width + text_width, obj->y + ty, text + end, slave->posStart + slave->posLen - end);
}

static void draw(HTMLObject *o, HTMLPainter *p, gint x, gint y, gint width, gint height, gint tx, gint ty) {
	HTMLTextSlave *textslave;
	HTMLTextMaster *owner;
	HTMLText *ownertext;
	CscHTMLFontStyle font_style;
	gchar *font_face;
	guint end;
	ArtIRect paint;

	html_object_calc_intersection(o, &paint, x, y, width, height);
	if (art_irect_empty(&paint))
		return;
	
	textslave = HTML_TEXT_SLAVE(o);
	owner = textslave->owner;
	ownertext = HTML_TEXT(owner);
	font_style = html_text_get_font_style(ownertext);
	font_face = html_text_get_font_face(ownertext);

	end = textslave->posStart + textslave->posLen;

	if (owner->select_start + owner->select_length <= textslave->posStart
	    || owner->select_start >= end) {
		draw_normal(textslave, p, font_style, font_face, x, y, width, height, tx, ty);
	} else {
		draw_highlighted(textslave, p, font_style, font_face, x, y, width, height, tx, ty);
	}
}

static gint
calc_min_width (HTMLObject *o,
		HTMLPainter *painter)
{
	return 0;
}

static gint
calc_preferred_width (HTMLObject *o,
		      HTMLPainter *painter)
{
	return 0;
}

static const gchar *
get_url (HTMLObject *o)
{
	HTMLTextSlave *slave;

	slave = HTML_TEXT_SLAVE (o);
	return html_object_get_url (HTML_OBJECT (slave->owner));
}

static HTMLObject *
check_point (HTMLObject *self,
	     HTMLPainter *painter,
	     gint x, gint y,
	     guint *offset_return,
	     gboolean for_cursor)
{
	return NULL;
}


void
html_text_slave_type_init (void)
{
	html_text_slave_class_init (&html_text_slave_class, HTML_TYPE_TEXTSLAVE, sizeof (HTMLTextSlave));
}

void
html_text_slave_class_init (HTMLTextSlaveClass *klass,
			    HTMLType type,
			    guint object_size)
{
	HTMLObjectClass *object_class;

	object_class = HTML_OBJECT_CLASS (klass);

	html_object_class_init (object_class, type, object_size);

	object_class->select_range = select_range;
	object_class->copy = copy;
	object_class->draw = draw;
	object_class->calc_size = calc_size;
	object_class->fit_line = fit_line;
	object_class->calc_min_width = calc_min_width;
	object_class->calc_preferred_width = calc_preferred_width;
	object_class->get_url = get_url;
	object_class->check_point = check_point;

	parent_class = &html_object_class;
}

void
html_text_slave_init (HTMLTextSlave *slave,
		      HTMLTextSlaveClass *klass,
		      HTMLTextMaster *owner,
		      gint posStart,
		      gint posLen)
{
	HTMLText *owner_text;
	HTMLObject *object;

	object = HTML_OBJECT (slave);
	owner_text = HTML_TEXT (owner);

	html_object_init (object, HTML_OBJECT_CLASS (klass));

	object->ascent = HTML_OBJECT (owner)->ascent;
	object->descent = HTML_OBJECT (owner)->descent;

	slave->posStart = posStart;
	slave->posLen = posLen;
	slave->owner = owner;

	/* text slaves has always min_width 0 */
	object->min_width = 0;
	object->change   &= ~HTML_CHANGE_MIN_WIDTH;
}

HTMLObject *
html_text_slave_new (HTMLTextMaster *owner, gint posStart, gint posLen)
{
	HTMLTextSlave *slave;

	slave = g_new (HTMLTextSlave, 1);
	html_text_slave_init (slave, &html_text_slave_class, owner, posStart, posLen);

	return HTML_OBJECT (slave);
}

guint html_text_slave_get_offset_for_pointer(HTMLTextSlave *slave, HTMLPainter *painter, gint x, gint y) {
	HTMLText *owner;
	CscHTMLFontStyle font_style;
	gchar *font_face;
	guint width, prev_width;
	guint i;

	g_return_val_if_fail(slave != NULL, 0);

	owner = HTML_TEXT(slave->owner);
	font_style = html_text_get_font_style(owner);
	font_face = html_text_get_font_face(owner);

	x -= HTML_OBJECT(slave)->x;

	prev_width = 0;
	for (i = 1; i <= slave->posLen; i++) {
		width = html_painter_calc_text_width(painter, owner->text + slave->posStart, i, font_style, font_face);

		if ((width + prev_width) / 2 >= x)
			return i - 1;

		prev_width = width;
	}

	return slave->posLen;
}

gint html_text_slave_trail_space_width(HTMLTextSlave *slave, HTMLPainter *painter) {
	
	if (slave->posLen == 0)
		return 0;

	if (HTML_TEXT(slave->owner)->text[slave->posStart + slave->posLen - 1] == ' ') {
		CscHTMLFontStyle font_style;
		gchar *font_face;

		font_style = html_text_get_font_style(HTML_TEXT(slave->owner));
		font_face = html_text_get_font_face(HTML_TEXT(slave->owner));
		return html_painter_calc_text_width(painter, " ", 1, font_style, font_face);
	} else {
		return 0;
	}
}
