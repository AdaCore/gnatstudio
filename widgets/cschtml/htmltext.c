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

#include <stdio.h>
#include <string.h>

#include "htmltext.h"
#include "htmlclueflow.h"
#include "htmlcursor.h"
#include "htmlentity.h"

HTMLTextClass html_text_class;
static HTMLObjectClass *parent_class = NULL;

#define HT_CLASS(x) HTML_TEXT_CLASS (HTML_OBJECT (x)->klass)

static void
get_tags (const HTMLText *text,
	  gchar *opening_tags,
	  gchar *ending_tags)
{
	CscHTMLFontStyle font_style;
	gchar *opening_p, *ending_p;
	guint size;

	font_style = text->font_style;

	/*
	  FIXME: this is completely broken in that there is no
	  possible way the tag order can come out right doing it this
	  way
	
	CZ: Also needs support for <font face=> */

	opening_p = opening_tags;
	ending_p = ending_tags;

	size = font_style & CSC_HTML_FONT_STYLE_SIZE_MASK;
	if (size != 0) {
		opening_p += sprintf (opening_p, "<FONT SIZE=%d>", size);
	}

	if (font_style & CSC_HTML_FONT_STYLE_BOLD) {
		opening_p += sprintf (opening_p, "<B>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_ITALIC) {
		opening_p += sprintf (opening_p, "<I>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_UNDERLINE) {
		opening_p += sprintf (opening_p, "<U>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_STRIKEOUT) {
		opening_p += sprintf (opening_p, "<S>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_FIXED) {
		opening_p += sprintf (opening_p, "<TT>");
		ending_p += sprintf (ending_p, "</TT>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_STRIKEOUT) {
		ending_p += sprintf (ending_p, "</S>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_UNDERLINE) {
		ending_p += sprintf (ending_p, "</U>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_ITALIC) {
		ending_p += sprintf (ending_p, "</I>");
	}

	if (font_style & CSC_HTML_FONT_STYLE_BOLD) {
		ending_p += sprintf (ending_p, "</B>");
	}

	if (size != 0) {
		ending_p += sprintf (ending_p, "</FONT SIZE=%d>", size);
	}


	*opening_p = 0;
	*ending_p = 0;
}

static void copy_helper(HTMLText *src, HTMLText *dest, guint offset, gint len) {
	
	if (len < 0)
		len = strlen(src->text);

	dest->text = g_strndup(src->text + offset, len);
	dest->text_len = len;

	dest->font_style = src->font_style;
	dest->font_face = src->font_face;
	dest->color = src->color;
	dest->color_allocated = src->color_allocated;
}

/* HTMLObject methods.  */

static void
reset (HTMLObject *self)
{
	HTMLText *text;

	text = HTML_TEXT (self);
	text->color_allocated = FALSE;

	(* HTML_OBJECT_CLASS (parent_class)->reset) (self);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	copy_helper (HTML_TEXT (self), HTML_TEXT (dest), 0, -1);
}

static gboolean calc_size(HTMLObject *self, HTMLPainter *painter) {
	HTMLText *text;
	CscHTMLFontStyle font_style;
	gchar *font_face;
	gint new_ascent, new_descent, new_width;
	gboolean changed;

	text = HTML_TEXT(self);
	font_style = html_text_get_font_style(text);
	font_face = html_text_get_font_face(text);

	new_ascent = html_painter_calc_ascent(painter, font_style, font_face);
	new_descent = html_painter_calc_descent(painter, font_style, font_face);
	new_width = html_painter_calc_text_width(painter, text->text, text->text_len, font_style, font_face);

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

static void draw(HTMLObject *o, HTMLPainter *p, gint x, gint y, gint width, gint height, gint tx, gint ty) {
	CscHTMLFontStyle font_style;
	gchar *font_face;
	HTMLText *htmltext;

	htmltext = HTML_TEXT(o);

	if (y + height < o->y - o->ascent || y > o->y + o->descent)
		return;

	font_style = html_text_get_font_style(htmltext);
	font_face = html_text_get_font_face(htmltext);
	html_painter_set_font_style(p, font_style);
	html_painter_set_font_face(p, font_face);

	html_painter_set_pen(p, html_text_get_color(htmltext, p));
	html_painter_draw_text(p, o->x + tx, o->y + ty, htmltext->text, -1);
}

static gboolean
accepts_cursor (HTMLObject *object)
{
	return TRUE;
}

static gboolean
save (HTMLObject *self,
      HTMLEngineSaveState *state)
{
	/* OK, doing these nasty things is not in my style, but in this case
           it's so unlikely to break and it's so handy and fast that I think
           it's almost acceptable.  */
#define RIDICULOUS_BUFFER_SIZE 16384
	gchar opening_tags[RIDICULOUS_BUFFER_SIZE];
	gchar closing_tags[RIDICULOUS_BUFFER_SIZE];
#undef RIDICULOUS_BUFFER_SIZE
	HTMLText *text;

	text = HTML_TEXT (self);

	get_tags (text, opening_tags, closing_tags);

	if (! html_engine_save_output_string (state, opening_tags))
		return FALSE;

	if (! html_engine_save_encode (state, text->text, text->text_len))
		return FALSE;

	if (! html_engine_save_output_string (state, closing_tags))
		return FALSE;

	return TRUE;
}

static gboolean
save_plain (HTMLObject *self,
      HTMLEngineSaveState *state)
{
	HTMLText *text;
	char *str = NULL;
	text = HTML_TEXT (self);

	if (! html_engine_save_output_string (state, text->text))
		return FALSE;
	
	return TRUE;
}




/* HTMLText methods.  */

static void
queue_draw (HTMLText *text,
	    HTMLEngine *engine,
	    guint offset,
	    guint len)
{
	html_engine_queue_draw (engine, HTML_OBJECT (text));
}

static HTMLText *
extract_text (HTMLText *text,
	      guint offset,
	      gint len)
{
	HTMLText *new;

	if (len < 0)
		len = text->text_len;

	if (offset + len > text->text_len)
		len = text->text_len - offset;

	new = g_malloc (HTML_OBJECT (text)->klass->object_size);

	(* HTML_OBJECT_CLASS (parent_class)->copy) (HTML_OBJECT (text), HTML_OBJECT (new));

	copy_helper (HTML_TEXT (text), HTML_TEXT (new), offset, len);

	return new;
}

/* #define DEBUG_NBSP */

static void
convert_nbsp (guchar *s, guint len)
{
	/* state of automata:
	   0..Text
	   1..Sequence <space>&nbsp;...&nbsp;
	*/
	guint state=0;

#ifdef DEBUG_NBSP
	printf ("convert_nbsp: ");
#endif
	while (len) {
		if (*s == ENTITY_NBSP || *s == ' ') {
			*s = state ? ENTITY_NBSP : ' ';
			state = 1;
		} else
			state = 0;
#ifdef DEBUG_NBSP
		if (*s == ENTITY_NBSP)
			printf ("&nbsp;");
		else
			printf ("%c", *s);
#endif
		len--;
		s++;
	}
#ifdef DEBUG_NBSP
	printf ("\n");
#endif
}

static guint
insert_text (HTMLText *text,
	     HTMLEngine *engine,
	     guint offset,
	     const gchar *s,
	     guint len)
{
	gchar *new_buffer;
	guint old_len;
	guint new_len;

	old_len = text->text_len;
	if (offset > old_len) {
		g_warning ("Cursor offset out of range for HTMLText::insert_text().");

		/* This should never happen, but the following will make sure
                   things are always fixed up in a non-segfaulting way.  */
		offset = old_len;
	}

	new_len    = old_len + len;
	new_buffer = g_malloc (new_len + 1);

	/* concatenate strings */
	memcpy (new_buffer,                text->text,        offset);
	memcpy (new_buffer + offset,       s,                 len);
	memcpy (new_buffer + offset + len, text->text+offset, old_len - offset);
	new_buffer[new_len] = '\0';

	/* do <space>&nbsp;...&nbsp; hack */
	convert_nbsp (new_buffer, new_len);

	/* set new values */
	g_free (text->text);
	text->text = new_buffer;
	text->text_len = new_len;

	/* update */
	html_object_change_set (HTML_OBJECT (text), HTML_CHANGE_ALL);
	if (HTML_OBJECT (text)->parent != NULL) {
		if (! html_object_relayout (HTML_OBJECT (text)->parent,
					    engine,
					    HTML_OBJECT (text))) 
			html_engine_queue_draw (engine, HTML_OBJECT (text)->parent);
	}

	return len;
}

static guint
remove_text (HTMLText *text,
	     HTMLEngine *engine,
	     guint offset,
	     guint len)
{
	gchar *new_buffer;
	guint old_len;
	guint new_len;

	/* The following code is very stupid and quite inefficient, but it is
           just for interactive editing so most likely people won't even
           notice.  */

	old_len = strlen (text->text);

	if (offset > old_len) {
		g_warning ("Cursor offset out of range for HTMLText::remove_text().");
		return 0;
	}

	if (offset + len > old_len || len == 0)
		len = old_len - offset;

	new_len = old_len - len;

	/* concat strings */
	new_buffer = g_malloc (new_len + 1);
	memcpy (new_buffer,          text->text,                offset);
	memcpy (new_buffer + offset, text->text + offset + len, old_len - offset - len + 1);

	/* do <space>&nbsp;...&nbsp; hack */
	convert_nbsp (new_buffer, new_len);

	/* set new values */
	g_free (text->text);
	text->text = new_buffer;
	text->text_len = new_len;

	/* update */
	html_object_change_set (HTML_OBJECT (text), HTML_CHANGE_ALL);
	html_object_relayout (HTML_OBJECT (text)->parent, engine, HTML_OBJECT (text));
	html_engine_queue_draw (engine, HTML_OBJECT (text)->parent);

	return len;
}

static void
get_cursor (HTMLObject *self,
	    HTMLPainter *painter,
	    guint offset,
	    gint *x1, gint *y1,
	    gint *x2, gint *y2)
{
	html_object_get_cursor_base (self, painter, offset, x2, y2);

	*x1 = *x2;
	*y1 = *y2 - self->ascent;
	*y2 += self->descent - 1;
}

static void get_cursor_base(HTMLObject *self, HTMLPainter *painter, guint offset, gint *x, gint *y) {
	CscHTMLFontStyle font_style;
	gchar *font_face;

	html_object_calc_abs_position(HTML_OBJECT(self), x, y);

	if (offset > 0) {
		font_style = html_text_get_font_style(HTML_TEXT(self));
		font_face = html_text_get_font_face(HTML_TEXT(self));
		*x += html_painter_calc_text_width (painter, HTML_TEXT (self)->text, offset, font_style, font_face);
	}
}

static HTMLText *split(HTMLText *self, guint offset) {
	HTMLText *new;

	new = g_malloc(HTML_OBJECT(self)->klass->object_size);
	html_text_init(new, HTML_TEXT_CLASS(HTML_OBJECT (self)->klass), self->text + offset, -1, self->font_style, &self->color, self->font_face);

	self->text = g_realloc(self->text, offset + 1);
	self->text[offset] = '\0';
	self->text_len = offset;
	html_object_change_set(HTML_OBJECT(self), HTML_CHANGE_MIN_WIDTH);

	return new;
}

static void
merge (HTMLText *text,
       HTMLText *other,
       gboolean prepend)
{
	g_warning ("HTMLText::merge not implemented.");
}

static gboolean check_merge(HTMLText *self, HTMLText *text) {
	
	if (HTML_OBJECT_TYPE(self) != HTML_OBJECT_TYPE(text))
		return FALSE;
	if (self->text_len == 0 || text->text_len == 0)
		return TRUE;
	if (self->font_style != text->font_style)
		return FALSE;
	if (g_strcasecmp(self->font_face, text->font_face) != 0)
		return FALSE;
	if (! gdk_color_equal (&self->color, &text->color))
		return FALSE;

	return TRUE;
}

/* This is necessary to merge the text-specified font style with that of the
   HTMLClueFlow parent.  */
static CscHTMLFontStyle get_font_style(const HTMLText *text) {
	HTMLObject *parent;
	CscHTMLFontStyle font_style;

	parent = HTML_OBJECT(text)->parent;

	if (HTML_OBJECT_TYPE(parent) == HTML_TYPE_CLUEFLOW) {
		CscHTMLFontStyle parent_style;

		parent_style = html_clueflow_get_default_font_style(HTML_CLUEFLOW (parent));
		font_style = csc_html_font_style_merge(parent_style, text->font_style);
	} else {
		font_style = csc_html_font_style_merge(CSC_HTML_FONT_STYLE_SIZE_3, text->font_style);
	}

	return font_style;
}

static const gchar *get_font_face(const HTMLText *text) {

	return text->font_face;
}

static const GdkColor *get_color(HTMLText *text, HTMLPainter *painter) {
	
	if (! text->color_allocated)
		html_painter_alloc_color(painter, &text->color);

	return &text->color;
}

static void set_font_style(HTMLText *text, HTMLEngine *engine, CscHTMLFontStyle style) {
	if (text->font_style == style)
		return;

	text->font_style = style;

	html_object_change_set(HTML_OBJECT(text), HTML_CHANGE_ALL);

	if (engine != NULL) {
		html_object_relayout(HTML_OBJECT(text)->parent, engine, HTML_OBJECT(text));
		html_engine_queue_draw(engine, HTML_OBJECT(text));
	}
}

static void set_font_face(HTMLText *text, HTMLEngine *engine, const gchar *font_face) {

	if (g_strcasecmp(text->font_face, font_face) == 0)
		return;

	text->font_face = font_face;

	html_object_change_set(HTML_OBJECT(text), HTML_CHANGE_ALL);

	if (engine != NULL) {
		html_object_relayout(HTML_OBJECT(text)->parent, engine, HTML_OBJECT(text));
		html_engine_queue_draw(engine, HTML_OBJECT(text));
	}
}

static void set_color(HTMLText *text, HTMLEngine *engine, const GdkColor *color) {

	if (gdk_color_equal(&text->color, color))
		return;

	text->color = *color;
	text->color_allocated = FALSE;

	if (engine != NULL) {
		html_object_relayout(HTML_OBJECT(text)->parent, engine, HTML_OBJECT(text));
		html_engine_queue_draw(engine, HTML_OBJECT(text));
	}
}

static void
destroy (HTMLObject *obj)
{
	g_free (HTML_TEXT (obj)->text);
	HTML_OBJECT_CLASS (parent_class)->destroy (obj);
}


void
html_text_type_init (void)
{
	html_text_class_init (&html_text_class, HTML_TYPE_TEXT, sizeof (HTMLText));
}

void
html_text_class_init (HTMLTextClass *klass,
		      HTMLType type,
		      guint object_size)
{
	HTMLObjectClass *object_class;

	object_class = HTML_OBJECT_CLASS (klass);

	html_object_class_init (object_class, type, object_size);

	object_class->destroy = destroy;
	object_class->reset = reset;
	object_class->copy = copy;
	object_class->draw = draw;
	object_class->accepts_cursor = accepts_cursor;
	object_class->calc_size = calc_size;
	object_class->get_cursor = get_cursor;
	object_class->get_cursor_base = get_cursor_base;
	object_class->save = save;
	object_class->save_plain = save_plain;

	/* HTMLText methods.  */

	klass->queue_draw = queue_draw;
	klass->extract_text = extract_text;
	klass->insert_text = insert_text;
	klass->remove_text = remove_text;
	klass->split = split;
	klass->get_font_style = get_font_style;
	klass->get_font_face = get_font_face;
	klass->get_color = get_color;
	klass->set_font_style = set_font_style;
	klass->set_font_face = set_font_face;
	klass->set_color = set_color;
	klass->merge = merge;
	klass->check_merge = check_merge;

	parent_class = &html_object_class;
}

void html_text_init(HTMLText *text_object, HTMLTextClass *klass, const gchar *text, gint len, CscHTMLFontStyle font_style, const GdkColor *color, const gchar *font_face) {
	HTMLObject *object;

	object = HTML_OBJECT(text_object);

	html_object_init(object, HTML_OBJECT_CLASS(klass));

	if (len == -1) {
		text_object->text_len = strlen(text);
		text_object->text = g_strdup(text);
	} else {
		text_object->text_len = len;
		text_object->text = g_strndup(text, len);
	}

	/* do <space>&nbsp;...&nbsp; hack */
	convert_nbsp(text_object->text, text_object->text_len);

	text_object->font_style = font_style;
	text_object->font_face = font_face;

	if (color != NULL) {
		text_object->color = *color;
	} else {
		text_object->color.red = 0;
		text_object->color.green = 0;
		text_object->color.blue = 0;
	}

	text_object->color_allocated = FALSE;
}

HTMLObject *html_text_new_with_len(const gchar *text, gint len, CscHTMLFontStyle font, const GdkColor *color, const gchar *font_face) {
	HTMLText *text_object;

	text_object = g_new(HTMLText, 1);

	html_text_init(text_object, &html_text_class, text, len, font, color, font_face);

	return HTML_OBJECT(text_object);
}

HTMLObject *html_text_new(const gchar *text, CscHTMLFontStyle font, const GdkColor *color, const gchar *font_face) {
	
	return html_text_new_with_len(text, -1, font, color, font_face);
}

guint
html_text_insert_text (HTMLText *text,
		       HTMLEngine *engine,
		       guint offset,
		       const gchar *p,
		       guint len)
{
	g_return_val_if_fail (text != NULL, 0);
	g_return_val_if_fail (engine != NULL, 0);
	g_return_val_if_fail (p != NULL, 0);

	if (len == 0)
		return 0;

	return (* HT_CLASS (text)->insert_text) (text, engine, offset, p, len);
}

guint
html_text_remove_text (HTMLText *text,
		       HTMLEngine *engine,
		       guint offset,
		       guint len)
{
	g_return_val_if_fail (text != NULL, 0);
	g_return_val_if_fail (engine != NULL, 0);

	return (* HT_CLASS (text)->remove_text) (text, engine, offset, len);
}

void
html_text_queue_draw (HTMLText *text,
		      HTMLEngine *engine,
		      guint offset,
		      guint len)
{
	g_return_if_fail (text != NULL);
	g_return_if_fail (engine != NULL);

	(* HT_CLASS (text)->queue_draw) (text, engine, offset, len);
}

HTMLText *
html_text_extract_text (HTMLText *text,
			guint offset,
			gint len)
{
	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (offset <= text->text_len, NULL);

	return (* HT_CLASS (text)->extract_text) (text, offset, len);
}

HTMLText *
html_text_split (HTMLText *text,
		 guint offset)
{
	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (offset > 0, NULL);

	return (* HT_CLASS (text)->split) (text, offset);
}

void
html_text_merge (HTMLText *text,
		 HTMLText *other,
		 gboolean prepend)
{
	g_return_if_fail (text != NULL);
	g_return_if_fail (other != NULL);
	g_return_if_fail (HTML_OBJECT_TYPE (text) == HTML_OBJECT_TYPE (other));

	return (* HT_CLASS (text)->merge) (text, other, prepend);
}

gboolean
html_text_check_merge (HTMLText *self,
		       HTMLText *text)
{
	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (text != NULL, FALSE);

	return (* HT_CLASS (text)->check_merge) (self, text);
}

CscHTMLFontStyle html_text_get_font_style(const HTMLText *text) {
	
	g_return_val_if_fail(text != NULL, CSC_HTML_FONT_STYLE_DEFAULT);

	return (* HT_CLASS(text)->get_font_style) (text);
}

const gchar *html_text_get_font_face(const HTMLText *text) {

	g_return_val_if_fail(text != NULL, NULL);

	return (* HT_CLASS(text)->get_font_face) (text);
}

const GdkColor *html_text_get_color(HTMLText *text, HTMLPainter *painter) {

	g_return_val_if_fail (text != NULL, NULL);
	g_return_val_if_fail (painter != NULL, NULL);

	return (* HT_CLASS (text)->get_color) (text, painter);
}

void html_text_set_font_style(HTMLText *text, HTMLEngine *engine, CscHTMLFontStyle style) {
	
	g_return_if_fail(text != NULL);

	return (* HT_CLASS(text)->set_font_style) (text, engine, style);
}

void html_text_set_font_face(HTMLText *text, HTMLEngine *engine, const gchar *font_face) {

	g_return_if_fail(text != NULL);

	return (* HT_CLASS(text)->set_font_face) (text, engine, font_face);
}

void html_text_set_color(HTMLText *text, HTMLEngine *engine, const GdkColor *color) {
	
	g_return_if_fail(text != NULL);
	g_return_if_fail(color != NULL);

	return (* HT_CLASS(text)->set_color) (text, engine, color);
}

void html_text_set_text(HTMLText *text, const gchar *new_text) {
	
	g_free(text->text);
	text->text = g_strdup(new_text);
	html_object_change_set(HTML_OBJECT(text), HTML_CHANGE_ALL);
}
