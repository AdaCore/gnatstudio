/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries

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
   kBoston, MA 02111-1307, USA.
*/

#include "htmltext.h"
#include "htmllinktext.h"
#include <string.h>

HTMLLinkTextClass html_link_text_class;

static HTMLTextClass *parent_class = NULL;

/* HTMLObject methods.  */

static void
destroy (HTMLObject *object)
{
	HTMLLinkText *link_text;

	link_text = HTML_LINK_TEXT (object);
	g_free (link_text->url);
	g_free (link_text->target);

	(* HTML_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_LINK_TEXT (dest)->url = g_strdup (HTML_LINK_TEXT (self)->url);
	HTML_LINK_TEXT (dest)->target = g_strdup (HTML_LINK_TEXT (self)->target);
}

static const gchar *
get_url (HTMLObject *object)
{
	return HTML_LINK_TEXT (object)->url;
}

static const gchar *
get_target (HTMLObject *object)
{
	return HTML_LINK_TEXT (object)->target;
}

static HTMLText *split(HTMLText *self, guint offset) {
	HTMLText *new;
	HTMLLinkText *link_text;

	link_text = HTML_LINK_TEXT(self);

	if (offset >= strlen(self->text) || offset == 0)
		return NULL;

	new = HTML_TEXT(html_link_text_new(self->text + offset,
					     self->font_style,
					     &self->color,
					     self->font_face,
					     link_text->url,
					     link_text->target));

	self->text = g_realloc(self->text, offset + 1);
	self->text_len = offset;
	self->text[offset] = '\0';

	return new;
}

static CscHTMLFontStyle get_font_style(const HTMLText *text) {
	CscHTMLFontStyle font_style;

	font_style = parent_class->get_font_style(text);
	font_style = csc_html_font_style_merge(font_style, CSC_HTML_FONT_STYLE_UNDERLINE);

	return font_style;
}

static const gchar *get_font_face(const HTMLText *text) {
	gchar *font_face;

	font_face = parent_class->get_font_face((HTMLText *)text);

	return font_face;
}

static gboolean
save (HTMLObject *self,
      HTMLEngineSaveState *state)
{
	if (! html_engine_save_output_string (state, "<A HREF=\"")
	    || ! html_engine_save_output_string (state, HTML_LINK_TEXT (self)->url)
	    || ! html_engine_save_output_string (state, "\">"))
		return FALSE;

	if (! HTML_OBJECT_CLASS (&html_text_class)->save (self, state))
		return FALSE;

	if (! html_engine_save_output_string (state, "</A>"))
		return FALSE;

	return TRUE;
}


void html_link_text_type_init (void) {
	
	html_link_text_class_init(&html_link_text_class, HTML_TYPE_LINKTEXT, sizeof(HTMLLinkText));
}

void html_link_text_class_init(HTMLLinkTextClass *klass, HTMLType type, guint size) {
	HTMLObjectClass *object_class;
	HTMLTextClass *text_class;

	object_class = HTML_OBJECT_CLASS (klass);
	text_class = HTML_TEXT_CLASS (klass);

	html_text_class_init (text_class, type, size);

	object_class->destroy = destroy;
	object_class->copy = copy;
	object_class->get_url = get_url;
	object_class->get_target = get_target;
	object_class->save = save;

	text_class->split = split;
	text_class->get_font_style = get_font_style;
	text_class->get_font_face = get_font_face;
	
	parent_class = &html_text_class;
}

void html_link_text_init(HTMLLinkText *link_text_object,
		     HTMLLinkTextClass *klass,
		     const gchar *text,
		     CscHTMLFontStyle font_style,
		     const GdkColor *color,
		     const gchar *font_face,
		     const gchar *url,
		     const gchar *target)
{
	HTMLText *text_object;

	text_object = HTML_TEXT (link_text_object);
	html_text_init(text_object, HTML_TEXT_CLASS (klass), text, -1, font_style, color, font_face);

	link_text_object->url = g_strdup(url);
	link_text_object->target = g_strdup(target);
}

HTMLObject *html_link_text_new (const gchar *text,
		    CscHTMLFontStyle font_style,
		    const GdkColor *color,
		    const gchar *font_face,
		    const gchar *url,
		    const gchar *target)
{
	HTMLLinkText *link_text_object;

	link_text_object = g_new(HTMLLinkText, 1);

	html_link_text_init(link_text_object, &html_link_text_class, text, font_style, color, font_face, url, target);

	return HTML_OBJECT(link_text_object);
}
