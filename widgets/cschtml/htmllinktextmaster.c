/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries

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

#include "htmltext.h"
#include "htmllinktextmaster.h"

HTMLLinkTextMasterClass html_link_text_master_class;

static HTMLTextMasterClass *parent_class = NULL;

/* HTMLObject methods.  */

static void
destroy (HTMLObject *object)
{
	HTMLLinkTextMaster *link_text_master;

	link_text_master = HTML_LINK_TEXT_MASTER (object);
	g_free (link_text_master->url);
	g_free (link_text_master->target);

	(* HTML_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	HTML_LINK_TEXT_MASTER (dest)->url = g_strdup (HTML_LINK_TEXT_MASTER (self)->url);
	HTML_LINK_TEXT_MASTER (dest)->target = g_strdup (HTML_LINK_TEXT_MASTER (self)->target);
}

static const gchar *
get_url (HTMLObject *object)
{
	return HTML_LINK_TEXT_MASTER (object)->url;
}

static const gchar *
get_target (HTMLObject *object)
{
	return HTML_LINK_TEXT_MASTER (object)->target;
}

static HTMLText *
split (HTMLText *self,
       guint offset)
{
	HTMLText *new;

	new = (* HTML_TEXT_CLASS (parent_class)->split) (self, offset);
	if (new == NULL)
		return NULL;

	HTML_LINK_TEXT_MASTER (new)->url = g_strdup (HTML_LINK_TEXT_MASTER (self)->url);
	HTML_LINK_TEXT_MASTER (new)->target = g_strdup (HTML_LINK_TEXT_MASTER (self)->target);

	return new;
}

static CscHTMLFontStyle get_font_style(const HTMLText *text) {
	CscHTMLFontStyle font_style;

	font_style = HTML_TEXT_CLASS(parent_class)->get_font_style(text);
	font_style = csc_html_font_style_merge(font_style, CSC_HTML_FONT_STYLE_UNDERLINE);

	return font_style;
}

static const gchar *get_font_face(const HTMLText *text) {
	gchar *font_face;

	font_face = HTML_TEXT_CLASS(parent_class)->get_font_face(text);

	return font_face;
}

static gboolean
save (HTMLObject *self,
      HTMLEngineSaveState *state)
{
	if (! html_engine_save_output_string (state, "<A HREF=\"")
	    || ! html_engine_save_output_string (state, HTML_LINK_TEXT_MASTER (self)->url)
	    || ! html_engine_save_output_string (state, "\">"))
		return FALSE;

	if (! HTML_OBJECT_CLASS (&html_text_class)->save (self, state))
		return FALSE;

	if (! html_engine_save_output_string (state, "</A>"))
		return FALSE;

	return TRUE;
}

static HTMLObject *get_selection(HTMLObject *self, guint *size_return) {
	HTMLObject *new;
	guint select_start, select_length;

	if (! self->selected)
		return NULL;

	select_start = HTML_TEXT_MASTER (self)->select_start;
	select_length = HTML_TEXT_MASTER (self)->select_length;

	new = html_link_text_master_new_with_len(HTML_TEXT(self)->text + select_start,
						 select_length,
						 HTML_TEXT(self)->font_style,
						 & HTML_TEXT (self)->color,
						 HTML_TEXT(self)->font_face,
						 HTML_LINK_TEXT_MASTER(self)->url,
						 HTML_LINK_TEXT_MASTER(self)->target);

	if (size_return != NULL)
		*size_return = select_length;

	return new;
}


void
html_link_text_master_type_init (void)
{
	html_link_text_master_class_init (&html_link_text_master_class,
					  HTML_TYPE_LINKTEXTMASTER,
					  sizeof (HTMLLinkTextMaster));
}

void
html_link_text_master_class_init (HTMLLinkTextMasterClass *klass,
				  HTMLType type,
				  guint size)
{
	HTMLObjectClass *object_class;
	HTMLTextClass *text_class;
	HTMLTextMasterClass *text_master_class;

	object_class = HTML_OBJECT_CLASS (klass);
	text_class = HTML_TEXT_CLASS (klass);
	text_master_class = HTML_TEXT_MASTER_CLASS (klass);

	html_text_master_class_init(text_master_class, type, size);

	object_class->destroy = destroy;
	object_class->copy = copy;
	object_class->get_url = get_url;
	object_class->get_target = get_target;
	object_class->save = save;
	object_class->get_selection = get_selection;

	text_class->split = split;
	text_class->get_font_style = get_font_style;
	text_class->get_font_face = get_font_face;

	parent_class = &html_text_master_class;
}

void
html_link_text_master_init(HTMLLinkTextMaster *link_text_master_object, HTMLLinkTextMasterClass *klass, const gchar *text, gint len, CscHTMLFontStyle font_style, const GdkColor *color, const gchar *font_face, const gchar *url, const gchar *target) {
	HTMLTextMaster *text_master_object;

	text_master_object = HTML_TEXT_MASTER(link_text_master_object);

	html_text_master_init(text_master_object, HTML_TEXT_MASTER_CLASS(klass), text, len, font_style, color, font_face);

	link_text_master_object->url = g_strdup(url);
	link_text_master_object->target = g_strdup(target);
}

HTMLObject *html_link_text_master_new_with_len(const gchar *text, gint len, CscHTMLFontStyle font_style, const GdkColor *color, const gchar *font_face, const gchar *url, const gchar *target) {
	HTMLLinkTextMaster *link_text_master_object;

	g_return_val_if_fail(text != NULL, NULL);

	link_text_master_object = g_new(HTMLLinkTextMaster, 1);

	html_link_text_master_init(link_text_master_object, &html_link_text_master_class, text, len, font_style, color, font_face, url, target);

	return HTML_OBJECT(link_text_master_object);
}

HTMLObject *html_link_text_master_new(const gchar *text, CscHTMLFontStyle font_style, const GdkColor *color, const gchar *font_face, const gchar *url, const gchar *target) {
	
	return html_link_text_master_new_with_len(text, -1, font_style, color, font_face, url, target);
}

void html_link_text_master_set_url(HTMLLinkTextMaster *link, const gchar *url) {
	
	g_free(link->url);
	link->url = g_strdup(url);
}
