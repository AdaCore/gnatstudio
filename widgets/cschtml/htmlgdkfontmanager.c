/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

   Copyright (C) 2000 Helix Code, Inc.
   
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

#include "htmlgdkfontmanager.h"
#include <string.h>

/* FIXME this should be dynamically done, and based on the base font name.  */

static guint real_font_sizes[CSC_HTML_FONT_STYLE_SIZE_MAX] = {
	8, 8, 10, 12, 14, 18, 19
};

static gchar *load_font(HTMLGdkFontManager *manager, CscHTMLFontStyle style, const gchar *face) {
	const gchar *weight_string;
	const gchar *slant_string;
	gchar *family_string = NULL;
	gchar *font_name = NULL;
	gint size;
	gint real_size;
	GdkFont *font;
	gchar *key;
	gint i;
	gchar **font_names;

	if (manager->font_hash == NULL)
		return;


	size = style & CSC_HTML_FONT_STYLE_SIZE_MASK;
	if (size == 0)
		size = 3;

	real_size = real_font_sizes[size];

	if (style & CSC_HTML_FONT_STYLE_BOLD)
		weight_string = "bold";
	else
		weight_string = "medium";

	if (style & CSC_HTML_FONT_STYLE_ITALIC)
		slant_string = "i";
	else
		slant_string = "r";

	font_names = g_strsplit(face, ",", 10);
	
	for (i = 0; font_names[i]; i++) {
		if (family_string != NULL) { g_free(family_string); }
		if (font_name != NULL) { g_free(font_name); }
		if (style & CSC_HTML_FONT_STYLE_FIXED)
			family_string = g_strdup("courier");
		else
			family_string = g_strdup(font_names[i]);

		font_name = g_strdup_printf("-*-%s-%s-%s-normal-*-%d-*-*-*-*-*-*-*", family_string, weight_string, slant_string, real_size);
		font = gdk_font_load(font_name);
		if (font != NULL) {
			break;
		}
	}
		
	if (font == NULL){
		g_warning("font `%s' not found", font_name);
		g_free(font_name);
		g_free(family_string);
		family_string = g_strdup("helvetica");
		font_name = g_strdup_printf("-*-helvetica-medium-r-normal-*-%d-*-*-*-*-*-*-*", real_size);
		font = gdk_font_load(font_name);
	}

	g_free(font_name);
	key = g_strdup_printf("%s%d", family_string, style);
	g_free(family_string);
	g_strfreev(font_names);
	
	g_hash_table_insert((GHashTable *)manager->font_hash, g_strdup(key), font);
	return key;
}

HTMLGdkFontManager *html_gdk_font_manager_new(void) {
	HTMLGdkFontManager *new;

	new = g_new(HTMLGdkFontManager, 1);
	new->font_hash = g_hash_table_new(g_str_hash, g_str_equal);

	return new;
}

void unref_fonts(gpointer key, GdkFont *font, gpointer user_data) {

	gdk_font_unref((GdkFont *)font);
	g_free(key);
}

void html_gdk_font_manager_destroy(HTMLGdkFontManager *manager) {
	
	g_return_if_fail(manager != NULL);

	g_hash_table_foreach((GHashTable *)manager->font_hash, (GHFunc) unref_fonts, NULL);
	g_hash_table_destroy(manager->font_hash);
	g_free(manager);
}

GdkFont *html_gdk_font_manager_get_font(HTMLGdkFontManager *manager, CscHTMLFontStyle style, const gchar *face) {
	GdkFont *font;
	gchar *key;
	
	g_return_val_if_fail(manager != NULL, NULL);
	g_return_val_if_fail(style < CSC_HTML_FONT_STYLE_MAX, NULL);

	if (face == NULL)
		face = g_strdup("lucida");

	key = load_font(manager, style, face);
	font = (GdkFont *)g_hash_table_lookup((GHashTable *)manager->font_hash, key);
	g_free(key);
	return gdk_font_ref((GdkFont *)font);
}
