/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 3 -*- */
/* This file is part of the CscHTML library.

   Copyright (C) 2000 Helix Code, Inc.
   Copyright (C) 2002 ACT-Europe

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
#include <pango/pango-font.h>

static guint real_font_sizes[CSC_HTML_FONT_STYLE_SIZE_MAX] = {
   6, 7, 8, 10, 12, 14, 16
};

/* Font size adjustment for the real_font_sizes array above, if any */
int _gdk_font_adjust = 0;

static void unref_fonts (gpointer key, GdkFont *font, gpointer user_data);

/*****************************
 ** html_gdk_font_manager_new
 *****************************/

HTMLGdkFontManager *
html_gdk_font_manager_new (void)
{
  HTMLGdkFontManager * new = g_new (HTMLGdkFontManager, 1);
  new->font_hash = g_hash_table_new(g_str_hash, g_str_equal);
  new->saved_font_adjust = _gdk_font_adjust;
  return new;
}

/***************
 ** unref_fonts
 ***************/

static void
unref_fonts (gpointer key, GdkFont *font, gpointer user_data)
{
  gdk_font_unref (font);
  g_free (key);
}

/*********************************
 ** html_gdk_font_manager_destroy
 *********************************/

void
html_gdk_font_manager_destroy (HTMLGdkFontManager *manager)
{
  g_return_if_fail (manager != NULL);
  g_hash_table_foreach (manager->font_hash, (GHFunc) unref_fonts, NULL);
  g_hash_table_destroy (manager->font_hash);
  g_free (manager);
}

/**********************************
 ** html_gdk_font_manager_get_font
 **********************************/

GdkFont *
html_gdk_font_manager_get_font
  (HTMLGdkFontManager *manager,
   CscHTMLFontStyle style,
   const gchar *face)
{
  gint   size;
  gchar* weight_string;
  gchar* slant_string;
  gchar **font_names;
  gchar* font_name;
  PangoFontDescription* descr;
  GdkFont *font = NULL;
  gint i;
  gchar  key [256];

  g_return_val_if_fail (manager != NULL, NULL);
  g_return_val_if_fail (style < CSC_HTML_FONT_STYLE_MAX, NULL);

  /* In case the font adjustment has changed */
  if (manager->saved_font_adjust != _gdk_font_adjust) {
     g_hash_table_foreach (manager->font_hash, (GHFunc) unref_fonts, NULL);
     g_hash_table_destroy (manager->font_hash);
     manager->font_hash = g_hash_table_new(g_str_hash, g_str_equal);
     manager->saved_font_adjust = _gdk_font_adjust;
  }

  if (face) {
    sprintf (key, "%s%d\n", face, style);
  } else {
    sprintf (key, "default%d\n", style);
  }
  font = (GdkFont *)g_hash_table_lookup((GHashTable *)manager->font_hash, key);
  if (font != NULL) {
     return gdk_font_ref (font);
  }

  /* Compute the most appropriate size for the font */

  size = style & CSC_HTML_FONT_STYLE_SIZE_MASK;
  if (size == 0)
    size = 3;

  size = real_font_sizes [size] + _gdk_font_adjust;

  /* Compute the weight of the font */

  if (style & CSC_HTML_FONT_STYLE_BOLD)
    weight_string = "bold ";
  else
    weight_string = "medium ";

   /* Compute the slant of the font */

  if (style & CSC_HTML_FONT_STYLE_ITALIC)
    slant_string = "italic ";
  else
    slant_string = "";

  /* Special handling for fixed fonts: make sure we use a valid one */

  if (style & CSC_HTML_FONT_STYLE_FIXED)
    font_names = g_strsplit ("courier new,courier", ",", 10);
  else if (face)
    font_names = g_strsplit (face, ",", 10);
  else {
    /* g_warning ("null font passed to html_gdk_font_manager_get_font"); */
    font_names = g_strsplit ("sans", ",", 10);
  }

  /* Find the first font that matches style */

  for (i = 0; (font == NULL) && (font_names[i]); i++) {
     font_name = g_strdup_printf
       ("%s %s%s%d", font_names [i], weight_string, slant_string, size);
     descr = pango_font_description_from_string (font_name);
     font = gdk_font_from_description (descr);
     pango_font_description_free (descr);
     g_free (font_name);
  }

  /* Fallback on a default font, that should exist on all systems */

  if (font == NULL) {
    g_warning ("font `%s' not found", face);

    descr = pango_font_description_from_string ("sans 10");
    font = gdk_font_from_description (descr);
    pango_font_description_free (descr);
  }

  g_strfreev (font_names);

  g_hash_table_insert
    ((GHashTable *)manager->font_hash, g_strdup(key), font);

  return gdk_font_ref (font);
}
