/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1999 Anders Carlsson (andersca@gnu.org)
              (C) 1997 Martin Jones (mjones@kde.org)
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

#include <string.h>
#include <gdk/gdk.h>
#include "htmlsettings.h"


static const int defaultFontSizes[HTML_NUM_FONT_SIZES] = { 8, 10, 12, 14, 18, 24, 32 };



#define SET_COLOR(w,r,g,b) \
        s->color [HTML ## w ## Color].red   = r; \
        s->color [HTML ## w ## Color].green = g; \
        s->color [HTML ## w ## Color].blue  = b

HTMLSettings *
html_settings_new (void)
{
	HTMLSettings *s = g_new0 (HTMLSettings, 1);
	
	s->fontBaseSize = 3;
	s->fontBaseFace = g_strdup ("times");
	s->fixedFontFace = g_strdup ("courier");

	SET_COLOR (Link,  0, 0, 0xffff);
	SET_COLOR (ALink, 0, 0, 0xffff);
	SET_COLOR (VLink, 0, 0, 0xffff);
	SET_COLOR (Bg,    0xffff, 0xffff, 0xffff);

	s->underlineLinks = TRUE;
	s->forceDefault = FALSE;

	html_settings_reset_font_sizes (s);

	return s;
}

void
html_settings_destroy (HTMLSettings *settings)
{
	g_return_if_fail (settings != NULL);

	g_free (settings->fontBaseFace);
	g_free (settings->fixedFontFace);

	g_free (settings);
}

void
html_settings_set_font_sizes (HTMLSettings *settings,
			      const gint *newFontSizes)
{
	guint i;

	for (i = 0; i < HTML_NUM_FONT_SIZES; i++)
		settings->fontSizes[i] = newFontSizes[i];
}

void
html_settings_get_font_sizes (HTMLSettings *settings,
			      gint *fontSizes)
{
	guint i;

	for (i = 0; i < HTML_NUM_FONT_SIZES; i++)
		fontSizes[i] = settings->fontSizes[i];
}

void
html_settings_reset_font_sizes (HTMLSettings *settings)
{
	html_settings_set_font_sizes (settings, defaultFontSizes);
}

void
html_settings_copy (HTMLSettings *dest,
		    HTMLSettings *src)
{
	g_free (dest->fontBaseFace);
	g_free (dest->fixedFontFace);

	memcpy (dest, src, sizeof (*dest));

	dest->fontBaseFace = g_strdup (src->fontBaseFace);
	dest->fixedFontFace = g_strdup (src->fixedFontFace);
}

void
html_settings_set_font_base_face (HTMLSettings *settings,
				  const gchar *face)
{
	g_free (settings->fontBaseFace);
	settings->fontBaseFace = g_strdup (face);
}

void
html_settings_free_colors (HTMLSettings *s, HTMLPainter *painter, gboolean all)
{
	GSList *cur = s->colors_to_free;
	GdkColor *c;

	/* free colors to free */
	if (cur) {
		while (cur) {
			c = (GdkColor *) cur->data;
			html_painter_free_color (painter, c);
			gdk_color_free (c);
			cur = cur->next;
		}
		g_slist_free (s->colors_to_free);
		s->colors_to_free = NULL;
	}

	/* all means that we free all allocated colors */
	if (all) {
		HTMLSettingsColor i = 0;

		for (; i < HTMLColors; i++) {
			if (s->color_allocated [i]) {
				html_painter_free_color (painter, &s->color [i]);
				s->color_allocated [i] = FALSE;
			}
		}
	}
}

void
html_settings_set_fixed_font_face (HTMLSettings *settings,
				   const gchar *face)
{
	g_free (settings->fixedFontFace);
	settings->fixedFontFace = g_strdup (face);
}

void
html_settings_set_color (HTMLSettings *s, HTMLSettingsColor idx, GdkColor *color)
{
	if (s->color_allocated [idx])
		s->colors_to_free = g_slist_prepend (s->colors_to_free, gdk_color_copy (&s->color [idx]));
	s->color [idx] = *color;
	s->color_allocated [idx] = FALSE;
}

const GdkColor *
html_settings_get_color (HTMLSettings *s, HTMLSettingsColor idx)
{
	return &s->color [idx];
}

const GdkColor *
html_settings_get_color_allocated (HTMLSettings *s, HTMLSettingsColor idx, HTMLPainter *painter)
{
	if (s->colors_to_free)
		html_settings_free_colors (s, painter, FALSE);

	if (!s->color_allocated [idx]) {
		html_painter_alloc_color (painter, &s->color [idx]);
		s->color_allocated [idx] = TRUE;
	}

	return &s->color [idx];
}

void
html_settings_reset (HTMLSettings *s, HTMLSettings *o, HTMLPainter *p)
{
	/* FIXME we do only colors now */

	HTMLSettingsColor i;

	html_settings_free_colors (s, p, TRUE);
	for (i=0; i < HTMLColors; i++)
		s->color [i] = o->color [i];
}
