/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
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

#ifndef _HTMLSETTINGS_H_
#define _HTMLSETTINGS_H_

#include <gdk/gdk.h>

typedef struct _HTMLSettings HTMLSettings;
typedef enum   _HTMLSettingsColor HTMLSettingsColor;

#include "htmlengine.h"
#include "htmlpainter.h"


#define HTML_NUM_FONT_SIZES 7

enum _HTMLSettingsColor {
	HTMLBgColor = 0,
	HTMLTextColor,
	HTMLLinkColor,
	HTMLVLinkColor,
	HTMLALinkColor,

	HTMLColors,
};

struct _HTMLSettings {
	gint fontSizes [HTML_NUM_FONT_SIZES];
	gint fontBaseSize;

	gchar *fontBaseFace;
	gchar *fixedFontFace;

	/* colors */
	GdkColor color [HTMLColors];
	gboolean color_allocated [HTMLColors];
	GSList  *colors_to_free;

	guint underlineLinks : 1;
	guint forceDefault : 1;
};


HTMLSettings *   html_settings_new                   (void);
void             html_settings_reset                 (HTMLSettings *settings,
						      HTMLSettings *orig,
						      HTMLPainter  *painter);
void          	 html_settings_destroy               (HTMLSettings *settings);
void   	         html_settings_set_font_sizes        (HTMLSettings *settings,
						      const gint *newFontSizes);
void 	      	 html_settings_get_font_sizes        (HTMLSettings *settings,
						      gint *fontSizes);
void 	      	 html_settings_reset_font_sizes      (HTMLSettings *settings);
void 	      	 html_settings_copy                  (HTMLSettings *dest, HTMLSettings *src);
void 	      	 html_settings_set_font_base_face    (HTMLSettings *settings, const gchar *face);
void 	      	 html_settings_set_fixed_font_face   (HTMLSettings *settings, const gchar *face);

/* colors handling */
void             html_settings_set_color             (HTMLSettings *settings,
						      HTMLSettingsColor idx,
						      GdkColor *color);
const GdkColor * html_settings_get_color             (HTMLSettings *settings,
						      HTMLSettingsColor idx);
const GdkColor * html_settings_get_color_allocated   (HTMLSettings *settings,
						      HTMLSettingsColor idx,
						      HTMLPainter *painter);
void             html_settings_free_colors           (HTMLSettings *s,
						      HTMLPainter *painter,
						      gboolean all);

#endif /* _HTMLSETTINGS_H_ */
