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

#ifndef _HTMLGDKFONTMANAGER_H_
#define _HTMLGDKFONTMANAGER_H_

#include <gdk/gdk.h>

#include "cschtmlfontstyle.h"

struct _HTMLGdkFontManager {
	GHashTable	*font_hash;
};
typedef struct _HTMLGdkFontManager HTMLGdkFontManager;

HTMLGdkFontManager *html_gdk_font_manager_new  (void);
void html_gdk_font_manager_destroy (HTMLGdkFontManager *manager);

/* Return a newly allocated font matching style.
   FACE might contain several font names, separates by commas. This function
   will return the first font that can be allocated with style */
GdkFont *html_gdk_font_manager_get_font
   (HTMLGdkFontManager *manager, CscHTMLFontStyle style, const gchar *face);


#endif /* _HTMLGDKFONTMANAGER_H_ */
