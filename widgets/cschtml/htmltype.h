/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* htmltype.h

   Copyright (C) 1999 Helix Code, Inc.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Ettore Perazzoli <ettore@comm2000.it>
*/

#ifndef _HTMLTYPE_H
#define _HTMLTYPE_H

#include <glib.h>


/* This is a list of the HTML object classes we are using.  We use a static
   system because (a) it's faster and (b) we don't need/want an extensible
   object system.

   If you add new HTML classes, you should (a) update this list and (b) update
   the `html_types_init()' function in `htmltype.c'.  */

enum _HTMLType {
	HTML_TYPE_NONE,
	HTML_TYPE_ANCHOR,
	HTML_TYPE_BULLET,
	HTML_TYPE_BUTTON,
	HTML_TYPE_CHECKBOX,
	HTML_TYPE_CLUE,
	HTML_TYPE_CLUEALIGNED,
	HTML_TYPE_CLUEFLOW,
	HTML_TYPE_CLUEH,
	HTML_TYPE_CLUEV,
	HTML_TYPE_EMBEDDED,
	HTML_TYPE_HIDDEN,
	HTML_TYPE_HSPACE,
	HTML_TYPE_IMAGE,
	HTML_TYPE_IMAGEINPUT,
	HTML_TYPE_LINKTEXT,
	HTML_TYPE_LINKTEXTMASTER,
	HTML_TYPE_OBJECT,
	HTML_TYPE_RADIO,
	HTML_TYPE_RULE,
	HTML_TYPE_SELECT,
	HTML_TYPE_TABLE,
	HTML_TYPE_TABLECELL,
	HTML_TYPE_TEXT,
	HTML_TYPE_TEXTAREA,
	HTML_TYPE_TEXTINPUT,
	HTML_TYPE_TEXTMASTER,
	HTML_TYPE_TEXTSLAVE,
	HTML_TYPE_VSPACE,
	HTML_TYPE_IFRAME,
	HTML_NUM_TYPES
};
typedef enum _HTMLType HTMLType;


void html_types_init (void);
const gchar *html_type_name (HTMLType type);

#endif
