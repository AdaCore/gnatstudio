/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1997 Martin Jones (mjones@kde.org)
              (C) 1997 Torben Weis (weis@kde.org)
	      (C) 1999 Ettore Perazzoli (ettore@gnu.org)

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

#ifndef _HTMLLINKTEXT_H_
#define _HTMLLINKTEXT_H_

#include "htmlobject.h"
#include "htmltext.h"

typedef struct _HTMLLinkText HTMLLinkText;
typedef struct _HTMLLinkTextClass HTMLLinkTextClass;

#define HTML_LINK_TEXT(x) ((HTMLLinkText *)(x))
#define HTML_LINK_TEXT_CLASS(x) ((HTMLLinkTextClass *)(x))

struct _HTMLLinkText {
	HTMLText text;

	gchar *url;
	gchar *target;
};

struct _HTMLLinkTextClass {
	HTMLTextClass text_class;
};

extern HTMLLinkTextClass html_link_text_class;

void        html_link_text_type_init   (void);
void        html_link_text_class_init  (HTMLLinkTextClass *klass,
					HTMLType           type,
					guint              object_size);
void        html_link_text_init        (HTMLLinkText      *link_text_object,
					HTMLLinkTextClass *klass,
					const gchar       *text,
					CscHTMLFontStyle   font_style,
					const GdkColor    *color,
					const gchar       *font_face,
					const gchar       *url,
					const gchar       *target);
HTMLObject *html_link_text_new         (const gchar       *text,
					CscHTMLFontStyle   font_style,
					const GdkColor    *color,
					const gchar       *font_face,
					const gchar       *url,
					const gchar       *target);

#endif /* _HTMLLINKTEXT_H_ */
