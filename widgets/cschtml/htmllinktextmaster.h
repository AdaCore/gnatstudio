/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1997 Martin Jones (mjones@kde.org)
              (C) 1997 Torben Weis (weis@kde.org)
	      (C) 1999, 2000 Helix Code, Inc.

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

#ifndef _HTMLLINKTEXTMASTER_H_
#define _HTMLLINKTEXTMASTER_H_

#include "htmlobject.h"
#include "htmltextmaster.h"

typedef struct _HTMLLinkTextMaster HTMLLinkTextMaster;
typedef struct _HTMLLinkTextMasterClass HTMLLinkTextMasterClass;

#define HTML_LINK_TEXT_MASTER(x) ((HTMLLinkTextMaster *)(x))
#define HTML_LINK_TEXT_MASTER_CLASS(x) ((HTMLLinkTextMasterClass *)(x))

struct _HTMLLinkTextMaster {
	HTMLTextMaster text_master;

	gchar *url;
	gchar *target;
};

struct _HTMLLinkTextMasterClass {
	HTMLTextMasterClass text_master_class;
};

extern HTMLLinkTextMasterClass html_link_text_master_class;

void        html_link_text_master_type_init     (void);
void        html_link_text_master_class_init    (HTMLLinkTextMasterClass *klass,
						 HTMLType                 type,
						 guint                    object_size);
void        html_link_text_master_init          (HTMLLinkTextMaster      *link_text_master,
						 HTMLLinkTextMasterClass *klass,
						 const gchar             *text,
						 gint			  len,
						 CscHTMLFontStyle         font_style,
						 const GdkColor          *color,
						 const gchar             *font_face,
						 const gchar             *url,
						 const gchar             *target);
HTMLObject *html_link_text_master_new           (const gchar             *text,
						 CscHTMLFontStyle         font_style,
						 const GdkColor          *color,
						 const gchar             *font_face,
						 const gchar             *url,
						 const gchar             *target);
HTMLObject *html_link_text_master_new_with_len  (const gchar             *text,
						 gint                     len,
						 CscHTMLFontStyle         font_style,
						 const GdkColor          *color,
						 const gchar             *font_face,
						 const gchar             *url,
						 const gchar             *target);

void        html_link_text_master_set_url       (HTMLLinkTextMaster      *link,
						 const gchar             *url);

#endif /* _HTMLLINKTEXTMASTER_H_ */
