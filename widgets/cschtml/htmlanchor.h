/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

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

#ifndef _HTMLANCHOR_H
#define _HTMLANCHOR_H

#include <glib.h>

#include "htmlobject.h"

typedef struct _HTMLAnchorClass HTMLAnchorClass;

#define HTML_ANCHOR(x) ((HTMLAnchor *) x)
#define HTML_ANCHOR_CLASS(x) ((HTMLAnchorClass *) x)

struct _HTMLAnchor {
	HTMLObject object;

	GString *name;
};

struct _HTMLAnchorClass {
	HTMLObjectClass object_class;
};


void         html_anchor_type_init   (void);
void         html_anchor_class_init  (HTMLAnchorClass *klass,
				      HTMLType         type,
				      guint            object_size);
HTMLObject  *html_anchor_new         (const gchar     *name);
void         html_anchor_init        (HTMLAnchor      *anchor,
				      HTMLAnchorClass *klass,
				      const gchar     *name);
const gchar *html_anchor_get_name    (HTMLAnchor      *anchor);

#endif /* _HTMLANCHOR_H */
