/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1997 Martin Jones (mjones@kde.org)
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
#ifndef HTMLEMBEDDED_H
#define HTMLEMBEDDED_H

#include "htmlobject.h"
#include "cschtml-embedded.h"
#include "htmlengine.h"

#define HTML_EMBEDDED(x) ((HTMLEmbedded *)(x))
#define HTML_EMBEDDED_CLASS(x) ((HTMLEmbeddedClass *)(x))

typedef struct _HTMLEmbedded HTMLEmbedded;
typedef struct _HTMLEmbeddedClass HTMLEmbeddedClass;

struct _HTMLEmbedded {
	HTMLObject object;
	
	gchar *name;
	gchar *value;
	HTMLForm *form;
	GtkWidget *widget, *parent;
	gint16 width, height;
	gboolean allocated;

	gint abs_x, abs_y;
};

struct _HTMLEmbeddedClass {
	HTMLObjectClass object_class;

	void (*reset) (HTMLEmbedded *element);
	gchar *(*encode) (HTMLEmbedded *element);
};


extern HTMLEmbeddedClass html_embedded_class;


void          html_embedded_type_init      (void);
void          html_embedded_class_init     (HTMLEmbeddedClass *klass,
					    HTMLType           type,
					    guint              object_size);
void          html_embedded_init           (HTMLEmbedded      *element,
					    HTMLEmbeddedClass *klass,
					    GtkWidget         *parent,
					    gchar             *name,
					    gchar             *value);
void          html_embedded_set_widget     (HTMLEmbedded      *element,
					    GtkWidget         *widget);
gchar        *html_embedded_get_name       (HTMLEmbedded      *element);
void          html_embedded_set_form       (HTMLEmbedded      *element,
					    HTMLForm          *form);
void          html_embedded_reset          (HTMLEmbedded      *element);
gchar        *html_embedded_encode         (HTMLEmbedded      *element);
gchar        *html_embedded_encode_string  (gchar             *str);
HTMLEmbedded *html_embedded_new_widget     (GtkWidget         *parent,
					    CscHTMLEmbedded   *eb);
void          html_embedded_size_recalc    (HTMLEmbedded      *em);

#endif /* HTMLEMBEDDED_H */
