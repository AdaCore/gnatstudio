/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library

    Copyright (C) 2000 Jonas Borgström <jonas_b@bitsmart.com>

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
#ifndef _HTMLSELECT_H_
#define _HTMLSELECT_H_

#include "htmlembedded.h"

#define HTML_SELECT(x) ((HTMLSelect *) (x))
#define HTML_SELECT_CLASS(x) ((HTMLSelectClass *) (x))

/* typedef struct _HTMLSelect HTMLSelect; In htmlobject.h for now */
typedef struct _HTMLSelectClass HTMLSelectClass;

struct _HTMLSelect {
	HTMLEmbedded element;
	gint size;
	gboolean multi;

	gint default_selected;

	GList *values;
	GList *strings;
	GList *default_selection;

	GtkWidget *clist;
};

struct _HTMLSelectClass {
	HTMLEmbeddedClass element_class;
};


extern HTMLSelectClass html_select_class;


void        html_select_type_init   (void);
void        html_select_class_init  (HTMLSelectClass *klass,
				     HTMLType         type,
				     guint            object_size);
void        html_select_init        (HTMLSelect      *ti,
				     HTMLSelectClass *klass,
				     GtkWidget       *parent,
				     gchar           *name,
				     gint             size,
				     gboolean         multi);
HTMLObject *html_select_new         (GtkWidget       *parent,
				     gchar           *name,
				     gint             size,
				     gboolean         multi);
void        html_select_add_option  (HTMLSelect      *select,
				     gchar           *value,
				     gboolean         selected);
void        html_select_set_text    (HTMLSelect      *select,
				     gchar           *text);

#endif /* _HTMLSELECT_H_ */
