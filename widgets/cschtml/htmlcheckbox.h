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
#ifndef _HTMLCHECKBOX_H_
#define _HTMLCHECKBOX_H_

#include "htmlembedded.h"

#define HTML_CHECKBOX(x) ((HTMLCheckBox *) (x))
#define HTML_CHECKBOX_CLASS(x) ((HTMLCheckBoxClass *) (x))

typedef struct _HTMLCheckBox HTMLCheckBox;
typedef struct _HTMLCheckBoxClass HTMLCheckBoxClass;

struct _HTMLCheckBox {
	HTMLEmbedded element;
	gint default_checked;
};

struct _HTMLCheckBoxClass {
	HTMLEmbeddedClass element_class;
};


extern HTMLCheckBoxClass html_checkbox_class;


void        html_checkbox_type_init   (void);
void        html_checkbox_class_init  (HTMLCheckBoxClass *klass,
				       HTMLType           type,
				       guint              object_size);
void        html_checkbox_init        (HTMLCheckBox      *checkbox,
				       HTMLCheckBoxClass *klass,
				       GtkWidget         *parent,
				       gchar             *name,
				       gchar             *value,
				       gboolean           checked);
HTMLObject *html_checkbox_new         (GtkWidget         *parent,
				       gchar             *name,
				       gchar             *value,
				       gboolean           checked);

#endif /* _HTMLCHECKBOX_H_ */
