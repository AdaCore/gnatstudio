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
#ifndef _HTMLTEXTINPUT_H_
#define _HTMLTEXTINPUT_H_

#include "htmlembedded.h"

#define HTML_TEXTINPUT(x) ((HTMLTextInput *) (x))
#define HTML_TEXTINPUT_CLASS(x) ((HTMLTextInputClass *) (x))

typedef struct _HTMLTextInput HTMLTextInput;
typedef struct _HTMLTextInputClass HTMLTextInputClass;

struct _HTMLTextInput {
	HTMLEmbedded element;
	gint size, maxlen;
	gboolean password;

	gchar *default_text;
};

struct _HTMLTextInputClass {
	HTMLEmbeddedClass element_class;
};


extern HTMLTextInputClass html_text_input_class;


void        html_text_input_type_init   (void);
void        html_text_input_class_init  (HTMLTextInputClass *klass,
					 HTMLType            type,
					 guint               object_size);
void        html_text_input_init        (HTMLTextInput      *ti,
					 HTMLTextInputClass *klass,
					 GtkWidget          *parent,
					 gchar              *name,
					 gchar              *value,
					 gint                size,
					 gint                maxlen,
					 gboolean            password);
HTMLObject *html_text_input_new         (GtkWidget          *parent,
					 gchar              *name,
					 gchar              *value,
					 gint                size,
					 gint                maxlen,
					 gboolean            password);

#endif /* _HTMLTEXTINPUT_H_ */
