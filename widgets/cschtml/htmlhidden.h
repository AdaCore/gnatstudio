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
#ifndef _HTMLHIDDEN_H_
#define _HTMLHIDDEN_H_

#include "htmlembedded.h"

#define HTML_HIDDEN(x) ((HTMLHidden *) (x))
#define HTML_HIDDEN_CLASS(x) ((HTMLHiddenClass *) (x))

typedef struct _HTMLHidden HTMLHidden;
typedef struct _HTMLHiddenClass HTMLHiddenClass;

struct _HTMLHidden {
	HTMLEmbedded element;
};

struct _HTMLHiddenClass {
	HTMLEmbeddedClass element_class;
};


extern HTMLHiddenClass html_hidden_class;


void        html_hidden_type_init   (void);
void        html_hidden_class_init  (HTMLHiddenClass *klass,
				     HTMLType         type,
				     guint            object_size);
void        html_hidden_init        (HTMLHidden      *hidden,
				     HTMLHiddenClass *klass,
				     gchar           *name,
				     gchar           *value);
HTMLObject *html_hidden_new         (gchar           *name,
				     gchar           *value);

#endif /* _HTMLHIDDEN_H_ */
