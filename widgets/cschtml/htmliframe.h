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

#ifndef _HTMLIFRAME_H_
#define _HTMLIFRAME_H_
#include "htmlobject.h"
#include "htmlembedded.h"

#define HTML_IFRAME(x) ((HTMLIFrame *)(x))
#define HTML_IFRAME_CLASS(x) ((HTMLIFrameClass *)(x))

typedef struct _HTMLIFrame HTMLIFrame;
typedef struct _HTMLIFrameClass HTMLIFrameClass;

struct _HTMLIFrame {
	HTMLEmbedded embedded;
	
	char *url;
	GtkWidget *scroll;
	GtkWidget *html;
	gint width;
	gint height;
	gboolean frameborder;
};

struct _HTMLIFrameClass {
	HTMLEmbeddedClass embedded_class;
};

void           html_iframe_type_init             (void);

void           html_iframe_class_init            (HTMLIFrameClass *klass,
						  HTMLType           type,
						  guint              object_size);

void           html_iframe_init                  (HTMLIFrame        *iframe,
						  HTMLIFrameClass   *klass,
						  GtkWidget         *parent,
						  char              *url,
						  gint               width,
						  gint               height,
						  gboolean           border);

HTMLObject *   html_iframe_new                   (GtkWidget *parent,
						  char *src,
						  gint width,
						  gint height,
						  gboolean border);

						  
						  
#endif


