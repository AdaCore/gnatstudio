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
#ifndef _HTMLIMAGEINPUT_H_
#define _HTMLIMAGEINPUT_H_

#include "htmlimage.h"
#include "htmlembedded.h"

#define HTML_IMAGEINPUT(x) ((HTMLImageInput *) (x))
#define HTML_IMAGEINPUT_CLASS(x) ((HTMLImageInputClass *) (x))

typedef struct _HTMLImageInput HTMLImageInput;
typedef struct _HTMLImageInputClass HTMLImageInputClass;

struct _HTMLImageInput {
	HTMLEmbedded element;

        HTMLImage *image;
	gint m_x, m_y;
};

struct _HTMLImageInputClass {
	HTMLEmbeddedClass element_class;
};


extern HTMLImageInputClass html_imageinput_class;


void        html_imageinput_type_init   (void);
void        html_imageinput_class_init  (HTMLImageInputClass *klass,
					 HTMLType             type,
					 guint                object_size);
void        html_imageinput_init        (HTMLImageInput      *img,
					 HTMLImageInputClass *klass,
					 HTMLImageFactory    *imf,
					 gchar               *name,
					 gchar               *url);
HTMLObject *html_imageinput_new         (HTMLImageFactory    *imf,
					 gchar               *name,
					 gchar               *url);

#endif /* _HTMLIMAGEINPUT_H_ */
