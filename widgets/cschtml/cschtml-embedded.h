/*
 *  Copyright (C) 2000 Helix Code Inc.
 *
 *  Authors: Michael Zucchi <notzed@helixcode.com>
 *
 *  An embedded html widget.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License
 *  as published by the Free Software Foundation; either version 2 of
 *  the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef _CSC_HTML_EMBEDDED_H
#define _CSC_HTML_EMBEDDED_H

#include <gtk/gtk.h>

#define CSC_HTML_EMBEDDED(obj) GTK_CHECK_CAST(obj, csc_html_embedded_get_type(), CscHTMLEmbedded)
#define CSC_HTML_EMBEDDED_CLASS(klass) GTK_CHECK_CLASS_CAST(klass, csc_html_embedded_get_type(), CscHTMLEmbeddedClass)
#define GTK_IS_CSCHTML_EMBEDDED(obj) GTK_CHECK_TYPE(obj, csc_html_embedded_get_type())


typedef struct _CscHTMLEmbedded      CscHTMLEmbedded;
typedef struct _CscHTMLEmbeddedClass CscHTMLEmbeddedClass;

typedef struct _CscHTMLEmbeddedPrivate CscHTMLEmbeddedPrivate;

struct _CscHTMLEmbedded {
	GtkBin bin;

	/* class id of this object */
	char *classid;
	char *name;
        char *type;

	/* parameters to class */
	int width, height;
	GHashTable *params;

	CscHTMLEmbeddedPrivate *priv;

	int descent;
};

struct _CscHTMLEmbeddedClass {
	GtkBinClass parent_class;

	void (*changed)(CscHTMLEmbedded *);
};

/* FIXME: There needs to be a way for embedded objects in forms to encode
   themselves for a form */

guint		csc_html_embedded_get_type	(void);
GtkWidget	*csc_html_embedded_new (char *classid, char *name, char *type, int width, int height);

void csc_html_embedded_set_parameter (CscHTMLEmbedded *ge, char *param, char *value);
char *csc_html_embedded_get_parameter (CscHTMLEmbedded *ge, char *param);
void csc_html_embedded_set_descent (CscHTMLEmbedded *ge, int descent);

#endif /* ! _CSC_HTML_EMBEDDED_H */
