/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 *  Copyright (C) 2000 Helix Code Inc.
 *
 *  Authors: Michael Zucchi <notzed@helixcode.com>
 *
 *  An embeddable html widget.
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


#include "cschtml-embedded.h"
#include "htmlengine.h"

static void csc_html_embedded_class_init (CscHTMLEmbeddedClass *class);
static void csc_html_embedded_init       (CscHTMLEmbedded *gspaper);

struct _CscHTMLEmbeddedPrivate {
};

static void csc_html_embedded_size_request (GtkWidget *widget, GtkRequisition *requisition);
static void csc_html_embedded_size_allocate (GtkWidget *widget, GtkAllocation *allocation);

/* saved parent calls */
static void (*old_add)(GtkContainer *container, GtkWidget *child);
static void (*old_remove)(GtkContainer *container, GtkWidget *child);

static GtkBin *parent_class;

enum {
	CHANGED,
	LAST_SIGNAL
};
	
static guint signals [LAST_SIGNAL] = { 0 };

guint
csc_html_embedded_get_type (void)
{
	static guint select_paper_type = 0;
	
	if (!select_paper_type) {
		GtkTypeInfo html_embedded_info = {
			"CscHTMLEmbedded",
			sizeof (CscHTMLEmbedded),
			sizeof (CscHTMLEmbeddedClass),
			(GtkClassInitFunc) csc_html_embedded_class_init,
			(GtkObjectInitFunc) csc_html_embedded_init,
			(GtkArgSetFunc) NULL,
			(GtkArgGetFunc) NULL
		};
    
		select_paper_type = gtk_type_unique (gtk_bin_get_type (), &html_embedded_info);
	}
  
	return select_paper_type;
}

static void
free_param(void *key, void *value, void *data)
{
	g_free(key);
	g_free(value);
}

static void
csc_html_embedded_finalize (GtkObject *object)
{
	CscHTMLEmbedded *eb = CSC_HTML_EMBEDDED(object);

	g_hash_table_foreach(eb->params, free_param, 0);
	g_hash_table_destroy(eb->params);
	g_free(eb->classid);
	g_free(eb->priv);
	g_free(eb->type);

	GTK_OBJECT_CLASS(parent_class)->finalize (object);
}

static void
csc_html_embedded_changed (CscHTMLEmbedded *ge)
{
	gtk_signal_emit (GTK_OBJECT (ge), signals[CHANGED]);
}

static void csc_html_embedded_add (GtkContainer *container, GtkWidget *child)
{
	g_return_if_fail (container != NULL);

	/* can't add something twice */
	g_return_if_fail( GTK_BIN(container)->child == NULL );

	old_add(container, child);
	csc_html_embedded_changed(CSC_HTML_EMBEDDED(container));
}

static void csc_html_embedded_remove (GtkContainer *container, GtkWidget *child)
{
	g_return_if_fail (container != NULL);
	g_return_if_fail( GTK_BIN(container)->child != NULL );

	old_remove(container, child);

	csc_html_embedded_changed(CSC_HTML_EMBEDDED(container));
}


static void
csc_html_embedded_class_init (CscHTMLEmbeddedClass *class)
{
	GtkObjectClass *object_class;
	GtkWidgetClass *widget_class;
	GtkContainerClass *container_class;

	object_class = (GtkObjectClass *) class;
	widget_class = (GtkWidgetClass*) class;
	container_class = (GtkContainerClass*) class;

	parent_class = gtk_type_class (gtk_bin_get_type ());

	signals [CHANGED] =
		gtk_signal_new ("changed",
				GTK_RUN_FIRST,
				object_class->type,
				GTK_SIGNAL_OFFSET (CscHTMLEmbeddedClass, changed),
				gtk_marshal_NONE__NONE,
				GTK_TYPE_NONE, 0);

	gtk_object_class_add_signals (object_class, signals, LAST_SIGNAL);

	object_class->finalize = csc_html_embedded_finalize;

	widget_class->size_request = csc_html_embedded_size_request;
	widget_class->size_allocate = csc_html_embedded_size_allocate;

	old_add = container_class->add;
	container_class->add = csc_html_embedded_add;
	old_remove = container_class->remove;
	container_class->remove = csc_html_embedded_remove;
}

static void
csc_html_embedded_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
	GtkBin *bin;
	
	g_return_if_fail (widget != NULL);
	g_return_if_fail (requisition != NULL);
	
	bin = GTK_BIN (widget);

	if (bin->child) {
		gtk_widget_size_request (bin->child, requisition);
	} else {
		requisition->width = widget->requisition.width;
		requisition->height = widget->requisition.height;
	}
}

static void
csc_html_embedded_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
	GtkBin *bin;
	
	g_return_if_fail (widget != NULL);
	g_return_if_fail (allocation != NULL);
	
	bin = GTK_BIN (widget);

	if (bin->child && GTK_WIDGET_VISIBLE (bin->child)) {
		gtk_widget_size_allocate(bin->child, allocation);
	}
	widget->allocation = *allocation;
}

static void
csc_html_embedded_init (CscHTMLEmbedded *ge)
{
	ge->descent = 0;
	ge->priv = g_malloc0(sizeof(*ge->priv));

	ge->params = g_hash_table_new (g_str_hash, g_str_equal);
}

/**
 * csc_html_embedded_new:
 *
 * Create a new CscHTMLEmbedded widget.
 * Note that this function should never be called outside of cschtml.
 * 
 * Return value: A new CscHTMLEmbedded widget.
 **/
GtkWidget *
csc_html_embedded_new (char *classid, char *name, char *type, int width, int height)
{
	CscHTMLEmbedded *em;

	em = (CscHTMLEmbedded *)( gtk_type_new (csc_html_embedded_get_type ()));
	em->width = width;
	em->height = height;
	em->type = type ? g_strdup(type) : NULL;
	em->classid = g_strdup(classid);
	em->name = g_strdup(name);

	return (GtkWidget *)em;
}

char *
csc_html_embedded_get_parameter (CscHTMLEmbedded *ge, char *param)
{
	return g_hash_table_lookup(ge->params, param);
}

void
csc_html_embedded_set_parameter (CscHTMLEmbedded *ge, char *param, char *value)
{
	if (param==0)
		return;
	g_hash_table_insert(ge->params, g_strdup(param), 
			    value ? g_strdup(value) : NULL);
}

void
csc_html_embedded_set_descent (CscHTMLEmbedded *ge, int descent)
{
	if (ge->descent == descent)
		return;

	ge->descent = descent;
	csc_html_embedded_changed(ge);
}
