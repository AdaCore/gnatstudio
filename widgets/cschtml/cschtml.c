/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

    Copyright 1999, 2000 Helix Code, Inc.

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

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "cscmarshal.h"
#include "htmlengine-edit-clueflowstyle.h"
#include "htmlengine-edit-copy.h"
#include "htmlengine-edit-cut.h"
#include "htmlengine-edit-delete.h"
#include "htmlengine-edit-fontstyle.h"
#include "htmlengine-edit-insert.h"
#include "htmlengine-edit-movement.h"
#include "htmlengine-edit-paste.h"
#include "htmlengine-edit.h"

#include "cschtml-embedded.h"
#include "cschtml-keybinding.h"
#include "cschtml-stream.h"
#include "cschtml-private.h"

static GtkLayoutClass *parent_class = NULL;

enum {
	TITLE_CHANGED,
	URL_REQUESTED,
	LOAD_DONE,
	LINK_CLICKED,
	SET_BASE,
	SET_BASE_TARGET,
	ON_URL,
	REDIRECT,
	SUBMIT,
	OBJECT_REQUESTED,
	CURRENT_PARAGRAPH_STYLE_CHANGED,
	CURRENT_PARAGRAPH_INDENTATION_CHANGED,
	CURRENT_PARAGRAPH_ALIGNMENT_CHANGED,
	INSERTION_FONT_STYLE_CHANGED,
	LAST_SIGNAL
};
static guint signals [LAST_SIGNAL] = { 0 };

/* Values for selection information.  FIXME: what about COMPOUND_STRING and
   TEXT?  */
enum _TargetInfo {  
	TARGET_STRING,
	TARGET_TEXT,
	TARGET_COMPOUND_TEXT
};
typedef enum _TargetInfo TargetInfo;

/* Interval for scrolling during selection.  */
#define SCROLL_TIMEOUT_INTERVAL 10


static CscHTMLParagraphStyle
clueflow_style_to_paragraph_style (HTMLClueFlowStyle style)
{
	switch (style) {
	case HTML_CLUEFLOW_STYLE_NORMAL:
		return CSC_HTML_PARAGRAPH_STYLE_NORMAL;
	case HTML_CLUEFLOW_STYLE_H1:
		return CSC_HTML_PARAGRAPH_STYLE_H1;
	case HTML_CLUEFLOW_STYLE_H2:
		return CSC_HTML_PARAGRAPH_STYLE_H2;
	case HTML_CLUEFLOW_STYLE_H3:
		return CSC_HTML_PARAGRAPH_STYLE_H3;
	case HTML_CLUEFLOW_STYLE_H4:
		return CSC_HTML_PARAGRAPH_STYLE_H4;
	case HTML_CLUEFLOW_STYLE_H5:
		return CSC_HTML_PARAGRAPH_STYLE_H5;
	case HTML_CLUEFLOW_STYLE_H6:
		return CSC_HTML_PARAGRAPH_STYLE_H6;
	case HTML_CLUEFLOW_STYLE_ADDRESS:
		return CSC_HTML_PARAGRAPH_STYLE_ADDRESS;
	case HTML_CLUEFLOW_STYLE_PRE:
		return CSC_HTML_PARAGRAPH_STYLE_PRE;
	case HTML_CLUEFLOW_STYLE_ITEMDOTTED:
		return CSC_HTML_PARAGRAPH_STYLE_ITEMDOTTED;
	case HTML_CLUEFLOW_STYLE_ITEMROMAN:
		return CSC_HTML_PARAGRAPH_STYLE_ITEMROMAN;
	case HTML_CLUEFLOW_STYLE_ITEMDIGIT:
		return CSC_HTML_PARAGRAPH_STYLE_ITEMDIGIT;
	default:		/* This should not really happen, though.  */
		return CSC_HTML_PARAGRAPH_STYLE_NORMAL;
	}
}

static HTMLClueFlowStyle
paragraph_style_to_clueflow_style (CscHTMLParagraphStyle style)
{
	switch (style) {
	case CSC_HTML_PARAGRAPH_STYLE_NORMAL:
		return HTML_CLUEFLOW_STYLE_NORMAL;
	case CSC_HTML_PARAGRAPH_STYLE_H1:
		return HTML_CLUEFLOW_STYLE_H1;
	case CSC_HTML_PARAGRAPH_STYLE_H2:
		return HTML_CLUEFLOW_STYLE_H2;
	case CSC_HTML_PARAGRAPH_STYLE_H3:
		return HTML_CLUEFLOW_STYLE_H3;
	case CSC_HTML_PARAGRAPH_STYLE_H4:
		return HTML_CLUEFLOW_STYLE_H4;
	case CSC_HTML_PARAGRAPH_STYLE_H5:
		return HTML_CLUEFLOW_STYLE_H5;
	case CSC_HTML_PARAGRAPH_STYLE_H6:
		return HTML_CLUEFLOW_STYLE_H6;
	case CSC_HTML_PARAGRAPH_STYLE_ADDRESS:
		return HTML_CLUEFLOW_STYLE_ADDRESS;
	case CSC_HTML_PARAGRAPH_STYLE_PRE:
		return HTML_CLUEFLOW_STYLE_PRE;
	case CSC_HTML_PARAGRAPH_STYLE_ITEMDOTTED:
		return HTML_CLUEFLOW_STYLE_ITEMDOTTED;
	case CSC_HTML_PARAGRAPH_STYLE_ITEMROMAN:
		return HTML_CLUEFLOW_STYLE_ITEMROMAN;
	case CSC_HTML_PARAGRAPH_STYLE_ITEMDIGIT:
		return HTML_CLUEFLOW_STYLE_ITEMDIGIT;
	default:		/* This should not really happen, though.  */
		return HTML_CLUEFLOW_STYLE_NORMAL;
	}
}

static HTMLHAlignType
paragraph_alignment_to_html (CscHTMLParagraphAlignment alignment)
{
	switch (alignment) {
	case CSC_HTML_PARAGRAPH_ALIGNMENT_LEFT:
		return HTML_HALIGN_LEFT;
	case CSC_HTML_PARAGRAPH_ALIGNMENT_RIGHT:
		return HTML_HALIGN_RIGHT;
	case CSC_HTML_PARAGRAPH_ALIGNMENT_CENTER:
		return HTML_HALIGN_CENTER;
	default:
		return HTML_HALIGN_LEFT;
	}
}

static CscHTMLParagraphAlignment
html_alignment_to_paragraph (HTMLHAlignType alignment)
{
	switch (alignment) {
	case HTML_HALIGN_LEFT:
		return CSC_HTML_PARAGRAPH_ALIGNMENT_LEFT;
	case HTML_HALIGN_CENTER:
		return CSC_HTML_PARAGRAPH_ALIGNMENT_CENTER;
	case HTML_HALIGN_RIGHT:
		return CSC_HTML_PARAGRAPH_ALIGNMENT_RIGHT;
	default:
		return CSC_HTML_PARAGRAPH_ALIGNMENT_LEFT;
	}
}

static void
update_styles (CscHTML *html)
{
	CscHTMLParagraphStyle paragraph_style;
	CscHTMLParagraphAlignment alignment;
	HTMLClueFlowStyle clueflow_style;
	HTMLEngine *engine;
	guint indentation;

	if (! html_engine_get_editable (html->engine))
		return;

	engine = html->engine;

	clueflow_style = html_engine_get_current_clueflow_style (engine);
	paragraph_style = clueflow_style_to_paragraph_style (clueflow_style);

	if (paragraph_style != html->paragraph_style) {
		html->paragraph_style = paragraph_style;
		gtk_signal_emit (GTK_OBJECT (html), signals[CURRENT_PARAGRAPH_STYLE_CHANGED],
				 paragraph_style);
	}

	indentation = html_engine_get_current_clueflow_indentation (engine);
	if (indentation != html->paragraph_indentation) {
		html->paragraph_style = paragraph_style;
		gtk_signal_emit (GTK_OBJECT (html), signals[CURRENT_PARAGRAPH_STYLE_CHANGED], paragraph_style);
	}

	alignment = html_alignment_to_paragraph (html_engine_get_current_clueflow_alignment (engine));
	if (alignment != html->paragraph_alignment) {
		html->paragraph_alignment = alignment;
		gtk_signal_emit (GTK_OBJECT (html), signals[CURRENT_PARAGRAPH_ALIGNMENT_CHANGED], alignment);
	}

	if (html_engine_update_insertion_font_style (engine))
		gtk_signal_emit (GTK_OBJECT (html), signals[INSERTION_FONT_STYLE_CHANGED], engine->insertion_font_style);
}


/* GTK+ idle loop handler.  */

static gint
idle_handler (gpointer data)
{
	CscHTML *html;
	HTMLEngine *engine;

	html = CSC_HTML (data);
	engine = html->engine;

	html_engine_make_cursor_visible (engine);

	gtk_adjustment_set_value (GTK_LAYOUT (html)->hadjustment, (gdouble) engine->x_offset);
	gtk_adjustment_set_value (GTK_LAYOUT (html)->vadjustment, (gdouble) engine->y_offset);

	csc_html_private_calc_scrollbars (html);

	html_engine_flush_draw_queue (engine);

	html->idle_handler_id = 0;
	return FALSE;
}

static void
queue_draw (CscHTML *html)
{
	if (html->idle_handler_id == 0)
		html->idle_handler_id = gtk_idle_add (idle_handler, html);
}


/* HTMLEngine callbacks.  */

static void
html_engine_title_changed_cb (HTMLEngine *engine, gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[TITLE_CHANGED], engine->title->str);
}

static void
html_engine_set_base_cb (HTMLEngine *engine, const gchar *base, gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[SET_BASE], base);
}

static void
html_engine_set_base_target_cb (HTMLEngine *engine, const gchar *base_target, gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[SET_BASE_TARGET], base_target);
}

static void
html_engine_load_done_cb (HTMLEngine *engine, gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[LOAD_DONE]);
}

static void
html_engine_url_requested_cb (HTMLEngine *engine,
			      const gchar *url,
			      CscHTMLStream *handle,
			      gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[URL_REQUESTED], url, handle);
}

static void
html_engine_draw_pending_cb (HTMLEngine *engine,
			     gpointer data)
{
	CscHTML *html;

	html = CSC_HTML (data);
	queue_draw (html);
}

static void
html_engine_redirect_cb (HTMLEngine *engine,
			 const gchar *url,
			 int delay,
			 gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);

	gtk_signal_emit (GTK_OBJECT (csc_html), signals[REDIRECT], url, delay);
}

static void
html_engine_submit_cb (HTMLEngine *engine,
		       const gchar *method,
		       const gchar *url,
		       const gchar *encoding,
		       gpointer data)
{
	CscHTML *csc_html;

	csc_html = CSC_HTML (data);

	gtk_signal_emit (GTK_OBJECT (csc_html), signals[SUBMIT], method, url, encoding);
}

static gboolean
html_engine_object_requested_cb (HTMLEngine *engine,
		       CscHTMLEmbedded *eb,
		       gpointer data)
{
	CscHTML *csc_html;
	gboolean ret_val = FALSE;

	csc_html = CSC_HTML (data);

	ret_val = FALSE;
	gtk_signal_emit (GTK_OBJECT (csc_html), signals[OBJECT_REQUESTED], eb, &ret_val);
	return ret_val;
}


/* Scroll timeout handling.  */

static void
inc_adjustment (GtkAdjustment *adj, gint doc_width, gint alloc_width, gint inc)
{
	gdouble value;
	gint max;

	value = adj->value + (gdouble) inc;
	
	if (doc_width > alloc_width)
		max = doc_width - alloc_width;
	else
		max = 0;

	if (value > (gdouble) max)
		value = (gdouble) max;
	else if (value < 0)
		value = 0.0;

	gtk_adjustment_set_value (adj, value);
}

static gint
scroll_timeout_cb (gpointer data)
{
	GtkWidget *widget;
	CscHTML *html;
	GtkLayout *layout;
	gint x_scroll, y_scroll;
	gint x, y;

	GDK_THREADS_ENTER ();

	widget = GTK_WIDGET (data);
	html = CSC_HTML (data);

	gdk_window_get_pointer (widget->window, &x, &y, NULL);

	if (x < 0) {
		x_scroll = x;
		x = 0;
	} else if (x >= widget->allocation.width) {
		x_scroll = x - widget->allocation.width + 1;
		x = widget->allocation.width;
	} else {
		x_scroll = 0;
	}
	x_scroll /= 2;

	if (y < 0) {
		y_scroll = y;
		y = 0;
	} else if (y >= widget->allocation.height) {
		y_scroll = y - widget->allocation.height + 1;
		y = widget->allocation.height;
	} else {
		y_scroll = 0;
	}
	y_scroll /= 2;

	if (html->in_selection && (x_scroll != 0 || y_scroll != 0)) {
		HTMLEngine *engine;

		engine = html->engine;
		html_engine_select_region (engine,
					   html->selection_x1, html->selection_y1,
					   x + engine->x_offset, y + engine->y_offset,
					   TRUE);
	}

	layout = GTK_LAYOUT (widget);

	inc_adjustment (layout->hadjustment, html_engine_get_doc_width (html->engine),
			widget->allocation.width, x_scroll);
	inc_adjustment (layout->vadjustment, html_engine_get_doc_height (html->engine),
			widget->allocation.height, y_scroll);

	GDK_THREADS_LEAVE ();

	return TRUE;
}

static void
setup_scroll_timeout (CscHTML *html)
{
	if (html->scroll_timeout_id != 0)
		return;

	html->scroll_timeout_id = gtk_timeout_add (SCROLL_TIMEOUT_INTERVAL,
						   scroll_timeout_cb, html);

	scroll_timeout_cb (html);
}

static void
remove_scroll_timeout (CscHTML *html)
{
	if (html->scroll_timeout_id == 0)
		return;

	gtk_timeout_remove (html->scroll_timeout_id);
	html->scroll_timeout_id = 0;
}


/* GtkObject methods.  */

static void
destroy (GtkObject *object)
{
	CscHTML *html;
	g_return_if_fail (GTK_IS_CSCHTML (object));

	html = CSC_HTML (object);

	/* Glide2 team: this widget is destroyed twice !??! */
	if (html->engine == NULL) {
           return;
	}

	g_free (html->pointer_url);

	gdk_cursor_destroy (html->hand_cursor);
	gdk_cursor_destroy (html->ibeam_cursor);

	if (html->idle_handler_id != 0)
		gtk_idle_remove (html->idle_handler_id);

	if (html->scroll_timeout_id != 0)
		gtk_timeout_remove (html->scroll_timeout_id);

	gtk_object_destroy (GTK_OBJECT (html->engine));
	html->engine = NULL;

	if (GTK_OBJECT_CLASS (parent_class)->destroy != NULL)
		(* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}


/* GtkWidget methods.  */

static gint
key_press_event (GtkWidget *widget,
		 GdkEventKey *event)
{
	CscHTML *html;
	HTMLEngine *engine;
	gboolean retval;
	gboolean do_update_styles;

	html = CSC_HTML (widget);
	engine = html->engine;

	if (! html_engine_get_editable (engine)) {
		/* FIXME handle differently in this case */
		return FALSE;
	}

	retval = csc_html_handle_key_event (CSC_HTML (widget), event, &do_update_styles);

	if (retval == TRUE) {
		queue_draw (html);
		if (do_update_styles)
			update_styles (html);
	}

	return retval;
}

static void
realize (GtkWidget *widget)
{
	CscHTML *html;

	g_return_if_fail (widget != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (widget));

	html = CSC_HTML (widget);

	if (GTK_WIDGET_CLASS (parent_class)->realize)
		(* GTK_WIDGET_CLASS (parent_class)->realize) (widget);

	gdk_window_set_events (html->layout.bin_window,
			       (gdk_window_get_events (html->layout.bin_window)
				| GDK_EXPOSURE_MASK | GDK_POINTER_MOTION_MASK
				| GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK
				| GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK));

	html_engine_realize (html->engine, html->layout.bin_window);

	gdk_window_set_cursor (widget->window, NULL);

	/* This sets the backing pixmap to None, so that scrolling does not
           erase the newly exposed area, thus making the thing smoother.  */
	gdk_window_set_back_pixmap (html->layout.bin_window, NULL, FALSE);
}

static void
unrealize (GtkWidget *widget)
{
	CscHTML *html = CSC_HTML (widget);
	
	html_gdk_painter_unrealize (HTML_GDK_PAINTER (html->engine->painter));

	if (GTK_WIDGET_CLASS (parent_class)->unrealize)
		(* GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static gint
expose (GtkWidget *widget, GdkEventExpose *event)
{
	CscHTML *html = CSC_HTML (widget);

	html_engine_draw (html->engine,
			  event->area.x, event->area.y,
			  event->area.width, event->area.height);

	if (GTK_WIDGET_CLASS (parent_class)->expose_event)
		(* GTK_WIDGET_CLASS (parent_class)->expose_event) (widget, event);

	return FALSE;
}

static void
size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
	CscHTML *html;

	g_return_if_fail (widget != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (widget));
	g_return_if_fail (allocation != NULL);
	
	html = CSC_HTML (widget);

	if (GTK_WIDGET_CLASS (parent_class)->size_allocate)
		( *GTK_WIDGET_CLASS (parent_class)->size_allocate) (widget, allocation);

	if (html->engine->width != allocation->width
	    || html->engine->height != allocation->height) {
		html->engine->width = allocation->width;
		html->engine->height = allocation->height;

		html_engine_calc_size (html->engine);

		csc_html_private_calc_scrollbars (html);
	}
}

static gint
motion_notify_event (GtkWidget *widget,
		     GdkEventMotion *event)
{
	CscHTML *html;
	HTMLEngine *engine;
	HTMLObject *obj;
	GdkModifierType mask;
	const gchar *url;
	gint x, y;
	HTMLType type;

	g_return_val_if_fail (widget != NULL, 0);
	g_return_val_if_fail (GTK_IS_CSCHTML (widget), 0);
	g_return_val_if_fail (event != NULL, 0);

	html = CSC_HTML (widget);
	engine = html->engine;

	if (event->is_hint) {
		gdk_window_get_pointer (GTK_LAYOUT (widget)->bin_window, &x, &y, &mask);
	} else {
		x = event->x;
		y = event->y;
	}

	obj = html_engine_get_object_at (engine,
					 x + engine->x_offset, y + engine->y_offset,
					 NULL, FALSE);
	if (html->button_pressed) {
		if (obj) {
			type = HTML_OBJECT_TYPE (obj);

			/* FIXME this is broken */

			if (type == HTML_TYPE_BUTTON ||
			    type ==  HTML_TYPE_CHECKBOX ||
			    type ==  HTML_TYPE_EMBEDDED ||
			    type ==  HTML_TYPE_HIDDEN ||
			    type ==  HTML_TYPE_IMAGEINPUT ||
			    type ==  HTML_TYPE_RADIO ||
			    type ==  HTML_TYPE_SELECT ||
			    type ==  HTML_TYPE_TEXTAREA ||
			    type ==  HTML_TYPE_TEXTINPUT ) {
				return FALSE;
			}
		}

		html->in_selection = TRUE;

		if (x < 0 || x >= widget->allocation.width
		    || y < 0 || y >= widget->allocation.height)
			setup_scroll_timeout (html);
		else
			remove_scroll_timeout (html);

		/* This will put the mark at the position of the
                   previous click.  */
		if (engine->mark == NULL && engine->editable)
			html_engine_set_mark (engine);

		html_engine_select_region (engine,
					   html->selection_x1, html->selection_y1,
					   x + engine->x_offset, y + engine->y_offset,
					   TRUE);
		
		if (html_engine_get_editable (engine))
			html_engine_jump_at (engine,
					     event->x + engine->x_offset,
					     event->y + engine->y_offset);
		return TRUE;
	}

	if (obj != NULL)
		url = html_object_get_url (obj);
	else
		url = NULL;

	if (url == NULL) {
		if (html->pointer_url != NULL) {
			g_free (html->pointer_url);
			html->pointer_url = NULL;
			gtk_signal_emit (GTK_OBJECT (html), signals[ON_URL], NULL);
		}

		if (obj != NULL && html_object_is_text (obj))
			gdk_window_set_cursor (widget->window, html->ibeam_cursor);
		else
			gdk_window_set_cursor (widget->window, NULL);
	} else {
		if (html->pointer_url == NULL || strcmp (html->pointer_url, url) != 0) {
			g_free (html->pointer_url);
			html->pointer_url = g_strdup (url);
			gtk_signal_emit (GTK_OBJECT (html), signals[ON_URL], url);
		}

		if (engine->editable)
			gdk_window_set_cursor (widget->window, html->ibeam_cursor);
		else
			gdk_window_set_cursor (widget->window, html->hand_cursor);
	}

	return TRUE;
}

static gint
button_press_event (GtkWidget *widget,
		    GdkEventButton *event)
{
	CscHTML *html;
	HTMLEngine *engine;
	gint value;

	html = CSC_HTML (widget);
	engine = html->engine;

	gtk_widget_grab_focus (widget);

	if (event->type == GDK_BUTTON_PRESS) {
		GtkAdjustment *vadj;
			
		vadj = GTK_LAYOUT (widget)->vadjustment;
		
		switch (event->button) {
		case 4:
			/* Mouse wheel scroll up.  */
			value = vadj->value - vadj->step_increment * 3;
			
			if (value < vadj->lower)
				value = vadj->lower;
			
			gtk_adjustment_set_value (vadj, value);
			return TRUE;
			break;
		case 5:
			/* Mouse wheel scroll down.  */
			value = vadj->value + vadj->step_increment * 3;
			
			if (value > (vadj->upper - vadj->page_size))
				value = vadj->upper - vadj->page_size;
			
			gtk_adjustment_set_value (vadj, value);
			return TRUE;
			break;
		case 2:
			csc_html_request_paste (widget, event->time);
			return TRUE;
			break;
		default:
			break;
		}
	}

	if (html_engine_get_editable (engine)) {
		html_engine_jump_at (engine,
				     event->x + engine->x_offset,
				     event->y + engine->y_offset);
		update_styles (html);
	}

	if (html->allow_selection) {
		gtk_grab_add (widget);
		gdk_pointer_grab (widget->window, TRUE,
				  (GDK_BUTTON_RELEASE_MASK
				   | GDK_BUTTON_MOTION_MASK
				   | GDK_POINTER_MOTION_HINT_MASK),
				  NULL, NULL, 0);

		html->selection_x1 = event->x + engine->x_offset;
		html->selection_y1 = event->y + engine->y_offset;
	}

	html->button_pressed = TRUE;

	html_engine_disable_selection (engine);

	return TRUE;
}

static gint
button_release_event (GtkWidget *widget,
		      GdkEventButton *event)
{
	CscHTML *html;

	html = CSC_HTML (widget);

	gtk_grab_remove (widget);
	gdk_pointer_ungrab (0);

	if (event->button == 1 && html->pointer_url != NULL && ! html->in_selection)
		gtk_signal_emit (GTK_OBJECT (widget), signals[LINK_CLICKED], html->pointer_url);

	html->button_pressed = FALSE;

	if (html->in_selection) {
		gtk_selection_owner_set (widget, GDK_SELECTION_PRIMARY, event->time);
		html->in_selection = FALSE;
		update_styles (html);
	}

	remove_scroll_timeout (html);

	return TRUE;
}

static gint
focus_in_event (GtkWidget *widget,
		GdkEventFocus *event)
{
	GTK_WIDGET_SET_FLAGS (widget, GTK_HAS_FOCUS);

	html_engine_set_focus (CSC_HTML (widget)->engine, TRUE);

	return FALSE;
}

static gint
focus_out_event (GtkWidget *widget,
		 GdkEventFocus *event)
{
	GTK_WIDGET_UNSET_FLAGS (widget, GTK_HAS_FOCUS);

	html_engine_set_focus (CSC_HTML (widget)->engine, FALSE);

	return FALSE;
}


/* X11 selection support.  */

static void
selection_get (GtkWidget        *widget, 
	       GtkSelectionData *selection_data_ptr,
	       guint             info,
	       guint             time)
{
	CscHTML *html;
	gchar *selection_string;
	
	g_return_if_fail (widget != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (widget));
	
	html = CSC_HTML (widget);
	selection_string = html_engine_get_selection_string (html->engine);
	
	if (selection_string != NULL) {
		if (info == TARGET_STRING)
			{
				gtk_selection_data_set (selection_data_ptr,
							GDK_SELECTION_TYPE_STRING, 8,
							selection_string, 
							strlen (selection_string));
			}
		else if ((info == TARGET_TEXT) || (info == TARGET_COMPOUND_TEXT))
			{
				guchar *text;
				GdkAtom encoding;
				gint format;
				gint new_length;
				
				gdk_string_to_compound_text (selection_string, 
							     &encoding, &format,
							     &text, &new_length);

				gtk_selection_data_set (selection_data_ptr,
							encoding, format,
							text, new_length);
				gdk_free_compound_text (text);
			}
		g_free (selection_string);
	}
}

/* receive a selection */
/* Signal handler called when the selections owner returns the data */
static void
selection_received (GtkWidget *widget,
		    GtkSelectionData *selection_data, 
		    guint time)
{
	g_return_if_fail (widget != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (widget));
	g_return_if_fail (selection_data != NULL);
	
	printf("got selection from system\n");
	
#if 0
	/* **** IMPORTANT **** Check to see if retrieval succeeded  */
	/* If we have more selection types we can ask for, try the next one,
	   until there are none left */
	if (selection_data->length < 0) {
		struct _zvtprivate *zp = _ZVT_PRIVATE(widget);
		
		/* now, try again with next selection type */
		if (csc_html_request_paste(widget, zp->lastselectiontype+1, time)==0)
			g_print ("Selection retrieval failed\n");
		return;
	}
#endif
	
	/* we will get a selection type of atom(UTF-8) for utf text,
	   perhaps that needs to do something different if the terminal
	   isn't actually in utf8 mode? */
	
	/* Make sure we got the data in the expected form */
	if (selection_data->type != GDK_SELECTION_TYPE_STRING) {
		g_print ("Selection \"STRING\" was not returned as strings!\n");
		return;
	}
	
	if (selection_data->length) {
		printf ("selection text \"%.*s\"\n",
			selection_data->length, selection_data->data); 

		html_engine_disable_selection (CSC_HTML (widget)->engine);
		html_engine_insert (CSC_HTML (widget)->engine, 
				    selection_data->data,
				    selection_data->length);
	}
}  

int
csc_html_request_paste (GtkWidget *widget, gint32 time)
{
  GdkAtom string_atom;

  string_atom = gdk_atom_intern ("STRING", FALSE);

  if (string_atom == GDK_NONE) {
    g_warning("WARNING: Could not get string atom\n");
  }
  /* And request the "STRING" target for the primary selection */
    gtk_selection_convert (widget, GDK_SELECTION_PRIMARY, string_atom,
			   time);
  return 1;
}


static gint
selection_clear_event (GtkWidget *widget,
		       GdkEventSelection *event)
{
	CscHTML *html;

	if (! gtk_selection_clear (widget, event))
		return FALSE;

	html = CSC_HTML (widget);

	html_engine_disable_selection (html->engine);
	html->in_selection = FALSE;

	return TRUE;
}


static void
set_adjustments (GtkLayout     *layout,
		 GtkAdjustment *hadj,
		 GtkAdjustment *vadj)
{
	CscHTML *html = CSC_HTML(layout);

	if (parent_class->set_scroll_adjustments)
		(* parent_class->set_scroll_adjustments) (layout, hadj, vadj);
}


/* Initialization.  */

static void
class_init (CscHTMLClass *klass)
{
	CscHTMLClass *html_class;
	GtkWidgetClass *widget_class;
	GtkObjectClass *object_class;
	GtkLayoutClass *layout_class;
	
	html_class = (CscHTMLClass *)klass;
	widget_class = (GtkWidgetClass *)klass;
	object_class = (GtkObjectClass *)klass;
	layout_class = (GtkLayoutClass *)klass;
	
	object_class->destroy = destroy;

	parent_class = gtk_type_class (GTK_TYPE_LAYOUT);

	signals [TITLE_CHANGED] = 
	  gtk_signal_new ("title_changed",
			  GTK_RUN_FIRST,
			  G_TYPE_FROM_CLASS (object_class),
			  GTK_SIGNAL_OFFSET (CscHTMLClass, title_changed),
			  gtk_marshal_NONE__STRING,
			  GTK_TYPE_NONE, 1,
			  GTK_TYPE_STRING);
	signals [URL_REQUESTED] =
	  gtk_signal_new ("url_requested",
			  GTK_RUN_FIRST,
			  G_TYPE_FROM_CLASS (object_class),
			  GTK_SIGNAL_OFFSET (CscHTMLClass, url_requested),
			  cschtml_VOID__STRING_POINTER,
			  GTK_TYPE_NONE, 2,
			  GTK_TYPE_STRING,
			  GTK_TYPE_POINTER);
	signals [LOAD_DONE] = 
	  gtk_signal_new ("load_done",
			  GTK_RUN_FIRST,
			  G_TYPE_FROM_CLASS (object_class),
			  GTK_SIGNAL_OFFSET (CscHTMLClass, load_done),
			  gtk_marshal_NONE__NONE,
			  GTK_TYPE_NONE, 0);
	signals [LINK_CLICKED] =
	  gtk_signal_new ("link_clicked",
			  GTK_RUN_FIRST,
			  G_TYPE_FROM_CLASS (object_class),
			  GTK_SIGNAL_OFFSET (CscHTMLClass, link_clicked),
			  gtk_marshal_NONE__STRING,
			  GTK_TYPE_NONE, 1,
			  GTK_TYPE_STRING);
	signals [SET_BASE] =
		gtk_signal_new ("set_base",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, set_base),
				gtk_marshal_NONE__STRING,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_STRING);
	signals [SET_BASE_TARGET] =
		gtk_signal_new ("set_base_target",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, set_base_target),
				gtk_marshal_NONE__STRING,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_STRING);
	
	signals [ON_URL] =
		gtk_signal_new ("on_url",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, on_url),
				gtk_marshal_NONE__STRING,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_STRING);
	
	signals [REDIRECT] =
		gtk_signal_new ("redirect",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, redirect),
				gtk_marshal_NONE__POINTER_INT,
				GTK_TYPE_NONE, 2,
				GTK_TYPE_STRING,
				GTK_TYPE_INT);
	
	signals [SUBMIT] =
		gtk_signal_new ("submit",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, submit),
				gtk_marshal_NONE__POINTER_POINTER_POINTER,
				GTK_TYPE_NONE, 3,
				GTK_TYPE_STRING,
				GTK_TYPE_STRING,
				GTK_TYPE_STRING);

	signals [OBJECT_REQUESTED] =
		gtk_signal_new ("object_requested",
				GTK_RUN_LAST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, object_requested),
				gtk_marshal_BOOL__POINTER,
				GTK_TYPE_BOOL, 1,
				GTK_TYPE_OBJECT);
	
	signals [CURRENT_PARAGRAPH_STYLE_CHANGED] =
		gtk_signal_new ("current_paragraph_style_changed",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, current_paragraph_style_changed),
				gtk_marshal_NONE__INT,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_INT);

	signals [CURRENT_PARAGRAPH_INDENTATION_CHANGED] =
		gtk_signal_new ("current_paragraph_indentation_changed",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, current_paragraph_indentation_changed),
				gtk_marshal_NONE__INT,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_INT);

	signals [CURRENT_PARAGRAPH_ALIGNMENT_CHANGED] =
		gtk_signal_new ("current_paragraph_alignment_changed",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, current_paragraph_alignment_changed),
				gtk_marshal_NONE__INT,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_INT);

	signals [INSERTION_FONT_STYLE_CHANGED] =
		gtk_signal_new ("insertion_font_style_changed",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (CscHTMLClass, insertion_font_style_changed),
				gtk_marshal_NONE__INT,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_INT);
	
	object_class->destroy = destroy;
	
	widget_class->realize = realize;
	widget_class->unrealize = unrealize;
	widget_class->key_press_event = key_press_event;
	widget_class->expose_event  = expose;
	widget_class->size_allocate = size_allocate;
	widget_class->motion_notify_event = motion_notify_event;
	widget_class->button_press_event = button_press_event;
	widget_class->button_release_event = button_release_event;
	widget_class->focus_in_event = focus_in_event;
	widget_class->focus_out_event = focus_out_event;
	widget_class->selection_get = selection_get;
	widget_class->selection_received = selection_received;
	widget_class->selection_clear_event = selection_clear_event;

	layout_class->set_scroll_adjustments = set_adjustments;
}

static void
init (CscHTML* html)
{
	static const GtkTargetEntry targets[] = {
		{ "STRING", 0, TARGET_STRING },
		{ "TEXT",   0, TARGET_TEXT }, 
		{ "COMPOUND_TEXT", 0, TARGET_COMPOUND_TEXT },
	};
	static const gint n_targets = sizeof(targets) / sizeof(targets[0]);

	GTK_WIDGET_SET_FLAGS (GTK_WIDGET (html), GTK_CAN_FOCUS);
	GTK_WIDGET_SET_FLAGS (GTK_WIDGET (html), GTK_APP_PAINTABLE);

	html->debug = FALSE;
	html->allow_selection = TRUE;
	html->allow_font_switch = TRUE;

	html->pointer_url = NULL;
	html->default_font_face = g_strdup("lucida");
	html->hand_cursor = gdk_cursor_new (GDK_HAND2);
	html->ibeam_cursor = gdk_cursor_new (GDK_XTERM);
	html->hadj_connection = 0;
	html->vadj_connection = 0;

	html->selection_x1 = 0;
	html->selection_y1 = 0;

	html->in_selection = FALSE;
	html->button_pressed = FALSE;

	html->load_in_progress = TRUE;

	html->idle_handler_id = 0;
	html->scroll_timeout_id = 0;

	html->paragraph_style = CSC_HTML_PARAGRAPH_STYLE_NORMAL;
	html->paragraph_alignment = CSC_HTML_PARAGRAPH_ALIGNMENT_LEFT;
	html->paragraph_indentation = 0;

	html->insertion_font_style = CSC_HTML_FONT_STYLE_DEFAULT;

	gtk_selection_add_targets (GTK_WIDGET (html),
				   GDK_SELECTION_PRIMARY,
				   targets, n_targets);
}


GtkType
csc_html_get_type (void)
{
	static GtkType html_type = 0;

	if (!html_type) {
		static const GtkTypeInfo html_info = {
			"CscHTML",
			sizeof (CscHTML),
			sizeof (CscHTMLClass),
			(GtkClassInitFunc) class_init,
			(GtkObjectInitFunc) init,
			/* reserved_1 */ NULL,
			/* reserved_2 */ NULL,
			(GtkClassInitFunc) NULL,
		};
		
		html_type = gtk_type_unique (GTK_TYPE_LAYOUT, &html_info);
	}

	return html_type;
}

GtkWidget *
csc_html_new (void)
{
	GtkWidget *html;

	html = gtk_type_new (csc_html_get_type ());
	csc_html_construct (html);
	return html;
}

void
csc_html_construct (GtkWidget *htmlw)
{
	CscHTML *html;

	g_return_if_fail (htmlw != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (htmlw));

	html = CSC_HTML (htmlw);

	html->engine = html_engine_new ();
	html->engine->widget = html; /* FIXME FIXME */

	gtk_signal_connect (GTK_OBJECT (html->engine), "title_changed",
			    GTK_SIGNAL_FUNC (html_engine_title_changed_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "set_base",
			    GTK_SIGNAL_FUNC (html_engine_set_base_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "set_base_target",
			    GTK_SIGNAL_FUNC (html_engine_set_base_target_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "load_done",
			    GTK_SIGNAL_FUNC (html_engine_load_done_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "url_requested",
			    GTK_SIGNAL_FUNC (html_engine_url_requested_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "draw_pending",
			    GTK_SIGNAL_FUNC (html_engine_draw_pending_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "redirect",
			    GTK_SIGNAL_FUNC (html_engine_redirect_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "submit",
			    GTK_SIGNAL_FUNC (html_engine_submit_cb), html);
	gtk_signal_connect (GTK_OBJECT (html->engine), "object_requested",
			    GTK_SIGNAL_FUNC (html_engine_object_requested_cb), html);
}

HTMLEngine *
csc_html_get_engine (CscHTML *html)
{
	g_return_if_fail (html != NULL);

	return html->engine;
}


void
csc_html_enable_debug (CscHTML *html,
		       gboolean debug)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html->debug = debug;
}


void
csc_html_allow_selection (CscHTML *html,
			  gboolean allow)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html->allow_selection = allow;
}

void csc_html_allow_font_switch(CscHTML *html, gboolean allow) {

	g_return_if_fail(html != NULL);
	g_return_if_fail(GTK_IS_CSCHTML(html));

	html->allow_font_switch = allow;
}

void csc_html_set_default_font_face(CscHTML *html, gchar *face) {

	g_return_if_fail(html != NULL);
	g_return_if_fail(face != NULL);
	g_return_if_fail(GTK_IS_CSCHTML(html));

	if (html->default_font_face != NULL) {
		g_free(html->default_font_face);
	}
	html->default_font_face = g_strdup(face);
}

const gchar *csc_html_get_default_font_face(const CscHTML *html) {

	g_return_if_fail(html != NULL);
	g_return_if_fail(GTK_IS_CSCHTML(html));

	return html->default_font_face;
}

CscHTMLStream *
csc_html_begin (CscHTML *html)
{
	CscHTMLStream *handle;

	handle = html_engine_begin (html->engine);
	if (handle == NULL)
		return NULL;

	html_engine_parse (html->engine);

	html->load_in_progress = TRUE;

	return handle;
}

void
csc_html_write (CscHTML *html,
		CscHTMLStream *handle,
		const gchar *buffer,
		size_t size)
{
	csc_html_stream_write (handle, buffer, size);
}

void
csc_html_end (CscHTML *html,
	      CscHTMLStream *handle,
	      CscHTMLStreamStatus status)
{
	csc_html_stream_close (handle, status);

	html->load_in_progress = FALSE;
}


const gchar *
csc_html_get_title (CscHTML *html)
{
	g_return_val_if_fail (html != NULL, NULL);
	g_return_val_if_fail (GTK_IS_CSCHTML (html), NULL);

	if (html->engine->title == NULL)
		return NULL;

	return html->engine->title->str;
}

gboolean
csc_html_jump_to_anchor (CscHTML *html,
			 const gchar *anchor)
{
	g_return_val_if_fail (html != NULL, FALSE);
	g_return_val_if_fail (GTK_IS_CSCHTML (html), FALSE);
	
	return html_engine_goto_anchor (html->engine, anchor);
}


gboolean
csc_html_save (CscHTML *html,
	       CscHTMLSaveReceiverFn receiver,
	       gpointer data)
{
	g_return_val_if_fail (html != NULL, FALSE);
	g_return_val_if_fail (GTK_IS_CSCHTML (html), FALSE);
	g_return_val_if_fail (receiver != NULL, FALSE);
	
	return html_engine_save (html->engine, receiver, data);
}

gboolean
csc_html_export (CscHTML *html,
		 const char *type,
		 CscHTMLSaveReceiverFn receiver,
		 gpointer data)
{
	g_return_val_if_fail (html != NULL, FALSE);
	g_return_val_if_fail (GTK_IS_CSCHTML (html), FALSE);
	g_return_val_if_fail (receiver != NULL, FALSE);
	
	if (strcmp (type, "text/html") == 0) {
		return html_engine_save (html->engine, receiver, data);
	} else if (strcmp (type, "text/plain") == 0) {
		return html_engine_save_plain (html->engine, receiver,
					       data);  
	} else {
		return FALSE;
	}
}


void
csc_html_private_calc_scrollbars (CscHTML *html)
{
	GtkLayout *layout;
	GtkAdjustment *vadj, *hadj;
	gint width, height;

	height = html_engine_get_doc_height (html->engine);
	width = html_engine_get_doc_width (html->engine);

	layout = GTK_LAYOUT (html);
	hadj = layout->hadjustment;
	vadj = layout->vadjustment;

	vadj->lower = 0;
	vadj->upper = height;
	vadj->page_size = html->engine->height;
	vadj->step_increment = 14; /* FIXME */
	vadj->page_increment = html->engine->height;

	hadj->lower = 0.0;
	hadj->upper = width;
	hadj->page_size = html->engine->width;
	hadj->step_increment = 14; /* FIXME */
	hadj->page_increment = html->engine->width;

	if (width != layout->width || height != layout->height)
		gtk_layout_set_size (layout, width, height);
}


void
csc_html_set_editable (CscHTML *html,
		       gboolean editable)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_set_editable (html->engine, editable);
}

gboolean
csc_html_get_editable  (const CscHTML *html)
{
	g_return_val_if_fail (html != NULL, FALSE);
	g_return_val_if_fail (GTK_IS_CSCHTML (html), FALSE);

	return html_engine_get_editable (html->engine);
}

void
csc_html_load_empty (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_load_empty (html->engine);
}

/* Editing.  */

void
csc_html_set_paragraph_style (CscHTML *html,
			      CscHTMLParagraphStyle style)
{
	HTMLClueFlowStyle current_style;
	HTMLClueFlowStyle clueflow_style;

	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	/* FIXME precondition: check if it's a valid style.  */

	clueflow_style = paragraph_style_to_clueflow_style (style);

	current_style = html_engine_get_current_clueflow_style (html->engine);
	if (current_style == clueflow_style)
		return;

	if (! html_engine_set_clueflow_style (html->engine, clueflow_style, 0, 0,
					      HTML_ENGINE_SET_CLUEFLOW_STYLE, TRUE))
		return;

	html->paragraph_style = style;

	gtk_signal_emit (GTK_OBJECT (html), signals[CURRENT_PARAGRAPH_STYLE_CHANGED],
			 style);
}

void
csc_html_indent (CscHTML *html,
		 gint delta)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_set_clueflow_style (html->engine, 0, 0, delta,
					HTML_ENGINE_SET_CLUEFLOW_INDENTATION, TRUE);

	update_styles (html);
}

void
csc_html_set_font_style (CscHTML *html,
			 CscHTMLFontStyle and_mask,
			 CscHTMLFontStyle or_mask)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_set_font_style (html->engine, and_mask, or_mask);
}

void
csc_html_align_paragraph (CscHTML *html,
			  CscHTMLParagraphAlignment alignment)
{
	HTMLHAlignType align;

	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	align = paragraph_alignment_to_html (alignment);

	html_engine_set_clueflow_style (html->engine, 0, align, 0,
					HTML_ENGINE_SET_CLUEFLOW_ALIGNMENT, TRUE);
}


/* Clipboard operations.  */

void
csc_html_cut (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_cut (html->engine, TRUE);
}

void
csc_html_copy (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_copy (html->engine);
}

void
csc_html_paste (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_paste (html->engine, TRUE);
}


/* Undo/redo.  */

void
csc_html_undo (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_undo (html->engine);
}

void
csc_html_redo (CscHTML *html)
{
	g_return_if_fail (html != NULL);
	g_return_if_fail (GTK_IS_CSCHTML (html));

	html_engine_redo (html->engine);
}

/* misc utils */

void
csc_html_set_default_background_color (CscHTML *html, GdkColor *c)
{
	html_settings_set_color (html->engine->defaultSettings, HTMLBgColor, c);
}
