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

#ifndef _CSCHTML_H_
#define _CSCHTML_H_

typedef struct _CscHTML	CscHTML;
typedef struct _CscHTMLClass	CscHTMLClass;

#include <sys/types.h>
#include <gtk/gtk.h>

#define GTK_TYPE_CSCHTML (csc_html_get_type())
#define CSC_HTML(obj) (GTK_CHECK_CAST((obj), GTK_TYPE_CSCHTML, CscHTML))
#define GTK_CSCHTML_CLASS(klass) (GTK_CHECK_CLASS_CAST((klass), GTK_TYPE_CSCHTML, CscHTMLClass))
#define GTK_IS_CSCHTML(obj) (GTK_CHECK_TYPE((obj), GTK_TYPE_CSCHTML))
#define GTK_IS_CSCHTML_CLASS(klass) (GTK_CHECK_CLASS_TYPE((klass), GTK_TYPE_CSCHTML))

typedef struct _CscHTMLStream CscHTMLStream;

enum _CscHTMLStreamStatus {
	CSC_HTML_STREAM_OK,
	CSC_HTML_STREAM_ERROR
};
typedef enum _CscHTMLStreamStatus CscHTMLStreamStatus;

enum _CscHTMLParagraphStyle {
	CSC_HTML_PARAGRAPH_STYLE_NORMAL,
	CSC_HTML_PARAGRAPH_STYLE_H1,
	CSC_HTML_PARAGRAPH_STYLE_H2,
	CSC_HTML_PARAGRAPH_STYLE_H3,
	CSC_HTML_PARAGRAPH_STYLE_H4,
	CSC_HTML_PARAGRAPH_STYLE_H5,
	CSC_HTML_PARAGRAPH_STYLE_H6,
	CSC_HTML_PARAGRAPH_STYLE_ADDRESS,
	CSC_HTML_PARAGRAPH_STYLE_PRE,
	CSC_HTML_PARAGRAPH_STYLE_ITEMDOTTED,
	CSC_HTML_PARAGRAPH_STYLE_ITEMROMAN,
	CSC_HTML_PARAGRAPH_STYLE_ITEMDIGIT,
};
typedef enum _CscHTMLParagraphStyle CscHTMLParagraphStyle;

enum _CscHTMLParagraphAlignment {
	CSC_HTML_PARAGRAPH_ALIGNMENT_LEFT,
	CSC_HTML_PARAGRAPH_ALIGNMENT_RIGHT,
	CSC_HTML_PARAGRAPH_ALIGNMENT_CENTER
};
typedef enum _CscHTMLParagraphAlignment CscHTMLParagraphAlignment;

#include "cschtml-embedded.h"

/* FIXME we should hide this stuff.  */
#include "htmlengine.h"
#include "htmlengine-save.h"
#include "cschtmlfontstyle.h"

typedef HTMLEngineSaveReceiverFn CscHTMLSaveReceiverFn;

struct _CscHTML {
	GtkLayout layout;

	HTMLEngine *engine;

	/* The URL of the link over which the pointer currently is.  NULL if
           the pointer is not over a link.  */
	gchar *pointer_url;

	/* The cursors we use within the widget.  */
	GdkCursor *hand_cursor;
	GdkCursor *arrow_cursor;
	GdkCursor *ibeam_cursor;

	gint selection_x1, selection_y1;

	guint in_selection : 1;
	guint button_pressed : 1;
	guint load_in_progress : 1;
	guint allow_font_switch : 1;

	guint debug : 1;
	guint allow_selection : 1;

	guint hadj_connection;
	guint vadj_connection;

	guint idle_handler_id;
	guint scroll_timeout_id;

	CscHTMLParagraphStyle paragraph_style;
	guint paragraph_indentation;
	CscHTMLParagraphAlignment paragraph_alignment;

	CscHTMLFontStyle insertion_font_style;
	gchar *default_font_face;
};

/* must be forward referenced *sigh* */
struct _HTMLEmbedded;

struct _CscHTMLClass {
	GtkLayoutClass parent_class;
	
        void (* title_changed)   (CscHTML *html, const gchar *new_title);
        void (* url_requested)   (CscHTML *html, const gchar *url, CscHTMLStream *handle);
        void (* load_done)       (CscHTML *html);
        void (* link_clicked)    (CscHTML *html, const gchar *url);
	void (* set_base)        (CscHTML *html, const gchar *base_url);
	void (* set_base_target) (CscHTML *html, const gchar *base_url);

	void (* on_url)		 (CscHTML *html, const gchar *url);
	void (* redirect)        (CscHTML *html, const gchar *url, int delay);
	void (* submit)          (CscHTML *html, const gchar *method, const gchar *url, const gchar *encoding);
	gboolean (* object_requested)(CscHTML *html, CscHTMLEmbedded *);

	void (* current_paragraph_style_changed) (CscHTML *html, CscHTMLParagraphStyle new_style);
	void (* current_paragraph_alignment_changed) (CscHTML *html, CscHTMLParagraphAlignment new_alignment);
	void (* current_paragraph_indentation_changed) (CscHTML *html, guint new_indentation);
	void (* insertion_font_style_changed) (CscHTML *html, CscHTMLFontStyle style);
};


/* Creation.  */
GtkType    csc_html_get_type  (void);
GtkWidget *csc_html_new       (void);
void       csc_html_construct (GtkWidget *htmlw);

/* Debugging.  */
void  csc_html_enable_debug  (CscHTML  *html,
			      gboolean  debug);

/* Behavior.  */
void  csc_html_allow_selection  (CscHTML  *html,
				 gboolean  allow);
int   csc_html_request_paste    (GtkWidget *widget,
				 gint32 time);

/* font switching and defaults */
void      csc_html_allow_font_switch(CscHTML *html, gboolean allow);
void      csc_html_set_default_font_face(CscHTML *html, gchar *face);
const gchar *csc_html_get_default_font_face(const CscHTML *html);

/* Loading.  */
CscHTMLStream *csc_html_begin       (CscHTML             *html);
void           csc_html_write       (CscHTML             *html,
				     CscHTMLStream       *handle,
				     const gchar         *buffer,
				     size_t               size);
void           csc_html_end         (CscHTML             *html,
				     CscHTMLStream       *handle,
				     CscHTMLStreamStatus  status);
void           csc_html_load_empty  (CscHTML             *html);

/* Saving.  */
gboolean  csc_html_save  (CscHTML               *html,
			  CscHTMLSaveReceiverFn  receiver,
			  gpointer               data);

gboolean  csc_html_export  (CscHTML               *html,
			    const char            *type,
			    CscHTMLSaveReceiverFn  receiver,
			    gpointer               data);

/* Streams for feeding the widget with extra data (e.g. images) at loading
   time.  */
CscHTMLStream *csc_html_stream_ref    (CscHTMLStream *handle);
void           csc_html_stream_unref  (CscHTMLStream *handle);

/* Editable support.  */
void      csc_html_set_editable  (CscHTML       *html,
				  gboolean       editable);
gboolean  csc_html_get_editable  (const CscHTML *html);


/* Title.  */
const gchar *csc_html_get_title  (CscHTML *html);

/* Anchors.  */
gboolean  csc_html_jump_to_anchor  (CscHTML *html,
				    const gchar *anchor);


/* Editing functions.  */

void  csc_html_set_paragraph_style  (CscHTML                   *html,
				     CscHTMLParagraphStyle      style);
void  csc_html_indent               (CscHTML                   *html,
				     gint                       delta);
void  csc_html_set_font_style       (CscHTML                   *html,
				     CscHTMLFontStyle           and_mask,
				     CscHTMLFontStyle           or_mask);
void  csc_html_align_paragraph      (CscHTML                   *html,
				     CscHTMLParagraphAlignment  alignment);

void  csc_html_cut    (CscHTML *html);
void  csc_html_copy   (CscHTML *html);
void  csc_html_paste  (CscHTML *html);

void  csc_html_undo  (CscHTML *html);
void  csc_html_redo  (CscHTML *html);

/* misc utils */

void  csc_html_set_default_background_color (CscHTML *html, GdkColor *c);

#endif /* _GTKHTML_H_ */
