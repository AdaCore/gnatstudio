/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
   Copyright (C) 1999, 2000 Helix Code, Inc.
   
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

#ifndef _HTMLENGINE_H_
#define _HTMLENGINE_H_

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>


typedef struct _HTMLEngine HTMLEngine;
typedef struct _HTMLEngineClass HTMLEngineClass;
#include "cschtml.h"

#include "htmltokenizer.h"
#include "htmlcolorset.h"
#include "htmlcursor.h"
#include "htmldrawqueue.h"
#include "htmlstack.h"
#include "htmlsettings.h"
#include "htmlpainter.h"
#include "htmlgdkpainter.h"
#include "htmlundo.h"
#include "htmlstringtokenizer.h"
#include "htmlengine-edit-selection-updater.h"
#include "htmlreplace.h"

#include "cschtml-embedded.h"


#define HTML_TYPE_ENGINE                 (html_engine_get_type ())
#define HTML_ENGINE(obj)                 (GTK_CHECK_CAST ((obj), HTML_TYPE_ENGINE, HTMLEngine))
#define HTML_ENGINE_CLASS(klass)         (GTK_CHECK_CLASS_CAST ((klass), HTML_TYPE_ENGINE, HTMLEngineClass))
#define HTML_IS_ENGINE(obj)              (GTK_CHECK_TYPE ((obj), HTML_TYPE_ENGINE))
#define HTML_IS_ENGINE_CLASS(klass)      (GTK_CHECK_CLASS_TYPE ((klass), HTML_TYPE_ENGINE))


/* FIXME extreme hideous ugliness in the following lines.  */

#define LEFT_BORDER 10
#define RIGHT_BORDER 10
#define TOP_BORDER 10
#define BOTTOM_BORDER 10

enum _HTMLGlossaryEntry {
	HTML_GLOSSARY_DL = 1,
	HTML_GLOSSARY_DD = 2
};
typedef enum _HTMLGlossaryEntry HTMLGlossaryEntry;

typedef struct _HTMLBlockStackElement HTMLBlockStackElement;


/* FIXME this needs splitting.  */

struct _HTMLEngine {
	GtkObject parent;

	HTMLColorSet *color_set;
	HTMLDrawQueue *draw_queue;

	HTMLPainter *painter;

	HTMLSettings *settings;
	HTMLSettings *defaultSettings;

	HTMLUndo *undo;

	GdkWindow *window;
	GdkGC *invert_gc;

	gboolean editable;
	GList *cut_buffer;

	/* Freeze counter.  When greater than zero, we never trigger relayouts
           nor repaints.  When going from nonzero to zero, we relayout and
           repaint everything.  */
	guint freeze_count;

	gboolean parsing;
	HTMLTokenizer *ht;
	HTMLStringTokenizer *st;
	HTMLObject *clue;
	
	HTMLObject *flow;

	gint leftBorder;
	gint rightBorder;
	gint topBorder;
	gint bottomBorder;

	/* Current indentation level.  */
	guint indent_level;

	/* For the widget */
	gint width;
	gint height;

	HTMLHAlignType divAlign;

	/* Number of tokens parsed in the current time-slice */
	gint parseCount;
	gint granularity;

	/* Offsets */
	gint x_offset, y_offset;

	gboolean inTitle;
	gboolean inPre;
	gboolean inOption;
	gboolean inTextArea;

	gboolean newPage;
 
	HTMLStack *font_style_stack; /* Font style stack, elements are CscHTMLFontStyles.  */
	HTMLStack *font_face_stack;	/* Font face stack, elements are gchar * */
	HTMLStack *color_stack;	/* Color stack, elements are GdkColors.  */
	HTMLStack *clueflow_style_stack; /* Clueflow style stack, elements are HTMLClueFlowStyles.  */

	gchar *url;
	gchar *target;

	HTMLBlockStackElement *blockStack;

	/* timer id to schedule paint events */
	guint updateTimer;

	/* timer id for parsing routine */
	guint timerId;

	/* Should the background be painted? */
	gboolean bDrawBackground;

	/* FIXME: replace with a `gchar *'?  */
	GString *title;

	gboolean writing;

	/* The background pixmap, an HTMLImagePointer */
        gpointer bgPixmapPtr;
  
	/* Stack of lists currently active */
	HTMLStack *listStack;

	/* Stack of embedded "object"'s */
	HTMLStack *embeddedStack;

	/* The associated widget.  */
	CscHTML *widget;

        gpointer image_factory;

	HTMLStack *glossaryStack; /* HTMLGlossaryEntry */

	/*
	 * This list holds strings which are displayed in the view,
	 * but are not actually contained in the HTML source.
	 * e.g. The numbers in an ordered list.
	 * FIXME?
	 */
	GList *tempStrings;

	HTMLForm *form;
	HTMLSelect *formSelect;
	HTMLTextArea *formTextArea;
	GList *formList;
	GString *formText;
	gboolean noWrap;

	/* This is TRUE if we cannot insert a paragraph break (which is just an
           extra empty line).  It's set to FALSE as soon as some element is
           added to a flow.  The purpose is to avoid having paragraph breaks to
           be inserted in sequence, or after elements that have some vspace of
           their own.  */
	gboolean avoid_para;

	/* This is TRUE if we want a paragraph break to be inserted before the
           next element.  */
	gboolean pending_para;

	/* Alignment for the pending paragraph we are going to insert.  */
	HTMLHAlignType pending_para_alignment;

	/* Whether we have the keyboard focus.  */
	guint have_focus : 1;

	/* --- */

	/* Editing stuff.  -- FIXME it should be in a separate object.  */

	/* The current position of the cursor.  */
	HTMLCursor *cursor;

	/* If no region is active, this is NULL.  Otherwise, this is
           one extreme of the selected region.  The other extreme is
           always the cursor.  */
	HTMLCursor *mark;

	/* Hide counter for the cursor.  When greater than zero, it
           means the cursor is invisible.  */
	gint cursor_hide_count;

	/* Timer ID for cursor blink.  */
	gint blinking_timer_id;

	/* Blinking status (visible/invisible).  */
	gboolean blinking_status;

	/* Font style for insertion.  If HTML_FONT_STYLE_DEFAULT, use that of
           the text we are in.  */
	CscHTMLFontStyle insertion_font_style;
	gchar *insertion_font_face;
	
	/* This is set to TRUE when at least one element is selected (in whole
           or in part), to FALSE when no item is selected at all.  */
	gboolean active_selection;

	/* This object is used to update the keyboard selection in the
           idle loop.  */
	HTMLEngineEditSelectionUpdater *selection_updater;

	/* search & replace */
	HTMLSearch  *search_info;
	HTMLReplace *replace_info;
};

/* must be forward referenced *sigh* */
struct _HTMLEmbedded;

struct _HTMLEngineClass {
	GtkObjectClass parent_class;
	
	void (* title_changed) (HTMLEngine *engine);
	void (* set_base) (HTMLEngine *engine, const gchar *base);
	void (* set_base_target) (HTMLEngine *engine, const gchar *base_target);
	void (* load_done) (HTMLEngine *engine);
        void (* url_requested) (HTMLEngine *engine, const char *url, CscHTMLStream *handle);
	void (* draw_pending) (HTMLEngine *engine);
        void (* redirect) (HTMLEngine *engine, const char *url, int delay);
        void (* submit) (HTMLEngine *engine, const gchar *method, const gchar *action, const gchar *encoding);
	gboolean (* object_requested) (HTMLEngine *engine, CscHTMLEmbedded *);
};


/* Object construction.  */
guint       html_engine_get_type      (void);
HTMLEngine *html_engine_new           (void);
void        html_engine_realize       (HTMLEngine *engine,
				       GdkWindow  *window);

/* Editability control.  */
void      html_engine_set_editable  (HTMLEngine *e,
				     gboolean    editable);
gboolean  html_engine_get_editable  (HTMLEngine *e);

/* Focus.  */
void  html_engine_set_focus  (HTMLEngine *engine,
			      gboolean    have_focus);

/* Parsing control.  */
CscHTMLStream *html_engine_begin            (HTMLEngine  *p);
void           html_engine_parse            (HTMLEngine  *p);
void           html_engine_stop_parser      (HTMLEngine  *e);

/* Rendering control.  */
void  html_engine_calc_size            (HTMLEngine *p);
gint  html_engine_get_doc_height       (HTMLEngine *p);
gint  html_engine_get_doc_width        (HTMLEngine *e);
void  html_engine_draw                 (HTMLEngine *e,
					gint        x,
					gint        y,
					gint        width,
					gint        height);
void  html_engine_draw_background      (HTMLEngine *e,
					gint        x,
					gint        y,
					gint        width,
					gint        height);

/* Scrolling.  */
void      html_engine_schedule_update      (HTMLEngine  *p);
void      html_engine_make_cursor_visible  (HTMLEngine  *e);
gboolean  html_engine_goto_anchor          (HTMLEngine  *e,
					    const gchar *anchor);

/* Draw/clear queue.  */
void  html_engine_flush_draw_queue  (HTMLEngine *e);
void  html_engine_queue_draw        (HTMLEngine *e,
				     HTMLObject *o);
void  html_engine_queue_clear       (HTMLEngine *e,
				     gint        x,
				     gint        y,
				     guint       width,
				     guint       height);

/* Getting objects through pointer positions.  */
HTMLObject  *html_engine_get_object_at  (HTMLEngine *e,
					 gint        x,
					 gint        y,
					 guint      *offset_return,
					 gboolean    for_cursor);
const gchar *html_engine_get_link_at    (HTMLEngine *e,
					 gint        x,
					 gint        y);

/* Form support.  */
void  html_engine_form_submitted  (HTMLEngine  *engine,
				   const gchar *method,
				   const gchar *action,
				   const gchar *encoding);

/* Misc.  (FIXME: Should die?) */
gchar *html_engine_canonicalize_url  (HTMLEngine *e,
				      const char *in_url);

/* Selection.  (FIXME: Maybe `htmlengine-edit' instead?)  */
void   html_engine_select_region         (HTMLEngine *e,
					  gint        x1,
					  gint        y1,
					  gint        x2,
					  gint        y2,
					  gboolean    queue_draw);
void   html_engine_unselect_all          (HTMLEngine *e,
					  gboolean    queue_draw);
void   html_engine_disable_selection     (HTMLEngine *e);
gchar *html_engine_get_selection_string  (HTMLEngine *e);

/* Freezing/thawing.  */
gboolean  html_engine_frozen  (HTMLEngine *engine);
void      html_engine_freeze  (HTMLEngine *engine);
void      html_engine_thaw    (HTMLEngine *engine);

/* Creating an empty document.  */
void      html_engine_load_empty                (HTMLEngine *engine);

/* Search & Replace */
gboolean  html_engine_search                    (HTMLEngine *e,
						 const gchar *text,
						 gboolean case_sensitive,
						 gboolean forward,
						 gboolean regular);
gboolean  html_engine_search_next               (HTMLEngine *e);
gboolean  html_engine_search_incremental        (HTMLEngine *e);

void      html_engine_replace                   (HTMLEngine *e,
						 const gchar *text,
						 const gchar *rep_text,
						 gboolean case_sensitive,
						 gboolean forward,
						 gboolean regular,
						 void (*ask)(HTMLEngine *, gpointer), gpointer ask_data);
void      html_engine_replace_do                (HTMLEngine *e, HTMLReplaceQueryAnswer answer);
gint      html_engine_replaced                  (void);

#endif /* _HTMLENGINE_H_ */
