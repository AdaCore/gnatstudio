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
   MERCHcANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#ifndef _HTMLOBJECT_H_
#define _HTMLOBJECT_H_

#include <glib.h>
#include "art_rect.h"

typedef struct _HTMLObjectClass HTMLObjectClass;
typedef struct _HTMLAnchor HTMLAnchor;
typedef struct _HTMLObject HTMLObject;
typedef struct _HTMLSearch HTMLSearch;

/* FIXME fix ugly dependency on HTMLForm.  */
typedef struct _HTMLForm HTMLForm;
/* FIXME fix ugly dependency on HTMLSelect.  */
typedef struct _HTMLSelect HTMLSelect;
/* FIXME fix ugly dependency on HTMLTextArea.  */
typedef struct _HTMLTextArea HTMLTextArea;

typedef enum {
	HTML_FIT_NONE,
	HTML_FIT_PARTIAL,
	HTML_FIT_COMPLETE
} HTMLFitType;

enum HTMLObjectFlags {
	HTML_OBJECT_FLAG_NONE = 0,
	HTML_OBJECT_FLAG_SEPARATOR = 1 << 0,
	HTML_OBJECT_FLAG_NEWLINE = 1 << 1,
	HTML_OBJECT_FLAG_SELECTED = 1 << 2,
	HTML_OBJECT_FLAG_ALLSELECTED = 1 << 3,
	HTML_OBJECT_FLAG_FIXEDWIDTH = 1 << 4,
	HTML_OBJECT_FLAG_ALIGNED = 1 << 5,
	HTML_OBJECT_FLAG_PRINTED = 1 << 6,
	HTML_OBJECT_FLAG_HIDDEN = 1 << 7
};

typedef enum {
	HTML_CLEAR_NONE,
	HTML_CLEAR_LEFT,
	HTML_CLEAR_RIGHT,
	HTML_CLEAR_ALL
} HTMLClearType;

typedef enum {
	HTML_VALIGN_TOP,
	HTML_VALIGN_CENTER,
	HTML_VALIGN_BOTTOM,
	HTML_VALIGN_NONE
} HTMLVAlignType;

typedef enum {
	HTML_HALIGN_LEFT,
	HTML_HALIGN_CENTER,
	HTML_HALIGN_RIGHT,
	HTML_HALIGN_NONE
} HTMLHAlignType;

typedef enum {
	HTML_CHANGE_NONE = 0,
	HTML_CHANGE_MIN_WIDTH = 1 << 0,
	HTML_CHANGE_ALL = 0x1,
} HTMLChangeFlags;


#include "htmlengine.h"
#include "htmltype.h"
#include "htmlpainter.h"
#include "htmlengine-save.h"


#define HTML_OBJECT(x)		((HTMLObject *) (x))
#define HTML_OBJECT_CLASS(x)	((HTMLObjectClass *) (x))
#define HTML_OBJECT_TYPE(x)     (HTML_OBJECT (x)->klass->type)

typedef void (* HTMLObjectForallFunc) (HTMLObject *self, gpointer data);

struct _HTMLObject {
	HTMLObjectClass *klass;

	/* Pointer to the parent object.  */
	HTMLObject *parent;

	HTMLObject *prev;
	HTMLObject *next;

	HTMLChangeFlags change;

	gint x, y;

	gint ascent, descent;

	gint min_width;
	gint width;
	gint max_width;

	/* With of following segment of objects in ClueFlow.  */
	gint nb_width;

	gint percent;

	guchar flags;

	/* FIXME maybe unify with `flags'?  */
	guint redraw_pending : 1;
	guint selected : 1;

	/* If an object has a redraw pending and is being destroyed, this flag
           is set to TRUE instead of g_free()ing the object.  When the draw
           queue is flushed, the g_free() is performed.  */
	guint free_pending : 1;
};

struct _HTMLObjectClass {
	HTMLType type;

	guint object_size;

	/* Destroy the object.  */
	void (* destroy) (HTMLObject *o);

	/* Copy an object into another one.  @dest can just point to a
           memory area of the proper size.  */
	void (* copy) (HTMLObject *self, HTMLObject *dest);

	/* Layout management and geometry handling.  */

	HTMLFitType (* fit_line) (HTMLObject *o, HTMLPainter *painter,
				  gboolean start_of_line, gboolean first_run,
				  gint width_left);
	gboolean (* calc_size) (HTMLObject *o, HTMLPainter *painter);
	gint (* calc_min_width) (HTMLObject *o, HTMLPainter *painter);
	gint (* calc_preferred_width) (HTMLObject *o, HTMLPainter *painter);
	void (* set_max_ascent) (HTMLObject *o, HTMLPainter *painter, gint a);
	void (* set_max_descent) (HTMLObject *o, HTMLPainter *painter, gint d);
	void (* set_max_width) (HTMLObject *o, HTMLPainter *painter, gint max_width);

	/* Relayout object `o' starting from child `child'.  This
           method can be called by the child when it changes any of
           its layout properties.  */

	gboolean (* layout) (HTMLObject *o, HTMLObject *child);

        /* This method is used to draw the object.  @x & @y are in
	   object coordinates (e.g. the same coordinate system as o->x
	   and o->y). @tx & @ty are used to translate the object
	   coordinates into painter coordinates.  */

	void (* draw) (HTMLObject *o,
		       HTMLPainter *painter,
		       gint x, gint y,
		       gint width, gint height,
		       gint tx, gint ty);

	/* "Transparent" objects (i.e. objects that don't draw all the
           area they occupy, such as text) should return `TRUE' here.  */

	gboolean (* is_transparent) (HTMLObject *self);

	/* This draws the background only.  If the object is
           transparent, it should simply forward the method to the
           parent.  */

	void (* draw_background) (HTMLObject *o,
				  HTMLPainter *painter,
				  gint x, gint y,
				  gint width, gint height,
				  gint tx, gint ty);

	/* Margins.  This should actually be used only by objects that
           contain other objects, so it should be in HTMLClue.  But
           HTMLTable does not derive from HTMLClue and we don't want
           to spend time reorganizing the hierarchy now.  */

	gint (* get_left_margin) (HTMLObject *self, gint y);
	gint (* get_right_margin) (HTMLObject *self, gint y);

	/* Resetting the object.  Do this before using a different
           HTMLPainter.  */

	void (* reset) (HTMLObject *o);

	const gchar * (* get_url) (HTMLObject *o);
	const gchar * (* get_target) (HTMLObject *o);

	HTMLAnchor * (* find_anchor) (HTMLObject *o, const gchar *name, gint *x, gint *y);

	void (* set_bg_color) (HTMLObject *o, GdkColor *color);

	HTMLObject * (* check_point) (HTMLObject *self, HTMLPainter *painter,
				      gint x, gint y, guint *offset_return,
				      gboolean for_cursor);

	/* Relayout this object.  The object will relayout all the children
           starting from `child'.  Children before `child' are not affected.
           The return value is FALSE if nothing has changed during relayout,
           TRUE otherwise.  */
	gboolean (* relayout) (HTMLObject *self, HTMLEngine *engine, HTMLObject *child);

	/* Return the vertical alignment for the object in an HTMLClueFlow.  If
           the returned value is `HTML_VALIGN_BOTTOM', the bottom of the object
           is aligned to the base line; if the value is `HTML_VALIGN_TOP', the
           top of the object is aligned to the top of the line; if the value is
           `HTML_VALIGN_CENTER', the center of the object is aligned to the
           baseline.  */
	HTMLVAlignType (* get_valign) (HTMLObject *self);

	/* Cursor handling.  */

	gboolean (* accepts_cursor) (HTMLObject *self);
	void (* get_cursor) (HTMLObject *self, HTMLPainter *painter, guint offset,
			     gint *x1, gint *y1, gint *x2, gint *y2);
	void (* get_cursor_base) (HTMLObject *self, HTMLPainter *painter, guint offset,
				  gint *x, gint *y);

	/* Container operations.  */

	void (* forall) (HTMLObject *self, HTMLObjectForallFunc func, gpointer data);
	gboolean (* is_container) (HTMLObject *self);

	/* Saving.  */

	gboolean (* save) (HTMLObject *self, HTMLEngineSaveState *state);
	gboolean (* save_plain) (HTMLObject *self, HTMLEngineSaveState *state);

	/* Page splitting (for printing).  */

	gint (* check_page_split) (HTMLObject *self, gint y);

	/* Selection.  */

	gboolean (* select_range) (HTMLObject *self, HTMLEngine *engine, guint start, gint length,
				   gboolean queue_draw);
	HTMLObject * (* get_selection) (HTMLObject *self, guint *size_return);
	void (* append_selection_string) (HTMLObject *self, GString *buffer);

	/* Search & Replace */
	gboolean (* search) (HTMLObject *self, HTMLSearch *info);
	gboolean (* search_next) (HTMLObject *self, HTMLSearch *info);
};


extern HTMLObjectClass html_object_class;


/* Basics.  */
void        html_object_type_init         (void);
void        html_object_init              (HTMLObject           *self,
					   HTMLObjectClass      *klass);
void        html_object_class_init        (HTMLObjectClass      *klass,
					   HTMLType              type,
					   guint                 object_size);
HTMLObject *html_object_new               (HTMLObject           *parent);
void        html_object_destroy           (HTMLObject           *self);
void        html_object_copy              (HTMLObject           *self,
					   HTMLObject           *dest);
HTMLObject *html_object_dup               (HTMLObject           *self);
void        html_object_set_parent        (HTMLObject           *self,
					   HTMLObject           *parent);
gint        html_object_get_left_margin   (HTMLObject           *self,
					   gint                  y);
gint        html_object_get_right_margin  (HTMLObject           *self,
					   gint                  y);
void        html_object_reset             (HTMLObject           *o);
gboolean    html_object_is_text           (HTMLObject           *object);
void        html_object_forall            (HTMLObject           *self,
					   HTMLObjectForallFunc  func,
					   gpointer              data);
gboolean    html_object_is_container      (HTMLObject           *self);
HTMLObject *html_object_next_not_slave    (HTMLObject           *self);
HTMLObject *html_object_prev_not_slave    (HTMLObject           *self);

/* do search request on object using info */
gboolean    html_object_search            (HTMLObject *self,
					   HTMLSearch *info);
gboolean    html_object_search_next       (HTMLObject *self,
					   HTMLSearch *info);

/* Drawing-related stuff.  */
void      html_object_draw             (HTMLObject  *o,
					HTMLPainter *p,
					gint         x,
					gint         y,
					gint         width,
					gint         height,
					gint         tx,
					gint         ty);
void      html_object_draw_background  (HTMLObject  *o,
					HTMLPainter *p,
					gint         x,
					gint         y,
					gint         width,
					gint         height,
					gint         tx,
					gint         ty);
gboolean  html_object_is_transparent   (HTMLObject  *self);
void      html_object_set_bg_color     (HTMLObject  *o,
					GdkColor    *color);

/* Layout.  */
HTMLFitType     html_object_fit_line              (HTMLObject  *o,
						   HTMLPainter *painter,
						   gboolean     start_of_line,
						   gboolean     first_run,
						   gint         width_left);
gboolean        html_object_calc_size             (HTMLObject  *o,
						   HTMLPainter *painter);
void            html_object_set_max_ascent        (HTMLObject  *o,
						   HTMLPainter *painter,
						   gint         a);
void            html_object_set_max_descent       (HTMLObject  *o,
						   HTMLPainter *painter,
						   gint         d);
void            html_object_set_max_width         (HTMLObject  *o,
						   HTMLPainter *painter,
						   gint         max_width);
gint            html_object_calc_min_width        (HTMLObject  *o,
						   HTMLPainter *painter);
gint            html_object_calc_preferred_width  (HTMLObject  *o,
						   HTMLPainter *painter);
void            html_object_calc_abs_position     (HTMLObject  *o,
						   gint        *x_return,
						   gint        *y_return);
void            html_object_calc_intersection     (HTMLObject *o, 
						   ArtIRect *intersection,
						   gint x, gint y, 
						   gint width, gint height);
gboolean        html_object_relayout              (HTMLObject  *obj,
						   HTMLEngine  *engine,
						   HTMLObject  *child);
HTMLVAlignType  html_object_get_valign            (HTMLObject  *self);

/* Links.  */
const gchar *html_object_get_url     (HTMLObject *o);
const gchar *html_object_get_target  (HTMLObject *o);

HTMLAnchor *html_object_find_anchor  (HTMLObject  *o,
				      const gchar *name,
				      gint        *x,
				      gint        *y);

/* Cursor.  */
gboolean    html_object_accepts_cursor   (HTMLObject  *obj);
void        html_object_get_cursor       (HTMLObject  *obj,
					  HTMLPainter *painter,
					  guint        offset,
					  gint        *x1,
					  gint        *y1,
					  gint        *x2,
					  gint        *y2);
void        html_object_get_cursor_base  (HTMLObject  *obj,
					  HTMLPainter *painter,
					  guint        offset,
					  gint        *x,
					  gint        *y);
HTMLObject *html_object_check_point      (HTMLObject  *clue,
					  HTMLPainter *painter,
					  gint         x,
					  gint         y,
					  guint       *offset_return,
					  gboolean     for_cursor);

/* Page splitting.  */
gint  html_object_check_page_split  (HTMLObject *self,
				     gint        y);

/* Selection.  */
gboolean    html_object_select_range             (HTMLObject *self,
						  HTMLEngine *engine,
						  guint       start,
						  gint        length,
						  gboolean    queue_draw);
HTMLObject *html_object_get_selection            (HTMLObject *self,
						  guint      *size_return);
void        html_object_append_selection_string  (HTMLObject *self,
						  GString    *buffer);

/* Saving.  */
gboolean  html_object_save  (HTMLObject          *self,
			     HTMLEngineSaveState *state);

gboolean  html_object_save_plain  (HTMLObject          *self,
				   HTMLEngineSaveState *state);

/* set change flag f of this object and of all its parents */
void  html_object_change_set  (HTMLObject      *self,
			       HTMLChangeFlags  f);

#endif /* _HTMLOBJECT_H_ */
