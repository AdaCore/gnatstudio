/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
   Copyright (C) 1999 Red Hat Software
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

   TODO:

     - implement proper animation loading (now it loops thru loaded frames
       and does not stop when loading is in progress and we are out of frames)
     - look at gdk-pixbuf to make gdk_pixbuf_compose work (look also
       on gk_pixbuf_render_to_drawable_alpha)
     - take care about all the frame->action values

*/

#include <glib.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "htmlobject.h"
#include "htmlimage.h"
#include "htmlengine.h"
#include "htmlpainter.h"

#include "cschtml-private.h"
#include "cschtml-stream.h"


/* HTMLImageFactory stuff.  */

struct _HTMLImageFactory {
	HTMLEngine *engine;
	GHashTable *loaded_images;
};


#define DEFAULT_SIZE 48


HTMLImageClass html_image_class;
static HTMLObjectClass *parent_class = NULL;

static HTMLImageAnimation *html_image_animation_new     (HTMLImage *image);
static void                html_image_animation_destroy (HTMLImageAnimation *anim);
static HTMLImagePointer   *html_image_pointer_new       (const char *filename, HTMLImageFactory *factory);
static void                html_image_pointer_destroy   (HTMLImagePointer *ip);

static void render_cur_frame (HTMLImage *image, gint nx, gint ny, const GdkColor *highlight_color);


static guint
get_actual_width (HTMLImage *image,
		  HTMLPainter *painter)
{
	GdkPixbuf *pixbuf = image->image_ptr->pixbuf;
	GdkPixbufAnimation *anim = image->image_ptr->animation;
	gint width;

	if (image->specified_width > 0) {
		width = image->specified_width * html_painter_get_pixel_size (painter);
	} else if (HTML_OBJECT (image)->percent > 0) {
		/* The cast to `gdouble' is to avoid overflow (eg. when
                   printing).  */
		width = ((gdouble) HTML_OBJECT (image)->max_width
			 * HTML_OBJECT (image)->percent) / 100;
	} else if (image->image_ptr == NULL || pixbuf == NULL) {
		width = DEFAULT_SIZE * html_painter_get_pixel_size (painter);
	} else {
		width = (((anim) ? gdk_pixbuf_animation_get_width (anim) : gdk_pixbuf_get_width (pixbuf))
			  * html_painter_get_pixel_size (painter));

		if (image->specified_height > 0) {
			double scale;

			scale =  ((double)image->specified_height) 
				/ ((anim) ? gdk_pixbuf_animation_get_height (anim) : gdk_pixbuf_get_height (pixbuf));
			
			width *= scale;
		}

	}

	return width;
}


static guint
get_actual_height (HTMLImage *image,
		   HTMLPainter *painter)
{
	GdkPixbuf *pixbuf = image->image_ptr->pixbuf;
	GdkPixbufAnimation *anim = image->image_ptr->animation;
	gint height;
		
	if (image->specified_height > 0) {
		height = image->specified_height * html_painter_get_pixel_size (painter);
	} else if (image->image_ptr == NULL || pixbuf == NULL) {
		height = DEFAULT_SIZE * html_painter_get_pixel_size (painter);
	} else {
		height = (((anim) ? gdk_pixbuf_animation_get_height (anim) : gdk_pixbuf_get_height (pixbuf))
			  * html_painter_get_pixel_size (painter));

		if ((image->specified_width > 0) || (HTML_OBJECT(image)->percent > 0)) {
			double scale;
			
			scale = ((double)get_actual_width (image, painter))
				/ (((anim) ? gdk_pixbuf_animation_get_width (anim) : gdk_pixbuf_get_width (pixbuf))
				   * html_painter_get_pixel_size (painter));
			
			height *= scale;
		} 
	}
	
	return height;
}


/* HTMLObject methods.  */

/* FIXME: We should close the stream here, too.  But in practice we cannot
   because the stream pointer might be invalid at this point, and there is no
   way to set it to NULL when the stream is closed.  This clearly sucks and
   must be fixed.  */
static void
destroy (HTMLObject *o)
{
	HTMLImage *image = HTML_IMAGE (o);

	html_image_factory_unregister (image->image_ptr->factory,
				       image->image_ptr, HTML_IMAGE (image));

	if (image->animation)
		html_image_animation_destroy (image->animation);

	g_free (image->url);
	g_free (image->target);

	HTML_OBJECT_CLASS (parent_class)->destroy (o);
}

static void
reset (HTMLObject *self)
{
	HTMLImage *image;
	
	image = HTML_IMAGE (self);
	image->color_allocated = FALSE;

	(* HTML_OBJECT_CLASS (parent_class)->reset) (self);
}

static void
copy (HTMLObject *self,
      HTMLObject *dest)
{
	HTMLImage *dimg = HTML_IMAGE (dest);
	HTMLImage *simg = HTML_IMAGE (self);

	/* FIXME not sure this is all correct.  */

	(* HTML_OBJECT_CLASS (parent_class)->copy) (self, dest);

	dimg->image_ptr = simg->image_ptr;
	dimg->border = simg->border;
	dimg->specified_width = simg->specified_width;
	dimg->specified_height = simg->specified_height;
	dimg->url = g_strdup (simg->url);
	dimg->target = g_strdup (simg->target);
	dimg->color = simg->color;
	dimg->have_color = simg->have_color;
	dimg->color_allocated = FALSE;
	dimg->valign = simg->valign;
	dimg->animation = NULL;          /* don't bother with animation copying now. TODO */
	dimg->hspace = simg->hspace;
	dimg->vspace = simg->vspace;

	/* add dest to image_ptr interests */
	dimg->image_ptr->interests = g_slist_prepend (dimg->image_ptr->interests, dimg);
}

static gint
calc_min_width (HTMLObject *o,
		HTMLPainter *painter)
{
	HTMLImage *image = HTML_IMAGE (o);
	guint pixel_size;
	guint min_width;

	pixel_size = html_painter_get_pixel_size (painter);

	if (o->percent > 0)
		min_width = pixel_size;
	else
		min_width = get_actual_width (HTML_IMAGE (o), painter);

	min_width += (image->border * 2 + 2 * image->hspace) * pixel_size;

	return min_width;
}

static gint
calc_preferred_width (HTMLObject *o,
		      HTMLPainter *painter)
{
	HTMLImage *image = HTML_IMAGE (o);
	guint pixel_size;
	guint width;

	pixel_size = html_painter_get_pixel_size (painter);
	width = get_actual_width (HTML_IMAGE (o), painter)
		+ (image->border * 2 + 2*image->hspace) * pixel_size;

	return width;
}

static gboolean
calc_size (HTMLObject *o,
	   HTMLPainter *painter)
{
	HTMLImage *image;
	guint pixel_size;
	guint width, height;
	gint old_width, old_ascent, old_descent;

	old_width = o->width;
	old_ascent = o->ascent;
	old_descent = o->descent;

	image = HTML_IMAGE (o);

	pixel_size = html_painter_get_pixel_size (painter);

	width = get_actual_width (image, painter);
	height = get_actual_height (image, painter);

	o->width  = width + (image->border + image->hspace) * 2 * pixel_size;
	o->ascent = height + (image->border + image->vspace) * 2 * pixel_size;
	o->descent = 0;

	if (o->descent != old_descent
	    || o->ascent != old_ascent
	    || o->width != old_width)
		return TRUE;

	return FALSE;
}

static void
draw (HTMLObject *o,
      HTMLPainter *painter,
      gint x, gint y,
      gint width, gint height,
      gint tx, gint ty)
{
	HTMLImage *image;
	GdkPixbuf *pixbuf;
	gint base_x, base_y;
	gint scale_width, scale_height;
	const GdkColor *highlight_color;
	guint pixel_size;
	ArtIRect paint;

	html_object_calc_intersection (o, &paint, x, y, width, height);
	if (art_irect_empty (&paint))
		return;

	image = HTML_IMAGE (o);

	pixbuf = image->image_ptr->pixbuf;
	pixel_size = html_painter_get_pixel_size (painter);

	if (pixbuf == NULL) {
		gint vspace, hspace;

		hspace = image->hspace * pixel_size;
		vspace = image->vspace * pixel_size;

		html_painter_draw_panel (painter, 
					 o->x + tx + hspace,
					 o->y + ty - o->ascent + vspace,
					 o->width - 2 * hspace,
					 o->ascent + o->descent - 2 * vspace,
					 CSC_HTML_ETCH_IN, 1);
		return;
	}

	base_x = o->x + tx + (image->border + image->hspace) * pixel_size;
	base_y = o->y + ty + (image->border + image->vspace) * pixel_size - o->ascent;

	scale_width = get_actual_width (image, painter);
	scale_height = get_actual_height (image, painter);

	if (o->selected)
		highlight_color = html_painter_get_default_highlight_color (painter);
	else
		highlight_color = NULL;

	if (image->border) {
		if (image->have_color) {
			if (!image->color_allocated) {
				html_painter_alloc_color (painter, &image->color);
				image->color_allocated = TRUE;
			}
			html_painter_set_pen (painter, &image->color);
		}
		
		html_painter_draw_panel (painter, 
					 base_x - image->border * pixel_size,
					 base_y - image->border * pixel_size,
					 scale_width + (2 * image->border) * pixel_size,
					 scale_height + (2 * image->border) * pixel_size,
					 CSC_HTML_ETCH_NONE, image->border);
		
	}
	if (image->animation) {
		image->animation->active = TRUE;
		image->animation->x = base_x;
		image->animation->y = base_y;
		image->animation->ex = image->image_ptr->factory->engine->x_offset;
		image->animation->ey = image->image_ptr->factory->engine->y_offset;

		render_cur_frame (image, base_x, base_y, highlight_color);
	} else {
		html_painter_draw_pixmap (painter, pixbuf,
					  base_x, base_y,
					  scale_width, scale_height,
					  highlight_color);
	}
}

static gboolean
save (HTMLObject *self,
      HTMLEngineSaveState *state)
{
	HTMLImage *image;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (state != NULL, FALSE);
	
	image = HTML_IMAGE (self);
	
	if (!html_engine_save_output_string (state, "<IMG SRC=\"%s\"", image->image_ptr->url))
	        return FALSE;	

	if (image->specified_width > 0) {
		if (!html_engine_save_output_string (state, " WIDTH=\"%d\"", image->specified_height))
			return FALSE;
	} else if (self->percent) {
		if (!html_engine_save_output_string (state, " WIDTH=\"%d\%\"", self->percent))
			return FALSE;
	}

	/* FIXME percent heights are supported in netscape/mozilla */
	if (image->specified_height > 0) {
		if (!html_engine_save_output_string (state, " HEIGHT=\"%d\"", image->specified_height))
			return FALSE;
	}

	if (image->vspace) {
		if (!html_engine_save_output_string (state, " VSPACE=\"%d\"", image->vspace))
			return FALSE;
	}

	if (image->hspace) {
		if (!html_engine_save_output_string (state, " HSPACE=\"%d\"", image->hspace))
			return FALSE;
	}

	if (image->vspace) {
		if (!html_engine_save_output_string (state, " VSPACE=\"%d\"", image->vspace))
			return FALSE;
	}

	/* FIXME this is the default set in htmlengine.c but there is no real way to tell
	 * if the usr specified it directly
	 */
	if (image->border != 2) {
		if (!html_engine_save_output_string (state, " BORDER=\"%d\"", image->border))
			return FALSE;
	}

	/* FIXME we are not preserving alt tags */
	if (!html_engine_save_output_string (state, ">"))
		return FALSE;
	
	return TRUE;
}

static const gchar *
get_url (HTMLObject *o)
{
	HTMLImage *image;

	image = HTML_IMAGE (o);
	return image->url;
}

static const gchar *
get_target (HTMLObject *o)
{
	HTMLImage *image;

	image = HTML_IMAGE (o);
	return image->target;
}

static gboolean
accepts_cursor (HTMLObject *o)
{
	return TRUE;
}

static HTMLVAlignType
get_valign (HTMLObject *self)
{
	HTMLImage *image;

	image = HTML_IMAGE (self);

	return image->valign;
}


void
html_image_type_init (void)
{
	html_image_class_init (&html_image_class, HTML_TYPE_IMAGE, sizeof (HTMLImage));
}

void
html_image_class_init (HTMLImageClass *image_class,
		       HTMLType type,
		       guint size)
{
	HTMLObjectClass *object_class;

	object_class = HTML_OBJECT_CLASS (image_class);

	html_object_class_init (object_class, type, size);

	object_class->copy = copy;
	object_class->reset = reset;
	object_class->draw = draw;	
	object_class->destroy = destroy;
	object_class->calc_min_width = calc_min_width;
	object_class->calc_preferred_width = calc_preferred_width;
	object_class->calc_size = calc_size;
	object_class->get_url = get_url;
	object_class->get_target = get_target;
	object_class->accepts_cursor = accepts_cursor;
	object_class->get_valign = get_valign;
	object_class->save = save;

	parent_class = &html_object_class;
}

void
html_image_init (HTMLImage *image,
		 HTMLImageClass *klass,
		 HTMLImageFactory *imf,
		 const gchar *filename,
		 const gchar *url,
		 const gchar *target,
		 gint16 width, gint16 height,
		 gint8 percent, gint8 border,
		 const GdkColor *color,
		 HTMLVAlignType valign)
{
	HTMLObject *object;

	g_assert (filename);

	object = HTML_OBJECT (image);

	html_object_init (object, HTML_OBJECT_CLASS (klass));

	image->url = g_strdup (url);
	image->target = g_strdup (target);

	image->specified_width = width;
	image->specified_height = height;
	image->border = border;

	image->color_allocated = FALSE;
	if (color) {
		image->color = *color;
		image->have_color = TRUE;
	}

	image->animation = NULL;

	image->hspace = 0;
	image->vspace = 0;

	if (valign == HTML_VALIGN_NONE)
		valign = HTML_VALIGN_BOTTOM;
	image->valign = valign;

	object->percent = percent;

	image->image_ptr = html_image_factory_register (imf, image, filename);
}

HTMLObject *
html_image_new (HTMLImageFactory *imf,
		const gchar *filename,
		const gchar *url,
		const gchar *target,
		gint16 width, gint16 height,
		gint8 percent, gint8 border,	
		const GdkColor *color,
		HTMLVAlignType valign)
{
	HTMLImage *image;

	image = g_new(HTMLImage, 1);

	html_image_init (image, &html_image_class,
			 imf,
			 filename,
			 url,
			 target,
			 width, height,
			 percent, border, 
			 color,
			 valign);

	return HTML_OBJECT (image);
}

void
html_image_set_spacing (HTMLImage *image, gint hspace, gint vspace)
{
	gboolean changed = FALSE;

	if (image->hspace != hspace) {
		image->hspace = hspace;
		changed = TRUE;
	}

	if (image->vspace != vspace) {
		image->vspace = vspace;
		changed = TRUE;
	}

	if (changed)
		html_engine_schedule_update (image->image_ptr->factory->engine);
}

void
html_image_set_url (HTMLImage *image, const gchar *url)
{
	g_assert (url);
	g_assert (*url);

	if (strcmp (image->image_ptr->url, url)) {
		HTMLImageFactory *imf = image->image_ptr->factory;

		html_image_factory_unregister (imf, image->image_ptr, HTML_IMAGE (image));
		image->image_ptr = html_image_factory_register (imf, image, url);
	}
}

void
html_image_set_valign (HTMLImage *image, HTMLVAlignType valign)
{
	if (image->valign != valign) {
		image->valign = valign;
		html_engine_schedule_update (image->image_ptr->factory->engine);
	}
}

void
html_image_set_border (HTMLImage *image, gint border)
{
	if (image->border != border) {
		image->border = border;
		html_engine_schedule_update (image->image_ptr->factory->engine);
	}
}

void
html_image_set_size (HTMLImage *image, gint w, gint percent, gint h)
{
	gboolean changed = FALSE;

	if (percent != HTML_OBJECT (image)->percent) {
		HTML_OBJECT (image)->percent = percent;
		changed = TRUE;
	}

	if (w != image->specified_width) {
		image->specified_width = w;
		changed = TRUE;
	}

	if (h != image->specified_height) {
		image->specified_height = h;
		changed = TRUE;
	}

	if (changed)
		html_engine_schedule_update (image->image_ptr->factory->engine);
}


static void
html_image_factory_end_pixbuf (CscHTMLStream *stream,
			       CscHTMLStreamStatus status,
			       gpointer user_data)
{
	HTMLImagePointer *ip = user_data;

	html_engine_schedule_update (ip->factory->engine);
	
	g_object_unref (G_OBJECT (ip->loader));
	ip->loader = NULL;
}

static void
html_image_factory_write_pixbuf (CscHTMLStream *stream,
				 const gchar *buffer,
				 guint size,
				 gpointer user_data)
{
	HTMLImagePointer *p = user_data;
	GError *error;

	/* FIXME ! Check return value */
	gdk_pixbuf_loader_write (p->loader, buffer, size, &error);
}

static void
html_image_factory_area_prepared (GdkPixbufLoader *loader, HTMLImagePointer *ip)
{
	if (!ip->animation) {
		GSList *cur;
		HTMLObject *o;

		ip->pixbuf    = gdk_pixbuf_loader_get_pixbuf (ip->loader);
		g_assert (ip->pixbuf);

		/* set change flags on images using this image_ptr */
		cur = ip->interests;
		while (cur) {
			if (cur->data) {
				o = HTML_OBJECT (cur->data);
				html_object_change_set (o, HTML_CHANGE_MIN_WIDTH);
			}
			cur = cur->next;
		}

		gdk_pixbuf_ref (ip->pixbuf);
		html_engine_schedule_update (ip->factory->engine);
	}
}

static void
render_cur_frame (HTMLImage *image, gint nx, gint ny, const GdkColor *highlight_color)
{
	HTMLPainter       *painter;
	HTMLImageAnimation *anim = image->animation;
	GdkPixbufAnimation *ganim = image->image_ptr->animation;
	GTimeVal time = {0, 0};
	GdkPixbufAnimationIter *iter = gdk_pixbuf_animation_get_iter (ganim, &time);
	GdkPixbuf *pixbuf;
	int delay = gdk_pixbuf_animation_iter_get_delay_time (iter);
	gint w, h;

	painter = image->image_ptr->factory->engine->painter;

	do {
		pixbuf = gdk_pixbuf_animation_iter_get_pixbuf (iter);
		w = gdk_pixbuf_get_width (pixbuf);
		h = gdk_pixbuf_get_height (pixbuf);
		html_painter_draw_pixmap (painter, pixbuf,
					  nx,
					  ny,
					  w, h,
					  highlight_color);
		time.tv_sec += delay;
	} while (gdk_pixbuf_animation_iter_advance (iter, &time));
}

static gint
html_image_animation_timeout (HTMLImage *image)
{
	HTMLImageAnimation *anim = image->animation;
	GdkPixbufAnimation *ganim = image->image_ptr->animation;
	HTMLEngine        *engine;
	gint nx, ny, nex, ney;

	anim->cur_frame = anim->cur_frame->next;
	if (!anim->cur_frame)
		// ??? anim->cur_frame = gdk_pixbuf_animation_get_frames (image->image_ptr->animation);
		anim->cur_frame = 0;

	/* FIXME - use gdk_pixbuf_composite instead of render_cur_frame
	   to be more efficient */
	/* render this step to helper buffer */
	/*
	  w = gdk_pixbuf_get_width (gdk_pixbuf_frame_get_pixbuf (frame));
	  h = gdk_pixbuf_get_height (gdk_pixbuf_frame_get_pixbuf (frame));
	
	  if (anim->cur_frame != image->image_ptr->animation->frames) {
		gdk_pixbuf_composite (gdk_pixbuf_frame_get_pixbuf (frame),
				      anim->pixbuf,
				      gdk_pixbuf_frame_get_x_offset (frame),
				      gdk_pixbuf_frame_get_y_offset (frame),
				      w, h,
				      0.0, 0.0, 1.0, 1.0,
				      ART_FILTER_NEAREST, 255);
	} else {
		gdk_pixbuf_copy_area (gdk_pixbuf_frame_get_pixbuf (frame),
				      0, 0, w, h,
				      anim->pixbuf,
				      gdk_pixbuf_frame_get_x_offset (frame),
				      gdk_pixbuf_frame_get_y_offset (frame));
				      } */

	/* draw only if animation is active - onscreen */

	engine = image->image_ptr->factory->engine;

	nex = engine->x_offset;
	ney = engine->y_offset;

	nx = anim->x - (nex - anim->ex);
	ny = anim->y - (ney - anim->ey);
	
	if (anim->active) {
		gint aw, ah;

		aw = gdk_pixbuf_animation_get_width (ganim);
		ah = gdk_pixbuf_animation_get_height (ganim);

		if (MAX(0, nx) < MIN(engine->width, nx+aw)
		    && MAX(0, ny) < MIN(engine->height, ny+ah)) {
			html_engine_draw (engine,
					  nx, ny,
					  aw, ah);
			/* html_engine_queue_draw (engine, HTML_OBJECT (image)); */
			
		}
		
	}

	anim->timeout = g_timeout_add (10, (GtkFunction) html_image_animation_timeout, (gpointer) image);
	//  ??? anim->timeout = g_timeout_add (10 * (gdk_pixbuf_frame_get_delay_time (frame)
	//				     ? gdk_pixbuf_frame_get_delay_time (frame) : 1),

	return FALSE;
}

static void
html_image_animation_start (HTMLImage *image)
{
	HTMLImageAnimation *anim = image->animation;

#if 0  /* ??? */
	if (anim && gdk_pixbuf_animation_get_num_frames (image->image_ptr->animation) > 1) {
		if (anim->timeout == 0) {
			GList *frames = gdk_pixbuf_animation_get_frames (image->image_ptr->animation);

			anim->cur_frame = frames->next;
			anim->cur_n = 1;
			anim->timeout = g_timeout_add (10 * gdk_pixbuf_frame_get_delay_time
						       ((GdkPixbufFrame *) frames->data),
						       (GtkFunction) html_image_animation_timeout, (gpointer) image);
		}
	}
#endif
}

static void
html_image_animation_stop (HTMLImageAnimation *anim)
{
	if (anim->timeout) {
		g_source_remove (anim->timeout);
		anim->timeout = 0;
	}
	anim->active  = 0;
}

HTMLImageFactory *
html_image_factory_new (HTMLEngine *e)
{
	HTMLImageFactory *retval;
	retval = g_new (HTMLImageFactory, 1);
	retval->engine = e;
	retval->loaded_images = g_hash_table_new (g_str_hash, g_str_equal);

	return retval;
}

static gboolean
cleanup_images (gpointer key, gpointer value, gpointer user_data)
{
	HTMLImagePointer *ptr;
	gboolean retval = FALSE;

	ptr = value;

	/* user data means: NULL only clean, non-NULL free */
	if (user_data){
		if (ptr->interests != NULL) {
			g_slist_free (ptr->interests);
			ptr->interests = NULL;
		}
	}

	/* clean only if this image is not used anymore */
	if (!ptr->interests){
		retval = TRUE;
		html_image_pointer_destroy (ptr);
	}

	return retval;
}

void
html_image_factory_cleanup (HTMLImageFactory *factory)
{
	g_return_if_fail (factory);
	g_hash_table_foreach_remove (factory->loaded_images, cleanup_images, NULL);
}

void
html_image_factory_free (HTMLImageFactory *factory)
{
	g_return_if_fail (factory);

	g_hash_table_foreach_remove (factory->loaded_images, cleanup_images, factory);
	g_hash_table_destroy (factory->loaded_images);
	g_free (factory);
}

static HTMLImageAnimation *
html_image_animation_new (HTMLImage *image)
{
	HTMLImageAnimation *animation;

	animation = g_new (HTMLImageAnimation, 1);
	//???animation->cur_frame = gdk_pixbuf_animation_get_frames (image->image_ptr->animation);
	animation->cur_frame = 0;
	animation->cur_n = 0;
	animation->timeout = 0;
	animation->pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8,
					    gdk_pixbuf_animation_get_width (image->image_ptr->animation),
					    gdk_pixbuf_animation_get_height (image->image_ptr->animation));
	animation->active = FALSE;

	return animation;
}

static void
html_image_animation_destroy (HTMLImageAnimation *anim)
{
	html_image_animation_stop (anim);
	gdk_pixbuf_unref (anim->pixbuf);
	g_free (anim);
}

static HTMLImagePointer *
html_image_pointer_new (const char *filename, HTMLImageFactory *factory)
{
	HTMLImagePointer *retval;

	retval = g_new (HTMLImagePointer, 1);
	retval->url = g_strdup (filename);
	retval->loader = gdk_pixbuf_loader_new ();
	retval->pixbuf = NULL;
	retval->animation = NULL;
	retval->interests = NULL;
	retval->factory = factory;

	return retval;
}

static void
html_image_pointer_destroy (HTMLImagePointer *ip)
{
	g_return_if_fail (ip != NULL);

	g_free (ip->url);
	if (ip->loader) {
		gdk_pixbuf_loader_close (ip->loader, NULL);
		g_object_unref (G_OBJECT (ip->loader));
	}
	if (ip->animation) {
		gdk_pixbuf_animation_unref (ip->animation);
	}
	if (ip->pixbuf) {
		gdk_pixbuf_unref (ip->pixbuf);
	}

	g_free (ip);
}

HTMLImagePointer *
html_image_factory_register (HTMLImageFactory *factory, HTMLImage *i, const char *filename)
{
	HTMLImagePointer *retval;

	g_return_val_if_fail (factory, NULL);
	g_return_val_if_fail (filename, NULL);

	retval = g_hash_table_lookup (factory->loaded_images, filename);

	if (!retval){
		CscHTMLStream *handle;

		retval = html_image_pointer_new (filename, factory);
		if (*filename) {
			g_signal_connect (G_OBJECT (retval->loader), "area_prepared",
					  G_CALLBACK (html_image_factory_area_prepared),
					  (void *)retval);

			handle = csc_html_stream_new (CSC_HTML (factory->engine->widget),
						      html_image_factory_write_pixbuf,
						      html_image_factory_end_pixbuf,
						      retval);

			g_hash_table_insert (factory->loaded_images, retval->url, retval);

		/* This is a bit evil, I think.  But it's a lot better here
		   than in the HTMLImage object.  FIXME anyway -- ettore  */
		
			gtk_signal_emit_by_name (GTK_OBJECT (factory->engine), "url_requested", filename,
						 handle);
		}
	}

	/* we add also NULL ptrs, as we dont want these to be cleaned out */
	retval->interests = g_slist_prepend (retval->interests, i);

	if (i) {
		i->image_ptr      = retval;

#if 0  /* ??? */
		if (retval->animation && gdk_pixbuf_animation_get_num_frames (retval->animation) > 1) {
			i->animation = html_image_animation_new (i);
			html_image_animation_start (i);
		}
#endif
	}

	return retval;
}

void
html_image_factory_unregister (HTMLImageFactory *factory, HTMLImagePointer *pointer, HTMLImage *i)
{
	pointer->interests = g_slist_remove (pointer->interests, i);
}

static void
stop_anim (gpointer key, gpointer value, gpointer user_data)
{
	HTMLImagePointer *ip = value;
	GSList *cur = ip->interests;
	HTMLImage *image;

	while (cur) {
		if (cur->data) {
			image = (HTMLImage *) cur->data;
			if (image->animation) {
				html_image_animation_stop (image->animation);
			}
		}
		cur = cur->next;
	}
}

void
html_image_factory_stop_animations (HTMLImageFactory *factory)
{
	g_hash_table_foreach (factory->loaded_images, stop_anim, NULL);
}

static void
deactivate_anim (gpointer key, gpointer value, gpointer user_data)
{
	HTMLImagePointer *ip = value;
	GSList *cur = ip->interests;
	HTMLImage *image;

	while (cur) {
		if (cur->data) {
			image = (HTMLImage *) cur->data;
			if (image->animation) {
				image->animation->active = 0;
			}
		}
		cur = cur->next;
	}
}

void
html_image_factory_deactivate_animations (HTMLImageFactory *factory)
{
	g_hash_table_foreach (factory->loaded_images, deactivate_anim, NULL);
}
