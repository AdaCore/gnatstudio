/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries

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
#ifndef _HTMLTEXT_H_
#define _HTMLTEXT_H_

#include "htmlobject.h"

typedef struct _HTMLText HTMLText;
typedef struct _HTMLTextClass HTMLTextClass;

#define HTML_TEXT(x) ((HTMLText *)(x))
#define HTML_TEXT_CLASS(x) ((HTMLTextClass *)(x))

struct _HTMLText {
	HTMLObject object;
	
	gchar *text;
	guint text_len;

	CscHTMLFontStyle font_style;
	gchar *font_face;
	GdkColor color;

	guint color_allocated : 1;
};

struct _HTMLTextClass {
	HTMLObjectClass object_class;

        guint        	   (* insert_text)    (HTMLText *text, HTMLEngine *engine,
					       guint offset, const gchar *p, guint len);
        guint        	   (* remove_text)    (HTMLText *text, HTMLEngine *engine,
					       guint offset, guint len);
        void         	   (* queue_draw)     (HTMLText *text, HTMLEngine *engine,
					       guint offset, guint len);
        	      
        HTMLText     	 * (* split)          (HTMLText *text, guint offset);
        HTMLText     	 * (* extract_text)   (HTMLText *text, guint offset, gint len);
        void         	   (* merge)          (HTMLText *text, HTMLText *other, gboolean prepend);
        gboolean     	   (* check_merge)    (HTMLText *self, HTMLText *text);

	CscHTMLFontStyle   (* get_font_style) (const HTMLText *text);
	const gchar      * (* get_font_face)  (const HTMLText *text);
	const GdkColor   * (* get_color)      (HTMLText *text, HTMLPainter *painter);

	void               (* set_font_style) (HTMLText *text, HTMLEngine *engine, CscHTMLFontStyle style);
	void               (* set_font_face)  (HTMLText *text, HTMLEngine *engine, const gchar *font_face);
	void               (* set_color)      (HTMLText *text, HTMLEngine *engine, const GdkColor *color);
};

extern HTMLTextClass html_text_class;

void        html_text_type_init    (void);
void        html_text_class_init   (HTMLTextClass    *klass,
				    HTMLType          type,
				    guint             object_size);
void        html_text_init         (HTMLText         *text_object,
				    HTMLTextClass    *klass,
				    const gchar      *text,
				    gint              len,
				    CscHTMLFontStyle  font_style,
				    const GdkColor   *color,
				    const gchar      *font_face);
HTMLObject *html_text_new          (const gchar      *text,
				    CscHTMLFontStyle  font_style,
				    const GdkColor   *color,
				    const gchar      *font_face);
HTMLObject *html_text_new_with_len (const gchar      *text,
				    gint              len,
				    CscHTMLFontStyle  font_style,
				    const GdkColor   *color,
				    const gchar      *font_face);

guint     html_text_insert_text   (HTMLText    *text,
				   HTMLEngine  *engine,
				   guint        offset,
				   const gchar *p,
				   guint        len);
HTMLText *html_text_extract_text  (HTMLText    *text,
				   guint        offset,
				   gint         len);
guint     html_text_remove_text   (HTMLText    *text,
				   HTMLEngine  *engine,
				   guint        offset,
				   guint        len);

void  html_text_queue_draw  (HTMLText   *text,
			     HTMLEngine *engine,
			     guint       offset,
			     guint       len);

HTMLText *html_text_split        (HTMLText  *text,
				  guint      offset);
void      html_text_merge        (HTMLText  *text,
				  HTMLText  *other,
				  gboolean   prepend);
gboolean  html_text_check_merge  (HTMLText  *self,
				  HTMLText  *text);

CscHTMLFontStyle  html_text_get_font_style  (const HTMLText *text);
const gchar *html_text_get_font_face (const HTMLText *text);
const GdkColor   *html_text_get_color       (HTMLText       *text,
					     HTMLPainter    *painter);

void  html_text_set_font_style  (HTMLText             *text,
				 HTMLEngine           *engine,
				 CscHTMLFontStyle      style);
void  html_text_set_font_face   (HTMLText             *text,
		                 HTMLEngine           *engine,
				 const gchar          *font_face);
void  html_text_set_color       (HTMLText             *text,
				 HTMLEngine           *engine,
				 const GdkColor       *color);
void  html_text_set_text        (HTMLText             *text,
				 const gchar          *new_text);

#endif /* _HTMLTEXT_H_ */
