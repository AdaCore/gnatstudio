/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries

   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
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

#ifndef _HTMLTEXTMASTER_H_
#define _HTMLTEXTMASTER_H_

#include "htmlobject.h"
#include "htmltext.h"

typedef struct _HTMLTextMaster HTMLTextMaster;
typedef struct _HTMLTextMasterClass HTMLTextMasterClass;

#define HTML_TEXT_MASTER(x) ((HTMLTextMaster *)(x))
#define HTML_TEXT_MASTER_CLASS(x) ((HTMLTextMasterClass *)(x))

struct _HTMLTextMaster {
	HTMLText text;

	guint select_start;
	guint select_length;
};

struct _HTMLTextMasterClass {
	HTMLTextClass text_class;
};

extern HTMLTextMasterClass html_text_master_class;

void        html_text_master_type_init    (void);                                
void        html_text_master_class_init   (HTMLTextMasterClass *class,           
					   HTMLType             type,            
					   guint                object_size);    
void        html_text_master_init         (HTMLTextMaster	*master,          
					   HTMLTextMasterClass	*klass,           
					   const gchar		*text,            
					   gint			len,             
					   CscHTMLFontStyle	font_style,      
					   const GdkColor	*color,
					   const gchar		*font_face);          
HTMLObject *html_text_master_new          (const gchar		*text,
					   CscHTMLFontStyle	font_style,
					   const GdkColor	*color,
					   const gchar		*font_face);
HTMLObject *html_text_master_new_with_len (const gchar		*text,
					   gint			len,
					   CscHTMLFontStyle	font_style,     
					   const GdkColor	*color,
					   const gchar		*font_face);
HTMLObject *html_text_master_get_slave    (HTMLTextMaster      *master,          
					   guint                offset);

gint        html_text_master_trail_space_width (HTMLTextMaster	*master,
						HTMLPainter	*painter);

void  html_text_master_destroy_slaves  (HTMLTextMaster *master);

#endif /* _HTMLTEXTMASTER_H_ */
