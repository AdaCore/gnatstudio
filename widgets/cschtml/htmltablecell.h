/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of CscHTML.

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

#ifndef _HTMLTABLECELL_H_
#define _HTMLTABLECELL_H_

#include "htmlcluev.h"
#include "htmlimage.h"


typedef struct _HTMLTableCell HTMLTableCell;
typedef struct _HTMLTableCellClass HTMLTableCellClass;

#define HTML_TABLE_CELL(x) ((HTMLTableCell *)(x))
#define HTML_TABLE_CELL_CLASS(x) ((HTMLTableCellClass *)(x))

struct _HTMLTableCell {
	HTMLClueV cluev;

	gint fixed_width;

	gint rspan;
	gint cspan;
	gint padding;
	gint refcount;

	GdkColor bg;
	guint have_bg : 1;
	guint bg_allocated : 1;

	HTMLImagePointer *bgPixmap;
	gboolean have_bgPixmap;
};

struct _HTMLTableCellClass {
	HTMLClueVClass cluev_class;
};


extern HTMLTableCellClass html_table_cell_class;


void        html_table_cell_type_init        (void);
void        html_table_cell_class_init       (HTMLTableCellClass *klass,
					      HTMLType            type,
					      guint               object_size);
void        html_table_cell_init             (HTMLTableCell      *cell,
					      HTMLTableCellClass *klass,
					      gint                percent,
					      gint                rs,
					      gint                cs,
					      gint                pad);
HTMLObject *html_table_cell_new              (gint                percent,
					      gint                rs,
					      gint                cs,
					      gint                pad);
void        html_table_cell_link             (HTMLTableCell      *cell);
void        html_table_cell_unlink           (HTMLTableCell      *cell);
void        html_table_cell_set_fixed_width  (HTMLTableCell      *cell,
					      gint                width);
void        html_table_cell_set_bg_pixmap    (HTMLTableCell      *cell,
					      HTMLImagePointer   *imagePtr);

#endif /* _HTMLTABLECELL_H_ */
