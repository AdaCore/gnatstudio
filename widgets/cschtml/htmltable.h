/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1997 Martin Jones (mjones@kde.org)
              (C) 1997 Torben Weis (weis@kde.org)

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
#ifndef _HTMLTABLE_H_
#define _HTMLTABLE_H_

#include <glib.h>
#include "htmlobject.h"
#include "htmlcluev.h"
#include "htmltablecell.h"

typedef struct _HTMLTable HTMLTable;
typedef struct _HTMLTableClass HTMLTableClass;

#define HTML_TABLE(x) ((HTMLTable *)(x))
#define HTML_TABLE_CLASS(x) ((HTMLTableClass *)(x))

enum _ColumnType {
	COLUMN_TYPE_FIXED,
	COLUMN_TYPE_PERCENT,
	COLUMN_TYPE_VARIABLE
};
typedef enum _ColumnType ColumnType;

typedef struct _ColumnInfo {
	gint startCol;
	gint colSpan;
	gint minSize;
	gint prefSize;
	gint maxSize;
	ColumnType colType;
} ColumnInfo;

typedef struct _RowInfo {
	gint *entry;
	gint nrEntries;
	gint minSize;
	gint prefSize;
} RowInfo;

struct _HTMLTable {
	HTMLObject object;

	gint specified_width;

	gint _minWidth;
	gint _prefWidth;
	HTMLTableCell ***cells;
	gint totalColumnInfos;
	gint col, totalCols;
	gint row, totalRows, allocRows;
	gint spacing;
	gint padding;
	gint border;
	HTMLClueV *caption;
	HTMLVAlignType capAlign;
	
	GArray *colInfo; /* ColumnInfo array */
	GArray *colType; /* ColType array */
	GArray *columnPos; /* integer array */
	GArray *columnPrefPos; /* integer array */
	GArray *columnOpt; /* integer array */
	GArray *colSpan; /* integer array */
	GArray *rowHeights; /* integer array */
};

struct _HTMLTableClass {
	HTMLObjectClass object_class;
};


HTMLTableClass html_table_class;


void        html_table_type_init   (void);
void        html_table_class_init  (HTMLTableClass *klass,
				    HTMLType        type,
				    guint           object_size);
void        html_table_init        (HTMLTable      *table,
				    HTMLTableClass *klass,
				    gint            width,
				    gint            percent,
				    gint            padding,
				    gint            spacing,
				    gint            border);
HTMLObject *html_table_new         (gint            width,
				    gint            percent,
				    gint            padding,
				    gint            spacing,
				    gint            border);

void  html_table_end_row       (HTMLTable     *table);
void  html_table_start_row     (HTMLTable     *table);
void  html_table_add_cell      (HTMLTable     *table,
				HTMLTableCell *cell);
void  html_table_end_table     (HTMLTable     *table);
gint  html_table_add_col_info  (HTMLTable     *table,
				gint           startCol,
				gint           colSpan,
				gint           minSize,
				gint           prefSize,
				gint           maxSize,
				ColumnType     coltype);

#endif /* _HTMLTABLE_H_ */
