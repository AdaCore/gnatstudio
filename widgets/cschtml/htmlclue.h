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
#ifndef HTMLCLUE_H
#define HTMLCLUE_H

#include "htmlobject.h"

#define HTML_CLUE(x) ((HTMLClue *)(x))
#define HTML_CLUE_CLASS(x) ((HTMLClueClass *)(x))

typedef struct _HTMLClue HTMLClue;
typedef struct _HTMLClueClass HTMLClueClass;

struct _HTMLClue {
	HTMLObject object;
	
	HTMLObject *head;
	HTMLObject *tail;
	HTMLObject *curr;
	
	HTMLVAlignType valign;
	HTMLHAlignType halign;
};

struct _HTMLClueClass {
	HTMLObjectClass object_class;

	gint (*get_left_clear) (HTMLClue *clue, gint y);
	gint (*get_right_clear) (HTMLClue *clue, gint y);
	void (*find_free_area) (HTMLClue *clue, gint y, gint width, gint height, gint indent, gint *y_pos, gint *lmargin, gint *rmargin);
	void (*append_right_aligned) (HTMLClue *clue, HTMLClue *aclue);
	void (*append_left_aligned) (HTMLClue *clue, HTMLClue *aclue);
	gboolean (*appended) (HTMLClue *clue, HTMLClue *aclue);
};


extern HTMLClueClass html_clue_class;


void      html_clue_type_init             (void);
void      html_clue_class_init            (HTMLClueClass *klass,
					   HTMLType       type,
					   guint          object_size);
void      html_clue_init                  (HTMLClue      *clue,
					   HTMLClueClass *klass);
gint      html_clue_get_left_clear        (HTMLClue      *clue,
					   gint           y);
gint      html_clue_get_right_clear       (HTMLClue      *clue,
					   gint           y);
void      html_clue_find_free_area        (HTMLClue      *clue,
					   gint           y,
					   gint           width,
					   gint           height,
					   gint           indent,
					   gint          *y_pos,
					   gint          *lmargin,
					   gint          *rmargin);
void      html_clue_append_right_aligned  (HTMLClue      *clue,
					   HTMLClue      *aclue);
void      html_clue_append_left_aligned   (HTMLClue      *clue,
					   HTMLClue      *aclue);
gboolean  html_clue_appended              (HTMLClue      *clue,
					   HTMLClue      *aclue);
void      html_clue_append_after          (HTMLClue      *clue,
					   HTMLObject    *o,
					   HTMLObject    *where);
void      html_clue_append                (HTMLClue      *clue,
					   HTMLObject    *o);
void      html_clue_prepend               (HTMLClue      *clue,
					   HTMLObject    *o);
void      html_clue_remove                (HTMLClue      *clue,
					   HTMLObject    *o);

#endif /* HTMLCLUE_H */
