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
#ifndef _HTMLVSPACE_H_
#define _HTMLVSPACE_H_

#include "htmlobject.h"

typedef struct _HTMLVSpace HTMLVSpace;
typedef struct _HTMLVSpaceClass HTMLVSpaceClass;

#define HTML_VSPACE(x) ((HTMLVSpace *)(x))

struct _HTMLVSpace {
	HTMLObject object;
	HTMLClearType clear;
};

struct _HTMLVSpaceClass {
	HTMLObjectClass object_class;
};


extern HTMLVSpaceClass html_vspace_class;


void        html_vspace_type_init   (void);
void        html_vspace_class_init  (HTMLVSpaceClass *klass,
				     HTMLType         type,
				     guint            object_size);
void        html_vspace_init        (HTMLVSpace      *vspace,
				     HTMLVSpaceClass *klass,
				     HTMLClearType    clear);
HTMLObject *html_vspace_new         (HTMLClearType    clear);

#endif /* _HTMLVSPACE_H_ */
