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
#ifndef _HTMLLIST_H_
#define _HTMLLIST_H_

#include "htmlobject.h"

typedef enum {
	HTML_LIST_TYPE_UNORDERED,
	HTML_LIST_TYPE_UNORDEREDPLAIN,
	HTML_LIST_TYPE_ORDERED,
	HTML_LIST_TYPE_MENU,
	HTML_LIST_TYPE_DIR
} HTMLListType;

typedef enum {
	HTML_LIST_NUM_TYPE_NUMERIC,
	HTML_LIST_NUM_TYPE_LOWALPHA,
	HTML_LIST_NUM_TYPE_UPALPHA,
	HTML_LIST_NUM_TYPE_LOWROMAN,
	HTML_LIST_NUM_TYPE_UPROMAN
} HTMLListNumType;

struct _HTMLList {
	HTMLListType type;
	HTMLListNumType numType;
	gint itemNumber;
};
typedef struct _HTMLList HTMLList;


HTMLList *html_list_new (HTMLListType t, HTMLListNumType nt);
void html_list_destroy (HTMLList *list);

#endif /* _HTMLLIST_H_ */
