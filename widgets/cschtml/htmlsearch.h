/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

   Copyright (C) 2000 Helix Code, Inc.
   Authors:           Radek Doulik (rodo@helixcode.com)

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

#ifndef _HTML_SEARCH_H_
#define _HTML_SEARCH_H_

#include <sys/types.h>
#include <regex.h>
#include "htmlobject.h"

struct _HTMLSearch {
	gchar *trans;
	gchar *text;
	guint  text_len;
	guint  found_len;

	gboolean case_sensitive;
	gboolean forward;

	GSList      *stack;
	GList       *found;
	HTMLObject *last;

	guint start_pos;
	guint stop_pos;

	regex_t *reb;        /* regex buffer */
};

HTMLSearch      *html_search_new            (const gchar *text,
					     gboolean case_sensitive,
					     gboolean forward,
					     gboolean regular);
void             html_search_destroy        (HTMLSearch *search);
void             html_search_push           (HTMLSearch *search, HTMLObject *obj);
HTMLObject      *html_search_pop            (HTMLSearch *search);
gboolean         html_search_child_on_stack (HTMLSearch *search, HTMLObject *obj);
gboolean         html_search_next_parent    (HTMLSearch *search);
#endif
