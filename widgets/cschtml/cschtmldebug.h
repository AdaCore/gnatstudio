/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

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

#ifndef _CSCHTML_DEBUG_H_
#define _CSCHTML_DEBUG_H_

#include "htmlobject.h"

void  csc_html_debug_log  (CscHTML     *html,
			   const gchar *format,
			   ...);

void  csc_html_debug_dump_tree         (HTMLObject *o,
					gint        level);
void  csc_html_debug_dump_object_type  (HTMLObject *o);
void  csc_html_debug_dump_table        (HTMLObject *o,
					gint        level);
void  csc_html_debug_dump_tree_simple  (HTMLObject *o,
					gint        level);
void  csc_html_debug_dump_list_simple  (GList      *list,
					gint        level);

#endif /* _CSCHTML_DEBUG_H_ */
