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

#ifndef _HTML_ENGINE_EDIT_PASTE_H
#define _HTML_ENGINE_EDIT_PASTE_H

#include "htmlengine.h"

void  html_engine_paste_buffer  (HTMLEngine *engine,
				 GList      *buffer);
void  html_engine_paste_object  (HTMLEngine *engine,
				 HTMLObject *object,
				 gboolean    undo);

void  html_engine_paste         (HTMLEngine *engine,
				 gboolean    do_undo);

#endif /* _HTML_ENGINE_EDIT_PASTE_H */
