/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

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

    Author: Ettore Perazzoli <ettore@helixcode.com>
*/

#ifndef _HTMLENGINE_SAVE_H
#define _HTMLENGINE_SAVE_H

typedef gboolean (* HTMLEngineSaveReceiverFn)  (const HTMLEngine *engine,
					        const gchar      *data,
					        guint             len,
						gpointer          user_data);


struct _HTMLEngineSaveState {
	const HTMLEngine *engine;
	HTMLEngineSaveReceiverFn receiver;
	guint br_count;

	guint error : 1;

	guint last_level;

	gpointer user_data;
};
typedef struct _HTMLEngineSaveState HTMLEngineSaveState;


/* Entity encoding.  This is used by the HTML objects to output stuff through
   entity-based encoding.  */
gboolean  html_engine_save_encode         (HTMLEngineSaveState *state,
					   const gchar         *buffer,
					   guint                length);
gboolean  html_engine_save_encode_string  (HTMLEngineSaveState *state,
					   const gchar         *s);

/* Output function (no encoding).  This is used for tags and other things that
   must not be entity-encoded.  */
gboolean  html_engine_save_output_string  (HTMLEngineSaveState *state,
					   const gchar         *format,
					   ...);

/* Saving a whole tree.  */
gboolean  html_engine_save  (const HTMLEngine         *engine,
			     HTMLEngineSaveReceiverFn  receiver,
			     gpointer                  user_data);

gboolean  html_engine_save_plain  (const HTMLEngine         *engine,
				   HTMLEngineSaveReceiverFn  receiver,
				   gpointer                  user_data);

#endif _HTMLENGINE_SAVE_H
