/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library

   Copyright (C) 2000 Helix Code, Inc.

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

#ifndef _HTML_UNDO_H
#define _HTML_UNDO_H

#define HTML_UNDO_LIMIT 1024

typedef struct _HTMLUndo HTMLUndo;

#include "htmlundo-action.h"

struct _HTMLUndo {
	/* List of undo actions (HTMLUndoAction).  */
	GList *undo_stack;
	guint undo_stack_size;

	/* List of redo actions (HTMLUndoAction).  */
	GList *redo_stack;
	guint redo_stack_size;

	/* these lists are stacks containing other
	   levels undo/redo after calling html_undo_level_start */
	GSList *undo_levels;
	GSList *redo_levels;
	guint   undo_levels_size;
};


HTMLUndo *html_undo_new      (void);
void      html_undo_destroy  (HTMLUndo   *undo);

void  html_undo_do_undo  (HTMLUndo   *undo,
			  HTMLEngine *engine);
void  html_undo_do_redo  (HTMLUndo   *undo,
			  HTMLEngine *engine);

void  html_undo_discard_redo  (HTMLUndo *undo);

void  html_undo_add_undo_action  (HTMLUndo       *undo,
				  HTMLUndoAction *action);
void  html_undo_add_redo_action  (HTMLUndo       *undo,
				  HTMLUndoAction *action);

void  html_undo_level_begin (HTMLUndo *undo, const gchar *description);
void  html_undo_level_end   (HTMLUndo *undo);

#endif /* _HTML_UNDO_H */
