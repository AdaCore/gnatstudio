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

#include "htmlundo-action.h"


HTMLUndoAction *
html_undo_action_new (const gchar *description,
		      HTMLUndoActionFunction function,
		      HTMLUndoActionClosureDestroyFunction closure_destroy_function,
		      gpointer closure,
		      gint position)
{
	HTMLUndoAction *action;

	g_return_val_if_fail (description != NULL, NULL);
	g_return_val_if_fail (function != NULL, NULL);

	action = g_new (HTMLUndoAction, 1);

	action->description = g_strdup (description);
	action->function = function;
	action->closure_destroy_function = closure_destroy_function;
	action->closure = closure;
	action->position = position;

	return action;
}

void
html_undo_action_destroy (HTMLUndoAction *action)
{
	g_return_if_fail (action != NULL);

	(* action->closure_destroy_function) (action->closure);

	g_free (action->description);
	g_free (action);
}
