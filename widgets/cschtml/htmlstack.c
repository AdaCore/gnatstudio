/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML widget.

    Copyright (C) 1999 Helix Code, Inc.

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

    Author: Ettore Perazzoli <ettore@gnu.org>
*/

#include "htmlengine.h"
#include "htmlstack.h"


HTMLStack *
html_stack_new (HTMLStackFreeFunc free_func)
{
	HTMLStack *new;

	new = g_new (HTMLStack, 1);
	new->free_func = free_func;
	new->list = NULL;

	return new;
}

void
html_stack_clear (HTMLStack *stack)
{
	GList *p;

	if (stack->free_func != NULL)
		for (p = stack->list; p != NULL; p = p->next)
			(* stack->free_func) (p->data);

	g_list_free (stack->list);
	stack->list = NULL;
}

void
html_stack_destroy (HTMLStack *stack)
{
	html_stack_clear (stack);
	g_free (stack);
}

gpointer
html_stack_pop (HTMLStack *stack)
{
	GList *first;
	gpointer data;

	g_return_val_if_fail (! html_stack_is_empty (stack), NULL);

	first = stack->list;
	stack->list = g_list_remove_link (stack->list, first);

	data = first->data;
	g_list_free (first);

	return data;
}

gpointer
html_stack_top (HTMLStack *stack)
{
	if (stack->list == NULL)
		return NULL;

	return stack->list->data;
}

void
html_stack_push (HTMLStack *stack,
		 gpointer data)
{
	stack->list = g_list_prepend (stack->list, data);
}

gboolean
html_stack_is_empty (HTMLStack *stack)
{
	return stack->list == NULL;
}

guint
html_stack_count (HTMLStack *stack)
{
	return g_list_length (stack->list);
}
