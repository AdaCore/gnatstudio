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

#ifndef _HTMLSTACK_H_
#define _HTMLSTACK_H_

typedef void (* HTMLStackFreeFunc) (gpointer data);

struct _HTMLStack {
	HTMLStackFreeFunc free_func;

	GList *list;
};
typedef struct _HTMLStack HTMLStack;


HTMLStack *html_stack_new (HTMLStackFreeFunc free_func);
void html_stack_clear (HTMLStack *stack);
void html_stack_destroy (HTMLStack *stack);
gpointer html_stack_pop (HTMLStack *stack);
gpointer html_stack_top (HTMLStack *stack);
void html_stack_push (HTMLStack *stack, gpointer data);
gboolean html_stack_is_empty (HTMLStack *stack);
guint html_stack_count (HTMLStack *stack);

#endif /* _HTMLSTACK_H_ */
