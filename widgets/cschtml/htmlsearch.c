/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

    Copyright (C) 2000 Helix Code, Inc.
    Authors:           Radek Doulik (rodo@helixcode.com)

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

    TODO:

           - now we go thru the html tree without take care about vertical
             position of paragraph. so it is possible to find first match
             on bottom of page (ie. first column of table) and the second
             one on top (ie. top of second comlumn)
	     [also Netscape didn't take care of it]

*/

#include <config.h>
#include "htmlsearch.h"
#include "htmlobject.h"
#include "htmlentity.h"

HTMLSearch *
html_search_new (const gchar *text, gboolean case_sensitive, gboolean forward, gboolean regular)
{
	HTMLSearch *ns = g_new (HTMLSearch, 1);
	gint i;

	ns->text           = g_strdup (text);
	ns->text_len       = strlen (text);
	ns->case_sensitive = case_sensitive;
	ns->forward        = forward;
	ns->stack          = NULL;
	ns->start_pos      = 0;
	ns->found          = NULL;

	/* translate table
	   could translate uppercase to lowercase if non case_sensitive */
	ns->trans = g_new (gchar, 256);
	for (i=0; i<256; i++) {
		ns->trans [i] = (case_sensitive) ? i : ((i>='A' && i<='Z') ? i+('a'-'A') : i);
	}
	ns->trans [ENTITY_NBSP] = ' ';

	if (regular) {
		const gchar *rv;

		ns->reb = g_new0 (regex_t, 1);
#ifdef HAVE_GNU_REGEX
		ns->reb->translate = ns->trans;
		rv = re_compile_pattern (ns->text, ns->text_len, ns->reb);
#else
		rv = regcomp (ns->reb, ns->text, (case_sensitive) ? 0 : REG_ICASE);
#endif
		if (rv) {
			g_warning (rv);
		}
	} else {
		ns->reb = NULL;
	}

	return ns;
}

void
html_search_destroy (HTMLSearch *search)
{
	g_free (search->text);
	if (search->stack)
		g_slist_free (search->stack);
	if (search->reb) {
		/* FIXME why this segfault for me? regfree (search->reb); */
		g_free (search->reb);
	}
	g_free (search->trans);

	g_free (search);
}

void
html_search_push (HTMLSearch *search, HTMLObject *obj)
{
	search->stack = g_slist_prepend (search->stack, obj);
}

HTMLObject *
html_search_pop (HTMLSearch *search)
{
	HTMLObject *obj;

	obj = HTML_OBJECT (search->stack->data);
	search->stack = g_slist_remove (search->stack, obj);

	return obj;
}

gboolean
html_search_child_on_stack (HTMLSearch *search, HTMLObject *obj)
{
	if (!search->stack) {
		return FALSE;
	}

	if (HTML_OBJECT (search->stack->data)->parent != obj) {
		return FALSE;
	}

	return TRUE;
}

gboolean
html_search_next_parent (HTMLSearch *search)
{
	if (search->stack
	    && search->stack->next) {
		return html_object_search (HTML_OBJECT (search->stack->next->data), search);
	}
	return FALSE;
}
