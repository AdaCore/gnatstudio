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

*/

#include "htmlengine.h"

HTMLReplace *
html_replace_new (const gchar *text, void (*ask) (HTMLEngine *, gpointer), gpointer ask_data)
{
	HTMLReplace *nr = g_new (HTMLReplace, 1);

	nr->text     = g_strdup (text);
	nr->replaced = 0;
	nr->ask      = ask;
	nr->ask_data = ask_data;

	return nr;
}

void
html_replace_destroy (HTMLReplace *replace)
{
	g_free (replace->text);
	g_free (replace);
}
