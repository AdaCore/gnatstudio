/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.
  
   Copyright (C) 1997 Martin Jones (mjones@kde.org)
   Copyright (C) 1997 Torben Weis (weis@kde.org)
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


#include <string.h>
#include <glib.h>

#include "htmltokenizer.h"
#include "htmlstringtokenizer.h"


HTMLStringTokenizer *
html_string_tokenizer_new (void)
{
	HTMLStringTokenizer *s;
	
	s = g_new (HTMLStringTokenizer, 1);

	s->pos = NULL;
	s->end = NULL;
	s->buffer = NULL;

	s->buffer_length = 0;

	return s;
}

void
html_string_tokenizer_destroy (HTMLStringTokenizer *st)
{
	g_return_if_fail (st != NULL);

	if (st->buffer)
		g_free (st->buffer);
	g_free (st);
}


enum _QuoteType {
	QUOTE_TYPE_NONE,
	QUOTE_TYPE_SINGLE,
	QUOTE_TYPE_DOUBLE
};
typedef enum _QuoteType QuoteType;

void
html_string_tokenizer_tokenize (HTMLStringTokenizer *t,
				const gchar *str,
				gchar *separators)
{
	const gchar *src, *x;
	QuoteType quoted;
	gint str_length;

	if (*str == '\0') {
		t->pos = 0;
		return;
	}
	
	str_length = strlen (str) + 1;
	
	if (t->buffer_length < str_length) {
		g_free (t->buffer);
		t->buffer = g_malloc (str_length);
		t->buffer_length = str_length;
	}

	src = str;
	t->end = t->buffer;

	quoted = QUOTE_TYPE_NONE;
	
	for (; *src != '\0'; src++) {
		x = strchr (separators, *src);
		if (*src == '\"' && !quoted)
			quoted = QUOTE_TYPE_DOUBLE;
		else if (*src == '\'' && !quoted)
			quoted = QUOTE_TYPE_SINGLE;
		else if ((*src == '\"' && quoted == QUOTE_TYPE_DOUBLE)
			 || (*src == '\'' && quoted == QUOTE_TYPE_SINGLE))
			quoted = QUOTE_TYPE_NONE;
		else if (x && !quoted)
			*(t->end)++ = 0;
		else
			*(t->end)++ = *src;
	}

	*(t->end) = 0;
	
	if (t->end - t->buffer <= 1)
		t->pos = NULL; /* No tokens */
	else
		t->pos = t->buffer;
}

gboolean
html_string_tokenizer_has_more_tokens (HTMLStringTokenizer *t)
{
	return (t->pos != NULL);
}

gchar *
html_string_tokenizer_next_token (HTMLStringTokenizer *t)
{
	gchar *ret;

	if (t->pos == NULL)
		return NULL;
	
	ret = t->pos;
	t->pos += strlen (ret) + 1;
	if (t->pos >= t->end)
		t->pos = NULL;
	
	return ret;
}
