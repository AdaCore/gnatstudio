/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* htmlentity.c
 *
 * Copyright (C) 1999  Helix Code, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Author: Ettore Perazzoli
 */

#include <string.h>

#include <glib.h>

#include "htmlentity.h"


struct _EntityEntry {
	guint value;
	const gchar *str;
};
typedef struct _EntityEntry EntityEntry;

static EntityEntry entity_table[] = {
	/*
	 * the 4 absolute ones,
	 */
	{ 34,	"quot" },
	{ 38,	"amp" },
	{ 39,	"apos" },
	{ 60,	"lt" },
	{ 62,	"gt" },

	/*
	 * A bunch still in the 128-255 range
	 * Replacing them depend really on the charset used.
	 */
	{ 160,	"nbsp" },
	{ 161,	"iexcl" },
	{ 162,	"cent" },
	{ 163,	"pound" },
	{ 164,	"curren" },
	{ 165,	"yen" },
	{ 166,	"brvbar" },
	{ 167,	"sect" },
	{ 168,	"uml" },
	{ 169,	"copy" },
	{ 170,	"ordf" },
	{ 171,	"laquo" },
	{ 172,	"not" },
	{ 173,	"shy" },
	{ 174,	"reg" },
	{ 175,	"macr" },
	{ 176,	"deg" },
	{ 177,	"plusmn" },
	{ 178,	"sup2" },
	{ 179,	"sup3" },
	{ 180,	"acute" },
	{ 181,	"micro" },
	{ 182,	"para" },
	{ 183,	"middot" },
	{ 184,	"cedil" },
	{ 185,	"sup1" },
	{ 186,	"ordm" },
	{ 187,	"raquo" },
	{ 188,	"frac14" },
	{ 189,	"frac12" },
	{ 190,	"frac34" },
	{ 191,	"iquest" },
	{ 192,	"Agrave" },
	{ 193,	"Aacute" },
	{ 194,	"Acirc" },
	{ 195,	"Atilde" },
	{ 196,	"Auml" },
	{ 197,	"Aring" },
	{ 198,	"AElig" },
	{ 199,	"Ccedil" },
	{ 200,	"Egrave" },
	{ 201,	"Eacute" },
	{ 202,	"Ecirc" },
	{ 203,	"Euml" },
	{ 204,	"Igrave" },
	{ 205,	"Iacute" },
	{ 206,	"Icirc" },
	{ 207,	"Iuml" },
	{ 208,	"ETH" },
	{ 209,	"Ntilde" },
	{ 210,	"Ograve" },
	{ 211,	"Oacute" },
	{ 212,	"Ocirc" },
	{ 213,	"Otilde" },
	{ 214,	"Ouml" },
	{ 215,	"times" },
	{ 216,	"Oslash" },
	{ 217,	"Ugrave" },
	{ 218,	"Uacute" },
	{ 219,	"Ucirc" },
	{ 220,	"Uuml" },
	{ 221,	"Yacute" },
	{ 222,	"THORN" },
	{ 223,	"szlig" },
	{ 224,	"agrave" },
	{ 225,	"aacute" },
	{ 226,	"acirc" },
	{ 227,	"atilde" },
	{ 228,	"auml" },
	{ 229,	"aring" },
	{ 230,	"aelig" },
	{ 231,	"ccedil" },
	{ 232,	"egrave" },
	{ 233,	"eacute" },
	{ 234,	"ecirc" },
	{ 235,	"euml" },
	{ 236,	"igrave" },
	{ 237,	"iacute" },
	{ 238,	"icirc" },
	{ 239,	"iuml" },
	{ 240,	"eth" },
	{ 241,	"ntilde" },
	{ 242,	"ograve" },
	{ 243,	"oacute" },
	{ 244,	"ocirc" },
	{ 245,	"otilde" },
	{ 246,	"ouml" },
	{ 247,	"divide" },
	{ 248,	"oslash" },
	{ 249,	"ugrave" },
	{ 250,	"uacute" },
	{ 251,	"ucirc" },
	{ 252,	"uuml" },
	{ 253,	"yacute" },
	{ 254,	"thorn" },
	{ 255,	"yuml" }
};


/* FIXME FIXME this function just sucks.  We should use gperf or something instead.  */

gulong
html_entity_parse (const gchar *s, guint len)
{
	guint i;

	for (i = 0; i < sizeof (entity_table) / sizeof (*entity_table); i++) {
		if (len == 0) {
			if (strcmp (entity_table[i].str, s) == 0)
				return entity_table[i].value;
		} else {
			if (strncmp (entity_table[i].str, s, len) == 0)
				return entity_table[i].value;
		}
	}

	return 0;
}

/* prepares text to draw/get_width, returned text is allocated using g_strdup so it could be g_free'ed */
gchar *
html_entity_prepare (const gchar * text)
{
	gchar *nt = g_strdup (text);
	gchar *s  = nt;

	while (s) {
		s = strchr (s, ENTITY_NBSP);
		if (s)
			*s = ' ';
			}

	return nt;
}
