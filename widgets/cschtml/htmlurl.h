/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  htmlurl.h

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

    Author: Ettore Perazzoli (ettore@helixcode.com)
*/

#ifndef _HTMLURL_H
#define _HTMLURL_H

#include <glib.h>

struct _HTMLURL {
	gchar *protocol;
	gchar *username;
	gchar *password;
	gchar *hostname;
	guint16 port;
	gchar *path;
	gchar *reference;
};
typedef struct _HTMLURL HTMLURL;


enum _HTMLURLDupFlags {
	HTML_URL_DUP_ALL = 0,
	HTML_URL_DUP_NOPROTOCOL = 1 << 0,
	HTML_URL_DUP_NOUSERNAME = 1 << 1,
	HTML_URL_DUP_NOPASSWORD = 1 << 2,
	HTML_URL_DUP_NOHOSTNAME = 1 << 3,
	HTML_URL_DUP_NOPORT = 1 << 4,
	HTML_URL_DUP_NOPATH = 1 << 5,
	HTML_URL_DUP_NOREFERENCE = 1 << 6,
	HTML_URL_DUP_NOCGIARGS = 1 << 7
};
typedef enum _HTMLURLDupFlags HTMLURLDupFlags;



HTMLURL *html_url_new (const gchar *s);
void html_url_destroy (HTMLURL *url);
HTMLURL *html_url_dup (const HTMLURL *url, HTMLURLDupFlags flags);

void html_url_set_protocol (HTMLURL *url, const gchar *protocol);
void html_url_set_username (HTMLURL *url, const gchar *username);
void html_url_set_password (HTMLURL *url, const gchar *password);
void html_url_set_hostname (HTMLURL *url, const gchar *password);
void html_url_set_port (HTMLURL *url, gushort port);
void html_url_set_path (HTMLURL *url, const gchar *path);
void html_url_set_reference (HTMLURL *url, const gchar *reference);

const gchar *html_url_get_protocol (const HTMLURL *url);
const gchar *html_url_get_username (const HTMLURL *url);
const gchar *html_url_get_password (const HTMLURL *url);
const gchar *html_url_get_hostname (const HTMLURL *url);
gushort html_url_get_port (const HTMLURL *url);
const gchar *html_url_get_path (const HTMLURL *url);
const gchar *html_url_get_reference (const HTMLURL *url);

gchar *html_url_to_string (const HTMLURL *url);

HTMLURL *html_url_append_path (const HTMLURL *url, const gchar *path);

#endif /* _HTMLURL_H */
