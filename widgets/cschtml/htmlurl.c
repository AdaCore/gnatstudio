/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  htmlurl.c

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

    Author: Ettore Perazzoli (ettore@helixcode.com)  */

#include <stdio.h>
#include <string.h>

#include "htmlurl.h"


static gchar *
strdup_nonempty_or_null (const gchar *s)
{
	if (s == NULL)
		return NULL;
	if (*s == '\0')
		return NULL;
	return g_strdup (s);
}

static gchar *
strndup_nonempty_or_null (const gchar *s, guint n)
{
	if (n == 0)
		return NULL;
	if (s == NULL)
		return NULL;
	if (*s == '\0')
		return NULL;
	return g_strndup (s, n);
}

static const gchar *
scan_host_info (HTMLURL *url, const gchar *s)
{
	const gchar *slash_ptr;
	const gchar *at_ptr;
	const gchar *colon_ptr;
	const gchar *host_ptr;
	const gchar *rest_ptr;

	slash_ptr = strchr (s, '/');
	if (slash_ptr == NULL) {
		host_ptr = s;
		rest_ptr = s + strlen (s);
	} else {
		at_ptr = memchr (s, '@', slash_ptr - s);

		if (at_ptr == NULL) {
			/* No `@', so it's going to be a hostname only.  */
			host_ptr = s;
		} else {
			host_ptr = at_ptr + 1;

			/* Check if we have a password.  */

			colon_ptr = memchr (s, ':', host_ptr - s);
			if (colon_ptr == NULL) {
				url->username  = strndup_nonempty_or_null (s, at_ptr - s);
			} else {
				url->username = strndup_nonempty_or_null (s, colon_ptr - s);
				url->password = strndup_nonempty_or_null (colon_ptr + 1,
									  slash_ptr - (colon_ptr + 1));
			}
		}

		rest_ptr = strchr (host_ptr, '/');
		if (rest_ptr == NULL)
			rest_ptr = host_ptr + strlen (host_ptr);
	}

	/* Look for a port number and set the host name.  */

	colon_ptr = memchr (host_ptr, ':', rest_ptr - host_ptr);
	if (colon_ptr == NULL) {
		url->hostname = strndup_nonempty_or_null
			(host_ptr, rest_ptr - host_ptr);
	} else {
		guint port;

		if (sscanf (colon_ptr + 1, "%d", &port) == 1)
			url->port = (guint16) port;

		url->hostname = strndup_nonempty_or_null
			(host_ptr, colon_ptr - host_ptr);
	}

	return rest_ptr;
}

HTMLURL *
html_url_new (const gchar *s)
{
	HTMLURL *new;
	const gchar *p;
	const gchar *path_start;
	const gchar *s_end;
	const gchar *reference_ptr;
	guint s_len;

	new = g_new (HTMLURL, 1);
	new->protocol = NULL;
	new->username = NULL;
	new->password = NULL;
	new->hostname = NULL;
	new->port = 0;
	new->path = NULL;
	new->reference = NULL;

	s_len = strlen (s);
	if (s_len == 0) {
		/* The Path can't be NULL.  */
		if (new->path == NULL)
			new->path = g_strdup ("/");
		return new;
	}

	s_end = s + s_len;

	/* Scan for the protocol part.  */
	/* FIXME I am assuming that the correct regexp for detecting it is
           `^[a-zA-Z0-9]:'.  */

	p = s;
	while ((*p >= 'a' && *p <= 'z')
	       || (*p >= 'A' && *p <= 'Z')
	       || (*p >= '0' && *p <= '9'))
		p++;

	if (*p != ':') {
		/* No protocol.  */
		path_start = s;
	} else {
		new->protocol = strndup_nonempty_or_null (s, p - s);

		/* Check for a host name and a port.  */
		if (p[1] == '/' && p[2] == '/')
			path_start = scan_host_info (new, p + 3);
		else
			path_start = p + 1;
	}

	/* Look for a reference.  */

	reference_ptr = NULL;
	p = s_end;
	while (p != path_start) {
		p--;
		if (*p == '#')
			reference_ptr = p + 1;
	}

	if (reference_ptr != NULL && *reference_ptr != '\0') {
		new->reference = strdup_nonempty_or_null (reference_ptr);
		if (*path_start != '/')
			new->path = g_strconcat ("/", path_start, NULL);
		else
			new->path = g_strndup (path_start,
					       ((reference_ptr - 1)
						- path_start));
	} else {
		new->path = strdup_nonempty_or_null (path_start);
	}

	/* The Path can't be NULL.  */
	if (new->path == NULL)
		new->path = g_strdup ("/");

#if 0
#define STRING_OR_NULL(s) ((s) == NULL ? "(null)" : (s))
	printf ("*** PARSING `%s'\n", s);
	printf ("\tprotocol: %s\n", STRING_OR_NULL (new->protocol));
	printf ("\tusername: %s\n", STRING_OR_NULL (new->username));
	printf ("\tpassword: %s\n", STRING_OR_NULL (new->password));
	printf ("\thostname: %s\n", STRING_OR_NULL (new->hostname));
	printf ("\tport: %u\n", (guint) new->port);
	printf ("\tpath: %s\n", STRING_OR_NULL (new->path));
	printf ("\treference: %s\n", STRING_OR_NULL (new->reference));
#undef STRING_OR_NULL
#endif

	return new;
}

void
html_url_destroy (HTMLURL *url)
{
	g_return_if_fail (url != NULL);

	g_free (url->protocol);
	g_free (url->username);
	g_free (url->password);
	g_free (url->hostname);
	g_free (url->path);

	g_free (url);
}

HTMLURL *
html_url_dup (const HTMLURL *url, HTMLURLDupFlags flags)
{
	HTMLURL *new;
	gchar *ptr;

	if (url == NULL)
		return NULL;

	new = g_new (HTMLURL, 1);

	if (flags & HTML_URL_DUP_NOPROTOCOL)
		new->protocol = NULL;
	else
		new->protocol = g_strdup (url->protocol);

	if (flags & HTML_URL_DUP_NOUSERNAME)
		new->username = NULL;
	else
		new->username = g_strdup (url->username);

	if (flags & HTML_URL_DUP_NOPASSWORD)
		new->password = NULL;
	else
		new->password = g_strdup (url->password);

	if (flags & HTML_URL_DUP_NOHOSTNAME)
		new->hostname = NULL;
	else
		new->hostname = g_strdup (url->hostname);

	if (flags & HTML_URL_DUP_NOPORT)
		new->port = 0;
	else
		new->port = url->port;

	if (flags & HTML_URL_DUP_NOPATH)
		new->path = NULL;
	else
		new->path = g_strdup (url->path);

	if (flags & HTML_URL_DUP_NOCGIARGS && new->path) {
		/* Cut the path after the first '?' */
		ptr = strchr(new->path, '?');
		if (ptr)
			*ptr = 0;
	}

	if (flags & HTML_URL_DUP_NOREFERENCE)
		new->reference = NULL;
	else
		new->reference = g_strdup (url->reference);

	return new;
}


#define SET_STR_FUNC(member)				\
void							\
html_url_set_##member (HTMLURL *url, const gchar *s)	\
{							\
	g_return_if_fail (url != NULL);			\
	g_return_if_fail (s != NULL);			\
							\
	g_free (url->member);				\
	url->member = g_strdup (s);			\
}

SET_STR_FUNC (protocol)
SET_STR_FUNC (username)
SET_STR_FUNC (password)
SET_STR_FUNC (hostname)
SET_STR_FUNC (path)
SET_STR_FUNC (reference)

void
html_url_set_port (HTMLURL *url, gushort port)
{
	g_return_if_fail (url != NULL);

	url->port = port;
}


#define GET_STR_FUNC(member)			\
const gchar *					\
html_url_get_##member (const HTMLURL *url)	\
{						\
	g_return_val_if_fail (url != NULL, 0);	\
						\
	return url->member;			\
}

GET_STR_FUNC (protocol)
GET_STR_FUNC (username)
GET_STR_FUNC (password)
GET_STR_FUNC (hostname)
GET_STR_FUNC (path)
GET_STR_FUNC (reference)

gushort
html_url_get_port (const HTMLURL *url)
{
	g_return_val_if_fail (url != NULL, 0);

	return url->port;
}


gchar *
html_url_to_string (const HTMLURL *url)
{
	guint reference_length;
	guint protocol_length;
	guint hostname_length;
	guint username_length;
	guint password_length;
	guint path_length;
	guint port_length;
	guint length;
	gchar *port_string;
	gchar *s;
	gchar *p;

	g_return_val_if_fail (url != NULL, NULL);

	reference_length = 0;
	protocol_length = 0;
	hostname_length = 0;
	username_length = 0;
	password_length = 0;
	path_length = 0;
	port_length = 0;
	port_string = NULL;

	length = 0;
	if (url->protocol != NULL && url->protocol[0] != '\0') {
		protocol_length = strlen (url->protocol);
		if (protocol_length > 0) {
			length += protocol_length;
			length += 1;	      /* ':' */
		}
	} 

	if (url->hostname != NULL && url->hostname[0] != '\0') {
		hostname_length = strlen (url->hostname);
		length += hostname_length;
		length += 2;	      /* '//' */

		if (url->username != NULL && *url->username != 0) {
			username_length = strlen (url->username);
			length += username_length;
			if (url->password != NULL && *url->password != '\0') {
				password_length = strlen (url->password);
				length += 1; /* ':' */
				length += password_length;
			}
			length += 1;  /* '@' */
		}

		if (url->port != 0) {
			port_string = g_strdup_printf ("%d", url->port);
			port_length = strlen (port_string);
			port_length += 1; /* ':' */
		}
	}

	if (url->path != NULL && url->path[0] != '\0') {
		path_length = strlen (url->path);
		length += path_length;

		if (url->reference != NULL && url->reference[0] != '\0') {

			reference_length = strlen (url->reference);
			length += 1;  /* '#' */
			length += strlen (url->reference);
		}
	}

	length += port_length;

	if (length == 0)
		return g_strdup ("");

	length++;		/* Final zero.  */

	s = g_malloc (length);
	p = s;

#define APPEND_MEMBER(member)					\
	G_STMT_START{						\
		memcpy (p, url->member, member##_length);	\
		p += member##_length;				\
	}G_STMT_END

#define APPEND_CHAR(c)				\
	*(p++) = c;


	if (protocol_length != 0) {
		APPEND_MEMBER (protocol);
		APPEND_CHAR (':');
	}

	if (hostname_length != 0) {
		APPEND_CHAR ('/');
		APPEND_CHAR ('/');

		if (username_length != 0) {
			APPEND_MEMBER (username);
			if (password_length != 0) {
				APPEND_CHAR (':');
				APPEND_MEMBER (password);
			}
			APPEND_CHAR ('@');
		}

		APPEND_MEMBER (hostname);

		if (port_length != 0) {
			APPEND_CHAR (':');
			memcpy (p, port_string, port_length);
			p += port_length - 1;
		}
	}

	/* Notice that the `path' part is always supposed to start with a
           slash, so we don't need to append the slash here.  */

	if (path_length != 0)
		APPEND_MEMBER (path);

	if (reference_length != 0) {
		APPEND_CHAR ('#');
		APPEND_MEMBER (reference);
	}

	*p = 0;

#undef APPEND

	return s;
}


#define PATH_SEP '/'
#define PATH_SEP_STR "/"

static char *
concat_dir_and_file (const char *dir, const char *file)
{
        /* If the directory name doesn't have a / on the end, we need
	   to add one so we get a proper path to the file */
	if (dir [strlen(dir) - 1] != PATH_SEP)
		return g_strconcat (dir, PATH_SEP_STR, file, NULL);
	else
		return g_strconcat (dir, file, NULL);
}

HTMLURL *
html_url_append_path (const HTMLURL *url,
		      const gchar *path)
{
	HTMLURL *new;
	gchar *new_path, *tmppath, *ptr;
	int i;

	new = html_url_dup (url, HTML_URL_DUP_NOPATH);

	g_assert(url->path != NULL);
	
	tmppath = g_strdup(url->path);

	/* Cut the path at the first '?' */
	if((ptr = strchr(tmppath, '?')))
		*ptr = 0;

	i = strlen(tmppath) - 1;

	/* Remove first '/' from the right */
	while(i && tmppath[i] != '/')
		i--;

	if(i)
		tmppath[i] = 0;
	else if(strlen(tmppath) > 1)
		tmppath[i] = 0;

	new_path = concat_dir_and_file (tmppath, path);

	html_url_set_path (new, new_path);
	g_free (new_path);
	g_free (tmppath);

	return new;
}
