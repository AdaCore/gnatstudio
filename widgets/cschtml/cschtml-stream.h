/*  This file is part of the CscHTML library.

    Copyright 1999, 2000 Helix Code, Inc.
    
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

#ifndef _CSCHTML_STREAM_H
#define _CSCHTML_STREAM_H

#include <glib.h>

#include "cschtml.h"

typedef void (* CscHTMLStreamCloseFunc) (CscHTMLStream *stream,
					 CscHTMLStreamStatus status,
					 gpointer user_data);
typedef void (* CscHTMLStreamWriteFunc) (CscHTMLStream *stream,
					 const gchar *buffer,
					 guint size,
					 gpointer user_data);

struct _CscHTMLStream {
	CscHTMLStreamWriteFunc write_func;
	CscHTMLStreamCloseFunc close_func;
	gpointer user_data;
};

CscHTMLStream *csc_html_stream_new      (CscHTML                *html,
					 CscHTMLStreamWriteFunc  write_func,
					 CscHTMLStreamCloseFunc  close_func,
					 gpointer                user_data);
void           csc_html_stream_write    (CscHTMLStream          *stream,
					 const gchar            *buffer,
					 size_t                  size);
void           csc_html_stream_destroy  (CscHTMLStream          *stream);
void           csc_html_stream_close    (CscHTMLStream          *stream,
					 CscHTMLStreamStatus     status);

#endif /* _CSCHTML_STREAM_H */
