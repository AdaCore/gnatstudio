/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
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

#include <config.h>

#include "cschtml-stream.h"


CscHTMLStream *
csc_html_stream_new (CscHTML *html,
		     CscHTMLStreamWriteFunc write_func,
		     CscHTMLStreamCloseFunc close_func,
		     gpointer user_data)
{
	CscHTMLStream *new_stream;
	
	new_stream = g_new (CscHTMLStream, 1);

	new_stream->write_func = write_func;
	new_stream->close_func = close_func;

	new_stream->user_data = user_data;
	
	return new_stream;
}

void
csc_html_stream_destroy (CscHTMLStream *stream)
{
	g_return_if_fail (stream != NULL);

	g_free (stream);
}

void
csc_html_stream_write (CscHTMLStream *stream,
		       const gchar *buffer,
		       size_t size)
{
	g_return_if_fail (stream != NULL);
	g_return_if_fail (buffer != NULL);
	g_return_if_fail (size > 0);
	
	if (stream->write_func != NULL)
		stream->write_func (stream, buffer, size, stream->user_data);
}

void
csc_html_stream_close (CscHTMLStream *stream,
		       CscHTMLStreamStatus status)
{
	g_return_if_fail (stream != NULL);
	
	if (stream->close_func != NULL)
		stream->close_func (stream, status, stream->user_data);
	
	csc_html_stream_destroy (stream);
}
