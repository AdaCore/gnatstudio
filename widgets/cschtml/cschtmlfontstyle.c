/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the CscHTML library.

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

#include "cschtmlfontstyle.h"

CscHTMLFontStyle
csc_html_font_style_merge (CscHTMLFontStyle a,
			   CscHTMLFontStyle b)
{
	CscHTMLFontStyle retval;

	if ((b & CSC_HTML_FONT_STYLE_SIZE_MASK) != 0)
		retval = ((b & CSC_HTML_FONT_STYLE_SIZE_MASK)
			  | (a & ~CSC_HTML_FONT_STYLE_SIZE_MASK));
	else
		retval = a;

	retval |= b & ~CSC_HTML_FONT_STYLE_SIZE_MASK;

	return retval;
}
