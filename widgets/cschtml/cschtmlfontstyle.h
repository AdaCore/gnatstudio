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

#ifndef _HTMLFONTSTYLE_H
#define _HTMLFONTSTYLE_H

enum _CscHTMLFontStyle {
	CSC_HTML_FONT_STYLE_DEFAULT = 0,
	CSC_HTML_FONT_STYLE_SIZE_1 = 1,
	CSC_HTML_FONT_STYLE_SIZE_2 = 2,
	CSC_HTML_FONT_STYLE_SIZE_3 = 3,
	CSC_HTML_FONT_STYLE_SIZE_4 = 4,
	CSC_HTML_FONT_STYLE_SIZE_5 = 5,
	CSC_HTML_FONT_STYLE_SIZE_6 = 6,
	CSC_HTML_FONT_STYLE_SIZE_7 = 7,
	CSC_HTML_FONT_STYLE_SIZE_MASK = 0x7,
	CSC_HTML_FONT_STYLE_BOLD = 1 << 3,
	CSC_HTML_FONT_STYLE_ITALIC = 1 << 4,
	CSC_HTML_FONT_STYLE_UNDERLINE = 1 << 5,
	CSC_HTML_FONT_STYLE_STRIKEOUT = 1 << 6,
	CSC_HTML_FONT_STYLE_FIXED = 1 << 7,
};
typedef enum _CscHTMLFontStyle CscHTMLFontStyle;

#define CSC_HTML_FONT_STYLE_SIZE_MAX 7
#define CSC_HTML_FONT_STYLE_MAX									\
	(CSC_HTML_FONT_STYLE_SIZE_MASK | CSC_HTML_FONT_STYLE_BOLD | CSC_HTML_FONT_STYLE_ITALIC	\
	 | CSC_HTML_FONT_STYLE_UNDERLINE | CSC_HTML_FONT_STYLE_STRIKEOUT | CSC_HTML_FONT_STYLE_FIXED)

CscHTMLFontStyle csc_html_font_style_merge(CscHTMLFontStyle a, CscHTMLFontStyle b);

#endif /* _HTMLFONTSTYLE_H */
