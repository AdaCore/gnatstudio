/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/* This file is part of the KDE libraries
    Copyright (C) 1997 Martin Jones (mjones@kde.org)
              (C) 1997 Torben Weis (weis@kde.org)

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
#ifndef _HTMLTEXTSLAVE_H_
#define _HTMLTEXTSLAVE_H_

#include "htmlobject.h"
#include "htmltextmaster.h"

typedef struct _HTMLTextSlave HTMLTextSlave;
typedef struct _HTMLTextSlaveClass HTMLTextSlaveClass;

#define HTML_TEXT_SLAVE(x) ((HTMLTextSlave *) (x))
#define HTML_TEXT_SLAVE_CLASS(x) ((HTMLTextSlaveClass *) (x))

struct _HTMLTextSlave {
	HTMLObject object;

	HTMLTextMaster *owner;
	gshort posStart;
	gshort posLen;
};

struct _HTMLTextSlaveClass {
	HTMLObjectClass object_class;
};


HTMLTextSlaveClass html_text_slave_class;


void        html_text_slave_type_init   (void);
void        html_text_slave_class_init  (HTMLTextSlaveClass *klass,
					 HTMLType            type,
					 guint               object_size);
void        html_text_slave_init        (HTMLTextSlave      *slave,
					 HTMLTextSlaveClass *klass,
					 HTMLTextMaster     *owner,
					 gint                posStart,
					 gint                posLen);
HTMLObject *html_text_slave_new         (HTMLTextMaster     *owner,
					 gint                posStart,
					 gint                posLen);

guint  html_text_slave_get_offset_for_pointer  (HTMLTextSlave *slave,
						HTMLPainter   *painter,
						gint           x,
						gint           y);


gint        html_text_slave_trail_space_width (HTMLTextSlave *slave,
					       HTMLPainter *painter);

#endif /* _HTMLTEXTSLAVE_H_ */
