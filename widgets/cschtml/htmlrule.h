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

#ifndef _HTMLRULE_H_
#define _HTMLRULE_H_

#include "htmlobject.h"

typedef struct _HTMLRule HTMLRule;
typedef struct _HTMLRuleClass HTMLRuleClass;

#define HTML_RULE(x) ((HTMLRule *) (x))
#define HTML_RULE_CLASS(x) ((HTMLRuleClass *) (x))

struct _HTMLRule {
	HTMLObject object;

	guint length;
	gchar size;
	gboolean shade;
	HTMLHAlignType halign;
};

struct _HTMLRuleClass {
	HTMLObjectClass object_class;
};


extern HTMLRuleClass html_rule_class;


void        html_rule_type_init   (void);
void        html_rule_class_init  (HTMLRuleClass  *klass,
				   HTMLType        type,
				   guint           object_size);
void        html_rule_init        (HTMLRule       *rule,
				   HTMLRuleClass  *klass,
				   gint            length,
				   gint            percent,
				   gint            size,
				   gboolean        shade,
				   HTMLHAlignType  halign);
HTMLObject *html_rule_new         (gint            length,
				   gint            percent,
				   gint            size,
				   gboolean        shade,
				   HTMLHAlignType  halign);

#endif /* _HTMLRULE_H_ */
