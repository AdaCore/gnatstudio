/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* htmltype.h

   Copyright (C) 1999, 2000 Helix Code, Inc.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Ettore Perazzoli <ettore@comm2000.it>
*/

#include "htmltype.h"

#include "htmlanchor.h"
#include "htmlbutton.h"
#include "htmlcheckbox.h"
#include "htmlclue.h"
#include "htmlcluealigned.h"
#include "htmlclueflow.h"
#include "htmlclueh.h"
#include "htmlcluev.h"
#include "htmlembedded.h"
#include "htmlhidden.h"
#include "htmliframe.h"
#include "htmlimage.h"
#include "htmlimageinput.h"
#include "htmllinktext.h"
#include "htmllinktextmaster.h"
#include "htmlobject.h"
#include "htmlradio.h"
#include "htmlrule.h"
#include "htmlselect.h"
#include "htmltable.h"
#include "htmltablecell.h"
#include "htmltext.h"
#include "htmltextarea.h"
#include "htmltextinput.h"
#include "htmltextmaster.h"
#include "htmltextslave.h"
#include "htmlvspace.h"


/* This is used to statically initialize all the classes we are using.  */

static gboolean types_inited = FALSE;

void
html_types_init (void)
{
	if (types_inited)
		return;

	html_anchor_type_init ();
	html_button_type_init();
	html_checkbox_type_init ();
	html_clue_type_init ();
	html_cluealigned_type_init ();
	html_clueflow_type_init ();
	html_clueh_type_init ();
	html_cluev_type_init ();
	html_embedded_type_init ();
	html_hidden_type_init ();
	html_image_type_init ();
	html_imageinput_type_init ();
	html_link_text_type_init ();
	html_link_text_master_type_init ();
	html_object_type_init ();
	html_radio_type_init ();
	html_rule_type_init ();
	html_select_type_init ();
	html_table_cell_type_init ();
	html_table_type_init ();
	html_textarea_type_init ();
	html_text_input_type_init ();
	html_text_master_type_init ();
	html_text_slave_type_init ();
	html_text_type_init ();
	html_vspace_type_init ();
	html_iframe_type_init ();

	types_inited = TRUE;
}


const gchar *
html_type_name (HTMLType type)
{
	g_return_val_if_fail (type != HTML_TYPE_NONE, NULL);
	g_return_val_if_fail (type < HTML_NUM_TYPES, NULL);

	switch (type) {
	case HTML_TYPE_ANCHOR:
		return "Anchor";
	case HTML_TYPE_BULLET:
 		return "Bullet";
	case HTML_TYPE_BUTTON:
 		return "Button";
	case HTML_TYPE_CHECKBOX:
 		return "CheckBox";
	case HTML_TYPE_CLUE:
 		return "Clue";
	case HTML_TYPE_CLUEALIGNED:
 		return "ClueAligned";
	case HTML_TYPE_CLUEFLOW:
 		return "ClueFlow";
	case HTML_TYPE_CLUEH:
 		return "ClueH";
	case HTML_TYPE_CLUEV:
 		return "ClueV";
	case HTML_TYPE_EMBEDDED:
 		return "Embedded";
	case HTML_TYPE_HIDDEN:
 		return "Hidden";
	case HTML_TYPE_HSPACE:
 		return "HSpace";
	case HTML_TYPE_IMAGE:
 		return "Image";
	case HTML_TYPE_IMAGEINPUT:
 		return "ImageInput";
	case HTML_TYPE_LINKTEXT:
 		return "LinkText";
	case HTML_TYPE_LINKTEXTMASTER:
 		return "LinkTextMaster";
	case HTML_TYPE_OBJECT:
 		return "Object";
	case HTML_TYPE_RADIO:
 		return "Radio";
	case HTML_TYPE_RULE:
 		return "Rule";
	case HTML_TYPE_SELECT:
 		return "Select";
	case HTML_TYPE_TABLE:
 		return "Table";
	case HTML_TYPE_TABLECELL:
 		return "TableCell";
	case HTML_TYPE_TEXT:
 		return "Text";
	case HTML_TYPE_TEXTAREA:
 		return "TextArea";
	case HTML_TYPE_TEXTINPUT:
 		return "TextInput";
	case HTML_TYPE_TEXTMASTER:
 		return "TextMaster";
	case HTML_TYPE_TEXTSLAVE:
 		return "TextSlave";
	case HTML_TYPE_VSPACE:
 		return "VSpace";
	case HTML_TYPE_IFRAME:
		return "IFrame";
	case HTML_TYPE_NONE:
 	case HTML_NUM_TYPES:
		/* Make compiler happy.  */
		g_assert_not_reached ();
	}

	g_assert_not_reached ();
	return NULL;
}

