/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

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

#ifndef _HTMLENGINE_EDIT_CLUEFLOW_STYLE_H
#define _HTMLENGINE_EDIT_CLUEFLOW_STYLE_H

#include "htmlengine.h"
#include "htmlclueflow.h"

enum _HTMLEngineSetClueFlowStyleMask {
	HTML_ENGINE_SET_CLUEFLOW_NONE = 0,
	HTML_ENGINE_SET_CLUEFLOW_STYLE = 1 << 0,
	HTML_ENGINE_SET_CLUEFLOW_ALIGNMENT = 1 << 1,
	HTML_ENGINE_SET_CLUEFLOW_INDENTATION = 1 << 2
};
typedef enum _HTMLEngineSetClueFlowStyleMask HTMLEngineSetClueFlowStyleMask;


HTMLClueFlowStyle  html_engine_get_current_clueflow_style        (HTMLEngine *engine);
guint              html_engine_get_current_clueflow_indentation  (HTMLEngine *engine);
HTMLHAlignType     html_engine_get_current_clueflow_alignment    (HTMLEngine *engine);

gboolean           html_engine_set_clueflow_style                (HTMLEngine                     *engine,
								  HTMLClueFlowStyle               style,
								  HTMLHAlignType                  alignment,
								  gint                            indentation_delta,
								  HTMLEngineSetClueFlowStyleMask  mask,
								  gboolean                        do_undo);

HTMLClueFlowStyle  html_engine_get_current_clueflow_style        (HTMLEngine *engine);
guint              html_engine_get_current_clueflow_indentation  (HTMLEngine *engine);
HTMLHAlignType     html_engine_get_current_clueflow_alignment    (HTMLEngine *engine);

#endif /* _HTMLENGINE_EDIT_CLUEFLOW_STYLE_H */
