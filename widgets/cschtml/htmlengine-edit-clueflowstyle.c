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

#include "htmlengine-edit-clueflowstyle.h"


/* Properties of a paragraph.  */
struct _ClueFlowProps {
	HTMLClueFlowStyle style;
	HTMLHAlignType alignment;
	guint8 indentation;
};
typedef struct _ClueFlowProps ClueFlowProps;

/* Data for the redo/undo operation.  */
struct _ClueFlowStyleOperation {
	/* Reference count.  */
	guint ref_count;
	
	/* Whether this is an undo or a redo operation.  */
	gboolean undo;

	/* Whether we should go backward or forward when re-setting
           the style.  */
	gboolean forward;

	/* List of properties for the paragraphs (ClueFlowProps).  */
	GList *prop_list;
};
typedef struct _ClueFlowStyleOperation ClueFlowStyleOperation;


static ClueFlowProps *
get_props (HTMLClueFlow *clueflow)
{
	ClueFlowProps *props;

	props = g_new (ClueFlowProps, 1);

	props->indentation = html_clueflow_get_indentation (clueflow);
	props->alignment = html_clueflow_get_halignment (clueflow);
	props->style = html_clueflow_get_style (clueflow);

	return props;
}

static void
free_prop_list (GList *list)
{
	GList *p;

	for (p = list; p != NULL; p = p->next) {
		ClueFlowProps *props;

		props = (ClueFlowProps *) p->data;
		g_free (props);
	}

	g_list_free (list);
}

static ClueFlowProps *
get_props_and_set (HTMLEngine *engine,
		   HTMLClueFlow *clueflow,
		   HTMLClueFlowStyle style,
		   HTMLHAlignType alignment,
		   gint indentation_delta,
		   HTMLEngineSetClueFlowStyleMask mask)
{
	ClueFlowProps *props;

	props = get_props (clueflow);

	if (mask & HTML_ENGINE_SET_CLUEFLOW_STYLE)
		html_clueflow_set_style (clueflow, engine, style);
	if (mask & HTML_ENGINE_SET_CLUEFLOW_ALIGNMENT)
		html_clueflow_set_halignment (clueflow, engine, alignment);

	if (mask & HTML_ENGINE_SET_CLUEFLOW_INDENTATION)
		html_clueflow_indent (clueflow, engine, indentation_delta);

	return props;
}


/* Undo/redo operations.  */

static void add_undo (HTMLEngine *engine, ClueFlowStyleOperation *op);
static void add_redo (HTMLEngine *engine, ClueFlowStyleOperation *op);

static void
undo_or_redo_destroy_closure (gpointer closure)
{
	ClueFlowStyleOperation *op;

	op = (ClueFlowStyleOperation *) closure;

	free_prop_list (op->prop_list);
	g_free (op);
}

static void
undo_or_redo (HTMLEngine *engine,
	      gpointer closure)
{
	ClueFlowStyleOperation *op, *new_op;
	ClueFlowProps *props, *orig_props;
	HTMLObject *obj;
	HTMLClueFlow *clueflow;
	GList *prop_list;
	GList *p;

	op = (ClueFlowStyleOperation *) closure;
	g_assert (op != NULL);
	g_assert (op->prop_list != NULL);

	obj = engine->cursor->object;
	g_assert (obj != NULL);

	prop_list = NULL;

	p = op->prop_list;

	while (p != NULL) {
		if (HTML_OBJECT_TYPE (obj->parent) != HTML_TYPE_CLUEFLOW) {
			g_warning ("(%s:%s)  Eeeek!  Unknown parent type `%s'.",
				   __FILE__, G_GNUC_FUNCTION,
				   html_type_name (HTML_OBJECT_TYPE (obj->parent)));
			break;
		}

		clueflow = HTML_CLUEFLOW (obj->parent);

		orig_props = get_props (clueflow);
		prop_list = g_list_prepend (prop_list, orig_props);

		props = (ClueFlowProps *) p->data;

		html_clueflow_set_style (clueflow, engine, props->style);
		html_clueflow_set_halignment (clueflow, engine, props->alignment);
		html_clueflow_set_indentation (clueflow, engine, props->indentation);

		p = p->next;
		if (p == NULL)
			break;

		/* Go forward object by object, until we find one
                   whose parent (i.e. paragraph) is different.  */
		do {
			if (op->forward)
				obj = html_object_next_for_cursor (obj);
			else
				obj = html_object_prev_for_cursor (obj);

			if (obj == NULL) {
				/* This should not happen.  */
				g_warning ("(%s:%s)  There were not enough paragraphs for "
					   "setting the paragraph style.",
					   __FILE__, G_GNUC_FUNCTION);
				break;
			}
		} while (obj != NULL && HTML_CLUEFLOW (obj->parent) == clueflow);
	}

	if (prop_list == NULL) {
		/* This should not happen.  */
		g_warning ("%s:%s Eeek!  Nothing done?", __FILE__, G_GNUC_FUNCTION);
		return;
	}

	prop_list = g_list_reverse (prop_list);

	new_op = g_new (ClueFlowStyleOperation, 1);
	new_op->forward = op->forward;
	new_op->undo = ! op->undo;
	new_op->prop_list = prop_list;

	if (new_op->undo)
		add_undo (engine, new_op);
	else
		add_redo (engine, new_op);
}

static HTMLUndoAction *
undo_action_from_op (HTMLEngine *engine,
		     ClueFlowStyleOperation *op)
{
	return html_undo_action_new ("paragraph style change",
				     undo_or_redo,
				     undo_or_redo_destroy_closure,
				     op,
				     html_cursor_get_position (engine->cursor));
}

static void
add_undo (HTMLEngine *engine,
	  ClueFlowStyleOperation *op)
{
	html_undo_add_undo_action (engine->undo, undo_action_from_op (engine, op));
}

static void
add_redo (HTMLEngine *engine,
	  ClueFlowStyleOperation *op)
{
	html_undo_add_redo_action (engine->undo, undo_action_from_op (engine, op));
}


/* "Do" operations.  */

static void
set_clueflow_style_in_region (HTMLEngine *engine,
			      HTMLClueFlowStyle style,
			      HTMLHAlignType alignment,
			      gint indentation_delta,
			      HTMLEngineSetClueFlowStyleMask mask,
			      gboolean do_undo)
{
	ClueFlowStyleOperation *op;
	HTMLClueFlow *clueflow;
	HTMLObject *start, *end, *p;
	ClueFlowProps *orig_props;
	GList *prop_list;
	gboolean undo_forward;

	if (html_cursor_precedes (engine->cursor, engine->mark)) {
		start = engine->cursor->object;
		end = engine->mark->object;
		undo_forward = TRUE;
	} else {
		start = engine->mark->object;
		end = engine->cursor->object;
		undo_forward = FALSE;
	}

	prop_list = NULL;

	p = start;
	while (p != NULL) {
		if (HTML_OBJECT_TYPE (p->parent) != HTML_TYPE_CLUEFLOW) {
			g_warning ("(%s:%s)  Eeeek!  Unknown parent type `%s'.",
				   __FILE__, G_GNUC_FUNCTION,
				   html_type_name (HTML_OBJECT_TYPE (p->parent)));
			break;
		}

		clueflow = HTML_CLUEFLOW (p->parent);
		orig_props = get_props_and_set (engine, clueflow,
						style, alignment, indentation_delta,
						mask);

		if (do_undo) {
			prop_list = g_list_prepend (prop_list, orig_props);
		} else {
			/* FIXME allocating and deallocating is a bit yucky.  */
			g_free (orig_props);
		}

		if (p == end)
			break;

		do
			p = html_object_next_for_cursor (p);
		while (p != NULL && HTML_CLUEFLOW (p->parent) == clueflow);
	}

	if (! do_undo)
		return;

	op = g_new (ClueFlowStyleOperation, 1);
	op->undo = TRUE;
	op->forward = undo_forward;

	if (undo_forward)
		op->prop_list = g_list_reverse (prop_list);
	else
		op->prop_list = prop_list;

	add_undo (engine, op);
}

static void
set_clueflow_style_at_cursor (HTMLEngine *engine,
			      HTMLClueFlowStyle style,
			      HTMLHAlignType alignment,
			      gint indentation_delta,
			      HTMLEngineSetClueFlowStyleMask mask,
			      gboolean do_undo)
{
	ClueFlowStyleOperation *op;
	ClueFlowProps *props;
	HTMLClueFlow *clueflow;
	HTMLObject *curr;

	curr = engine->cursor->object;

	g_return_if_fail (curr != NULL);
	g_return_if_fail (curr->parent != NULL);
	g_return_if_fail (HTML_OBJECT_TYPE (curr->parent) == HTML_TYPE_CLUEFLOW);

	clueflow = HTML_CLUEFLOW (curr->parent);
	props = get_props_and_set (engine, clueflow,
				   style, alignment, indentation_delta,
				   mask);

	if (! do_undo) {
		g_free (props);
		return;
	}

	op = g_new (ClueFlowStyleOperation, 1);
	op->undo = TRUE;
	op->forward = TRUE;	/* (It does not really matter as it's just one para.)  */
	op->prop_list = g_list_append (NULL, props);

	add_undo (engine, op);
}


gboolean
html_engine_set_clueflow_style (HTMLEngine *engine,
				HTMLClueFlowStyle style,
				HTMLHAlignType alignment,
				gint indentation_delta,
				HTMLEngineSetClueFlowStyleMask mask,
				gboolean do_undo)
{
	g_return_val_if_fail (engine != NULL, FALSE);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), FALSE);

	html_undo_discard_redo (engine->undo);

	if (engine->active_selection)
		set_clueflow_style_in_region (engine,
					      style, alignment, indentation_delta,
					      mask,
					      do_undo);
	else
		set_clueflow_style_at_cursor (engine,
					      style, alignment, indentation_delta,
					      mask,
					      do_undo);

	/* This operation can never fail.  */
	return TRUE;
}


/* The following functions are used to report the current indentation
   as it should be shown e.g in a toolbar.  */

static HTMLClueFlow *
get_current_para (HTMLEngine *engine)
{
	HTMLObject *current;
	HTMLObject *parent;

	current = engine->cursor->object;
	if (current == NULL)
		return NULL;

	parent = current->parent;
	if (parent == NULL)
		return NULL;

	if (HTML_OBJECT_TYPE (parent) != HTML_TYPE_CLUEFLOW)
		return NULL;

	return HTML_CLUEFLOW (parent);
}

HTMLClueFlowStyle
html_engine_get_current_clueflow_style (HTMLEngine *engine)
{
	HTMLClueFlow *para;

	/* FIXME TODO region */

	g_return_val_if_fail (engine != NULL, HTML_CLUEFLOW_STYLE_NORMAL);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), HTML_CLUEFLOW_STYLE_NORMAL);

	para = get_current_para (engine);
	if (para == NULL)
		return HTML_CLUEFLOW_STYLE_NORMAL;

	return para->style;
}
 
guint
html_engine_get_current_clueflow_indentation (HTMLEngine *engine)
{
	HTMLClueFlow *para;

	/* FIXME TODO region */

	g_return_val_if_fail (engine != NULL, 0);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), 0);

	para = get_current_para (engine);
	if (para == NULL)
		return 0;

	return para->level;
}

HTMLHAlignType
html_engine_get_current_clueflow_alignment (HTMLEngine *engine)
{
	HTMLClueFlow *para;

	/* FIXME TODO region */

	g_return_val_if_fail (engine != NULL, HTML_HALIGN_LEFT);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), HTML_HALIGN_LEFT);

	para = get_current_para (engine);
	if (para == NULL)
		return 0;

	return HTML_CLUE (para)->halign;
}
