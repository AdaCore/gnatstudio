/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  This file is part of the CscHTML library.

    Copyright (C) 1997 Martin Jones (mjones@kde.org)
    Copyright (C) 1997 Torben Weis (weis@kde.org)
    Copyright (C) 1999 Anders Carlsson (andersca@gnu.org)
    Copyright (C) 1999, 2000 Helix Code, Inc.

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

/* RULE: You should never create a new flow without inserting anything in it.
   If `e->flow' is not NULL, it must contain something.  */

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "cscmarshal.h"
#include "cschtml-private.h"
#include "cschtml-stream.h"

#include "htmlengine.h"
#include "htmlengine-edit.h"
#include "htmlengine-edit-cursor.h"
#include "htmlengine-edit-movement.h"
#include "htmlengine-cutbuffer.h"
#include "htmlengine-edit-paste.h"

#include "htmlanchor.h"
#include "htmlrule.h"
#include "htmlobject.h"
#include "htmlclueh.h"
#include "htmlcluev.h"
#include "htmlcluealigned.h"
#include "htmlvspace.h"
#include "htmlimage.h"
#include "htmllinktext.h"
#include "htmllinktextmaster.h"
#include "htmllist.h"
#include "htmltable.h"
#include "htmltablecell.h"
#include "htmltextmaster.h"
#include "htmltextslave.h"
#include "htmltext.h"
#include "htmlclueflow.h"
#include "htmlstack.h"
#include "htmlstringtokenizer.h"
#include "htmlform.h"
#include "htmlbutton.h"
#include "htmltextinput.h"
#include "htmlradio.h"
#include "htmlcheckbox.h"
#include "htmlhidden.h"
#include "htmlselect.h"
#include "htmltextarea.h"
#include "htmlimageinput.h"
#include "htmlstack.h"
#include "htmlsearch.h"
#include "htmliframe.h"


static void      html_engine_class_init    (HTMLEngineClass     *klass);
static void      html_engine_init          (HTMLEngine          *engine);
static gboolean  html_engine_timer_event   (HTMLEngine          *e);
static gboolean  html_engine_update_event  (HTMLEngine          *e);
static void      html_engine_write         (CscHTMLStream       *stream,
					    const gchar         *buffer,
					    guint                size,
					    gpointer             data);
static void      html_engine_end           (CscHTMLStream       *stream,
					    CscHTMLStreamStatus  status,
					    gpointer             data);

static void      parse_one_token           (HTMLEngine *p,
					    HTMLObject *clue,
					    const gchar *str);
static void      parse_input               (HTMLEngine *e,
					    const gchar *s,
					    HTMLObject *_clue);
static void      parse_iframe              (HTMLEngine *e,
					    const gchar *s,
					    HTMLObject *_clue);
static void      parse_f                   (HTMLEngine *p,
					    HTMLObject *clue,
					    const gchar *str);

static void      html_object_changed       (CscHTMLEmbedded *eb,
					    HTMLEngine *e);


static GtkLayoutClass *parent_class = NULL;

enum {
	SET_BASE_TARGET,
	SET_BASE,
	LOAD_DONE,
	TITLE_CHANGED,
	URL_REQUESTED,
	DRAW_PENDING,
	REDIRECT,
	SUBMIT,
	OBJECT_REQUESTED,
	LAST_SIGNAL
};
	
static guint signals [LAST_SIGNAL] = { 0 };

#define TIMER_INTERVAL 300

enum ID {
	ID_ADDRESS, ID_B, ID_BIG, ID_BLOCKQUOTE, ID_CAPTION, ID_CITE, ID_CODE,
	ID_DIR, ID_DIV, ID_EM, ID_FONT, ID_HEADER, ID_I, ID_KBD, ID_OL, ID_PRE,
	ID_SMALL, ID_STRONG, ID_U, ID_UL, ID_TEXTAREA, ID_TD, ID_TH, ID_TT, ID_VAR
};

/* Font handling.  */

static CscHTMLFontStyle current_font_style(HTMLEngine *e) {
	CscHTMLFontStyle style;

	if (html_stack_is_empty(e->font_style_stack))
		return CSC_HTML_FONT_STYLE_DEFAULT;

	style = GPOINTER_TO_INT(html_stack_top(e->font_style_stack));
	return style;
}

static gchar *current_font_face(HTMLEngine *e) {
	gchar *face;

	if (html_stack_is_empty(e->font_face_stack))
		return e->widget->default_font_face;

	face = html_stack_top(e->font_face_stack);
	return face;
}

static CscHTMLFontStyle push_font_style(HTMLEngine *e, CscHTMLFontStyle new_attrs) {
	CscHTMLFontStyle current;
	CscHTMLFontStyle new;

	current = current_font_style(e);
	new = csc_html_font_style_merge(current, new_attrs);

	html_stack_push(e->font_style_stack, GINT_TO_POINTER(new));

	return new;
}

static void push_font_face(HTMLEngine *e, gchar *newFace) {

	if (e->widget->allow_font_switch == FALSE) {
		return;
	}
	
	html_stack_push(e->font_face_stack, newFace);
}
	
static void pop_font_style(HTMLEngine *e) {
	
	html_stack_pop(e->font_style_stack);
}

static void pop_font_face(HTMLEngine *e) {

	if (html_stack_is_empty(e->font_face_stack))
		return;

	html_stack_pop(e->font_face_stack);
}

/* Color handling.  */

static const GdkColor *
current_color (HTMLEngine *e)
{
	const GdkColor *color;

	if (html_stack_is_empty (e->color_stack))
		color = html_settings_get_color (e->settings, HTMLTextColor);
	else
		color = html_stack_top (e->color_stack);

	return color;
}

static void
push_color (HTMLEngine *e,
	   const  GdkColor *color)
{
	html_stack_push (e->color_stack, (gpointer) color);
}

static void
pop_color (HTMLEngine *e)
{
	html_stack_pop (e->color_stack);
}

static gboolean
parse_color (const gchar *text,
	     GdkColor *color)
{
	gchar *tmp;

	if (gdk_color_parse (text, color))
		return TRUE;

	tmp = alloca (strlen (text) + 2);
	*tmp = '#';
	strcpy (tmp + 1, text);

	return gdk_color_parse (tmp, color);
}


/* ClueFlow style handling.  */

static HTMLClueFlowStyle
current_clueflow_style (HTMLEngine *e)
{
	HTMLClueFlowStyle style;

	if (html_stack_is_empty (e->clueflow_style_stack))
		return HTML_CLUEFLOW_STYLE_NORMAL;

	style = (HTMLClueFlowStyle) GPOINTER_TO_INT (html_stack_top (e->clueflow_style_stack));
	return style;
}

static void
push_clueflow_style (HTMLEngine *e,
		     HTMLClueFlowStyle style)
{
	html_stack_push (e->clueflow_style_stack, GINT_TO_POINTER (style));
}

static void
pop_clueflow_style (HTMLEngine *e)
{
	html_stack_pop (e->clueflow_style_stack);
}



/* Utility functions.  */

static void new_flow (HTMLEngine *e, HTMLObject *clue, HTMLObject *first_object);
static void close_flow (HTMLEngine *e, HTMLObject *clue);

static HTMLObject *create_empty_text(HTMLEngine *e) {

	return html_text_master_new("", current_font_style(e), current_color(e), current_font_face(e));
}

static void
insert_paragraph_break (HTMLEngine *e,
			HTMLObject *clue)
{
	close_flow (e, clue);
	new_flow (e, clue, create_empty_text (e));
	close_flow (e, clue);
}

static void
add_line_break (HTMLEngine *e,
		HTMLObject *clue,
		HTMLClearType clear)
{
#if 0
	if (e->flow != NULL) {
		if (HTML_OBJECT_TYPE (HTML_CLUE (e->flow)->tail) == HTML_TYPE_VSPACE)
			html_clue_append (HTML_CLUE (e->flow), create_empty_text (e));
		html_clue_append (HTML_CLUE (e->flow), html_vspace_new (clear));
		return;
	}
#endif

	if (HTML_CLUE (clue)->head)
		new_flow (e, clue, NULL);
	else
		new_flow (e, clue, create_empty_text (e));
}

static void
close_anchor (HTMLEngine *e)
{
	if (e->url == NULL && e->target == NULL)
		return;

	if (e->url != NULL) {
		g_free (e->url);
		e->url = NULL;
	}

	g_free (e->target);
	e->target = NULL;

	pop_color (e);
}

static void
close_flow (HTMLEngine *e,
	    HTMLObject *clue)
{
	HTMLObject *last;

	if (e->flow == NULL)
		return;

	last = HTML_CLUE (e->flow)->tail;
	if (last == NULL) {
		html_clue_append (HTML_CLUE (e->flow), create_empty_text (e));
	} else if (HTML_OBJECT_TYPE (last) == HTML_TYPE_VSPACE) {
		html_clue_remove (HTML_CLUE (e->flow), last);
		html_object_destroy (last);
	} else if (HTML_CLUE (e->flow)->tail != HTML_CLUE (e->flow)->head
		   && HTML_OBJECT_TYPE (last) == HTML_TYPE_TEXTMASTER
		   && HTML_TEXT (last)->text_len == 1
		   && HTML_TEXT (last)->text [0] == ' ') {
		html_clue_remove (HTML_CLUE (e->flow), last);
		html_object_destroy (last);
	}

	e->flow = NULL;
}

static void
new_flow (HTMLEngine *e,
	  HTMLObject *clue,
	  HTMLObject *first_object)
{
	close_flow (e, clue);

	e->flow = html_clueflow_new (current_clueflow_style (e), e->indent_level);

	HTML_CLUE (e->flow)->halign = e->divAlign;

	if (first_object)
		html_clue_append (HTML_CLUE (e->flow), first_object);

	html_clue_append (HTML_CLUE (clue), e->flow);
}

static void
append_element (HTMLEngine *e,
		HTMLObject *clue,
		HTMLObject *obj)
{
	if (e->pending_para) {
		insert_paragraph_break (e, clue);
		e->pending_para = FALSE;
	}

	e->avoid_para = FALSE;

	if (e->flow == NULL)
		new_flow (e, clue, obj);
	else
		html_clue_append (HTML_CLUE (e->flow), obj);
}


static gboolean
check_prev (const HTMLObject *p,
	    HTMLType type,
	    CscHTMLFontStyle font_style,
	    const GdkColor *color)
{
	if (p == NULL)
		return FALSE;

	if (HTML_OBJECT_TYPE (p) != type)
		return FALSE;

	if (HTML_TEXT (p)->font_style != font_style)
		return FALSE;

	if (! gdk_color_equal (&HTML_TEXT (p)->color, color))
		return FALSE;

	return TRUE;
}

static void insert_text(HTMLEngine *e, HTMLObject *clue, const gchar *text) {
	CscHTMLFontStyle font_style;
	const gchar *font_face;
	HTMLObject *prev;
	HTMLType type;
	const GdkColor *color;
	gboolean create_link;

	if (e->url != NULL || e->target != NULL)
		create_link = TRUE;
	else
		create_link = FALSE;

	font_style = current_font_style(e);
	font_face = current_font_face(e);
	color = current_color (e);

	if (e->pending_para || e->flow == NULL || HTML_CLUE (e->flow)->head == NULL) {
		while (*text == ' ')
			text++;
		if (*text == 0)
			return;
	}

	if (e->flow == NULL)
		prev = NULL;
	else
		prev = HTML_CLUE (e->flow)->tail;

	if (e->url != NULL || e->target != NULL)
		type = HTML_TYPE_LINKTEXT;
	else
		type = HTML_TYPE_TEXT;

	if (! check_prev (prev, type, font_style, color)) {
		HTMLObject *obj;

		if (create_link)
			obj = html_link_text_master_new(text, font_style, color, font_face, e->url, e->target);
		else
			obj = html_text_master_new(text, font_style, color, font_face);

		append_element (e, clue, obj);
	} else {
		HTMLText *text_prev;
		gchar *new_text;

		/* Reuse existing element.  */
		/* FIXME this sucks.  */

		text_prev = HTML_TEXT (prev);

		new_text = g_strconcat (text_prev->text, text, NULL);
		g_free (text_prev->text);
		text_prev->text = new_text;
	}
}


/* Block stack.  */

typedef void (*BlockFunc)(HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *el);

struct _HTMLBlockStackElement {
	BlockFunc exitFunc;

	gint id;
	gint level;
	gint miscData1;
	gint miscData2;
	HTMLBlockStackElement *next;
};

static HTMLBlockStackElement *
block_stack_element_new (gint id, gint level, BlockFunc exitFunc, 
			 gint miscData1, gint miscData2, HTMLBlockStackElement *next)
{
	HTMLBlockStackElement *se;

	se = g_new0 (HTMLBlockStackElement, 1);
	se->id = id;
	se->level = level;
	se->miscData1 = miscData1;
	se->miscData2 = miscData2;
	se->next = next;
	se->exitFunc = exitFunc;
	return se;
}

static void
block_stack_element_free (HTMLBlockStackElement *elem)
{
	g_free (elem);
}

static void
push_block (HTMLEngine *e, gint id, gint level,
	    BlockFunc exitFunc,
	    gint miscData1,
	    gint miscData2)
{
	HTMLBlockStackElement *elem;

	elem = block_stack_element_new (id, level, exitFunc, miscData1, miscData2, e->blockStack);
	e->blockStack = elem;
}

static void
free_block (HTMLEngine *e)
{
	HTMLBlockStackElement *elem = e->blockStack;

	while (elem != 0) {
		HTMLBlockStackElement *tmp = elem;

		elem = elem->next;
		block_stack_element_free (tmp);
	}
	e->blockStack = 0;
}

static void
pop_block (HTMLEngine *e, gint id, HTMLObject *clue)
{
	HTMLBlockStackElement *elem, *tmp;
	gint maxLevel;

	elem = e->blockStack;
	maxLevel = 0;

	while ((elem != 0) && (elem->id != id)) {
		if (maxLevel < elem->level) {
			maxLevel = elem->level;
		}
		elem = elem->next;
	}
	if (elem == 0)
		return;
	if (maxLevel > elem->level)
		return;
	
	elem = e->blockStack;
	
	while (elem) {
		tmp = elem;
		if (elem->exitFunc != 0)
			(*(elem->exitFunc))(e, clue, elem);
		if (elem->id == id) {
			e->blockStack = elem->next;
			elem = 0;
		}
		else {
			elem = elem->next;
		}

		block_stack_element_free (tmp);
	}
}


/* The following are callbacks that are called at the end of a block.  */

static void
block_end_font (HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *elem)
{
	pop_font_style (e);
	pop_font_face (e);
}

static void
block_end_clueflow_style (HTMLEngine *e,
			  HTMLObject *clue,
			  HTMLBlockStackElement *elem)
{
	close_flow (e, clue);
	pop_clueflow_style (e);

	e->divAlign = elem->miscData1;
}

static void
block_end_pre ( HTMLEngine *e, HTMLObject *_clue, HTMLBlockStackElement *elem)
{
	block_end_clueflow_style (e, _clue, elem);
	e->inPre = FALSE;
}

static void
block_end_color_font (HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *elem)
{
	pop_color (e);
	block_end_font (e, clue, elem);
}

static void
block_end_list (HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *elem)
{
	html_list_destroy (html_stack_pop (e->listStack));

	close_flow (e, clue);
	
	e->indent_level = elem->miscData1;

	if (e->indent_level == 0) {
		e->pending_para = FALSE;
		e->avoid_para = TRUE;
	}
}

static void
block_end_quote (HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *elem)
{
	close_flow (e, clue);

	e->indent_level = elem->miscData1;

	e->pending_para = FALSE;
	e->avoid_para = TRUE;
}

static void
block_end_div (HTMLEngine *e, HTMLObject *clue, HTMLBlockStackElement *elem)
{
	close_flow (e, clue);

	e->divAlign =  (HTMLHAlignType) elem->miscData1;
}


static gchar *
parse_body (HTMLEngine *p, HTMLObject *clue, const gchar *end[], gboolean toplevel)
{
	gchar *str;

	while (html_tokenizer_has_more_tokens (p->ht) && p->parsing) {
		str = html_tokenizer_next_token (p->ht);

		if (*str == '\0')
			continue;

		if ( *str == ' ' && *(str+1) == '\0' ) {
			/* if in* is set this text belongs in a form element */
			if (p->inOption || p->inTextArea)
				p->formText = g_string_append (p->formText, " ");
			else if (p->inTitle) {
				g_string_append (p->title, " ");
			} else if (p->flow != NULL) {
				insert_text (p, clue, " ");
			}
		} else if (*str != TAG_ESCAPE) {
			if (p->inOption || p->inTextArea)
				g_string_append (p->formText, str);
			else if (p->inTitle) {
				g_string_append (p->title, str);
			}
			else {
				insert_text (p, clue, str);
			}
		}
		else {
			gint i  = 0;
			str++;
			
			while (end [i] != 0) {
				if (strncasecmp (str, end[i], strlen(end[i])) == 0) {
					return str;
				}
				i++;
			}
			
			/* The tag used for line break when we are in <pre>...</pre> */
			if (*str == '\n')
				add_line_break (p, clue, HTML_CLEAR_NONE);
			else
				parse_one_token (p, clue, str);
		}
	}

	if (!html_tokenizer_has_more_tokens (p->ht) && toplevel && !p->writing)
		html_engine_stop_parser (p);

	return 0;
}

static gchar *
discard_body (HTMLEngine *p, const gchar *end[])
{
	gchar *str = NULL;

	while (html_tokenizer_has_more_tokens (p->ht) && p->parsing) {
		str = html_tokenizer_next_token (p->ht);

		if (*str == '\0')
			continue;

		if ((*str == ' ' && *(str+1) == '\0')
		    || (*str != TAG_ESCAPE)) {
			/* do nothing */
		}
		else {
			gint i  = 0;
			str++;
			
			while (end [i] != 0) {
				if (strncasecmp (str, end[i], strlen(end[i])) == 0) {
					return str;
				}
				i++;
			}
		}
	}

	return 0;
}

/* EP CHECK: finished except for the settings stuff (see `FIXME').  */
static const gchar *
parse_table (HTMLEngine *e, HTMLObject *clue, gint max_width,
	     const gchar *attr)
{
	static const gchar *endthtd[] = { "</th", "</td", "</tr", "<th", "<td", "<tr", "</table", "</body", 0 };
	static const char *endcap[] = { "</caption>", "</table>", "<tr", "<td", "<th", "</body", 0 };    
	static const gchar *endall[] = { "</caption>", "</table", "<tr", "<td", "<th", "</th", "</td", "</tr","</body", 0 };
	HTMLTable *table;
	const gchar *str = 0;
	gint width = 0;
	gint percent = 0;
	gint padding = 1;
	gint spacing = 2;
	gint border = 0;
	gchar has_cell = 0;
	gboolean done = FALSE;
	gboolean tableTag = TRUE;
	gboolean firstRow = TRUE;
	gboolean noCell = TRUE;
	gboolean tableEntry;
	gboolean noWrap_save;
	HTMLVAlignType rowvalign = HTML_VALIGN_NONE;
	HTMLHAlignType rowhalign = HTML_HALIGN_NONE;
	HTMLHAlignType align = HTML_HALIGN_NONE;
	HTMLClueV *caption = 0;
	HTMLVAlignType capAlign = HTML_VALIGN_BOTTOM;
	HTMLHAlignType olddivalign = e->divAlign;
	HTMLClue *oldflow = HTML_CLUE (e->flow);
	gint old_indent_level = e->indent_level;
	GdkColor tableColor, rowColor, bgColor;
	gboolean have_tableColor, have_rowColor, have_bgColor;
	gboolean have_tablePixmap, have_rowPixmap, have_bgPixmap;
	gint rowSpan;
	gint colSpan;
	gint cellwidth;
	gint cellpercent;
	gboolean fixedWidth;
	HTMLVAlignType valign;
	HTMLTableCell *cell;
	gpointer tablePixmapPtr = NULL;
	gpointer rowPixmapPtr = NULL;
	gpointer bgPixmapPtr = NULL;

	have_tablePixmap = FALSE;
	have_rowPixmap = FALSE;
	have_bgPixmap = FALSE;

	have_tableColor = FALSE;
	have_rowColor = FALSE;
	have_bgColor = FALSE;

	csc_html_debug_log (e->widget, "start parse\n");

	noWrap_save = e->noWrap;

	html_string_tokenizer_tokenize (e->st, attr, " >");
	while (html_string_tokenizer_has_more_tokens (e->st)) {
		const gchar *token = html_string_tokenizer_next_token (e->st);
		if (strncasecmp (token, "cellpadding=", 12) == 0) {
			padding = atoi (token + 12);
		}
		else if (strncasecmp (token, "cellspacing=", 12) == 0) {
			spacing = atoi (token + 12);
		}
		else if (strncasecmp (token, "border", 6) == 0) {
			if (*(token + 6) == '=')
				border = atoi (token + 7);
			else
				border = 1;
		}
		else if (strncasecmp (token, "width=", 6) == 0) {
			if (strchr (token + 6, '%')) {
				percent = atoi (token + 6);
			} else if (strchr (token + 6, '*')) {
				/* Ignore */
			} else if (isdigit (*(token + 6))) {
				width = atoi (token + 6);
			}
		}
		else if (strncasecmp (token, "align=", 6) == 0) {
			if (strcasecmp (token + 6, "left") == 0)
				align = HTML_HALIGN_LEFT;
			else if (strcasecmp (token + 6, "right") == 0)
				align = HTML_HALIGN_RIGHT;
			else if (strcasecmp (token + 6, "center") == 0)
				align = HTML_HALIGN_CENTER;
		}
		else if (strncasecmp (token, "bgcolor=", 8) == 0
			 && !e->defaultSettings->forceDefault) {
			if (parse_color (token + 8, &tableColor)) {
				rowColor = tableColor;
				have_rowColor = have_tableColor = TRUE;
			}
		}
		else if (strncasecmp (token, "background=", 11) == 0
			 && token [12]
			 && !e->defaultSettings->forceDefault) {
			tablePixmapPtr = html_image_factory_register(e->image_factory, NULL, token + 11);

			if(tablePixmapPtr) {
				rowPixmapPtr = tablePixmapPtr;
				have_tablePixmap = have_rowPixmap = TRUE;
			}
		}
	}

	table = HTML_TABLE (html_table_new (width, 
					    percent, padding,
					    spacing, border));
	e->indent_level = 0;

	while (!done && html_tokenizer_has_more_tokens (e->ht)) {
		str = html_tokenizer_next_token (e->ht);
		
		/* Every tag starts with an escape character */
		if (str[0] == TAG_ESCAPE) {
			str++;

			tableTag = TRUE;

			for (;;) {
				if (strncmp (str, "</table", 7) == 0) {
					close_anchor (e);
					done = TRUE;
					break;
				}

				if ( strncmp( str, "<caption", 8 ) == 0 ) {
					html_string_tokenizer_tokenize( e->st, str + 9, " >" );
					while ( html_string_tokenizer_has_more_tokens (e->st) ) {
						const char* token = html_string_tokenizer_next_token(e->st);
						if ( strncasecmp( token, "align=", 6 ) == 0) {
							if ( strncasecmp( token+6, "top", 3 ) == 0)
								capAlign = HTML_VALIGN_TOP;
						}
					}

					caption = HTML_CLUEV (html_cluev_new (0, 0, 100));

					e->divAlign = HTML_HALIGN_CENTER;
					e->flow = 0;

					push_block (e, ID_CAPTION, 3, NULL, 0, 0);
					str = parse_body ( e, HTML_OBJECT (caption), endcap, FALSE );
					pop_block (e, ID_CAPTION, HTML_OBJECT (caption) );

					table->caption = caption;
					table->capAlign = capAlign;

					e->flow = 0;

					if ( str == 0 ) { 
						/* CC: Close table description in case of a malformed
						   table before returning! */
						if ( !firstRow )
							html_table_end_row (table);
						html_table_end_table (table); 
						html_object_destroy (HTML_OBJECT (table));
						e->divAlign = olddivalign;
						e->flow = HTML_OBJECT (oldflow);
						e->noWrap = noWrap_save;

						return 0;
					}

					if (strncmp( str, "</caption", 9) == 0 ) {
						// HTML Ok!
						break; // Get next token from 'ht'
					}
					else {
						// Bad HTML
						// caption ended with </table> <td> <tr> or <th>
						continue; // parse the returned tag
					}
				}

				if (strncmp (str, "<tr", 3) == 0) {
					if (!firstRow)
						html_table_end_row (table);
					html_table_start_row (table);
					firstRow = FALSE;
					rowvalign = HTML_VALIGN_NONE;
					rowhalign = HTML_HALIGN_NONE;

					if (have_tableColor) {
						rowColor = tableColor;
						have_rowColor = TRUE;
					} else {
						have_rowColor = FALSE;
					}

					if (have_tablePixmap) {
						rowPixmapPtr = tablePixmapPtr;
						have_rowPixmap = TRUE;
					} else {
						have_rowPixmap = FALSE;
					}

					html_string_tokenizer_tokenize (e->st, str + 4, " >");
					while (html_string_tokenizer_has_more_tokens (e->st)) {
						const gchar *token = html_string_tokenizer_next_token (e->st);
						if (strncasecmp (token, "valign=", 7) == 0) {
							if (strncasecmp (token + 7, "top", 3) == 0)
								rowvalign = HTML_VALIGN_TOP;
							else if (strncasecmp (token + 7, "bottom", 6) == 0)
								rowvalign = HTML_VALIGN_BOTTOM;
							else
								rowvalign = HTML_VALIGN_CENTER;
						} else if (strncasecmp (token, "align", 6) == 0) {
							if (strcasecmp (token + 6, "left") == 0)
								rowhalign = HTML_HALIGN_LEFT;
							else if (strcasecmp (token + 6, "right") == 0)
								rowhalign = HTML_HALIGN_RIGHT;
							else if (strcasecmp (token + 6, "center") == 0)
								rowhalign = HTML_HALIGN_CENTER;
						} else if (strncasecmp (token, "bgcolor=", 8) == 0) {
							have_rowColor = parse_color (token + 8, &rowColor);
						} else if (strncasecmp (token, "background=", 11) == 0
							   && token [12]
							   && !e->defaultSettings->forceDefault) {
							rowPixmapPtr = html_image_factory_register(e->image_factory, NULL, token + 11);
							if(rowPixmapPtr)
								have_rowPixmap = TRUE;
						}
					}
					break;
				} /* Hack to fix broken html in bonsai */
				else if (strncmp (str, "<form", 5) == 0 || strncmp (str, "</form", 6) == 0) {
					parse_f (e, clue, str + 1);
				}

				/* Check for <td> and <th> */
				tableEntry = *str == '<' && *(str + 1) == 't' &&
					(*(str + 2) == 'd' || *(str + 2) == 'h');
				if (tableEntry || noCell) {
					gboolean heading = FALSE;
					noCell = FALSE;

					if (tableEntry && *(str + 2) == 'h') {
						csc_html_debug_log (e->widget, "<th>\n");
						heading = TRUE;
					}

					/* <tr> may not be specified for the first row */
					if (firstRow) {
						/* Bad HTML: No <tr> tag present */
						html_table_start_row (table);
						firstRow = FALSE;
					}

					rowSpan = 1;
					colSpan = 1;
					cellwidth = clue->max_width;
					cellpercent = -1;
					fixedWidth = FALSE;
					e->noWrap = FALSE;

					if (have_rowColor) {
						bgColor = rowColor;
						have_bgColor = TRUE;
					} else {
						have_bgColor = FALSE;
					}

					if (have_rowPixmap) {
						bgPixmapPtr = rowPixmapPtr;
						have_bgPixmap = TRUE;
					} else {
						have_bgPixmap = FALSE;
					}

					valign = (rowvalign == HTML_VALIGN_NONE ?
						  HTML_VALIGN_CENTER : rowvalign);

					if (heading)
						e->divAlign = (rowhalign == HTML_HALIGN_NONE ? 
							       HTML_HALIGN_CENTER : rowhalign);
					else
						e->divAlign = (rowhalign == HTML_HALIGN_NONE ?
							       HTML_HALIGN_LEFT : rowhalign);

					if (tableEntry) {
						html_string_tokenizer_tokenize (e->st, str + 4, " >");
						while (html_string_tokenizer_has_more_tokens (e->st)) {
							const gchar *token = html_string_tokenizer_next_token (e->st);
							if (strncasecmp (token, "rowspan=", 8) == 0) {
								rowSpan = atoi (token + 8);
								if (rowSpan < 1)
									rowSpan = 1;
							}
							else if (strncasecmp (token, "colspan=", 8) == 0) {
								colSpan = atoi (token + 8);
								if (colSpan < 1)
									colSpan = 1;
							}
							else if (strncasecmp (token, "valign=", 7) == 0) {
								if (strncasecmp (token + 7, "top", 3) == 0)
									valign = HTML_VALIGN_TOP;
								else if (strncasecmp (token + 7, "bottom", 6) == 0)
									valign = HTML_VALIGN_BOTTOM;
								else 
									valign = HTML_VALIGN_CENTER;
							}
							else if (strncasecmp (token, "align=", 6) == 0) {
								if (strcasecmp (token + 6, "center") == 0)
									e->divAlign = HTML_HALIGN_CENTER;
								else if (strcasecmp (token + 6, "right") == 0)
									e->divAlign = HTML_HALIGN_RIGHT;
								else if (strcasecmp (token + 6, "left") == 0)
									e->divAlign = HTML_HALIGN_LEFT;
							}
							else if (strncasecmp (token, "height=", 6) == 0) {
								/* FIXME cell height negotiation needs to be added
								 * it shouldn't be very difficult
								 */
							}
							else if (strncasecmp (token, "width=", 6) == 0) {
								if (strchr (token + 6, '%')) {
									csc_html_debug_log (e->widget, "percent!\n");
									cellpercent = atoi (token + 6);
								}
								else if (strchr (token + 6, '*')) {
									/* ignore */
								}
								else if (isdigit (*(token + 6))) {
									cellwidth = atoi (token + 6);
									cellpercent = 0;
									fixedWidth = TRUE;
								}
							}
							else if (strncasecmp (token, "bgcolor=", 8) == 0
								 && !e->defaultSettings->forceDefault) {
								have_bgColor = parse_color (token + 8,
											    &bgColor);
							}
							else if (strncasecmp (token, "nowrap", 6) == 0) {

								e->noWrap = TRUE;
								push_clueflow_style (e, HTML_CLUEFLOW_STYLE_NOWRAP);
							}
							else if (strncasecmp (token, "background=", 11) == 0
								 && token [12]
								 && !e->defaultSettings->forceDefault) {
								
								bgPixmapPtr = html_image_factory_register(e->image_factory, 
													  NULL, token + 11);
								if(bgPixmapPtr)
									have_bgPixmap = TRUE;

							}
						}
					}

					if (e->pending_para) {
						insert_paragraph_break (e, clue);
						e->pending_para = FALSE;
					}

					cell = HTML_TABLE_CELL (html_table_cell_new (cellpercent,
										     rowSpan, colSpan,
										     padding));
					html_object_set_bg_color (HTML_OBJECT (cell),
								  have_bgColor ? &bgColor : NULL);

					if(have_bgPixmap)
						html_table_cell_set_bg_pixmap(cell, bgPixmapPtr);

					HTML_CLUE (cell)->valign = valign;
					if (fixedWidth)
						html_table_cell_set_fixed_width (cell, cellwidth);
 
					html_table_add_cell (table, cell);
					has_cell = 1;
					e->flow = NULL;

					e->avoid_para = TRUE;

					if (!tableEntry) {
						/* Put all the junk between <table>
						   and the first table tag into one row */
						push_block (e, ID_TD, 3, NULL, 0, 0);
						str = parse_body (e, HTML_OBJECT (cell), endall, FALSE);
						pop_block (e, ID_TD, HTML_OBJECT (cell));

						if (e->pending_para) {
							insert_paragraph_break (e, HTML_OBJECT (cell));
							e->pending_para = FALSE;
						}

						html_table_end_row (table);
						html_table_start_row (table);
					} else {
						if (heading) {
							push_font_style (e, CSC_HTML_FONT_STYLE_BOLD);
							push_block (e, ID_TH, 3,
								    block_end_font,
								    FALSE, 0);
							str = parse_body (e, HTML_OBJECT (cell), endthtd, FALSE);
							pop_block (e, ID_TH, HTML_OBJECT (cell));
						} else {
							push_block (e, ID_TD, 3, NULL, 0, 0);
							str = parse_body (e, HTML_OBJECT (cell), endthtd, FALSE);
							pop_block (e, ID_TD, HTML_OBJECT (cell));
						}
						if (e->pending_para) {
							insert_paragraph_break (e, HTML_OBJECT (cell));
							e->pending_para = FALSE;
						}
					}

					if (str == 0) {
						/* Close table description in case of
						   a malformed table before returning! */
						if (!firstRow)
							html_table_end_row (table);
						html_table_end_table (table);
						html_object_destroy (HTML_OBJECT (table));
						e->divAlign = olddivalign;
						e->flow = HTML_OBJECT (oldflow);
						e->noWrap = noWrap_save;

						return 0;
					}
					if (e->noWrap) {

						e->noWrap = FALSE;
						pop_clueflow_style (e);
					}

					if ((strncmp (str, "</td", 4) == 0) ||
					    (strncmp (str, "</th", 4) == 0)) {
						/* HTML ok! */
						break; /* Get next token from 'ht' */
					}
					else {
						/* Bad HTML */
						continue;
					}
				}

				/* Unknown or unhandled table-tag: ignore */
				break;
				
			}
		}
	}
		
	e->indent_level = old_indent_level;
	e->divAlign = olddivalign;
	e->flow = HTML_OBJECT (oldflow);

	if (has_cell) {
		/* The ending "</table>" might be missing, so we close the table
		   here...  */
		if (!firstRow)
			html_table_end_row (table);
		html_table_end_table (table);

		if (align != HTML_HALIGN_LEFT && align != HTML_HALIGN_RIGHT) {
			close_flow (e, clue);

			if (align != HTML_HALIGN_NONE) {
				olddivalign = e->divAlign;
				e->divAlign = align;
			}

			append_element (e, clue, HTML_OBJECT (table));

			close_flow (e, clue);

			if (align != HTML_HALIGN_NONE)
				e->divAlign = olddivalign;
		}
	} else {
		/* Last resort: remove tables that do not contain any cells */
		html_object_destroy (HTML_OBJECT (table));
	}

	e->noWrap = noWrap_save;
	
	csc_html_debug_log (e->widget, "Returning: %s\n", str);
	return str;
}

static void
parse_object (HTMLEngine *e, HTMLObject *clue, gint max_width,
	     const gchar *attr)
{
	char *classid=NULL;
	char *name=NULL;
	char *type = NULL;
	char *str = NULL;
	int width=-1,height=-1;
	static const gchar *end[] = { "</object", 0};

	
	html_string_tokenizer_tokenize( e->st, attr, " >" );
	
	/* this might have to do something different for form object
	   elements - check the spec MPZ */
	while (html_string_tokenizer_has_more_tokens (e->st) ) {
		const char* token;
		
		token = html_string_tokenizer_next_token (e->st);
		if (strncasecmp (token, "classid=", 8) == 0) {
			classid = g_strdup (token + 8);
		} else if (strncasecmp (token, "name=", 6) == 0 ) {
			name = g_strdup (token + 6);
		} else if ( strncasecmp (token, "width=", 6) == 0) {
			width = atoi (token + 6);
		} else if (strncasecmp (token, "height=", 7) == 0) {
			height = atoi (token + 7);
		} else if (strncasecmp (token, "type=", 5) == 0) {
			type = g_strdup (token + 5);
		}
	}

	if (classid) {
		CscHTMLEmbedded *eb;
		HTMLEmbedded *el;
		gboolean ret_val;
		
		eb = (CscHTMLEmbedded *)csc_html_embedded_new(classid, name, type, width, height);
		html_stack_push (e->embeddedStack, eb);
		
		el = html_embedded_new_widget(GTK_WIDGET (e->widget), eb);
		
		gtk_object_set_data(GTK_OBJECT(eb), "embeddedelement", el);
		gtk_signal_connect(GTK_OBJECT(eb), "changed", html_object_changed, e);
		
		ret_val = FALSE;
		gtk_signal_emit (GTK_OBJECT (e), signals[OBJECT_REQUESTED], eb, &ret_val);
		
		g_free(classid);
		g_free(name);
		
		if (ret_val) {
			append_element(e, clue, HTML_OBJECT(el));
			/* automatically add this to a form if it is part of one */
			if (e->form) {
				html_form_add_element (e->form, HTML_EMBEDDED (el));
			}
			str = discard_body (e, end);
		} else {
			html_object_destroy (HTML_OBJECT (el));
			str = parse_body (e, clue, end, FALSE);
		}
	} else {
		g_warning("Object with no classid, ignored\n");
	}
	
	if (!str || strncmp( str, "/object", 7 ) == 0 ) {		
		if (! html_stack_is_empty (e->embeddedStack)) {
			CscHTMLEmbedded *eb;
			
			eb = html_stack_pop (e->embeddedStack);
		}
	}
}

static void
parse_input (HTMLEngine *e, const gchar *str, HTMLObject *_clue)
{
	enum InputType { CheckBox, Hidden, Radio, Reset, Submit, Text, Image,
			 Button, Password, Undefined };
	HTMLObject *element = NULL;
	const char *p;
	enum InputType type = Text;
	gchar *name = NULL;
	gchar *value = NULL;
	gchar *imgSrc = NULL;
	gboolean checked = FALSE;
	int size = 20;
	int maxLen = -1;
	int imgHSpace = 0;
	int imgVSpace = 0;

	html_string_tokenizer_tokenize (e->st, str, " >");

	while (html_string_tokenizer_has_more_tokens (e->st)) {
		const gchar *token = html_string_tokenizer_next_token (e->st);

		if ( strncasecmp( token, "type=", 5 ) == 0 ) {
			p = token + 5;
			if ( strncasecmp( p, "checkbox", 8 ) == 0 )
				type = CheckBox;
			else if ( strncasecmp( p, "password", 8 ) == 0 )
				type = Password;
			else if ( strncasecmp( p, "hidden", 6 ) == 0 )
				type = Hidden;
			else if ( strncasecmp( p, "radio", 5 ) == 0 )
				type = Radio;
			else if ( strncasecmp( p, "reset", 5 ) == 0 )
				type = Reset;
			else if ( strncasecmp( p, "submit", 5 ) == 0 )
				type = Submit;
			else if ( strncasecmp( p, "button", 6 ) == 0 )
				type = Button;
			else if ( strncasecmp( p, "text", 5 ) == 0 )
				type = Text;
			else if ( strncasecmp( p, "Image", 5 ) == 0 )
				type = Image;
		}
		else if ( strncasecmp( token, "name=", 5 ) == 0 ) {
			name = g_strdup(token + 5);
		}
		else if ( strncasecmp( token, "value=", 6 ) == 0 ) {
			value = g_strdup(token + 6);
		}
		else if ( strncasecmp( token, "size=", 5 ) == 0 ) {
			size = atoi( token + 5 );
		}
		else if ( strncasecmp( token, "maxlength=", 10 ) == 0 ) {
			maxLen = atoi( token + 10 );
		}
		else if ( strncasecmp( token, "checked", 7 ) == 0 ) {
			checked = TRUE;
		}
		else if ( strncasecmp( token, "src=", 4 ) == 0 ) {
			imgSrc = g_strdup (token + 4);
		}
		else if ( strncasecmp( token, "onClick=", 8 ) == 0 ) {
			/* TODO: Implement Javascript */
		}
		else if ( strncasecmp( token, "hspace=", 7 ) == 0 ) {
			imgHSpace = atoi (token + 7);
		}
		else if ( strncasecmp( token, "vspace=", 7 ) == 0 ) {
			imgVSpace = atoi (token + 7);
		}
	}
	switch ( type ) {
	case CheckBox:
		element = html_checkbox_new(GTK_WIDGET(e->widget), name, value, checked);
		break;
	case Hidden:
		{
		HTMLObject *hidden = html_hidden_new(name, value);

		html_form_add_hidden (e->form, HTML_HIDDEN (hidden));

		break;
		}
	case Radio:
		element = html_radio_new(GTK_WIDGET(e->widget), name, value, checked, &e->form->radio_group);
		break;
	case Reset:
		element = html_button_new(GTK_WIDGET(e->widget), name, value, BUTTON_RESET);
		break;
	case Submit:
		element = html_button_new(GTK_WIDGET(e->widget), name, value, BUTTON_SUBMIT);
		break;
	case Button:
		element = html_button_new(GTK_WIDGET(e->widget), name, value, BUTTON_NORMAL);
	case Text:
	case Password:
		element = html_text_input_new(GTK_WIDGET(e->widget), name, value, size, maxLen, (type == Password));
		break;
	case Image:
		element = html_imageinput_new (e->image_factory, name, imgSrc);
		html_image_set_spacing (HTML_IMAGE (element), imgHSpace, imgVSpace);
		break;
	case Undefined:
		g_warning ("Unknown <input type>\n");
		break;
	}
	if (element) {

		append_element (e, _clue, element);
		html_form_add_element (e->form, HTML_EMBEDDED (element));
	}

	if (name)
		g_free (name);
	if (value)
		g_free (value);
	if (imgSrc)
		g_free (imgSrc);
}

static void
parse_iframe (HTMLEngine *e, const gchar *str, HTMLObject *_clue) 
{
	char *src = NULL;
	char *width = NULL;
	char *height = NULL;
	char *align = NULL;
	HTMLObject *iframe;
	static const gchar *end[] = { "</iframe", 0};

	width = "-1";
	height = "-1";

	html_string_tokenizer_tokenize (e->st, str, " >");

	while (html_string_tokenizer_has_more_tokens (e->st)) {
		const gchar *token = html_string_tokenizer_next_token (e->st);

		if ( strncasecmp( token, "src=", 4 ) == 0 ) {
			src = g_strdup(token + 4);
		} else if ( strncasecmp( token, "width=", 6 ) == 0 ) {
			width = g_strdup(token + 6);
		} else if ( strncasecmp( token, "height=", 7 ) == 0 ) {
			height = g_strdup( token + 7 );
		} else if ( strncasecmp( token, "align=", 6 ) == 0 ) {
			align = g_strdup( token + 6 );
		} else if ( strncasecmp( token, "longdesc=", 9 ) == 0 ) {
			/* TODO: Ignored */
		} else if ( strncasecmp( token, "name=", 7 ) == 0 ) {
			/* TODO: Ignored */
		} else if ( strncasecmp( token, "scrolling=", 7 ) == 0 ) {
			/* TODO: implement this damn thing */
		} else if ( strncasecmp( token, "border=", 7 ) == 0 ) {
			/* TODO: implement this damn thing */
		}

	}	
		
	if (src) {
		iframe = html_iframe_new (GTK_WIDGET (e->widget),
					  src, atoi(width), atoi (height), FALSE);
		append_element (e, _clue, iframe);
		discard_body (e, end);
	} else {
		parse_body (e, _clue, end, FALSE);
	}
	
}


/*
  <a               </a>
  <address>        </address>
  <area            </area>
*/
static void
parse_a (HTMLEngine *e, HTMLObject *_clue, const gchar *str)
{
#if 0							/* FIXME TODO */
	if ( strncmp( str, "area", 4 ) == 0 ) {
		GString *href = g_string_new (NULL);
		GString *coords = g_string_new (NULL);
		GString *atarget = g_string_new (baseTarget->str);

		if ( mapList.isEmpty() )
			return;

		html_string_tokenizer_tokenize (e->st, str + 5, " >");

		HTMLArea::Shape shape = HTMLArea::Rect;

		while ( stringTok->hasMoreTokens() )
		{
			const char* p = stringTok->nextToken();

			if ( strncasecmp( p, "shape=", 6 ) == 0 ) {
				if ( strncasecmp( p+6, "rect", 4 ) == 0 )
					shape = HTMLArea::Rect;
				else if ( strncasecmp( p+6, "poly", 4 ) == 0 )
					shape = HTMLArea::Poly;
				else if ( strncasecmp( p+6, "circle", 6 ) == 0 )
					shape = HTMLArea::Circle;
			} else if ( strncasecmp( p, "href=", 5 ) == 0 ) {
				p += 5;
				if ( *p == '#' ) { /* FIXME TODO */
					g_warning ("#references are not implemented yet.");
					KURL u( actualURL );
					u.setReference( p + 1 );
					href = u.url();
				}
				else 
				{
					KURL u( baseURL, p );
					href = u.url();
				}
			}
			else if ( strncasecmp( p, "target=", 7 ) == 0 )
			{
				atarget = p+7;
			}
			else if ( strncasecmp( p, "coords=", 7 ) == 0 )
			{
				coords = p+7;
			}
		}

		if ( !coords.isEmpty() && !href.isEmpty() )
		{
			HTMLArea *area = 0;

			switch ( shape )
			{
			case HTMLArea::Rect:
			{
				int x1, y1, x2, y2;
				sscanf( coords, "%d,%d,%d,%d", &x1, &y1, &x2, &y2 );
				QRect rect( x1, y1, x2-x1, y2-y1 );
				area = new HTMLArea( rect, href, atarget );
				debugM( "Area Rect %d, %d, %d, %d\n", x1, y1, x2, y2 );
			}
			break;

			case HTMLArea::Circle:
			{
				int xc, yc, rc;
				sscanf( coords, "%d,%d,%d", &xc, &yc, &rc );
				area = new HTMLArea( xc, yc, rc, href, atarget );
				debugM( "Area Circle %d, %d, %d\n", xc, yc, rc );
			}
			break;

			case HTMLArea::Poly:
			{
				debugM( "Area Poly " );
				int count = 0, x, y;
				QPointArray parray;
				const char *ptr = coords;
				while ( ptr )
				{
					x = atoi( ptr );
					ptr = strchr( ptr, ',' );
					if ( ptr )
					{
						y = atoi( ++ptr );
						parray.resize( count + 1 );
						parray.setPoint( count, x, y );
						debugM( "%d, %d  ", x, y );
						count++;
						ptr = strchr( ptr, ',' );
						if ( ptr ) ptr++;
					}
				}
				debugM( "\n" );
				if ( count > 2 )
					area = new HTMLArea( parray, href, atarget );
			}
			break;
			}

			if ( area )
				mapList.getLast()->addArea( area );
		}
	} else
#endif
		if ( strncmp( str, "address", 7) == 0 ) {
			push_clueflow_style (e, HTML_CLUEFLOW_STYLE_ADDRESS);
			close_flow (e, _clue);
			push_block (e, ID_ADDRESS, 2, block_end_clueflow_style,
				    e->divAlign, 0);
		} else if ( strncmp( str, "/address", 8) == 0 ) {
			pop_block (e, ID_ADDRESS, _clue);
		} else if ( strncmp( str, "a ", 2 ) == 0 ) {
			gchar *tmpurl = NULL;
			/* gchar *target = NULL; */

			const gchar *p;

			close_anchor (e);

			html_string_tokenizer_tokenize( e->st, str + 2, " >" );

			while ( ( p = html_string_tokenizer_next_token (e->st) ) != 0 ) {
				if ( strncasecmp( p, "href=", 5 ) == 0 ) {

					tmpurl = g_strdup (p + 5);

					/* FIXME visited? */
				} else if ( strncasecmp( p, "name=", 5 ) == 0 ) {
					if (e->flow == 0 )
						html_clue_append (HTML_CLUE (_clue),
								  html_anchor_new (p + 5));
					else
						html_clue_append (HTML_CLUE (e->flow),
								  html_anchor_new (p + 5));
				} else if ( strncasecmp( p, "target=", 7 ) == 0 ) {
#if 0							/* FIXME TODO */
					target = g_strdup (p + 7);
					parsedTargets.append( target );
#endif
				}
			}

#if 0
			if ( !target
			     && e->baseTarget != NULL
			     && e->baseTarget[0] != '\0' ) {
				target = g_strdup (e->baseTarget);
				/*  parsedTargets.append( target ); FIXME TODO */
			}
#endif

			if (tmpurl != NULL) {
				if (e->url != NULL)
					g_free (e->url);
				e->url = tmpurl;
			}

			if (e->url != NULL || e->target != NULL)
				push_color (e, gdk_color_copy ((GdkColor *) html_settings_get_color
							       (e->settings, HTMLLinkColor)));
		} else if ( strncmp( str, "/a", 2 ) == 0 ) {
			close_anchor (e);
		}
}


/*
  <b>              </b>
  <base
  <basefont                        unimplemented
  <big>            </big>
  <blockquote>     </blockquote>
  <body
  <br
*/
/* EP CHECK All done except for the color specifications in the `<body>'
   tag.  */
static void parse_b(HTMLEngine *e, HTMLObject *clue, const gchar *str) {
	GdkColor color;
	gchar *baseFace;

	if (strncmp (str, "basefont", 8) == 0) {
		baseFace = g_strdup(str + 8);
		push_font_face(e, baseFace);
	}
	else if ( strncmp(str, "base", 4 ) == 0 ) {
		html_string_tokenizer_tokenize( e->st, str + 5, " >" );
		while ( html_string_tokenizer_has_more_tokens (e->st) ) {
			const char* token = html_string_tokenizer_next_token(e->st);
			if ( strncasecmp( token, "target=", 7 ) == 0 ) {
				gtk_signal_emit (GTK_OBJECT (e), signals[SET_BASE_TARGET], token + 7);
			} else if ( strncasecmp( token, "href=", 5 ) == 0 ) {
				gtk_signal_emit (GTK_OBJECT (e), signals[SET_BASE], token + 5);
			}
		}
	}
	else if ( strncmp(str, "big", 3 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_SIZE_4);
		push_block (e, ID_BIG, 1, block_end_font, 0, 0);
	} else if ( strncmp(str, "/big", 4 ) == 0 ) {
		pop_block (e, ID_BIG, clue);
	} else if ( strncmp(str, "blockquote", 10 ) == 0 ) {
		push_block (e, ID_BLOCKQUOTE, 2, block_end_quote, e->indent_level, e->indent_level);
		e->avoid_para = TRUE;
		e->pending_para = FALSE;
		e->indent_level = e->indent_level + 1;
		close_flow (e, clue);
	} else if ( strncmp(str, "/blockquote", 11 ) == 0 ) {
		e->avoid_para = TRUE;
		pop_block (e, ID_BLOCKQUOTE, clue);
	} else if (strncmp (str, "body", 4) == 0) {
		html_string_tokenizer_tokenize (e->st, str + 5, " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			gchar *token;

			token = html_string_tokenizer_next_token (e->st);
			csc_html_debug_log (e->widget, "token is: %s\n", token);

			if (strncasecmp (token, "bgcolor=", 8) == 0) {
				csc_html_debug_log (e->widget, "setting color\n");
				if (parse_color (token + 8, &color)) {
					csc_html_debug_log (e->widget, "bgcolor is set\n");
					html_settings_set_color (e->settings, HTMLBgColor, &color);
				} else {
					csc_html_debug_log (e->widget, "Color `%s' could not be parsed\n", token);
				}
			} else if (strncasecmp (token, "background=", 11) == 0
				   && token [12]
				   && ! e->defaultSettings->forceDefault) {
				gchar *bgurl;

				bgurl = g_strdup (token + 11);
				if (e->bgPixmapPtr != NULL)
					html_image_factory_unregister(e->image_factory, e->bgPixmapPtr, NULL);
				e->bgPixmapPtr = html_image_factory_register(e->image_factory, NULL, bgurl);
				g_free (bgurl);
			} else if ( strncasecmp( token, "text=", 5 ) == 0
				    && !e->defaultSettings->forceDefault ) {
				if (parse_color (token + 5, &color)) {
					if (! html_stack_is_empty (e->color_stack))
						pop_color (e);
					html_settings_set_color (e->settings, HTMLTextColor, &color);
					push_color (e, gdk_color_copy (&color));
				}
			} else if ( strncasecmp( token, "link=", 5 ) == 0
				    && !e->defaultSettings->forceDefault ) {
				parse_color (token + 5, &color);
				html_settings_set_color (e->settings, HTMLLinkColor, &color);
			} else if ( strncasecmp( token, "vlink=", 6 ) == 0
				    && !e->defaultSettings->forceDefault ) {
				parse_color (token + 6, &color);
				html_settings_set_color (e->settings, HTMLVLinkColor, &color);
			} else if ( strncasecmp( token, "alink=", 6 ) == 0
				    && !e->defaultSettings->forceDefault ) {
				parse_color (token + 6, &color);
				html_settings_set_color (e->settings, HTMLALinkColor, &color);
			} else if ( strncasecmp( token, "leftmargin=", 11 ) == 0) {
				e->leftBorder = atoi (token + 11);
			} else if ( strncasecmp( token, "rightmargin=", 12 ) == 0) {
				e->rightBorder = atoi (token + 12);
			} else if ( strncasecmp( token, "topmargin=", 10 ) == 0) {
				e->topBorder = atoi (token + 10);
			} else if ( strncasecmp( token, "bottommargin=", 13 ) == 0) {
				e->bottomBorder = atoi (token + 13);
			} else if ( strncasecmp( token, "marginwidth=", 12 ) == 0) {
				e->leftBorder = e->rightBorder = atoi (token + 12);
			} else if ( strncasecmp( token, "marginheight=", 13 ) == 0) {
				e->topBorder = e->bottomBorder = atoi (token + 13);
			}
		}

		csc_html_debug_log (e->widget, "parsed <body>\n");
	}
	else if (strncmp (str, "br", 2) == 0) {
		HTMLClearType clear;

		clear = HTML_CLEAR_NONE;

		html_string_tokenizer_tokenize (e->st, str + 3, " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			gchar *token = html_string_tokenizer_next_token (e->st);
			
			if (strncasecmp (token, "clear=", 6) == 0) {
				csc_html_debug_log (e->widget, "%s\n", token);
				if (strncasecmp (token + 6, "left", 4) == 0)
					clear = HTML_CLEAR_LEFT;
				else if (strncasecmp (token + 6, "right", 5) == 0)
					clear = HTML_CLEAR_RIGHT;
				else if (strncasecmp (token + 6, "all", 3) == 0)
					clear = HTML_CLEAR_ALL;
			}
		}

		add_line_break (e, clue, clear);
	}
	else if (strncmp (str, "b", 1) == 0) {
		if (str[1] == '>' || str[1] == ' ') {
			push_font_style (e, CSC_HTML_FONT_STYLE_BOLD);
			push_block (e, ID_B, 1, block_end_font, FALSE, FALSE);
		}
	}
	else if (strncmp (str, "/b", 2) == 0) {
		pop_block (e, ID_B, clue);
	}
}


/*
  <center>         </center>
  <cite>           </cite>
  <code>           </code>
  <cell>           </cell>
  <comment>        </comment>      unimplemented
*/
/* EP CHECK OK except for the font in `<code>'.  */
static void
parse_c (HTMLEngine *e, HTMLObject *clue, const gchar *str)
{
	if (strncmp (str, "center", 6) == 0) {
		close_flow (e, clue);
		e->divAlign = HTML_HALIGN_CENTER;
	}
	else if (strncmp (str, "/center", 7) == 0) {
		close_flow (e, clue);
		e->divAlign = HTML_HALIGN_LEFT;
	}
	else if (strncmp( str, "cite", 4 ) == 0) {
		push_font_style (e, CSC_HTML_FONT_STYLE_ITALIC | CSC_HTML_FONT_STYLE_BOLD);
		push_block(e, ID_CITE, 1, block_end_font, 0, 0);
	}
	else if (strncmp( str, "/cite", 5) == 0) {
		pop_block (e, ID_CITE, clue);
	} else if (strncmp(str, "code", 4 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_FIXED);
		push_block (e, ID_CODE, 1, block_end_font, 0, 0);
	} else if (strncmp(str, "/code", 5 ) == 0 ) {
		pop_block (e, ID_CODE, clue);
	}
}


/*
  <dir             </dir>          partial
  <div             </div>
  <dl>             </dl>
  <dt>             </dt>
*/
/* EP CHECK: dl/dt might be wrong.  */
/* EP CHECK: dir might be wrong.  */
static void
parse_d ( HTMLEngine *e, HTMLObject *_clue, const char *str )
{
	if ( strncmp( str, "dir", 3 ) == 0 ) {
		close_anchor(e);
		push_block (e, ID_DIR, 2, block_end_list, e->indent_level, FALSE);
		html_stack_push (e->listStack, html_list_new (HTML_LIST_TYPE_DIR,
							      HTML_LIST_NUM_TYPE_NUMERIC));
		e->indent_level++;
		/* FIXME shouldn't it create a new flow? */
	} else if ( strncmp( str, "/dir", 4 ) == 0 ) {
		pop_block (e, ID_DIR, _clue);
	} else if ( strncmp( str, "div", 3 ) == 0 ) {
		push_block (e, ID_DIV, 1, block_end_div, e->divAlign, FALSE);

		html_string_tokenizer_tokenize( e->st, str + 4, " >" );
		while ( html_string_tokenizer_has_more_tokens (e->st) ) {
			const char* token = html_string_tokenizer_next_token (e->st);
			if ( strncasecmp( token, "align=", 6 ) == 0 ) {
				if ( strcasecmp( token + 6, "right" ) == 0 )
					e->divAlign = HTML_HALIGN_RIGHT;
				else if ( strcasecmp( token + 6, "center" ) == 0 )
					e->divAlign = HTML_HALIGN_CENTER;
				else if ( strcasecmp( token + 6, "left" ) == 0 )
					e->divAlign = HTML_HALIGN_LEFT;
			}
		}

		if (e->flow != NULL && HTML_CLUE (e->flow)->head != NULL)
			close_flow (e, _clue);
	} else if ( strncmp( str, "/div", 4 ) == 0 ) {
		pop_block (e, ID_DIV, _clue );
	} else if ( strncmp( str, "dl", 2 ) == 0 ) {
		close_anchor (e);
		if ( html_stack_top(e->glossaryStack) != NULL )
			e->indent_level++;
		html_stack_push (e->glossaryStack, GINT_TO_POINTER (HTML_GLOSSARY_DL));
		/* FIXME shouldn't it create a new flow? */
		add_line_break (e, _clue, HTML_CLEAR_ALL);
	} else if ( strncmp( str, "/dl", 3 ) == 0 ) {
		if ( html_stack_top (e->glossaryStack) == NULL)
			return;

		if ( GPOINTER_TO_INT (html_stack_top (e->glossaryStack)) == HTML_GLOSSARY_DD ) {
			html_stack_pop (e->glossaryStack);
			if (e->indent_level > 0)
				e->indent_level--;
		}

		html_stack_pop (e->glossaryStack);
		if ( html_stack_top (e->glossaryStack) != NULL ) {
			if (e->indent_level > 0)
				e->indent_level--;
		}

		add_line_break (e, _clue, HTML_CLEAR_ALL);
	} else if (strncmp( str, "dt", 2 ) == 0) {
		if (html_stack_top (e->glossaryStack) == NULL)
			return;

		if (GPOINTER_TO_INT (html_stack_top (e->glossaryStack)) == HTML_GLOSSARY_DD) {
			html_stack_pop (e->glossaryStack);
			if (e->indent_level > 0)
				e->indent_level--;
		}

		close_flow (e, _clue);
	} else if (strncmp( str, "dd", 2 ) == 0) {
		if (html_stack_top (e->glossaryStack) == NULL)
			return;

		if (GPOINTER_TO_INT (html_stack_top (e->glossaryStack)) != HTML_GLOSSARY_DD ) {
			html_stack_push (e->glossaryStack,
					 GINT_TO_POINTER (HTML_GLOSSARY_DD) );
			e->indent_level++;
		}

		close_flow (e, _clue);
	}
}


/*
  <em>             </em>
*/
/* EP CHECK: OK.  */
static void
parse_e (HTMLEngine *e, HTMLObject *_clue, const gchar *str)
{
	if ( strncmp( str, "em", 2 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_ITALIC);
		push_block (e, ID_EM, 1, block_end_font, FALSE, FALSE);
	} else if ( strncmp( str, "/em", 3 ) == 0 ) {
		pop_block (e, ID_EM, _clue);
	}
}

/*
  <font>           </font>
  <form>           </form>         partial
  <frame           <frame>
  <frameset        </frameset>
*/
static void parse_f(HTMLEngine *p, HTMLObject *clue, const gchar *str) {

	if (strncmp (str, "font", 4) == 0) {
		GdkColor *color;
		gint newSize;
		gchar *newFace = NULL;

		newSize = current_font_style(p) & CSC_HTML_FONT_STYLE_SIZE_MASK;

		/* The GdkColor API is not const safe!  */
		color = gdk_color_copy((GdkColor *)current_color(p));

		html_string_tokenizer_tokenize(p->st, str + 5, " >");

		while (html_string_tokenizer_has_more_tokens(p->st)) {
			const gchar *token = html_string_tokenizer_next_token(p->st);
			if (strncasecmp(token, "size=", 5) == 0) {
				gint num = atoi(token + 5);

				if (*(token + 5) == '+' || *(token + 5) == '-')
					newSize = CSC_HTML_FONT_STYLE_SIZE_3 + num;
				else
					newSize = num;
				if (newSize > CSC_HTML_FONT_STYLE_SIZE_MAX)
					newSize = CSC_HTML_FONT_STYLE_SIZE_MAX;
				else if (newSize < CSC_HTML_FONT_STYLE_SIZE_1)
					newSize = CSC_HTML_FONT_STYLE_SIZE_1;
			} else if (strncasecmp(token, "face=", 5) == 0) {
				newFace = g_strdup(token + 5);
			} else if (strncasecmp (token, "color=", 6) == 0) {
				parse_color (token + 6, color);
			}
		}

		push_color(p, color);
		push_font_style(p, newSize);
		if (newFace != NULL) {
			push_font_face(p, newFace);
		}

		push_block(p, ID_FONT, 1, block_end_color_font, FALSE, FALSE);
		
	} else if (strncmp (str, "/font", 5) == 0) {
		pop_block(p, ID_FONT, clue);
		
	} else if (strncmp (str, "form", 4) == 0) {
                gchar *action = NULL;
                gchar *method = "GET";
                gchar *target = NULL;

		html_string_tokenizer_tokenize(p->st, str + 5, " >");
		while (html_string_tokenizer_has_more_tokens (p->st)) {
			const gchar *token = html_string_tokenizer_next_token(p->st);

                        if (strncasecmp(token, "action=", 7) == 0) {
                                action = g_strdup(token + 7);
                        } else if (strncasecmp(token, "method=", 7) == 0) {
                                if (strncasecmp(token + 7, "post", 4) == 0)
                                        method = "POST";
                        } else if (strncasecmp(token, "target=", 7) == 0) {
				target = g_strdup(token + 7);
                        }
                }

                p->form = html_form_new (p, action, method);
                p->formList = g_list_append (p->formList, p->form);
		
		if (action)
			g_free(action);
		if (target)
			g_free(target);

		if (! p->avoid_para) {
			close_anchor (p);
			p->avoid_para = TRUE;
			p->pending_para = TRUE;
		}
	} else if (strncmp (str, "/form", 5) == 0) {
		p->form = NULL;

		if (! p->avoid_para) {
			close_anchor (p);
			p->avoid_para = TRUE;
			p->pending_para = TRUE;
		}
	}
}


/*
  <h[1-6]>         </h[1-6]>
  <hr
*/
/* EP CHECK: OK */
static void
parse_h (HTMLEngine *p, HTMLObject *clue, const gchar *str)
{
	if (*str == 'h'
	    && (str[1] >= '1' && str[1] <= '6')) {
		HTMLHAlignType align;

		align = p->divAlign;

		html_string_tokenizer_tokenize (p->st, str + 3, " >");
		while (html_string_tokenizer_has_more_tokens (p->st)) {
			const gchar *token;

			token = html_string_tokenizer_next_token (p->st);
			if ( strncasecmp( token, "align=", 6 ) == 0 ) {
				if ( strcasecmp( token + 6, "center" ) == 0 )
					align = HTML_HALIGN_CENTER;
				else if ( strcasecmp( token + 6, "right" ) == 0 )
					align = HTML_HALIGN_RIGHT;
				else if ( strcasecmp( token + 6, "left" ) == 0 )
					align = HTML_HALIGN_LEFT;
			}
		}
		
		/* Start a new flow box */

		push_clueflow_style (p, HTML_CLUEFLOW_STYLE_H1 + (str[1] - '1'));
		close_flow (p, clue);

		push_block (p, ID_HEADER, 2, block_end_clueflow_style, p->divAlign, 0);

		p->divAlign = align;

		p->pending_para = FALSE;
		p->avoid_para = TRUE;
	} else if (*(str) == '/' && *(str + 1) == 'h'
		   && (*(str + 2) >= '1' && *(str + 2) <= '6')) {
		/* Close tag.  */
		pop_block (p, ID_HEADER, clue);
		p->avoid_para = TRUE;
		p->pending_para = FALSE;
	}
	else if (strncmp (str, "hr", 2) == 0) {
		gint size = 2;
		gint length = clue->max_width;
		gint percent = 100;
		HTMLHAlignType align = HTML_HALIGN_CENTER;
		gboolean shade = TRUE;

		close_flow (p, clue);

		html_string_tokenizer_tokenize (p->st, str + 3, " >");
		while (html_string_tokenizer_has_more_tokens (p->st)) {
			gchar *token = html_string_tokenizer_next_token (p->st);
			if (strncasecmp (token, "align=", 6) == 0) {
				if (strcasecmp (token + 6, "left") == 0)
					align = HTML_HALIGN_LEFT;
				else if (strcasecmp (token + 6, "right") == 0)
					align = HTML_HALIGN_RIGHT;
				else if (strcasecmp (token + 6, "center") == 0)
					align = HTML_HALIGN_CENTER;
			}
			else if (strncasecmp (token, "size=", 5) == 0) {
				size = atoi (token + 5);
			}
			else if (strncasecmp (token, "width=", 6) == 0) {
				if (strchr (token + 6, '%'))
					percent = atoi (token + 6);
				else if (isdigit (*(token + 6))) {
					length = atoi (token + 6);
					percent = 0;
				}
			}
			else if (strncasecmp (token, "noshade", 6) == 0) {
				shade = FALSE;
			}
		}

		append_element (p, clue, html_rule_new (length, percent, size, shade, align));
		close_flow (p, clue);
	}
}


/*
  <i>              </i>
  <img                             partial
  <input                           partial
  <iframe                          partial
*/
/* EP CHECK: map support missing.  `<input>' missing.  */
static void
parse_i (HTMLEngine *p, HTMLObject *_clue, const gchar *str)
{
	if (strncmp (str, "img", 3) == 0) {
		HTMLObject *image = 0;
		gchar *token = 0; 
		gint width = -1;
		gchar *tmpurl = NULL;
		gint height = -1;
		gint percent = 0;
		gint hspace = 0;
		gint vspace = 0;
		HTMLHAlignType align = HTML_HALIGN_NONE;
		gint border = 0;
		HTMLVAlignType valign = HTML_VALIGN_NONE;
		const GdkColor *color = NULL;
		
		color = current_color (p);

		if (p->url != NULL || p->target != NULL)
			border = 2;

		html_string_tokenizer_tokenize (p->st, str + 4, " >");
		while (html_string_tokenizer_has_more_tokens (p->st)) {
			token = html_string_tokenizer_next_token (p->st);
			if (strncasecmp (token, "src=", 4) == 0) {
				tmpurl = g_strdup (token + 4);
			} else if (strncasecmp (token, "width=", 6) == 0) {
				if (strchr (token + 6, '%'))
					percent = atoi (token + 6);
				else if (isdigit (*(token + 6)))
					width = atoi (token + 6);
			}
			else if (strncasecmp (token, "height=", 7) == 0) {
				height = atoi (token + 7);
			}
			else if (strncasecmp (token, "border=", 7) == 0) {
				border = atoi (token + 7);
			}
			else if (strncasecmp (token, "hspace=", 7) == 0) {
				hspace = atoi (token + 7);
			}
			else if (strncasecmp (token, "vspace=", 7) == 0) {
				vspace = atoi (token + 7);
			}
			else if (strncasecmp (token, "align=", 6) == 0) {
				if (strcasecmp (token + 6, "left") == 0)
					align = HTML_HALIGN_LEFT;
				else if (strcasecmp (token + 6, "right") == 0)
					align = HTML_HALIGN_RIGHT;
				else if (strcasecmp (token + 6, "top") == 0)
					valign = HTML_VALIGN_TOP;
				else if (strcasecmp (token + 6, "center") == 0)
					valign = HTML_VALIGN_CENTER;
				else if (strcasecmp (token + 6, "bottom") ==0)
					valign = HTML_VALIGN_BOTTOM;
			}
#if 0							/* FIXME TODO map support */
			else if ( strncasecmp( token, "usemap=", 7 ) == 0 )
			{
				if ( *(token + 7 ) == '#' )
				{
					// Local map. Format: "#name"
					usemap = token + 7;
				}
				else
				{
					KURL u( baseURL, token + 7 );
					usemap = u.url();
				}
			}
			else if ( strncasecmp( token, "ismap", 5 ) == 0 )
			{
				ismap = true;
			}
#endif
		}

		if (tmpurl != 0) {
			if (align != HTML_HALIGN_NONE)
				valign = HTML_VALIGN_BOTTOM;
			else if (valign == HTML_VALIGN_NONE)
				valign = HTML_VALIGN_BOTTOM;

			image = html_image_new (p->image_factory, tmpurl,
						p->url, p->target,
						width, height,
						percent, border, color, valign);

			if (hspace < 0)
				hspace = 0;
			if (vspace < 0)
				vspace = 0;

			html_image_set_spacing (HTML_IMAGE (image), hspace, vspace);

			g_free(tmpurl);
				
			if (align == HTML_HALIGN_NONE) {
				append_element (p, _clue, image);
			} else {
				/* We need to put the image in a HTMLClueAligned.  */
				/* Man, this is *so* gross.  */
				HTMLClueAligned *aligned = HTML_CLUEALIGNED (html_cluealigned_new (NULL, 0, 0, _clue->max_width, 100));
				HTML_CLUE (aligned)->halign = align;
				html_clue_append (HTML_CLUE (aligned), HTML_OBJECT (image));
				append_element (p, _clue, HTML_OBJECT (aligned));
			}
		}		       
	}
	else if (strncmp( str, "input", 5 ) == 0) {
		if (p->form == NULL)
			return;
		
		parse_input( p, str + 6, _clue );
	} else if (strncmp( str, "iframe", 6) == 0) {
		parse_iframe (p, str + 7, _clue);
	} else if ( strncmp (str, "i", 1 ) == 0 ) {
		if ( str[1] == '>' || str[1] == ' ' ) {
			push_font_style (p, CSC_HTML_FONT_STYLE_ITALIC);
			push_block (p, ID_I, 1, block_end_font, FALSE, FALSE);
		}
	} else if ( strncmp( str, "/i", 2 ) == 0 ) {
		pop_block (p, ID_I, _clue);
	}
}


/*
  <kbd>            </kbd>
*/
/* EP CHECK: OK but font is wrong.  */
static void
parse_k (HTMLEngine *e, HTMLObject *_clue, const gchar *str)
{
	if ( strncmp(str, "kbd", 3 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_FIXED);
		push_block (e, ID_KBD, 1, block_end_font, 0, 0);
	} else if ( strncmp(str, "/kbd", 4 ) == 0 ) {
		pop_block (e, ID_KBD, _clue);
	}
}


/*
  <listing>        </listing>      unimplemented
  <li>
*/
/* EP CHECK: OK */
static void
parse_l (HTMLEngine *p, HTMLObject *clue, const gchar *str)
{
	if (strncmp (str, "link", 4) == 0) {
	}
	else if (strncmp (str, "li", 2) == 0) {
		HTMLListType listType;
		HTMLListNumType listNumType;
		gint listLevel;
		gint itemNumber;

		listType = HTML_LIST_TYPE_UNORDERED;
		listNumType = HTML_LIST_NUM_TYPE_NUMERIC;

		listLevel = 1;
		itemNumber = 1;

		close_anchor (p);

		if (! html_stack_is_empty (p->listStack)) {
			HTMLList *top;

			top = html_stack_top (p->listStack);

			listType = top->type;
			listNumType = top->numType;
			itemNumber = top->itemNumber;

			listLevel = html_stack_count (p->listStack);
		}

		if (p->pending_para) {
			insert_paragraph_break (p, clue);
			p->pending_para = FALSE;
		}

		close_flow (p, clue);

		p->flow = html_clueflow_new (HTML_CLUEFLOW_STYLE_ITEMDOTTED, p->indent_level);
		html_clue_append (HTML_CLUE (clue), p->flow);

		p->avoid_para = TRUE;

		if (! html_stack_is_empty (p->listStack)) {
			HTMLList *list;

			list = html_stack_top (p->listStack);
			list->itemNumber++;
		}
	}
}

/*
 <meta>
*/

static void
parse_m (HTMLEngine *e, HTMLObject *_clue, const gchar *str )
{
	int refresh = 0;
	int refresh_delay = 0;
	gchar *refresh_url = NULL;

	if ( strncmp( str, "meta", 4 ) == 0 ) {
		html_string_tokenizer_tokenize( e->st, str + 5, " >" );
		while ( html_string_tokenizer_has_more_tokens (e->st) ) {

			const gchar* token = html_string_tokenizer_next_token(e->st);
			if ( strncasecmp( token, "http-equiv=", 11 ) == 0 ) {
				if ( strncasecmp( token + 11, "refresh", 7 ) == 0 )
					refresh = 1;
			} else if ( strncasecmp( token, "content=", 8 ) == 0 ) {
				if(refresh) {
					const gchar *content;
					content = token + 8;

					/* The time in seconds until the refresh */
					refresh_delay = atoi(content);

					html_string_tokenizer_tokenize(e->st, content, ",;> ");
					while ( html_string_tokenizer_has_more_tokens (e->st) ) {
						const gchar* token = html_string_tokenizer_next_token(e->st);
						if ( strncasecmp( token, "url=", 4 ) == 0 )
							refresh_url = g_strdup (token + 4);
					}
					
					gtk_signal_emit (GTK_OBJECT (e), signals[REDIRECT], refresh_url, refresh_delay);
					
					if(refresh_url)
						g_free(refresh_url);
				}
			}
		}
	}
}

/* FIXME TODO parse_n missing. */

/* called when some state in an embedded html object has changed ... do a redraw */
static void
html_object_changed(CscHTMLEmbedded *eb, HTMLEngine *e)
{
	HTMLEmbedded *el;

	el = gtk_object_get_data(GTK_OBJECT(eb), "embeddedelement");
	if (el)
		html_embedded_size_recalc(el);

	html_engine_schedule_update(e);
}


/*
<ol>             </ol>           partial
<option
<object
*/
/* EP CHECK: `<ol>' does not handle vspace correctly.  */
static void
parse_o (HTMLEngine *e, HTMLObject *_clue, const gchar *str )
{
	if ( strncmp( str, "ol", 2 ) == 0 ) {
		HTMLListNumType listNumType;
		HTMLList *list;

		close_anchor (e);

		if ( html_stack_is_empty (e->listStack) ) {
			/* FIXME */
			push_block (e, ID_OL, 2, block_end_list, e->indent_level, TRUE);
		} else {
			push_block (e, ID_OL, 2, block_end_list, e->indent_level, FALSE);
		}

		listNumType = HTML_LIST_NUM_TYPE_NUMERIC;

		html_string_tokenizer_tokenize( e->st, str + 3, " >" );

		while ( html_string_tokenizer_has_more_tokens (e->st) ) {
			const char* token;

			token = html_string_tokenizer_next_token (e->st);

			if ( strncasecmp( token, "type=", 5 ) == 0 ) {
				switch ( *(token+5) )
				{
				case '1':
					listNumType = HTML_LIST_NUM_TYPE_NUMERIC;
					break;
				case 'i':
					listNumType = HTML_LIST_NUM_TYPE_LOWROMAN;
					break;

				case 'I':
					listNumType = HTML_LIST_NUM_TYPE_UPROMAN;
					break;

				case 'a':
					listNumType = HTML_LIST_NUM_TYPE_LOWALPHA;
					break;

				case 'A':
					listNumType = HTML_LIST_NUM_TYPE_UPALPHA;
					break;
				}
			}
		}

		list = html_list_new (HTML_LIST_TYPE_ORDERED, listNumType);
		html_stack_push (e->listStack, list);

		e->indent_level++;
	}
	else if ( strncmp( str, "/ol", 3 ) == 0 ) {
		pop_block (e, ID_OL, _clue);
	}
	else if ( strncmp( str, "option", 6 ) == 0 ) {
		gchar *value = NULL;
		gboolean selected = FALSE;

		if ( !e->formSelect )
			return;

		html_string_tokenizer_tokenize( e->st, str + 3, " >" );

		while ( html_string_tokenizer_has_more_tokens (e->st) ) {
			const char* token;
			
			token = html_string_tokenizer_next_token (e->st);
			
			if ( strncasecmp( token, "value=", 6 ) == 0 ) {

				value = g_strdup (token + 6);
			}
			else if ( strncasecmp( token, "selected", 8 ) == 0 ) {

				selected = TRUE;
			}
		}

		if ( e->inOption )
			html_select_set_text (e->formSelect, e->formText->str);

		html_select_add_option (e->formSelect, value, selected );

		e->inOption = TRUE;
		g_string_assign (e->formText, "");
	} else if ( strncmp( str, "/option", 7 ) == 0 ) {
		if ( e->inOption )
			html_select_set_text (e->formSelect, e->formText->str);

		e->inOption = FALSE;
	} else if ( strncmp( str, "object", 6 ) == 0 ) {
		parse_object (e, _clue, _clue->max_width, str + 6);
	}
}


/*
  <p
  <pre             </pre>
*/
/* EP CHECK: OK except for the `<pre>' font.  */
static void
parse_p (HTMLEngine *e, HTMLObject *clue, const gchar *str)
{
	if ( strncmp( str, "pre", 3 ) == 0 ) {
		close_flow (e, clue);
		push_clueflow_style (e, HTML_CLUEFLOW_STYLE_PRE);
		e->inPre = TRUE;
		push_block (e, ID_PRE, 2, block_end_pre, e->divAlign, 0);
	} else if ( strncmp( str, "/pre", 4 ) == 0 ) {
		pop_block (e, ID_PRE, clue);
		close_flow (e, clue);
	} else if ( strncmp( str, "param", 5) == 0 ) {
		if (! html_stack_is_empty (e->embeddedStack)) {
			CscHTMLEmbedded *eb;
			char *name = NULL, *value = NULL;

			eb = html_stack_top (e->embeddedStack);
			html_string_tokenizer_tokenize (e->st, str + 6, " >");
			while ( html_string_tokenizer_has_more_tokens (e->st) ) {
				const char *token = html_string_tokenizer_next_token (e->st);
				if ( strncasecmp( token, "name=", 5 ) == 0 ) {
					name = g_strdup(token+5);
				} else if ( strncasecmp( token, "value=", 6 ) == 0 ) {
					value = g_strdup(token+6);
				}
			}

			if (name!=NULL) {
				csc_html_embedded_set_parameter(eb, name, value);
			}
			g_free(name);
			g_free(value);
		}					
	} else if (*(str) == 'p' && ( *(str + 1) == ' ' || *(str + 1) == '>')) {
		gchar *token;

		push_block (e, ID_DIV, 1, block_end_div, e->divAlign, FALSE);

		html_string_tokenizer_tokenize (e->st, (gchar *)(str + 2), " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			token = html_string_tokenizer_next_token (e->st);
			if (strncasecmp (token, "align=", 6) == 0) {
				if (strcasecmp (token + 6, "center") == 0)
					e->divAlign = HTML_HALIGN_CENTER;
				else if (strcasecmp (token + 6, "right") == 0)
					e->divAlign = HTML_HALIGN_RIGHT;
				else if (strcasecmp (token + 6, "left") == 0)
					e->divAlign = HTML_HALIGN_LEFT;
			}
		}

		if (! e->avoid_para) {
			close_anchor (e);
			e->avoid_para = TRUE;
			e->pending_para = TRUE;
		}
	} else if (*(str) == '/' && *(str + 1) == 'p'
		   && (*(str + 2) == ' ' || *(str + 2) == '>')) {

		pop_block (e, ID_DIV, clue );

		if (! e->avoid_para) {

			e->avoid_para = TRUE;
			e->pending_para = TRUE;
		}
	}
}


/*
  <select>            </select>
  <small>             </small>
  <strong>            </strong>
*/
static void
parse_s (HTMLEngine *e, HTMLObject *clue, const gchar *str)
{
	if (strncmp (str, "small", 3) == 0) {
		push_font_style (e, CSC_HTML_FONT_STYLE_SIZE_2);
		push_block (e, ID_SMALL, 1, block_end_font, 0, 0);
	} else if (strncmp (str, "/small", 4) == 0 ) {
		pop_block (e, ID_SMALL, clue);
	} else if (strncmp (str, "strong", 6) == 0) {
		push_font_style (e, CSC_HTML_FONT_STYLE_BOLD);
		push_block (e, ID_STRONG, 1, block_end_font, 0, 0);
	} else if (strncmp (str, "/strong", 7) == 0) {
		pop_block (e, ID_STRONG, clue);
	} else if (strncmp (str, "select", 6) == 0) {
                gchar *name = NULL;
		gint size = 0;
		gboolean multi = FALSE;

		if (!e->form)
			return;
                    
		html_string_tokenizer_tokenize (e->st, str + 7, " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			const gchar *token = html_string_tokenizer_next_token (e->st);

                        if ( strncasecmp( token, "name=", 5 ) == 0 )
                        {
				name = g_strdup(token + 5);
                        }
                        else if ( strncasecmp( token, "size=", 5 ) == 0 )
                        {
				size = atoi (token + 5);
                        }
                        else if ( strncasecmp( token, "multiple", 8 ) == 0 )
                        {
				multi = TRUE;
                        }
                }
                
                e->formSelect = HTML_SELECT (html_select_new (GTK_WIDGET(e->widget), name, size, multi));
                html_form_add_element (e->form, HTML_EMBEDDED ( e->formSelect ));

		append_element (e, clue, HTML_OBJECT (e->formSelect));
		
		if (name)
			g_free(name);
	}
	else if (strncmp (str, "/select", 7) == 0) {
		if ( e->inOption )
			html_select_set_text (e->formSelect, e->formText->str);

		e->inOption = FALSE;
		e->formSelect = NULL;
	}
}


/*
  <table           </table>        most
  <textarea        </textarea>
  <title>          </title>
  <tt>             </tt>
*/
/* EP CHECK: `<tt>' uses the wrong font.  `<textarea>' is missing.  Rest is
   OK.  */
static void
parse_t (HTMLEngine *e, HTMLObject *clue, const gchar *str)
{
	if (strncmp (str, "table", 5) == 0) {
		close_anchor (e);

		close_flow (e, clue);
		parse_table (e, clue, clue->max_width, str + 6);
		close_flow (e, clue);

		e->avoid_para = FALSE;
	}
	else if (strncmp (str, "title", 5) == 0) {
		e->inTitle = TRUE;
		e->title = g_string_new ("");
	}
	else if (strncmp (str, "/title", 6) == 0) {
		e->inTitle = FALSE;

		gtk_signal_emit (GTK_OBJECT (e), signals[TITLE_CHANGED]);
	}
	else if ( strncmp( str, "tt", 2 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_FIXED);
		push_block (e, ID_TT, 1, block_end_font, 0, 0);
	} else if ( strncmp( str, "/tt", 3 ) == 0 ) {
		pop_block (e, ID_TT, clue);
	}
	else if (strncmp (str, "textarea", 8) == 0) {
                gchar *name = NULL;
		gint rows = 5, cols = 40;

		if (!e->form)
			return;
                    
		html_string_tokenizer_tokenize (e->st, str + 9, " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			const gchar *token = html_string_tokenizer_next_token (e->st);

                        if ( strncasecmp( token, "name=", 5 ) == 0 )
                        {
				name = g_strdup(token + 5);
                        }
                        else if ( strncasecmp( token, "rows=", 5 ) == 0 )
                        {
				rows = atoi (token + 5);
                        }
                        else if ( strncasecmp( token, "cols=", 5 ) == 0 )
                        {
				cols = atoi (token + 5);
                        }
                }
                
                e->formTextArea = HTML_TEXTAREA (html_textarea_new (GTK_WIDGET(e->widget), name, rows, cols));
                html_form_add_element (e->form, HTML_EMBEDDED ( e->formTextArea ));

		append_element (e, clue, HTML_OBJECT (e->formTextArea));

		g_string_assign (e->formText, "");
		e->inTextArea = TRUE;

		push_block(e, ID_TEXTAREA, 3, NULL, 0, 0);
		
		if(name)
			g_free(name);
	}
	else if (strncmp (str, "/textarea", 9) == 0) {
		pop_block(e, ID_TEXTAREA, clue);

		if ( e->inTextArea )
			html_textarea_set_text (e->formTextArea, e->formText->str);

		e->inTextArea = FALSE;
		e->formTextArea = NULL;
	}

}


/*
  <u>              </u>
  <ul              </ul>
*/
/* EP CHECK: OK */
static void
parse_u (HTMLEngine *e, HTMLObject *clue, const gchar *str)
{
	if (strncmp (str, "ul", 2) == 0) {
		HTMLListType type;

		close_anchor (e);
		close_flow (e, clue);

		if (html_stack_is_empty (e->listStack))
			push_block (e, ID_UL, 2, block_end_list, e->indent_level, TRUE);
		else
			push_block (e, ID_UL, 2, block_end_list, e->indent_level, FALSE);

		type = HTML_LIST_TYPE_UNORDERED;

		html_string_tokenizer_tokenize (e->st, str + 3, " >");
		while (html_string_tokenizer_has_more_tokens (e->st)) {
			gchar *token = html_string_tokenizer_next_token (e->st);
			if (strncasecmp (token, "plain", 5) == 0)
				type = HTML_LIST_TYPE_UNORDEREDPLAIN;
		}
		
		html_stack_push (e->listStack, html_list_new (type, HTML_LIST_NUM_TYPE_NUMERIC));
		e->flow = NULL;

		if (e->pending_para && e->indent_level > 0)
			insert_paragraph_break (e, clue);

		e->indent_level++;

		e->avoid_para = TRUE;
		e->pending_para = FALSE;
	} else if (strncmp (str, "/ul", 3) == 0) {
		pop_block (e, ID_UL, clue);
	}
	else if (strncmp (str, "u", 1) == 0) {
		if (str[1] == '>' || str[1] == ' ') {
			push_font_style (e, CSC_HTML_FONT_STYLE_UNDERLINE);
			push_block (e, ID_U, 1, block_end_font, FALSE, FALSE);
		}
	}
	else if (strncmp (str, "/u", 2) == 0) {
		pop_block (e, ID_U, clue);
	}
}


/*
  <var>            </var>
*/
/* EP CHECK: OK */
static void
parse_v (HTMLEngine *e, HTMLObject * _clue, const char *str )
{
	if ( strncmp(str, "var", 3 ) == 0 ) {
		push_font_style (e, CSC_HTML_FONT_STYLE_FIXED);
	   	push_block(e, ID_VAR, 1, block_end_font, 0, 0);
	} else if ( strncmp( str, "/var", 4 ) == 0) {
		pop_block(e, ID_VAR, _clue);
	}
}


/* Parsing vtable.  */

typedef void (*HTMLParseFunc)(HTMLEngine *p, HTMLObject *clue, const gchar *str);
static HTMLParseFunc parseFuncArray[26] = {
	parse_a,
	parse_b,
	parse_c,
	parse_d,
	parse_e,
	parse_f,
	NULL,
	parse_h,
	parse_i,
	NULL,
	parse_k,
	parse_l,
	parse_m,
	NULL,
	parse_o,
	parse_p,
	NULL,
	NULL,
	parse_s,
	parse_t,
	parse_u,
	parse_v,
	NULL,
	NULL,
	NULL,
	NULL
};

static void
parse_one_token (HTMLEngine *p, HTMLObject *clue, const gchar *str)
{
	if (*str == '<') {
		gint indx;
		
		str++;
		
		if (*str == '/')
			indx = *(str + 1) - 'a';
		else
			indx = *str - 'a';

		if (indx >= 0 && indx < 26) {
			/* FIXME: This should be removed */
			if (parseFuncArray[indx] != NULL) {
				(* parseFuncArray[indx])(p, clue, str);
			} else {
				g_warning ("Unsupported tag `%s'", str);
			}
		}

	}
}


guint
html_engine_get_type (void)
{
	static guint html_engine_type = 0;

	if (!html_engine_type) {
		static const GtkTypeInfo html_engine_info = {
			"HTMLEngine",
			sizeof (HTMLEngine),
			sizeof (HTMLEngineClass),
			(GtkClassInitFunc) html_engine_class_init,
			(GtkObjectInitFunc) html_engine_init,
			/* reserved_1 */ NULL,
			/* reserved_2 */ NULL,
			(GtkClassInitFunc) NULL,
		};
		
		html_engine_type = gtk_type_unique (GTK_TYPE_OBJECT, &html_engine_info);
	}

	return html_engine_type;
}

static void
html_engine_destroy (GtkObject *object)
{
	HTMLEngine *engine;
	GList *p;

	engine = HTML_ENGINE (object);

	html_undo_destroy (engine->undo);

	if (engine->cut_buffer != NULL)
		html_engine_cut_buffer_destroy (engine->cut_buffer);

	html_color_set_destroy (engine->color_set);

	if (engine->invert_gc != NULL)
		gdk_gc_destroy (engine->invert_gc);

	html_cursor_destroy (engine->cursor);
	if (engine->mark != NULL)
		html_cursor_destroy (engine->mark);

	html_tokenizer_destroy (engine->ht);
	html_string_tokenizer_destroy (engine->st);
	html_settings_destroy (engine->settings);
	html_settings_destroy (engine->defaultSettings);
	if (engine->clue != NULL)
		html_object_destroy (engine->clue);
	engine->clue = NULL;
	html_image_factory_free (engine->image_factory);

	gtk_object_destroy (GTK_OBJECT (engine->painter));

	html_stack_destroy (engine->color_stack);
	html_stack_destroy (engine->font_style_stack);
	html_stack_destroy (engine->font_face_stack);
	html_stack_destroy (engine->clueflow_style_stack);

	html_stack_destroy (engine->listStack);
	html_stack_destroy (engine->glossaryStack);
	html_stack_destroy (engine->embeddedStack);

	for (p = engine->tempStrings; p != NULL; p = p->next)
		g_free (p->data);
	g_list_free (engine->tempStrings);

	html_draw_queue_destroy (engine->draw_queue);

	html_engine_edit_selection_updater_destroy (engine->selection_updater);

	if (engine->search_info)
		html_search_destroy (engine->search_info);

        /* Finally, destroy timers.  */

	if (engine->timerId != 0)
		gtk_timeout_remove (engine->timerId);
	if (engine->blinking_timer_id != 0)
		gtk_timeout_remove (engine->blinking_timer_id);
	if (engine->updateTimer != 0)
		gtk_timeout_remove (engine->updateTimer);

	GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
html_engine_class_init (HTMLEngineClass *klass)
{
	GtkObjectClass *object_class;

	object_class = (GtkObjectClass *)klass;

	parent_class = gtk_type_class (GTK_TYPE_OBJECT);

	signals [SET_BASE] =
		gtk_signal_new ("set_base",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, set_base),
				gtk_marshal_NONE__STRING,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_STRING);

	signals [SET_BASE_TARGET] =
		gtk_signal_new ("set_base_target",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, set_base_target),
				gtk_marshal_NONE__STRING,
				GTK_TYPE_NONE, 1,
				GTK_TYPE_STRING);

	signals [LOAD_DONE] = 
		gtk_signal_new ("load_done",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, load_done),
				gtk_marshal_NONE__NONE,
				GTK_TYPE_NONE, 0);

	signals [TITLE_CHANGED] = 
		gtk_signal_new ("title_changed",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, title_changed),
				gtk_marshal_NONE__NONE,
				GTK_TYPE_NONE, 0);

	signals [URL_REQUESTED] =
		gtk_signal_new ("url_requested",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, url_requested),
				cschtml_VOID__STRING_POINTER,
				GTK_TYPE_NONE, 2,
				GTK_TYPE_STRING,
				GTK_TYPE_POINTER);

	signals [DRAW_PENDING] =
		gtk_signal_new ("draw_pending",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, draw_pending),
				gtk_marshal_NONE__NONE,
				GTK_TYPE_NONE, 0);

	signals [REDIRECT] =
		gtk_signal_new ("redirect",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, redirect),
				gtk_marshal_NONE__POINTER_INT,
				GTK_TYPE_NONE, 2,
				GTK_TYPE_STRING,
				GTK_TYPE_INT);

	signals [SUBMIT] =
		gtk_signal_new ("submit",
				GTK_RUN_FIRST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, submit),
				gtk_marshal_NONE__POINTER_POINTER_POINTER,
				GTK_TYPE_NONE, 3,
				GTK_TYPE_STRING,
				GTK_TYPE_STRING,
				GTK_TYPE_STRING);

	signals [OBJECT_REQUESTED] =
		gtk_signal_new ("object_requested",
				GTK_RUN_LAST,
				G_TYPE_FROM_CLASS (object_class),
				GTK_SIGNAL_OFFSET (HTMLEngineClass, object_requested),
				gtk_marshal_BOOL__POINTER,
				GTK_TYPE_BOOL, 1,
				GTK_TYPE_POINTER);

	object_class->destroy = html_engine_destroy;

	/* Initialize the HTML objects.  */
	html_types_init ();
}

static void
html_engine_init (HTMLEngine *engine)
{
	/* STUFF might be missing here!   */

	engine->freeze_count = 0;

	engine->window = NULL;
	engine->invert_gc = NULL;

	engine->color_set = html_color_set_new ();
	engine->painter = html_gdk_painter_new (TRUE);
	html_painter_set_color_set (engine->painter, engine->color_set);
	
	engine->newPage = FALSE;

	engine->editable = FALSE;
	engine->cut_buffer = NULL;

	engine->ht = html_tokenizer_new ();
	engine->st = html_string_tokenizer_new ();
	engine->settings = html_settings_new ();
	engine->defaultSettings = html_settings_new ();
	engine->image_factory = html_image_factory_new(engine);

	engine->undo = html_undo_new ();

	engine->font_style_stack = html_stack_new (NULL);
	engine->font_face_stack = html_stack_new (NULL);
	engine->color_stack = html_stack_new ((HTMLStackFreeFunc) gdk_color_free);
	engine->clueflow_style_stack = html_stack_new (NULL);

	engine->listStack = html_stack_new ((HTMLStackFreeFunc) html_list_destroy);
	engine->glossaryStack = html_stack_new (NULL);
	engine->embeddedStack = html_stack_new ((HTMLStackFreeFunc) gtk_object_unref);

	engine->url = NULL;
	engine->target = NULL;

	engine->leftBorder = LEFT_BORDER;
	engine->rightBorder = RIGHT_BORDER;
	engine->topBorder = TOP_BORDER;
	engine->bottomBorder = BOTTOM_BORDER;

	engine->inPre = FALSE;
	engine->noWrap = FALSE;
	engine->inTitle = FALSE;

	engine->tempStrings = NULL;

	engine->draw_queue = html_draw_queue_new (engine);

	engine->formList = NULL;

	engine->avoid_para = TRUE;
	engine->pending_para = FALSE;

	engine->indent_level = 0;

	engine->have_focus = FALSE;

	engine->cursor = html_cursor_new ();
	engine->mark = NULL;
	engine->cursor_hide_count = 0;
	engine->blinking_timer_id = 0;
	engine->blinking_status = FALSE;
	engine->insertion_font_style = CSC_HTML_FONT_STYLE_DEFAULT;
	engine->insertion_font_face = NULL;
	engine->active_selection = FALSE;

	engine->selection_updater = html_engine_edit_selection_updater_new (engine);

	engine->search_info = NULL;
}

HTMLEngine *
html_engine_new (void)
{
	HTMLEngine *engine;

	engine = gtk_type_new (html_engine_get_type ());

	return engine;
}

void
html_engine_realize (HTMLEngine *e,
		     GdkWindow *window)
{
	GdkGCValues gc_values;

	g_return_if_fail (e != NULL);
	g_return_if_fail (window != NULL);

	e->window = window;

	html_gdk_painter_realize (HTML_GDK_PAINTER (e->painter), window);

	gc_values.function = GDK_INVERT;
	e->invert_gc = gdk_gc_new_with_values (e->window, &gc_values, GDK_GC_FUNCTION);
}


/* This function makes sure @engine can be edited properly.  In order
   to be editable, the beginning of the document must have the
   following structure:
   
     HTMLClueV (cluev)
       HTMLClueFlow (head)
 	 HTMLObject (child) */
static void
ensure_editable (HTMLEngine *engine)
{
	HTMLObject *cluev;
	HTMLObject *head;
	HTMLObject *child;

	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	cluev = engine->clue;
	if (cluev == NULL)
		engine->clue = cluev = html_cluev_new (0, 0, 100);

	head = HTML_CLUE (cluev)->head;
	if (head == NULL || HTML_OBJECT_TYPE (head) != HTML_TYPE_CLUEFLOW) {
		HTMLObject *clueflow;

		clueflow = html_clueflow_new (HTML_CLUEFLOW_STYLE_NORMAL, 0);
		html_clue_prepend (HTML_CLUE (cluev), clueflow);

		head = clueflow;
	}

	child = HTML_CLUE (head)->head;
	if (child == NULL) {
		HTMLObject *text_master;
		GdkColor black = { 0, 0, 0, 0 }; /* FIXME */

		text_master = html_text_master_new("", CSC_HTML_FONT_STYLE_DEFAULT, &black, NULL);
		html_clue_prepend (HTML_CLUE (head), text_master);
	}
}


void
html_engine_draw_background (HTMLEngine *e,
			     gint x, gint y, gint w, gint h)
{
	HTMLImagePointer *bgpixmap;
	GdkPixbuf *pixbuf = NULL;

	/* return if no background pixmap is set */
	bgpixmap = e->bgPixmapPtr;
	if (bgpixmap && bgpixmap->pixbuf) {
		pixbuf = bgpixmap->pixbuf;
	}

	html_painter_draw_background (e->painter, 
				      html_settings_get_color_allocated (e->settings, HTMLBgColor, e->painter),
				      pixbuf,
				      x, y,
				      w, h,
				      e->x_offset + x, e->y_offset + y);
}

void
html_engine_stop_parser (HTMLEngine *e)
{
	if (!e->parsing)
		return;

	if (e->timerId != 0) {
		gtk_timeout_remove (e->timerId);
		e->timerId = 0;
	}
	
	e->parsing = FALSE;

	html_stack_clear (e->color_stack);
	html_stack_clear (e->font_style_stack);
	html_stack_clear (e->font_face_stack);
	html_stack_clear (e->clueflow_style_stack);
}

CscHTMLStream *
html_engine_begin (HTMLEngine *e)
{
	CscHTMLStream *new_stream;

	html_tokenizer_begin (e->ht);
	
	free_block (e); /* Clear the block stack */

	html_engine_stop_parser (e);
	e->writing = TRUE;

	new_stream = csc_html_stream_new (CSC_HTML (e->widget),
					  html_engine_write,
					  html_engine_end,
					  e);

	html_image_factory_stop_animations (e->image_factory);
	e->newPage = TRUE;

	return new_stream;
}

void
html_engine_write (CscHTMLStream *handle,
		   const gchar *buffer,
		   guint size,
		   gpointer data)
{
	HTMLEngine *e;

	e = HTML_ENGINE (data);

	if (buffer == NULL)
		return;

	html_tokenizer_write (e->ht, buffer, size);

	if (e->parsing && e->timerId == 0) {
		e->timerId = gtk_timeout_add (TIMER_INTERVAL,
					      (GtkFunction) html_engine_timer_event,
					      e);
	}
}

static gboolean
html_engine_update_event (HTMLEngine *e)
{
	e->updateTimer = 0;

	html_engine_calc_size (e);

	if (GTK_LAYOUT (e->widget)->vadjustment == NULL
	    || ! html_gdk_painter_realized (HTML_GDK_PAINTER (e->painter)))
		return FALSE;
	
	/* Scroll page to the top on first display */
	if (e->newPage) {
		gtk_adjustment_set_value (GTK_LAYOUT (e->widget)->vadjustment, 0);
		e->newPage = FALSE;
	}

	if (! e->parsing && e->editable)
		html_cursor_home (e->cursor, e);

	html_image_factory_deactivate_animations (e->image_factory);
	html_engine_draw (e, 0, 0, e->width, e->height);
	
	/* Is y_offset too big? */
	if (html_engine_get_doc_height (e) - e->y_offset < e->height) {
		e->y_offset = html_engine_get_doc_height (e) - e->height;
		if (e->y_offset < 0)
			e->y_offset = 0;
	}
		
	/* Is x_offset too big? */
	if (html_engine_get_doc_width (e) - e->x_offset < e->width) {
		e->x_offset = html_engine_get_doc_width (e) - e->width;
		if (e->x_offset < 0)
			e->x_offset = 0;
	}

	/* Adjust the scrollbars */
	csc_html_private_calc_scrollbars (e->widget);

	return FALSE;
}


void
html_engine_schedule_update (HTMLEngine *p)
{
	if(p->updateTimer == 0)
		p->updateTimer = gtk_timeout_add (TIMER_INTERVAL,
						  (GtkFunction) html_engine_update_event,
						  p);
}


gboolean
html_engine_goto_anchor (HTMLEngine *e,
			 const gchar *anchor)
{
	GtkAdjustment *vadj;
	HTMLAnchor *a;
	gint x, y;

	g_return_val_if_fail (anchor != NULL, FALSE);

	if (!e->clue)
		return FALSE;

	x = y = 0;
	a = html_object_find_anchor (e->clue, anchor, &x, &y);

	if (a == NULL) {
		/* g_warning ("Anchor: \"%s\" not found", anchor); */
		return FALSE;
	}

	vadj = GTK_LAYOUT (e->widget)->vadjustment;

	if (y < vadj->upper - vadj->page_size)
		gtk_adjustment_set_value (vadj, y);
	else
		gtk_adjustment_set_value (vadj, vadj->upper - vadj->page_size);

	return TRUE;
}

static gboolean
html_engine_timer_event (HTMLEngine *e)
{
	static const gchar *end[] = { "</body>", 0};
	gint lastHeight;
	gboolean retval = TRUE;

	/* Has more tokens? */
	if (!html_tokenizer_has_more_tokens (e->ht) && e->writing) {
		retval = FALSE;
		goto out;
	}

	/* Getting height */
	lastHeight = html_engine_get_doc_height (e);

	e->parseCount = e->granularity;

	/* Parsing body height */
	if (parse_body (e, e->clue, end, TRUE))
	  	html_engine_stop_parser (e);

	html_engine_schedule_update (e);

	if (!e->parsing)
		retval = FALSE;

 out:
	if (!retval) {
		if(e->updateTimer != 0) {
			gtk_timeout_remove (e->updateTimer);
			html_engine_update_event (e);
		}
			
		e->timerId = 0;
	}

	return retval;
}

/* This makes sure that the last HTMLClueFlow is non-empty.  */
static void
ensure_last_clueflow (HTMLEngine *engine)
{
	HTMLObject *new_textmaster;
	HTMLClue *clue;
	HTMLClue *last_clueflow;

	clue = HTML_CLUE (engine->clue);
	if (clue == NULL)
		return;

	last_clueflow = HTML_CLUE (clue->tail);
	if (last_clueflow == NULL)
		return;

	if (last_clueflow->tail != NULL)
		return;

	new_textmaster = html_text_master_new("", CSC_HTML_FONT_STYLE_DEFAULT, html_settings_get_color(engine->settings, HTMLTextColor), NULL);
	html_clue_prepend (last_clueflow, new_textmaster);
}

static void
html_engine_end (CscHTMLStream *stream,
		 CscHTMLStreamStatus status,
		 gpointer data)
{
	HTMLEngine *e;

	e = HTML_ENGINE (data);

	e->writing = FALSE;

	while (html_engine_timer_event (e))
		;

	if (e->timerId != 0) {
		gtk_timeout_remove (e->timerId);
		e->timerId = 0;
	}

	html_tokenizer_end (e->ht);

	ensure_last_clueflow (e);
	
	if (e->editable) {
		ensure_editable (e);
		html_cursor_home (e->cursor, e);
	}

	gtk_signal_emit (GTK_OBJECT (e), signals[LOAD_DONE]);
	html_image_factory_stop_animations (e->image_factory);
	html_image_factory_cleanup (e->image_factory);
}


void
html_engine_draw (HTMLEngine *e,
		  gint x, gint y,
		  gint width, gint height)
{
	gint tx, ty;

	/* This case happens when the widget has not been shown yet.  */
	if (width == 0 || height == 0)
		return;
	
	tx = -e->x_offset + e->leftBorder;
	ty = -e->y_offset + e->topBorder;

	html_painter_begin (e->painter, x, y, x + width, y + height);

	html_engine_draw_background (e, x, y, width, height);

	if (e->clue)
		html_object_draw (e->clue,
				  e->painter,
				  x + e->x_offset - e->leftBorder,
				  y + e->y_offset - e->topBorder,
				  width,
				  height,
				  tx, ty);

	html_painter_end (e->painter);

	if (e->editable)
		html_engine_draw_cursor_in_area (e, x, y, width, height);
}


gint
html_engine_get_doc_width (HTMLEngine *e)
{
	if (e->clue)
		return e->clue->width + e->leftBorder + e->rightBorder;
	else
		return e->leftBorder + e->rightBorder;
}

gint
html_engine_get_doc_height (HTMLEngine *e)
{
	gint height;

	if (e->clue) {
		height = e->clue->ascent;
		height += e->clue->descent;
		height += e->topBorder;
		height += e->bottomBorder;

		return height;
	}
	
	return 0;
}

void
html_engine_calc_size (HTMLEngine *e)
{
	gint max_width, min_width;

	if (e->clue == 0)
		return;

	html_object_reset (e->clue);

	max_width = e->width - e->leftBorder - e->rightBorder;
	e->clue->width = max_width;

	min_width = html_object_calc_min_width (e->clue, e->painter);
	if (min_width > max_width)
		max_width = min_width;

	html_object_set_max_width (e->clue, e->painter, max_width);
	html_object_calc_size (e->clue, e->painter);

	e->clue->x = 0;
	e->clue->y = e->clue->ascent;
}

static void
destroy_form (gpointer data, gpointer user_data)
{
	html_form_destroy (HTML_FORM(data));
}

void
html_engine_parse (HTMLEngine *e)
{
	html_engine_stop_parser (e);

	/* reset search & replace */
	if (e->search_info) {
		html_search_destroy (e->search_info);
		e->search_info = NULL;
	}
	if (e->replace_info) {
		html_replace_destroy (e->replace_info);
		e->replace_info = NULL;
	}

	if (e->clue != NULL)
		html_object_destroy (e->clue);

	g_list_foreach (e->formList, destroy_form, NULL);

	g_list_free (e->formList);

	e->formList = NULL;
	e->form = NULL;
	e->formSelect = NULL;
	e->formTextArea = NULL;
	e->inOption = FALSE;
	e->inTextArea = FALSE;
	e->formText = g_string_new ("");

	e->flow = NULL;

	e->indent_level = 0;

	/* reset to default border size */
	e->leftBorder   = LEFT_BORDER;
	e->rightBorder  = RIGHT_BORDER;
	e->topBorder    = TOP_BORDER;
	e->bottomBorder = BOTTOM_BORDER;

	/* reset settings to default ones */
	html_settings_reset (e->settings, e->defaultSettings, e->painter);
		
	e->clue = html_cluev_new (0, 0, 100);
	HTML_CLUE (e->clue)->valign = HTML_VALIGN_TOP;
	HTML_CLUE (e->clue)->halign = HTML_HALIGN_LEFT;

	e->cursor->object = e->clue;

	/* Free the background pixmap */
	if (e->bgPixmapPtr) {
		html_image_factory_unregister(e->image_factory, e->bgPixmapPtr, NULL);
		e->bgPixmapPtr = NULL;
	}

	e->parsing = TRUE;
	e->avoid_para = TRUE;
	e->pending_para = FALSE;

	e->pending_para_alignment = HTML_HALIGN_LEFT;

	e->timerId = gtk_timeout_add (TIMER_INTERVAL, (GtkFunction) html_engine_timer_event, e);
}


HTMLObject *
html_engine_get_object_at (HTMLEngine *e,
			   gint x, gint y,
			   guint *offset_return,
			   gboolean for_cursor)
{
	HTMLObject *clue;
	HTMLObject *obj;

	clue = HTML_OBJECT (e->clue);
	if (clue == NULL)
		return NULL;

	if (for_cursor) {
		gint width, height;

		width = clue->width;
		height = clue->ascent + clue->descent;

		if (width == 0 || height == 0)
			return NULL;

		if (x < e->leftBorder)
			x = e->leftBorder;
		else if (x >= e->leftBorder + width)
			x = e->leftBorder + width - 1;

		if (y < e->topBorder)
			y = e->topBorder;
		else if (y >= e->topBorder + height) {
			x = e->leftBorder + width - 1;
			y = e->topBorder + height - 1;
		}
	}

	obj = html_object_check_point (clue,
				       e->painter,
				       x - e->leftBorder, y - e->topBorder,
				       offset_return,
				       for_cursor);

	return obj;
}

const gchar *
html_engine_get_link_at (HTMLEngine *e, gint x, gint y)
{
	HTMLObject *obj;

	if (e->clue == NULL)
		return NULL;

	obj = html_engine_get_object_at (e, x, y, NULL, FALSE);

	if (obj != NULL)
		return html_object_get_url (obj);

	return NULL;
}


/**
 * html_engine_set_editable:
 * @e: An HTMLEngine object
 * @editable: A flag specifying whether the object must be editable
 * or not
 * 
 * Make @e editable or not, according to the value of @editable.
 **/
void
html_engine_set_editable (HTMLEngine *e,
			  gboolean editable)
{
	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	if ((e->editable && editable) || (! e->editable && ! editable))
		return;

	html_engine_disable_selection (e);

	if (! e->editable && editable)
		html_cursor_home (e->cursor, e);

	html_engine_draw (e, 0, 0, e->width, e->height);
	e->editable = editable;

	if (editable) {
		ensure_editable (e);
		if (e->have_focus)
			html_engine_setup_blinking_cursor (e);
	} else {
		if (e->have_focus)
			html_engine_stop_blinking_cursor (e);
	}
}

gboolean
html_engine_get_editable (HTMLEngine *e)
{
	g_return_val_if_fail (e != NULL, FALSE);
	g_return_val_if_fail (HTML_IS_ENGINE (e), FALSE);

	if (e->editable && ! e->parsing && e->timerId == 0)
		return TRUE;
	else
		return FALSE;
}


void
html_engine_set_focus (HTMLEngine *engine,
		       gboolean have_focus)
{
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	if (engine->editable) {
		if (! engine->have_focus && have_focus)
			html_engine_setup_blinking_cursor (engine);
		else if (engine->have_focus && ! have_focus)
			html_engine_stop_blinking_cursor (engine);
	}

	engine->have_focus = have_focus;
}


void
html_engine_make_cursor_visible (HTMLEngine *e)
{
	HTMLCursor *cursor;
	HTMLObject *object;
	gint x1, y1, x2, y2;

	g_return_if_fail (e != NULL);

	if (! e->editable)
		return;

	cursor = e->cursor;
	object = cursor->object;
	if (object == NULL)
		return;

	html_object_get_cursor (object, e->painter, cursor->offset, &x1, &y1, &x2, &y2);

	x1 += e->leftBorder;
	y1 += e->topBorder;
	x2 += e->leftBorder;
	y2 += e->topBorder;

	if (x1 + e->leftBorder >= e->x_offset + e->width)
		e->x_offset = x1 + e->leftBorder - e->width + 1;
	else if (x1 < e->x_offset + e->leftBorder)
		e->x_offset = x1 - e->leftBorder;

	if (y2 + e->topBorder >= e->y_offset + e->height)
		e->y_offset = y2 + e->topBorder - e->height + 1;
	else if (y1 < e->y_offset + e->topBorder)
		e->y_offset = y1 - e->topBorder;
}


/* Draw queue handling.  */

void
html_engine_flush_draw_queue (HTMLEngine *e)
{
	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	html_draw_queue_flush (e->draw_queue);
}

void
html_engine_queue_draw (HTMLEngine *e, HTMLObject *o)
{
	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));
	g_return_if_fail (o != NULL);

	if (e->freeze_count == 0)
		html_draw_queue_add (e->draw_queue, o);
}

void
html_engine_queue_clear (HTMLEngine *e,
			 gint x, gint y,
			 guint width, guint height)
{
	g_return_if_fail (e != NULL);

	if (e->freeze_count == 0)
		html_draw_queue_add_clear (e->draw_queue, x, y, width, height,
					   html_settings_get_color (e->settings, HTMLBgColor));
}


void
html_engine_form_submitted (HTMLEngine *e,
			    const gchar *method,
			    const gchar *action,
			    const gchar *encoding)
{
	gtk_signal_emit (GTK_OBJECT (e), signals[SUBMIT], method, action, encoding);

}


/* Selection handling.  */

struct _SelectRegionData {
	HTMLEngine *engine;
	HTMLObject *obj1, *obj2;
	guint offset1, offset2;
	gint x1, y1, x2, y2;
	guint select : 1;
	guint queue_draw : 1;
	guint active_selection : 1;
};
typedef struct _SelectRegionData SelectRegionData;

static void
select_region_forall (HTMLObject *self,
		      gpointer data)
{
	SelectRegionData *select_data;
	gboolean changed;

	select_data = (SelectRegionData *) data;
	changed = FALSE;

	if (self == select_data->obj1 || self == select_data->obj2) {
		select_data->active_selection = TRUE;
		if (select_data->obj1 == select_data->obj2) {
			if (select_data->offset2 > select_data->offset1)
				changed = html_object_select_range (select_data->obj1,
								    select_data->engine,
								    select_data->offset1,
								    select_data->offset2 - select_data->offset1,
								    select_data->queue_draw);
			else if (select_data->offset2 < select_data->offset1)
				changed = html_object_select_range (select_data->obj1,
								    select_data->engine,
								    select_data->offset2,
								    select_data->offset1 - select_data->offset2,
								    select_data->queue_draw);
			select_data->select = FALSE;
		} else {
			guint offset;

			if (self == select_data->obj1)
				offset = select_data->offset1;
			else
				offset = select_data->offset2;

			if (select_data->select) {
				changed = html_object_select_range (self,
								    select_data->engine,
								    0, offset,
								    select_data->queue_draw);
				select_data->select = FALSE;
			} else {
				changed = html_object_select_range (self,
								    select_data->engine,
								    offset, -1,
								    select_data->queue_draw);
				select_data->select = TRUE;
			}
		}
	} else {
		if (select_data->select) {
			changed = html_object_select_range (self, select_data->engine,
							    0, -1, select_data->queue_draw);
			select_data->active_selection = TRUE;
		} else {
			changed = html_object_select_range (self, select_data->engine,
							    0, 0, select_data->queue_draw);
		}
	}

	if (self->parent == select_data->obj1 || self->parent == select_data->obj2) {
		HTMLObject *next;
		gint y;

		if (self->parent == select_data->obj1)
			y = select_data->y1;
		else
			y = select_data->y2;

		next = self->next;
		while (next != NULL
		       && (HTML_OBJECT_TYPE (next) == HTML_TYPE_TEXTMASTER
			   || HTML_OBJECT_TYPE (next) == HTML_TYPE_LINKTEXTMASTER)) {
			next = next->next;
		}

		if (next == NULL
		    || (next->y + next->descent != self->y + self->descent
			&& next->y - next->ascent > y)) {
			select_data->select = ! select_data->select;
		}
	}
}

/* FIXME this implementation could be definitely smarter.  */
/* FIXME maybe this should go into `htmlengine-edit.c'.  */
void
html_engine_select_region (HTMLEngine *e,
			   gint x1, gint y1,
			   gint x2, gint y2,
			   gboolean queue_draw)
{
	SelectRegionData *data;
	gint x, y;

	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	if (e->clue == NULL)
		return;

	data = g_new (SelectRegionData, 1);

	data->engine = e;
	data->obj1 = html_engine_get_object_at (e, x1, y1, &data->offset1, TRUE);
	data->obj2 = html_engine_get_object_at (e, x2, y2, &data->offset2, TRUE);
	data->select = FALSE;
	data->queue_draw = queue_draw;

	if (data->obj1 == NULL || data->obj2 == NULL
	    || (data->obj1 == data->obj2 && data->offset1 == data->offset2)) {
		html_engine_unselect_all (e, queue_draw);
		g_free (data);
		return;
	}

	html_object_calc_abs_position (data->obj1, &x, &y);
	data->x1 = x1 - x;
	data->y1 = y1 - y;

	html_object_calc_abs_position (data->obj2, &x, &y);
	data->x2 = x2 - x;
	data->y2 = y2 - y;

	html_object_forall (e->clue, select_region_forall, data);

	e->active_selection = data->active_selection;
	csc_html_debug_log (e->widget, "Active selection: %s\n", e->active_selection ? "TRUE" : "FALSE");

	g_free (data);
}

static void
unselect_forall (HTMLObject *self,
		 gpointer data)
{
	SelectRegionData *select_data;

	select_data = (SelectRegionData *) data;
	html_object_select_range (self, select_data->engine, 0, 0, select_data->queue_draw);
}

void
html_engine_unselect_all (HTMLEngine *e,
			  gboolean queue_draw)
{
	SelectRegionData *select_data;

	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	if (! e->active_selection)
		return;

	if (e->clue == NULL)
		return;

	select_data = g_new (SelectRegionData, 1);
	select_data->engine = e;
	select_data->queue_draw = queue_draw;

	html_object_forall (e->clue, unselect_forall, select_data);

	g_free (select_data);

	e->active_selection = FALSE;

	html_engine_edit_selection_updater_reset (e->selection_updater);

	csc_html_debug_log (e->widget, "Active selection: FALSE\n");
}

void
html_engine_disable_selection (HTMLEngine *e)
{
	g_return_if_fail (e != NULL);
	g_return_if_fail (HTML_IS_ENGINE (e));

	if (e->editable) {
		if (e->mark == NULL)
			return;

		html_cursor_destroy (e->mark);
		e->mark = NULL;
	}

	html_engine_unselect_all (e, TRUE);
}


/* Retrieving the selection as a string.  */

static void
get_selection_forall_func (HTMLObject *self,
			   gpointer data)
{
	GString *buffer;

	buffer = (GString *) data;
	html_object_append_selection_string (self, buffer);
}

gchar *
html_engine_get_selection_string (HTMLEngine *engine)
{
	GString *buffer;
	gchar *string;

	g_return_val_if_fail (engine != NULL, NULL);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), NULL);

	if (engine->clue == NULL)
		return NULL;

	buffer = g_string_new (NULL);
	html_object_forall (engine->clue, get_selection_forall_func, buffer);

	string = buffer->str;
	g_string_free (buffer, FALSE);

	return string;
}


/* Freeze/thaw.  */

gboolean
html_engine_frozen (HTMLEngine *engine)
{
	g_return_val_if_fail (engine != NULL, FALSE);
	g_return_val_if_fail (HTML_IS_ENGINE (engine), FALSE);

	return engine->freeze_count > 0;
}

void
html_engine_freeze (HTMLEngine *engine)
{
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	engine->freeze_count++;
}

void
html_engine_thaw (HTMLEngine *engine)
{
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));
	g_return_if_fail (engine->freeze_count > 0);

	engine->freeze_count--;

	if (engine->freeze_count == 0) {
		/* FIXME This should happen in the idle loop, and should be
                   more conservative about the area to redraw.  This is gross.  */

		html_engine_calc_size (engine);
		html_engine_draw (engine, 0, 0, engine->width, engine->height);
	}
}


/**
 * html_engine_load_empty:
 * @engine: An HTMLEngine object
 * 
 * Load an empty document into the engine.
 **/
void
html_engine_load_empty (HTMLEngine *engine)
{
	g_return_if_fail (engine != NULL);
	g_return_if_fail (HTML_IS_ENGINE (engine));

	/* FIXME: "slightly" hackish.  */
	html_engine_stop_parser (engine);
	html_engine_parse (engine);
	html_engine_stop_parser (engine);

	ensure_editable (engine);
}

static void
move_to_found (HTMLEngine *e, HTMLSearch *info)
{
	HTMLObject *first = HTML_OBJECT (info->found->data);
	HTMLObject *last = HTML_OBJECT (g_list_last (info->found)->data);
	HTMLTextSlave *slave;
	gint x, y, ex, ey, w, h;
	gint nx = e->x_offset;
	gint ny = e->y_offset;

	/* x,y is top-left corner, ex+w,ey+h is bottom-right */
	html_object_calc_abs_position (HTML_OBJECT (info->found->data), &x, &y);

	/* find slave where starts selection and get its coordinates as upper-left corner */
	while (first->next && HTML_OBJECT_TYPE (first->next) == HTML_TYPE_TEXTSLAVE) {
		first = first->next;
		slave = HTML_TEXT_SLAVE (first);
		if (slave->posStart+slave->posLen >= info->start_pos) {
			html_object_calc_abs_position (HTML_OBJECT (slave), &x, &y);
			break;
		}
	}

	/* the same with last */
	html_object_calc_abs_position (last, &ex, &ey);

	while (last->next && HTML_OBJECT_TYPE (last->next) == HTML_TYPE_TEXTSLAVE) {
		last = last->next;
		slave = HTML_TEXT_SLAVE (last);
		if (slave->posStart+slave->posLen >= info->start_pos) {
			html_object_calc_abs_position (HTML_OBJECT (slave), &ex, &ey);
			break;
		}
	}


	w = ex - x + last->width;
	h = ey - y + last->ascent+last->descent;

	printf ("html_engine_display_area %d %d %d %d\n", x, y, w, h);

	/* now calculate cschtml adustments */
	if (x <= e->x_offset)
		nx = x;
	else if (x + w > e->x_offset + e->width)
		nx = x + w - e->width;

	if (y <= e->y_offset)
		ny = y;
	else if (y + h > e->y_offset + e->height)
		ny = y + h - e->height;

	/* finally adjust them if they changed */
	if (e->x_offset != nx)
		gtk_adjustment_set_value (GTK_LAYOUT (e->widget)->hadjustment, nx);

	if (e->y_offset != ny)
		gtk_adjustment_set_value (GTK_LAYOUT (e->widget)->vadjustment, ny);
}

static void
display_search_results (HTMLEngine *e, HTMLSearch *info)
{
	GList *cur = info->found;
	guint len  = info->found_len;
	guint pos  = info->start_pos;
	guint cur_len;

	if (e->editable) {
		html_engine_disable_selection (e);
		html_cursor_jump_to (e->cursor, e, (HTMLObject *) info->found->data, info->start_pos);
		html_engine_set_mark (e);
		html_cursor_jump_to (e->cursor, e, info->last, info->stop_pos);
	} else {
		html_engine_unselect_all (e, TRUE);
		e->active_selection = TRUE;

		/* go thru all objects (Text's) in found list and do select_range on it */
		while (cur) {
			cur_len = HTML_TEXT (cur->data)->text_len;
			printf ("select len: %d range obj: %p pos: %d len: %d\n", info->found_len,
				HTML_OBJECT (cur->data), pos, (cur_len-pos < len) ? cur_len-pos : len);
			html_object_select_range (HTML_OBJECT (cur->data), e, pos,
						  (cur_len-pos < len) ? cur_len-pos : len, TRUE);
			len -= cur_len-pos;
			pos  = 0;
			cur  = cur->next;
		}
	}
	if (info->found)
		move_to_found (e, info);
}

gboolean
html_engine_search (HTMLEngine *e, const gchar *text,
		    gboolean case_sensitive, gboolean forward, gboolean regular)
{
	gboolean retval;
	HTMLSearch *info;

	printf ("html_engine_search cs: %d fw: %d re: %d\n", case_sensitive, forward, regular);

	if (e->search_info) {
		html_search_destroy (e->search_info);
	}

	info = e->search_info = html_search_new (text, case_sensitive, forward, regular);
	html_search_push (info, e->clue);
	retval = html_object_search (e->clue, info);

	if (retval) {
		display_search_results (e, info);
	}

	return retval;
}

gboolean
html_engine_search_next (HTMLEngine *e)
{
	HTMLSearch *info = e->search_info;
	gboolean retval = FALSE;

	if (!info) {
		return FALSE;
	}

	printf ("search_next\n");

	if (info->stack) {
		retval = html_object_search (HTML_OBJECT (info->stack->data), info);
	} else {
		html_search_push (info, e->clue);
		retval = html_object_search (e->clue, info);
	}
	if (retval) {
		display_search_results (e, info);
	} else {
		html_search_pop (info);
		html_engine_disable_selection (e);
	}

	return retval;
}

gboolean
html_engine_search_incremental (HTMLEngine *e)
{
	return FALSE;
}

void
html_engine_replace (HTMLEngine *e, const gchar *text, const gchar *rep_text,
		     gboolean case_sensitive, gboolean forward, gboolean regular,
		     void (*ask)(HTMLEngine *, gpointer), gpointer ask_data)
{
	printf ("html_engine_replace\n");

	if (e->replace_info)
		html_replace_destroy (e->replace_info);
	e->replace_info = html_replace_new (rep_text, ask, ask_data);

	if (html_engine_search (e, text, case_sensitive, forward, regular))
		ask (e, ask_data);
}

static void replace(HTMLEngine *e) {
	HTMLObject *first = HTML_OBJECT(e->search_info->found->data);
	HTMLObject *new_text;

	html_engine_edit_selection_update_now(e->selection_updater);

	new_text = html_text_master_new(e->replace_info->text, HTML_TEXT(first)->font_style, &HTML_TEXT(first)->color, HTML_TEXT(first)->font_face);
	html_engine_paste_object(e, new_text, TRUE);

	/* update search info to point just behind replaced text */
	g_list_free (e->search_info->found);
	e->search_info->found = g_list_append (NULL, e->cursor->object);
	e->search_info->start_pos = e->search_info->stop_pos = e->cursor->offset-1;
	e->search_info->found_len = 0;
	html_search_pop  (e->search_info);
	html_search_push (e->search_info, e->cursor->object->parent);
}

void
html_engine_replace_do (HTMLEngine *e, HTMLReplaceQueryAnswer answer)
{
	g_assert (e->replace_info);

	switch (answer) {
	case RQA_ReplaceAll:
		html_undo_level_begin (e->undo, "replace all");
		replace (e);
		while (html_engine_search_next (e))
			replace (e);
		html_undo_level_end (e->undo);
	case RQA_Cancel:
		html_replace_destroy (e->replace_info);
		e->replace_info = NULL;
		html_engine_disable_selection (e);
		break;

	case RQA_Replace:
		html_undo_level_begin (e->undo, "replace");
		replace (e);
		html_undo_level_end (e->undo);
	case RQA_Next:
		if (html_engine_search_next (e))
			e->replace_info->ask (e, e->replace_info->ask_data);
		else
			html_engine_disable_selection (e);
		break;
	}
}
