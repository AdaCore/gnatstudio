"""
Functionnality and actions related to multi cursors
"""


import GPS
from gps_utils import *
from text_utils import goto_word_start, goto_word_end
import re

xml_conf = """
<key action="Add multi cursor and go down">shift-alt-Down</key>
<key action="Add multi cursor and go up">shift-alt-Up</key>

<key action="Move to next word">primary-Right</key>
<key action="Move to previous word">primary-Left</key>

<key action="Move to next word (extend selection)">shift-primary-Right</key>
<key action="Move to previous word (extend selection)">shift-primary-Left</key>

<key action="Move to previous char">Left</key>
<key action="Move to next char">Right</key>

<key action="Move to previous char (extend selection)">shift-Left</key>
<key action="Move to next char (extend selection)">shift-Right</key>

<key action="Move to previous line (extend selection)">shift-Up</key>
<key action="Move to next line (extend selection)">shift-Down</key>

<key action="Move to previous line">Up</key>
<key action="Move to next line">Down</key>

<key action="Delete word forward" >primary-Delete</key>
<key action="Delete word backward" >primary-BackSpace</key>

<key action="forward delete" >Delete</key>
<key action="backward delete" >BackSpace</key>

<key action="goto beginning of line" >Home</key>
<key action="goto end of line" >End</key>

<key action="goto beginning of line (extend selection)" >shift-Home</key>
<key action="goto end of line (extend selection)" >shift-End</key>
"""

mc_on_entity_color = GPS.Preference("Plugins/multi_cursors/mc_on_entity_color")

mc_on_entity_color.create(
    "Color for 'multi cursor on all references'", "color",
    "Color for 'multi cursor on all references'"
    " You must restart gps to take changes into account.",
    "#3070A0"
)


@interactive("Editor", name="Add multi cursor and go down")
def mc_down():
    ed = GPS.EditorBuffer.get()
    view = ed.current_view()
    loc = view.cursor()
    ed.add_cursor(loc)

    ed.get_cursors()[0].set_manual_sync()
    view.goto(ed.at(loc.line() + 1, loc.column()))
    ed.set_cursors_auto_sync()


@interactive("Editor", name="Add multi cursor and go up")
def mc_up():
    ed = GPS.EditorBuffer.get()
    view = ed.current_view()
    loc = view.cursor()
    ed.add_cursor(loc)

    ed.get_cursors()[0].set_manual_sync()
    view.goto(ed.at(max(loc.line() - 1, 1), loc.column()))
    ed.set_cursors_auto_sync()

id_pattern = re.compile(r"[\w0-9_]")


@interactive("Editor", name="Add multi cursor to next occurence of selection")
def mc_select_next_occurence():
    ed = GPS.EditorBuffer.get()
    mc_sel = ed.create_overlay("mc_selection")
    text = ed.get_chars(ed.selection_start(),
                        ed.selection_end().forward_char(-1))

    st, end = ed.current_view().cursor().search(text)
    while st.has_overlay(mc_sel):
        st, end = end.search(text)

    mc = ed.add_cursor(st)
    mc.move(st)
    mc.move(end, True)
    ed.update_cursors_selection()
    ed.set_cursors_auto_sync()


@interactive("Editor", name="Add multi cursor to all references of entity")
def mc_all_entity_references():

    def get_word_bounds(loc):
        loc_id_start = goto_word_start(loc)
        loc_id_end = goto_word_end(loc)
        # Check the case when we are at the end of a word
        if not id_pattern.match(loc.get_char()):
            ploc = loc.forward_char(-1)
            # If we are really not in an identifier, exit
            if not id_pattern.match(ploc.get_char()):
                return
            else:
                loc_id_end = ploc

        return loc_id_start, loc_id_end

    editor = GPS.EditorBuffer.get()
    marks = []
    loc = editor.current_view().cursor()
    loc_id_start, loc_id_end = get_word_bounds(loc)
    identifier = editor.get_chars(loc_id_start, loc_id_end)

    try:
        entity = Entity(identifier, editor.file(),
                        loc_id_start.line(), loc_id_start.column())
    except GPS.Exception:
        return

    overlay = editor.create_overlay("entityrefs_overlay")
    overlay.set_property(
        "background", mc_on_entity_color.get()
    )

    def loc_tuple(loc):
        return loc.line(), loc.column()

    mark_start = loc_id_start.create_mark()
    mark_end = loc_id_end.forward_char().create_mark(left_gravity=False)

    def apply_overlay(editor, mark_start, mark_end, overlay):
        """
        Apply overlay overlay between mark_start and mark end
        if mark_start - mark_end >= 1 char
        """
        lstart = mark_start.location()
        lend = mark_end.location().forward_char(-1)
        if lend >= lstart:
            editor.apply_overlay(overlay, lstart, lend)

    # noinspection PyUnusedLocal
    def on_edit(hook_name, file_name):
        """
        Event handler on insert/delete. Mainly ensures that the current field
        in alias expansion is highlighted (via the aliases overlay)
        """
        if editor == EditorBuffer.get(file_name):
            editor.remove_overlay(
                overlay,
                editor.beginning_of_buffer(),
                editor.end_of_buffer()
            )
            for mark_start, mark_end in marks:
                apply_overlay(editor, mark_start, mark_end, overlay)

    # noinspection PyUnusedLocal
    def on_move(hook_name, file_name, line, column):
        """
        Event handler on cursor move. Gets out of alias expansion mode
        when the cursor gets out of the zone.
        """
        start_loc = mark_start.location()
        end_loc = mark_end.location()
        cursor_loc = editor.current_view().cursor()
        if not (start_loc <= cursor_loc <= end_loc):
            exit_alias_expansion()

    def exit_alias_expansion():
        editor.remove_overlay(
            overlay,
            editor.beginning_of_buffer(),
            editor.end_of_buffer()
        )
        editor.remove_all_slave_cursors()
        Hook("character_added").remove(on_edit)
        Hook("location_changed").remove(on_move)

    marks.append((mark_start, mark_end))
    apply_overlay(editor, mark_start, mark_end, overlay)
    cursor_loc_t = loc_tuple(loc)
    word_offset = loc.column() - loc_id_start.column()

    locs = [editor.at(floc.line(), floc.column())
            for floc in entity.references()
            if floc.file() == editor.file()]

    locs_set = set()
    for s_loc in locs:
        loc = s_loc.forward_char(word_offset)
        loc_t = loc_tuple(loc)
        if not (loc_t in locs_set or loc_t == cursor_loc_t):
            locs_set.add(loc_t)
            s, e = get_word_bounds(loc)
            ms = s.create_mark()
            me = e.forward_char().create_mark(left_gravity=False)
            marks.append((ms, me))
            apply_overlay(editor, ms, me, overlay)
            editor.add_cursor(loc)

    Hook("character_added").add(on_edit)
    Hook("location_changed").add(on_move)

GPS.parse_xml(xml_conf)
