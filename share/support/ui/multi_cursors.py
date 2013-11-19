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

<key action="Move to previous character">Left</key>
<key action="Move to next character">Right</key>

<key action="Move to previous line">Up</key>
<key action="Move to next line">Down</key>

<key action="Delete word forward" >primary-Delete</key>

<key action="Remove all multi cursors" >primary-Escape</key>

<key action="goto beginning of line" >Home</key>
<key action="goto end of line" >End</key>
"""


@interactive("Editor",  name="Remove all multi cursors")
def remove_all_multi_cursors():
    buffer = GPS.EditorBuffer.get()
    buffer.remove_all_multi_cursors()


@interactive("Editor", name="Add multi cursor and go down")
def mc_down():
    buffer = GPS.EditorBuffer.get()
    view = buffer.current_view()
    loc = view.cursor()
    buffer.add_multi_cursor(loc)

    buffer.set_multi_cursors_manual_sync()
    view.goto(GPS.EditorLocation(buffer, loc.line() + 1, loc.column()))
    buffer.set_multi_cursors_auto_sync()


@interactive("Editor", name="Add multi cursor and go up")
def mc_up():
    buffer = GPS.EditorBuffer.get()
    view = buffer.current_view()
    loc = view.cursor()
    buffer.add_multi_cursor(loc)

    buffer.set_multi_cursors_manual_sync()
    view.goto(GPS.EditorLocation(buffer, max(loc.line() - 1, 1), loc.column()))
    buffer.set_multi_cursors_auto_sync()

id_pattern = re.compile(r"[\w0-9_]")


@interactive("Editor", name="Add multi cursor to all references of entity")
def mc_all_entity_references():

    editor = GPS.EditorBuffer.get()
    marks = []
    overlay = editor.create_overlay("entityrefs_overlay")
    overlay.set_property(
        "background", "#37A"
    )

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

    def loc_tuple(loc):
        return (loc.line(), loc.column())

    loc = editor.current_view().cursor()
    loc_id_start, loc_id_end = get_word_bounds(loc)
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

    def on_move(hook_name, file_name, line, column):
        """
        Event handler on cursor move. Gets out of alias expansion mode
        when the cursor gets out of the zone.
        """
        start_loc = mark_start.location()
        end_loc = mark_end.location()
        cursor_loc = editor.current_view().cursor()
        if not (start_loc <= cursor_loc <= end_loc):
            exit()

    def exit():
        editor.remove_overlay(
            overlay,
            editor.beginning_of_buffer(),
            editor.end_of_buffer()
        )
        editor.remove_all_multi_cursors()
        Hook("character_added").remove(on_edit)
        Hook("location_changed").remove(on_move)

    marks.append((mark_start, mark_end))
    apply_overlay(editor, mark_start, mark_end, overlay)
    cursor_loc_t = loc_tuple(loc)
    word_offset = loc.column() - loc_id_start.column()
    identifier = editor.get_chars(loc_id_start, loc_id_end)

    entity = Entity(
        identifier, editor.file(), loc_id_start.line(), loc_id_start.column()
    )

    locs = [EditorLocation(editor, floc.line(), floc.column())
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
            editor.add_multi_cursor(loc)

    Hook("character_added").add(on_edit)
    Hook("location_changed").add(on_move)

GPS.parse_xml(xml_conf)
