import GPS
from gps_utils import *
from text_utils import goto_word_start, goto_word_end
import re

EditorLocation.goto_word_start = goto_word_start
EditorLocation.goto_word_end = goto_word_end

xml_conf = """
<key action="Add multi cursor and go down">shift-alt-Down</key>
<key action="Add multi cursor and go up">shift-alt-Up</key>

<key action="Move to next word">control-Right</key>
<key action="Move to previous word">control-Left</key>

<key action="Move to previous character">Left</key>
<key action="Move to next character">Right</key>

<key action="Move to previous line">Up</key>
<key action="Move to next line">Down</key>

<key action="Delete word forward" >control-Delete</key>

<key action="Remove all multi cursors" >control-Escape</key>

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

    def loc_tuple(loc):
        return (loc.line(), loc.column())

    editor = GPS.EditorBuffer.get()
    loc = editor.current_view().cursor()
    loc_id_start = loc.goto_word_start()
    loc_id_end = loc.goto_word_end()

    # Check the case when we are at the end of a word
    if not id_pattern.match(loc.get_char()):
        ploc = loc.forward_char(-1)
        # If we are really not in an identifier, exit
        if not id_pattern.match(ploc.get_char()):
            return
        else:
            loc_id_end = ploc

    cursor_loc_t = loc_tuple(loc)
    word_offset = loc.column() - loc_id_start.column()
    identifier = editor.get_chars(loc_id_start, loc_id_end)

    entity = Entity(identifier, editor.file(), loc_id_start.line(), loc_id_start.column())

    locs = [EditorLocation(editor, floc.line(), floc.column())
            for floc in entity.references()
            if floc.file() == editor.file()]

    locs_set = set()
    for s_loc in locs:
        loc = s_loc.forward_char(word_offset)
        loc_t = loc_tuple(loc)
        if not (loc_t in locs_set or loc_t == cursor_loc_t):
            locs_set.add(loc_t)
            editor.add_multi_cursor(loc)

GPS.parse_xml(xml_conf)
