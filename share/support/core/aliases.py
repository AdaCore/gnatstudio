"""
This file provides the core functionnality for interactive aliases expansion
in GPS
"""


from GPS import *
from gps_utils import *
from itertools import izip_longest
from collections import defaultdict
from text_utils import goto_word_start
import re
from color_utils import Color

subst_pattern = re.compile("%\(.*?\)|%_")
id_pattern = re.compile(r"[^\w0-9_]")

xml_conf = """
<key action="Toggle to next alias field">control-Tab</key>
<key action="Expand alias under cursor">control-o</key>
"""

color_pref_name = "Plugins/aliases/color_current_field"
Preference(color_pref_name).create(
    "Aliases current field color",
    "color",
    "Background color for the current field under completion",
    "#AAF"
)


def get_paragraph_color():
    """
    Get the preference corresponding to paragraph background
    color, and turn it into an hex string
    """
    pref_string = GPS.Preference("Src-Editor-Reference-Style").get()
    c = Color(
        pref_string.split("@")[2]
    )
    return c.to_hex(), c.shade_or_lighten(0.1).to_hex()


def get_comments_colors():
    """
    Get the preference corresponding to comment color
    """
    _, c1, c2 = GPS.Preference("Src-Editor-Comments-Variant")\
                   .get().split("@")
    if c2 == "white" or c2 == "rgb(255,255,255)":
        c2 = None
    return c1, c2


def reset_overlay(ed_buffer):
    """
    Reset the aliases overlay completely
    """
    ed_buffer.remove_overlay(
        ed_buffer.aliases_overlay,
        ed_buffer.alias_begin_mark.location(),
        ed_buffer.alias_end_mark.location()
    )

def is_in_alias_expansion(ed_buffer):
    """
    Returns true if the ed_buffer is in the process of alias expansion
    """
    return bool(getattr(ed_buffer, "current_alias_mark_index", None))

def exit_alias_expand(ed_buffer):
    """
    Exit alias expansion.
    """
    ed_buffer.remove_all_multi_cursors()
    reset_overlay(ed_buffer)
    ed_buffer.remove_overlay(
        ed_buffer.aliases_background_overlay,
        ed_buffer.alias_begin_mark.location().beginning_of_line(),
        ed_buffer.alias_end_mark.location()
    )
    ed_buffer.remove_overlay(
        ed_buffer.aliases_background_overlay_1,
        ed_buffer.beginning_of_buffer(),
        ed_buffer.end_of_buffer()
    )
    ed_buffer.current_alias_mark_index = None
    ed_buffer.alias_marks = None
    ed_buffer.alias_end_mark = None
    ed_buffer.alias_begin_mark = None
    Hook("character_added").remove(on_edit)
    Hook("location_changed").remove(on_move)
    ed_buffer.finish_undo_group()


@interactive("Editor", name="Expand alias under cursor")
def expand_alias_action():
    """
    Action to expand the alias under cursor directly from
    the editor
    """
    ed_buffer = EditorBuffer.get()
    if is_in_alias_expansion(ed_buffer):
        return
    ed_buffer.start_undo_group()
    cursor_loc = ed_buffer.current_view().cursor().forward_char(-1)
    start_loc = goto_word_start(cursor_loc)
    alias_name = ed_buffer.get_chars(start_loc, cursor_loc)
    ed_buffer.delete(start_loc, cursor_loc)
    alias = Alias.get(alias_name)
    if alias:
        expand_alias(ed_buffer, alias)


@interactive("Editor",  name="Toggle to next alias field")
def toggle_next_field(ed_buffer=None):
    """
    When in alias expansion, toggle to next field
    """

    if not ed_buffer:
        ed_buffer = EditorBuffer.get()

    try:
        reset_overlay(ed_buffer)

        ed_buffer.apply_overlay(
            ed_buffer.aliases_background_overlay_1,
            ed_buffer.beginning_of_buffer(),
            ed_buffer.end_of_buffer()
        )

        ed_buffer.apply_overlay(
            ed_buffer.aliases_background_overlay,
            ed_buffer.alias_begin_mark.location().beginning_of_line(),
            ed_buffer.alias_end_mark.location()
        )

        i = ed_buffer.current_alias_mark_index

        if i is None:
            return

        if i >= len(ed_buffer.alias_marks):
            if ed_buffer.last_alias_mark:
                ed_buffer.current_view().goto(ed_buffer.last_alias_mark.location())
                exit_alias_expand(ed_buffer)
                # ??? Doesn't work every time if executed only one
                execute_action("/Edit/Format Selection")
                execute_action("/Edit/Format Selection")
            else:
                exit_alias_expand(ed_buffer)
            return

        ed_buffer.remove_all_multi_cursors()
        marks = ed_buffer.alias_marks[i]

        # Delete the placeholder text
        for mark_start, mark_end in marks:
            lstart = mark_start.location()
            lend = mark_end.location().forward_char(-1)
            if lend >= lstart:
                ed_buffer.delete(lstart, lend)

        ed_buffer.current_view().goto(marks[0][0].location())
        try:
            execute_action("/Edit/Format Selection")
        except:
            pass

        reset_overlay(ed_buffer)

        # Add multi cursors for every other mark
        if len(marks) > 1:
            for mark_begin, mark_end in marks[1:]:
                ed_buffer.add_multi_cursor(mark_begin.location())

        ed_buffer.current_alias_mark_index += 1

    except AttributeError:
        return


def apply_overlay(ed_buffer, mark_start, mark_end, overlay):
    """
    Apply overlay overlay between mark_start and mark end
    if mark_start - mark_end >= 1 char
    """
    lstart = mark_start.location()
    lend = mark_end.location().forward_char(-1)
    if lend >= lstart:
        ed_buffer.apply_overlay(overlay, lstart, lend)


def on_edit(hook_name, file_name):
    """
    Event handler on insert/delete. Mainly ensures that the current field
    in alias expansion is highlighted (via the aliases overlay)
    """
    ed_buffer = EditorBuffer.get(file_name)
    if ed_buffer.current_alias_mark_index > 0:
        marks_list = ed_buffer.alias_marks[ed_buffer.current_alias_mark_index - 1]
        reset_overlay(ed_buffer)
        for mark_start, mark_end in marks_list:
            apply_overlay(ed_buffer, mark_start, mark_end, ed_buffer.aliases_overlay)


def on_move(hook_name, file_name, line, column):
    """
    Event handler on cursor move. Gets out of alias expansion mode
    when the cursor gets out of the zone.
    """
    ed_buffer = EditorBuffer.get(file_name)
    index = ed_buffer.current_alias_mark_index - 1
    start_mark, end_mark = ed_buffer.alias_marks[index][0]
    start_loc = start_mark.location()
    end_loc = end_mark.location()
    cursor_loc = ed_buffer.current_view().cursor()
    if not (start_loc <= cursor_loc <= end_loc):
        exit_alias_expand(ed_buffer)


def expand_alias(ed_buffer, alias):
    """
    Expand given alias in the given editor buffer at the point where the cursor
    is.
    """
    text_chunks = subst_pattern.split(alias.expansion)
    substs = [s[2:-1] if s != "%_" else s
              for s in subst_pattern.findall(alias.expansion)]
    alias_labels = defaultdict(list)

    ed_buffer.aliases_overlay = ed_buffer.create_overlay("aliases_overlay")
    ed_buffer.aliases_overlay.set_property(
        "background", Preference(color_pref_name).get()
    )

    color, color1 = get_paragraph_color()
    ed_buffer.aliases_background_overlay_1 = ed_buffer.create_overlay(
        "aliases_background_overlay_1"
    )
    ed_buffer.aliases_background_overlay_1.set_property(
        "paragraph-background", color1
    )
    ed_buffer.aliases_background_overlay = ed_buffer.create_overlay(
        "aliases_background_overlay"
    )
    ed_buffer.aliases_background_overlay.set_property(
        "paragraph-background", color
    )

    ed_buffer.aliases_overlay_next = ed_buffer.create_overlay("aliases_overlay_next")
    c1, c2 = get_comments_colors()
    ed_buffer.aliases_overlay_next.set_property("foreground", c1)
    if c2:
        ed_buffer.aliases_overlay_next.set_property("background", "#124")

    # Create a mark with right gravity so it will stay at the end of what we
    # have inserted, giving us the current insert point
    ed_buffer.alias_begin_mark = ed_buffer.current_view().cursor().create_mark()
    ed_buffer.alias_end_mark = ed_buffer.current_view().cursor().create_mark(
        left_gravity=False
    )
    insert_mark = ed_buffer.alias_end_mark

    for text, subst in izip_longest(text_chunks, substs):
        ed_buffer.insert(insert_mark.location(), text)
        if subst:
            alias_labels[subst].append(insert_mark.location().create_mark())

    ed_buffer.alias_marks = []
    substs_set = set()
    for subst in substs:
        if subst not in substs_set and subst != "%_":
            ed_buffer.alias_marks.append(
                [(m, m.location().create_mark(left_gravity=False))
                 for m in alias_labels[subst]]
            )
            for m in alias_labels[subst]:
                ed_buffer.insert(m.location(), "<{0}>".format(subst))
            substs_set.add(subst)

    for marks_list in ed_buffer.alias_marks:
        for mark_start, mark_end in marks_list:
            apply_overlay(
                ed_buffer, mark_start, mark_end, ed_buffer.aliases_overlay_next
            )

    if "%_" in alias_labels:
        ed_buffer.last_alias_mark = alias_labels["%_"][0]
    else:
        ed_buffer.last_alias_mark = None

    ed_buffer.current_alias_mark_index = 0
    Hook("character_added").add(on_edit)
    Hook("location_changed").add(on_move)

    ed_buffer.indent(ed_buffer.alias_begin_mark.location(),
                  ed_buffer.alias_end_mark.location())
    toggle_next_field(ed_buffer)

EditorBuffer.expand_alias = expand_alias
parse_xml(xml_conf)
