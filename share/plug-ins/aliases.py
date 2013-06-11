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
    pref_string = GPS.Preference("Src-Editor-Reference-Style").get()
    print pref_string
    c = Color(
        pref_string.split("@")[2]
    )
    return c.shade_or_lighten(0.1).to_hex()


def get_comments_colors():
    _, c1, c2 = GPS.Preference("Src-Editor-Comments-Variant")\
                   .get().split("@")
    if c2 == "white" or c2 == "rgb(255,255,255)":
        c2 = None
    return c1, c2


def reset_overlay(editor):
    """
    Reset the aliases overlay completely
    """
    editor.remove_overlay(
        editor.aliases_overlay,
        editor.alias_begin_mark.location(),
        editor.alias_end_mark.location()
    )


def exit_alias_expand(editor):
    """
    Exit alias expansion.
    """
    editor.remove_all_multi_cursors()
    reset_overlay(editor)
    editor.remove_overlay(
        editor.aliases_background_overlay,
        editor.alias_begin_mark.location().beginning_of_line(),
        editor.alias_end_mark.location()
    )
    editor.current_alias_mark_index = None
    editor.alias_marks = None
    editor.current_alias_mark_index = None
    editor.alias_end_mark = None
    editor.alias_begin_mark = None
    Hook("character_added").remove(on_edit)
    Hook("location_changed").remove(on_move)


@interactive("Editor", name="Expand alias under cursor")
def expand_alias_action():
    """
    Action to expand the alias under cursor directly from
    the editor
    """
    editor = EditorBuffer.get()
    cursor_loc = editor.current_view().cursor().forward_char(-1)
    start_loc = goto_word_start(cursor_loc)
    alias_name = editor.get_chars(start_loc, cursor_loc)
    editor.delete(start_loc, cursor_loc)
    alias = Alias.get(alias_name)
    if alias:
        expand_alias(editor, alias)


@interactive("Editor",  name="Toggle to next alias field")
def toggle_next_field(editor=None):
    """
    When in alias expansion, toggle to next field
    """

    if not editor:
        editor = EditorBuffer.get()

    try:
        reset_overlay(editor)

        editor.apply_overlay(
            editor.aliases_background_overlay,
            editor.alias_begin_mark.location().beginning_of_line(),
            editor.alias_end_mark.location()
        )

        i = editor.current_alias_mark_index

        if i is None:
            return

        if i >= len(editor.alias_marks):
            if editor.last_alias_mark:
                editor.current_view().goto(editor.last_alias_mark.location())
                exit_alias_expand(editor)
                # ??? Doesn't work every time if executed only one
                execute_action("/Edit/Format Selection")
                execute_action("/Edit/Format Selection")
            else:
                exit_alias_expand(editor)
            return

        editor.remove_all_multi_cursors()
        marks = editor.alias_marks[i]

        # Delete the placeholder text
        for mark_start, mark_end in marks:
            lstart = mark_start.location()
            lend = mark_end.location().forward_char(-1)
            if lend >= lstart:
                editor.delete(lstart, lend)

        editor.current_view().goto(marks[0][0].location())
        try:
            execute_action("/Edit/Format Selection")
        except:
            pass

        reset_overlay(editor)

        # Add multi cursors for every other mark
        if len(marks) > 1:
            for mark_begin, mark_end in marks[1:]:
                editor.add_multi_cursor(mark_begin.location())

        editor.current_alias_mark_index += 1

    except AttributeError:
        return


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
    editor = EditorBuffer.get(file_name)
    if editor.current_alias_mark_index > 0:
        marks_list = editor.alias_marks[editor.current_alias_mark_index - 1]
        reset_overlay(editor)
        for mark_start, mark_end in marks_list:
            apply_overlay(editor, mark_start, mark_end, editor.aliases_overlay)


def on_move(hook_name, file_name, line, column):
    """
    Event handler on cursor move. Gets out of alias expansion mode
    when the cursor gets out of the zone.
    """
    editor = EditorBuffer.get(file_name)
    index = editor.current_alias_mark_index - 1
    start_mark, end_mark = editor.alias_marks[index][0]
    start_loc = start_mark.location()
    end_loc = end_mark.location()
    cursor_loc = editor.current_view().cursor()
    if not (start_loc <= cursor_loc <= end_loc):
        exit_alias_expand(editor)


def expand_alias(editor, alias):
    """
    Expand given alias in the given editor buffer at the point where the cursor
    is.
    """
    text_chunks = subst_pattern.split(alias.expansion)
    substs = [s[2:-1] if s != "%_" else s
              for s in subst_pattern.findall(alias.expansion)]
    alias_labels = defaultdict(list)

    editor.aliases_overlay = editor.create_overlay("aliases_overlay")
    editor.aliases_overlay.set_property(
        "background", Preference(color_pref_name).get()
    )

    editor.aliases_background_overlay = editor.create_overlay(
        "aliases_background_overlay"
    )
    editor.aliases_background_overlay.set_property(
        "paragraph-background", get_paragraph_color()
    )

    editor.aliases_overlay_next = editor.create_overlay("aliases_overlay_next")
    c1, c2 = get_comments_colors()
    editor.aliases_overlay_next.set_property("foreground", c1)
    if c2:
        editor.aliases_overlay_next.set_property("background", "#124")

    # Create a mark with right gravity so it will stay at the end of what we
    # have inserted, giving us the current insert point
    editor.alias_begin_mark = editor.current_view().cursor().create_mark()
    editor.alias_end_mark = editor.current_view().cursor().create_mark(
        left_gravity=False
    )
    insert_mark = editor.alias_end_mark

    for text, subst in izip_longest(text_chunks, substs):
        editor.insert(insert_mark.location(), text)
        if subst:
            alias_labels[subst].append(insert_mark.location().create_mark())

    editor.alias_marks = []
    substs_set = set()
    for subst in substs:
        if subst not in substs_set and subst != "%_":
            editor.alias_marks.append(
                [(m, m.location().create_mark(left_gravity=False))
                 for m in alias_labels[subst]]
            )
            for m in alias_labels[subst]:
                editor.insert(m.location(), "<{0}>".format(subst))
            substs_set.add(subst)

    for marks_list in editor.alias_marks:
        for mark_start, mark_end in marks_list:
            apply_overlay(
                editor, mark_start, mark_end, editor.aliases_overlay_next
            )

    if "%_" in alias_labels:
        editor.last_alias_mark = alias_labels["%_"][0]
    else:
        editor.last_alias_mark = None

    editor.current_alias_mark_index = 0
    Hook("character_added").add(on_edit)
    Hook("location_changed").add(on_move)

    editor.indent(editor.alias_begin_mark.location(),
                  editor.alias_end_mark.location())
    toggle_next_field(editor)

EditorBuffer.expand_alias = expand_alias
parse_xml(xml_conf)
