"""
This file provides the core functionnality for interactive aliases expansion
in GPS
"""

from GPS import Hook, Alias, EditorBuffer
import GPS
from gs_utils import interactive
from collections import OrderedDict
from itertools import zip_longest
from text_utils import goto_word_start
import re
from color_utils import Color

subst_pattern = re.compile("%\(.*?\)|%_")
# The regexp used to detect aliases parameters

lsp_subst_pattern = re.compile("\${[^}]*}|\$[0-9]")
# The regexp used to detect LSP snippets parameters

color_pref_name = "Src-Editor-Ephemeral-Smart"

aliases_editor_filename = None
# The filename that currently has alias expansion


def get_paragraph_color():
    """
    Get the preference corresponding to paragraph background
    color, and turn it into an hex string
    """
    pref_string = GPS.Preference("Src-Editor-Reference-Style").get()
    c = Color(pref_string.split("@")[2])
    return c.hex, c.shade_or_lighten(0.1).hex


def get_comments_colors():
    """
    Get the preference corresponding to comment color
    """
    _, c1, c2 = GPS.Preference("Src-Editor-Comments-Variant")\
                   .get().split("@")
    if c2 == "white" or c2 == "rgb(255,255,255)":
        c2 = None
    return c1, c2


def get_current_field_bg_color():
    """
    Get the background color that should be used to highlight
    aliases' current fields.
    """
    _, _, bg_color = GPS.Preference(color_pref_name).get().split("@")
    return bg_color


def reset_overlay(editor):
    """
    Reset the aliases overlay completely
    """
    editor.remove_overlay(
        editor.aliases_overlay,
        editor.alias_begin_mark.location(),
        editor.alias_end_mark.location()
    )


def filter_alias_expansion(context):
    try:
        return is_in_alias_expansion(EditorBuffer.get(open=False, force=False))
    except Exception:
        return False


def is_in_alias_expansion(editor):
    """
    Returns true if the editor is in the process of alias expansion
    """
    return getattr(editor, "current_alias_mark_index", None) is not None


def move_expected_while_in_alias(editor):
    """
    Returns true if cursor moves are expected and should not cancel alias mode
    """
    return bool(getattr(editor, "alias_move_expected", None))


def exit_alias_expand(editor):
    """
    Exit alias expansion.
    """
    global aliases_editor_filename
    aliases_editor_filename = None

    editor.remove_all_slave_cursors()
    reset_overlay(editor)
    editor.remove_overlay(
        editor.aliases_background_overlay,
        editor.alias_begin_mark.location().beginning_of_line(),
        editor.alias_end_mark.location()
    )
    editor.remove_overlay(
        editor.aliases_overlay_next,
        editor.alias_begin_mark.location().beginning_of_line(),
        editor.alias_end_mark.location()
    )

    editor.current_alias_mark_index = None
    editor.alias_marks = []
    editor.alias_end_mark = None
    editor.alias_begin_mark = None
    Hook("location_changed").remove(on_move)
    Hook("mdi_child_selected").remove(on_mdi_child_change)


@interactive("Editor", name="Expand alias under cursor")
def expand_alias_action():
    """
    Action to expand the alias under cursor
    the editor
    """
    editor = EditorBuffer.get(open=False, force=False)
    if not editor:
        return

    if is_in_alias_expansion(editor):
        return
    with editor.new_undo_group():
        cursor_loc = editor.current_view().cursor().forward_char(-1)
        start_loc = goto_word_start(cursor_loc)
        alias_name = editor.get_chars(start_loc, cursor_loc)
        editor.delete(start_loc, cursor_loc)
        alias = Alias.get(alias_name)
        if alias:
            expand_alias(editor, alias)


def toggle_field(editor=None, backward=False, first=False):
    if not editor or not is_in_alias_expansion(editor):
        editor = EditorBuffer.get()

    try:
        if editor:
            reset_overlay(editor)

            editor.apply_overlay(
                editor.aliases_background_overlay,
                editor.alias_begin_mark.location().beginning_of_line(),
                editor.alias_end_mark.location()
            )

            if editor.current_alias_mark_index is None:
                return

            if first:
                editor.current_alias_mark_index = 0
            elif backward:
                editor.current_alias_mark_index -= 1
            else:
                editor.current_alias_mark_index += 1

            index = editor.current_alias_mark_index

            if index < 0:
                exit_alias_expand(editor)
                return
            if index > len(editor.alias_marks) - 1:
                if editor.last_alias_mark:
                    editor.alias_move_expected = True
                    editor.current_view().goto(
                        editor.last_alias_mark.location())
                    editor.alias_move_expected = False
                exit_alias_expand(editor)
                return False

            editor.alias_move_expected = True
            try:
                editor.remove_all_slave_cursors()
                marks = editor.alias_marks[index]

                editor.current_view().goto(marks[0][0].location())

                # Add multi cursors for every other mark
                if len(marks) > 1:
                    for mark_begin, mark_end in marks[1:]:
                        editor.add_cursor(mark_begin.location())

                # Select the placeholder text
                for j, cursor in enumerate(editor.cursors()):
                    cursor.move(marks[j][1].location(), True)
            finally:
                editor.alias_move_expected = False
    except AttributeError:
        return


@interactive("Editor",
             name="Toggle to previous alias field",
             filter=filter_alias_expansion)
def toggle_prev_field():
    """
    When in alias expansion, toggle to previous field
    """
    toggle_field(backward=True)


@interactive("Editor",
             name="Toggle to next alias field",
             filter=filter_alias_expansion)
def toggle_next_field():
    """
    When in alias expansion, toggle to next field
    """
    toggle_field(backward=False)


def apply_overlay(editor, mark_start, mark_end, overlay):
    """
    Apply overlay overlay between mark_start and mark end
    if mark_start - mark_end >= 1 char
    """
    lstart = mark_start.location()
    lend = mark_end.location().forward_char(-1)
    if lend >= lstart:
        editor.apply_overlay(overlay, lstart, lend)


def on_move(hook_name, file_name, line, column):
    """
    Event handler on cursor move. Gets out of alias expansion mode
    when the cursor gets out of the zone.
    """
    editor = EditorBuffer.get(file_name)

    # This hook is global: it could happen that we are calling it on another
    # editor than the one where the alias expansion is occurring: simply
    # return in this case.
    # Similarly, if we are expecting a cursor move at this point, do not
    # exit the alias expansion mode.
    if not is_in_alias_expansion(editor) \
            or move_expected_while_in_alias(editor):
        return

    index = editor.current_alias_mark_index
    start_mark, end_mark = editor.alias_marks[index][0]
    start_loc = start_mark.location()
    end_loc = end_mark.location()
    cursor_loc = editor.current_view().cursor()
    if not (start_loc <= cursor_loc <= end_loc):
        exit_alias_expand(editor)


def on_mdi_child_change(hook_name, child):
    """
    Exit alias expansion when the focused editor changes.
    """
    if aliases_editor_filename:
        editor = GPS.EditorBuffer.get(GPS.File(aliases_editor_filename))
        exit_alias_expand(editor)


def expand_alias(editor, alias, from_lsp=False):
    """
    Expand given alias in the given editor buffer at the point where the cursor
    is.
    The given alias can be either a :class:`GPS.Alias` instance or a LSP
    completion snippet passed as a string (when `from_lsp` is True).
    """
    # Remove the hook functions if any since we don't want to add them
    # several times when dealing with nested alias expansions.
    Hook("location_changed").remove(on_move)
    Hook("mdi_child_selected").remove(on_mdi_child_change)

    Hook("location_changed").add(on_move)
    Hook("mdi_child_selected").add(on_mdi_child_change)

    expansion = alias if from_lsp else alias.get_expanded()
    text_chunks = (lsp_subst_pattern.split(expansion)
                   if from_lsp else subst_pattern.split(expansion))

    # The pattern used to recognize where the last mark (i.e: final tab stop)
    # should be placed
    last_mark_pattern = "$0" if from_lsp else "%_"

    # Get the parameter substitutions via the appropriate regexp.
    # The "${<number>:" at the beginning and the '}' at the end
    # are removed from the LSP parameter substitutions
    # through the 's[4:-1] if not s[1:2].isalpha()' list comprehension
    # condition.
    if from_lsp:
        substs = [s[4:-1] if not s[1:2].isdigit() else s
                  for s in lsp_subst_pattern.findall(expansion)]
    else:
        substs = [s[2:-1] if s != "%_" else s
                  for s in subst_pattern.findall(expansion)]

    # Don't do anything if we did not find any parameter substitutions
    # in the LSP snippet
    if from_lsp and not substs:
        return

    global aliases_editor_filename
    aliases_editor_filename = editor.file().path

    if not hasattr(editor, "alias_marks"):
        editor.alias_marks = []
        editor.alias_begin_mark = None

    substs_marks_dict = OrderedDict()

    editor.aliases_overlay = editor.create_overlay("aliases_overlay")
    editor.aliases_overlay.set_property(
        "background", get_current_field_bg_color()
    )

    color, color1 = get_paragraph_color()

    editor.aliases_background_overlay = editor.create_overlay(
        "aliases_background_overlay"
    )
    editor.aliases_background_overlay.set_property(
        "paragraph-background", color
    )

    editor.aliases_overlay_next = editor.create_overlay(
        "aliases_overlay_next"
    )
    c1, c2 = get_comments_colors()
    editor.aliases_overlay_next.set_property("foreground", c1)
    if c2:
        editor.aliases_overlay_next.set_property("background", "#124")

    # Create a mark with right gravity so it will stay at the end of what we
    # have inserted, giving us the current insert point

    new_alias_begin_mark = editor.current_view().cursor().create_mark()
    new_alias_end_mark = editor.current_view().cursor().create_mark(
        left_gravity=False)
    new_last_alias_mark = None
    new_last_alias_mark_idx = -1

    insert_mark = new_alias_end_mark
    insert_loc = insert_mark.location()

    text_to_insert = ""

    # Construct the text to insert by appending the text chunks and the
    # substitutions that have been found

    for text, subst in zip_longest(text_chunks, substs):
        text_to_insert += text

        # If we find final tab stop, get its index in the string to insert
        # to create the corresponding mark once the text is inserted
        if subst and subst == last_mark_pattern:
            new_last_alias_mark_idx = len(text_to_insert.decode("utf-8"))
        elif subst:
            default_value = "" if from_lsp else alias.get_default_value(subst)
            value = default_value if default_value else subst

            text_to_insert += value

    editor.insert(insert_loc, text_to_insert)

    if new_last_alias_mark_idx != -1:
        new_last_alias_mark = insert_loc.forward_char(
            new_last_alias_mark_idx).create_mark()

    idx = 0
    new_alias_marks = []

    text_to_insert = text_to_insert.decode("utf-8")

    for subst in substs:
        if subst != last_mark_pattern:

            # Search for the inserted substitution value to know where to place
            # its corresponding start and end marks
            default_value = "" if from_lsp else alias.get_default_value(subst)
            value = default_value if default_value else subst
            value = value.decode("utf-8")

            idx = text_to_insert.find(value, idx)

            subst_loc_start = insert_loc.forward_char(idx)
            subst_loc_end = subst_loc_start.forward_char(len(value))
            subst_mark_start = subst_loc_start.create_mark()
            subst_mark_end = subst_loc_end.create_mark(left_gravity=False)

            # Insert the start and end marks of the given subsitution in a
            # dictionary.
            # This is needed since a substitution can occur several times
            # (e.g: procedure %name is %_ end %name): in these cases, we
            # want to link all the occurrences marks to the same substitution.

            if subst in substs_marks_dict:
                substs_marks_dict[subst].append(
                    (subst_mark_start, subst_mark_end))
            else:
                substs_marks_dict[subst] = [(subst_mark_start, subst_mark_end)]

            idx += len(subst)

    new_alias_marks = list(substs_marks_dict.values())

    for marks_list in new_alias_marks:
        for mark_start, mark_end in marks_list:
            apply_overlay(
                editor, mark_start, mark_end, editor.aliases_overlay_next
            )

    # If we are dealing with a nested alias expansions, append the new alias
    # marks to the existing ones and don't override the alias begin/end
    # marks.

    if len(editor.alias_marks) > 0:
        editor.alias_marks = new_alias_marks + \
            editor.alias_marks[editor.current_alias_mark_index:]
    else:
        editor.alias_marks = new_alias_marks
        editor.last_alias_mark = new_last_alias_mark
        editor.alias_begin_mark = new_alias_begin_mark
        editor.alias_end_mark = new_alias_end_mark

    editor.current_alias_mark_index = 0

    editor.alias_move_expected = True
    editor.alias_move_expected = False
    toggle_field(editor=editor, backward=False, first=True)


def expand_lsp_snippet(snippet):
    """
    Expand the given LSP snippet in the current editor.
    """

    editor = EditorBuffer.get(open=False, force=False)
    if not editor:
        return

    expand_alias(editor=editor, alias=snippet, from_lsp=True)


EditorBuffer.expand_alias = expand_alias
