"""
This plug-in provides a function to reformat the current line
or selection.
It tries to do a number of things, which can be configured
separately to suit your work style.

This function is available in GPS through the action
"Format Selection", which is bound to the <tab> key by default.
"""

import GPS
from gps_utils import interactive, make_interactive, in_editor
import pygps
import aliases
import align
from gi.repository import Gtk


if not GPS.Logger("PREVENT_ALIGN_ON_TAB").active:
    tabs_align_selection = GPS.Preference("Editor/tabs_align_selection")
    tabs_align_selection.create(
        "Align selection on tab", "boolean",
        "Whether <tab> should also align arrows, use clauses and "
        "assignments (:=) when multiple lines are selected.",
        True)


@interactive(name='Format selection', category='Editor',
             filter="Source editor")
def smart_tab():
    """
    This action is the default binding for the tab key, and will
    apply different behaviors depending on the current context.

    When expanding aliases, <tab> will move to the next field.
    Otherwise, when multiple lines are selected it will try to align
    special sequences like ":", "=>", "use" and ":=".
    Finally, it will automatically indent the selected lines.
    """

    editor = GPS.EditorBuffer.get()

    # When expanding aliases, <tab> should move to the next field

    if aliases.is_in_alias_expansion(editor):
        aliases.toggle_next_field(editor)
        return

    # If multiple lines are selected, perform various alignments

    if not GPS.Logger("PREVENT_ALIGN_ON_TAB").active:
        if tabs_align_selection.get():
            start = editor.selection_start()
            end = editor.selection_end()

            if abs(start.line() - end.line()) >= 1:
                align.align_colons()
                align.align_arrows()
                align.align_use_clauses()
                align.align_assignments()

    # Otherwise, reformat the current selection

    if editor.file().language() == "python":
        o = editor.selection_end().column()
        d = python_parse_tab(editor,
                             editor.selection_start(),
                             editor.selection_end())
    else:
        action = GPS.Action("Autoindent selection")
        if not action.execute_if_possible():
            editor.insert(editor.current_view().cursor(), "\t")


def python_parse_tab(e, beginning, end):
    """
       parse the text for python files when hitting tab
       * return the correction of number of whitespaces needed
       * e is a GPS.EditorBuffer object
       * beginning and end are selection area's GPS.EditorBuffer.Location
       * cursor position correced after return
    """

    # if multiple lines selected, indent each one by order
    if beginning.line() != end.line():
        for i in range(beginning.line(), end.line()+1):
            d = python_parse_tab(e,
                                 e.at(i, beginning.column()),
                                 e.at(i, end.column()))
        return d

    if (end.line() == e.lines_count() and
            e.get_chars(end.beginning_of_line(), end.end_of_line()) is ""):
        return 0

    source = e.get_chars().splitlines()

    # get current indent number

    last = source[end.line()-1]
    current = len(last) - len(last.lstrip(" "))

    # 0 if at line 1 no indentation needed
    # default: previous_indent is 0, next indent level is 0

    previous_indent = 0
    level = 0
    indent = 0
    # if more than 1 lines' text:
    # modify previous_indent and level according to prefixes in codes

    if end.line() > 1:

        # 1 find the next indent level
        # default: no level change, previous line decides
        group = []
        prev = source[end.line()-2]
        previous_indent = len(prev) - len(prev.lstrip(" "))

        tmphead = prev.lstrip(" ")

        # case : enter subprogram, innermost level decides
        if tmphead.endswith(":"):
            level = 1
            group = ["if", "else", "for", "while",
                     "def", "class", "try", "except"]
        else:
            # case: return to a function, previous def decides
            if tmphead.startswith("return"):
                level = -2
                group = ["def"]

            # case: break out loops, innermost loop decides
            if tmphead.startswith("break") or \
               tmphead.startswith("continue"):
                level = -1
                group = ["for", "while"]

        # 2 find prev indent quantity (# of whitespaces)
        prefix = ""
        begin = 0

        # not for the case that no indentation needed
        if level != 0:
            for i in range(end.line()-2, -1, -1):
                for pref in group:
                    if source[i].lstrip(" ").startswith(pref):
                        begin = i
                        prefix = pref
                        break

                # if hit the prefix during loop, modify previous_indent
                if prefix is not "":
                    previous_indent = len(source[begin].split(prefix)[0])
                    break

        # 3 find the correct indent number
        level = 0 if level < 0 else level
        indent = previous_indent + level*4

    # 4 make corrections for cursor
    d = indent - current

    if d == 0:
        # if indent quantity is correct, and cursor at a wild place
        # move it to the indentation end
        if current == len(last):
            e.main_cursor().move(end.end_of_line())
    else:
        # under-indent: add blank
        if d > 0:
            e.insert(e.at(end.line(), 1), " "*d)
        # over-indent: delete blank
        if d < 0:
            e.delete(e.at(end.line(), 1), e.at(end.line(), -d))
        # adjust cursor position by relative quantity
        e.main_cursor().move(e.at(end.line(), end.column()+d))

    return d


@interactive(name='smart escape',
             category='Editor',
             filter="Source editor")
def smart_escape():
    """
    This action is the default binding for the Escape key, and will
    interrupt the current alias expansion (if any).
    """
    editor = GPS.EditorBuffer.get()
    if aliases.is_in_alias_expansion(editor):
        aliases.exit_alias_expand(editor)

    editor.remove_all_slave_cursors()
