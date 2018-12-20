"""Provides sort functions in the editors

This file provides two sort functions, which can be used to sort lines
in a source file.
To use: first select the lines that you wish to sort, and then select
one of the two menus:
  - /Edit/Selection/Sort
  - /Edit/Selection/Sort Reverse
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import string
from gps_utils import interactive


@interactive("Editor", filter="Source editor",
             name="sort selected lines descending")
def sort_selection_revert():
    """Sorts the current selection, in descending order"""
    sort_selection(revert=True)


@interactive("Editor", filter="Source editor",
             name="sort selected lines ascending")
def sort_selection(revert=False):
    """Sorts the current selection, in ascending order"""
    GPS.current_context(refresh=True)
    ed = GPS.EditorBuffer.get()   # current editor, always
    start = ed.selection_start()
    to = ed.selection_end()

    # If the selection is empty, do nothing

    if to - start == 0:
        return

    # If the end is at the first column we really want to sort the lines
    # before the current one.

    if to.column() == 1:
        to = to.forward_char(-1)

    selection = ed.get_chars(start, to)

    lines = string.split(selection, "\n")
    # strip off extraneous trailing "" line
    lines = lines[:-1]

    # If the selection contains less than two lines, do nothing
    if len(lines) < 2:
        return

    language = ed.file().language().lower()
    case_sensitive = language not in ("ada", "project file")

    if case_sensitive:
        lines.sort()
    else:
        lines.sort(key=str.lower)

    if revert:
        lines.reverse()
    with ed.new_undo_group():
        ed.delete(start, to)
        ed.insert(start, "\n".join(lines) + "\n")
